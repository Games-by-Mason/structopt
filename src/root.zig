const std = @import("std");
const Allocator = std.mem.Allocator;
const ArgIterator = std.process.ArgIterator;
const FieldEnum = std.meta.FieldEnum;
const log = std.log.scoped(.structopt);
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;
const expectEqualSlices = std.testing.expectEqualSlices;
const expectError = std.testing.expectError;

pub const Error = error{ OutOfMemory, Parser, Help };

pub const Command = struct {
    const Self = @This();

    /// May be elided on the root command.
    name: ?[:0]const u8,
    description: ?[]const u8 = null,
    named_args: []const NamedArg = &.{},
    positional_args: []const PositionalArg = &.{},
    subcommands: []const Command = &.{},

    pub fn Result(comptime self: Command) type {
        return struct {
            /// May be elided on the root command.
            program_name: ?[]const u8,
            named: @FieldType(self.Subcommand(), "named"),
            positional: @FieldType(self.Subcommand(), "positional"),
            subcommand: @FieldType(self.Subcommand(), "subcommand"),

            fn fromSubcommand(program_name: []const u8, subcommand: self.Subcommand()) @This() {
                return .{
                    .program_name = program_name,
                    .named = subcommand.named,
                    .positional = subcommand.positional,
                    .subcommand = subcommand.subcommand,
                };
            }
        };
    }

    /// The result of parsing a command
    pub fn Subcommand(comptime self: Command) type {
        var named_fields: [self.named_args.len]std.builtin.Type.StructField = undefined;
        for (self.named_args, 0..) |arg, i| {
            if (arg.type == ?bool) {
                @compileError(arg.long ++ ": booleans cannot be nullable");
            }
            if (@typeInfo(arg.type) == .optional and arg.accum) {
                @compileError(arg.long ++ ": accum arguments cannot be nullable");
            }
            const T = if (arg.accum) std.array_list.Managed(arg.type) else arg.type;
            named_fields[i] = .{
                .name = arg.long,
                .type = T,
                .default_value_ptr = arg.default,
                .is_comptime = false,
                .alignment = @alignOf(T),
            };
        }
        const NamedResults = @Type(.{ .@"struct" = .{
            .layout = .auto,
            .fields = &named_fields,
            .decls = &.{},
            .is_tuple = false,
        } });

        var positional_fields: [self.positional_args.len]std.builtin.Type.StructField = undefined;
        for (self.positional_args, 0..) |arg, i| {
            if (@typeInfo(arg.type) == .optional) {
                @compileError(arg.meta ++ ": positional arguments cannot be nullable");
            }
            if (arg.type == bool) {
                @compileError(arg.meta ++ ": positional arguments cannot be booleans");
            }
            positional_fields[i] = .{
                .name = arg.meta,
                .type = arg.type,
                .default_value_ptr = null,
                .is_comptime = false,
                .alignment = @alignOf(arg.type),
            };
        }
        const PositionalResults = @Type(.{ .@"struct" = .{
            .layout = .auto,
            .fields = &positional_fields,
            .decls = &.{},
            .is_tuple = false,
        } });

        var command_tags: [self.subcommands.len]std.builtin.Type.EnumField = undefined;
        for (self.subcommands, 0..) |command, i| {
            command_tags[i] = .{
                .name = command.name.?,
                .value = i,
            };
        }
        const SubcommandTag = @Type(.{ .@"enum" = .{
            .tag_type = u16,
            .fields = &command_tags,
            .decls = &.{},
            .is_exhaustive = true,
        } });

        const Subcommands = if (self.subcommands.len > 0) b: {
            var command_fields: [self.subcommands.len]std.builtin.Type.UnionField = undefined;
            for (self.subcommands, 0..) |command, i| {
                const Current = Subcommand(command);
                command_fields[i] = .{
                    .name = command.name.?,
                    .type = Current,
                    .alignment = @alignOf(Current),
                };
            }
            break :b @Type(.{ .@"union" = .{
                .layout = .auto,
                .tag_type = SubcommandTag,
                .fields = &command_fields,
                .decls = &.{},
            } });
        } else void;

        return struct {
            named: NamedResults,
            positional: PositionalResults,
            subcommand: ?Subcommands,
        };
    }

    /// Frees the parsed result. Only necessary if lists are used.
    pub fn parseFree(comptime self: @This(), result: self.Result()) void {
        inline for (self.named_args) |named_arg| {
            if (named_arg.accum) {
                @field(result.named, named_arg.long).deinit();
            }
        }
    }

    /// Show the help menu
    pub fn usageBrief(self: @This()) void {
        self.usageImpl(true);
    }

    pub fn usage(self: @This()) void {
        self.usageImpl(false);
    }

    fn usageImpl(self: @This(), comptime brief: bool) void {
        log.info("{f}", .{self.fmtUsage(brief)});
    }

    /// Parse the command line arguments for this process, exit on help or failure. Panics on OOM.
    pub fn parseOrExit(
        self: @This(),
        gpa: Allocator,
        iter: *std.process.ArgIterator,
    ) self.Result() {
        return self.parseFromIter(gpa, iter) catch |err| switch (err) {
            error.Help => std.process.exit(0),
            error.Parser => std.process.exit(2),
            error.OutOfMemory => @panic("OOM"),
        };
    }

    /// Parse the command line arguments for this process, return the defaults on help or failure.
    /// Requires every argument have a default.
    pub fn parseOrDefaults(self: @This(), iter: *std.process.ArgIterator) self.Result() {
        return self.parse(iter) catch |err| switch (err) {
            error.Help, error.Parser => .{},
        };
    }

    /// Parse the command line arguments for this process.
    pub fn parse(self: @This(), gpa: Allocator) Error!self.Result() {
        var iter = try std.process.argsWithAllocator(gpa);
        defer iter.deinit();
        return self.parseFromIter(gpa, &iter);
    }

    /// Parse commands from the given slice.
    pub fn parseFromSlice(
        self: @This(),
        gpa: Allocator,
        args: []const [:0]const u8,
    ) Error!self.Result() {
        const Iter = struct {
            slice: []const [:0]const u8,

            fn init(slice: []const [:0]const u8) @This() {
                return .{ .slice = slice };
            }

            fn next(iter: *@This()) ?[:0]const u8 {
                if (iter.slice.len == 0) return null;
                const result = iter.slice[0];
                iter.slice = iter.slice[1..];
                return result;
            }

            fn skip(iter: *@This()) bool {
                if (iter.slice.len == 0) return false;
                iter.slice = iter.slice[1..];
                return true;
            }
        };

        var iter = Iter.init(args);
        return self.parseFromIter(gpa, &iter);
    }

    /// Parses the arguments from an iterator, usually `std.process.ArgIterator`.
    pub fn parseFromIter(self: @This(), gpa: Allocator, iter: anytype) Error!self.Result() {
        const program_name = iter.*.next() orelse {
            log.err("expected program name", .{});
            return error.Parser;
        };
        const command = try self.parseCommand(gpa, iter);
        return .fromSubcommand(program_name, command);
    }

    fn parseCommand(self: @This(), gpa: Allocator, iter: anytype) Error!self.Subcommand() {
        // Validate types
        if (self.name) |name| comptime validateLongName(name);
        inline for (self.named_args) |arg| {
            comptime validateLongName(arg.long);
            if (arg.short) |short| comptime validateShortName(short);
        }

        // Initialize result with all defaults set
        const ParsedCommand = self.Subcommand();
        var result: ParsedCommand = undefined;
        result.subcommand = null;
        inline for (self.named_args) |arg| {
            if (arg.default) |default| {
                @field(result.named, arg.long) = @as(
                    *const arg.type,
                    @ptrCast(@alignCast(default)),
                ).*;
            }
        }

        // Parse the arguments
        const NamedArgEnum = FieldEnum(@FieldType(ParsedCommand, "named"));
        var named_args: std.EnumSet(NamedArgEnum) = .{};
        inline for (self.named_args) |named_arg| {
            if (named_arg.default != null or named_arg.accum) {
                named_args.insert(std.meta.stringToEnum(NamedArgEnum, named_arg.long).?);
            }
            if (named_arg.accum) {
                @field(result.named, named_arg.long) = .init(gpa);
            }
        }

        // Parse the named arguments
        var peeked: ?[:0]const u8 = while (iter.*.next()) |arg_str| {
            // Stop parsing if we find a positional argument
            if (arg_str[0] != '-') {
                break arg_str;
            }

            // Check for help
            try self.checkHelp(arg_str);

            // Parse the argument name
            const arg_name = if (arg_str.len > 2 and arg_str[1] == '-')
                arg_str[2..]
            else
                arg_str[1..];
            const negated = std.mem.startsWith(u8, arg_name, "no-");
            const lookup = if (negated) arg_name[3..] else arg_name;

            // Look for this argument in the list of fields
            const field_enum = b: {
                if (lookup.len == 1) {
                    break :b getShortArgs(self).get(lookup);
                } else {
                    break :b std.meta.stringToEnum(NamedArgEnum, lookup);
                }
            } orelse {
                log.err("unexpected argument \"{s}\"", .{arg_name});
                self.usageBrief();
                return error.Parser;
            };
            if (std.meta.fields(NamedArgEnum).len == 0) unreachable;

            // Mark this argument as found
            named_args.insert(field_enum);

            // Parse the argument value
            switch (field_enum) {
                inline else => |field_enum_inline| {
                    const field = @typeInfo(@FieldType(ParsedCommand, "named")).@"struct"
                        .fields[@intFromEnum(field_enum_inline)];
                    if (negated) {
                        if (field.type == bool) {
                            @field(result.named, field.name) = false;
                        } else if (@typeInfo(field.type) == .optional) {
                            @field(result.named, field.name) = null;
                        } else if (comptime self.argIsAccum(@intFromEnum(field_enum_inline))) {
                            @field(result.named, field.name).clearRetainingCapacity();
                        } else {
                            log.err("unexpected argument \"{s}\"", .{arg_name});
                            self.usageBrief();
                            return error.Parser;
                        }
                    } else {
                        var peeked: ?[:0]const u8 = null;
                        if (comptime self.argIsAccum(@intFromEnum(field_enum_inline))) {
                            const Items = @TypeOf(@field(result.named, field.name).items);
                            const Item = @typeInfo(Items).pointer.child;
                            try @field(result.named, field.name).append(try self.parseValue(
                                Item,
                                field.name,
                                iter,
                                &peeked,
                            ));
                        } else {
                            @field(result.named, field.name) = try self.parseValue(
                                field.type,
                                field.name,
                                iter,
                                &peeked,
                            );
                        }
                    }
                },
            }
        } else null;

        // Parse positional arguments
        inline for (self.positional_args) |field| {
            const value = try self.parseValue(
                field.type,
                field.meta,
                iter,
                &peeked,
            );
            @field(result.positional, field.meta) = value;
        }

        // Make sure all non optional args were found
        for (std.meta.tags(NamedArgEnum)) |arg| {
            if (!named_args.contains(arg)) {
                log.err("missing required argument \"{t}\"", .{arg});
                self.usageBrief();
                return error.Parser;
            }
        }

        // Parse the command, if any
        if (peeked orelse iter.*.next()) |next| b: {
            // Check for help
            try self.checkHelp(next);

            // Check if it matches a command
            if (self.subcommands.len > 0) {
                const Commands = @typeInfo(@FieldType(self.Subcommand(), "subcommand"))
                    .optional.child;
                if (std.meta.stringToEnum(FieldEnum(Commands), next)) |command_enum| {
                    switch (command_enum) {
                        inline else => |cmd| {
                            const parsed_command = try self.subcommands[@intFromEnum(cmd)]
                                .parseCommand(gpa, iter);
                            const Current = @typeInfo(@FieldType(self.Subcommand(), "subcommand"))
                                .optional.child;
                            result.subcommand = @unionInit(Current, @tagName(cmd), parsed_command);
                            break :b;
                        },
                    }
                }
            }

            // Emit an error
            log.err("unexpected command \"{s}\"", .{next});
            self.usageBrief();
            return error.Parser;
        }

        return result;
    }

    fn argIsAccum(self: @This(), arg: usize) bool {
        if (arg >= self.named_args.len) return false;
        return self.named_args[arg].accum;
    }

    fn checkHelp(self: @This(), arg_str: []const u8) error{Help}!void {
        if (std.mem.eql(u8, arg_str, "-h") or std.mem.eql(u8, arg_str, "--help")) {
            self.usage();
            return error.Help;
        }
    }

    fn getShortArgs(
        comptime self: @This(),
    ) std.StaticStringMap(FieldEnum(@FieldType(self.Subcommand(), "named"))) {
        const ArgEnum = FieldEnum(@FieldType(self.Subcommand(), "named"));
        const max_len = self.named_args.len;
        comptime var short_args: [max_len]struct { []const u8, ArgEnum } = undefined;
        comptime var len = 0;
        inline for (self.named_args) |arg| {
            if (arg.short) |short| {
                short_args[len] = .{ &.{short}, @field(ArgEnum, arg.long) };
                len += 1;
            }
        }
        return std.StaticStringMap(ArgEnum).initComptime(short_args[0..len]);
    }

    fn parseValue(
        self: @This(),
        Type: type,
        comptime arg_str: []const u8,
        iter: anytype,
        peeked: *?[:0]const u8,
    ) Error!Type {
        // If we're optional, get the inner type
        const Inner = switch (@typeInfo(Type)) {
            .optional => |optional| optional.child,
            else => Type,
        };

        // Parse the type
        switch (@typeInfo(Inner)) {
            .bool => return true,
            .@"enum" => {
                const value_str = try self.parseValueStr(arg_str, iter, peeked);
                return std.meta.stringToEnum(Inner, value_str) orelse {
                    log.err("{s}: unexpected value \"{s}\"", .{
                        arg_str,
                        value_str,
                    });
                    self.usageBrief();
                    return error.Parser;
                };
            },
            .int => {
                const value_str = try self.parseValueStr(arg_str, iter, peeked);
                return std.fmt.parseInt(Inner, value_str, 0) catch {
                    log.err("{s}: expected {}, found \"{s}\"", .{
                        arg_str,
                        Inner,
                        value_str,
                    });
                    self.usageBrief();
                    return error.Parser;
                };
            },
            .float => {
                const value_str = try self.parseValueStr(arg_str, iter, peeked);
                return std.fmt.parseFloat(Inner, value_str) catch {
                    log.err("{s}: expected {}, found \"{s}\"", .{
                        arg_str,
                        Inner,
                        value_str,
                    });
                    self.usageBrief();
                    return error.Parser;
                };
            },
            .pointer => |pointer| {
                if (pointer.child != u8 or !pointer.is_const) {
                    unsupportedArgumentType(arg_str, Type);
                }
                return try self.parseValueStr(arg_str, iter, peeked);
            },
            else => unsupportedArgumentType(arg_str, Type),
        }
    }

    fn parseValueStr(
        self: @This(),
        arg_str: []const u8,
        iter: anytype,
        peeked: *?[:0]const u8,
    ) Error![:0]const u8 {
        const value_str = peeked.* orelse iter.*.next() orelse {
            log.err("{s}: expected a value", .{arg_str});
            self.usageBrief();
            return error.Parser;
        };

        if (value_str[0] == '-') {
            // Check for help
            try self.checkHelp(value_str);

            // Emit an error
            log.err("{s}: expected a value", .{arg_str});
            self.usageBrief();
            return error.Parser;
        }

        peeked.* = null;

        return value_str;
    }

    fn formatUsage(
        self: FormatUsageData,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        // At what column to start help
        const col = 50;

        // Brief
        try writer.writeAll("usage:");
        if (self.options.name) |name| try writer.print(" {s}", .{name});
        try writer.writeByte('\n');
        if (self.brief) {
            try writer.print("--help for more info", .{});
            return;
        }

        // Help message
        if (self.options.description) |description| {
            try writer.print("\n{s}\n", .{description});
        }

        // Named args
        if (self.options.named_args.len > 0) {
            try writer.writeAll("\noptions:\n");
            inline for (self.options.named_args) |arg| {
                try writeArg(
                    col,
                    writer,
                    false,
                    arg.long,
                    arg.short,
                    arg.type,
                    arg.description,
                    arg.accum,
                    arg.default,
                );
            }
        }

        // Positional args
        if (self.options.positional_args.len > 0) {
            try writer.writeAll("\npositional arguments:\n");
            inline for (self.options.positional_args) |arg| {
                try writeArg(
                    col,
                    writer,
                    true,
                    arg.meta,
                    null,
                    arg.type,
                    arg.description,
                    false,
                    null,
                );
            }
        }

        // Subcommands:
        if (self.options.subcommands.len > 0) {
            try writer.writeAll("\nsubcommands:\n");
            inline for (self.options.subcommands) |subcommand| {
                const sub_fmt = "  {s}";
                const sub_args = comptime .{subcommand.name.?};
                try writer.print(sub_fmt, sub_args);
                if (subcommand.description) |description| {
                    const count = std.fmt.count(sub_fmt, sub_args);
                    if (std.math.sub(usize, col, count) catch null) |padding| {
                        try writer.splatByteAll(' ', padding);
                    }
                    try writer.print("{s}\n", .{description});
                }
            }
        }
    }

    fn writeArg(
        col: usize,
        writer: anytype,
        positional: bool,
        comptime long: []const u8,
        short: ?u8,
        T: type,
        description: ?[]const u8,
        accum: bool,
        default: ?*const anyopaque,
    ) !void {
        // Get the inner type if optional
        const Inner: type = switch (@typeInfo(T)) {
            .optional => |optional| optional.child,
            else => T,
        };

        // Write the argument, and measure how many characters it took
        var count = if (short) |s| b: {
            const name_fmt = "  -{c}, --{s}{f}";
            const name_args = .{ s, long, fmtType(T) };
            try writer.print(name_fmt, name_args);
            break :b std.fmt.count(name_fmt, name_args);
        } else b: {
            const prefix = if (positional) "" else "--";
            const name_fmt = "  {s}{s}{f}";
            const name_args = .{ prefix, long, fmtType(T) };
            try writer.print(name_fmt, name_args);
            break :b std.fmt.count(name_fmt, name_args);
        };

        if (default) |untyped| {
            const typed: *const T = @ptrCast(@alignCast(untyped));
            const default_fmt = if (Inner == []const u8 or Inner == [:0]const u8 or @typeInfo(Inner) == .@"enum") b: {
                break :b " (={s})";
            } else b: {
                break :b " (={any})";
            };
            const default_args = if (@typeInfo(Inner) == .@"enum") b: {
                if (Inner != T) {
                    if (typed.*) |some| {
                        break :b .{@tagName(some)};
                    } else {
                        break :b .{"null"};
                    }
                } else {
                    break :b .{@tagName(typed.*)};
                }
            } else if (Inner != T and (Inner == []const u8 or Inner == [:0]const u8)) b: {
                if (typed.*) |some| {
                    break :b .{some};
                } else {
                    break :b .{"null"};
                }
            } else b: {
                break :b .{typed.*};
            };
            count += std.fmt.count(default_fmt, default_args);
            try writer.print(default_fmt, default_args);
        }

        if (accum) {
            const str = " (accum)";
            count += str.len;
            try writer.print(str, .{});
        }

        // Write the help message offset by the correct number of characters
        if (description) |desc| {
            if (std.math.sub(usize, col, count) catch null) |padding| {
                try writer.splatByteAll(' ', padding);
            }
            try writer.writeAll(desc);
        }
        try writer.writeByte('\n');

        // If we're an enum, list all the options
        if (@typeInfo(Inner) == .@"enum") {
            for (std.meta.fieldNames(Inner)) |name| {
                try writer.splatByteAll(' ', if (positional) 4 else 6);
                try writer.print("{s}\n", .{name});
            }
        }

        // If we're optional, a boolean, or a list, display the "no-" variant
        if (Inner != T or Inner == bool or accum) {
            try writer.print("  --no-{s}\n", .{long});
        }
    }

    const FormatUsageData = struct {
        options: Self,
        brief: bool,
    };

    fn fmtUsage(self: @This(), brief: bool) std.fmt.Alt(FormatUsageData, formatUsage) {
        return .{ .data = .{ .options = self, .brief = brief } };
    }

    fn FormatType(T: type) type {
        return struct {
            pub fn format(_: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
                const Inner = switch (@typeInfo(T)) {
                    .optional => |optional| optional.child,
                    else => T,
                };
                const optional = if (Inner != T) "?" else "";
                switch (@typeInfo(Inner)) {
                    .bool => {},
                    .int, .float => try writer.print(" <{s}{s}>", .{ optional, @typeName(Inner) }),
                    .@"enum" => if (optional.len > 0) try writer.print(" {s}", .{optional}),
                    .pointer => try writer.print(" <{s}string>", .{optional}),
                    else => unreachable,
                }
            }
        };
    }

    fn fmtType(T: type) FormatType(T) {
        return .{};
    }
};

pub const PositionalArg = struct {
    meta: [:0]const u8,
    description: ?[:0]const u8,
    type: type,

    pub const Options = struct {
        meta: [:0]const u8,
        description: ?[:0]const u8 = null,
    };

    pub fn init(Type: type, options: Options) @This() {
        // The user could do this directly, we're just maintaining syntactic similarity with named
        // args for copy paste convenience.
        return .{
            .meta = options.meta,
            .description = options.description,
            .type = Type,
        };
    }
};

pub const NamedArg = struct {
    long: [:0]const u8,
    short: ?u8 = null,
    description: ?[:0]const u8,
    type: type,
    default: ?*const anyopaque,
    accum: bool = false,

    pub fn Options(Type: type) type {
        return struct {
            long: [:0]const u8,
            short: ?u8 = null,
            description: ?[:0]const u8 = null,
            required: bool = false,
            default: union(enum) {
                required: void,
                value: Type,
            } = .required,
            accum: bool = false,
        };
    }

    pub const AccumOptions = struct {
        long: [:0]const u8,
        short: ?u8 = null,
        description: ?[:0]const u8 = null,
    };

    pub fn init(Type: type, options: Options(Type)) @This() {
        // The user could do this directly, but it is tricky to set default correctly--not only is
        // the address of and pointer cast necessary giving up type safety, null is a valid default
        // value, which is syntactically tricky to get right! This wrapper makes it easy.
        return .{
            .long = options.long,
            .short = options.short,
            .description = options.description,
            .type = Type,
            .default = switch (options.default) {
                .required => null,
                .value => |v| @ptrCast(&v),
            },
            .accum = false,
        };
    }

    pub fn initAccum(Type: type, options: AccumOptions) @This() {
        // For now, we don't support the required or default options on accumulation arguments. This
        // can be added later if there's a use for them.
        return .{
            .long = options.long,
            .short = options.short,
            .description = options.description,
            .type = Type,
            .accum = true,
            .default = null,
        };
    }
};

fn validateLongName(comptime name: []const u8) void {
    if (name.len < 2) {
        // This prevents collisions with short names, and makes parsing a little simpler
        @compileError("long names must be at least two characters");
    }
    if (name[0] == '-') {
        @compileError("argument may not start with '-': " ++ name);
    }
    if (std.mem.startsWith(u8, name, "no-")) {
        @compileError("argument may not start with \"no-\": " ++ name);
    }
    if (std.mem.eql(u8, name, "help")) {
        @compileError("use of reserved argument name: " ++ name);
    }
    inline for (name) |c| {
        switch (c) {
            'a'...'z', 'A'...'Z', '0'...'9', '-', '_' => {},
            else => @compileError(
                "unsupported character '" ++ @as([1]u8, .{c}) ++ "' in argument: " ++ name,
            ),
        }
    }
}

fn validateShortName(comptime c: u8) void {
    switch (c) {
        'a'...'z', 'A'...'Z', '0'...'9' => {},
        else => @compileError(
            "unsupported character '" ++ @as([1]u8, .{c}) ++ "' as short name",
        ),
    }
}

fn unsupportedArgumentType(comptime arg_str: []const u8, ty: type) noreturn {
    @compileError(arg_str ++ ": unsupported argument type " ++ @typeName(ty));
}

test "all types nullable required" {
    const Enum = enum { foo, bar };
    const options: Command = .{
        .name = "command-name",
        .named_args = &.{
            NamedArg.init(bool, .{
                .long = "bool",
            }),
            NamedArg.init(?u32, .{
                .long = "u32",
            }),
            NamedArg.init(?Enum, .{
                .long = "enum",
            }),
            NamedArg.init(?[]const u8, .{
                .long = "string",
            }),
            NamedArg.init(?f32, .{
                .long = "f32",
            }),
        },
        .positional_args = &.{
            PositionalArg.init(u8, .{
                .meta = "U8",
            }),
            PositionalArg.init([]const u8, .{
                .meta = "STRING",
            }),
            PositionalArg.init(Enum, .{
                .meta = "ENUM",
            }),
            PositionalArg.init(f32, .{
                .meta = "F32",
            }),
        },
    };

    const Result = options.Result();
    const undef: Result = undefined;
    try expectEqual(5, std.meta.fields(@FieldType(Result, "named")).len);
    try expectEqual(4, std.meta.fields(@FieldType(Result, "positional")).len);
    try expect(@FieldType(Result, "subcommand") == ?void);
    try expectEqual(bool, @TypeOf(undef.named.bool));
    try expectEqual(?u32, @TypeOf(undef.named.u32));
    try expectEqual(?Enum, @TypeOf(undef.named.@"enum"));
    try expectEqual(?[]const u8, @TypeOf(undef.named.string));
    try expectEqual(?f32, @TypeOf(undef.named.f32));
    try expectEqual(u8, @TypeOf(undef.positional.U8));
    try expectEqual([]const u8, @TypeOf(undef.positional.STRING));
    try expectEqual(Enum, @TypeOf(undef.positional.ENUM));
    try expectEqual(f32, @TypeOf(undef.positional.F32));

    // All set
    try expectEqual(Result{
        .program_name = "path",
        .named = .{
            .bool = true,
            .u32 = 123,
            .@"enum" = .bar,
            .string = "world",
            .f32 = 1.5,
        },
        .positional = .{
            .U8 = 1,
            .STRING = "hello",
            .ENUM = .foo,
            .F32 = 2.5,
        },
        .subcommand = null,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--u32",
        "123",
        "--enum",
        "bar",
        "--string",
        "world",
        "--bool",
        "--f32",
        "1.5",

        "1",
        "hello",
        "foo",
        "2.5",
    }));

    // All null
    try expectEqual(Result{
        .program_name = "path1",
        .named = .{
            .bool = false,
            .u32 = null,
            .@"enum" = null,
            .string = null,
            .f32 = null,
        },
        .positional = .{
            .U8 = 1,
            .STRING = "hello",
            .ENUM = .foo,
            .F32 = 10.1,
        },
        .subcommand = null,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path1",

        "--no-u32",
        "--no-enum",
        "--no-string",
        "--no-bool",
        "--no-f32",

        "1",
        "hello",
        "foo",
        "10.1",
    }));
}

test "all types defaults" {
    const Enum = enum { foo, bar };
    const options: Command = .{
        .name = "command-name",
        .named_args = &.{
            NamedArg.init(bool, .{
                .long = "bool",
                .default = .{ .value = true },
            }),
            NamedArg.init(u32, .{
                .long = "u32",
                .default = .{ .value = 123 },
            }),
            NamedArg.init(Enum, .{
                .long = "enum",
                .default = .{ .value = .bar },
            }),
            NamedArg.init([]const u8, .{
                .long = "string",
                .default = .{ .value = "default" },
            }),
            NamedArg.init(f32, .{
                .long = "f32",
                .default = .{ .value = 123.456 },
            }),
        },
        .positional_args = &.{
            PositionalArg.init(u8, .{
                .meta = "U8",
            }),
            PositionalArg.init([]const u8, .{
                .meta = "STRING",
            }),
            PositionalArg.init(Enum, .{
                .meta = "ENUM",
            }),
            PositionalArg.init(f32, .{
                .meta = "F32",
            }),
        },
    };

    const Result = options.Result();
    const undef: Result = undefined;
    try expectEqual(5, std.meta.fields(@FieldType(Result, "named")).len);
    try expectEqual(4, std.meta.fields(@FieldType(Result, "positional")).len);
    try expect(@FieldType(Result, "subcommand") == ?void);
    try expectEqual(bool, @TypeOf(undef.named.bool));
    try expectEqual(u32, @TypeOf(undef.named.u32));
    try expectEqual(Enum, @TypeOf(undef.named.@"enum"));
    try expectEqual([]const u8, @TypeOf(undef.named.string));
    try expectEqual(f32, @TypeOf(undef.named.f32));
    try expectEqual(u8, @TypeOf(undef.positional.U8));
    try expectEqual([]const u8, @TypeOf(undef.positional.STRING));
    try expectEqual(Enum, @TypeOf(undef.positional.ENUM));
    try expectEqual(f32, @TypeOf(undef.positional.F32));

    // All set
    try expectEqual(Result{
        .program_name = "path",
        .named = .{
            .bool = true,
            .u32 = 123,
            .@"enum" = .bar,
            .string = "world",
            .f32 = 1.5,
        },
        .positional = .{
            .U8 = 1,
            .STRING = "hello",
            .ENUM = .foo,
            .F32 = 2.5,
        },
        .subcommand = null,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--u32",
        "123",
        "--enum",
        "bar",
        "--string",
        "world",
        "--bool",
        "--f32",
        "1.5",

        "1",
        "hello",
        "foo",
        "2.5",
    }));

    // All skipped
    try expectEqual(Result{
        .program_name = "path",
        .named = .{
            .bool = true,
            .u32 = 123,
            .@"enum" = .bar,
            .string = "default",
            .f32 = 123.456,
        },
        .positional = .{
            .U8 = 1,
            .STRING = "hello",
            .ENUM = .foo,
            .F32 = 5.5,
        },
        .subcommand = null,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "1",
        "hello",
        "foo",
        "5.5",
    }));
}

test "all types defaults and nullable but not null" {
    const Enum = enum { foo, bar };
    const options: Command = .{
        .name = "command-name",
        .named_args = &.{
            NamedArg.init(bool, .{
                .long = "bool",
                .default = .{ .value = true },
            }),
            NamedArg.init(?u32, .{
                .long = "u32",
                .default = .{ .value = 123 },
            }),
            NamedArg.init(?Enum, .{
                .long = "enum",
                .default = .{ .value = .bar },
            }),
            NamedArg.init(?[]const u8, .{
                .long = "string",
                .default = .{ .value = "default" },
            }),
            NamedArg.init(?f32, .{
                .long = "f32",
                .default = .{ .value = 123.456 },
            }),
        },
        .positional_args = &.{
            PositionalArg.init(u8, .{
                .meta = "U8",
            }),
            PositionalArg.init([]const u8, .{
                .meta = "STRING",
            }),
            PositionalArg.init(Enum, .{
                .meta = "ENUM",
            }),
            PositionalArg.init(f32, .{
                .meta = "F32",
            }),
        },
    };

    const Result = options.Result();
    const undef: Result = undefined;
    try expectEqual(5, std.meta.fields(@FieldType(Result, "named")).len);
    try expectEqual(4, std.meta.fields(@FieldType(Result, "positional")).len);
    try expect(@FieldType(Result, "subcommand") == ?void);
    try expectEqual(bool, @TypeOf(undef.named.bool));
    try expectEqual(?u32, @TypeOf(undef.named.u32));
    try expectEqual(?Enum, @TypeOf(undef.named.@"enum"));
    try expectEqual(?[]const u8, @TypeOf(undef.named.string));
    try expectEqual(?f32, @TypeOf(undef.named.f32));
    try expectEqual(u8, @TypeOf(undef.positional.U8));
    try expectEqual([]const u8, @TypeOf(undef.positional.STRING));
    try expectEqual(Enum, @TypeOf(undef.positional.ENUM));
    try expectEqual(f32, @TypeOf(undef.positional.F32));

    // All set
    try expectEqual(Result{
        .program_name = "path",
        .named = .{
            .bool = true,
            .u32 = 123,
            .@"enum" = .bar,
            .string = "world",
            .f32 = 1.5,
        },
        .positional = .{
            .U8 = 1,
            .STRING = "hello",
            .ENUM = .foo,
            .F32 = 2.5,
        },
        .subcommand = null,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--u32",
        "123",
        "--enum",
        "bar",
        "--string",
        "world",
        "--bool",
        "--f32",
        "1.5",

        "1",
        "hello",
        "foo",
        "2.5",
    }));

    // All skipped
    try expectEqual(Result{
        .program_name = "path",
        .named = .{
            .bool = true,
            .u32 = 123,
            .@"enum" = .bar,
            .string = "default",
            .f32 = 123.456,
        },
        .positional = .{
            .U8 = 1,
            .STRING = "hello",
            .ENUM = .foo,
            .F32 = 2.5,
        },
        .subcommand = null,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "1",
        "hello",
        "foo",
        "2.5",
    }));

    // All null
    try expectEqual(Result{
        .program_name = "path",
        .named = .{
            .bool = false,
            .u32 = null,
            .@"enum" = null,
            .string = null,
            .f32 = null,
        },
        .positional = .{
            .U8 = 1,
            .STRING = "hello",
            .ENUM = .foo,
            .F32 = 2.5,
        },
        .subcommand = null,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--no-u32",
        "--no-enum",
        "--no-string",
        "--no-bool",
        "--no-f32",

        "1",
        "hello",
        "foo",
        "2.5",
    }));
}

test "all types defaults and nullable and null" {
    const Enum = enum { foo, bar };
    const options: Command = .{
        .name = "command-name",
        .named_args = &.{
            NamedArg.init(bool, .{
                .long = "bool",
                .default = .{ .value = false },
            }),
            NamedArg.init(?u32, .{
                .long = "u32",
                .default = .{ .value = null },
            }),
            NamedArg.init(?Enum, .{
                .long = "enum",
                .default = .{ .value = null },
            }),
            NamedArg.init(?[]const u8, .{
                .long = "string",
                .default = .{ .value = null },
            }),
            NamedArg.init(?f32, .{
                .long = "f32",
                .default = .{ .value = null },
            }),
        },
        .positional_args = &.{
            PositionalArg.init(u8, .{
                .meta = "U8",
            }),
            PositionalArg.init([]const u8, .{
                .meta = "STRING",
            }),
            PositionalArg.init(Enum, .{
                .meta = "ENUM",
            }),
            PositionalArg.init(f32, .{
                .meta = "F32",
            }),
        },
    };

    const Result = options.Result();
    const undef: Result = undefined;
    try expectEqual(5, std.meta.fields(@FieldType(Result, "named")).len);
    try expectEqual(4, std.meta.fields(@FieldType(Result, "positional")).len);
    try expect(@FieldType(Result, "subcommand") == ?void);
    try expectEqual(bool, @TypeOf(undef.named.bool));
    try expectEqual(?u32, @TypeOf(undef.named.u32));
    try expectEqual(?Enum, @TypeOf(undef.named.@"enum"));
    try expectEqual(?[]const u8, @TypeOf(undef.named.string));
    try expectEqual(?f32, @TypeOf(undef.named.f32));
    try expectEqual(u8, @TypeOf(undef.positional.U8));
    try expectEqual([]const u8, @TypeOf(undef.positional.STRING));
    try expectEqual(Enum, @TypeOf(undef.positional.ENUM));
    try expectEqual(f32, @TypeOf(undef.positional.F32));

    // All set
    try expectEqual(Result{
        .program_name = "path",
        .named = .{
            .bool = true,
            .u32 = 123,
            .@"enum" = .bar,
            .string = "world",
            .f32 = 1.5,
        },
        .positional = .{
            .U8 = 1,
            .STRING = "hello",
            .ENUM = .foo,
            .F32 = 2.5,
        },
        .subcommand = null,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--u32",
        "123",
        "--enum",
        "bar",
        "--string",
        "world",
        "--bool",
        "--f32",
        "1.5",

        "1",
        "hello",
        "foo",
        "2.5",
    }));

    // All skipped
    try expectEqual(Result{
        .program_name = "path",
        .named = .{
            .bool = false,
            .u32 = null,
            .@"enum" = null,
            .string = null,
            .f32 = null,
        },
        .positional = .{
            .U8 = 1,
            .STRING = "hello",
            .ENUM = .foo,
            .F32 = 2.5,
        },
        .subcommand = null,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "1",
        "hello",
        "foo",
        "2.5",
    }));
}

test "all types required" {
    const Enum = enum { foo, bar };
    const options: Command = .{
        .name = "command-name",
        .named_args = &.{
            NamedArg.init(bool, .{
                .long = "bool",
                .short = 'b',
            }),
            NamedArg.init(u32, .{
                .long = "u32",
                .short = 'u',
            }),
            NamedArg.init(Enum, .{
                .long = "enum",
                .short = 'e',
            }),
            NamedArg.init([]const u8, .{
                .long = "string",
                .short = 's',
            }),
            NamedArg.init(f32, .{
                .long = "f32",
                .short = 'f',
            }),
        },
        .positional_args = &.{
            PositionalArg.init(u8, .{
                .meta = "U8",
            }),
            PositionalArg.init([]const u8, .{
                .meta = "STRING",
            }),
            PositionalArg.init(Enum, .{
                .meta = "ENUM",
            }),
            PositionalArg.init(f32, .{
                .meta = "F32",
            }),
        },
    };

    const Result = options.Result();
    const undef: Result = undefined;
    try expectEqual(5, std.meta.fields(@FieldType(Result, "named")).len);
    try expectEqual(4, std.meta.fields(@FieldType(Result, "positional")).len);
    try expect(@FieldType(Result, "subcommand") == ?void);
    try expectEqual(bool, @TypeOf(undef.named.bool));
    try expectEqual(u32, @TypeOf(undef.named.u32));
    try expectEqual(Enum, @TypeOf(undef.named.@"enum"));
    try expectEqual([]const u8, @TypeOf(undef.named.string));
    try expectEqual(f32, @TypeOf(undef.named.f32));
    try expectEqual(u8, @TypeOf(undef.positional.U8));
    try expectEqual([]const u8, @TypeOf(undef.positional.STRING));
    try expectEqual(Enum, @TypeOf(undef.positional.ENUM));
    try expectEqual(f32, @TypeOf(undef.positional.F32));

    // All set
    try expectEqual(Result{
        .program_name = "path",
        .named = .{
            .bool = true,
            .u32 = 123,
            .@"enum" = .bar,
            .string = "world",
            .f32 = 1.5,
        },
        .positional = .{
            .U8 = 1,
            .STRING = "hello",
            .ENUM = .foo,
            .F32 = 2.5,
        },
        .subcommand = null,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--u32",
        "123",
        "--enum",
        "bar",
        "--string",
        "world",
        "--bool",
        "--f32",
        "1.5",

        "1",
        "hello",
        "foo",
        "2.5",
    }));

    // Repeated args
    try expectEqual(Result{
        .program_name = "path",
        .named = .{
            .bool = false,
            .u32 = 321,
            .@"enum" = .foo,
            .string = "updated",
            .f32 = 3.5,
        },
        .positional = .{
            .U8 = 1,
            .STRING = "hello",
            .ENUM = .foo,
            .F32 = 2.5,
        },
        .subcommand = null,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--u32",
        "123",
        "--enum",
        "bar",
        "--string",
        "world",
        "--bool",
        "--f32",
        "1.5",

        "--u32",
        "321",
        "--enum",
        "foo",
        "--string",
        "updated",
        "--no-bool",
        "--f32",
        "3.5",

        "1",
        "hello",
        "foo",
        "2.5",
    }));

    // Short names args
    try expectEqual(Result{
        .program_name = "path",
        .named = .{
            .bool = true,
            .u32 = 321,
            .@"enum" = .foo,
            .string = "updated",
            .f32 = 1.5,
        },
        .positional = .{
            .U8 = 1,
            .STRING = "hello",
            .ENUM = .foo,
            .F32 = 2.5,
        },
        .subcommand = null,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--u",
        "321",
        "-e",
        "foo",
        "-s",
        "updated",
        "-b",
        "-f",
        "1.5",

        "1",
        "hello",
        "foo",
        "2.5",
    }));
}

test "no args" {
    const options: Command = .{ .name = "command-name" };
    const Result = options.Result();
    try expectEqual(0, std.meta.fields(@FieldType(Result, "named")).len);
    try expectEqual(0, std.meta.fields(@FieldType(Result, "positional")).len);
    try expect(@FieldType(Result, "subcommand") == ?void);
    try expectEqual(
        Result{
            .program_name = "path",
            .named = .{},
            .positional = .{},
            .subcommand = null,
        },
        try options.parseFromSlice(std.testing.allocator, &.{"path"}),
    );
}

test "only positional" {
    const Enum = enum { foo, bar };
    const options: Command = .{
        .name = "command-name",
        .positional_args = &.{
            PositionalArg.init(u8, .{
                .meta = "U8",
            }),
            PositionalArg.init([]const u8, .{
                .meta = "STRING",
            }),
            PositionalArg.init(Enum, .{
                .meta = "ENUM",
            }),
            PositionalArg.init(f32, .{
                .meta = "F32",
            }),
        },
    };

    const Result = options.Result();
    const undef: Result = undefined;
    try expectEqual(0, std.meta.fields(@FieldType(Result, "named")).len);
    try expectEqual(4, std.meta.fields(@FieldType(Result, "positional")).len);
    try expect(@FieldType(Result, "subcommand") == ?void);
    try expectEqual(u8, @TypeOf(undef.positional.U8));
    try expectEqual([]const u8, @TypeOf(undef.positional.STRING));
    try expectEqual(Enum, @TypeOf(undef.positional.ENUM));
    try expectEqual(f32, @TypeOf(undef.positional.F32));

    // All set
    try expectEqual(Result{
        .program_name = "path",
        .named = .{},
        .positional = .{
            .U8 = 1,
            .STRING = "hello",
            .ENUM = .foo,
            .F32 = 1.5,
        },
        .subcommand = null,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "1",
        "hello",
        "foo",
        "1.5",
    }));
}

test "only named" {
    const Enum = enum { foo, bar };
    const options: Command = .{
        .name = "command-name",
        .named_args = &.{
            NamedArg.init(bool, .{
                .long = "bool",
            }),
            NamedArg.init(?u32, .{
                .long = "u32",
            }),
            NamedArg.init(?Enum, .{
                .long = "enum",
            }),
            NamedArg.init(?[]const u8, .{
                .long = "string",
            }),
            NamedArg.init(?f32, .{
                .long = "f32",
            }),
        },
    };

    const Result = options.Result();
    const undef: Result = undefined;
    try expectEqual(5, std.meta.fields(@FieldType(Result, "named")).len);
    try expectEqual(0, std.meta.fields(@FieldType(Result, "positional")).len);
    try expect(@FieldType(Result, "subcommand") == ?void);
    try expectEqual(bool, @TypeOf(undef.named.bool));
    try expectEqual(?u32, @TypeOf(undef.named.u32));
    try expectEqual(?Enum, @TypeOf(undef.named.@"enum"));
    try expectEqual(?[]const u8, @TypeOf(undef.named.string));
    try expectEqual(?f32, @TypeOf(undef.named.f32));

    // All set
    try expectEqual(Result{
        .program_name = "path",
        .named = .{
            .bool = true,
            .u32 = 123,
            .@"enum" = .bar,
            .string = "world",
            .f32 = 1.5,
        },
        .positional = .{},
        .subcommand = null,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--u32",
        "123",
        "--enum",
        "bar",
        "--string",
        "world",
        "--bool",
        "--f32",
        "1.5",
    }));
}

test "help menu" {
    const Enum = enum { foo, bar };
    const no_help: Command = .{
        .name = "command-name",
        .named_args = &.{
            NamedArg.init(bool, .{
                .long = "bool",
            }),
            NamedArg.init(?u32, .{
                .long = "u32",
            }),
            NamedArg.init(?Enum, .{
                .long = "enum",
            }),
            NamedArg.init(?[]const u8, .{
                .long = "string",
            }),
            NamedArg.init(?f32, .{
                .long = "float",
            }),
            NamedArg.initAccum([]const u8, .{
                .long = "list",
            }),
        },
        .positional_args = &.{
            PositionalArg.init(u8, .{
                .meta = "U8",
            }),
            PositionalArg.init([]const u8, .{
                .meta = "STRING",
            }),
            PositionalArg.init(Enum, .{
                .meta = "ENUM",
            }),
            PositionalArg.init(f32, .{
                .meta = "F32",
            }),
        },
    };

    const with_help: Command = .{
        .name = "command-name",
        .description = "command help",
        .named_args = &.{
            NamedArg.init(bool, .{
                .long = "bool",
                .description = "bool help",
            }),
            NamedArg.init(?u32, .{
                .long = "u32",
                .description = "u32 help",
            }),
            NamedArg.init(?Enum, .{
                .long = "enum",
                .description = "enum help",
            }),
            NamedArg.init(?[]const u8, .{
                .long = "string",
                .description = "string help",
            }),
            NamedArg.init(?f32, .{
                .long = "float",
                .description = "float help",
            }),
            NamedArg.initAccum([]const u8, .{
                .long = "list",
                .description = "string list help",
            }),
        },
        .positional_args = &.{
            PositionalArg.init(u8, .{
                .meta = "U8",
                .description = "u8 help",
            }),
            PositionalArg.init([]const u8, .{
                .meta = "STRING",
                .description = "string help",
            }),
            PositionalArg.init(Enum, .{
                .meta = "ENUM",
                .description = "enum help",
            }),
            PositionalArg.init(f32, .{
                .meta = "F32",
                .description = "f32 help",
            }),
        },
    };

    const with_help_with_defaults_optional: Command = .{
        .name = "command-name",
        .description = "command help",
        .named_args = &.{
            NamedArg.init(bool, .{
                .long = "bool",
                .description = "bool help",
                .default = .{ .value = true },
            }),
            NamedArg.init(?u32, .{
                .long = "u32",
                .description = "u32 help",
                .default = .{ .value = 10 },
            }),
            NamedArg.init(?Enum, .{
                .long = "enum",
                .description = "enum help",
                .default = .{ .value = .foo },
            }),
            NamedArg.init(?[]const u8, .{
                .long = "string",
                .description = "string help",
                .default = .{ .value = "foo" },
            }),
            NamedArg.init(?f32, .{
                .long = "float",
                .description = "float help",
                .default = .{ .value = 1.5 },
            }),
            NamedArg.initAccum([]const u8, .{
                .long = "list",
                .description = "string list help",
            }),
        },
        .positional_args = &.{
            PositionalArg.init(u8, .{
                .meta = "U8",
                .description = "u8 help",
            }),
            PositionalArg.init([]const u8, .{
                .meta = "STRING",
                .description = "string help",
            }),
            PositionalArg.init(Enum, .{
                .meta = "ENUM",
                .description = "enum help",
            }),
            PositionalArg.init(f32, .{
                .meta = "F32",
                .description = "f32 help",
            }),
        },
    };

    const with_help_with_defaults_optional_null: Command = .{
        .name = "command-name",
        .description = "command help",
        .named_args = &.{
            NamedArg.init(bool, .{
                .long = "bool",
                .description = "bool help",
                .default = .{ .value = true },
            }),
            NamedArg.init(?u32, .{
                .long = "u32",
                .description = "u32 help",
                .default = .{ .value = null },
            }),
            NamedArg.init(?Enum, .{
                .long = "enum",
                .description = "enum help",
                .default = .{ .value = null },
            }),
            NamedArg.init(?[]const u8, .{
                .long = "string",
                .description = "string help",
                .default = .{ .value = null },
            }),
            NamedArg.init(?f32, .{
                .long = "float",
                .description = "float help",
                .default = .{ .value = null },
            }),
            NamedArg.initAccum([]const u8, .{
                .long = "list",
                .description = "string list help",
            }),
        },
        .positional_args = &.{
            PositionalArg.init(u8, .{
                .meta = "U8",
                .description = "u8 help",
            }),
            PositionalArg.init([]const u8, .{
                .meta = "STRING",
                .description = "string help",
            }),
            PositionalArg.init(Enum, .{
                .meta = "ENUM",
                .description = "enum help",
            }),
            PositionalArg.init(f32, .{
                .meta = "F32",
                .description = "f32 help",
            }),
        },
    };

    const with_help_with_defaults: Command = .{
        .name = "command-name",
        .description = "command help",
        .named_args = &.{
            NamedArg.init(bool, .{
                .long = "bool",
                .description = "bool help",
                .default = .{ .value = true },
            }),
            NamedArg.init(?u32, .{
                .long = "u32",
                .description = "u32 help",
                .default = .{ .value = 10 },
            }),
            NamedArg.init(?Enum, .{
                .long = "enum",
                .description = "enum help",
                .default = .{ .value = .foo },
            }),
            NamedArg.init(?[]const u8, .{
                .long = "string",
                .description = "string help",
                .default = .{ .value = "foo" },
            }),
            NamedArg.init(?f32, .{
                .long = "float",
                .description = "float help",
                .default = .{ .value = 1.5 },
            }),
            NamedArg.initAccum([]const u8, .{
                .long = "list",
                .description = "string list help",
            }),
        },
        .positional_args = &.{
            PositionalArg.init(u8, .{
                .meta = "U8",
                .description = "u8 help",
            }),
            PositionalArg.init([]const u8, .{
                .meta = "STRING",
                .description = "string help",
            }),
            PositionalArg.init(Enum, .{
                .meta = "ENUM",
                .description = "enum help",
            }),
            PositionalArg.init(f32, .{
                .meta = "F32",
                .description = "f32 help",
            }),
        },
    };

    const with_subcommand: Command = .{
        .name = null,
        .description = "command help",
        .named_args = &.{
            NamedArg.init(bool, .{
                .long = "bool",
                .description = "bool help",
                .default = .{ .value = true },
            }),
        },
        .positional_args = &.{
            PositionalArg.init(u8, .{
                .meta = "U8",
                .description = "u8 help",
            }),
        },
        .subcommands = &.{.{
            .name = "subcommand",
            .description = "subcommand help",
            .named_args = &.{
                NamedArg.init(bool, .{
                    .long = "bool2",
                    .description = "bool2 help",
                    .default = .{ .value = true },
                }),
            },
        }},
    };

    {
        const found = try std.fmt.allocPrint(
            std.testing.allocator,
            "{f}",
            .{no_help.fmtUsage(true)},
        );
        defer std.testing.allocator.free(found);
        try expectEqualStrings(
            \\usage: command-name
            \\--help for more info
        , found);
    }

    {
        const found = try std.fmt.allocPrint(
            std.testing.allocator,
            "{f}",
            .{with_help.fmtUsage(true)},
        );
        defer std.testing.allocator.free(found);
        try expectEqualStrings(
            \\usage: command-name
            \\--help for more info
        , found);
    }

    {
        const found = try std.fmt.allocPrint(
            std.testing.allocator,
            "{f}",
            .{no_help.fmtUsage(false)},
        );
        defer std.testing.allocator.free(found);
        try expectEqualStrings(
            \\usage: command-name
            \\
            \\options:
            \\  --bool
            \\  --no-bool
            \\  --u32 <?u32>
            \\  --no-u32
            \\  --enum ?
            \\      foo
            \\      bar
            \\  --no-enum
            \\  --string <?string>
            \\  --no-string
            \\  --float <?f32>
            \\  --no-float
            \\  --list <string> (accum)
            \\  --no-list
            \\
            \\positional arguments:
            \\  U8 <u8>
            \\  STRING <string>
            \\  ENUM
            \\    foo
            \\    bar
            \\  F32 <f32>
            \\
        , found);
    }

    {
        const found = try std.fmt.allocPrint(
            std.testing.allocator,
            "{f}",
            .{with_help.fmtUsage(false)},
        );
        defer std.testing.allocator.free(found);
        try expectEqualStrings(
            \\usage: command-name
            \\
            \\command help
            \\
            \\options:
            \\  --bool                                          bool help
            \\  --no-bool
            \\  --u32 <?u32>                                    u32 help
            \\  --no-u32
            \\  --enum ?                                        enum help
            \\      foo
            \\      bar
            \\  --no-enum
            \\  --string <?string>                              string help
            \\  --no-string
            \\  --float <?f32>                                  float help
            \\  --no-float
            \\  --list <string> (accum)                         string list help
            \\  --no-list
            \\
            \\positional arguments:
            \\  U8 <u8>                                         u8 help
            \\  STRING <string>                                 string help
            \\  ENUM                                            enum help
            \\    foo
            \\    bar
            \\  F32 <f32>                                       f32 help
            \\
        , found);
    }

    {
        const found = try std.fmt.allocPrint(
            std.testing.allocator,
            "{f}",
            .{with_help_with_defaults_optional.fmtUsage(false)},
        );
        defer std.testing.allocator.free(found);
        try expectEqualStrings(
            \\usage: command-name
            \\
            \\command help
            \\
            \\options:
            \\  --bool (=true)                                  bool help
            \\  --no-bool
            \\  --u32 <?u32> (=10)                              u32 help
            \\  --no-u32
            \\  --enum ? (=foo)                                 enum help
            \\      foo
            \\      bar
            \\  --no-enum
            \\  --string <?string> (=foo)                       string help
            \\  --no-string
            \\  --float <?f32> (=1.5)                           float help
            \\  --no-float
            \\  --list <string> (accum)                         string list help
            \\  --no-list
            \\
            \\positional arguments:
            \\  U8 <u8>                                         u8 help
            \\  STRING <string>                                 string help
            \\  ENUM                                            enum help
            \\    foo
            \\    bar
            \\  F32 <f32>                                       f32 help
            \\
        , found);
    }

    {
        const found = try std.fmt.allocPrint(
            std.testing.allocator,
            "{f}",
            .{with_help_with_defaults_optional_null.fmtUsage(false)},
        );
        defer std.testing.allocator.free(found);
        try expectEqualStrings(
            \\usage: command-name
            \\
            \\command help
            \\
            \\options:
            \\  --bool (=true)                                  bool help
            \\  --no-bool
            \\  --u32 <?u32> (=null)                            u32 help
            \\  --no-u32
            \\  --enum ? (=null)                                enum help
            \\      foo
            \\      bar
            \\  --no-enum
            \\  --string <?string> (=null)                      string help
            \\  --no-string
            \\  --float <?f32> (=null)                          float help
            \\  --no-float
            \\  --list <string> (accum)                         string list help
            \\  --no-list
            \\
            \\positional arguments:
            \\  U8 <u8>                                         u8 help
            \\  STRING <string>                                 string help
            \\  ENUM                                            enum help
            \\    foo
            \\    bar
            \\  F32 <f32>                                       f32 help
            \\
        , found);
    }

    {
        const found = try std.fmt.allocPrint(
            std.testing.allocator,
            "{f}",
            .{with_help_with_defaults.fmtUsage(false)},
        );
        defer std.testing.allocator.free(found);
        try expectEqualStrings(
            \\usage: command-name
            \\
            \\command help
            \\
            \\options:
            \\  --bool (=true)                                  bool help
            \\  --no-bool
            \\  --u32 <?u32> (=10)                              u32 help
            \\  --no-u32
            \\  --enum ? (=foo)                                 enum help
            \\      foo
            \\      bar
            \\  --no-enum
            \\  --string <?string> (=foo)                       string help
            \\  --no-string
            \\  --float <?f32> (=1.5)                           float help
            \\  --no-float
            \\  --list <string> (accum)                         string list help
            \\  --no-list
            \\
            \\positional arguments:
            \\  U8 <u8>                                         u8 help
            \\  STRING <string>                                 string help
            \\  ENUM                                            enum help
            \\    foo
            \\    bar
            \\  F32 <f32>                                       f32 help
            \\
        , found);
    }

    {
        const found = try std.fmt.allocPrint(
            std.testing.allocator,
            "{f}",
            .{with_subcommand.fmtUsage(false)},
        );
        defer std.testing.allocator.free(found);
        try expectEqualStrings(
            \\usage:
            \\
            \\command help
            \\
            \\options:
            \\  --bool (=true)                                  bool help
            \\  --no-bool
            \\
            \\positional arguments:
            \\  U8 <u8>                                         u8 help
            \\
            \\subcommands:
            \\  subcommand                                      subcommand help
            \\
        , found);
    }
}

test "help argument" {
    const options: Command = .{
        .name = "command-name",
        .named_args = &.{
            NamedArg.init([]const u8, .{
                .long = "named-1",
            }),
            NamedArg.init([]const u8, .{
                .long = "named-2",
            }),
        },
        .positional_args = &.{
            PositionalArg.init([]const u8, .{
                .meta = "POS-1",
            }),
            PositionalArg.init([]const u8, .{
                .meta = "POS-2",
            }),
        },
    };

    try expectError(error.Help, options.parseFromSlice(std.testing.allocator, &.{
        "path",
        "--help",
    }));

    try expectError(error.Help, options.parseFromSlice(std.testing.allocator, &.{
        "path",
        "-h",
    }));

    try expectError(error.Help, options.parseFromSlice(std.testing.allocator, &.{
        "path",
        "--named-1",
        "--help",
    }));

    try expectError(error.Help, options.parseFromSlice(std.testing.allocator, &.{
        "path",
        "--named-1",
        "-h",
    }));

    try expectError(error.Help, options.parseFromSlice(std.testing.allocator, &.{
        "path",
        "--named-1",
        "foo",
        "--named-2",
        "--help",
    }));

    try expectError(error.Help, options.parseFromSlice(std.testing.allocator, &.{
        "path",
        "--named-1",
        "foo",
        "--named-2",
        "-h",
    }));

    try expectError(error.Help, options.parseFromSlice(std.testing.allocator, &.{
        "path",
        "--named-1",
        "foo",
        "--named-2",
        "bar",
        "--help",
    }));

    try expectError(error.Help, options.parseFromSlice(std.testing.allocator, &.{
        "path",
        "--named-1",
        "foo",
        "--named-2",
        "bar",
        "-h",
    }));

    try expectError(error.Help, options.parseFromSlice(std.testing.allocator, &.{
        "path",
        "--named-1",
        "foo",
        "--named-2",
        "bar",
        "baz",
        "--help",
    }));

    try expectError(error.Help, options.parseFromSlice(std.testing.allocator, &.{
        "path",
        "--named-1",
        "foo",
        "--named-2",
        "bar",
        "baz",
        "-h",
    }));

    try expectError(error.Help, options.parseFromSlice(std.testing.allocator, &.{
        "path",
        "--named-1",
        "foo",
        "--named-2",
        "bar",
        "baz",
        "qux",
        "--help",
    }));

    try expectError(error.Help, options.parseFromSlice(std.testing.allocator, &.{
        "path",
        "--named-1",
        "foo",
        "--named-2",
        "bar",
        "baz",
        "qux",
        "-h",
    }));
}

test "default field values" {
    const options: Command = .{
        .name = "command-name",
        .named_args = &.{
            NamedArg.init(?u8, .{
                .long = "named-1",
                .default = .{ .value = null },
            }),
            NamedArg.init(?u8, .{
                .long = "named-2",
                .default = .{ .value = 10 },
            }),
            NamedArg.init(?u8, .{
                .long = "named-3",
                .default = .required,
            }),
            NamedArg.init(u8, .{
                .long = "named-4",
                .default = .{ .value = 10 },
            }),
            NamedArg.init(?u8, .{
                .long = "named-5",
                .default = .required,
            }),
        },
        .positional_args = &.{
            PositionalArg.init(u8, .{
                .meta = "POS-1",
            }),
        },
    };
    const Result = options.Result();
    const Named = @FieldType(Result, "named");
    const Positional = @FieldType(Result, "positional");
    try expectEqual(
        null,
        @as(*const ?u8, @ptrCast(std.meta.fieldInfo(Named, .@"named-1").default_value_ptr.?)).*,
    );
    try expectEqual(
        10,
        @as(*const ?u8, @ptrCast(std.meta.fieldInfo(Named, .@"named-2").default_value_ptr.?)).*.?,
    );
    try expectEqual(null, std.meta.fieldInfo(Named, .@"named-3").default_value_ptr);
    try expectEqual(
        10,
        @as(*const u8, @ptrCast(std.meta.fieldInfo(Named, .@"named-4").default_value_ptr.?)).*,
    );
    try expectEqual(null, std.meta.fieldInfo(Named, .@"named-5").default_value_ptr);
    try expectEqual(null, std.meta.fieldInfo(Positional, .@"POS-1").default_value_ptr);
}

test "lists" {
    const options: Command = .{
        .name = "command-name",
        .named_args = &.{
            NamedArg.initAccum([]const u8, .{
                .long = "list",
            }),
        },
    };

    {
        const result = try options.parseFromSlice(std.testing.allocator, &.{
            "path",
        });
        defer options.parseFree(result);
        try expectEqualSlices([]const u8, &.{}, result.named.list.items);
    }

    {
        const result = try options.parseFromSlice(std.testing.allocator, &.{
            "path",

            "--list",
            "foo",
            "--list",
            "bar",
        });
        defer options.parseFree(result);
        try expectEqualSlices([]const u8, &.{ "foo", "bar" }, result.named.list.items);
    }

    {
        const result = try options.parseFromSlice(std.testing.allocator, &.{
            "path",

            "--list",
            "foo",
            "--list",
            "bar",

            "--no-list",
        });
        defer options.parseFree(result);
        try expectEqualSlices([]const u8, &.{}, result.named.list.items);
    }

    {
        const result = try options.parseFromSlice(std.testing.allocator, &.{
            "path",

            "--list",
            "foo",
            "--list",
            "bar",

            "--no-list",

            "--list",
            "baz",
        });
        defer options.parseFree(result);
        try expectEqualSlices([]const u8, &.{"baz"}, result.named.list.items);
    }
}

test "null terminated strings" {
    const options: Command = .{
        .name = "command-name",
        .named_args = &.{
            NamedArg.init([:0]const u8, .{
                .long = "foo",
            }),
        },
        .positional_args = &.{
            PositionalArg.init([:0]const u8, .{
                .meta = "BAR",
            }),
        },
    };
    {
        const result = try options.parseFromSlice(std.testing.allocator, &.{
            "command-name",
            "--foo",
            "foo-value",
            "bar-value",
        });
        defer options.parseFree(result);
        try expectEqualStrings("foo-value", result.named.foo);
        try expectEqualStrings("bar-value", result.positional.BAR);
    }
}

test "subcommands" {
    const options: Command = .{
        .name = "command-name",
        .named_args = &.{
            NamedArg.init([]const u8, .{
                .long = "foo",
            }),
        },
        .positional_args = &.{
            PositionalArg.init(u8, .{
                .meta = "BAR",
            }),
        },
        .subcommands = &.{
            .{
                .name = "sub1",
                .named_args = &.{
                    NamedArg.init([]const u8, .{
                        .long = "sub1a",
                    }),
                },
                .positional_args = &.{
                    PositionalArg.init(u8, .{
                        .meta = "SUB1B",
                    }),
                },
            },
            .{
                .name = "sub2",
                .named_args = &.{
                    NamedArg.init([]const u8, .{
                        .long = "sub2a",
                    }),
                },
                .positional_args = &.{},
            },
        },
    };

    const Result = options.Result();
    const undef: Result = undefined;
    try expectEqual(1, std.meta.fields(@FieldType(Result, "named")).len);
    try expectEqual(1, std.meta.fields(@FieldType(Result, "positional")).len);
    try expectEqual(
        2,
        std.meta.fields(@typeInfo(@FieldType(Result, "subcommand")).optional.child).len,
    );
    try expectEqual([]const u8, @TypeOf(undef.named.foo));
    try expectEqual(u8, @TypeOf(undef.positional.BAR));

    const Sub1 = options.subcommands[0].Result();
    const undef1: Sub1 = undefined;
    try expectEqual(1, std.meta.fields(@FieldType(Sub1, "named")).len);
    try expectEqual(1, std.meta.fields(@FieldType(Sub1, "positional")).len);
    try expect(@FieldType(Sub1, "subcommand") == ?void);
    try expectEqual([]const u8, @TypeOf(undef1.named.sub1a));
    try expectEqual(u8, @TypeOf(undef1.positional.SUB1B));

    const Sub2 = options.subcommands[1].Result();
    const undef2: Sub2 = undefined;
    try expectEqual(1, std.meta.fields(@FieldType(Sub2, "named")).len);
    try expectEqual(0, std.meta.fields(@FieldType(Sub2, "positional")).len);
    try expect(@FieldType(Sub2, "subcommand") == ?void);
    try expectEqual([]const u8, @TypeOf(undef2.named.sub2a));

    // No subcommand
    try expectEqual(Result{
        .program_name = "path",
        .named = .{
            .foo = "foo",
        },
        .positional = .{
            .BAR = 10,
        },
        .subcommand = null,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--foo",
        "foo",
        "10",
    }));

    // First subcommand
    try expectEqual(Result{
        .program_name = "path",
        .named = .{
            .foo = "foo",
        },
        .positional = .{
            .BAR = 10,
        },
        .subcommand = .{
            .sub1 = .{
                .named = .{
                    .sub1a = "nested",
                },
                .positional = .{
                    .SUB1B = 24,
                },
                .subcommand = null,
            },
        },
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--foo",
        "foo",
        "10",

        "sub1",
        "--sub1a",
        "nested",
        "24",
    }));

    // Second subcommand
    try expectEqual(Result{
        .program_name = "path",
        .named = .{
            .foo = "foo",
        },
        .positional = .{
            .BAR = 10,
        },
        .subcommand = .{
            .sub2 = .{
                .named = .{
                    .sub2a = "nested2",
                },
                .positional = .{},
                .subcommand = null,
            },
        },
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--foo",
        "foo",
        "10",

        "sub2",
        "--sub2a",
        "nested2",
    }));
}
