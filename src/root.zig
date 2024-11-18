const std = @import("std");
const Allocator = std.mem.Allocator;
const ArgIterator = std.process.ArgIterator;
const FieldEnum = std.meta.FieldEnum;
const log = std.log.scoped(.structopt);
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;
const expectEqualSlices = std.testing.expectEqualSlices;
const expectError = std.testing.expectError;

pub const Error = error{ OutOfMemory, Parser, Help };

pub const Command = struct {
    const Self = @This();

    name: []const u8,
    description: ?[]const u8 = null,
    named_args: []const NamedArg = &.{},
    positional_args: []const PositionalArg = &.{},

    /// The result of parsing a command
    pub fn Result(comptime self: Command) type {
        // Allocate fields
        const fields_len = self.positional_args.len + self.named_args.len;
        var fields: [fields_len]std.builtin.Type.StructField = undefined;

        // Add the named args
        for (self.named_args, self.positional_args.len..fields_len) |arg, i| {
            if (arg.type == ?bool) {
                @compileError(arg.long ++ ": booleans cannot be nullable");
            }
            if (@typeInfo(arg.type) == .optional and arg.accum) {
                @compileError(arg.long ++ ": accum arguments cannot be nullable");
            }
            validateLongName(arg.long);
            if (arg.short) |short| validateShortName(short);
            const T = if (arg.accum) std.ArrayList(arg.type) else arg.type;
            fields[i] = .{
                .name = arg.long,
                .type = T,
                .default_value = arg.default,
                .is_comptime = false,
                .alignment = @alignOf(T),
            };
        }

        // Add the positional args
        for (self.positional_args, 0..self.positional_args.len) |arg, i| {
            if (@typeInfo(arg.type) == .optional) {
                @compileError(arg.meta ++ ": positional arguments cannot be nullable");
            }
            if (arg.type == bool) {
                @compileError(arg.meta ++ ": positional arguments cannot be booleans");
            }
            fields[i] = .{
                .name = arg.meta,
                .type = arg.type,
                .default_value = null,
                .is_comptime = false,
                .alignment = @alignOf(arg.type),
            };
        }

        // Create the type
        return @Type(.{ .@"struct" = .{
            .layout = .auto,
            .fields = &fields,
            .decls = &.{},
            .is_tuple = false,
        } });
    }

    /// Frees the parsed result. Only necessary if lists are used.
    pub fn parseFree(comptime self: @This(), result: self.Result()) void {
        inline for (self.named_args) |named_arg| {
            if (named_arg.accum) {
                @field(result, named_arg.long).deinit();
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
        log.info("{}", .{self.fmtUsage(brief)});
    }

    /// Parse the command line arguments for this process, exit on help or failure. Panics on OOM.
    pub fn parseOrExit(self: @This(), gpa: Allocator, iter: *std.process.ArgIterator) self.Result() {
        return self.parse(gpa, iter) catch |err| switch (err) {
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

    /// Parse the command line arguments for this process
    pub fn parse(self: @This(), gpa: Allocator, iter: *std.process.ArgIterator) Error!self.Result() {
        return self.parseFromAnyIter(gpa, &iter);
    }

    /// Parse the given commands (for testing purposes)
    fn parseFromSlice(self: @This(), gpa: Allocator, args: []const []const u8) Error!self.Result() {
        const Iter = struct {
            slice: []const []const u8,

            fn init(slice: []const []const u8) @This() {
                return .{ .slice = slice };
            }

            fn next(iter: *@This()) ?[]const u8 {
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
        return self.parseFromAnyIter(gpa, &iter);
    }

    /// Parser implementation
    fn parseFromAnyIter(self: @This(), gpa: Allocator, iter: anytype) Error!self.Result() {
        // Initialize result with all defaults set
        const ParsedCommand = self.Result();
        var result: ParsedCommand = undefined;
        inline for (self.named_args) |arg| {
            if (arg.default) |default| {
                @field(result, arg.long) = @as(*const arg.type, @alignCast(@ptrCast(default))).*;
            }
        }

        // Parse the arguments
        const ArgEnum = FieldEnum(ParsedCommand);
        var args: std.EnumMap(ArgEnum, enum { positional, found }) = .{};
        inline for (self.positional_args) |positional_arg| {
            args.put(stringToEnum(ArgEnum, positional_arg.meta).?, .positional);
        }
        inline for (self.named_args) |named_arg| {
            if (named_arg.default != null or named_arg.accum) {
                args.put(stringToEnum(ArgEnum, named_arg.long).?, .found);
            }
            if (named_arg.accum) {
                @field(result, named_arg.long) = .init(gpa);
            }
        }

        // Skip the executable path
        _ = iter.*.skip();

        // Parse the named arguments
        var peeked = while (iter.*.next()) |arg_str| {
            // Stop parsing if we find a positional argument
            if (arg_str[0] != '-') {
                break arg_str;
            }

            // Check for help
            try self.checkHelp(arg_str);

            // Parse the argument name
            const arg_name = if (arg_str.len > 2 and arg_str[1] == '-') arg_str[2..] else arg_str[1..];
            const negated = std.mem.startsWith(u8, arg_name, "no-");
            const lookup = if (negated) arg_name[3..] else arg_name;

            // Look for this argument in the list of fields
            const field_enum = b: {
                if (lookup.len == 1) {
                    break :b getShortArgs(self).get(lookup);
                } else {
                    break :b stringToEnum(ArgEnum, lookup);
                }
            } orelse {
                log.err("unexpected argument \"{s}\"", .{arg_name});
                self.usageBrief();
                return error.Parser;
            };
            if (std.meta.fields(ArgEnum).len == 0) unreachable;

            // Make sure the argument belongs in this section, mark it as found
            if (args.fetchPut(field_enum, .found)) |state| {
                switch (state) {
                    .found => {},
                    .positional => {
                        log.err("unexpected argument \"{s}\"", .{arg_name});
                        self.usageBrief();
                        return error.Parser;
                    },
                }
            }

            // Parse the argument value
            switch (field_enum) {
                inline else => |field_enum_inline| {
                    const field = std.meta.fieldInfo(ParsedCommand, field_enum_inline);
                    if (negated) {
                        if (field.type == bool) {
                            @field(result, field.name) = false;
                        } else if (@typeInfo(field.type) == .optional) {
                            @field(result, field.name) = null;
                        } else if (comptime self.argIsAccum(@intFromEnum(field_enum_inline))) {
                            @field(result, field.name).clearRetainingCapacity();
                        } else {
                            log.err("unexpected argument \"{s}\"", .{arg_name});
                            self.usageBrief();
                            return error.Parser;
                        }
                    } else {
                        var peeked: ?[]const u8 = null;
                        if (comptime self.argIsAccum(@intFromEnum(field_enum_inline))) {
                            const Items = @TypeOf(@field(result, field.name).items);
                            const Item = @typeInfo(Items).pointer.child;
                            try @field(result, field.name).append(try self.parseValue(
                                Item,
                                field.name,
                                iter,
                                &peeked,
                            ));
                        } else {
                            @field(result, field.name) = try self.parseValue(
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
            @field(result, field.meta) = value;
        }

        // Make sure there are no remaining arguments
        if (peeked orelse iter.*.next()) |next| {
            // Check for help
            try self.checkHelp(next);

            // Emit an error
            log.err("unexpected positional argument \"{s}\"", .{next});
            self.usageBrief();
            return error.Parser;
        }

        // Make sure all non optional args were found
        for (std.meta.tags(ArgEnum)) |arg| {
            if (args.get(arg) == null) {
                log.err("missing required argument \"{s}\"", .{@tagName(arg)});
                self.usageBrief();
                return error.Parser;
            }
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

    fn getShortArgs(comptime self: @This()) std.StaticStringMap(FieldEnum(self.Result())) {
        const ArgEnum = FieldEnum(self.Result());
        const max_len = self.positional_args.len + self.named_args.len;
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
        peeked: *?[]const u8,
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
        peeked: *?[]const u8,
    ) Error![]const u8 {
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
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        // At what column to start help
        const col = 25;

        // Brief
        try writer.print("usage: {s}\n", .{self.options.name});
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
                );
            }
        }
    }

    fn writeArg(
        col: usize,
        writer: anytype,
        positional: bool,
        comptime long: []const u8,
        short: ?u8,
        T: ?type,
        description: ?[]const u8,
        accum: bool,
    ) !void {
        // Get the inner type if optional
        const Inner: ?type = if (T) |Some| switch (@typeInfo(Some)) {
            .optional => |optional| optional.child,
            else => T,
        } else T;

        // Write the argument, and measure how many characters it took
        const count = if (short) |s| b: {
            const lhs_fmt = "  -{c}, --{s}{}";
            const lhs_args = .{ s, long, fmtType(Inner) };
            try writer.print(lhs_fmt, lhs_args);
            break :b std.fmt.count(lhs_fmt, lhs_args);
        } else b: {
            const prefix = if (positional) "" else "--";
            const lhs_fmt = "  {s}{s}{}";
            const lhs_args = .{ prefix, long, fmtType(Inner) };
            try writer.print(lhs_fmt, lhs_args);
            break :b std.fmt.count(lhs_fmt, lhs_args);
        };

        if (accum) {
            try writer.print(" (accum)", .{});
        }

        // Write the help message offset by the correct number of characters
        if (description) |desc| {
            if (std.math.sub(usize, col, count) catch null) |padding| {
                try writer.writeByteNTimes(' ', padding);
            }
            try writer.writeAll(desc);
        }
        try writer.writeByte('\n');

        // If we're an enum, list all the options
        if (Inner) |Some| {
            if (@typeInfo(Some) == .@"enum") {
                for (std.meta.fieldNames(Some)) |name| {
                    try writer.writeByteNTimes(' ', if (positional) 4 else 6);
                    try writer.print("{s}\n", .{name});
                }
            }
        }

        // If we're optional, a boolean, or a list, display the "no-" variant
        if (Inner != T or Inner == bool or accum) {
            try writer.print("  --no-{s}\n", .{long});
        }
    }

    fn formatType(
        T: ?type,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        const Some = T orelse return;
        switch (@typeInfo(Some)) {
            .bool => {},
            .int, .float => try writer.print(" <{s}>", .{@typeName(Some)}),
            .@"enum" => {},
            .pointer => try writer.writeAll(" <string>"),
            else => unreachable,
        }
    }

    const FormatUsageData = struct {
        options: Self,
        brief: bool,
    };

    fn fmtUsage(self: @This(), brief: bool) std.fmt.Formatter(formatUsage) {
        return .{ .data = .{ .options = self, .brief = brief } };
    }

    fn fmtType(T: ?type) std.fmt.Formatter(formatType) {
        return .{ .data = T };
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

// ISSUE(https://github.com/ziglang/zig/issues/20310): Calling `std.meta.stringToEnum` directly
// fails to compile under the current version of Zig if the struct only has a single argument. This
// wrapper works around the issue.
pub fn stringToEnum(comptime T: type, str: []const u8) ?T {
    if (@typeInfo(T).@"enum".fields.len == 1) {
        if (std.mem.eql(u8, str, @typeInfo(T).@"enum".fields[0].name)) {
            return @enumFromInt(@typeInfo(T).@"enum".fields[0].value);
        }
        return null;
    }
    return std.meta.stringToEnum(T, str);
}

test "all types nullable required" {
    const Enum = enum { foo, bar };
    const options: Command = .{
        .name = "command name",
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
        },
    };

    const Result = options.Result();
    const undef: Result = undefined;
    try expectEqual(7, std.meta.fields(Result).len);
    try expectEqual(bool, @TypeOf(undef.bool));
    try expectEqual(?u32, @TypeOf(undef.u32));
    try expectEqual(?Enum, @TypeOf(undef.@"enum"));
    try expectEqual(?[]const u8, @TypeOf(undef.string));
    try expectEqual(u8, @TypeOf(undef.U8));
    try expectEqual([]const u8, @TypeOf(undef.STRING));
    try expectEqual(Enum, @TypeOf(undef.ENUM));

    // All set
    try expectEqual(Result{
        .bool = true,
        .u32 = 123,
        .@"enum" = .bar,
        .string = "world",
        .U8 = 1,
        .STRING = "hello",
        .ENUM = .foo,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--u32",
        "123",
        "--enum",
        "bar",
        "--string",
        "world",
        "--bool",

        "1",
        "hello",
        "foo",
    }));

    // All null
    try expectEqual(Result{
        .bool = false,
        .u32 = null,
        .@"enum" = null,
        .string = null,
        .U8 = 1,
        .STRING = "hello",
        .ENUM = .foo,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--no-u32",
        "--no-enum",
        "--no-string",
        "--no-bool",

        "1",
        "hello",
        "foo",
    }));
}

test "all types defaults" {
    const Enum = enum { foo, bar };
    const options: Command = .{
        .name = "command name",
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
        },
    };

    const Result = options.Result();
    const undef: Result = undefined;
    try expectEqual(7, std.meta.fields(Result).len);
    try expectEqual(bool, @TypeOf(undef.bool));
    try expectEqual(u32, @TypeOf(undef.u32));
    try expectEqual(Enum, @TypeOf(undef.@"enum"));
    try expectEqual([]const u8, @TypeOf(undef.string));
    try expectEqual(u8, @TypeOf(undef.U8));
    try expectEqual([]const u8, @TypeOf(undef.STRING));
    try expectEqual(Enum, @TypeOf(undef.ENUM));

    // All set
    try expectEqual(Result{
        .bool = true,
        .u32 = 123,
        .@"enum" = .bar,
        .string = "world",
        .U8 = 1,
        .STRING = "hello",
        .ENUM = .foo,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--u32",
        "123",
        "--enum",
        "bar",
        "--string",
        "world",
        "--bool",

        "1",
        "hello",
        "foo",
    }));

    // All skipped
    try expectEqual(Result{
        .bool = true,
        .u32 = 123,
        .@"enum" = .bar,
        .string = "default",
        .U8 = 1,
        .STRING = "hello",
        .ENUM = .foo,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "1",
        "hello",
        "foo",
    }));
}

test "all types defaults and nullable but not null" {
    const Enum = enum { foo, bar };
    const options: Command = .{
        .name = "command name",
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
        },
    };

    const Result = options.Result();
    const undef: Result = undefined;
    try expectEqual(7, std.meta.fields(Result).len);
    try expectEqual(bool, @TypeOf(undef.bool));
    try expectEqual(?u32, @TypeOf(undef.u32));
    try expectEqual(?Enum, @TypeOf(undef.@"enum"));
    try expectEqual(?[]const u8, @TypeOf(undef.string));
    try expectEqual(u8, @TypeOf(undef.U8));
    try expectEqual([]const u8, @TypeOf(undef.STRING));
    try expectEqual(Enum, @TypeOf(undef.ENUM));

    // All set
    try expectEqual(Result{
        .bool = true,
        .u32 = 123,
        .@"enum" = .bar,
        .string = "world",
        .U8 = 1,
        .STRING = "hello",
        .ENUM = .foo,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--u32",
        "123",
        "--enum",
        "bar",
        "--string",
        "world",
        "--bool",

        "1",
        "hello",
        "foo",
    }));

    // All skipped
    try expectEqual(Result{
        .bool = true,
        .u32 = 123,
        .@"enum" = .bar,
        .string = "default",
        .U8 = 1,
        .STRING = "hello",
        .ENUM = .foo,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "1",
        "hello",
        "foo",
    }));

    // All null
    try expectEqual(Result{
        .bool = false,
        .u32 = null,
        .@"enum" = null,
        .string = null,
        .U8 = 1,
        .STRING = "hello",
        .ENUM = .foo,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--no-u32",
        "--no-enum",
        "--no-string",
        "--no-bool",

        "1",
        "hello",
        "foo",
    }));
}

test "all types defaults and nullable and null" {
    const Enum = enum { foo, bar };
    const options: Command = .{
        .name = "command name",
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
        },
    };

    const Result = options.Result();
    const undef: Result = undefined;
    try expectEqual(7, std.meta.fields(Result).len);
    try expectEqual(bool, @TypeOf(undef.bool));
    try expectEqual(?u32, @TypeOf(undef.u32));
    try expectEqual(?Enum, @TypeOf(undef.@"enum"));
    try expectEqual(?[]const u8, @TypeOf(undef.string));
    try expectEqual(u8, @TypeOf(undef.U8));
    try expectEqual([]const u8, @TypeOf(undef.STRING));
    try expectEqual(Enum, @TypeOf(undef.ENUM));

    // All set
    try expectEqual(Result{
        .bool = true,
        .u32 = 123,
        .@"enum" = .bar,
        .string = "world",
        .U8 = 1,
        .STRING = "hello",
        .ENUM = .foo,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--u32",
        "123",
        "--enum",
        "bar",
        "--string",
        "world",
        "--bool",

        "1",
        "hello",
        "foo",
    }));

    // All skipped
    try expectEqual(Result{
        .bool = false,
        .u32 = null,
        .@"enum" = null,
        .string = null,
        .U8 = 1,
        .STRING = "hello",
        .ENUM = .foo,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "1",
        "hello",
        "foo",
    }));
}

test "all types required" {
    const Enum = enum { foo, bar };
    const options: Command = .{
        .name = "command name",
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
        },
    };

    const Result = options.Result();
    const undef: Result = undefined;
    try expectEqual(7, std.meta.fields(Result).len);
    try expectEqual(bool, @TypeOf(undef.bool));
    try expectEqual(u32, @TypeOf(undef.u32));
    try expectEqual(Enum, @TypeOf(undef.@"enum"));
    try expectEqual([]const u8, @TypeOf(undef.string));
    try expectEqual(u8, @TypeOf(undef.U8));
    try expectEqual([]const u8, @TypeOf(undef.STRING));
    try expectEqual(Enum, @TypeOf(undef.ENUM));

    // All set
    try expectEqual(Result{
        .bool = true,
        .u32 = 123,
        .@"enum" = .bar,
        .string = "world",
        .U8 = 1,
        .STRING = "hello",
        .ENUM = .foo,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--u32",
        "123",
        "--enum",
        "bar",
        "--string",
        "world",
        "--bool",

        "1",
        "hello",
        "foo",
    }));

    // Repeated args
    try expectEqual(Result{
        .bool = false,
        .u32 = 321,
        .@"enum" = .foo,
        .string = "updated",
        .U8 = 1,
        .STRING = "hello",
        .ENUM = .foo,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--u32",
        "123",
        "--enum",
        "bar",
        "--string",
        "world",
        "--bool",

        "--u32",
        "321",
        "--enum",
        "foo",
        "--string",
        "updated",
        "--no-bool",

        "1",
        "hello",
        "foo",
    }));

    // Short names args
    try expectEqual(Result{
        .bool = true,
        .u32 = 321,
        .@"enum" = .foo,
        .string = "updated",
        .U8 = 1,
        .STRING = "hello",
        .ENUM = .foo,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--u",
        "321",
        "-e",
        "foo",
        "-s",
        "updated",
        "-b",

        "1",
        "hello",
        "foo",
    }));
}

test "no args" {
    const options: Command = .{ .name = "command name" };
    const Result = options.Result();
    try expectEqual(0, std.meta.fields(Result).len);
    try expectEqual(Result{}, try options.parseFromSlice(std.testing.allocator, &.{"path"}));
}

test "only positional" {
    const Enum = enum { foo, bar };
    const options: Command = .{
        .name = "command name",
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
        },
    };

    const Result = options.Result();
    const undef: Result = undefined;
    try expectEqual(3, std.meta.fields(Result).len);
    try expectEqual(u8, @TypeOf(undef.U8));
    try expectEqual([]const u8, @TypeOf(undef.STRING));
    try expectEqual(Enum, @TypeOf(undef.ENUM));

    // All set
    try expectEqual(Result{
        .U8 = 1,
        .STRING = "hello",
        .ENUM = .foo,
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "1",
        "hello",
        "foo",
    }));
}

test "only named" {
    const Enum = enum { foo, bar };
    const options: Command = .{
        .name = "command name",
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
        },
    };

    const Result = options.Result();
    const undef: Result = undefined;
    try expectEqual(4, std.meta.fields(Result).len);
    try expectEqual(bool, @TypeOf(undef.bool));
    try expectEqual(?u32, @TypeOf(undef.u32));
    try expectEqual(?Enum, @TypeOf(undef.@"enum"));
    try expectEqual(?[]const u8, @TypeOf(undef.string));

    // All set
    try expectEqual(Result{
        .bool = true,
        .u32 = 123,
        .@"enum" = .bar,
        .string = "world",
    }, try options.parseFromSlice(std.testing.allocator, &.{
        "path",

        "--u32",
        "123",
        "--enum",
        "bar",
        "--string",
        "world",
        "--bool",
    }));
}

test "help menu" {
    const Enum = enum { foo, bar };
    const no_help: Command = .{
        .name = "command name",
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
        },
    };

    const with_help: Command = .{
        .name = "command name",
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
        },
    };

    {
        const found = try std.fmt.allocPrint(std.testing.allocator, "{}", .{no_help.fmtUsage(true)});
        defer std.testing.allocator.free(found);
        try expectEqualStrings(
            \\usage: command name
            \\--help for more info
        , found);
    }

    {
        const found = try std.fmt.allocPrint(std.testing.allocator, "{}", .{with_help.fmtUsage(true)});
        defer std.testing.allocator.free(found);
        try expectEqualStrings(
            \\usage: command name
            \\--help for more info
        , found);
    }

    {
        const found = try std.fmt.allocPrint(std.testing.allocator, "{}", .{no_help.fmtUsage(false)});
        defer std.testing.allocator.free(found);
        try expectEqualStrings(
            \\usage: command name
            \\
            \\options:
            \\  --bool
            \\  --no-bool
            \\  --u32 <u32>
            \\  --no-u32
            \\  --enum
            \\      foo
            \\      bar
            \\  --no-enum
            \\  --string <string>
            \\  --no-string
            \\  --list <string> (accum)
            \\  --no-list
            \\
            \\positional arguments:
            \\  U8 <u8>
            \\  STRING <string>
            \\  ENUM
            \\    foo
            \\    bar
            \\
        , found);
    }

    {
        const found = try std.fmt.allocPrint(std.testing.allocator, "{}", .{with_help.fmtUsage(false)});
        defer std.testing.allocator.free(found);
        try expectEqualStrings(
            \\usage: command name
            \\
            \\command help
            \\
            \\options:
            \\  --bool                 bool help
            \\  --no-bool
            \\  --u32 <u32>            u32 help
            \\  --no-u32
            \\  --enum                 enum help
            \\      foo
            \\      bar
            \\  --no-enum
            \\  --string <string>      string help
            \\  --no-string
            \\
            \\positional arguments:
            \\  U8 <u8>                u8 help
            \\  STRING <string>        string help
            \\  ENUM                   enum help
            \\    foo
            \\    bar
            \\
        , found);
    }
}

test "help argument" {
    const options: Command = .{
        .name = "command name",
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
        .name = "command name",
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
    try expectEqual(null, @as(*const ?u8, @ptrCast(std.meta.fieldInfo(Result, .@"named-1").default_value.?)).*);
    try expectEqual(10, @as(*const ?u8, @ptrCast(std.meta.fieldInfo(Result, .@"named-2").default_value.?)).*.?);
    try expectEqual(null, std.meta.fieldInfo(Result, .@"named-3").default_value);
    try expectEqual(10, @as(*const u8, @ptrCast(std.meta.fieldInfo(Result, .@"named-4").default_value.?)).*);
    try expectEqual(null, std.meta.fieldInfo(Result, .@"named-5").default_value);
    try expectEqual(null, std.meta.fieldInfo(Result, .@"POS-1").default_value);
}

test "lists" {
    const options: Command = .{
        .name = "command name",
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
        try expectEqualSlices([]const u8, &.{}, result.list.items);
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
        try expectEqualSlices([]const u8, &.{ "foo", "bar" }, result.list.items);
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
        try expectEqualSlices([]const u8, &.{}, result.list.items);
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
        try expectEqualSlices([]const u8, &.{"baz"}, result.list.items);
    }
}
