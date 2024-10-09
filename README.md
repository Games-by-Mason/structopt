# Structopt

This is the argument parser I use in my engine and various supporting tools. It's a work in progress, I'm adding features to it as I need them.

# WIP array args
- default values:
	- we can either forbid these, or allow initializing from a slice
	- to initialize from a slice, we need to actually store that slice somewhere...
	- it's unclear where, and it's kinda annoying trying to do this
	- maybe lets just have these always default to empty
- required:
	- can we make these required or no?
- parsefree?
- support --name arg --name arg as well as --name arg arg?
- make sure not to allow as positional args
- test help menu and stuff, look at all existing tests
