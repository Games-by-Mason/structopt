# Structopt

This is the argument parser I use in my engine and various supporting tools. It's a work in progress, I'm only adding features to it as I need them.

I'm uploading this here so I can file issue for later, and convince myself to move onto more important stuff!

## Work In Progress
### Help menu
* Defaults are not listed
* Column width is arbitrarily set
* `--help`/`-h` is not an automatic option
* Brief help doesn't list args on one line

### Testing
* Errors are not tested (I should return a structured error type and test that)
	* Extra args
	* Skipping required args
	* Bad values
	* Using `-no` incorrectly
	* Extra args at end
	* Anywhere else errors are logged

### Nice to have
* A default formatter so options can be logged at startup
