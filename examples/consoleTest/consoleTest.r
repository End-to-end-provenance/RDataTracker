# Tests of ddg functions.  No useful computation here.
# This test should not produce any errors and does not use 
# files or snapshots so that ddg text files can be easily 
# compared to be sure everything is working as expected.

## Directories

ddg.library <- Sys.getenv("DDG_LIBRARY")
if (ddg.library == "") {
	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
}
source(ddg.library)

ddg.init("/Users/barbaralerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/consoleTest/consoleTest.r",
		"/Users/barbaralerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/consoleTest/ddg",
		enable.console=TRUE)

f <- function(x) {
	g(x)
	h(x)
	return(1)
}

g <- function(x) {
	ddg.procedure()
	ddg.data.in(substitute(x))
	return(1)
}

h <- function(x) {
	ddg.procedure()
	ddg.data.in(substitute(x))
	return(1)
}

someVector <- function() {
	return(c(1, 3, 5))
}

### Run script

x <- 10
ddg.data(x)
f(x)
f(x)

# Then user should do things at console and end by calling ddg.save() from the console.

ddg.save()