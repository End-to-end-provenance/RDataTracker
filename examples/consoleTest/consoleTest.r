# Tests of ddg functions.  No useful computation here.
# This test should not produce any errors and does not use 
# files or snapshots so that ddg text files can be easily 
# compared to be sure everything is working as expected.
 
# Author @Barbara Lerner

# Modified by Luis Perez 7-Jul-2014
# Modified by Luis Perez 17-Jul-2014

# The removal of a call to ddg.data(x) has occured. As this is a console script,
# the test case is expected to fail when run using RScript (produce warnings and error)
# This is THE EXPECTED BEHAVIOUR

# [1] 1Warning messages:
#   
#   1: In .ddg.insert.error.message(error.msg) : No data node found for x
# 2: In .ddg.insert.error.message(error.msg) : No data node found for x
# [1] 1
# Warning messages:
#   1: In .ddg.insert.error.message(error.msg) : No data node found for x
# 2: In .ddg.insert.error.message(error.msg) : No data node found for x


library(RDataTracker)

## Directories
testDir <- "[DIR_DEFAULT]/"
setwd(testDir)

ddg.r.script.path = paste(testDir,"consoleTest.r",sep="")
ddg.path = paste(testDir,"[DDG-DIR]",sep="")

ddg.init(ddg.r.script.path,
         ddg.path,
		enable.console=TRUE)

#ddg.library <- Sys.getenv("DDG_LIBRARY")
#if (ddg.library == "") {
#	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
#}
#source(ddg.library)

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

f(x)
f(x)

# Then user should do things at console and end by calling ddg.save() from the console.

ddg.save(quit=TRUE)
