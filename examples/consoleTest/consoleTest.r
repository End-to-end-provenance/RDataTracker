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
#source("/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/R/RDataTracker.R")

# get initial time
startTime <- Sys.time()
invisible(force(startTime))

## Directories
if (interactive()) {
  testDir <- getwd()
  ddgDir <- "ddg"
} else {
  testDir <- "[DIR_DEFAULT]"
  ddgDir <- "[DDG-DIR]"
  setwd(testDir)
}

ddg.r.script.path = paste(testDir,"consoleTest.r",sep="/")
ddg.path = paste(testDir,ddgDir,sep="/")

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
	ddg.procedure(lookup.ins=TRUE)
	return(1)
}

h <- function(x) {
	ddg.procedure(lookup.ins=TRUE)
	return(1)
}

someVector <- function() {
	return(c(1, 3, 5))
}

### Run script

x <- 10

f(x)
f(x)

z <- list(1, "c", 1:3)
names(z) <- list("a", "b", "c")

# Then user should do things at console and end by calling ddg.save() from the console.

ddg.save(quit=TRUE)

# Calculate total time of execution
endTime <- Sys.time()
cat("Execution Time =", difftime(endTime, startTime,units="secs"))
