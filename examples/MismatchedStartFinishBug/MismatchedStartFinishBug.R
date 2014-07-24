# Translation of Sampling Generator code originally written in VisualBasic into R
# VisualBasic author:  Sivan Margalit
# R translator:  Barbara Lerner
# April 2014

# Modified by Luis Perez 7-Jul-2014
# Modified by Luis Perez 17-Jul-2014

# Load the library to create the provenance graphs.  All the function calls below that begin "ddg."
# are calls to functions in this library.

# Works with 0.5.003
# Does not work with 0.5.004
#source("/Users/blerner/Documents/Process/DataProvenance/RDataTracker_0.5.004/RDataTracker/R/RDataTracker.R_0.5.004.1.R")
#source("/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/R/RDataTracker.R")
library(RDataTracker)

# get initial time
startTime <- Sys.time()
invisible(force(startTime))

## Directories
testDir <- "[DIR_DEFAULT]/"
setwd(testDir)

ddg.r.script.path = paste(testDir,"MismatchedStartFinishBug.R",sep="")
ddg.path = paste(testDir,"[DDG-DIR]",sep="")

options(warn=2)
ddg.debug.off()

# Initialize the provenance graph
ddg.init(ddg.r.script.path,
         ddg.path,
		     enable.console = TRUE)

f <- function () {
	ddg.start()
	a <- 1
	ddg.finish()
}

ddg.debug.on()
x <- f()
ddg.debug.off()
ddg.save(quit=TRUE)

# Calculate total time of execution
endTime <- Sys.time()
cat("Execution Time =", difftime(endTime, startTime,units="secs"))
