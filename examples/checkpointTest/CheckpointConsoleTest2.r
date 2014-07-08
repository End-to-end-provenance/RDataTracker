###
# Test Script for Checkpoint Functionality. 
# Author @Barbara Lerner

# Modified by Luis Perez 7-Jul-2014

#ddg.library <- Sys.getenv("DDG_LIBRARY")
#if (ddg.library == "") {
#	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
#}

library(RDataTracker)
## Directories
testDir <- "[DIR_DEFAULT]/"
setwd(testDir)

ddg.r.script.path = paste(testDir,"CheckpointTest.r",sep="")
ddg.path = paste(testDir,"ddg",sep="")

#source(ddg.library)

#source("checkpoint.r")

set.n.to.1 <- function() {
	ddg.procedure()
	n <<- 1
	ddg.data.out("set.n.to.1", "n", n)
	checkpoint1 <<- ddg.checkpoint()
}

set.n.to.2 <- function() {
	ddg.procedure()
	
	ddg.data.in("set.n.to.2", "n")
	n <<- 2
	ddg.data.out("set.n.to.2", "n", n)
	print (paste("At end of before, n =", n))
	checkpoint2 <<- ddg.checkpoint()
}

increment <- function() {
	ddg.procedure()
	
	ddg.data.in("increment", "n")
	n <<- n + 1
	ddg.data.out("increment", "n", n)
	print (paste("At end of increment, n =", n))
	
}

#ddg.run(before)

