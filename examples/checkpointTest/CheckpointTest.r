###
# Test Script for Checkpoint Functionality. 
# Author @Barbara Lerner

# Modified by Luis Perez 7-Jul-2014
# Modified by Luis Perez 17-Jul-2014

rm (list=ls())

#ddg.library <- Sys.getenv("DDG_LIBRARY")
#if (ddg.library == "") {
#	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
#}
#source(ddg.library)
# library(RDataTracker)
## Directories
testDir <- "[DIR_DEFAULT]/"
setwd(testDir)

# source the RDataTracker Library
source("../../R/DDGCheckpoint.R")

# get initial time
startTime <- Sys.time()
invisible(force(startTime))


f <- function() {
	ddg.procedure("f")
	ddg.data.in("n", "f")
	n
}

main <- function() {
	ddg.start("main")
	n <<- 1
	ddg.data("n", n)
	print (paste("n before checkpoint 1 =", f()))
	checkpoint.file.1 <- ddg.checkpoint("checkpoint1")

	ddg.procedure("n <- 2")
	ddg.data.in("n", "n <- 2")
	n <<- 2
	ddg.data.out("n", n, "n <- 2")
	print (paste("n before checkpoint 2 =", f()))
	checkpoint.file.2 <- ddg.checkpoint()
	print("Returned from checkpoint function")

	print(paste("Restoring ", checkpoint.file.1))
	ddg.restore(checkpoint.file.1)
	print (paste("n after restore from checkpoint 1 =", f()))
	if (n != 1) {
		stop ("restore of checkpoint 1 failed!")
	}
	
	ddg.procedure("n <- 3")
	ddg.data.in("n", "n <- 3")
	n <<- 3
	ddg.data.out("n", n, "n <- 3")
	
	f()
	
	print(paste("Restoring ", checkpoint.file.2))
	ddg.restore(checkpoint.file.2)
	if (n != 2) {
		stop ("restore of checkpoint 2 failed!")
	}
	print (paste("n after restore from checkpoint 2 =", f()))
	
	
	ddg.finish("main")
}

### Run script

ddg.run(
		paste(testDir,"CheckpointTest.r",sep=""),
		paste(testDir,"[DDG-DIR]", sep=""),
		main,
		enable.console=FALSE)

ddg.save(quit=TRUE)

# Calculate total time of execution
endTime <- Sys.time()
cat("Execution Time =", difftime(endTime, startTime,units="secs"))
