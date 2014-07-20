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
source("../../R/RDataTracker.r")

main <- function() {
	ddg.start("main")
	
	ddg.procedure("f")
	ddg.file.out ("testfile.txt", pname="f")
	ddg.file.out("testfile2.txt", pname="f")
	write("This file will be deleted", "testfile3.txt")
	ddg.file.out("testfile3.txt", pname="f")
	
	ddg.procedure("g")
	ddg.file.out("testfile.txt", pname="g")
	
	checkpoint <- ddg.checkpoint("my checkpoint")
	
	ddg.procedure("h")
	write("abc",file="testfile2.txt",append=TRUE)
	ddg.file.out("testfile.txt", pname="h")
	ddg.file.out("testfile2.txt", pname="h")
	unlink("testfile3.txt")
	
	ddg.restore(checkpoint)
	
	ddg.procedure("i", ins=list("testfile.txt", "testfile2.txt", "testfile3.txt"))
	
	ddg.finish("main")
}

### Run script

ddg.run(main, 
		paste(testDir,"CheckpointFileTest.r",sep=""),
		paste(testDir,"[DDG-DIR]",sep=""))

ddg.save(quit=TRUE)
