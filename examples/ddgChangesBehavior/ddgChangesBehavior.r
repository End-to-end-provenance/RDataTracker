### Originally written by Barbara Lerner
# Modified by Luis Perez 7-Jul-2014
# Modified by Luis Perez 17-Jul-2014

rm (list=ls())

#ddg.library <- Sys.getenv("DDG_LIBRARY")
#if (ddg.library == "") {
#	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
#}
#source(ddg.library)

library(RDataTracker)

# get initial time
startTime <- Sys.time()
invisible(force(startTime))

## Directories
testDir <- "[DIR_DEFAULT]/"
setwd(testDir)

ddg.r.script.path = paste(testDir,"ddgChangesBehavior.r",sep="")
ddg.path = paste(testDir,"ddg",sep="")

f <- function(x) {
	return(1)
}

f2 <- function(x) {
	ddg.procedure(lookup.ins=TRUE)
	return(1)
}

f3 <- function() {
	stop("f3 stopped execution")
}

main <- function() {
	ddg.start("main")
	
	a <- 3
	f(a)
	ddg.data(a)
	f2(a)
	
	b <- 1/0
	f(b)
	ddg.data(b)
	f2(b)
	
	# d doesn't exist yet
	f(d)
	ddg.data(d)
	d <- 1
	f2(d)
	
	d <- 6
	f(d[[2]])
	ddg.data(d[[2]])
	f2(d[[2]])
	
	f(f3)
	ddg.data(f3)
	f2(f3)
	
	ddg.data("no.such.var", no.such.var)
	
	ddg.finish("main")
}

### Run script

ddg.run(main, 
        ddg.r.script.path,
        ddg.path)

ddg.save(quit=TRUE)

# Calculate total time of execution
endTime <- Sys.time()
cat("Execution Time =", difftime(endTime, startTime,units="secs"))
