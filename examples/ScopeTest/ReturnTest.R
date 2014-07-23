library(RDataTracker)
#source("/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/R/RDataTracker.R")

if (interactive()) {
	testDir <- getwd()
} else {
	testDir <- "[DIR_DEFAULT]"
	setwd(testDir)
}

ddg.r.script.path = paste(testDir,"ReturnTest.R",sep="/")
ddg.path = paste(testDir,"[DDG-DIR]",sep="/")
ddg.init(ddg.r.script.path,
		 ddg.path,
         enable.console=TRUE)

f <- function(p_a) {
  return (f2(p_a))
}

f2 <- function(p_a) {
   ddg.procedure(lookup.ins=TRUE)
   ddg.return(10)
   return(10)
}

a <- 1
ddg.data(a)
b <- 2
ddg.data(b)
d <- f2(a) + f2(b)
e <- f(3)

ddg.save()
