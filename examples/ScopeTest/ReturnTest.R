#library(RDataTracker)
source("/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/R/RDataTracker.R")

if (interactive()) {
	testDir <- getwd()
	ddgDir <- "ddg"
} else {
	testDir <- "[DIR_DEFAULT]"
  ddgDir <- "[DDG-DIR]"
	setwd(testDir)
}

ddg.r.script.path = paste(testDir,"ReturnTest.R",sep="/")
ddg.path = paste(testDir, ddgDir, sep="/")
ddg.init(ddg.r.script.path,
		 ddg.path,
         enable.console=TRUE)

f <- function (aa, bb) {
  ddg.procedure(lookup.ins=TRUE)
  ddg.return(f2(aa) + f2(bb))
}

f2 <- function(p_a) {
   ddg.procedure(lookup.ins=TRUE)
   ddg.return(10)
}

a <- 1
ddg.data(a)
b <- 2
ddg.data(b)
#d <- f2(a) + f2(b)
ddg.grabhistory()
ddg.debug.on()
d <- f(a, b)
#ddg.eval("d <- f(a, b)")
ddg.save()
