library(RDataTracker)
#source("/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/R/RDataTracker.R")

if (interactive()) {
	testDir <- getwd()
	ddgDir <- "ddg"
} else {
	testDir <- "[DIR_DEFAULT]"
  ddgDir <- "[DDG-DIR]"
	setwd(testDir)
}

ddg.r.script.path = paste(testDir,"NestedFunctions.R",sep="/")
ddg.path = paste(testDir, ddgDir, sep="/")
ddg.init(ddg.r.script.path,
		 ddg.path,
         enable.console=TRUE)

f <- function (aa, bb) {
  ddg.function()
  retValue <- f2(aa) + f2(bb)
  ddg.return.value(retValue)
}

f2 <- function(p_a) {
   ddg.function()
   retValue <- f3(p_a)
   return(ddg.return.value(retValue))
}

f3 <- function(x) {
  return(ddg.return.value(x + 1))
}

a <- 1
b <- 2
d <- f(a, b)

ddg.save(quit=TRUE)
