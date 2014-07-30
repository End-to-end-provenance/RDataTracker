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

ddg.r.script.path = paste(testDir,"ReturnTest.R",sep="/")
ddg.path = paste(testDir, ddgDir, sep="/")
ddg.init(ddg.r.script.path,
         ddg.path,
         enable.console=TRUE)

f <- function (aa, bb) {
  ddg.procedure(lookup.ins=TRUE)
  retValue <- 3
  ddg.data(retValue)
  ddg.return(retValue)
}


a <- 1
b <- 2
d <- f(a, b)
ddg.save()
