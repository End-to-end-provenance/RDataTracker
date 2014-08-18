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
         enable.console=FALSE)

f <- function (aa, bb) {
  ddg.function()
#  ddg.eval("retValue <- f2(aa) + f2(bb)")
  retValue <- f2(aa) + f2(bb)
  ddg.return(retValue)
}

f2 <- function(p_a) {
   ddg.function()
   ddg.return(10)
}

f3 <- function(aa, bb) {
   ddg.function()
   ddg.return (aa + bb)
}

f4 <- function(n) {
  ddg.function()
  if (n == 0) {
    ddg.return(0)
  }
  else {
    ddg.return(f4(n-1))
  }
}

f5 <- function(n) {
  if (n == 0) {
    ddg.return(0)
  }
  else {
    ddg.return(f5(n-1))
  }
}

a <- 1
ddg.data(a)
b <- 2
ddg.data(b)
#d <- f2(a) + f2(b)
ddg.grabhistory()
#d <- f(a, b)
ddg.eval("d <- f(a, b)")
stopifnot(d == 20)

ddg.eval("e <- f3(a, b)")

ddg.eval("g <- f2(a) + f2(b)")

print ("The following tests recursion.  The DDGs are not correct.")
print("Seeing a difference here may be a good thing!")
f4(3)
f5(3)
ddg.save(quit=TRUE)
