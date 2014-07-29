# TODO: Add comment
# 
# Author: blerner
###############################################################################


library(RDataTracker)
#source("/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/R/RDataTracker.R")

# get initial time
startTime <- Sys.time()
invisible(force(startTime))

if (interactive()) {
  testDir <- getwd()
  ddgDir <- "ddg"
} else {
  testDir <- "[DIR_DEFAULT]"
  ddgDir <- "[DDG-DIR]"
  setwd(testDir)
}

ddg.r.script.path = paste(testDir,"ScopeTest.R",sep="/")
ddg.path = paste(testDir,ddgDir,sep="/")

ddg.init(ddg.r.script.path,
		ddg.path,
     enable.console=TRUE)

options(warn=1)

f <- function() {
   a <<- b * 10
   ddg.procedure(ins=list("b"), outs.data=list("a"))
   ddg.return(a)
}

g <- function(a) {
    c <- a + 10
    d <- 1000
    #ddg.procedure(lookup.ins=TRUE, outs.data=list("c", "d"))
    ddg.procedure(lookup.ins=TRUE)
    ddg.return(c)
}

h <- function() {
   d <- 333
   ddg.procedure("h", ins=list("d"))
   ddg.return(d)
}

i <- function() {
   x <<- 1000
   ddg.procedure(outs.data=list("x"))
   ddg.return(j(x))
}

j <- function(xx) {
   ddg.data(xx)
   # Following works.
   #ddg.procedure(ins=list("x"))
 
   # This works, too.
   #ddg.procedure(ins=list("a"))

   # This does not work.  It does not find a.  x & a have different scopes
   ddg.procedure(ins=list("xx", "a"))
   return(3)
}

k <- function (xx = 0, yy = 1) {
  ddg.procedure(lookup.ins=TRUE)
	ddg.return (xx + yy)
}

a <- 1
b <- a + 1

f()

c <- 100
if (g(c) != 110) print("g(c) returned the wrong value")

d <- g(c)

h()

i()

k(a, b)
k(a)
k(yy = b)
k()

foobar <- read.csv("foobar.csv")

ddg.file("foobar.csv")
ddg.procedure("Read raw data files", ins=list("foobar.csv"))


ddg.save(quit=TRUE)

# Calculate total time of execution
endTime <- Sys.time()
cat("Execution Time =", difftime(endTime, startTime,units="secs"))
