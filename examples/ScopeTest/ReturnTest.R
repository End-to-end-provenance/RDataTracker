library(RDataTracker)
#source("/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/R/RDataTracker.R")

ddg.init("/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/examples/ScopeTest/ReturnTest.R",
         "/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/examples/ScopeTest/ddg",
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
