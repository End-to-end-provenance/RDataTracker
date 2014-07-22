source("/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/R/RDataTracker.R")

ddg.init("/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/examples/ScopeTest/ReturnConsoleTest.R",
         "/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/examples/ScopeTest/ddg",
    enable.console=TRUE)

f <- function(p_a) {
  return (f2(p_a))
}

f2 <- function(p_a) {
   ddg.procedure(lookup.ins=TRUE)
   return(10)
}

a <- 1
#b <- f(a)
d <- f2(a)
#d <- f2(f(a))
#d <- f(f2(a))
#e <- d

ddg.save()
