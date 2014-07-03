#library(RDataTracker)
source("/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/R/RDataTracker.R")
ddg.init("/Users/blerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/consoleTest/consoleTest2.R",
		"/Users/blerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/consoleTest/ddg",
		enable.console=TRUE)

ddg.start("A")
a <- 10
c <- 100
ddg.finish("A")
ddg.start("B")
b <- a + c
a <- 20
ddg.finish("B")
ddg.save()
