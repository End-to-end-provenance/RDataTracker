# Author @Barbara Lerner

# Modified by Luis Perez 7-Jul-2014
# Modified by Luis Perez 17-Jul-2014

library(RDataTracker)

## Directories
testDir <- "[DIR_DEFAULT]/"
setwd(testDir)

ddg.r.script.path = paste(testDir,"consoleTest2.r",sep="")
ddg.path = paste(testDir,"[DDG-DIR]",sep="")

ddg.init(ddg.r.script.path,
         ddg.path,
    enable.console=TRUE)

#source("/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/R/RDataTracker.R")
#ddg.init("D:/Users/Luis/Documents/Harvard School Work/Summer 2014/RDataTracker/examples/consoleTest/consoleTest2.R",
#		"D:/Users/Luis/Documents/Harvard School Work/Summer 2014/RDataTracker/examples/consoleTest/ddg",
#		enable.console=TRUE)

ddg.start("A")
a <- 10
c <- 100
ddg.finish("A")
ddg.start("B")
b <- a + c
a <- 20
ddg.finish("B")
ddg.save(quit=TRUE)
