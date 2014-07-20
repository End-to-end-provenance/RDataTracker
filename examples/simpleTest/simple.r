###
# Very simple test script to assure us that ddg is still handling simple data as expected
###

# Modified by Luis Perez 7-Jul-2014

library(RDataTracker)

## Directories
testDir <- "[DIR_DEFAULT]/"
setwd(testDir)

ddg.r.script.path = paste(testDir,"simple.r",sep="")
ddg.path = paste(testDir,"ddg",sep="")

# Initialize the provenance graph
ddg.init(ddg.r.script.path,
         ddg.path,
    enable.console=FALSE)


a <- 5
ddg.data("a")
b <- 5
ddg.data("b")
c <- a + b
ddg.procedure("Add", ins=list("a","b"), outs.data=list("c"))

ddg.save(quit=TRUE)
