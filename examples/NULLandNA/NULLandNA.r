# Created by Luis Perez. Last modified 7-Jul-2014.
# Modified by Luis Perez 17-Jul-2014

library(RDataTracker)
#source("D:/Users/Luis/Documents/Harvard School Work/Summer 2014/RDataTracker/R/RDataTracker.R")

# get initial time
startTime <- Sys.time()
invisible(force(startTime))

## Directories
testDir <- "[DIR_DEFAULT]/"
setwd(testDir)

ddg.r.script.path = paste(testDir,"NULLandNA.r",sep="")
ddg.path = paste(testDir,"[DDG-DIR]",sep="")

ddg.init(ddg.r.script.path,ddg.path,enable.console=TRUE)

x<- NA
y <- NULL

# create integer 
z <- 5
a <- "character"

# change type to a character
storage.mode(z) <- a

ddg.save(quit=TRUE)

# Calculate total time of execution
endTime <- Sys.time()
cat("Execution Time =", difftime(endTime, startTime,units="secs"))
