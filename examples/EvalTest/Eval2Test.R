#################################
### QUALITY CONTROL 15-MINUTE ###
#################################

# Quality control for 15-minute time-series data
# QC tests: maximum value, minimum value, repeated value, slope
# Create plots in gui and save as jpeg files
# Record DDG in text format
# Datetime field = datetime, format = YYYY-MM-DDTHH:MM:SS
# ERB rev. 22-Nov-2013

# Modified by Luis Perez 22-Jul-2014

### R packages

library(chron)
require(tcltk)
require(gWidgets)
options(guiToolkit="tcltk")

### Directories

#ddg.library <- Sys.getenv("DDG_LIBRARY")
#if (ddg.library == "") {
# ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
#}
#source(ddg.library)
library(RDataTracker)
#source("/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/R/RDataTracker.R")

# get initial time
startTime <- Sys.time()
invisible(force(startTime))

## Directories
if (interactive()) {
  testDir <- getwd()
  ddgDir <- "ddg"
} else {
  testDir <- "[DIR_DEFAULT]"
  setwd(testDir)
  ddgDir <- "[DDG-DIR]"
}

ddg.r.script.path = paste(testDir,"Eval2Test.R",sep="/")
ddg.path = paste(testDir,ddgDir,sep="/")

ddg.init(ddg.r.script.path,
         ddg.path,
    enable.console=TRUE)

### Functions

read.data <- function() {
  v1 <- c(1, 2, 3)
  v2 <- c(2, 4, 6)
  zz <- data.frame(v1,v2)
  
  ddg.function()
  #ddg.procedure(lookup.ins=TRUE)
  ddg.return.value(zz)
}

f <- function() {
  return (10)
}

f2 <- function() {
  ddg.return.value(20)
}

ddg.start("Source + eval + function")
ddg.eval("all.data <- read.data()")
ddg.finish("Source + eval + function")


ddg.start("Source + function")
all.data <- read.data()
ddg.finish("Source + function")

ddg.start("Source + eval")
ddg.eval("x <- f()")
ddg.finish("Source + eval")

ddg.start("Source + eval + ddg.return.value - ddg.function")
ddg.eval("y <- f2()")
ddg.finish("Source + eval + ddg.return.value - ddg.function")

ddg.save(quit=TRUE)

# Calculate total time of execution
endTime <- Sys.time()
cat("Execution Time =", difftime(endTime, startTime,units="secs"))
