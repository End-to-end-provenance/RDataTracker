# TODO: Add comment
# 
# Author: blerner
###############################################################################

start.time <- Sys.time()
library(RDataTracker)
#source("/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/R/RDataTracker.R")

## Directories
if (interactive()) {
  testDir <- getwd()
  ddgDir <- "ddg"
} else {
  testDir <- "[DIR_DEFAULT]"
  ddgDir <- "[DDG-DIR]"
  setwd(testDir)
}

ddg.r.script.path = paste(testDir,"SensorManagement.R",sep="/")
ddg.path = paste(testDir,ddgDir,sep="/")

ddg.init(ddg.r.script.path,
		ddg.path,
     enable.console=TRUE)

### Functions

read.data <- function(file.name, start.date, end.date, met.variable) {
  ddg.function()
  raw.data <- 1:1000
  ddg.return(raw.data)
}

plot.data <- function(data, outfile) {
  ddg.function(outs.graphic=outfile)
}

calibrate <- function(data) {
  ddg.function()
  calibrated.data <- data * 1.1
  ddg.return(calibrated.data)
}

quality.control <- function(data) {
  ddg.return("qc.data")
}

gap.fill <- function(data) {
  ddg.return("gap.filled.data")
}

write.result <- function(data) {
  ddg.function(outs.file="processed-data.csv")
}

ddg.file("met-daily.csv")
raw.data <- read.data("met-daily.csv", "Jul-1-2014", "Jul-31-2014", "airt")
plot.data(raw.data, "raw-plot.jpeg")

ddg.start("analyze.data")
calibrated.data <- calibrate(raw.data)
plot.data(calibrated.data, "calibrated-plot.jpeg")
qc.data <- quality.control(calibrated.data)
plot.data(qc.data, "quality-controlled-plot.jpeg")
gf.data <- gap.fill(qc.data)
plot.data(gf.data, "gap-filled-plot.jpeg")
ddg.finish("analyze.data")

write.result(gf.data)

ddg.save()
end.time <- Sys.time()
print(paste("Execution time =", (end.time - start.time)))

