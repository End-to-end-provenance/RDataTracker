###
# Simple script which demonstrates shadowing issue
###

# Modified by Luis Perez 7-Jul-2014

library(RDataTracker)

# get initial time
startTime <- Sys.time()
invisible(force(startTime))

## Directories
testDir <- "[DIR_DEFAULT]/"
setwd(testDir)

ddg.r.script.path = paste(testDir,"scoping_error.r",sep="")
ddg.path = paste(testDir,"ddg",sep="")

# Initialize the provenance graph
ddg.init(ddg.r.script.path,
         ddg.path,
    enable.console=FALSE)

### Data Provenance Graph 

sum <- function(x,y){
  z <- x + y
  ddg.function(outs.data=list("z"))
  return(z)
}

val1 <- 5
val2 <- 4
ddg.data('val1')
ddg.data('val2')
sum(val1,val2)

ddg.save(quit=TRUE)

# Calculate total time of execution
endTime <- Sys.time()
cat("Execution Time =", difftime(endTime, startTime,units="secs"))
