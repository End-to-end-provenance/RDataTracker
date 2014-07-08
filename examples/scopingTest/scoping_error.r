###
# Simple script which demonstrates shadowing issue
###

# Modified by Luis Perez 7-Jul-2014

library(RDataTracker)

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
  ddg.procedure(lookup.ins=TRUE,outs.data=list("z"))
  return(z)
}

val1 <- 5
val2 <- 4
ddg.data('val1')
ddg.data('val2')
sum(val1,val2)

ddg.save()
