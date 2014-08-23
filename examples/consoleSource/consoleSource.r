library(RDataTracker)

# get initial time
startTime <- Sys.time()
invisible(force(startTime))

## Directories
testDir <- "[DIR_DEFAULT]/"
setwd(testDir)

ddg.r.script.path = paste(testDir,"consoleSource.r",sep="")
ddg.path = paste(testDir,"[DDG-DIR]",sep="")

ddg.init(ddg.r.script.path,
         ddg.path,
    enable.console=TRUE)

# Do some simple variable assignments here
fun <- function(a,b){
  ddg.function()
  ddg.return.value(a+b)
}

# add stuff
x <- 6
y <- 10
z <- fun(x,y)

# source a script which actually used z and sets w in the global environment
ddg.source("source1.r")

# use w and new z value
v <- fun(w,z)

# source other scripts, first script with collapsible nodes
ddg.source("source2.r", ignore.ddg.calls=FALSE, ignore.init=TRUE)

# then script without them, where we ignore ddg.calls
ddg.source("source3.r")

# same script, but we DON'T ignore ddg.calls (or init, but there's no init in the script)
ddg.source("source3.r", ignore.ddg.calls=FALSE)

# a script which we call with a local environment
ddg.source("source4.r", local=T)

# source which tets the local aspect of this thing!

# another script, we ignore everything but do it all on a new environment
# ddg.source("source4.r", local=baseenv())
ddg.start("Stuff")
# we use some of the variables set in source3.r
m <- 10
f(m)
f(x)
ddg.finish("Stuff")

ddg.save(quit=TRUE)

# Calculate total time of execution
endTime <- Sys.time()
cat("Execution Time =", difftime(endTime, startTime,units="secs"))
