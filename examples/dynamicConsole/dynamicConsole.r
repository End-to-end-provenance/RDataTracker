# Test the ability to dynamically turn the console on and off
#
# Author: Luis Perez
###############################################################################

library(RDataTracker)

# get initial time
startTime <- Sys.time()
invisible(force(startTime))

testDir <- "[DIR_DEFAULT]/"
setwd(testDir)

ddg.r.script.path = paste(testDir,"dynamicConsole.r",sep="")
ddg.path = paste(testDir,"[DDG-DIR]",sep="")

ddg.init(ddg.r.script.path,
    ddg.path,
     enable.console=TRUE)

# Turn off console for definitions
ddg.console.off()

f <- function() {
   a <<- b * 10
   ddg.function(outs.data=list("a"))
   return(a)
}

g <- function(a) {
    c <- a + 10
    d <- 1000
    #ddg.procedure(lookup.ins=TRUE, outs.data=list("c", "d"))
    ddg.function(outs.data=list("c", "d"))
    return(c)
}

# turh on for a single definition
ddg.console.on()
h <- function() {
   d <- 333
   ddg.function()
   return(d)
}
ddg.console.off()

i <- function() {
   x <<- 1000
   ddg.function(outs.data=list("x"))
   return(j(x))
}

j <- function(xx) {
   ddg.data(xx)
   # Following works.
   #ddg.procedure(ins=list("x"))
 
   # This works, too.
   #ddg.procedure(ins=list("a"))

   # This does not work.  It does not find a.  x & a have different scopes
   ddg.function()
}

# Turn on the console to capture some good information
ddg.console.on()
a <- 1
b <- a + 1

f()

c <- 100
ddg.console.off()
if (g(c) != 110) print("g(c) returned the wrong value")
ddg.console.on()

d <- g(c)

# Turn off the consle for a single call to h()
ddg.console.off()
h()
ddg.console.on()

# a double console empty node is creared HERE if no commands are around :(
x <- 5
i()

# Turn off for the rest of the script
ddg.console.off()

foobar <- read.csv("foobar.csv")
ddg.file("foobar.csv")
ddg.procedure("Read raw data files", ins=list("foobar.csv"))


ddg.save(quit=TRUE)

# Calculate total time of execution
endTime <- Sys.time()
cat("Execution Time =", difftime(endTime, startTime,units="secs"))
