library(RDataTracker)
setwd("[DIR_DEFAULT]")
r.script.path <- paste(getwd(),"/consoleEdges.r",sep="")
ddgdir <- paste(getwd(),"/ddg",sep="")

# Inititialize DDG
ddg.init(r.script.path,ddgdir,enable.console=TRUE)

f <- function(x){return(x)}

# assign a value to a
a <- 5

# use value in procedure
c <- f(a)

# assign a new value to a
a <- 10

# save the ddg.
ddg.save()