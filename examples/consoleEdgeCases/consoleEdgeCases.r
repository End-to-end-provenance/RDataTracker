library(RDataTracker)
setwd("[DIR_DEFAULT]")
r.script.path <- paste(getwd(),"/consoleEdgeCases.r",sep="")
ddgdir <- paste(getwd(),"/[DDG-DIR]",sep="")

# Inititialize DDG
ddg.init(r.script.path,ddgdir,enable.console=TRUE)

f <- function(x){return(x)}

# assign a value to a
a <- 5

# use value in procedure
c <- f(a)

# assign a new value to a
a <- 10

# pretend to assign a value to b
if(FALSE) b <- a else b <- 3

# save the ddg.
ddg.save()
