library(RDataTracker)
setwd("D:/Users/Luis/Documents/Harvard School Work/Summer 2014/RDataTracker/examples/consoleIf")
r.script.path <- paste(getwd(),"/consoleIf.r",sep="")
ddgdir <- paste(getwd(),"/ddg",sep="")

# Inititialize DDG
ddg.init(r.script.path,ddgdir,enable.console=TRUE)

# assign a value to a
a <- 5

# pretend to assign a value to b
if(FALSE) b <- a else b <- 3

# save the ddg.
ddg.save()