###
# Simple script to highlight error when assignments are 
# of the form fun(x) <- y
###

### Data Provenance Graph 
setwd("D:/Users/Luis/Dropbox/HarvardForest/RDataTracker Annotations/errors")
r.script.path <- paste(getwd(),"/storage.mode_error.r",sep="")
ddgdir <- paste(getwd(),"/ddg",sep="")
dir.create(ddgdir, showWarnings = FALSE)
library(RDataTracker)

# Intitialize DDG
ddg.init(r.script.path,ddgdir,enable.console=TRUE)



ddg.save()
