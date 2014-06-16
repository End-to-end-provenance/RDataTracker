################################
### Interactive Timer Script ###
################################

# For more information on behaviour, look at interactiveTimer.r. This script is simply a wrapper around that
# which allows for multiple scripts to be processed sequentially and which saves the data to timingData.csv

setwd("D:/Users/Luis/Dropbox/HarvardForest/RDataTracker Annotations/Utilities")
source("timerFunctions.r")
source("helpers.r")


### Function: wrapper function to mimic the interactive script by resetting the environment on each call
timeScript <- function(filePath, fileName){
  setInitialVal(filePath)
  results <- calcResults(fileName)
  colnames(results) <- c("Execution Time (Min)","File Size (kB)")
  
  return(results)
}

### Function: returns the filesize of a script

### Main script
# list out scripts to test
scr0 <- c("/aaron script", "Simes dendrochronology master analysis file")
scr1 <- c("/calculate square root", "calculate-square-root")
scr2 <- c("/daily solar radiation", "daily-solar-radiation")
scripts <- list(src0,scr1,scr2)

# combine by row
rowResults <- rbind(lapply(scripts,function(x){
  return(timeScript(x[1],x[2])) 
}))

# create data frame
#results <- data.frame(matrix(unlist(rowResults), byrow=TRUE, ncol = 3))

# add column of names as first, create dataframe, reorder columns
#results$script <- lapply(scripts, function(x){ return(paste(x[2], ".r", sep=""))})
#results <- results[c(7,1,2,3,4,5,6)]

$names(results) <- c("Script", "Minimal Annotations","Size", "Vanilla Script", "Size", "Manual Annotations", "Size")


setwd("D:/Users/Luis/Dropbox/HarvardForest/RDataTracker Annotations")

dfile <- paste("timingData", "-")
write.csv(as.matrix(results),"timingData.csv", row.names = FALSE)


