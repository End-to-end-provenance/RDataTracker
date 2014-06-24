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
scr.zero <- list("/aaron script", "Simes dendrochronology master analysis file")
scr.one <- list("/calculate square root", "calculate-square-root")
scr.two <- list("/daily solar radiation", "daily-solar-radiation")
scr.three <- list("/seminar one", "R_REU_S1")
scr.four <- list("/seminar two", "R_REU_S2")
scripts <- list(scr.zero, scr.one, scr.two,src.three, src.four)

# combine by row
rowResults <- rbind(lapply(scripts,function(x){
  return(timeScript(x[1],x[2])) 
}))

# set working directory to write out in the right location
setwd("D:/Users/Luis/Dropbox/HarvardForest/RDataTracker Annotations")

dfile <- paste("timingData", "-", gsub(" ","T",gsub(":",".",Sys.time())), ".csv", sep="")
write.csv(rowResults,dfile, row.names = FALSE)


