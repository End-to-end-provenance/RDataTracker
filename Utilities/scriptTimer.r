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
  return(results)
}

### Function: returns the filesize of a script

### Main script
# list out scripts to test
src.zero <- list("/aaron script", "Simes dendrochronology master analysis file")
src.one <- list("/calculate square root", "calculate-square-root")
src.two <- list("/daily solar radiation", "daily-solar-radiation")
src.three <- list("/seminar one", "R_REU_S1")
src.four <- list("/seminar two", "R_REU_S2")
src.six <- list("/seminar four", "RWorkshop-Session4")
scripts <- list(src.zero, src.one, src.two,src.three, src.four, src.six)

# results
results <- lapply(scripts,function(x){
  return(timeScript(x[[1]],x[[2]])) 
})

# combine by row
rowResults <- Reduce(rbind, results, data.frame())

# set working directory to write out in the right location
setwd("D:/Users/Luis/Dropbox/HarvardForest/RDataTracker Annotations/TimingResults")

dfile <- paste("timingData", "-", gsub(" ","T",gsub(":",".",Sys.time())), ".csv", sep="")
write.csv(rowResults,dfile, row.names = FALSE)


