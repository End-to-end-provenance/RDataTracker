################################
### Interactive Timer Script ###
################################

# For more information on behaviour, look at interactiveTimer.r. This script is simply a wrapper around that
# which allows for multiple scripts to be processed sequentially and which saves the data to timingData.csv

if (!exists("base.dir")) base.dir <- "D:/Users/Luis/Documents/Harvard School Work/Summer 2014/RDataTracker"
util.dir <- paste0(base.dir, "/utilities")
test.dir <- paste0(base.dir, "/exaples")
setwd(util.dir)
source("timerFunctions.r")
source("helpers.r")


### Function: wrapper function to mimic the interactive script by resetting the environment on each call
timeScript <- function(filePath, fileName){
  # test.dir is defined in timerFunctions.r
  setInitialVal(filePath, base=test.dir)
  results <- calcResults(fileName)
  return(results)
}

### Function: returns the filesize of a script

### Main script
# list out scripts to test
src.zero <- list("path" = "/aaron script", "script" = "Simes dendrochronology master analysis file")
src.one <- list("path" = "/calculate square root", "script" = "calculate-square-root")
src.two <- list("path" = "/daily solar radiation", "script" = "daily-solar-radiation")
src.three <- list("path" = "/seminar one", "script" = "R_REU_S1")
src.four <- list("path" = "/seminar two", "script" = "R_REU_S2")
src.six <- list("path" = "/seminar four", "script" = "RWorkshop-Session4")
scripts <- list(src.zero, src.one, src.two,src.three, src.four, src.six)

# results
results <- lapply(scripts,function(x){
  return(timeScript(x$path,x$script)) 
})

# combine by row
rowResults <- Reduce(rbind, results, data.frame())

# set working directory to write out in the right location
setwd(paste0(base.dir, "/examples/_timingResults")

dfile <- paste0("timingData", "-", gsub(" ","T",gsub(":",".",Sys.time())), ".csv")
write.csv(rowResults,dfile, row.names = FALSE)


