################################
### Interactive Timer Script ###
################################

# For more information on behaviour, look at interactiveTimer.r. This script is simply a wrapper around that
# which allows for multiple scripts to be processed sequentially and which saves the data to timingData.csv

if (!exists("base.dir")) base.dir <- "D:/Users/Luis/Documents/Harvard School Work/Summer 2014/RDataTracker"
util.dir <- paste0(base.dir, "/utilities")
test.dir <- paste0(base.dir, "/examples")
setwd(util.dir)
source("timerFunctions.r")
source("helpers.r")

### Function: wrapper function to mimic the interactive script by resetting the environment on each call
scriptTimer.timeScript <- function(filePath, fileName){
  # test.dir is defined in timerFunctions.r
  setInitialVal(filePath, base=test.dir)
  results <- calcResults(fileName)
  return(results)
}

# function removing -min.r and -annotated.r
removeEnd <- function(name){
  return(sub("-*.r$", ".r", name, ignore.case=TRUE))
}

### Function: returns the filesize of a script

# set working directory to write out in the right location
writeOutput <- function(results){
  # browser()
  dfile <- paste0("timingData", "-", gsub(" ","T",gsub(":",".",Sys.time())), ".csv")
  write.csv(results,dfile, row.names = FALSE)
}

### Main script
scriptTimer.main <- function(){
  # browser()
  # NOTE THAT THE CODE IS WRITTEN SO THAT IT LOOKS FOR script-clean.r and template_script-annnotated.r
  
  ### WE HAVE AUTOMATED THE BELOW
  setwd(test.dir)
  
  # remove all temporary localTimingScripts folders (so no duplicates show up later)
  unlink(list.files(pattern="^localTimingScripts$", full.names=T, recursive=T, include.dirs=T), recursive=T)
  
  # list out scripts to test
  test.dirs <- list.files(pattern="-clean.r$", full.names=T, recursive=T, ignore.case=T)

  # create script testing input
  scripts <- Map(function(x,y){
    list("path"=sub("^.", "",x), "script"=sub("-clean.r$", "", y, ignore.case=TRUE))
    }, 
    dirname(test.dirs),
    basename(test.dirs)
  )
  
  # results
  results <- lapply(scripts,function(x){
    return(scriptTimer.timeScript(x$path,x$script)) 
  })

  # combine by row
  rowResults <- Reduce(rbind, results, data.frame())

  # add columns specifying type of annotation (no longer needed)
  # rowResults$annotType <- as.factor(sapply(rowResults$script.file, findType))
  rowResults$r.data.version <- as.character(packageVersion("RDataTracker"))
  
  # print output
  print(rowResults)
  str(rowResults)

  # write output 
  setwd(paste0(base.dir, "/examples/_timingResults"))
  writeOutput(rowResults)
}

options <- commandArgs(trailingOnly = TRUE)
if ("execute" %in% options) scriptTimer.main()

