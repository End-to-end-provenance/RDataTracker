###################
### Time Script ###
###################

# Takes as input the name of a single script, script.R, and executes that script
# with no instrumentation, then executes the same script but with minimal 
# instrumentation, and finally executes script-annotated.R. All scripts should be 
# placed in subfolder in "D:/Users/Luis/Dropbox/HarvardForest/RDataTracker Annotations"

# It automatically creates minimum annotations for both -min.R and -annoate.R if
# they don't exist

### R Packages

library(chron)
require(tcltk)
require(gWidgets)
options(guiToolkit="tcltk")

source("helpers.r")

### Function which initializes counter and working directory, as well as other
#   global parameters
# @param wd : the working directory
setInitialVal <- function(wd){
  setwd(paste("D:/Users/Luis/Dropbox/HarvardForest/RDataTracker Annotations",wd,sep=""))
}

### Function which returns the string of R code necessary at the beginning of a 
#   file for minimal DDG instrumentation
# @param scriptDir - a string for the directory of the script
# @param ddgDirPath - a string for the path of the ddgDirectory
startMinInst <- function(scriptPath,ddgDirPath){
  src <- 'source("D:/Users/Luis/Documents/Harvard School Work/Summer 2014/RDataTracker/R/RDataTracker.R")'
  # lib <- "library(RDataTracker)"
  init <- paste("ddg.init('", scriptPath, "','",ddgDirPath, "',enable.console=TRUE)",sep="")
  hist <- paste("loadhistory('", scriptPath, "')",sep="")
  timestamp <- paste("##------ ", date(), " ------##", sep="")
  return(paste(src,init,hist,timestamp,sep="\n"))
}

### Function which returns the string of R code necessart at the end of a file 
#   for minimal DDG instrumentation
endMinInst <- function(){
  return("ddg.save()")
}


### Function: Time the execution time of a string of R code
# @param file - an R file to be sourced, evaluated, and timed
# $return diff - the difference between the time of initial execution and final
timeForEval <- function(file) {
  startTime <- Sys.time()
  source(file, local=TRUE)
  endTime <- Sys.time()
  return(endTime - startTime)
}

### Function: Read input file specified by the user into a single string 
# @param
# $return - a string of the file specifie by the user
readInput <- function(fileName) {
  return(readChar(fileName, file.info(fileName)$size))
}

### Function: Gets file name from the user and returns it as script. Accepts
#   inputs of type (/)script(.)(r/R)
getFileName <- function(){
  input <- INPUT("Please enter script name:")
  pattern <- ".r$" # matches .r at end of string
  replacement <- ""
  return(gsub(pattern,replacement,input,ignore.case=TRUE))
  
} 

### Function: Gets the location of the script files to be tested. The input should be
#   of the form /path/to/test which is relative to the directory of timer.R
getFilePath <- function(){
  input <- INPUT("Please enter script path. Begins with / and create path relative to timer.R.")
  if (substr(input,1,1) != '/') {
    input <- paste("/",input,sep="")
    warning("Adding / to beginning of input!. Input is now '", input, "'", sep="")
    return(input)
  }
  else return(as.character(input))
}

### Function: Takes in the script specified by inp and annotates it minimally, then
#   writes out the result to the filename specified by out
writeMinInstr <- function(inp,out){
  # read input
  expr <- readInput(inp)
  
  # create minExpr
  ddgDirPath <- paste(getwd(),"/ddg-min",sep="")
  dir.create(ddgDirPath,showWarnings=FALSE)
  scriptPath <- paste(getwd(),"/",out,sep="")
  minExpr <- paste(startMinInst(scriptPath,ddgDirPath),"\n",expr,"\n",endMinInst(),sep="")
  
  # create minScript file
  sFile <- file(out)
  write(minExpr,sFile)
  close(sFile)
  
  # write out file to history
  
}

### Function: Calculate time of execution for script.R, minimal annotations, and
#   script-annotated.R
# @param fileName - the name of the file, contained in the directory, which needs 
#                   to be tested the assumption is made that fileName-annoted.r 
#                   also exists in the same directory
# $return - a list of the form [time of script, time of min annotations, time of annotated]
calcResults <- function(fileName) {
  # get file name and file path
  names = sapply(list("-min.r",".r", "-annotated.r"), function(x) {
    return(paste(fileName, x, sep=""))
  })
  
  # create minimum instrumentation file
  writeMinInstr(names[2],names[1])
  
  # create minimum instrumentation file to wd if non-existent annotated file
  if (!file.exists(names[3])) writeMinInstr(names[2],names[3])
  
  return(sapply(names, timeForEval))
}

### Main script
# set initial conditions
timerMain <- function(){
  setInitialVal(getFilePath())

  # calculate results
  results <- calcResults(getFileName())

  # print results
  print(results)
}
