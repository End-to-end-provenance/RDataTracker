#############################
### Time Functions Script ###
#############################

# Contains functions useful for timing scripts. Used by interactiveTimer.r and scriptTimer. The functions 
# are useful for timing scripts found in subdirectories of D:/Users/Luis/Dropbox/HarvardForest/RDataTracker Annotations
# or whatever is set below as the working directory by the setInitialVal function.

### Global Constants
# species the number of lines between which a .grabhistory call is inserted
histLineLim <- 500

### Function which initializes counter and working directory, as well as other
#   global parameters
# @param wd : the working directory
setInitialVal <- function(wd){
  setwd(paste("D:/Users/Luis/Dropbox/HarvardForest/RDataTracker Annotations",wd,sep=""))
  
  # clear history
  rm(list = ls())
  
  # set history retention to a 2^14 lines
  Sys.setenv("R_HISTSIZE" = 16384)
}

### Function which generates code to trick RStudio into loading source file into history so 
#   minimual instrumentation of DDG works correctly.
.loadHistory <- function(scriptPath){
  hist <- paste("loadhistory('", scriptPath, "')",sep="")
  timestamp <- paste("##------ ", date(), " ------##", sep="")
  return(paste(hist,timestamp,sep="\n"))
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

### Function which returns the correct line of code to be written out for minimal
# instrucmentation.
# @param line - a single line of code read from the input file
# @param lineNum - the line number of "line" in the script (defined as the [number
#                  of previous non-single new characters] + 1)
# @return - a modified string similar to line but which includes necessary
#                 annotations for that line
annotateLine <- function(line, lineNum) {
  if (lineNum %% histLineLim == 0) return(paste("ddg.grabhistory()",line,sep="\n"))
  else return(line)
}


### Function: Time the execution time of a string of R code
# @param file - an R file to be sourced, evaluated, and timed
# $return diff - the difference between the time of initial execution and final
timeForEval <- function(file) {
  startTime <- Sys.time()
  source(file, local = T, echo = F,verbose = F)
  endTime <- Sys.time()
  return(endTime - startTime)
}

### Function: Returns in list format the information we wish to store for one script
# @param file - and R file for which we need the required information
# $return info - a list of collected file data
scriptInfo <- function(file) {
  time <- timeForEval(file)
  fInfo <- file.info(file)
  size <- fInfo[file,'size']/(2^10) # convert to megabytes
  return(list(time,size))
}

### Function: Read input file specified by the user into a single string 
# @param
# $return - a string of the file specified by the user. The string is already
#           annoted line by line, but nothing has been added before or after. 
#           that is done by endMinInst and startMinInst. 
readInput <- function(fileName) {
  histLineNum <- 1 # keeps tack of line number in context of the history file (ignore blank lines)
  lineNum <- 1 # keeps track of the actual line number
  strFile <- "" # constructes the file as a string
  
  # we need for loop because lapply (and others) don't guarantee order
  for (line in readLines(fileName)){
    strFile <- paste(strFile,annotateLine(line,histLineNum),sep="\n")

    # updates
    histLineNum <- ifelse(line != "", histLineNum + 1, histLineNum)
    lineNum <- lineNum + 1
  }
  return(strFile)
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
  
  # retuns results for each name in a list of (name time, name space)
  return(as.data.frame(sapply(names, scriptInfo)))
}
