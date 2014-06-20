#############################
### Time Functions Script ###
#############################

# Contains functions useful for timing scripts. Used by interactiveTimer.r and scriptTimer. The functions 
# are useful for timing scripts found in subdirectories of D:/Users/Luis/Dropbox/HarvardForest/RDataTracker Annotations
# or whatever is set below as the working directory by the setInitialVal function.

### Global Constants (all of format time.name to avoid naming conflicts)
# specifies the number of lines (maximum) between which a .grabhistory call is inserted
time.histLineLim <- 500

# specifies the number of lines that a single command can span
# time.histLineLim - time.cmdLineLim marks the start of a section from which a .grabhistory
# command can be inserted.
time.cmdLineLim <- 50

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
.startHistory <- function(scriptPath){
  hist <- "" # paste("loadhistory('", scriptPath, "')",sep="")
  myTimeStamp <<- paste("##------ ", date(), " ------##", sep="")
  return(paste(hist,myTimeStamp,sep="\n"))
}

### Function which generates the chunk of code to truck our library into loading
#   the current script file into history and into using the corrent myTimeStamp for
#   that loading.
.endHistory <- function(scriptPath){
  cmd <- paste(".ddg.console.node('", scriptPath, "','", myTimeStamp, "')", sep="")
  return(cmd)
}

### Function which returns the string of R code necessary at the beginning of a 
#   file for minimal DDG instrumentation
# @param scriptDir - a string for the directory of the script
# @param ddgDirPath - a string for the path of the ddgDirectory
startMinInst <- function(scriptPath,ddgDirPath){
  src <- 'source("D:/Users/Luis/Documents/Harvard School Work/Summer 2014/RDataTracker/R/RDataTracker.R")'
  # lib <- "library(RDataTracker)"
  hist <- .startHistory(scriptPath)
  init <- paste("ddg.init('", scriptPath, "','",ddgDirPath, "',enable.console=TRUE)",sep="")
  return(paste(src,init,hist,sep="\n"))
}

### Function which returns the string of R code necessary at the end of a file 
#   for minimal DDG instrumentation
endMinInst <- function(scriptPath){
  sourceSave <- .endHistory(scriptPath)
  ddgSave <- "ddg.save()"
  return(paste(sourceSave, "ddg.save()", sep="\n"))
}

### Function which returns whether or not the current line in the history is one
#   at which we should attempt to insert a .grabhistory command.
# @param lineNum - the line number in the history that is being queried
# $return - a boolean specifying viability of this line as insertion point
atSaveLocation <- function(histLineNum){
  # correction for last insertion
  histLineNum <- histLineNum + time.corrLines
  return(histLineNum %% time.histLineLim > (time.histLineLim - time.cmdLineLim))
}

### Function which returns the correct line of code to be written out for minimal
# instrucmentation.
# @param line - a single line of code read from the input file
# @param lineNum - the line number of "line" in the script (defined as the [number
#                  of previous non-single new characters] + 1)
# @return - a modified string similar to line but which includes necessary
#                 annotations for that line
annotateLine <- function(line, histLineNum) {
  # we are at a save section and non-empty command 
  if (atSaveLocation(histLineNum) && line != ""){
    # add lines only if NOT splitting a command (more info in OneNote Book)
    tryCatch({
      cmd = parse(text=line)

      # we add time.corrLines to keep everything at within time.histLineLim even if we
      # insert at locations onether than histLineNum
      time.corrLines <<- time.histLineLim - histLineNum

      grabHistory <- "ddg.grabhistory()"
      return(paste(grabHistory, line,sep="\n"))  
    }, error = function(e){
      # cannot parse line, so in middle of command
      #warning(line, e)
      return(line)
    }, warning = function(w){
      #warning(line,w)
      return(line)
    }) 
  }

  # empty command or don't want to save
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
  # calculate exectution time
  time <- timeForEval(file)

  # calculate script file size
  fInfo <- file.info(file)
  fsize <- fInfo[file,'size']/(2^10) # convert to megabytes

  return(list(time,fsize))
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
  time.corrLines <<- 0 # global correction lines
  
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
  minExpr <- paste(startMinInst(scriptPath,ddgDirPath),"\n",expr,"\n",
                   endMinInst(scriptPath),sep="")
  
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
  extns <- list("-min", "", "-annotated")
  names <- sapply(extns, function(x) {
    return(paste(fileName, x, ".r", sep=""))
  })
  
  # create minimum instrumentation file
  writeMinInstr(names[2],names[1])
  
  # create minimum instrumentation file to wd if non-existent annotated file
  if (!file.exists(names[3])) writeMinInstr(names[2],names[3])

  # create data frame with 3 tested files as rows, columns are data returned by scriptInfo
  dFrame <- data.frame(matrix(unlist(sapply(names, scriptInfo)), byrow=T, nrow=3))
  
  # add DDG Dir location
  dFrame$ddgDir <- sapply(extns, function(x) {
    return(paste("/ddg", x, sep=""))
  })

  # add DDG dir sizes
  dFrame$ddgDirSize <- sapply(dFrame$ddgDir, function(x) {
    return(dirSize(x))
  })

  # add file names and column names, reorder
  dFrame$names <- names
  dFrame <- dFrame[c(5,1,2,3,4)]
  colnames(dFrame) <- c("Script File", "Execution Time (min)", "File Size (kB)",
                        "DDG Dir Loc.", "DDG Dir Size (kB)")
  
  return(dFrame)

}
