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

# specified the directory where all of our test scripts are found
if (!exists("base.dir")) base.dir <- "D:/Users/Luis/Documents/Harvard School Work/Summer 2014/RDataTracker"
if (!exists("test.dir")) test.dir <- paste0(base.dir,"/examples")

### Function which initializes counter and working directory, as well as other
#   global parameters
# @param wd : the working directory
setInitialVal <- function(wd, base=test.dir){
  setwd(paste0(test.dir,wd))
  
  # clear history
  rm(list = ls())
  
  # set history retention to a 2^14 lines
  Sys.setenv("R_HISTSIZE" = 16384)
}

### Function which generates code to trick RStudio into loading source file into history so 
#   minimual instrumentation of DDG works correctly.
.startHistory <- function(scriptPath){
  hist <- "" # paste("loadhistory('", scriptPath, "')",sep="")
  myTimeStamp <<- paste0("##------ ", date(), " ------##")
  return(paste(hist,myTimeStamp,sep="\n"))
}

### Function which generates the chunk of code to trick our library into loading
#   the current script file into history and into using the corrent myTimeStamp for
#   that loading.
.endHistory <- function(scriptPath){
  cmd <- paste0(".ddg.console.node('", scriptPath, "','", myTimeStamp, "')")
  return(cmd)
}

### Function which returns the string of R code necessary at the beginning of a 
#   file for minimal DDG instrumentation
# @param scriptDir - a string for the directory of the script
# @param ddgDirPath - a string for the path of the ddgDirectory
startMinInst <- function(scriptPath,ddgDirPath, console=TRUE){
  src <- paste0('source("', base.dir, '/R/RDataTracker.R")')
  lib <- "library(RDataTracker)"
  rdt <- if (!is.na(console) && console) src else lib
  hist <- if (!is.na(console) && console) .startHistory(scriptPath) else ""
  console.val <- as.character(!is.na(console))
  init <- paste0("ddg.init('", scriptPath, "','",ddgDirPath, "',enable.console=", console.val, ")")
  wd <- paste0("setwd(", getwd(), ")")
  return(paste(rdt,init,hist,sep="\n"))
}

### Function which returns the string of R code necessary at the end of a file 
#   for minimal DDG instrumentation
# If path is null, then simply ddg.save() is added at the end
endMinInst <- function(scriptPath = NULL){
  sourceSave <- if (!is.null(scriptPath)) .endHistory(scriptPath) else ""
  ddgSave <- "ddg.save(quit=TRUE)"
  return(paste(sourceSave, ddgSave, sep="\n"))
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
timeForEval <- function(file, from.source=FALSE) {
  startTime <- Sys.time()
  force(startTime)
  src.fun <-  if (from.source) function(...){ddg.source(..., ignore.ddg.calls=F)}
              else source

  source(file, local = T, echo = F,verbose = F)
  endTime <- Sys.time()
  return(difftime(endTime, startTime,units="secs"))
}

### Function: Returns in list format the information we wish to store for one script
# @param file - and R file for which we need the required information
# $return info - a list of collected file data
scriptInfo <- function(file) {
  # Printint out some information
  cat(paste0("\n\n\n\nStart Evaluation of ", file, "\n\n\n\n"))
  # calculate exectution time
  time <- timeForEval(file)
  cat(paste0("\n\n\n\nExecution Time for ", file, " : ", time, "\n\n\n\n"))

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
    input <- paste0("/",input)
    warning("Adding / to beginning of input!. Input is now '", input, "'", sep="")
    return(input)
  }
  else return(as.character(input))
}

### Function: Takes in the script specified by inp and annotates it minimally, then
#   writes out the result to the filename specified by out
writeMinInstr <- function(inp,out, console=TRUE){
  # read input
  expr <- if(!is.na(console) && console) readInput(inp) else paste(readLines(inp), collapse="\n") 
  
  # create minExpr
  ddgDirPath <- paste0(getwd(),  if (is.na(console)) "/ddg-annotated" 
                                else if (console) "/ddg-min" 
                                else "/ddg-source")
  scriptPath <- paste0(getwd(),"/",out)
  minExpr <- paste0(startMinInst(scriptPath,ddgDirPath),"\n",expr,"\n",
                   endMinInst(if (is.na(console) || !console) NULL else scriptPath))
  
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
  extns <- list("min" = "-min","source"= "-source", "original" = "-clean", "full" = "-annotated")
  names <- sapply(extns, function(x) {
    return(paste0(fileName, x, ".r"))
  }, simplify=FALSE)

  # get script locations
  scriptLoc <- sapply(names, function(x) {
    return(paste0("localTimingScripts/", x))
  }, simplify=FALSE)

  # get ddg directories
  dirs <- sapply(extns, function(x) {
    return(if (x != "-clean") paste0("ddg", x) else NA)
  })

  # delete directories
  sapply(dirs, function(dir){if (!is.na(dir)) unlink(paste0(dir), recursive=T)})

  # delete the scripts directory
  unlink("localTimingScripts", recursive=T)
  dir.create("localTimingScripts", showWarnings=F)

  # copy the original file over to a new directory
  file.copy(names$original, scriptLoc$original)

  # create minimum and source instrumentation file inside localTimingScripts
  # browser()
  writeMinInstr(scriptLoc$original,scriptLoc$min)
  writeMinInstr(scriptLoc$original,scriptLoc$source,console = FALSE)
  
  # create minimum instrumentation file to wd if non-existent annotated file
  template <- paste0("template_", names$full)
  if (!file.exists(template)) writeMinInstr(scriptLoc$original,scriptLoc$full)
  else writeMinInstr(template, scriptLoc$full, console=NA)


  # create data frame with 4 tested files as rows, columns are data returned by scriptInfo
  dFrame <- data.frame(matrix(unlist(sapply(scriptLoc, scriptInfo)), byrow=T, nrow=4))
  
  # add DDG Dir location
  dFrame$ddgDir <- dirs

  # add DDG dir sizes
  dFrame$ddgDirSize <- sapply(dFrame$ddgDir, function(x) {
    return(dirSize(x))
  })

  # add type of script information
  dFrame$type <- names(extns)

  # add file localtion
  dFrame$scriptLoc <- scriptLoc

  # add file names and column names, reorder
  dFrame$names <- names

  # reformat structure of data frame
  dFrame <- dFrame[c(7,6,5,1,2,3,4)]
  colnames(dFrame) <- c("Script.File", "Script.Loc", "Type", "Execution.Time.min", "File.Size.kB",
                        "DDG.Dir.Loc.", "DDG.Dir.Size.kB")
  
  return(dFrame)

}
