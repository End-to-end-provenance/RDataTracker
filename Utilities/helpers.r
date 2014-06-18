######################
### Helpers Script ###
######################

# Contains useful helper functions to be used throughout the summer

### R Packages

library(chron)
require(tcltk)
require(gWidgets)
options(guiToolkit="tcltk")

### Function which presents a pop-up window and returns user intput
# @params message - string to be presented to user as message
# $return CHOICE - the user input (not necessarily a string)
INPUT <- function(message) {
  # open dialog box for user input
  CHOICE <- NA
  w <- gbasicdialog(title=message, handler = function(h,...) CHOICE <<- svalue(input))
  input <- gedit("", initial.msg="", cont=w, width=20)
  addHandlerChanged(input, handler=function (h,...) {
    CHOICE <<- svalue(input)
    dispose(w)
  })
  visible(w, set=TRUE)
  
  return(CHOICE)
}


### Function: Returns the size of the an arbitrary directory
# @params dir - the name of the directory. The directory must be contained inside
#               the working directory.
# $return - the size, in mB of the directory (all files and folder insider, recursively)
#           If the directory does not exists, it return NA
dirSize <- function(dir){
  fdir <- paste(getwd(),dir,sep="")
  tryCatch({
    dirFiles <- list.files(path=fdir, full.names = TRUE, recursive = TRUE)
  }, warning = function(w) {
    return(NA)
  })
  dirInfos <- file.info(dirFiles)
  dirSizes <- dirInfos$size
  dirSize <- sum(dirSizes)/2^10 # we want result in kB
  return(dirSize)
}
