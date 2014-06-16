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