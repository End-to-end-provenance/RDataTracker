# Copyright (C) President and Fellows of Harvard College and 
# Trustees of Mount Holyoke College, 2014, 2015, 2016, 2017, 2018.

# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public
#   License along with this program.  If not, see
#   <http://www.gnu.org/licenses/>.

############## Console.R ###########################

# Functions used to collect provenance from console commands.
# This works by using the R history file.  Every time that a 
# ddg function is executed, we create a set of nodes corresponding
# to the statements executed since the last ddg function was executed.
# We add a timestamp to the history file so that we know where to
# start each time.
#
# Provenance collected in this way will have less detail about the 
# data.  If a variable has been modified multiple times since the
# last ddg function, it will only know what the last value assigned to a variable
# is.
#
# Nodes collected in console mode will appear in a start-finish block.

#' Initialize the variables used for dealing with console commands.
#' Initially, the console is disabled.
#' @return nothing
.ddg.init.console.vars <- function () {
  # Keep track of history.
  .ddg.set(".ddg.history.timestamp", NULL)
  
  # No history file.
  .ddg.set(".ddg.history.file", NULL)
  
  # Console is disabled.
  .ddg.set(".ddg.enable.console", FALSE)
}

#' Set the enable console flag according to the parameter
#' passed in.
#' 
#' @param enable.console If TRUE, console nodes will be created.
#' @return nothing
.ddg.set.enable.console <- function (enable.console) {
  .ddg.set (".ddg.enable.console", enable.console)
}

#' @returnType logical
#' @return whether console nodes are being constructed
.ddg.enable.console <- function() {
  return (.ddg.get(".ddg.enable.console"))
}

#' ddg.console.off creates a final console node and then
#' turns off the console mode.  If we are not in console mode,
#' it does nothing.
#' @return nothing
ddg.console.off <- function() {
  if (!.ddg.is.init()) return(invisible())
  
  # Capture history if console was on up to this point.
  if (interactive() && .ddg.enable.console()) {
    .ddg.console.node()
  }
  
  # Set the console to off.
  .ddg.set(".ddg.enable.console", FALSE)
}

#' ddg.console.on turns on the console mode of DDG construction.
#' If it was not already on, it writes a timestamp to the history
#' file.
#' @return nothing
ddg.console.on <- function() {
  if (!.ddg.is.init()) return(invisible())
  
  # Write a new timestamp if we're turning on the console so
  # we only capture history from this point forward.
  if (!.ddg.enable.console()) .ddg.write.timestamp.to.history()
  .ddg.set(".ddg.enable.console", TRUE)
}

#' Initializes the history file.  It does nothing if we are not
#' in console mode.
#' 
#' @return nothing
.ddg.init.history.file <- function () {
  if (!interactive()) return()
  if (!.ddg.enable.console()) return()
    
  # Make the history size big.  Remember its original value so
  # it can be restored on exit.
  .ddg.set('ddg.original.hist.size', Sys.getenv('R_HISTSIZE'))
  Sys.setenv("R_HISTSIZE" = 16384)
  
  # Create empty history file
  ddg.history.file <- paste(.ddg.path.data(), "/.ddghistory", sep="")
  .ddg.set(".ddg.history.file", ddg.history.file)
  file.create(ddg.history.file, showWarnings=FALSE)
  
  # One timestamp keeps track of last ddg.save (the default).
  .ddg.write.timestamp.to.history()
  
  # Save the history if the platform supports it.
  tryCatch (utils::savehistory(ddg.history.file),
      error = function(e) {})
}

#' Restore the size of the history file.  Should be called when we 
#' exit.
.ddg.restore.history.size <- function() {
  if (.ddg.is.set('ddg.original.hist.size')) Sys.setenv("R_HISTSIZE"=.ddg.get('ddg.original.hist.size'))
}

#' .ddg.loadhistory takes in the name of a history file, opens it,
#' scans it for the last occurrence of the string specified by
#' timestamp, and returns the lines from that point forward.
#' 
#' @param hist.file name of history file.
#' @param timestamp timestamp string.
#' @returnType a vector of strings
#' @return lines in the history file since the last timestamp
.ddg.loadhistory <- function(hist.file, timestamp) {
  # Read from specified file.
  history <- readLines(hist.file)
  history.lines <- length(history)
  
  # Find the timestamp specified in the history.  There may be
  # more than one with the same timestamp, so pick the last of
  # these.
  history.timestamp.line <- utils::tail(which(history == timestamp), 1)
  
  if (length(history.timestamp.line) == 0) {
    error.msg <- paste("Part of history is missing. DDG may be incomplete! Tried reading from",
        hist.file, "but could not find timestamp:", timestamp)
    
    .ddg.insert.error.message(error.msg)
    history.timestamp.line <- 0
  }
  
  # Need to check if the timestamp line is the last line in the file
  # explicitly.  If we don't do that and take the vector, we will
  # get the last line in the file since R will create a descending
  # sequence for the vector.
  if (history.timestamp.line == history.lines) return (vector())
  
  return(history[(history.timestamp.line+1):history.lines])
}

#' .ddg.savehistory saves the current and unsaved R command history
#' to the specified file if that file matches the DDG history file.
#' Note: the commented section of code appends the information to
#' this file.
#' 
#' savehistory is not supported on all R platforms.  If it is not supported,
#' this will fail silently.
#' 
#' @param hist.file name of history file.
#' @return nothing
.ddg.savehistory <- function(hist.file) {
  
  if (.ddg.is.set(".ddg.history.file")) {
    history.file <- .ddg.get(".ddg.history.file")
    if (!is.null (history.file) && history.file == hist.file) {
      utils::savehistory(hist.file)
    }
  }
}

#' .ddg.console.node creates a console node.  It does nothing if we
#' are not in console mode.
#' @return nothing
.ddg.console.node <- function() {
  if (!interactive()) return()
  if (!.ddg.enable.console()) return()
  if (.ddg.is.sourced()) return()
    
  # Don't do anything if sourcing, because history isn't necessary
  # in this case.
  # Do we need this test??
  if(.ddg.enable.source()) return()
  
  ddg.history.file=.ddg.get(".ddg.history.file")
  ddg.history.timestamp=.ddg.get(".ddg.history.timestamp")
  
  # Only continue if these values exists.
  if ((is.null(ddg.history.file) || is.null(ddg.history.timestamp))) return ()
  
  # Grab any new commands that might still be in history.
  tryCatch (
      {
        # Saving history is not supported on all platforms.
        .ddg.savehistory(ddg.history.file)
        
        # Load from extended history since last time we wrote out
        # a console node.
        new.lines <- .ddg.loadhistory(ddg.history.file, ddg.history.timestamp)
        
        # Parse the lines into individual commands.
        parsed.commands <- .ddg.parse.lines(new.lines)
        
        # Create the provenance for the new commands since last timestamp.
        if (length(parsed.commands) > 0) {
          .ddg.parse.commands(parsed.commands,
              environ = .GlobalEnv,
              run.commands=FALSE)
        }
      },
      error =
          function(e) {
      })
    
}

#' .ddg.parse.lines takes as input a vector of lines corresponding to
#' the history of an R script. It parses
#' and converts them to expression objects. Each expression might span
#' multiple lines. 
#' 
#' @param lines set of lines from command history or R script.
#' @returnType list of expression objects
#' @return the parsed version of the R text passed in.  Calls to ddg.eval
#'    are not included in the returned list.
.ddg.parse.lines <- function(lines) {
  # No new lines passed in, so return an empty vector.
  if (length(lines) == 0) return(vector())
  
  # Parse the new lines.
  parsed.commands <- parse(text=lines)
  parsed.commands <- Filter(function(cmd) {return (!is.call(cmd) || !grepl("^ddg.eval", cmd[[1]]))}, parsed.commands)
  return(parsed.commands)
}

#' .ddg.write.timestamp.to.history writes the current timestamp to
#' the R history. The timestamp function does not work properly in
#' Windows from within RStudio (the arguments are ignored).  In this
#' case we create our own timestamp value and hope that the time
#' does not change between when we set .ddg.history.timestamp and
#' when the timestamp function inserts the timestamp in the history.
.ddg.write.timestamp.to.history <- function() {
  # if (Sys.getenv("RSTUDIO") != "" && Sys.info()['sysname'] == "Windows") {
  #   .ddg.set(".ddg.history.timestamp", paste("##------", date(), "------##"))
  #   timestamp(quiet=TRUE)
  # }
  # else {
    .ddg.set(".ddg.history.timestamp", utils::timestamp(prefix = "##-ddg-- ", quiet=TRUE))
  # }
}


