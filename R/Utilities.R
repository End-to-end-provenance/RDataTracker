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

########################## Utilities.R #############################

# This file contains utility functions used through RDataTracker / provR.

########  Helpers to manage ddg variables

#' .ddg.set sets a variable in the ddg environment
#' @param var the name of the variable
#' @param value the value it should have
#' @return the value set
#' @noRd

.ddg.set <- function(var, value) {
  .ddg.env[[var]] <- value
  return(invisible(.ddg.env[[var]]))
}

#' .ddg.is.set returns True if the variable is set in the ddg environment
#' @param var the variable to check
#' @return TRUE if the variable is set in the ddg environment
#' @noRd

.ddg.is.set <- function(var) {
  return(exists(var, envir=.ddg.env))
}

#' .ddg.get returns the value of the variable in the ddg environment
#' @param var the variable to get
#' @return the variable's value.  If the variable is not set, NULL is returned
#'    and an error node is added to the DDG.  It likely indicates a 
#'    bug within RDataTraker.
#' @noRd

.ddg.get <- function(var) {
  if (!.ddg.is.set(var)) {
    error.msg <- paste("No binding for", var, ". DDG may be incorrect!")
    .ddg.insert.error.message(error.msg)
    return(NULL)
  }
  else {
    return(.ddg.env[[var]])
  }
}

#############  Getters that are widely used

#' .ddg.debug.lib returns True if internal debugging outpt is enabled
#' @return TRUE if internal debugging output is enabled
#' @noRd

.ddg.debug.lib <- function() {
  return (.ddg.get("ddg.debug.lib"))
}

#' .ddg.script.mode returns True if in script mode
#' @return True if in script mode
#' @noRd

.ddg.script.mode <- function() {
  # Set to FALSE if not set
  if (!.ddg.is.set("ddg.script.mode")) {
    .ddg.set("ddg.script.mode", FALSE)
  }

  return(.ddg.get("ddg.script.mode"))
}

#' .ddg.r.script.path returns the path to the R script that is being
#' executed (script mode) or NULL (console mode).
#' @return the R script path (script mode) or NULL (console mode)
#' @noRd

.ddg.r.script.path <- function() {
  # Set to NULL if not set
  if (!.ddg.is.set("ddg.r.script.path")) {
    .ddg.set("ddg.r.script.path", NULL)
  }

  return(.ddg.get("ddg.r.script.path"))
}

#' .ddg.path returns the current provenance graph directory
#' @return the current provenance graph directory
#' @noRd

.ddg.path <- function() {
  # Set to NULL if not set
  if (!.ddg.is.set("ddg.path")) {
    .ddg.set("ddg.path", NULL)
  }

  return (.ddg.get("ddg.path"))
}

#' .ddg.dir.dir returns the name of the folder where data are stored 
#' in the provenance graph.  This is just the folder name, not a path.
#' @return the name of the data folder
#' @noRd

.ddg.data.dir <- function() {
  return ("data")
}

#' .ddg.path.data returns the path to the directory where snapshots and copies
#' of files are stored
#' @return the path to the snapshot and file copy directory
#' @noRd

.ddg.path.data <- function() {
  return(paste(.ddg.path(), .ddg.data.dir(), sep="/"))
}

#' .ddg.path.debug returns the path to the directory where debugging information
#' is stored
#' @return the path to the debugging directory
#' @noRd

.ddg.path.debug <- function() {
  return(paste(.ddg.path(), "/debug", sep=""))
}

#' .ddg.path.scripts returns the path to the directory where copies of scripts
#' are stored
#' @return the path to the scripts directory
#' @noRd

.ddg.path.scripts <- function() {
  return(paste(.ddg.path(), "/scripts", sep=""))
}

##### Mutators for specific common actions

#' .ddg.inc increments a ddg counter
#' @param var a counter variable
#' @return the original value + 1
#' @noRd

.ddg.inc <- function(var) {
  value <- .ddg.get(var)
  return (.ddg.set(var, value + 1))
}

#' ddg.dec decrements a ddg counter
#' @param var a counter variable
#' @return the original value - 1
#' @noRd

.ddg.dec <- function(var) {
  value <- .ddg.get(var)
  .ddg.set(var, value - 1)
}

#' .ddg.add.rows adds rows to a ddg data frame
#' @param df the data frame variable
#' @param new.rows the rows to add
#' @return the data frame with the new rows added to it
#' @noRd

.ddg.add.rows <- function(df, new.rows) {
  table <- .ddg.get(df)
  return (.ddg.set(df, rbind(table, new.rows)))
}

######### Other utility functions

#' ddg.debug.lib.on turns on debugging of DDG construction.
#' @return nothing
#' @noRd

.ddg.debug.lib.on <- function () {
  .ddg.set("ddg.debug.lib", TRUE)
}

#' ddg.debug.lib.off turns off debugging of DDG construction.
#' @return nothing
#' @noRd

.ddg.debug.lib.off <- function () {
  .ddg.set("ddg.debug.lib", FALSE)
}

#' .ddg.format.time reformats time string.
#' @param time input time string formatted as yyyy-mm-dd hh:mm:ss
#' @return time formatted as yyyy-mm-ddThh.mm.ss.TZ
#' @noRd

.ddg.format.time <- function(time) {
  formatted.time <- strftime(time, format="%Y-%m-%dT%H.%M.%S", usetz=TRUE)
  
  # The strftime call leaves a space between time and time
  # zone. We remove that here.
  return (sub(" ", "", formatted.time))
}

#' .ddg.timestamp returns the current data and time from the system
#' @return the current system date and time
#' @noRd

.ddg.timestamp <- function() {
  ts <- Sys.time()
  return (.ddg.format.time(ts))
}

#' .ddg.remove.tab.and.eol.chars replaces tabs and end of line characters
#' in a string with a single space
#' @param str input string.
#' @return the same string but with tabs and end of line charcters replaced
#' @noRd

.ddg.remove.tab.and.eol.chars <- function (str) {
  if (!is.character(str)) return (str)
  
  # Replace returns, new lines, and tabs with spaces.
  str <- gsub("\r", " ", str)
  str <- gsub("\n", " ", str)
  str <- gsub("\t", " ", str)
  return(str)
}

#' .ddg.insert.error.message issues a warning and inserts an
#' exception node after the last procedure step. The name of the node
#' is "error.msg" and the value is the error message passed to this
#' function. 
#' @param msg error message to record
#' @param msg.type label of the node, defaults to "error.msg"
#' @param doWarn if true, this function displays a warning.  Defaults to TRUE
#' @return nothing
#' @noRd

.ddg.insert.error.message <- function(msg, msg.type="error.msg", doWarn = TRUE) {
  if (doWarn) {
    warning(msg)
  }
  .ddg.data.node("Exception", msg.type, msg, "ddg.library")
  .ddg.lastproc2data(msg.type, dscope="ddg.library")
}

#' .ddg.installedpackages returns a dataframe with information on installed packages
#' @return A data frame with 2 columns: package and version, both strings
#' @noRd

.ddg.installedpackages <- function()
{
  print ("In .ddg.installedpackages")
  packages <- sessioninfo::session_info(include_base = TRUE)
  packages <- packages[[2]]
  installed <- packages[packages$attached, ]
  
  # We need to create a new data frame
  package <- installed$package
  version <- installed$loadedversion
  installed <- data.frame (package, version)
  print ("installed:")
  print (installed)
  return(installed)
}
