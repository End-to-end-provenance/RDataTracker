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

#################################################################

#  This file contains utility functions used through RDataTracker.

########  Helpers to manage ddg variables

#' Sets a variable in the ddg environment
#' @param var the name of the variable
#' @param value the value it should have
#' @return the value set
.ddg.set <- function(var, value) {
  .ddg.env[[var]] <- value
  return(invisible(.ddg.env[[var]]))
}

#' @param var the variable to check
#' @return TRUE if the variable is set in the ddg environment
.ddg.is.set <- function(var) {
  return(exists(var, envir=.ddg.env))
}

#' @param var the variable to get
#' @return the variable's value.  If the variable is not set, NULL is returned
#'    and an error node is added to the DDG.  It likely indicates a 
#'    bug within RDataTraker.
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

#' @return TRUE if internal debugging output should be produced.
.ddg.debug.lib <- function() {
  return (.ddg.get("ddg.debug.lib"))
}

#' @return the path to the ddg directory
.ddg.path <- function() {
  return (.ddg.get("ddg.path"))
}

#' @return the name of the folder where the data are stored in the
#'   ddg.  This is just the folder name, not a path.
.ddg.data.dir <- function() {
  return ("data")
}

#' @return the path to the ddg directory where snapshots and copies of files are stored
.ddg.path.data <- function() {
  return(paste(.ddg.path(), .ddg.data.dir() , sep="/"))
}

#' @return the path to the ddg directory where debugging information is stored
.ddg.path.debug <- function() {
  return(paste(.ddg.path(), "/debug", sep=""))
}

#' @return the path to the ddg directory where copies of scripts are stored
.ddg.path.scripts <- function() {
  return(paste(.ddg.path(), "/scripts", sep=""))
}

##### Mutators for specific common actions

#' Increments a ddg counter
#' @param var a counter variable
#' @returnType integer
#' @return the original value + 1
.ddg.inc <- function(var) {
  value <- .ddg.get(var)
  return (.ddg.set(var, value + 1))
}

#' Decrements a ddg counter
#' @param var a counter variable
#' @returnType integer
#' @return the original value - 1
.ddg.dec <- function(var) {
  value <- .ddg.get(var)
  .ddg.set(var, value - 1)
}

#' Adds rows to a ddg data frame
#' @param df the data frame variable
#' @param new.rows the rows to add
#' @returnType a data frame, same type as the one passed in
#' @return the data frame with the new rows added to it
.ddg.add.rows <- function(df, new.rows) {
  table <- .ddg.get(df)
  return (.ddg.set(df, rbind(table, new.rows)))
}

######### Other utility functions

#' ddg.debug.lib.on turns on debugging of DDG construction.
.ddg.debug.lib.on <- function () {
  .ddg.set("ddg.debug.lib", TRUE)
}

#' ddg.debug.lib.off turns off debugging of DDG construction.
.ddg.debug.lib.off <- function () {
  .ddg.set("ddg.debug.lib", FALSE)
}


#' .ddg.format.time reformats time string.
#' 
#' @param time input time string formatted as yyyy-mm-dd hh:mm:ss
#' @returnType string
#' @return time formatted as yyyy-mm-ddThh.mm.ss
.ddg.format.time <- function(time) {
  formatted.time <- strftime(time, format="%Y-%m-%dT%H.%M.%S",usetz=TRUE)
  
  # The strftime call leaves a space between time and time
  # zone. We remove that here.
  return (sub(" ", "", formatted.time))
}

#' @returnType string formatted as yyyy-mm-ddThh.mm.ss
#' @return the current date and time from the system.
.ddg.timestamp <- function() {
  ts <- Sys.time()
  return (.ddg.format.time(ts))
}

#' @param str input string.
#' @returnType string
#' @return the same string but with tabs and end of line charcters
#'    replaced with a single space
#' .ddg.replace.quotes <- function(str) {
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
#' 
#' @param msg error message to record
#' @param msg.type label of the node, defaults to "error.msg"
#' @param doWarn if true, this function displays a warning.  Defaults to TRUE
#' 
.ddg.insert.error.message <- function(msg, msg.type="error.msg", doWarn = TRUE) {
  if (doWarn) {
    warning(msg)
  }
  .ddg.data.node("Exception", msg.type, msg, "ddg.library")
  .ddg.lastproc2data(msg.type, dscope="ddg.library")
}


