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

######################### SourcedScripts.R ########################

# This file contains functions used to manage information about sourced
# scripts.

# The script table contains these fields:
# snum - the unique script id
# sname - the name of the script file
# stime - the timestamp of script file


#' .ddg.init.sourced.scripts initializes the data needed to manage sourced scripts
#' @return nothing
#' @noRd

.ddg.init.sourced.scripts <- function () {
  # Number of first sourced script (main script).
  .ddg.set("ddg.next.script.num", 1)
  
  # Table of sourced scripts
  .ddg.set("ddg.sourced.scripts", NULL)
}

#' .ddg.next.script.num returns the value to use for the next script id
#' @return the value of the next script id
#' @noRd

.ddg.next.script.num <- function() {
  return(.ddg.get("ddg.next.script.num"))
}

#' .ddg.sourced.scripts returns a dataframe containing the sourced script table
#' @return a data frame containing the sourced script table
#' @noRd

.ddg.sourced.scripts <- function() {
  return(.ddg.get("ddg.sourced.scripts"))
}

#' .ddg.save.sourced.script.table writes the sourced script table to a csv file.
#' Useful for debugging. The file will be in the debug directory in a file called 
#' sourced-scripts.csv.
#' @return nothing
#' @noRd

.ddg.save.sourced.script.table <- function () {
  # Save if script is sourced.
  if (.ddg.script.mode()) {
    # Save sourced script table to file.
    fileout <- paste(.ddg.path.debug(), "/sourced-scripts.csv", sep="")
    ddg.sourced.scripts <- .ddg.get("ddg.sourced.scripts")
    ddg.sourced.scripts2 <- ddg.sourced.scripts[ddg.sourced.scripts$snum >= 0, ]
    utils::write.csv(ddg.sourced.scripts2, fileout, row.names=FALSE)
  }
}

#' .ddg.store.script.info records a new script in the sourced scripts table.
#' @param sname the name of the script file
#' @return the unique id of the script
#' @noRd

.ddg.store.script.info <- function (sname) {
  snum <- .ddg.next.script.num()
  stime <- .ddg.format.time( file.info(sname)$mtime )
  shash <- .ddg.calculate.hash (sname) 
  
  if (snum == 1) {
    df <- data.frame(snum, sname, stime, shash, stringsAsFactors=FALSE)
  } else {
    df<- rbind(.ddg.sourced.scripts(), 
               c(snum, normalizePath(sname, winslash = "/", mustWork = FALSE), stime, shash))
  }
  .ddg.set("ddg.sourced.scripts", df)
  
  # Increment script number.
  .ddg.inc("ddg.next.script.num")
  return (snum)
}

#' .ddg.store.console.info records that the R code came from the console.
#' @return the unique id for the console
#' @noRd

.ddg.store.console.info <- function () {
  df <- data.frame(1, "console", Sys.time(), "NA", stringsAsFactors=FALSE)
  colnames(df) <- c("snum", "sname", "stime", "shash")
  .ddg.set("ddg.sourced.scripts", df)
  
  # Increment script number.
  .ddg.inc("ddg.next.script.num")
  return (1)
}
