# Copyright (C) President and Fellows of Harvard College and 
# Trustees of Mount Holyoke College, 2014, 2015, 2016, 2017.

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

########################### DDGHash.R ###########################

# This file contains the functions that calculate and record hash 
# values for input and output files.


#' .ddg.init.filenodes initializes the list of input and output
#' file nodes.
#' @return nothing
#' @noRd

.ddg.init.filenodes <- function() {
  # List of files read and written
  .ddg.set("ddg.infilenodes", character())
  .ddg.set("ddg.outfilenodes", character())
  
  # Boolean of whether there are any file nodes
  .ddg.set("ddg.hasfilenodes", FALSE)
}

#' .ddg.add.infiles adds the infile list to the list of files read
#' @param files files to add to the list of files read
#' @return nothing
#' @noRd

.ddg.add.infiles <- function (files) {
  .ddg.set("ddg.infilenodes", c(.ddg.get("ddg.infilenodes"), files))
}

#' .ddg.add.outfiles adds the outfile list to the list of files written
#' @param files files to add to the list of files written
#' @return nothing 
#' @noRd

.ddg.add.outfiles <- function (files) {
  .ddg.set("ddg.outfilenodes", c(.ddg.get("ddg.outfilenodes"), files))
}

#' .ddg.calculate.hash calculates the hash value for the file
#' @param filepath the full path to the file
#' @return the hash value based on the hash algorithm specified when 
#' prov.run or prov.init was called. Returns "" if the digest cannot 
#' be computed, for example, if the file does not exist.
#' @noRd

.ddg.calculate.hash <- function(filepath) {
 	.ddg.set("ddg.hasfilenodes", TRUE)
  # This function will cause certain tests to fail if run with pdf files
  # or other non-text files with internal timestamps. This could also cause 
  # these files to sync incorrectly in the workflow, but given that reading 
  # in a pdf file is unlikely, this should not be an overly large issue.
  dhash <- digest::digest(file=filepath, algo=.ddg.get("ddg.hash.algorithm"))
  if (is.null(dhash)) {
    dhash <- ""
  }
  return (dhash)
}