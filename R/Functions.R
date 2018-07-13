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

################  Functions involved in dealing with functions declared in an R script ################
#
# The function table contains an entry for each function called in the script.
# The columns of the table are:
# - ddg.pnum - the procedure node id where the call happened
# - ddg.fun - the name of the function
# - ddg.lib - the library the function comes from


#' Initializes the function table
#' @return nothing 
.ddg.init.function.table <- function () {
  .ddg.set("ddg.function.nodes" , 
      data.frame(
          ddg.pnum = numeric(),
          ddg.fun = character(),
          ddg.lib = character(), 
          stringsAsFactors=FALSE))
}

#' Return the function table
#' @returnType a data frame with 3 columns:  \cr
#' - ddg.pnum - the procedure node id where the call happened \cr
#' - ddg.fun - the name of the function \cr
#' - ddg.lib - the library the function comes from
#' @return the function table
.ddg.function.nodes <- function() {
  return( .ddg.get("ddg.function.nodes") )
}

#' Save the function table to a file for debugging purposes.
#' The name of the file is function-nodes.csv
#' @return nothing
.ddg.save.function.table <- function () {
  # save function nodes table to file
  fileout <- paste(.ddg.path.debug(), "/function-nodes.csv", sep="")
  write.csv(.ddg.function.nodes(), fileout, row.names=FALSE)
}

#' Add new functions to the function table
#' @param pfunctions a data frame pairing functions with the libraries they come from
#' @return nothing
.ddg.add.to.function.table <- function (pfunctions) {
  if( is.null(pfunctions) || is.na(pfunctions)) {
    return()
  } 
    
  pfunctions <- cbind( "ddg.pnum" = rep(.ddg.pnum(), nrow(pfunctions)) , pfunctions )
  ddg.function.nodes <- rbind( .ddg.function.nodes() , pfunctions )
  .ddg.set( "ddg.function.nodes" , ddg.function.nodes )
}
