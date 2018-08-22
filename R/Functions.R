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

######################### Functions.R ###########################

# The function table contains an entry for each function called 
# in the script. The columns of the table are:
# - ddg.pnum - the procedure node id where the call happened
# - ddg.fun - the name of the function
# - ddg.lib - the library the function comes from


#' .ddg.init.function.table initializes the function table
#' @return nothing
#' @noRd

.ddg.init.function.table <- function () {
  .ddg.set("ddg.function.nodes", 
      data.frame(
          ddg.pnum = numeric(),
          ddg.fun = character(),
          ddg.lib = character(), 
          stringsAsFactors=FALSE))
}

#' .ddg.function.nodes returns the function table as a data frame with 3 columns:
#' ddg.pnum - the procedure node id where the call happened
#' ddg.fun - the name of the function
#' ddg.lib - the library the function comes from
#' @return the function table
#' @noRd

.ddg.function.nodes <- function() {
  return( .ddg.get("ddg.function.nodes") )
}

#' .ddg.save.function.table saves the function table to a file for debugging purposes.
#' The name of the file is function-nodes.csv
#' @return nothing
#' @noRd

.ddg.save.function.table <- function () {
  # save function nodes table to file
  fileout <- paste(.ddg.path.debug(), "/function-nodes.csv", sep="")
  utils::write.csv(.ddg.function.nodes(), fileout, row.names=FALSE)
}

#' .ddg.add.to.function.table adds new functions to the function table
#' @param functions.called vector of names of functions called
#' @return nothing
#' @noRd

.ddg.add.to.function.table <- function (pfunctions) {
  
  if( is.null(pfunctions) || is.na(pfunctions) || nrow(pfunctions) == 0) {
    return()
  } 
  
  libfunctions <- pfunctions [grepl ("package:", pfunctions$ddg.lib), ]
  if ( nrow(libfunctions) > 0 )
  {
    libfunctions$ddg.lib <- sub("package:", "", libfunctions$ddg.lib)
    libfunctions <- cbind( "ddg.pnum" = rep(.ddg.pnum(), nrow(libfunctions)), libfunctions )
    ddg.function.nodes <- rbind( .ddg.function.nodes(), libfunctions )
    .ddg.set( "ddg.function.nodes", ddg.function.nodes )
  } 
}

#' .ddg.get.function.info finds and returns the names of function calls to external
#' packages as well as the names of the packages used.
#' @param function.names 
#' @return a data frame pairing functions with the libraries they come from
#' @noRd

.ddg.get.function.info <- function( function.names )
{
  # edge case: no functions/potential function calls
  if( all(sapply(function.names, is.null)) )
    return(NA)
  
  # functions with unknown libraries
  ddg.fun <- function.names[[1]]
  ddg.lib <- NULL
  
  # identify which of the variable names are functions
  if( ! is.null(function.names[[2]]) )
  {
    vars <- sapply( function.names[[2]], 
        function(name) {
          if( ! .ddg.is.set(name) ) return(NULL)
          else return( get(name) )
        } )
    vars <- sapply( vars, is.function )
    
    # append to list of functions with unknown libraries
    ddg.fun <- append( ddg.fun, names(vars[vars == TRUE]) )
  }
  
  # obtain library information from functions
  fn.frame <- function.names[[3]]
  if (!is.null (fn.frame)) {
    fn.frame[2] <- paste0 ("package:", fn.frame[2], collapse="")
  }
  
  if( length(ddg.fun) > 0 )
  {
    ddg.lib <- sapply( ddg.fun, .ddg.where )
    ddg.lib <- sapply( ddg.lib, environmentName )
        
    ddg.fun <- names(ddg.lib)
    ddg.lib <- unname(ddg.lib)
      
    fn.frame <- rbind( fn.frame, data.frame(ddg.fun, ddg.lib, stringsAsFactors=FALSE) )
  }
  
  # return
  fn.frame <- unique(fn.frame)
  return( fn.frame )
}
