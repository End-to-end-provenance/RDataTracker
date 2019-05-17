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

############  FunctionsDefined.R   #################################

# This file manages the table that tracks the nonlocal variables
# set and used by a function defined within the script.

#' .ddg.init.funciton.def.table
#' 
#' Initialize the function definition table.  It contains an entry
#' for each function within the script.  An entry has 3 columns:
#' function name, list of nonlocals set by the function, and the
#' list of nonlocals used by the function.
#' 
#' @return nothing
#' @noRd 
.ddg.init.function.def.table <- function () {
  .ddg.set("ddg.function.defs", 
      data.frame(
          func.name = character(),
          nonlocals.set = vector(),
          nonlocals.used = vector(),
          stringsAsFactors=FALSE))
}

#' .ddg.save.func.decl.info
#' 
#' Saves the information about nonlocals set and used by 
#' a function.
#' 
#' Note:  we assume that each function defined by the user has a unique name.
#' To allow for duplicate names, we would need a way to map from a function call
#' the the appropriate entry in this table.
#' 
#' @param funcname the name of the function
#' @param funcdecl the parsed declaration of the function.  This should be
#'   everything from "function" to the closing curly brace
#' @return nothing
#' @noRd 
.ddg.save.func.decl.info <- function (funcname, funcdecl) {
  # Gather the information
  nonlocals.set <- .ddg.find.nonlocals.set (funcdecl)
  nonlocals.used <- .ddg.find.nonlocals.used (funcdecl)
  
  # Create the row for this function
  new.row <- data.frame (func.name=character(1), stringsAsFactors=FALSE )
  new.row$func.name[1] <- as.character(funcname)

  new.row$nonlocals.set <- vector ("list", 1)
  if (length(nonlocals.set > 0)) {
    new.row$nonlocals.set[[1]] <- nonlocals.set
  }
  
  new.row$nonlocals.used <- vector ("list", 1)
  if (length(nonlocals.used > 0)) {
    new.row$nonlocals.used[[1]] <- nonlocals.used
  }
  
  # Add to the table
  ddg.func.defs <- .ddg.get ("ddg.function.defs")
  if (nrow (ddg.func.defs [ddg.func.defs$func.name == funcname, ]) > 0) {
    warning (paste ("Multiple functions defined with name", funcname, 
      ".  Provenance about use of non-locals inside these functions may be incorrect."))
  }
  else {
    ddg.func.defs <- rbind( ddg.func.defs, new.row)
    .ddg.set( "ddg.function.defs", ddg.func.defs )
  }
}

#' .ddg.find.nonlocals.set
#' 
#' Find all the nonlocal variables set in a function declaration.  This function
#' is passed a function declaration and looks for the <<- syntax indicating
#' that a non-local is being set. 
#' 
#' @param funcdecl the parsed declaration of the function.  This should be
#'   everything from "function" to the closing curly brace
#' @return a vector containing all non-local variables assigned in the function
#' @noRd 
.ddg.find.nonlocals.set <- function (funcdecl) {
  return (.ddg.find.assign (funcdecl[[3]], globals.only = TRUE))
}

#' .ddg.get.nonlocals.set
#' 
#' Get the nonlocals set by calls to user-defined functions.  This function
#' is given a table that includes function names.  It looks up information
#' previously stored about the function to identify the nonlocals set.
#' @param pfunctions a table of functions called.  The table contains a column
#'    for the function name and the library the function is defined inside of.
#'    When processing the table, functions from libraries or the base package
#'    are ignored.
#' @return a vector containing the names of non-local variables set by 
#'    a call to a function
#' @noRd
.ddg.get.nonlocals.set <- function (pfunctions) {
  if (.ddg.is.null.or.na (pfunctions) || nrow(pfunctions) == 0) {
    return(vector())
  } 
  
  localfunctions <- pfunctions [!grepl ("package:", pfunctions$ddg.lib), ]
  localfunctions <- localfunctions [localfunctions$ddg.lib != "base", ]
  localfunctions <- localfunctions$ddg.fun
  return (sapply (localfunctions, .ddg.lookup.nonlocals.set))
}

#' .ddg.lookup.nonlocals.set
#' 
#' Get the nonlocals set by calls to a function
#' @param funcname the name of the function
#' @return a vector containing the names of non-local variables set by 
#'    a call to the function whose name is passed in
#' @noRd

.ddg.lookup.nonlocals.set <- function (funcname) {
  ddg.func.defs <- .ddg.get ("ddg.function.defs")
  nonlocals <- ddg.func.defs [ddg.func.defs$func.name == funcname, "nonlocals.set"]
  if (length(nonlocals) == 0) return (character())
  return (nonlocals[[1]])
}

#' .ddg.find.nonlocals.used
#' 
#' Find all the nonlocal variables used in a function declaration.  This function
#' is passed a function declaration.  It examines the function line-by-line looking
#' for variables being set and being used.  On each line where a variable is
#' used that has not previously been assigned as a local, it remembers this as
#' a use of a non-local.
#' 
#' The result is approximate and conservative.  That is, if it is possible the
#' variable is non-local, it will be recorded as non-local.  The results are
#' approximate since static analysis of control constructs can not determine
#' which statements will be executed. 
#' 
#' @param funcdecl the parsed declaration of the function.  This should be
#'   everything from "function" to the closing curly brace
#' @return a vector containing all non-local variables use in the function
#' @noRd 
.ddg.find.nonlocals.used <- function (funcdecl) {
  # Parameters are locals  
  funcparams <- funcdecl[[2]]
  if (is.null (funcparams)) {
    vars.assigned <- character()
  }
  else {
    vars.assigned <- names (funcparams)
  }
  
  funcbody <- funcdecl[[3]]
  
  # If the body is a block, repeat for each statement in order.
  if (funcbody [[1]] == "{" && length(funcbody) > 1) {
    nonlocal.uses <- character()
    for (i in 2:length(funcbody)) {
      var.uses <- .ddg.find.var.uses (funcbody[[i]])
      nonlocal.uses <- unique (c (nonlocal.uses, setdiff (var.uses, vars.assigned)))
      var.assigned <- .ddg.find.simple.assign (funcbody[[i]])
      
      # Only add a variable as a local if it is not already a local and we have
      # not already seen the same name used as a non-local.
      if (var.assigned != "" && !(var.assigned %in% vars.assigned) 
        && !(var.assigned%in% nonlocal.uses)) {
          vars.assigned <- c(vars.assigned, var.assigned)
      }
    }
  }
  
  # The function body consists of a single statement.  In this case,
  # we just need to exclude parameters.
  else {
    var.uses <- .ddg.find.var.uses (funcbody)
    nonlocal.uses <- setdiff (var.uses, vars.assigned)
  }
  
  return (nonlocal.uses)
}

#' .ddg.get.nonlocals.used
#' 
#' Get the nonlocals used by calls to user-defined functions.  This function
#' is given a table that includes function names.  It looks up information
#' previously stored about the function to identify the nonlocals used.
#' @param pfunctions a table of functions called.  The table contains a column
#'    for the function name and the library the function is defined inside of.
#'    When processing the table, functions from libraries or the base package
#'    are ignored.
#' @return a vector containing the names of non-local variables used by 
#'    a call to a function
#' @noRd
.ddg.get.nonlocals.used <- function (pfunctions) {
  if (.ddg.is.null.or.na (pfunctions)) {
    return ()
  }

  else if( nrow(pfunctions) == 0) {
    return()
  } 
  
  localfunctions <- pfunctions [!grepl ("package:", pfunctions$ddg.lib), ]
  localfunctions <- localfunctions [localfunctions$ddg.lib != "base", ]
  localfunctions <- localfunctions$ddg.fun
  return (unlist (lapply (localfunctions, .ddg.lookup.nonlocals.used)))
}

#' .ddg.lookup.nonlocals.used
#' 
#' Get the nonlocals used by calls to a function
#' @param funcname the name of the function
#' @return a vector containing the names of non-local variables used by 
#'    a call to the function whose name is passed in
#' @noRd
.ddg.lookup.nonlocals.used <- function (funcname) {
  ddg.func.defs <- .ddg.get ("ddg.function.defs")
  nonlocals <- ddg.func.defs [ddg.func.defs$func.name == funcname, "nonlocals.used"]
  if (length(nonlocals) == 0) return (character())
  return (nonlocals[[1]])
}
