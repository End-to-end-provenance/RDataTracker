
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

#########################################################

# Contains functions used to manage the table of return values.
#
# Information about return values are stored in a table, with 1
# row for each return statement executed
#
# The columns of the data frame are:
#
# ddg.call - the text of the function call that was made, like f7(n)
# line - the start line of the statement that contained the function call
# return.used - if TRUE, it means that the value that was returned by this call
#    is already linked to.  This is important to be able to distinguish recursive calls
# return.node.id - the id of the data node that holds the return value
#
# ddg.num.returns is the number of return values in the table


#' .ddg.create.return.value.rows creates a data frame of empty rows to put in 
#' the return value table. It is faster to add a bunch of empty rows and update 
#' them than to add one row at a time.
#' @param size the number of rows to create
#' @return A data frame with 4 columns:
#' ddg.call - the text of the function call that was made, like f7(n)
#' line - the start line of the statement that contained the function call
#' return.used - if TRUE, it means that the value that was returned by this call
#'    is already linked to.  This is important to be able to distinguish recursive calls
#' return.node.id - the id of the data node that holds the return value

.ddg.create.return.value.rows <- function (size=100) {
  return (data.frame(
          ddg.call=character(size),
          line = integer(size),
          return.used = logical(size),
          return.node.id = integer(size),
          stringsAsFactors=FALSE))
}

#' .ddg.init.return.values initializes the data used to manage return values
#' @return nothing

.ddg.init.return.values <- function () {
  .ddg.set(".ddg.return.values", .ddg.create.return.value.rows(100))
  .ddg.set(".ddg.num.returns", 0)
}

#' .ddg.save.return.value.table writes the return value information to a csv table.
#' Useful for debugging. The file will be in the debug directory in a file called
#'  function-returns.csv
#' @return nothing

.ddg.save.return.value.table <- function () {
  # Save function return table to file.
  fileout <- paste(.ddg.path.debug(), "/function-returns.csv", sep="")
  ddg.returns <- .ddg.get(".ddg.return.values")
  
  # Remove the empty rows from the table.
  ddg.returns2 <- ddg.returns[ddg.returns$return.node.id > 0, ]
  utils::write.csv(ddg.returns2, fileout, row.names=FALSE)
}


#' .ddg.get.matching.return.value.nodes finds the return value nodes that match 
#' the function calls made on the line that is passed in.
#' @param command a DDGStatement object containing one or more function calls
#' @return the return node ids of the data nodes that
#' correspond to the values returned by the function calls passed in

.ddg.get.matching.return.value.nodes <- function (command) {
  # Find the return values that have not been used yet.  If the start line of
  # the function call is known, only keep entries for that line.
  returns <- .ddg.get(".ddg.return.values")
  if (!is.na(command@pos@startLine)) {
    unused.returns <- 
      returns[!returns$return.used & returns$return.node.id > 0 & 
              !is.na(returns$line) & returns$line == command@pos@startLine, ]
  }
  else {
    unused.returns <- returns[!returns$return.used & returns$return.node.id > 0, ]
  }
  if (nrow(unused.returns) == 0) return()
  #print (paste(".ddg.get.matching.return.value.nodes: unused.returns:", unused.returns))
  
  # See which of these are called from the command we are
  # processing now.
  unused.calls <- unused.returns$ddg.call
  command.text <- gsub(" ", "", command@text)
  uses <- sapply(unused.calls, function(call) {grepl(call, command.text, fixed=TRUE)})
  #print (paste (".ddg.link.function.returns: uses:", uses))
  
  # Extracts for the return value nodes.
  return (unused.returns[uses, ]$return.node.id)
}

#' .ddg.set.return.value.used marks a return value as being used.
#' @param data.num the data node id for the return value being used
#' @return nothing

.ddg.set.return.value.used <- function(data.num) {
  returns <- .ddg.get(".ddg.return.values")
  returns$return.used[returns$return.node.id == data.num] <- TRUE
  .ddg.set(".ddg.return.values", returns)
}

#' .ddg.add.to.return.values adds a new entry to the return value table
#' @param call.text the text of the function call
#' @return nothing

.ddg.add.to.return.values <- function (call.text) {
  ddg.return.values <- .ddg.get(".ddg.return.values")
  ddg.num.returns <- .ddg.get(".ddg.num.returns")
  
  # Make the table bigger if necessary
  if (nrow(ddg.return.values) == ddg.num.returns) {
    new.rows <- .ddg.create.return.value.rows (100)
    .ddg.add.rows(".ddg.return.values", new.rows)
    ddg.return.values <- .ddg.get(".ddg.return.values")
  }
  
  # Update the table.
  ddg.num.returns <- ddg.num.returns + 1
  ddg.return.values$ddg.call[ddg.num.returns] <- call.text
  ddg.return.values$return.used[ddg.num.returns] <- FALSE
  ddg.return.values$return.node.id[ddg.num.returns] <- .ddg.dnum()
  
  # Determine the line number of the call
  ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
  ddg.return.values$line[ddg.num.returns] <- 
      if (length(ddg.cur.cmd.stack) == 0) NA
      else ddg.cur.cmd.stack[length(ddg.cur.cmd.stack) - 1][[1]]@pos@startLine
  
  # Save the values
  .ddg.set(".ddg.return.values", ddg.return.values)
  .ddg.set(".ddg.num.returns", ddg.num.returns)
}
