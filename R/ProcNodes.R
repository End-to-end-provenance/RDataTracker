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

################  Functions involved in managing the procedure nodes ################
#
# Procedure nodes are stored in a data frame, with one row for each node.
# The columns of the data frame are:
#
# type - Operation, Start, Finish, Binding, Incomplete
# num - a unique number
# name - the label for the node
# value - often this is the same as the name
# return.linked - If TRUE, this means the procedure node has an output data node
#   representing its return value.  This is needed to handle recursive functions
#   correctly.
# time - when the node was created
# snum - the script number the statement is from
# startLine - the line on the script where the statement starts
# startCol - the column of the script where the statement starts
# endLine - the line on the script where the statement ends
# endCol - the line on the script where the statement ends
#
# ddg.pnum is the number associated with the last procedure node created

#' Initializes the information needed to manage procedure nodes.
#' 
#' @return nothing
.ddg.init.proc.nodes <- function () {
  .ddg.set("ddg.proc.nodes", .ddg.create.proc.node.rows())
  
  # Initialize the procedure node counter
  .ddg.set("ddg.pnum", 0)
}

#' .ddg.is.proc.type returns TRUE for any type of procedure node.
#' This is used for type-checking.
#' 
#' @param type procedure node type.
#' @returnType logical
#' @return true for any type of procedure node
.ddg.is.proc.type <- function(type) {
  return(type %in% c("Operation", "Start", "Finish", "Binding", "Incomplete"))
}

#' Return the counter used to assign procedure node ids
#' @returnType integer 
#' @return the data node id of the last procedure node created
.ddg.pnum <- function() {
  return (.ddg.get("ddg.pnum"))
}

#' Create a data frame of empty rows to put in the procedure node table.
#' It is faster to add a bunch of empty rows and update them than to 
#' add one row at a time
#' @param size the number of rows to add
#' @returnType a dataframe 
#' @return a data frame with size rows, with all columns being empty vectors
.ddg.create.proc.node.rows <- function (size=100) {
  return (data.frame(
          ddg.type = character(size),
          ddg.num = numeric(size),
          ddg.name = character(size),
          ddg.value = character(size),
          ddg.return.linked = logical(size),
          ddg.time = numeric(size),
          ddg.snum = numeric(size),
          ddg.startLine = numeric(size),
          ddg.startCol = numeric(size),
          ddg.endLine = numeric(size),
          ddg.endCol = numeric(size),
          stringsAsFactors=FALSE))
}

#' Returns the procedure node table
#' @returnType a data frame
#' @return the procedure node table
.ddg.proc.node.table <- function() {
  return (.ddg.get("ddg.proc.nodes"))
}

#' Returns the filled rows of the procedure node table
#' @returnType a data frame
#' @return the filled rows of the procedure node table
.ddg.proc.nodes <- function() {
  ddg.proc.nodes <- .ddg.get("ddg.proc.nodes")
  return (ddg.proc.nodes [ddg.proc.nodes$ddg.num > 0, ])
}

#' Mark the procedure node as being linked to a return value
#' 
#' @param pn the id of the procedure node to mark
#' @return nothing
.ddg.proc.node.returned <- function(pn) {
  ddg.proc.nodes <- .ddg.proc.node.table()
  ddg.proc.nodes$ddg.return.linked[pn] <- TRUE
  .ddg.set("ddg.proc.nodes", ddg.proc.nodes)
}


#' .ddg.proc.node.exists returns true if there is a
#' procedure node with the given name
#' 
#' @param pname the name of a procedure node to look up
#' @returnType logical
#' @return true if there is a node with the given name
.ddg.proc.node.exists <- function(pname) {
  ddg.proc.nodes <- .ddg.proc.node.table()
  matching.nodes <- ddg.proc.nodes[ddg.proc.nodes$ddg.name == pname, ]
  return (nrow(matching.nodes) > 0)
}

#' .ddg.proc.number gets the number of the nearest preceding
#' procedure node with the matching name
#' 
#' @param pname name of procedure node to look for
#' @param find.unreturned.function if true, only return the number if the
#'    procedure has not previously been linked to a return value
#' @returnType integer
#' @return the id of a procedure node matching the name, or 0 if none was found
.ddg.proc.number <- function(pname, find.unreturned.function=FALSE) {
  #print (paste0("Looking for function ", pname))
  ddg.proc.nodes <- .ddg.proc.node.table()
  
  if (find.unreturned.function) {
    matching <- ddg.proc.nodes[ddg.proc.nodes$ddg.name == pname & !ddg.proc.nodes$ddg.return.linked, ]
  }
  else {
    matching <- ddg.proc.nodes[ddg.proc.nodes$ddg.name == pname, ]
  }
  
  if (nrow (matching) > 0) {
    return (matching$ddg.num[nrow(matching)])
  }

  # Error message if no match is found.
  #print ("Returning error!")
  error.msg <- paste("No procedure node found for", pname)
  if (.ddg.debug.lib()) print (sys.calls())
  .ddg.insert.error.message(error.msg)
  return(0)
}

#' .ddg.proc.name returns the name of a procedure node. It returns a
#' empty string if no match is found.
#' 
#' @param pnum node number in procedure node table.
#' @returnType string
#' @return name of the procedure node with the given id.  Returns an empty
#' string if there is no node with the given id.
.ddg.proc.name <- function(pnum) {
  if (pnum < 1 || pnum > .ddg.pnum()) {
    error.msg <- paste("No name found for procedure number", pnum)
    .ddg.insert.error.message(error.msg)
    return ("")
  }
  
  return(.ddg.proc.node.table()$ddg.name[pnum])
}

#' Write the procedure nodes to a csv table.  Useful for debugging.
#' The file will be in the debug directory in a file called procedure-nodes.csv
.ddg.save.debug.proc.nodes <- function () {
  # Save procedure nodes table to file.
  fileout <- paste(.ddg.path.debug(), "/procedure-nodes.csv", sep="")
  ddg.proc.nodes <- .ddg.proc.nodes()
  write.csv(ddg.proc.nodes, fileout, row.names=FALSE)
}

#' Records a new procedure node in the procedure node table
#' @param ptype  the type of the node
#' @param pname the name for the node
#' @param pvalue ???
#' @param ptime elapsed time
#' @param snum the number the script is in (main script = 0)
#' @param pos the starting and ending line and column location of the statement
#' 
#' @return nothing
.ddg.record.proc <- function(ptype, pname, pvalue, ptime, snum=NA, pos=NA) {
  if (!.ddg.is.proc.type (ptype)) {
    print (paste (".ddg.record.proc: bad value for ptype - ", ptype))
  }
  
  # Increment procedure node counter.
  ddg.pnum <- .ddg.inc("ddg.pnum")
  
  # If the table is full, make it bigger.
  ddg.proc.nodes <- .ddg.proc.node.table()
  if (nrow(ddg.proc.nodes) < ddg.pnum) {
    ddg.proc.nodes <- .ddg.add.rows("ddg.proc.nodes", .ddg.create.proc.node.rows())
  }
  
  # Fill in the next row
  ddg.proc.nodes$ddg.type[ddg.pnum] <- ptype
  ddg.proc.nodes$ddg.num[ddg.pnum] <- ddg.pnum
  ddg.proc.nodes$ddg.name[ddg.pnum] <- pname
  ddg.proc.nodes$ddg.value[ddg.pnum] <- pvalue
  ddg.proc.nodes$ddg.time[ddg.pnum] <- ptime
  
  ddg.proc.nodes$ddg.snum[ddg.pnum] <- snum
  if (is.object(pos) && length(pos@startLine == 1)) {
    ddg.proc.nodes$ddg.startLine[ddg.pnum] <- pos@startLine
    ddg.proc.nodes$ddg.startCol[ddg.pnum] <- pos@startCol
    ddg.proc.nodes$ddg.endLine[ddg.pnum] <- pos@endLine
    ddg.proc.nodes$ddg.endCol[ddg.pnum] <- pos@endCol
  }
  else {
    ddg.proc.nodes$ddg.startLine[ddg.pnum] <- NA
    ddg.proc.nodes$ddg.startCol[ddg.pnum] <- NA
    ddg.proc.nodes$ddg.endLine[ddg.pnum] <- NA
    ddg.proc.nodes$ddg.endCol[ddg.pnum] <- NA
  }
  
  # Save the table
  .ddg.set("ddg.proc.nodes", ddg.proc.nodes)
  
  if (.ddg.debug.lib()) {
    print (paste("Adding procedure node", ddg.pnum, "named", pname))
  }
}

#' .ddg.proc.node creates a procedure node.
#' 
#' @param ptype type of procedure node.
#' @param pname name of procedure node.
#' @param pvalue (optional) value of procedure node.
#' @param functions.called vector of names of functions called in this procedure node
#' @param console (optional) if TRUE, console mode is enabled.
#' @param cmd the DDGStatement corresponding to this procedure node
.ddg.proc.node <- function(ptype, pname, pvalue="", functions.called=NULL, console=FALSE,
    cmd = NULL) {

  if (!.ddg.is.proc.type (ptype)) {
    print (paste (".ddg.proc.node: bad value for ptype - ", ptype))
  }
  
  # We're not in a console node but we're capturing data
  # automatically.
  if (.ddg.enable.console()) {
    
    # Capture graphic output of previous procedure node.
    if (!console && !.ddg.enable.source() && interactive()) {
      .ddg.console.node()
    }
  }
  
  if (is.null(cmd)) {
    snum <- NA
    pos <- NA
  }
  else {
    snum <- cmd@script.num
    pos <- cmd@pos
  }
  
  # Record start & finish information
  if (ptype == "Start") {
    .ddg.starts.open <- .ddg.get (".ddg.starts.open")
    .ddg.starts.open <- c(.ddg.starts.open, pname)
    .ddg.set (".ddg.starts.open", .ddg.starts.open)
  }
  else if (ptype == "Finish") {
    .ddg.starts.open <- .ddg.get (".ddg.starts.open")
    num.starts.open <- length(.ddg.starts.open)
    if (num.starts.open > 0) {
      last.start.open <- .ddg.starts.open[num.starts.open]
      if (num.starts.open > 1) {
        .ddg.starts.open <- .ddg.starts.open[1:num.starts.open-1]
      }
      else {
        .ddg.starts.open <- vector()
      }
      .ddg.set (".ddg.starts.open", .ddg.starts.open)
      if (last.start.open != pname) {
        .ddg.insert.error.message("Start and finish nodes do not match")
      }
    }
    else {
      .ddg.insert.error.message("Attempting to create a finish node when there are no open blocks")
    }
  }
  .ddg.set(".ddg.last.proc.node.created", paste(ptype, pname))
  
  ptime <- .ddg.elapsed.time()
  
  # Record in procedure node table
  .ddg.record.proc(ptype, pname, pvalue, ptime, snum, pos)
  
  # append the function call information to function nodes
  if( !is.null(functions.called) && !is.na(functions.called)) {
    pfunctions <- .ddg.get.function.info(functions.called)
    .ddg.add.to.function.table (pfunctions)
  }
  if (.ddg.debug.lib()) print(paste("proc.node:", ptype, pname))
}


