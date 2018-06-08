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

############################ IOTrace.R #############################

# Contains the functions needed to trace input and output operations, including
# the reading and writing of files, and the creation of plots.

#' Initialize the information about functions that read from files
#' 
#' @return a data frame consisting of one row for each output function.
#' Each row contains the function name, the name of the paramter that
#' holds the file argument, and the position of the parameter that
#' contains the file argument.
.ddg.create.file.write.functions.df <- function () {
  # Functions that write files.  We include the lowest level functions
  # used in R.  For example, write.csv is not in the list because it
  # uses write.table to do the output.
  function.names <-
      c ("write.table", "write", "writeLines",
          "writeChar", "writeBin", 
          "saveRDS", "save", "dput", "dump")
  
  # The argument that represents the file name
  param.names <-
      c ("file", "file", "con", 
          "con", "con", 
          "file", "file", "file", "file")
  
  # Position of the file parameter if it is passed by position
  param.pos <-
      c (2, 2, 2,
          2, 2,
          2, 0, 2, 2)
  
  return (data.frame (function.names, param.names, param.pos, stringsAsFactors=FALSE))
}

#' Initialize the data needed to trace I/O functions
.ddg.init.iotrace <- function () {
  # Record the information about the output functions
  .ddg.set (".ddg.file.write.functions.df", .ddg.create.file.write.functions.df ())
  
  # Create an empty list for the output files
  .ddg.clear.output.file()
  
  # Start tracing of output functions
  # capture.output is called twice to capture the output that is going to standard output and to
  # standard error.  These are messages that say "Tracing..." and list each function being
  # traced.
  trace.one <- function (f) {capture.output(capture.output(trace (as.name(f), ddg.trace.output, print=FALSE), type="message"))} 
  lapply(.ddg.get(".ddg.file.write.functions.df")$function.names, trace.one)
}

.ddg.stop.iotracing <- function () {
  # Stop tracing output functions.  Will this be a problem if ddg.save is called from the console?
  # capture.output is used to prevent "Untracing" messages from appearing in the output
  capture.output (untrace(.ddg.get(".ddg.file.write.functions.df")$function.names), type="message")
}

#' Clears out the list of output files.  This should be 
#' called on initialization and after the file nodes are created.
#' 
#' @return nothing
.ddg.clear.output.file <- function () {
  .ddg.set ("output.files", character())
}

#' Add a file name to the output list.
#' 
#' @param fname the name of the file to add to the list, or a connection object
#' 
#' @return nothing
.ddg.add.output.file <- function (fname) {
  output.files <- .ddg.get("output.files")
  .ddg.set ("output.files", append(output.files, fname))
}

#' Called when one of the output functions is called in a script.
#' This function saves the name of the file that is being written in 
#' the output.files variable so that the proper nodes can be created when
#' the statement doing the output is complete.
#' 
#' This function needs to be public so that trace can find it when the
#' traced function executes.
#' 
#' @return nothing
ddg.trace.output <- function () {

  # Get the frame corresponding to the output function being traced
  frame.number <- .ddg.get.traced.function.frame.number()
  
  # Check if the function that called the output function is a ddg function.
  # If it is, ignore this call.
  output.caller.name <- as.character(sys.call (frame.number - 1)[[1]])
  if (startsWith (output.caller.name, "ddg") || startsWith (output.caller.name, ".ddg")) {
    return()
  }
  
  # Check that the function is not being called due to saving a snapshot file.
  if (length (grep ("^.ddg.snapshot", sys.calls())) > 0) {
    return()
  }
  
  # Get the name of the output function
  call <- match.call (sys.function(frame.number)@original, sys.call (frame.number), 
      expand.dots = TRUE, envir = sys.frame(frame.number))
  fname <- as.character(call[[1]])
  # print (paste ("Output function traced: ", fname))
  
  # Get the name of the file parameter for the output function
  file.write.functions <- .ddg.get (".ddg.file.write.functions.df")
  file.param.name <- file.write.functions$param.names[file.write.functions$function.names == fname]
  # print (paste ("Output file parameter:", file.param.name))
  
  # Get the name of the file or connection being written to
  output.file.argument <- call[[file.param.name]]
  
  # If there is no binding for the parameter name, look for the 
  # parameter by position.
  if (is.null (output.file.argument)) {
    file.param.pos <- file.write.functions$param.pos[file.write.functions$function.names == fname]
    
    # If there is no parameter in that position, it must 
    # be using a default value, like stdout, so just return
    if (file.param.pos >= length(call)) return()
    
    output.file.argument <- call[[file.param.pos + 1]]
  }
  
  # If we have a name instead of a value, get the value
  if (is.symbol (output.file.argument)) {
    output.file.name <- get (as.character(output.file.argument), sys.frame(frame.number-1))
  }
  
  # We already have a value
  else {
    output.file.name <- output.file.argument
  }
  
  # Save the file name so the file node can be created when the statement is complete.
  # we do not want to create the nodes because the procedure node to connect to does not
  # exist yet, and the file has not been written to yet.
  .ddg.add.output.file (output.file.name)
}

#' Get the frame number for a function being traced
#' 
#' @return the frame number of the function being traced.  
#' Returns NULL if there is no occurrence of .doTrace
#' on the stack.
.ddg.get.traced.function.frame.number <- function() {
  calls <- sys.calls()
  calls <- mapply( `[[` , calls , 1 , SIMPLIFY = TRUE )
  
  doTrace.frame <- which( calls == ".doTrace" )
  
  if( length(doTrace.frame) > 0 )
  {
    return (doTrace.frame - 1)
  }
  else
  {
    return (NULL)
  }
  
}

#' Creates file nodes and data in edges for any files that are written by
#' the last statement executed.  It knows what the files are by looking
#' in the output.files variable stored in the ddg environment.
#' 
.ddg.create.file.write.nodes.and.edges <- function () {
  # Get the list of files that have been written by the last statement.
  files.written <- .ddg.get ("output.files")
  
  for (file in files.written) {
    if (.ddg.is.connection(file)) {
      conn <- as.numeric(file)
      # If it is a closed connection, use the file it is connected to
      # If it is still open, don't use it because the contents on disk won't
      # be correct until it is closed.
      if (.ddg.get.connection.isopen(conn)) {
        next
      }
      file <- .ddg.get.connection.description(conn)
    }
    
    # Check that the file exists.  If it does, we will assume that
    # it was created by the write call that just executed.
    if (file.exists (file)) {
      # Create the file node and edge
      ddg.file.out (file)
    }
  }

  # Clear the list of output files now that they have been handled.
  .ddg.clear.output.file ()
}
