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
# the reading and writing of files, opening and closing of connections,
# and the creation of plots.
#
# In each case, there are a number of standard functions defined:
# 1. Make a data frame containing the names of functions to trace, and the 
#    names of the parameters identifying the function/connection
# 2. Define the function that trace calls.  This will filter out
#    some cases where we don't want to create nodes.  Then, it will
#    identify the file/connection being manipulated and add them to 
#    a list.
# 3. Define a function that goes through the list and creates the
#    file nodes and edges.
#
# Note that we don't want to combine functions 2 & 3 above because function
# 2 must be called while inside the read/write/close function.  Function 
# 3 cannot be called until the R statement containing the call completes
# so that the procedure node exists to connect the file node to.

#' Initialize the data needed to trace I/O functions
.ddg.init.iotrace <- function () {
  # Record the information about the input and output functions
  .ddg.set (".ddg.file.write.functions.df", .ddg.create.file.write.functions.df ())
  .ddg.set (".ddg.file.read.functions.df", .ddg.create.file.read.functions.df ())
  .ddg.set (".ddg.file.close.functions.df", .ddg.create.file.close.functions.df ())
  
  
  # Create an empty list for the input and output files
  .ddg.clear.input.file()
  .ddg.clear.output.file()
  
  # Start tracing of input and output functions
  # capture.output is called twice to capture the output that is going to standard output and to
  # standard error.  These are messages that say "Tracing..." and list each function being
  # traced.
  # Note that we need to use the RDataTracker::: notation for the functions for trace to call
  # so that it can find those functions without making them publicly available in 
  # the namespace.
  trace.oneOutput <- function (f) {capture.output(capture.output(trace (as.name(f), RDataTracker:::.ddg.trace.output, print=FALSE), type="message"))} 
  lapply(.ddg.get(".ddg.file.write.functions.df")$function.names, trace.oneOutput)
  trace.oneInput <- function (f) {capture.output(capture.output(trace (as.name(f), RDataTracker:::.ddg.trace.input, print=FALSE), type="message"))} 
  lapply(.ddg.get(".ddg.file.read.functions.df")$function.names, trace.oneInput)
  trace.oneClose <- function (f) {capture.output(capture.output(trace (as.name(f), RDataTracker:::.ddg.trace.close, print=FALSE), type="message"))} 
  lapply(.ddg.get(".ddg.file.close.functions.df")$function.names, trace.oneClose)
  
}

#' Stop tracing I/O calls.  This should be called when RDT finishes.
.ddg.stop.iotracing <- function () {
  # Stop tracing output functions.  Will this be a problem if ddg.save is called from the console?
  # capture.output is used to prevent "Untracing" messages from appearing in the output
  capture.output (untrace(.ddg.get(".ddg.file.write.functions.df")$function.names), type="message")
  capture.output (untrace(.ddg.get(".ddg.file.read.functions.df")$function.names), type="message")
  capture.output (untrace(.ddg.get(".ddg.file.close.functions.df")$function.names), type="message")
}

################### Helper functions ######################3

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

#' Returns true if there is a call to the passed in function anywhere on
#' the call stack.  
#' 
#' @param func The name of a function
.ddg.inside.call.to <- function (func) {
  is.call.to <- function (call) { 
    if (is.symbol(call[[1]])) {
      return (as.character(call[[1]]) == func)
    }
    return (FALSE)
  }
  calls.found <- sapply (sys.calls(), is.call.to )
  return (any (calls.found))
}

##################  Functions to handle tracing of read functions ##################

#' Initialize the information about functions that read from files
#' 
#' @return a data frame consisting of one row for each input function.
#' Each row contains the function name, and the name of the paramter that
#' holds the file argument.
.ddg.create.file.read.functions.df <- function () {
  # Functions that read files
  function.names <-
      c ("read.table", 
          "read.dcf", 
          "readRDS",
          "readLines", "readBin", "readChar", "scan", "load", "readRenviron")
  
  # The argument that represents the file name
  param.names <-
      c ("file", 
          "file", 
          "file",
          "con", "con", "con", "file", "file", "path")
  
  return (data.frame (function.names, param.names, stringsAsFactors=FALSE))
}

#' Clears out the list of input files.  This should be 
#' called on initialization and after the file nodes are created.
#' 
#' @return nothing
.ddg.clear.input.file <- function () {
  .ddg.set ("input.files", character())
}

#' Add a file name to the input list.
#' 
#' @param fname the name of the file to add to the list, or a connection object
#' 
#' @return nothing
.ddg.add.input.file <- function (fname) {
  input.files <- .ddg.get("input.files")
  
  if (.ddg.is.connection(fname)) {
    fname <- showConnections(TRUE)[as.character(fname), "description"]
  }
  
  # Only add the file to the list if it is not already there.  It could be 
  # there if there are multiple functions called indirectly in one R statement
  # that read from the same file, like readLines and scan.
  if (!(fname %in% input.files)) {
    .ddg.set ("input.files", c(input.files, list(fname)))
  }
}

#' Called when one of the input functions is called in a script.
#' This function saves the name of the file that is being read from in
#' the input.files variable so that the proper nodes can be created when
#' the statement doing the output is complete.
#' 
#' @return nothing 
.ddg.trace.input <- function () {
  
  # Get the frame corresponding to the output function being traced
  frame.number <- .ddg.get.traced.function.frame.number()
  
  # Filter out some calls based on what function called the input function.
  # The is.symbol test is used because it is possible that the caller is a 
  # closure and thus does not have a name.
  input.caller <- sys.call (frame.number - 1)[[1]]
  if (is.symbol (input.caller)) {
    input.caller.name <- as.character(input.caller)
    
    # When ddg.source is called for the main script it is in frame 4, and
    # the input function is in frame 5.  
    # If a script sources another script, we see ddg.source on the stack twice
    # The second occurrence will be at a higher frame number.
    # We do not want a file node for the main script, since is not an input 
    # to the script, but we do for calls to source within the main script.
    # These are translated to ddg.source when we execute.
    if (input.caller.name == "ddg.source") {
      if (frame.number == 5) {
        return()
      }
    }
    
    # Check if the function that called the input function is any other ddg function.
    # If it is, ignore this call.  .ddg.load.history is an example of a 
    # function that does input that we would want to ignore.  
    else if (startsWith (input.caller.name, "ddg") || startsWith (input.caller.name, ".ddg")) {
      return()
    }
    
    # Don't collect provenance when loading library packages
    else if (.ddg.inside.call.to ("library")) {
      return()
    }
  }
  
  # Get the name of the input function
  call <- sys.call (frame.number)
  fname <- as.character(call[[1]])
  #print (paste ("Input function traced: ", fname))
  
  # Get the name of the file parameter for the input function
  file.read.functions <- .ddg.get (".ddg.file.read.functions.df")
  file.param.name <- file.read.functions$param.names[file.read.functions$function.names == fname]
  #print (paste ("Input file parameter:", file.param.name))
  
  # Get the value of the file parameter  
  input.file.name <- eval (as.symbol(file.param.name), env = sys.frame(frame.number))
  #print (paste ("input.file.name =", input.file.name))
  
  # Save the file name so the file node can be created when the statement is complete.
  # we do not want to create the nodes because the procedure node to connect to does not
  # exist yet.
  .ddg.add.input.file (input.file.name)
}

#' Creates file nodes and data in edges for any files that were read during 
#' execution of the last R statement
#' 
#' @return nothing
.ddg.create.file.read.nodes.and.edges <- function () {
  # Get the list of files that have been read by the last statement.
  files.read <- .ddg.get ("input.files")
  
  # Adds the files read to ddg.infilenodes for use in determining reads
  # and writes in the hashtable.
  .ddg.set("ddg.infilenodes", c(.ddg.get("ddg.infilenodes"), files.read))
  
  for (file in files.read) {
    # Use URL node for URLs and for socket connections
    if (grepl ("://", file) || startsWith (file, "->"))
    {
      if (grepl ("://", file) ) {
        # Save the Web page
        url.copy <- .ddg.url.copy (file)
        .ddg.url.node(file, url.copy)
      }
      else {
        # Maybe we should change the node type to be "Remote" or something?
        .ddg.url.node(file, file)
      }
      .ddg.data2proc(file, environmentName(.GlobalEnv))
    }
    
    # Handle files
    else {
      # Only create the node and edge if there actually is a file
      if (file.exists(file)) {
        # Create the file node and edge
        ddg.file(file)
        ddg.data.in(basename(file))
      }
      
      # If the filename contains a :, then it is referencing a file within 
      # a zip file, so checck that the zip file exists.      
      else if (grepl(":", file)) {
        zipfile <- sub (":.*", "", file)
        if (file.exists (zipfile)) {
          # Create the file node and edge
          ddg.file(zipfile, file)
          ddg.data.in(file)
        }
      }
    }
  }
  
  # Clear the list of input files now that they have been handled.
  .ddg.clear.input.file ()
}

##################  Functions to handle tracing of write functions ##################


#' Initialize the information about functions that write to files
#' 
#' @return a data frame consisting of one row for each output function.
#' Each row contains the function name, and the name of the paramter that
#' holds the file argument.
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
  
  return (data.frame (function.names, param.names, stringsAsFactors=FALSE))
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
  
  # Only add the file to the list if it is not already there.  It could be 
  # there if there are multiple functions called indirectly in one R statement
  # that write to the same file.
  if (!(fname %in% output.files)) {
    .ddg.set ("output.files", append(output.files, fname))
  }
}

#' Called when one of the output functions is called in a script.
#' This function saves the name of the file that is being written in 
#' the output.files variable so that the proper nodes can be created when
#' the statement doing the output is complete.
#' 
#' @return nothing
.ddg.trace.output <- function () {
  #print ("In .ddg.trace.output")
  
  # Get the frame corresponding to the output function being traced
  frame.number <- .ddg.get.traced.function.frame.number()
  
  # Check if the function that called the output function is a ddg function.
  # If it is, ignore this call.  The is.call check is here because it is
  # possible that the caller is a closure and thus does not have a name.
  output.caller <- sys.call (frame.number - 1)[[1]]
  if (is.symbol (output.caller)) {
    output.caller.name <- as.character(output.caller)
    if (startsWith (output.caller.name, "ddg") || startsWith (output.caller.name, ".ddg")) {
      return()
    }
  }
  
  # Check that the function is not being called due to saving a snapshot file.
  if (length (grep ("^.ddg.snapshot", sys.calls())) > 0) {
    return()
  }
  
  # Get the name of the output function
  call <- sys.call (frame.number)
  fname <- as.character(call[[1]])
  # print (paste ("Output function traced: ", fname))
  
  # Get the name of the file parameter for the output function
  file.write.functions <- .ddg.get (".ddg.file.write.functions.df")
  file.param.name <- file.write.functions$param.names[file.write.functions$function.names == fname]
  # print (paste ("Output file parameter:", file.param.name))
  
  # Get the value of the file parameter  
  output.file.name <- eval (as.symbol(file.param.name), env = sys.frame(frame.number))
  # print (paste ("output.file.name =", output.file.name))

  # Save the file name so the file node can be created when the statement is complete.
  # we do not want to create the nodes because the procedure node to connect to does not
  # exist yet, and the file has not been written to yet.
  .ddg.add.output.file (output.file.name)
}

#' Creates file nodes and data out edges for any files that are written by
#' the last statement executed.  It knows what the files are by looking
#' in the output.files variable stored in the ddg environment.
#' 
.ddg.create.file.write.nodes.and.edges <- function () {
  # Get the list of files that have been written by the last statement.
  files.written <- .ddg.get ("output.files")
  
  for (file in files.written) {
    #print (paste ("file written: ", file))
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


################ Functions to manage connections ####################3

#' Returns true if the object passed in is a connection
#'
#' @param value an R object
#'
#' @return true if the R object is a connection used to do I/O
.ddg.is.connection <- function (value) {
  return ("connection" %in% class(value))
}

#' Returns a matrix containing the list of open connections
#' 
#' @return a matrix containing information about all open connections
#' 
.ddg.get.open.connections <- function () { 
  return (showConnections(FALSE))
}

#' Returns the thing that the connection connects to.  This can be a 
#' filename, URL, socket, etc.
#' 
#' @param conn a connection.  This can either be a connection object
#' or the number associated with the connection.  
#' 
#' @return a description of the input/output connected to
.ddg.get.connection.description <- function (conn) {
  return (showConnections(TRUE)[as.character(conn), "description"])  
}

#' Returns true if the connection is still open.
#' 
#' @param conn a connection.  This can either be a connection object
#' or the number associated with the connection.  
#' 
#' @return TRUE if the connection is open
.ddg.is.connection.open <- function (conn) {
  return (showConnections(TRUE)[as.character(conn), "isopen"] == "opened")  
}

#' Returns true if the given connection was opened for reading, whether or not
#' the connection is currently open.
#' 
#' @param conn a connection.  This can either be a connection object
#' or the number associated with the connection.
#' 
#' @return true if the given connection is readable
.ddg.can.read.connection <- function (conn) {
  return (showConnections(TRUE)[as.character(conn), "can read"] == "yes")  
}

#' Returns true if the given connection was opened for writing, whether or not
#' the connection is currently open.
#' 
#' @param conn a connection.  This can either be a connection object
#' or the number associated with the connection.
#' 
#' @return true if the given connection is writable
.ddg.can.write.connection <- function (conn) {
  return (showConnections(TRUE)[as.character(conn), "can write"] == "yes")  
}

#' Initialize the information about functions that read from files
#' 
#' @return a data frame containing 2 columns:  
#'     names of functions that close connections, and
#'     name of the parameter that holds the connection
.ddg.create.file.close.functions.df <- function () {
  # Functions that close connections
  function.names <- c ("close.connection")
  
  # The argument that represents the connection name
  param.names <- c ("con")
  
  return (data.frame (function.names, param.names, stringsAsFactors=FALSE))
}

#' Called when any of the functions to close connections is called.
#' This will add the description of any connection that was open for
#' writing to the list for which output file nodes should be created.
#' There are a few exceptions where a close function is called but
#' no node will be created:  if called directly from a ddg function, or if
#' any call on the stack is to capture.output, parse, or .ddg.snapshot,
#' or if there is any read or write function on the call stack.  If one of 
#' the read or write functions is closing the connection, then we will 
#' already be creating the right nodes.
#' 
#' @return nothing
.ddg.trace.close <- function () {
  #print ("In .ddg.trace.close")
  
  # Get the frame corresponding to the close function being traced
  frame.number <- .ddg.get.traced.function.frame.number()
  
  # Check if the function that called the close function is a ddg function.
  # If it is, ignore this call.  The is.symbol check is here because it is
  # possible that the caller is a closure and thus does not have a name.
  close.caller <- sys.call (frame.number - 1)[[1]]
  if (is.symbol (close.caller)) {
    close.caller.name <- as.character(close.caller)
    if (startsWith (close.caller.name, "ddg") || startsWith (close.caller.name, ".ddg")) {  # 
      return()
    }
  }
  
  # Check that the function is not being called due to a call to capture output (used to 
  # hide standard output), parse (used to read the script being executed), or .ddg.snapshot
  # (used to save copies of complex data values)
  if (.ddg.inside.call.to ("capture.output") || .ddg.inside.call.to ("parse") 
      || .ddg.inside.call.to (".ddg.snapshot")) {
    return()
  }
  
  # Check that we are not inside any read or write functions.  If we are,
  # the appropriate nodes will be created by those functions
  read.funs <- .ddg.get(".ddg.file.read.functions.df")$function.names
  if (any (sapply (read.funs, .ddg.inside.call.to))) {
    return()
  }
  
  write.funs <- .ddg.get(".ddg.file.write.functions.df")$function.names
  if (any (sapply (write.funs, .ddg.inside.call.to))) {
    return()
  }
  
  # Get the name of the close function
  call <- sys.call (frame.number)
  fname <- as.character(call[[1]])
  
  # Get the name of the connection parameter for the close function
  file.close.functions <- .ddg.get (".ddg.file.close.functions.df")
  file.param.name <- file.close.functions$param.names[file.close.functions$function.names == fname]
  
  # Get the value of the connection parameter  
  close.conn <- eval (as.symbol(file.param.name), env = sys.frame(frame.number))
  
  # If the connection was opened for writing, then add the connection
  # to the list for which we create output file nodes.  We do not need 
  # to do anything if the connection was only open for reading because the
  # read code will have already created the node.
  if (.ddg.can.write.connection (close.conn)) {
    .ddg.add.output.file (.ddg.get.connection.description(close.conn))
  }
}

#' Create nodes for any writable connections that are open. This is intended to 
#' be called when a script is finishing, so that we will have the connections
#' associated with files that may have been written to, but not closed.
#' 
#' @return nothing
.ddg.create.file.nodes.for.open.connections <- function () {
  openConns <- .ddg.get.open.connections()
  lapply (openConns[openConns[, 'can write'] == "yes", "description"], .ddg.add.output.file)
  .ddg.create.file.write.nodes.and.edges ()
}

