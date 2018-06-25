
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
  #print ("Initializing io tracing")
  
  # Record the information about the input and output functions
  .ddg.set (".ddg.file.write.functions.df", .ddg.create.file.write.functions.df ())
  .ddg.set (".ddg.file.read.functions.df", .ddg.create.file.read.functions.df ())
  .ddg.set (".ddg.file.close.functions.df", .ddg.create.file.close.functions.df ())
  .ddg.set (".ddg.graphics.functions.df", .ddg.create.graphics.functions.df ())
  .ddg.set (".ddg.graphics.update.functions.df", ls(which(search()=="package:graphics")))
  .ddg.set (".ddg.add.device.output", FALSE)
  .ddg.set (".ddg.add.device.io", FALSE)
  .ddg.set (".ddg.add.device.close", FALSE)
  .ddg.set (".ddg.no.graphics.file", FALSE)
  
  
  # Create an empty list for the input, output, and files
  .ddg.clear.input.file()
  .ddg.clear.output.file()
  .ddg.clear.graphics.file()
  .ddg.clear.device.nodes ()
  .ddg.create.device.table ()
  
  # Start tracing of input and output functions
  # capture.output is called twice to capture the output that is going to standard output and to
  # standard error.  These are messages that say "Tracing..." and list each function being
  # traced.
  # Note that we need to use the RDataTracker::: notation for the functions for trace to call
  # so that it can find those functions without making them publicly available in 
  # the namespace.
  trace.oneOutput <- function (f) {capture.output(capture.output(trace (as.name(f), RDataTracker:::.ddg.trace.output, print=FALSE), type="message"))} 
  #trace.oneOutput <- function (f) {capture.output(capture.output(trace (as.name(f), RDataTracker:::.ddg.trace.output), type="message"))} 
  lapply(.ddg.get(".ddg.file.write.functions.df")$function.names, trace.oneOutput)

  trace.oneInput <- function (f) {capture.output(capture.output(trace (as.name(f), RDataTracker:::.ddg.trace.input, print=FALSE), type="message"))} 
  #trace.oneInput <- function (f) {capture.output(capture.output(trace (as.name(f), RDataTracker:::.ddg.trace.input), type="message"))} 
  lapply(.ddg.get(".ddg.file.read.functions.df")$function.names, trace.oneInput)

  trace.oneClose <- function (f) {capture.output(capture.output(trace (as.name(f), RDataTracker:::.ddg.trace.close, print=FALSE), type="message"))} 
  #trace.oneClose <- function (f) {capture.output(capture.output(trace (as.name(f), RDataTracker:::.ddg.trace.close), type="message"))} 
  lapply(.ddg.get(".ddg.file.close.functions.df")$function.names, trace.oneClose)

  #print ("Tracing graphics open")
  trace.oneGraphicsOpen <- function (f) {capture.output(capture.output(trace (as.name(f), RDataTracker:::.ddg.trace.graphics.open, print=FALSE), type="message"))} 
  #trace.oneGraphicsOpen <- function (f) {trace (as.name(f), RDataTracker:::.ddg.trace.graphics.open)} 
  lapply(.ddg.get(".ddg.graphics.functions.df")$function.names, trace.oneGraphicsOpen)

  #print ("Tracing graphics update")
  trace.oneGraphicsUpdate <- function (f) {capture.output(capture.output(trace (as.name(f), RDataTracker:::.ddg.trace.graphics.update, print=FALSE), type="message"))} 
  #trace.oneGraphicsUpdate <- function (f) {trace (as.name(f), RDataTracker:::.ddg.trace.graphics.update)} 
  lapply(.ddg.get(".ddg.graphics.update.functions.df"), trace.oneGraphicsUpdate)
  
  #print ("Tracing dev.off")
  capture.output(capture.output(trace (dev.off, RDataTracker:::.ddg.trace.graphics.close, print=FALSE), type="message"))
  #print ("Done initializing IO tracing")
}

#' Stop tracing I/O calls.  This should be called when RDT finishes.
.ddg.stop.iotracing <- function () {
  # Stop tracing output functions.  Will this be a problem if ddg.save is called from the console?
  # capture.output is used to prevent "Untracing" messages from appearing in the output
  capture.output (untrace(.ddg.get(".ddg.file.write.functions.df")$function.names), type="message")
  capture.output (untrace(.ddg.get(".ddg.file.read.functions.df")$function.names), type="message")
  capture.output (untrace(.ddg.get(".ddg.file.close.functions.df")$function.names), type="message")
  capture.output (untrace(.ddg.get(".ddg.graphics.functions.df")$function.names), type="message")
  capture.output (untrace(.ddg.get(".ddg.graphics.update.functions.df")), type="message")
  capture.output (untrace(dev.off), type="message")
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
  if (!(fname %in% output.files) && !endsWith (fname, ".snapshot")) {
    print (paste ("Adding output file", fname))
    print (sys.calls())
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
  #print (paste ("Output file parameter:", file.param.name))
  
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
  
  #print(paste("ddg.trace.close: close.caller =", close.caller))
  
  # Get the name of the close function
  call <- sys.call (frame.number)
  fname <- as.character(call[[1]])
  #print (paste (".ddg.trace.close: fname = ", fname))
  
  # Get the name of the connection parameter for the close function
  file.close.functions <- .ddg.get (".ddg.file.close.functions.df")
  file.param.name <- file.close.functions$param.names[file.close.functions$function.names == fname]
  #print (paste (".ddg.trace.close: file.param.name = ", file.param.name))
  
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



################ Functions to track graphics calls ####################

.ddg.create.graphics.nodes.and.edges <- function () {
  .ddg.add.graphics.device.node()
  .ddg.add.graphics.io ()
  .ddg.capture.graphics()
  .ddg.clear.device.nodes ()
}

.ddg.clear.device.nodes <- function () {
  .ddg.set (".ddg.new.device.nodes", character())
}

.ddg.add.device.node <- function (new.device.node) {
  device.nodes <- .ddg.get (".ddg.new.device.nodes")
  .ddg.set (".ddg.new.device.nodes", append(device.nodes, new.device.node))
}

.ddg.create.device.table <- function() {
  device.table <- 
      data.frame(device.number = numeric(),
                 file.name = character(),
                 stringsAsFactors = FALSE)
  .ddg.set (".ddg.device.table", device.table)
}

.ddg.add.to.device.table <- function (device.number, file.name) {
  device.table <- .ddg.get (".ddg.device.table")
  if (device.number %in% device.table$device.number) {
    device.table$file.name[device.table$device.number == device.number] <- file.name
  }
  else {
    print (paste (".ddg.add.to.device.table: device.number =", device.number))
    print (paste (".ddg.add.to.device.table: file.name =", file.name))
    print (sys.calls())
    device.table <- rbind (device.table, data.frame (device.number, file.name,
            stringsAsFactors = FALSE))
  }
  #print (device.table)
  .ddg.set (".ddg.device.table", device.table)
}

.ddg.get.file.for.device <- function (device.number) {
  device.table <- .ddg.get (".ddg.device.table")
  #print (paste ("Getting file for device", device.number))
  if (device.number %in% device.table$device.number) {
    return (device.table$file.name[device.table$device.number == device.number])
  }
  else {
    return ("")
  }
  
}

# Initialize the information about functions that initialize graphics devices
.ddg.create.graphics.functions.df <- function () {
  # Functions that read files
  function.names <-
      c ("pdf", "postscript", "bmp", "jpeg", "png", "tiff", "X11")
  
  # The argument that represents the file name
  param.names <-
      c ("file", "file", "filename", "filename", "filename", "filename", NA)
  
  return (data.frame (function.names, param.names, stringsAsFactors=FALSE))
}

.ddg.trace.graphics.open <- function () {
  #print ("Found graphics call")
  #print (sys.calls())
  
  if (.ddg.inside.call.to (".ddg.capture.graphics") || .ddg.inside.call.to (".ddg.trace.graphics.update")) {
    return()
  }
  
  #print ("In .ddg.trace.graphics.open")
  
  # Get the frame corresponding to the graphics function being traced
  frame.number <- .ddg.get.traced.function.frame.number()
  
  # Get the name of the graphics function
  call <- sys.call (frame.number)
  fname <- as.character(call[[1]])
  
  # Remove the package name if present
  if (length(fname > 1)) {
    fname <- fname[length(fname)]
  }
  #print(paste (".ddg.trace.graphics.open: fname =", fname))
  
  # Get the name of the file parameter for the graphics function
  graphics.functions <- .ddg.get (".ddg.graphics.functions.df")
  file.param.name <- graphics.functions$param.names[graphics.functions$function.names == fname]

  # X11 device writes to the screen so there is no file parameter
  if (is.na (file.param.name)) {
    .ddg.set(".ddg.no.graphics.file", TRUE)
    .ddg.set(".ddg.last.graphics.file", "")
  }
  else {
    #print(paste (".ddg.trace.graphics: file.param.name =", file.param.name))
  
    # Get the value of the file parameter  
    file <- eval (as.symbol(file.param.name), env = sys.frame(frame.number))
    #print(paste (".ddg.trace.graphics.open: file =", file))
  
    .ddg.add.graphics.file (file)
  }
  .ddg.set (".ddg.add.device.output", TRUE)
}

.ddg.add.graphics.device.node <- function() {
  if (!.ddg.get (".ddg.add.device.output")) {
    return()
  } 
  
  #print ("In .ddg.add.graphics.device.node")
  
  #if (!.ddg.get (".ddg.no.graphics.file")) {
  if (names(dev.cur()) != "RStudioGD") {
    print (paste ("current device =", names(dev.cur())))
    .ddg.add.to.device.table (dev.cur (), .ddg.get (".ddg.last.graphics.file"))
    
    tryCatch(
        # Allows dev.print to work when we want to save the plot.
        # Only do this if the graphics is going to a file.
        {#print (".ddg.add.graphics.device.node: calling dev.control")
        dev.control("enable")},
        error = function (e) return()
    )
  }

  # Add the newly-opened graphics device to the list of open devices
  .ddg.set("ddg.open.devices", union(.ddg.get("ddg.open.devices"), dev.cur()))

    
#  # Find all the graphics files that have potentially been opened.
#  # Remember these file names until we find the dev.off call and then
#  # determine which was written.
#  new.possible.graphics.files.open <- .ddg.find.files (main.object, .ddg.get(".ddg.graphics.functions.df"), env)
#  if (!is.null(new.possible.graphics.files.open)) {
#    #print(paste(".ddg.set.grpahics.files: opened", new.possible.graphics.files.open))
#    if (!is.null(.ddg.get ("possible.graphics.files.open"))) {
#      possible.graphics.files.open <- .ddg.get ("possible.graphics.files.open")
#      .ddg.set ("possible.graphics.files.open",
#          c (new.possible.graphics.files.open, possible.graphics.files.open))
#    }
#    
#    else {
#      .ddg.set ("possible.graphics.files.open", new.possible.graphics.files.open)
#    }
#    #print (paste (".ddg.set.graphics.files: Found ", new.possible.graphics.files.open))
#    
#  }
  
  #print(paste(".ddg.add.graphics.device.node: dev.cur =", dev.cur()))
  dev.node.name <- paste0("dev.", dev.cur())
  .ddg.data.node("Data", dev.node.name, "graph", NULL)
#  .ddg.proc2data(main.object@abbrev, dev.node.name)
  .ddg.lastproc2data(dev.node.name)
  .ddg.set (".ddg.add.device.output", FALSE)
  .ddg.add.device.node (dev.node.name)
}


#' Clears out the list of graphics files.  This should be 
#' called on initialization and after the device nodes are created.
#' 
#' @return nothing
.ddg.clear.graphics.file <- function () {
  .ddg.set ("graphics.files", character())
}

#' Add a file name to the graphics list.
#' 
#' @param fname the name of the file to add to the list
#' 
#' @return nothing
.ddg.add.graphics.file <- function (fname) {
  graphics.files <- .ddg.get("graphics.files")
  #print(paste("In .ddg.add.graphics.file"))
  
  # Only add the file to the list if it is not already there.  It could be 
  # there if there are multiple functions called indirectly in one R statement
  # that write to the same file.
  if (!(fname %in% graphics.files)) {
    #print(paste("Adding graphics file: ", fname))
    .ddg.set ("graphics.files", append(graphics.files, fname))
    #print (paste ("graphics.files =", .ddg.get("graphics.files")))
    
    .ddg.set (".ddg.last.graphics.file", fname)
  }
}

.ddg.trace.graphics.update <- function () {
  if (.ddg.inside.call.to (".ddg.capture.graphics") ) { 
    return()
  }
  
  #print ("In .ddg.trace.graphics.update")
  .ddg.set (".ddg.add.device.io", TRUE)
}

# Add data in and data out nodes that represent the current device.
#
# cmd - Assumed to be a function that modifies the graphics device,
# such as a function in the base graphics package.

.ddg.add.graphics.io <- function () {
  if (!.ddg.get (".ddg.add.device.io")) {
    return ()
  }
  
  #print ("In .ddg.add.graphics.io")
  
  # Try adding the input edge.  It is not a problem if the node 
  # can't be found.  It means that the output is going to the
  # RStudio window, not a file, so there has been no call like pdf
  # or jpg that would have created the data node.
  dev.node.name <- paste0("dev.", dev.cur())
  
  if (!(dev.node.name %in% .ddg.get (".ddg.new.device.nodes"))) {
    if (dev.cur() %in% .ddg.get("ddg.open.devices")) {
      #print (paste (".ddg.add.graphics.io: Creating input edge for", dev.node.name))
      .ddg.data2proc(dev.node.name, dscope = NULL)
    }
    else {
      # Add the newly-opened graphics device to the list of open devices
      .ddg.set("ddg.open.devices", union(.ddg.get("ddg.open.devices"), dev.cur()))    
    }
    
    # Add an output node with the same name
    #print (paste (".ddg.add.graphics.io: Creating node for output device", dev.node.name))
    .ddg.data.node("Data", dev.node.name, "graph", NULL)
    #print ("Creating edge to output device")
    .ddg.lastproc2data(dev.node.name)
    .ddg.add.device.node (dev.node.name)
  }
  
  .ddg.set (".ddg.add.device.io", FALSE)
}

.ddg.trace.graphics.close <- function () {
  if (.ddg.inside.call.to (".ddg.capture.graphics") ) { 
    return()
  }
  
  print ("In .ddg.trace.graphics.close")
  print (paste ("dev.list =", dev.list(), names(dev.list()), collapse=", "))
  print (paste ("dev.cur =", dev.cur()))
  
  .ddg.set (".ddg.add.device.close", TRUE)
  
  if (.ddg.get(".ddg.no.graphics.file") || names(dev.cur()) == "RStudioGD") {
    file <- .ddg.capture.current.graphics()
    .ddg.set(".ddg.no.graphics.file", FALSE)
    .ddg.add.graphics.file (file)
    .ddg.add.to.device.table (dev.cur (), file)
  }
}

.ddg.capture.graphics <- function(called.from.save = FALSE) {
  if (!.ddg.get (".ddg.add.device.close") && !called.from.save) {
    #print ("Returning - .ddg.add.device.close is false")
    return()
  }
  
  #print ("In .ddg.capture.graphics")
  
  if (called.from.save) {
    #print (dev.list())
  }

#  proc.node.name <- 
#      if (is.null(cmd)) NULL
#      else if (is.character(cmd)) cmd
#      else cmd@abbrev
  
  dev.number <- .ddg.get(".ddg.dev.number")
  .ddg.set("ddg.open.devices", setdiff(.ddg.get("ddg.open.devices"), dev.number))
  #print (paste ("ddg.capture.graphics: Device closed: ", dev.number))
  
  dev.name <- .ddg.get.file.for.device (dev.number)
  #if (names(dev.cur()) == "RStudioGD") {
  if (dev.name == "") { 
    graphics.file <- .ddg.capture.current.graphics()
  }
  
  else {
    
    graphics.file <- .ddg.get.file.for.device (dev.number)
  
#    graphics.files <- .ddg.get ("graphics.files")
#    #print (paste ("ddg.capture.graphics: graphics.files =", graphics.files))
#    #if (length(graphics.files) == 0 && !.ddg.get(".ddg.no.graphics.file")) {
#    if (length(graphics.files) == 0 && !.ddg.get(".ddg.no.graphics.file") && names(dev.cur()) != "RStudioGD") {
#      print ("Returning - no graphics files found")
#      return()
#    }
#    
#    print (paste ("Open graphics files =", graphics.files))
#    graphics.file.info <- file.info(graphics.files)
#    print (paste ("graphics.file.info =", graphics.file.info))
#    print (paste ("graphics.file.info$mtime =", graphics.file.info$mtime))
#    print (paste ("is.na(graphics.file.info$mtime", is.na(graphics.file.info$mtime)))
#    
#    if (.ddg.get(".ddg.no.graphics.file") || all(is.na(graphics.file.info$mtime))) {
#  #  if (is.na(graphics.file.info$mtime)) {
#      graphics.file <- .ddg.capture.current.graphics()
#  #    # The file no longer exists.  Create a temporary file.
#  #    graphics.file <- paste0("dev.off.", .ddg.dnum()+1, ".pdf")
#  #    print(paste(".ddg.capture.graphics: writing to ", graphics.file))
#  #    
#  #    # Save the graphic to a file temporarily
#  #    #print(sys.calls())
#  #    print (paste ("dev.cur =", dev.cur()))
#  #    dev.print(device=pdf, file=graphics.file)
#      
#    }
#    else {
#      latest.file.date.row <- which.max (graphics.file.info$mtime)
#      #print (paste ("latest.file.date.row =", latest.file.date.row))
#      graphics.file <- graphics.files[latest.file.date.row]
      #print (paste("ddg.capture.graphics: Latest graphics file =", graphics.file))
      
#      # Check if the device is still open and close it if it is
#      # We need to do this so that the file.out call can
#      # copy the file.
#      if (dev.number %in% dev.list()) {
#        dev.off(dev.number)
#      }
      
 #   }
    
 #   graphics.files <- graphics.files [graphics.files != graphics.file]
 #   print (paste ("Setting graphics.files to", graphics.files))
 #   .ddg.set ("graphics.files =", graphics.files)
    
  }
  
  # Check if the device is still open and close it if it is
  # We need to do this so that the file.out call can
  # copy the file.
  if (dev.number %in% dev.list()) {
    dev.off(dev.number)
  }
  

  #print (paste (".ddg.capture.graphics: Creating file node for", graphics.file))
  ddg.file.out (graphics.file)

  # Add an input edge from the current device
  dev.node.name <- paste0("dev.", dev.number)
  #print(paste(".ddg.capture.current.graphics: dev.node.name =", dev.node.name))
  #print(".ddg.capture.graphics: creating in edge")

  # If the device was opened but never written to there will be no node.
  if (.ddg.data.node.exists (dev.node.name)) {
    .ddg.data2proc(dev.node.name, NULL)
  }
  #print(".ddg.capture.graphics: done creating in edge")

  #.ddg.capture.current.graphics(cmd, possible.graphics.files.open[latest.file.date.row])
  #print(paste(".ddg.capture.graphics: writing to ", possible.graphics.files.open[latest.file.date.row]))
  .ddg.set (".ddg.add.device.close", FALSE)
  .ddg.set(".ddg.no.graphics.file", FALSE)
  
  if (called.from.save && dev.cur() != 1) {
    .ddg.set(".ddg.dev.number", dev.cur())
    .ddg.capture.graphics (TRUE)
  }
  
  return(graphics.file)


#  #print(paste(".ddg.capture.graphics: ", proc.node.name))
#  if (!is.null(.ddg.get ("possible.graphics.files.open")) && !is.null(proc.node.name)) {
#    possible.graphics.files.open <- .ddg.get ("possible.graphics.files.open")
#  
#    # Find the most recent file
#    #print(paste(".ddg.capture.graphics: possible.graphics.files.open =", possible.graphics.files.open))
#    #print(".ddg.capture.graphics: getting file info")
#    graphics.file.info <- file.info(possible.graphics.files.open)
#    #print(".ddg.capture.graphics: getting modification time")
#    latest.file.date.row <- which.max (graphics.file.info$mtime)
#    
#    # Check if the device is still open and close it if it is
#    # We need to do this so that the file.out call can
#    # copy the file.
#    if (dev.number %in% dev.list()) dev.off(dev.number)
#    
#    #print(".ddg.capture.graphics: creating file node")
#    
#    if (!is.null(proc.node.name)) {
#      ddg.file.out (possible.graphics.files.open[latest.file.date.row], pname=proc.node.name)
#      
#      # Add an input edge from the current device
#      dev.node.name <- paste0("dev.", dev.number)
#      #print(paste(".ddg.capture.current.graphics: dev.node.name =", dev.node.name))
#      #print(".ddg.capture.graphics: creating in edge")
#      
#      # If the device was opened but never written to there will be no node.
#      if (.ddg.data.node.exists (dev.node.name)) {
#        .ddg.data2proc(dev.node.name, NULL, proc.node.name)
#      }
#      #print(".ddg.capture.graphics: done creating in edge")
#      
#      #.ddg.capture.current.graphics(cmd, possible.graphics.files.open[latest.file.date.row])
#      #print(paste(".ddg.capture.graphics: writing to ", possible.graphics.files.open[latest.file.date.row]))
#      .ddg.set ("possible.graphics.files.open", NULL)
#    }
#    
#    return(possible.graphics.files.open[latest.file.date.row])
#  }
#  
#  # Output is going to the display, so we need to make up a name
#  #dev.file <- .ddg.capture.current.graphics(proc.node.name)
#  
#  if (called.from.save) {
#    #print(paste(".ddg.capture.graphics: dev.file =", dev.file))
#    #print(paste(".ddg.capture.graphics: proc.node.name =", proc.node.name))
#    ddg.file.out (dev.file, pname=proc.node.name)
#    #print(paste(".ddg.capture.graphics: returned from ddg.file.out"))
#    
#    # Remove the temporary file
#    file.remove(dev.file)
#    
#    # Add an input edge from the current device
#    dev.node.name <- paste0("dev.", dev.cur())
#    #print(paste(".ddg.capture.current.graphics: dev.node.name =", dev.node.name))
#    # If the device was opened but never written to there will be no node.
#    if (.ddg.data.node.exists (dev.node.name)) {
#      .ddg.data2proc(dev.node.name, NULL, proc.node.name)
#    }
#  }
#  
#  return (dev.file)  
}

# Captures what is on the current display to a file, creates a file node
# and connects to the ddg.
.ddg.capture.current.graphics <- function() {
  file <- paste0("dev.off.", .ddg.dnum()+1, ".pdf")
  print(paste(".ddg.capture.current.graphics: writing to ", file))
  
  # Save the graphic to a file temporarily
  #print(sys.calls())
  print (paste ("dev.list =", dev.list(), names(dev.list()), collapse=", "))
  print (paste ("dev.cur =", dev.cur()))
  dev.print(device=pdf, file=file)
  #print ("dev.print complete")
  #.ddg.set ("possible.graphics.files.open", file)
  return(file)
}

