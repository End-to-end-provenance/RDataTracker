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

#' Initialize the data needed to trace I/O functions
.ddg.init.iotrace <- function () {
  # Record the information about the input and output functions
  .ddg.set (".ddg.file.write.functions.df", .ddg.create.file.write.functions.df ())
  .ddg.set (".ddg.file.read.functions.df", .ddg.create.file.read.functions.df ())
#  .ddg.set (".ddg.file.open.functions.df", .ddg.create.file.open.functions.df ())
  .ddg.set (".ddg.file.close.functions.df", .ddg.create.file.close.functions.df ())
  
  
  # Create an empty list for the input and output files
  .ddg.clear.input.file()
  .ddg.clear.output.file()
  
  # Start tracing of input and output functions
  # capture.output is called twice to capture the output that is going to standard output and to
  # standard error.  These are messages that say "Tracing..." and list each function being
  # traced.
  #print ("Tracing input and output functions")
  # Note that we need to use the RDataTracker::: notation for the functions for trace to call
  # so that it can find those functions without making them publicly available in 
  # the namespace.
  trace.oneOutput <- function (f) {capture.output(capture.output(trace (as.name(f), RDataTracker:::.ddg.trace.output, print=FALSE), type="message"))} 
  lapply(.ddg.get(".ddg.file.write.functions.df")$function.names, trace.oneOutput)
  trace.oneInput <- function (f) {capture.output(capture.output(trace (as.name(f), RDataTracker:::.ddg.trace.input, print=FALSE), type="message"))} 
  lapply(.ddg.get(".ddg.file.read.functions.df")$function.names, trace.oneInput)
#  trace.oneOpen <- function (f) {capture.output(capture.output(trace (as.name(f), exit = ddg.trace.open, print=FALSE), type="message"))} 
#  lapply(.ddg.get(".ddg.file.open.functions.df")$function.names, trace.oneOpen)
  #print ("Tracing is initialized")
  trace.oneClose <- function (f) {capture.output(capture.output(trace (as.name(f), RDataTracker:::.ddg.trace.close, print=FALSE), type="message"))} 
  lapply(.ddg.get(".ddg.file.close.functions.df")$function.names, trace.oneClose)
  
}

.ddg.stop.iotracing <- function () {
  # Stop tracing output functions.  Will this be a problem if ddg.save is called from the console?
  # capture.output is used to prevent "Untracing" messages from appearing in the output
  capture.output (untrace(.ddg.get(".ddg.file.write.functions.df")$function.names), type="message")
  capture.output (untrace(.ddg.get(".ddg.file.read.functions.df")$function.names), type="message")
  #capture.output (untrace(.ddg.get(".ddg.file.open.functions.df")$function.names), type="message")
  capture.output (untrace(.ddg.get(".ddg.file.close.functions.df")$function.names), type="message")
}

#' Returns true if the object passed in is a connection
#'
#' @param value an R object
#'
#' @return true if the R object is a connection used to do I/O
.ddg.is.connection <- function (value) {
  return ("connection" %in% class(value))
}

.ddg.get.connection.description <- function (conn) {
#  return (showConnections(TRUE)[as.numeric(conn)+1, "description"])
#  return (showConnections(TRUE)[conn, "description"])
  return (showConnections(TRUE)[as.character(conn), "description"])  
  
}

.ddg.is.connection.open <- function (conn) {
#  return (showConnections(TRUE)[as.numeric(conn)+1, "isopen"] == "opened")
  return (showConnections(TRUE)[as.character(conn), "isopen"] == "opened")  
  
}

#.ddg.add.connection <- function (conn) {
#  # Save the current connections so we have access to them if they are
#  # changed before we get the information we need.
#  .ddg.set (".ddg.connections", showConnections(TRUE))
#}

#' Returns true if the connection is open
#'
#' @param conn a connection object
#'
#' @return true if the connection is open
#.ddg.get.connection.isopen <- function (conn) { 
#  conns <- .ddg.get (".ddg.connections")
#  if (nrow(conns) >= conn[1]) return (TRUE)
#  return (conns[as.character(conn[1]), "isopen"] == "opened")
#}

#' @return a matrix containing information about all open connections
#' 
.ddg.get.open.connections <- function () { 
#  conns <- .ddg.get (".ddg.connections")
#  openConns <- conns[conns [, "isopen"] == "opened",,drop=FALSE]
#  return (openConns)
  conns <- showConnections(TRUE)
  conns[conns[, "isopen"] == "opened", ]
}

.ddg.can.read.connection <- function (conn) {
#  conns <- showConnections(TRUE)
#  return (conns[as.numeric(conn)+1, "can read"] == "yes")
  return (showConnections(TRUE)[as.character(conn), "can read"] == "yes")  
}

.ddg.can.write.connection <- function (conn) {
#  conns <- showConnections(TRUE)
#  print(conns)
#  return (conns[as.numeric(conn)+1, "can write"] == "yes")
#  print (showConnections(TRUE))
#  print (conn)
#  print (as.character(conn))
  return (showConnections(TRUE)[as.character(conn), "can write"] == "yes")  
}

#' Returns the name of the I/O item the connection communicates with.
#' For example, this could be a file name, a URL, a host/port pair 
#' from a socket connection, etc.
#'
#' @param conn This should be a connection object
#' 
#' @return a text description of what is connected to.
#'
#.ddg.get.connection.description <- function (conn) { 
#  if (.ddg.is.connection(conn)) {
#    conn <- conn[1]
#  }
#  .ddg.get (".ddg.connections")[as.character(conn[1]), "description"]
#}

#.ddg.create.file.open.functions.df <- function () {
#  # Functions that open connections
#  function.names <-
#      c ("file", "url", "gzfile", "bzfile", "xzfile", 
#          "unz", "pipe", "fifo", "socketConnection", "open")
#  
#  # The argument that represents the file name
#  param.names <-
#      c ("description", "description", "description", "description", "description", 
#          "description", "description", "description", "host", "con")
#  
#  # Position of the file parameter if it is passed by position
#  param.pos <-
#      c (1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#  
#  return (data.frame (function.names, param.names, param.pos, stringsAsFactors=FALSE))
#}
#
#ddg.trace.open <- function () {
#  print ("ddg.trace.open called")
#  .ddg.add.connection ()
#}

# Initialize the information about functions that read from files
.ddg.create.file.close.functions.df <- function () {
  # Functions that read files
  function.names <-
 #     c ("close", "close.connection")
      c ("close.connection")
  
  # The argument that represents the file name
  param.names <-
#      c ("con", "con")
      c ("con")
  
  # Position of the file parameter if it is passed by position
  param.pos <-
#      c (1, 1)
      c (1)
  
  return (data.frame (function.names, param.names, param.pos, stringsAsFactors=FALSE))
}

.ddg.inside.call.to <- function (func) {
  #print (paste ("Looking for call to", func))
  is.call.to <- function (call) { 
    if (is.symbol(call[[1]])) {
      #print (paste ("Comparing to ", call[[1]]))
      return (startsWith (as.character(call[[1]]), func))
    }
    return (FALSE)
  }
  calls.found <- sapply (sys.calls(), is.call.to )
  #print (paste( "calls.found = ", calls.found))
  return (any (calls.found))
}

.ddg.trace.close <- function () {
  
  # Get the frame corresponding to the output function being traced
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
  
  # Check that the function is not being called due to a call to capture output
  #if (length (grep ("^capture.output", sys.calls())) > 0) {
  if (.ddg.inside.call.to ("capture.output") || .ddg.inside.call.to ("parse") 
      || .ddg.inside.call.to (".ddg.snapshot")) {
    return()
  }
  
  read.funs <- .ddg.get(".ddg.file.read.functions.df")$function.names
  
  if (any (sapply (read.funs, .ddg.inside.call.to))) {
                   #function (read.fun) {return (grepl (read.fun, sys.calls())) }))) {
     return()
  }
  
  write.funs <- .ddg.get(".ddg.file.write.functions.df")$function.names
  
  if (any (sapply (write.funs, .ddg.inside.call.to))) {
    #function (read.fun) {return (grepl (read.fun, sys.calls())) }))) {
    return()
  }
  
  #print ("Tracing close function")
  #print(sys.calls())
  
  # Get the name of the close function
  call <- sys.call (frame.number)
  fname <- as.character(call[[1]])
  #print (paste ("Close function traced: ", fname))
  
  # Get the name of the file parameter for the close function
  file.close.functions <- .ddg.get (".ddg.file.close.functions.df")
  file.param.name <- file.close.functions$param.names[file.close.functions$function.names == fname]
  #print (paste ("Close file parameter:", file.param.name))
  
  # Get the value of the file parameter  
  close.conn <- eval (as.symbol(file.param.name), env = sys.frame(frame.number))
  #print (paste ("close.conn =", close.conn))

  .ddg.add.closed.connection (close.conn)
}

.ddg.add.closed.connection <- function (close.conn) {
  # Save the file name so the file node can be created when the statement is complete.
  # we do not want to create the nodes because the procedure node to connect to does not
  # exist yet, and the file has not been written to yet.
  #print(showConnections(TRUE))
  #print (close.conn)
  #print (as.character(close.conn))
  if (.ddg.can.write.connection (close.conn) && !(close.conn %in% c("stdin", "stderr"))) {
    #print ("Adding as output")
    .ddg.add.output.file (.ddg.get.connection.description(close.conn))
  }
  
}

.ddg.created.file.nodes.for.open.connections <- function () {
  openConns <- .ddg.get.open.connections()
  lapply (names(openConns[, "description"]), .ddg.add.closed.connection)
  .ddg.create.file.write.nodes.and.edges ()
}

# Creates file nodes and data out edges for any files that are closed in this cmd
# cmd - text command
# cmd.expr - parsed command
.ddg.create.file.close.nodes.and.edges <- function (cmd = NULL, env = NULL, allOpen = FALSE) {
  # Find all the files potentially closed in this command.
  # This may include files that are not actually closed if the
  # close calls are within an if-statement, for example.
#  files.closed <- 
#      if (is.null(cmd)) character()
#      else .ddg.find.files.closed(cmd, env)
#  
#  # Check for closeAllConnections.  It is a special case because it takes no 
#  # parameters.  .ddg.find.files expects there to be a parameter identifying the
#  # connection to close.
#  if (allOpen || (!is.null(cmd) && .ddg.has.call.to (cmd@parsed, "closeAllConnections"))) {
#    #print ("Found closeAllConnections")
#    openConns <- .ddg.get.open.connections()
#    #print(paste("openConns =", openConns))
#    files.closed <- unique (c (files.closed, openConns[, "description"]))
#    #print(paste("files.closed =", files.closed))
#  }
#  
#  for (file in files.closed) {
#    #print(paste("Closed", file))
#    #print(str(file))
#    # Check for a connection.  It will be a number encoded as a string.
#    # Turn off warnings and try the coercion and then turn warnings back on
#    save.warn <- getOption("warn")
#    options(warn=-1)
#    conn <- as.numeric(file)
#    options (warn=save.warn)
#    if (!is.na (conn)) file <- .ddg.get.connection.description(conn)
#    
#    # Check that the file exists.  If it does, we will assume that
#    # it was created by the write call that we just found.
#    if (file.exists (file)) {
#      # Create the file node and edge
#      pname <-
#          if (is.null(cmd)) NULL
#          else cmd@abbrev
#      #print(paste("Calling ddg.file.out: file=", file, "  pname =", pname))
#      ddg.file.out (file, pname=pname)
#    }
#  }
}



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

#' Initialize the information about functions that read from files
#' 
#' @return a data frame consisting of one row for each input function.
#' Each row contains the function name, the name of the paramter that
#' holds the file argument, and the position of the parameter that
#' contains the file argument.
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
  
  # Position of the file parameter if it is passed by position
  param.pos <-
      c (1, 
          1, 
          1,
          1, 1, 1, 1, 1, 1)
  
  return (data.frame (function.names, param.names, param.pos, stringsAsFactors=FALSE))
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
#' This function needs to be public so that trace can find it when the
#' traced function executes.
#' 
#' @return nothing
.ddg.trace.output <- function () {

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
      #print (paste ("file connected: ", file))
    }
    
    # Check that the file exists.  If it does, we will assume that
    # it was created by the write call that just executed.
    if (file.exists (file)) {
      #print("Creating file node")
      # Create the file node and edge
      ddg.file.out (file)
    }
    #else {
     # print ("File does not exist -- no node created")
    #}
  }

  # Clear the list of output files now that they have been handled.
  .ddg.clear.output.file ()
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
    #print (paste ("Found connection", fname))
    fname <- showConnections(TRUE)[as.character(fname), "description"]
  }
  
  # Only add the file to the list if it is not already there.  It could be 
  # there if there are multiple functions called indirectly in one R statement
  # that read from the same file, like readLines and scan.
  if (!(fname %in% input.files)) {
    #print (paste ("Adding input file: ", fname))
    #print (class(fname))
    #print (str(fname))
    #print (paste ("Is a connection?", .ddg.is.connection(fname)))
    #.ddg.set ("input.files", append(input.files, fname))
    .ddg.set ("input.files", c(input.files, list(fname)))
  }
}



.ddg.trace.input <- function () {
  #print ("Found input function")
  
  # Get the frame corresponding to the output function being traced
  frame.number <- .ddg.get.traced.function.frame.number()
  
  # Check if the function that called the input function is a ddg function.
  # If it is, ignore this call.  .ddg.load.history is an example of a 
  # function that does input that we would want to ignore.  The is.call
  # test is used because it is possible that the caller is a closure
  # and thus does not have a name.
  input.caller <- sys.call (frame.number - 1)[[1]]
  if (is.symbol (input.caller)) {
      input.caller.name <- as.character(input.caller)
      if (startsWith (input.caller.name, "ddg") || startsWith (input.caller.name, ".ddg")) {
        return()
      }
  }
  
  #print (sys.calls())
  #print (paste("frame.number =", frame.number))
  #print (sys.call(frame.number))
  #tryCatch (print (paste("function with tracing =", sys.function(frame.number))), error = function (e) {print (e)} )
  #tryCatch (print (paste("original function =", sys.function(frame.number)@original)), error = function (e) {print (e)})

  # Get the name of the input function
#  call <- match.call (sys.function(frame.number)@original, sys.call (frame.number), 
#      expand.dots = TRUE, envir = sys.frame(frame.number))
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

# Creates file nodes and data in edges for any files that are read in this cmd
# cmd - text command
# cmd.expr - parsed command
.ddg.create.file.read.nodes.and.edges <- function () {
  # Get the list of files that have been read by the last statement.
  files.read <- .ddg.get ("input.files")
  
  #print (paste ("files.read =", files.read))
  
  # Adds the files read to ddg.infilenodes for use in determining reads
  # and writes in the hashtable.
  .ddg.set("ddg.infilenodes", c(.ddg.get("ddg.infilenodes"), files.read))
  #print(paste("Adding", files.read, "to ddg.infilenodes"))
  
  for (file in files.read) {
    #print (paste("file =", file))
#    # Check for a connection.  It will be a number encoded as a string.
#    if (.ddg.is.connection(file)) {
#      file <- .ddg.get.connection.description(file)
#      print(paste("connected to", file))
#    }
#    else {
#      print (class(file))
#      print (paste ("Is a connection?", .ddg.is.connection(file)))
#    }
#    
    
   #print(paste(".ddg.create.file.read.nodes.and.edges: file =", file))
   # Use URL node for URLs and for socket connections
   if (grepl ("://", file) || startsWith (file, "->"))
   {
     #print ("Creating url node")
     if (grepl ("://", file) ) {
       # Save the Web page
       url.copy <- .ddg.url.copy (file)
       .ddg.url.node(file, url.copy)
     }
     else {
       # Maybe we should change the node type to be "Remote" or something?
       .ddg.url.node(file, file)
     }
     #.ddg.data2proc(file, environmentName(.GlobalEnv), cmd@abbrev)
     .ddg.data2proc(file, environmentName(.GlobalEnv))
      
    }
    else {
      # Only create the node and edge if there actually is a file
      if (file.exists(file)) {
        #print ("Creating file node")
        # Create the file node and edge
        ddg.file(file)
        #ddg.data.in(basename(file), pname=cmd@abbrev)
        ddg.data.in(basename(file))
      }
      
      # If the filename contains a :, then it is referencing a file within 
      # a zip file, so checck that the zip file exists.      
      else if (grepl(":", file)) {
        zipfile <- sub (":.*", "", file)
        if (file.exists (zipfile)) {
          #print ("Creating file node")
          # Create the file node and edge
          ddg.file(zipfile, file)
          #ddg.data.in(file, pname=cmd@abbrev)
          ddg.data.in(file)
        }
      }
    }
  }

  # Clear the list of input files now that they have been handled.
  .ddg.clear.input.file ()

}

