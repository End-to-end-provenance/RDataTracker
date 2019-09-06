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

# This file contains the functions needed to trace input and output 
# operations, including the reading and writing of files, opening and 
# closing of connections, and the creation of plots.
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
# 2 must be called while inside the read/write/close/graphics function.  Function 
# 3 cannot be called until the R statement containing the call completes
# so that the procedure node exists to connect the file node to.
#
# WARNING:  The tracing code filters out calls where specific ddg functions
# are on the call stack.  If the names of those functions are changed, the
# code here will need to change as well.  


#' .ddg.init.iotrace initialize the data needed to trace I/O functions
#' @return nothing
#' @noRd

.ddg.init.iotrace <- function () {
  #print ("Initializing io tracing")
  
  # Store the starting graphics device.
  .ddg.set("ddg.open.devices", vector())
  
  # Record the information about the input and output functions
  .ddg.set ("ddg.file.write.functions.df", .ddg.create.file.write.functions.df ())
  .ddg.set ("ddg.file.read.functions.df", .ddg.create.file.read.functions.df ())
  .ddg.set ("ddg.file.close.functions.df", .ddg.create.file.close.functions.df ())
  .ddg.set ("ddg.graphics.functions.df", .ddg.create.graphics.functions.df ())
  .ddg.set ("ddg.graphics.update.functions.df", ls(which(search()=="package:graphics")))
  .ddg.set ("ddg.add.device.output", FALSE)
  .ddg.set ("ddg.add.device.io", FALSE)
  .ddg.set ("ddg.add.device.close", FALSE)
  .ddg.set ("ddg.no.graphics.file", TRUE)
  
  # When true, it means that ggsave was called without
  # a plot parameter.
  .ddg.set ("ddg.implicit.plot", FALSE)
  
  # When true, it means that ggplot was called to start
  # a new plot.
  .ddg.set ("ddg.ggplot.created", FALSE)
  
  # On Travis, calling ggsave creates Rplots.pdf, while it does not
  # on the Mac.  Maybe it is because Travis runs headless???  In 
  # any case, if ggsave creates it, we will delete it so it
  # does not show up in the ddg, causing regression tests to fail.
  .ddg.set ("ddg.remove.Rplots", FALSE)
  
  # If Rplots.pdf is created by ggsave and we are unable to delete it,
  # this flag will prevent an Rplots.pdf node from being added to the
  # end of the DDG.
  .ddg.set("ddg.ignore.rplots", FALSE)
  
  # Create an empty list for the input, output, and files
  .ddg.clear.input.file()
  .ddg.clear.output.file()
  .ddg.clear.device.nodes ()
  .ddg.create.device.table ()
  
  # Start tracing of input and output functions
  # capture.output is called twice to capture the output that is going to 
  # standard output and to standard error.  These are messages that say 
  # "Tracing..." and list each function being traced.
  # Note that we need to use the rdt::: notation for the functions for 
  # trace to call so that it can find those functions without making them 
  # publicly available in the namespace.
  # ggplot2 functions are traced individually because the package name needs to 
  # be included.
  
  trace.oneOutput <- 
    function (f) {
      utils::capture.output(
        utils::capture.output(trace (as.name(f), 
                                     function () .ddg.trace.output(), 
                                     print=FALSE), 
                              type="message"))
    } 
  lapply(.ddg.get("ddg.file.write.functions.df")$function.names, trace.oneOutput)
  utils::capture.output(
    utils::capture.output(trace (ggplot2::ggplot, 
            function () .ddg.trace.output (), 
                                 print=FALSE), 
                          type="message"))
  
  trace.oneInput <- 
    function (f) {
      utils::capture.output(
        utils::capture.output(trace (as.name(f), 
                function () .ddg.trace.input (), 
                                     print=FALSE), 
                              type="message"))
    } 
  lapply(.ddg.get("ddg.file.read.functions.df")$function.names, trace.oneInput)

  trace.oneClose <- 
    function (f) {
      utils::capture.output(
        utils::capture.output(trace (as.name(f), 
                function () .ddg.trace.close (), 
                                     print=FALSE), 
                              type="message"))
    } 
  lapply(.ddg.get("ddg.file.close.functions.df")$function.names, trace.oneClose)
  utils::capture.output(
    utils::capture.output(trace (ggplot2::ggsave, 
            function () .ddg.trace.close (), 
                                 print=FALSE), 
                          type="message"))
  
  #print ("Tracing graphics open")
  # trace (grDevices::pdf, rdt:::.ddg.trace.graphics.open, print=TRUE)
  trace.oneGraphicsOpen <- 
    function (f) {
      utils::capture.output(
        utils::capture.output(trace (as.name(f), 
                function () .ddg.trace.graphics.open (), 
                                     print=FALSE), 
                              type="message"))
    } 
  lapply(.ddg.get("ddg.graphics.functions.df")$function.names, trace.oneGraphicsOpen)
  
  #print ("Tracing graphics update")
  trace.oneGraphicsUpdate <- 
    function (f) {
      utils::capture.output(
        utils::capture.output(trace (as.name(f), 
                function () .ddg.trace.graphics.update (), 
                                     print=FALSE), 
                              type="message"))
    } 
  lapply(.ddg.get("ddg.graphics.update.functions.df"), trace.oneGraphicsUpdate)
  
  #print ("Tracing dev.off")
  utils::capture.output(
    utils::capture.output(trace (grDevices::dev.off, 
            function () .ddg.trace.graphics.close (), 
                                 print=FALSE), 
                          type="message"))
  #print ("Done initializing IO tracing")
}

#' .ddg.stop.iotracing stops tracing I/O calls.  This should be called when RDT finishes.
#' @return nothing
#' @noRd

.ddg.stop.iotracing <- function () {
  
  # Stop tracing output functions.  
  # utils::capture.output is used to prevent "Untracing" messages from appearing 
  # in the output
  utils::capture.output (
    untrace(.ddg.get("ddg.file.write.functions.df")$function.names), 
    type="message")
  utils::capture.output (untrace(.ddg.get("ddg.file.read.functions.df")$function.names), 
                         type="message")
  utils::capture.output (
    untrace(.ddg.get("ddg.file.close.functions.df")$function.names), 
    type="message")
  utils::capture.output (untrace(.ddg.get("ddg.graphics.functions.df")$function.names), 
                         type="message")
  utils::capture.output (untrace(.ddg.get("ddg.graphics.update.functions.df")), 
                         type="message")
  utils::capture.output (untrace(grDevices::dev.off), type="message")
  
  utils::capture.output (untrace(ggplot2::ggplot), type="message")
  utils::capture.output (untrace(ggplot2::ggsave), type="message")
}

################### Helper functions ######################3

#' .ddg.get.traced.function.frame.number gets the frame number for a function 
#' being traced
#' @return the frame number of the function being traced.  
#' Returns NULL if there is no occurrence of .doTrace
#' on the stack.
#' @noRd

.ddg.get.traced.function.frame.number <- function() {
  .ddg.get.frame.number.for.func (".doTrace") - 1
}

#' .ddg.get.frame.number.for.func gets the frame number for a function 
#' being called
#' @return the frame number of the function being called.  
#' Returns NULL if there is no occurrence of the function
#' on the stack.
#' @noRd
.ddg.get.frame.number.for.func <- function (func.name) {
  calls <- sys.calls()
  calls <- mapply( `[[`, calls, 1, SIMPLIFY = TRUE )
  
  func.frame <- which( calls == func.name )
  
  if( length(func.frame) > 0 )
  {
    # Return the last one
    return (func.frame[length(func.frame)])
  }
  else
  {
    return (NULL)
  }
}

#' .ddg.is.call.to determines if the call passed in is a call to the passed in function
#' @param call a parse tree for a function call
#' @param func the name of a function
#' @return TRUE if the call passed in is a call to the function name passed in
#' @noRd

.ddg.is.call.to <- function (call, func) { 
  # Check for function name
  if (is.symbol(call[[1]])) {
    return (as.character(call[[1]]) == func)
  }
  
  # Check for a function name qualified by its package
  if (is.call(call[[1]]) && call[[1]][[1]] == "::") {
    return (as.character(call[[1]][[3]]) == func)
  }
  
  return (FALSE)
}

#' .ddg.num.calls.to returns the number of calls to the passed in function
#' @param func the name of a function to look for
#' @return the number of calls to the function on the stack
#' @noRd

.ddg.num.calls.to <- function (func) {
  calls.found <- sapply (sys.calls(), .ddg.is.call.to, func )
  return (sum(calls.found))
}


#' .ddg.inside.call.to returns True if there is a call to the passed in 
#' function anywhere on the call stack.  
#' @param func The name of a function
#' @return True if there is a call to the passed in function
#' @noRd

.ddg.inside.call.to <- function (func) {
  calls.found <- sapply (sys.calls(), .ddg.is.call.to, func )
  return (any (calls.found))
}

#' .ddg.get.call.to returns the most recent call on the stack that 
#' is to the named function.
#' @param func The name of a function
#' @return The call to that function
#' @noRd
.ddg.get.call.to <- function (func) {
  return (Find(function(call).ddg.is.call.to(call, func), sys.calls(), right=TRUE))
}

##################  Functions to handle tracing of read functions ##################

#' .ddg.create.file.read.functions.df initialize the information about functions 
#' that read from files
#' @return a data frame consisting of one row for each input function.
#' Each row contains the function name, and the name of the paramter that
#' holds the file argument.
#' @noRd

.ddg.create.file.read.functions.df <- function () {
  # Functions that read files
  function.names <-
      c ("read.table", 
          "read.dcf", 
          "readRDS",
          "readLines", "readBin", "readChar", "scan", "load", "readRenviron",
          
          # Sometimes source calls readLines but not always.  readLines is called
          # when called from RStudio, but not when called from Rscript.
          "source")
  
  # The argument that represents the file name
  param.names <-
      c ("file", 
          "file", 
          "file",
          "con", "con", "con", "file", "file", "path",
          "file")
  
  return (data.frame (function.names, param.names, stringsAsFactors=FALSE))
}

#' .ddg.clear.input.file clears out the list of input files.  This should be 
#' called on initialization and after the file nodes are created.
#' @return nothing
#' @noRd

.ddg.clear.input.file <- function () {
  .ddg.set ("input.files", character())
}

#' .ddg.add.input.file adds a file name to the input list.
#' @param fname the name of the file to add to the list, or a connection object
#' @return nothing
#' @noRd

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

#' .ddg.trace.input is called when one of the input functions is called in a script.
#' This function saves the name of the file that is being read from in
#' the input.files variable so that the proper nodes can be created when
#' the statement doing the output is complete. 
#' @return nothing
#' @noRd

.ddg.trace.input <- function () {
  
  # Get the frame corresponding to the output function being traced
  frame.number <- .ddg.get.traced.function.frame.number()
  
  # Filter out some calls based on what function called the input function.
  # The is.symbol test is used because it is possible that the caller is a 
  # closure and thus does not have a name.
  input.caller <- sys.call (frame.number - 1)[[1]]
  if (is.symbol (input.caller)) {
    input.caller.name <- as.character(input.caller)
    
    # Check if the function that called the input function is any ddg function.
    # If it is, ignore this call.  .ddg.load.history is an example of a 
    # function that does input that we would want to ignore.  
    if (startsWith (input.caller.name, "ddg") || 
              startsWith (input.caller.name, ".ddg") || 
              startsWith (input.caller.name, "prov")) {
      return()
    }
    
  }
  
  # Don't collect provenance when loading library packages.  Also, when writing out the
  # json, files get read in order to identify package version numbers.
  if (.ddg.inside.call.to ("library") || 
      .ddg.inside.call.to ("loadNamespace") ||
      .ddg.inside.call.to (".ddg.json.string")) {
    return()
  }
  
  # If we are sourcing a script, record the file as a sourced script instead of
  # as an input file.
  if (.ddg.inside.call.to ("source")) {
    if (.ddg.inside.call.to ("readLines")) {
      # The information was recorded when the call to source was found.
      return()
    }
    source.call <- .ddg.get.call.to ("source")
    frame.number <- .ddg.get.frame.number.for.func ("source")
    sourced.file.name <- eval (as.symbol ("file"), envir=sys.frame(frame.number))
    
    if (.ddg.is.connection(sourced.file.name)) {
      sourced.file.name <- showConnections(TRUE)[as.character(sourced.file.name), "description"]
    }
        
    # Store script number & name.
    snum <- .ddg.store.script.info (sourced.file.name)
    
    # Save a copy of the script
    sname <- basename(sourced.file.name)
    file.copy(sourced.file.name, paste(.ddg.path.scripts(), sname, sep="/"))
    return ()
  }
  
  # Get the name of the input function
  call <- sys.call (frame.number)
  fname <- as.character(call[[1]])
  
  # Remove the package name if present
  if (!is.symbol (fname) && length(fname > 1)) {
    fname <- fname[length(fname)]
  }
  
  #print (paste ("Input function traced: ", fname))
  
  # Get the name of the file parameter for the input function
  file.read.functions <- .ddg.get ("ddg.file.read.functions.df")
  file.param.name <- 
    file.read.functions$param.names[file.read.functions$function.names == fname]
  #print (paste ("Input file parameter:", file.param.name))
  
  # Get the value of the file parameter  
  input.file.name <- eval (as.symbol(file.param.name), envir = sys.frame(frame.number))
  #print (paste ("input.file.name =", input.file.name))

  # Save the file name so the file node can be created when the statement is complete.
  # we do not want to create the nodes because the procedure node to connect to does not
  # exist yet.
  .ddg.add.input.file (input.file.name)
}


#' .ddg.create.file.read.nodes.and.edges creates file nodes and data in edges for any files 
#' that were read during execution of the last R statement
#' @return nothing
#' @noRd

.ddg.create.file.read.nodes.and.edges <- function () {
  # Get the list of files that have been read by the last statement.
  files.read <- .ddg.get ("input.files")
  
  # Adds the files read to ddg.infilenodes for use in determining reads
  # and writes in the hashtable.
  .ddg.add.infiles (files.read)
  
  for (file in files.read) {
    # print (paste ("file read: ", file))
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
        if( ! .ddg.data.node.exists(file, dscope="undefined", dtype="File") )
          .ddg.file.copy(file)
        
        .ddg.data2proc(basename(file), dscope="undefined")
      }
      
      # If the filename contains a :, then it is referencing a file within 
      # a zip file, so checck that the zip file exists.      
      else if (grepl(":", file)) {
        zipfile <- sub (":.*", "", file)
        if (file.exists (zipfile)) {
          # Create the file node and edge
          .ddg.file.copy(zipfile, file, NULL)
          .ddg.data2proc(file, dscope="undefined")
        }
      }
    }
  }
  
  # Clear the list of input files now that they have been handled.
  .ddg.clear.input.file ()
}

#' .ddg.url.copy saves the contents of a web page referenced by a URL in the data
#' directory
#' @param url the URL as a string
#' @return the name of the file where the copy is stored.  This is 
#'   a relative path beginning with the data directory.
#' @noRd

.ddg.url.copy <- function (url) {
  # Get last part of the url.
  file.name <- basename(url)
  
  # Add number to file name.
  dfile <- paste(.ddg.dnum()+1, "-", file.name, sep="")
  
  # Get path plus file name to where the file will be copied
  dpath <- paste(.ddg.path.data(), "/", dfile, sep="")
  
  # Download and save the webpage
  curl::curl_download (url, dpath)
  
  if (.ddg.debug.lib()) print(paste("url.copy: ", url))
  return (paste(.ddg.data.dir(), dfile, sep="/"))
}

##################  Functions to handle tracing of write functions ##################


#' .ddg.create.file.write.functions.df initialize the information about functions 
#' that write to files
#' @return a data frame consisting of one row for each output function.
#' Each row contains the function name, and the name of the parameter that
#' holds the file argument.
#' @noRd

.ddg.create.file.write.functions.df <- function () {
  # Functions that write files.  We include the lowest level functions
  # used in R.  For example, write.csv is not in the list because it
  # uses write.table to do the output.
  function.names <-
      c ("write.table", "write", "writeLines",
          "writeChar", "writeBin", 
          "saveRDS", "save", "dput", "dump",
          "data")
  
  # The argument that represents the file name
  param.names <-
      c ("file", "file", "con", 
          "con", "con", 
          "file", "file", "file", "file",
          "...")
  
  return (data.frame (function.names, param.names, stringsAsFactors=FALSE))
}


#' .ddg.clear.output.file clears out the list of output files. This should be 
#' called on initialization and after the file nodes are created. 
#' @return nothing
#' @noRd

.ddg.clear.output.file <- function () {
  .ddg.set ("output.files", character())
  .ddg.set ("ddg.output.data", character())
}

#' .ddg.add.output.file adds a file name to the output list.
#' @param fname the name of the file to add to the list, or a connection object
#' @return nothing
#' @noRd

.ddg.add.output.file <- function (fname) {
  output.files <- .ddg.get("output.files")
  
  # Only add the file to the list if it is not already there.  It could be 
  # there if there are multiple functions called indirectly in one R statement
  # that write to the same file.
  if (!(fname %in% output.files) && is.character(fname) && 
      !endsWith (fname, ".snapshot")) {
    #print (paste ("Adding output file", fname))
    #print (sys.calls())
    .ddg.set ("output.files", append(output.files, fname))
  }
}

#' .ddg.add.output.data adds a name to the output list.
#' @param fname the name of the dataset to add to the list
#' @return nothing
#' @noRd

.ddg.add.output.data <- function (dname) {
  if (length(dname) == 0) return()

  output.data <- .ddg.get("ddg.output.data")
  
  # Only add the file to the list if it is not already there.  It could be 
  # there if there are multiple functions called indirectly in one R statement
  # that write to the same file.
  if (!(dname %in% output.data) && is.character(dname)) {
    #print (paste ("Adding output file", fname))
    #print (sys.calls())
    .ddg.set ("ddg.output.data", append(output.data, dname))
  }
}

#' .ddg.trace.output is called when one of the output functions is called in a script.
#' This function saves the name of the file that is being written in 
#' the output.files variable so that the proper nodes can be created when
#' the statement doing the output is complete.
#' @return nothing
#' @noRd

.ddg.trace.output <- function () {
  #print ("In .ddg.trace.output")
  
  # Get the frame corresponding to the output function being traced
  frame.number <- .ddg.get.traced.function.frame.number()
  
  # Check if the function that called the output function is a ddg function.
  # If it is, ignore this call.  The is.call check is here because it is
  # possible that the caller is a closure and thus does not have a name.
  # The frame.number might be 1 if we are in console mode.
  if (frame.number > 1) {
    output.caller <- sys.call (frame.number - 1)[[1]]
    if (is.symbol (output.caller)) {
      output.caller.name <- as.character(output.caller)
      if (startsWith (output.caller.name, "ddg") || 
          startsWith (output.caller.name, ".ddg") || 
          startsWith (output.caller.name, "prov")) {
        return()
      }
    }
  }
  
  # Check that the function is not being called due to saving a snapshot file.
  if (length (grep ("^.ddg.save.snapshot", sys.calls())) > 0) {
    return()
  }
  
  # Get the name of the output function
  call <- sys.call (frame.number)
  fname <- as.character(call[[1]])

  # Remove the package name if present
  if (length(fname > 1)) {
    fname <- fname[length(fname)]
  }
  
  #print (paste ("Output function traced: ", fname))
  
  # Set a flag to indicate that a new plot is started but
  # its name is not known yet.
  if (fname == "ggplot") {
    .ddg.set ("ddg.ggplot.created", TRUE)
    .ddg.set ("ddg.last.ggplot", "")
  }
  
  else if (fname == "data") {
    # The as.character gives us a vector that includes "vector" as its first element
    # The [-1] removes that element.
    output.data <- as.character(substitute(vector (...), env = sys.frame(frame.number)))[-1]
    .ddg.add.output.data (output.data)
  }
  
  else {
    # Get the name of the file parameter for the output function
    file.write.functions <- .ddg.get ("ddg.file.write.functions.df")
    file.param.name <- 
      file.write.functions$param.names[file.write.functions$function.names == fname]
    #print (paste ("Output file parameter:", file.param.name))
    
    # Get the value of the file parameter  
    output.file.name <- eval (as.symbol(file.param.name), envir = sys.frame(frame.number))
    #print (paste ("output.file.name =", output.file.name))
  
    # Save the file name so the file node can be created when the statement is complete.
    # we do not want to create the nodes because the procedure node to connect to does not
    # exist yet, and the file has not been written to yet.
    .ddg.add.output.file (output.file.name)
   }
}

#' .ddg.create.file.write.nodes.and.edges creates file nodes and data out edges for any files
#' that are written by the last statement executed.  It knows what the files are by looking
#' in the output.files variable stored in the ddg environment.
#' @return nothing
#' @noRd

.ddg.create.file.write.nodes.and.edges <- function () {
  # Get the list of files that have been written by the last statement.
  files.written <- .ddg.get ("output.files")
  
  for (file in files.written) {
    # print (paste ("file written: ", file))
    if (.ddg.is.connection(file)) {
      conn <- as.numeric(file)
      # If it is a closed connection, use the file it is connected to
      # If it is still open, don't use it because the contents on disk won't
      # be correct until it is closed.
      if (.ddg.is.connection.open(conn)) {
        next
      }
      file <- .ddg.get.connection.description(conn)
    }
    
    # Check that the file exists.  If it does, we will assume that
    # it was created by the write call that just executed.
    if (file.exists (file)) {
      # Create the file node and edge
      #print ("Copying file")
      .ddg.file.out (file)
    }
  }

  # If this file is written by ggsave and the plot was implicit, 
  # add an input edge for the last plot.
  if (.ddg.get ("ddg.implicit.plot")) {
    .ddg.data2proc (.ddg.get("ddg.last.ggplot"), dscope=NULL)
    
    # Clear the flag
    .ddg.set ("ddg.implicit.plot", FALSE)
  }
  
  # If Rplots was surprisingly created by Travis, delete it!
  # This seems to happen because Travis runs headless.
  if (.ddg.get ("ddg.remove.Rplots") && file.exists("Rplots.pdf")) {
    unlink ("Rplots.pdf")

    if (file.exists("Rplots.pdf")) {
      .ddg.set("ddg.ignore.rplots", TRUE)
    }

    .ddg.set ("ddg.remove.Rplots", FALSE)
  }
  
  # Handle data sets loaded with the data function.
  datasets <- .ddg.get ("ddg.output.data")
  sapply (datasets, 
          function (dataset) {
            .ddg.data.node ("Data", dataset, eval(as.name(dataset), envir=globalenv()), environmentName(globalenv()))
            .ddg.lastproc2data (dataset)    
          }
  )

  # Clear the list of output files now that they have been handled.
  .ddg.clear.output.file ()
}

#' .ddg.file.out creates a data node of type File.  The label
#' is the filename with the directory removed.
#' It copies the file to the DDG directory. A data flow edge
#' is also created from creating procedure node pname to the new file node.
#' 
#' @param filename name of the file.  The name should include the path
#'   to the file if it is not in the working directory.
#' @return the full path to the file that is saved.
#' @noRd
 
.ddg.file.out <- function(filename) {
  # Adds the files written to ddg.outfilenodes for use in determining reads
  # and writes in the hashtable.
  .ddg.add.outfiles (filename)
  
  dname <- basename(filename)
  
  # Create output file node called filename and copy file.
  saved.file <- .ddg.file.copy(filename, dname)
  
  # Create data flow edge from operation node to file node.
  .ddg.lastproc2data (dname)
  
  return (saved.file)
}


################ Functions to manage connections ####################3

#' .ddg.is.connection returns true if the object passed in is a connection
#' @param value an R object
#' @return true if the R object is a connection used to do I/O
#' @noRd

.ddg.is.connection <- function (value) {
  return ("connection" %in% class(value))
}

#' .ddg.get.open.connections returns a matrix containing the list of open connections
#' @return a matrix containing information about all open connections
#' @noRd

.ddg.get.open.connections <- function () { 
  return (showConnections(FALSE))
}

#' .ddg.get.connection.description returns the thing that the connection connects to.
#' This can be a filename, URL, socket, etc.
#' @param conn a connection.  This can either be a connection object
#' or the number associated with the connection.  
#' @return a description of the input/output connected to
#' @noRd

.ddg.get.connection.description <- function (conn) {
  return (showConnections(TRUE)[as.character(conn), "description"])  
}

#' .ddg.is.connection.open returns true if the connection is still open.
#' @param conn a connection.  This can either be a connection object
#' or the number associated with the connection.  
#' @return TRUE if the connection is open
#' @noRd

.ddg.is.connection.open <- function (conn) {
  return (showConnections(TRUE)[as.character(conn), "isopen"] == "opened")  
}

#' .ddg.can.read.connection returns true if the given connection was opened for reading, 
#' whether or not the connection is currently open.
#' @param conn a connection.  This can either be a connection object
#' or the number associated with the connection.
#' @return true if the given connection is readable
#' @noRd

.ddg.can.read.connection <- function (conn) {
  return (showConnections(TRUE)[as.character(conn), "can read"] == "yes")  
}

#' .ddg.can.write.connection returns true if the given connection was opened for writing, 
#' whether or not the connection is currently open.
#' @param conn a connection.  This can either be a connection object
#' or the number associated with the connection.
#' @return true if the given connection is writable
#' @noRd

.ddg.can.write.connection <- function (conn) {
  return (showConnections(TRUE)[as.character(conn), "can write"] == "yes")  
}

#' .ddg.create.file.close.functions.df initializes the information about functions 
#' that read from files
#' @return a data frame containing 2 columns:  
#'   names of functions that close connections, and
#'   name of the parameter that holds the connection
#' @noRd

.ddg.create.file.close.functions.df <- function () {
  # Functions that close connections
  function.names <- c ("close.connection")
  
  # The argument that represents the connection name
  param.names <- c ("con")
  
  return (data.frame (function.names, param.names, stringsAsFactors=FALSE))
}

#' .ddg.trace.close is called when any of the functions to close connections 
#' is called. This will add the description of any connection that was open for
#' writing to the list for which output file nodes should be created.
#' There are a few exceptions where a close function is called but
#' no node will be created:  if called directly from a ddg function, or if
#' any call on the stack is to capture.output, parse, or .ddg.snapshot,
#' or if there is any read or write function on the call stack.  If one of 
#' the read or write functions is closing the connection, then we will 
#' already be creating the right nodes. 
#' @return nothing
#' @noRd

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
    if (startsWith (close.caller.name, "ddg") || 
        startsWith (close.caller.name, ".ddg") || 
        startsWith (close.caller.name, "prov")) {  # 
      #print ("Returning - inside a ddg function")
      return()
    }
  }
  
  # Check that the function is not being called due to a call to capture output (used to 
  # hide standard output), parse (used to read the script being executed), or 
  # .ddg.snapshot (used to save copies of complex data values)
  if (.ddg.inside.call.to ("capture.output") || .ddg.inside.call.to ("parse") 
      || .ddg.inside.call.to (".ddg.snapshot") 
      || .ddg.inside.call.to(".ddg.save.annotated.script")) {
    #print ("Returning -- inside capture.ouput, parse or .ddg.snapshot")
      return()
    }
  
  # Check that we are not inside any read or write functions.  If we are,
  # the appropriate nodes will be created by those functions
  read.funs <- .ddg.get("ddg.file.read.functions.df")$function.names
  if (any (sapply (read.funs, .ddg.inside.call.to))) {
    #print ("Returning -- inside a read function")
    return()
  }
  
  write.funs <- .ddg.get("ddg.file.write.functions.df")$function.names
  if (any (sapply (write.funs, .ddg.inside.call.to))) {
    #print ("Returning -- inside a write function")
    return()
  }
  
  #print(paste("ddg.trace.close: close.caller =", close.caller))
  
  # Get the name of the close function
  call <- sys.call (frame.number)
  fname <- as.character(call[[1]])

  # Remove the package name if present
  if (length(fname > 1)) {
    fname <- fname[length(fname)]
  }
  # print (paste (".ddg.trace.close: fname = ", fname))

  if (fname == "ggsave") {
    filename <- eval (as.symbol("filename"), envir=sys.frame(frame.number))
    .ddg.add.output.file (filename)
    full.call <- match.call (ggplot2::ggsave, call, envir=sys.frame(frame.number))
    param.names <- names(full.call)
    
    # The plot parameter is optional in ggsave.  If not provided,
    # we need to link to the last plot created.  Set a flag so 
    # that is done after the statement completes.
    if (!("plot" %in% param.names)) {
      .ddg.set("ddg.implicit.plot", TRUE)
    }
    
    # Remember that Rplots.pdf did not exist and was not
    # explicitly requested.  If ggsave creates it, we 
    # will delete it after ggsave completes so the node
    # does not appear in the ddg.  This is a Travis issue,
    # which I believe happens because Travis runs headless, resulting
    # in an extra node in the ddg.
    if (filename != "Rplots.pdf" && !file.exists("Rplots.pdf")) {
      .ddg.set ("ddg.remove.Rplots", TRUE)
    }
  }
  else {
    # Get the name of the connection parameter for the close function
    file.close.functions <- .ddg.get ("ddg.file.close.functions.df")
    file.param.name <- 
      file.close.functions$param.names[file.close.functions$function.names == fname]
    #print (paste (".ddg.trace.close: fname = ", fname))
    #print (paste (".ddg.trace.close: file.param.name = ", file.param.name))
  
    # Get the value of the connection parameter  
    close.conn <- eval (as.symbol(file.param.name), envir = sys.frame(frame.number))
  
    # If the connection was opened for writing, then add the connection
    # to the list for which we create output file nodes.  We do not need 
    # to do anything if the connection was only open for reading because the
    # read code will have already created the node.
    if (.ddg.can.write.connection (close.conn)) {
      .ddg.add.output.file (.ddg.get.connection.description(close.conn))
    }
  }
}

#' .ddg.create.file.nodes.for.open.connections creates nodes for any writable connections
#' that are open. This is intended to be called when a script is finishing, so that we will 
#' have the connections associated with files that may have been written to, but not closed.
#' @return nothing
#' @noRd

.ddg.create.file.nodes.for.open.connections <- function () {
  openConns <- .ddg.get.open.connections()
  lapply (openConns[openConns[, "can write"] == "yes", "description"], 
          .ddg.add.output.file)
  .ddg.create.file.write.nodes.and.edges ()
}


################ Functions to track graphics calls ####################

#' .ddg.create.graphics.nodes.and.edges creates all the nodes and edges associated with 
#' graphics functions executed in the last line of R code.
#' @return nothing
#' @noRd

.ddg.create.graphics.nodes.and.edges <- function () {
  .ddg.add.graphics.device.node()
  .ddg.add.graphics.io ()
  .ddg.capture.graphics()
  .ddg.clear.device.nodes ()
}

#' .ddg.clear.device.nodes clears the information that we need to reset with each 
#' R statement executed.
#' @return nothing
#' @noRd

.ddg.clear.device.nodes <- function () {
  .ddg.set ("ddg.new.device.nodes", character())
  .ddg.set ("ddg.rplots.pdf.saved", FALSE)
  .ddg.set ("ddg.captured.devices", numeric())
}

#' .ddg.add.device.node adds a device node.
#' .ddg.new.device.nodes is the list of device nodes created in the previous
#' R statement.  Since an R statement may result in multiple calls to graphics
#' functions, we want to remember which dev nodes we have created so we don't
#' end up with duplicates attached to the same node. 
#' @return nothing
#' @noRd

.ddg.add.device.node <- function (new.device.node) {
  device.nodes <- .ddg.get ("ddg.new.device.nodes")
  .ddg.set ("ddg.new.device.nodes", append(device.nodes, new.device.node))
}

#' .ddg.create.device.table creates an empty device table to remember which file
#' names are associated with each graphic device
#' @return nothing
#' @noRd

.ddg.create.device.table <- function() {
  device.table <- 
      data.frame(device.number = numeric(),
                 file.name = character(),
                 stringsAsFactors = FALSE)
  .ddg.set ("ddg.device.table", device.table)
}

#' .ddg.add.to.device.table adds a binding between a device number and a file name
#' to the device table. 
#' @param device.number the number of the graphics device
#' @param file.name the name of the file being written to
#' @return nothing
#' @noRd

.ddg.add.to.device.table <- function (device.number, file.name) {
  device.table <- .ddg.get ("ddg.device.table")
  
  # If the number is in the table, update the associated file name
  if (device.number %in% device.table$device.number) {
    device.table$file.name[device.table$device.number == device.number] <- file.name
  }
  
  # Add a new entry for the device number and file name
  else {
    device.table <- rbind (device.table, data.frame (device.number, file.name,
            stringsAsFactors = FALSE))
  }

  .ddg.set ("ddg.device.table", device.table)
}

#' .ddg.get.file.for.device returns the file name associated with a graphics device
#' @param device.number the number of the graphics device to look up 
#' @return the name of the file associated with the device number.
#' Returns an empty string if the device number is not in the table.
#' @noRd

.ddg.get.file.for.device <- function (device.number) {
  device.table <- .ddg.get ("ddg.device.table")

  if (device.number %in% device.table$device.number) {
    return (device.table$file.name[device.table$device.number == device.number])
  }
  else {
    return ("")
  }
}

#' .ddg.create.graphics.functions.df initialize the information about functions that 
#' initialize graphics devices
#' @return a data frame consisting of one row for each function.
#' Each row contains the function name, and the name of the parameter that
#' holds the file argument.
#' @noRd

.ddg.create.graphics.functions.df <- function () {
  sysname <- Sys.info()[["sysname"]]
  # Functions that read files and the names of the arguments that hold file names
  if (sysname == "Windows") {
    function.names <-
        c ("pdf", "cairo_pdf", "postscript", "cairo_ps", "bmp", "jpeg", 
           "png", "svg", "tiff", "x11", "X11", "windows")
    param.names <-
        c ("file", "filename", "file", "filename", "filename", "filename", 
           "filename", "filename", "filename", NA, NA, NA)
  }
  else if (sysname == "Darwin") {  # Running on a Mac
    function.names <-
        c ("pdf", "cairo_pdf", "postscript", "cairo_ps", "bmp", "jpeg", 
           "png", "svg", "tiff", "x11", "X11", "quartz")
    param.names <-
        c ("file", "filename", "file", "filename", "filename", "filename", 
           "filename", "filename", "filename", NA, NA, NA)
  }
  else {  # Running on Linux
    function.names <-
        c ("pdf", "cairo_pdf", "postscript", "cairo_ps", "bmp", "jpeg", 
           "png", "svg", "tiff", "x11", "X11")
    param.names <-
        c ("file", "filename", "file", "filename", "filename", "filename", 
           "filename", "filename", "filename", NA, NA)
  }
  
  return (data.frame (function.names, param.names, stringsAsFactors=FALSE))
}

#' .ddg.trace.graphics.open is called when a function that opens a graphics device is called.
#' If this call was due to a call to .ddg.capture.graphics or .ddg.trace.graphics.update,
#' the function returns without doing anything.
#' Otherwise, if a file was created to hold the graphics, it records the file name.
#' It also sets the .ddg.add.device.output flag so that when the current R statement completes
#' the appropriate nodes and edges can be created.
#' @return nothing
#' @noRd

.ddg.trace.graphics.open <- function () {
  
  if (.ddg.inside.call.to (".ddg.capture.graphics") || 
      .ddg.inside.call.to (".ddg.trace.graphics.update") ||
      .ddg.inside.call.to (".ddg.graphic.snapshot") ||
      .ddg.inside.call.to ("ggsave")) {
    return()
  }
  
  #print ("In .ddg.trace.graphics.open")
  #print (sys.calls())
  
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
  graphics.functions <- .ddg.get ("ddg.graphics.functions.df")
  file.param.name <- 
    graphics.functions$param.names[graphics.functions$function.names == fname]

  # X11 and quartz device writes to the screen so there is no file parameter
  if (is.na (file.param.name)) {
    .ddg.set("ddg.no.graphics.file", TRUE)
    .ddg.set("ddg.last.graphics.file", "")
  }
  else {
    #print(paste (".ddg.trace.graphics: file.param.name =", file.param.name))
  
    # Get the value of the file parameter  
    file <- eval (as.symbol(file.param.name), envir = sys.frame(frame.number))
    #print(paste (".ddg.trace.graphics.open: file =", file))
    .ddg.set("ddg.no.graphics.file", FALSE)
    .ddg.set ("ddg.last.graphics.file", file)
  }
  
  # Set the flag to tell .ddg.add.graphics.device.node that it has work to do 
  # when it gets called.  We cannot call that function here because we 
  # need to wait until the R statement completes execution so that the 
  # procedure node exists before we create the graphics data nodes and edges.
  .ddg.set ("ddg.add.device.output", TRUE)
}

#' .ddg.add.graphics.device.node creates an output node for a graphics device and 
#' connects it to the last procedural node.  Does nothing if the last R statement
#' did not write to a graphics device. 
#' @return nothing
#' @noRd

.ddg.add.graphics.device.node <- function() {
  # Check if a graphics device was written to
  if (!.ddg.get ("ddg.add.device.output")) {
    return()
  } 
  
  #print ("In .ddg.add.graphics.device.node")
  #print (paste ("dev.list =", grDevices::dev.list(), 
  #              names(grDevices::dev.list()), collapse=", "))
  #print (paste ("dev.cur =", grDevices::dev.cur()))
  
  if (!names(grDevices::dev.cur()) %in% c("RStudioGD", "quartz", "windows")) {
    # Record the binding between the current device and the graphics file, if
    # a file is being used.
    if (.ddg.is.set ("ddg.last.graphics.file") && 
        .ddg.get("ddg.last.graphics.file") != "") {
      .ddg.add.to.device.table (grDevices::dev.cur (), 
                                .ddg.get ("ddg.last.graphics.file"))
    }
    else {
      .ddg.set("ddg.no.graphics.file", TRUE)
    }
    
    tryCatch(
        # Allows dev.print to work when we want to save the plot.
        # Only do this if the graphics is going to a file.  It seems
        # that it should also work if the output is going to the screen, but
        # it doesn't.
        grDevices::dev.control("enable"),
        error = function (e) return()
    )
  }

  # Add the newly-opened graphics device to the list of open devices
  .ddg.set("ddg.open.devices", union(.ddg.get("ddg.open.devices"), grDevices::dev.cur()))

  # Create a node for the graphics device and connect it to the last procedural node.
    dev.node.name <- paste0("dev.", grDevices::dev.cur())

    # Create graphics device node and edge if ddg.details is True.
    if (.ddg.details()) {
      .ddg.device.node(dev.node.name)
      .ddg.lastproc2data(dev.node.name)
    }

  # Remember that the device node was created for this statement to avoid duplicates.
  .ddg.set ("ddg.add.device.output", FALSE)
  .ddg.add.device.node (dev.node.name)
}

#' .ddg.trace.graphics.update is called when a function that updates graphics is called.
#' If the call is within a call to .ddg.capture.graphics, it does nothing.
#' Otherwise, it sets a flag so that we create the device node with
#' input and output edges when the R statement completes.
#' @return nothing
#' @noRd

.ddg.trace.graphics.update <- function () {
  if (.ddg.inside.call.to (".ddg.capture.graphics") || .ddg.inside.call.to ("ggsave")) { 
    return()
  }
  
  #print ("In .ddg.trace.graphics.update")
  #print (sys.calls())
  .ddg.set ("ddg.add.device.io", TRUE)
}

#' .ddg.add.graphics.io adds data in and data out nodes that represent the 
#' current device.
#' @return nothing
#' @noRd

.ddg.add.graphics.io <- function () {
  # Check if the last R statement updated graphics
  if (!.ddg.get ("ddg.add.device.io")) {
    return ()
  }
  
  #print ("In .ddg.add.graphics.io")
  
  dev.node.name <- paste0("dev.", grDevices::dev.cur())
  
  # Make sure we did not already create the device node for this statement. 
  if (!(dev.node.name %in% .ddg.get ("ddg.new.device.nodes"))) {
    
    # Check if there is already a node for this device. 
    if (grDevices::dev.cur() %in% .ddg.get("ddg.open.devices")) {

      # Create graphics device node and edges if ddg.details is True
      if (.ddg.details()) {
        # Create an input edge from that node to the last procedure node
        .ddg.data2proc(dev.node.name, dscope = NULL)

        # Add an output node with the same name and make it an output from
        # the last procedure node.
        .ddg.device.node(dev.node.name)
        .ddg.lastproc2data(dev.node.name)
      }

      # Remember that the node was created.
      .ddg.add.device.node (dev.node.name)
    }
    
    # If there is no previous device node for this device, it means 
    # that the output is going to the default graphics device, not a file, 
    # so there has been no call like pdf or jpg that would have created the data node.
    # In that case, treat this like a device creation, rather than an update.
    else {
      # Add the newly-opened graphics device to the list of open devices
      .ddg.set ("ddg.add.device.output", TRUE)
      .ddg.add.graphics.device.node ()
      return()
    }
    
  }
  
  # Clear the flag to prepare for the next statement.
  .ddg.set ("ddg.add.device.io", FALSE)
}

#' .ddg.trace.graphics.close is called when a graphics device is closed.
#' If the graphics is going to the screen, it saves it to a file,
#' since we need to do that before the device closes.  If it is
#' going to a file, we need to wait until after the device is
#' closed to copy the file.
#' @return nothing
#' @noRd

.ddg.trace.graphics.close <- function () {
  if (.ddg.inside.call.to (".ddg.capture.graphics") || .ddg.inside.call.to ("ggsave")) { 
    return()
  }
  
  #print ("In .ddg.trace.graphics.close")
  #print (paste ("dev.list =", grDevices::dev.list(), 
  #              names(grDevices::dev.list()), collapse=", "))
  #print (paste ("dev.cur =", grDevices::dev.cur()))
  
  # Set the flag so that .ddg.capture.graphics executes after the
  # R statement completes.
  .ddg.set ("ddg.add.device.close", TRUE)
  .ddg.set("ddg.dev.number", grDevices::dev.cur())
  
  
  # Output is going to the screen
  if (.ddg.get("ddg.no.graphics.file") || names(grDevices::dev.cur()) == "RStudioGD") {
    # Write the graphics to a file and record the file name
    # in the device table.
    file <- .ddg.capture.current.graphics()
    .ddg.set("ddg.no.graphics.file", FALSE)
    if (!is.null(file)) {
      .ddg.set ("ddg.last.graphics.file", file)
      .ddg.add.to.device.table (grDevices::dev.cur (), file)
    }
  }
}

#' .ddg.capture.graphics captures the screen graphics to a file
#' @param called.from.save If true, it will recursively capture the graphics
#' from all open devices.
#' @return nothing
#' @noRd

.ddg.capture.graphics <- function(called.from.save = FALSE) {
  if (!.ddg.get ("ddg.add.device.close") && !called.from.save) {
    return()
  }
  
  #print ("In .ddg.capture.graphics")
  
  # Determine which device to capture graphics for.  When called.from.save
  # we will be capturing graphics from all open devices.
  if (called.from.save) {
    dev.number <- grDevices::dev.cur()
    
    # Device 1 is standard output.  When this comes up as dev.cur, it
    # means we are done capturing graphics.
    if (dev.number == 1) {
      return()
    }
  }
  else {
    dev.number <- .ddg.get("ddg.dev.number")
  }
  #print (paste ("ddg.capture.graphics: Device being captured: ", dev.number))
  
  # Remove from the open.devices list so that we do not get a device node created
  .ddg.set("ddg.open.devices", setdiff(.ddg.get("ddg.open.devices"), dev.number))
  
  # If graphics is going to a file, determine what file
  dev.name <- .ddg.get.file.for.device (dev.number)
  if (dev.name == "") {
    # Capture screen graphics
    graphics.file <- .ddg.capture.current.graphics()
  }
  
  else {
    graphics.file <- .ddg.get.file.for.device (dev.number)
    
    # Check if the device is still open and close it if it is
    # We need to do this so that the file.out call can
    # copy the file.
    if (dev.number %in% grDevices::dev.list() && dev.number != 1) {
      grDevices::dev.off(dev.number)
    }
  }
  
  # If going to a file, copy the file and create a node for it.
  if (!is.null (graphics.file)) {
    # print (paste ("graphics file: ", graphics.file))
    .ddg.file.out (graphics.file)
    
    # Delete files that were created by capturing the screen
    if (startsWith (graphics.file, "dev.off") && file.exists(graphics.file)) {
      file.remove (graphics.file)
    }
  
    # Add an input edge from the current device
    dev.node.name <- paste0("dev.", dev.number)
  
    # If the device was opened but never written to there will be no previous node.
    # so don't try to create the edge in that case.
    if (.ddg.data.node.exists (dev.node.name)) {
      .ddg.data2proc(dev.node.name, NULL)
    }
    
    # Clear this flag to indicate that the graphics file has been saved.
    .ddg.set ("ddg.no.graphics.file", TRUE)
  }

  # If called from save, we should capture all the open graphics devices
  if (called.from.save) {
    # Remember which devices have been captured
    .ddg.set ("ddg.captured.devices", c(.ddg.get("ddg.captured.devices"), dev.number))
    
    # If the device just captured is still the current device, move on to the next
    # open device.  If it is not the current device, use the current device.
    if (dev.number == grDevices::dev.cur()) {
      grDevices::dev.set()
    }
    
    # If the current device has not been captured yet, recurse to save the next one.
    if (!(grDevices::dev.cur() %in% .ddg.get("ddg.captured.devices"))) {
      .ddg.set("ddg.dev.number", grDevices::dev.cur())
      .ddg.capture.graphics (TRUE)
    }
  }
  
  .ddg.set ("ddg.add.device.close", FALSE)
  return()
}

#' .ddg.capture.current.graphics captures what is on the current display to a file, 
#' creates a file node and connects to the ddg.
#' @return the name of the file containing the captured graphics
#' @noRd

.ddg.capture.current.graphics <- function() {
  #print ("In .ddg.capture.current.graphics")
  #print(sys.calls())
  
  # Create the file name to save the screen graphics to
  file <- paste0("dev.off.", .ddg.dnum()+1, ".pdf")
  
  # Save the graphic to a file temporarily
  file.written <- NULL
  
  # dev.print fails when running from the test scripts, or Rscript in general
  # In that case, check for the existence of Rplots.pdf, which is 
  # where Rscript places plots sent to the default graphics.
  if (names(grDevices::dev.cur()) == "pdf" && !.ddg.get("ddg.ignore.rplots")) {
    if (file.exists ("Rplots.pdf") && !.ddg.get("ddg.rplots.pdf.saved")) {

      if (grDevices::dev.cur() != 1) { 
        grDevices::dev.off()
      }

      .ddg.set ("ddg.rplots.pdf.saved", TRUE)
      return("Rplots.pdf")
    }
  }

  tryCatch (
    {
      # Try to save the graphics to a file
      grDevices::dev.print(device=grDevices::pdf, file=file)  
      file.written <- file
    },
    error = function(e) {
      # If the dev.off file was created, delete it.
      if( file.exists(file) )
        file.remove(file)
    }
  )
  return(file.written)
}
