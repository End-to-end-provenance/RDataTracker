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

############################ API.R #############################

# This file contains files that are exported:

# prov.init - intializes a new provenance graph
# prov.save - saves the current provenance graph
# prov.quit - saves the current provenance graph and finalizes it
# prov.run - initiates execution of a script
# prov.json - returns the current provenance graph as a prov-json string


#' .ddg.init intializes a new provenance graph.
#' @param prov.dir the directory where the provenance graph will be 
#' saved. If not provided, the directory specified by the prov.dir 
#' option is used. Otherwise the R session temporary directory
#' is used.
#' @param overwrite if FALSE, includes a time stamp in the provenance
#' @param save.debug If TRUE, debug files are saved to the debug directory.
#' This is intended for developers of the RDataTracker / provR package.
#' @return .ddg.init initializes the provenance collector.  The .ddg.init
#' function does not return a value.
#' @noRd

.ddg.init <- function(prov.dir = NULL, overwrite = TRUE, save.debug = FALSE) {
  preloaded = loadedNamespaces()
  .ddg.set("ddg.preloaded.libraries", preloaded)
  
  # Initialize tables
  #print ("Initiailzing tables")
  .ddg.init.tables()
  
  # Set up for console mode
  if (!.ddg.script.mode()) {
    .ddg.set("ddg.r.script.path", "console.R")
    .ddg.set("ddg.details", TRUE)
  }
  
  # Get R script path
  #print ("Getting R script path")
  r.script.path <- .ddg.r.script.path()
  
  # Set path for provenance graph
  #print ("Setting provenance path")
  .ddg.set.path (prov.dir, r.script.path, overwrite)
  
  # Save value of save.debug
  .ddg.set("ddg.save.debug", save.debug)
  
  # Remove files from DDG directory
  #print ("Removing ddg files")
  .ddg.flush.ddg()
  
  # Create DDG directories
  #print ("Creating directories")
  .ddg.init.environ()
  
  # Script mode: adjust & store R script path
  if (.ddg.script.mode()) {
    #print ("Setting up script mode")
    
    # RMarkdown script
    if (tools::file_ext(r.script.path) == "Rmd") {
      .ddg.set("ddg.is.rmarkdown", TRUE)
      .ddg.set("ddg.rmd.script.path", r.script.path)
      output.path <- paste(.ddg.path.scripts(), "/", 
                            basename(tools::file_path_sans_ext(r.script.path)), ".R", sep = "")
      print(paste("Please note: if your RMarkdown script is not deterministic (that is,",
					"if it does not always produce the same result as is the case when it relies on random number generation),", 
					"then the provenance collected may not exactly match the PDF document produced by RMarkdown."))
      .ddg.markdown(r.script.path, output.path)
      .ddg.set("ddg.r.script.path", output.path)
      
      # R script
    } else {
      .ddg.set("ddg.is.rmarkdown", FALSE)
      .ddg.set("ddg.markdown.output", NULL)
      .ddg.set("ddg.r.script.path",
               normalizePath(r.script.path, winslash = "/", mustWork = FALSE))
    }
    
    # Console mode: Set up a callback to be notified after each statement
    # completes execution and build the corresponding portions of the 
    # provenance graph.
  } else {
    #print ("Setting up console mode")
    .ddg.set("ddg.is.rmarkdown", FALSE)
    .ddg.set("ddg.markdown.output", NULL)
    .ddg.store.console.info ()
    .ddg.set("ddg.console.commands", vector())
    
    .ddg.trace.task <- function (task, result, success, printed) {  
      # Create the provenance for the new command
      command <- deparse(task)
      trimmed <- trimws(command[1])
      if (!startsWith(trimmed, "prov.init") && !startsWith(trimmed, "prov.save")) {
        .ddg.add.to.console(deparse(task))
        #print(deparse(task))
      }
      .ddg.parse.commands(as.expression(task), script.name="Console", script.num=1, environ = .GlobalEnv, 
                          run.commands = FALSE)
      return(TRUE)    
    }
    
    .ddg.set ("ddg.taskCallBack.id", addTaskCallback(.ddg.trace.task))
  }
  
  # Mark graph as initilized.
  .ddg.set("ddg.initialized", TRUE)
  
  # Add a Console start node if running from the console.
  if (!.ddg.script.mode()) {
    .ddg.add.start.node (node.name = "Console")
  }
  
  .ddg.set("ddg.func.depth", 0)
  .ddg.set("ddg.cur.cmd.stack", vector())
  
  # Initialize the table used to track use of non-locals
  # within functions
  #print ("init func def table")
  .ddg.init.function.def.table ()
  
  # A named list, where the name is a variable of type environment
  # Associated with each environment name is a vector of the variables
  # in that environment. 
  .ddg.set ("ddg.envList", list())
  
  # Records the libraries loaded by the script itself
  .ddg.set ("ddg.script.libraries", vector())
  
  # Store time when script begins execution.
  .ddg.set("ddg.start.time", .ddg.timestamp())
  
  # Initialize the I/O tracing code
  #print ("Init io tracing")
  .ddg.init.iotrace ()
  
  
  
  #print ("returning from .ddg.init")
  
  invisible()
}

#' .ddg.set.path sets the path where the DDG will be stored.  It creates the 
#' directory if it does not currently exist.
#' The base directory is set as follows:
#' (1) the directory specified by the user in the parameter prov.dir, or
#' (2) the directory specified by the user as the value of the option "prov.dir", or
#' (3) the R session temporary directory. If the directory specified by the user
#' is a period (.), the base directory is set to the current working directory.
#' The provenance graph is stored in a subdirectory of the base directory called 
#' "prov_console" in console mode or "prov_[script name]" in script mode. If overwrite = 
#' FALSE, a timestamp is added to the directory name.
#' @param prov.dir name of directory.  This can be a directory name, ".", or NULL.
#' @param r.script.path the full path to the R script file that is being 
#' executed.
#' @param overwrite If FALSE, a timestamp is added to the directory name
#' @return the name of the directory where the ddg should be stored
#' @noRd

.ddg.set.path <- function(prov.dir, r.script.path, overwrite) {
  
  # Directory specified by prov.dir parameter
  if (!is.null(prov.dir)) {
    if (prov.dir == ".") {
      base.dir <- getwd()
    } else {
      base.dir <- normalizePath(prov.dir, winslash = "/", mustWork = TRUE)
    }
  } 
  
  else {
    # Directory specified as an option for prov.dir
    prov.dir.option <- getOption("prov.dir")
    if (!is.null(prov.dir.option) && prov.dir.option != "") {
      if (prov.dir.option == ".") {
        base.dir <- getwd()
      } else {
        base.dir <- normalizePath(getOption("prov.dir"), winslash = "/", mustWork = TRUE)
      }
    } 
    
    # R session temporary directory
    else {
      # Normalize path
      base.dir <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
    }
  }
  
  # Remove final slash if present
  base.dir <- sub("/$", "", base.dir)
  
  # Console mode
  if (!.ddg.script.mode()) {
    ddg.path <- paste(base.dir, "/prov_console", sep="")
    console.dir <- paste(base.dir, "/console", sep="")
    .ddg.set("ddg.console.dir", console.dir)
    if (!dir.exists(console.dir)) dir.create(console.dir, recursive = TRUE)
    
    # Script mode
  } else {
    ddg.path <- paste(base.dir, "/prov_", 
                      basename(tools::file_path_sans_ext(r.script.path)), sep="")
  }
  
  # Add timestamp if overwrite = FALSE
  if (!overwrite) ddg.path <- paste(ddg.path, "_", .ddg.timestamp(), sep="")
  
  # Create directory if it does not exist
  if (!dir.exists(ddg.path)) dir.create(ddg.path, recursive = TRUE)
  
  .ddg.set("ddg.path", ddg.path)
}

#' .ddg.flush.ddg removes all files from the current DDG directories. 
#' @return nothing
#' @noRd

.ddg.flush.ddg <- function() {
  # Remove all files
  unlink(paste(.ddg.path(), "*.*", sep="/"))
  unlink(paste(.ddg.path.data(), "*.*", sep="/"))
  unlink(paste(.ddg.path.debug(), "*.*", sep="/"))
  unlink(paste(.ddg.path.scripts(), "*.*", sep="/"))
  
  invisible()
}

#' .ddg.save saves the current provenance graph to a prov-json file.
#' If more R statements are executed, the provenance for these statements
#' is added to the graph.
#' @param save.debug If TRUE, debug files are saved to the debug directory.
#'   This is intended for developers of the RDataTracker / provR package.
#' @return .ddg.save writes the current provenance to a file but does not 
#'   return a value.
#' @noRd

.ddg.save <- function(save.debug = FALSE) {
  if (!.ddg.is.init()) return(invisible())
  
  # If running from the console create a Console finish node.
  # Also create a start node for the next segment.
  if (!.ddg.script.mode()) {
    .ddg.add.finish.node ()
    .ddg.add.start.node (node.name = "Console")
    writeLines(.ddg.get("ddg.console.commands"), paste (.ddg.path.scripts(), "console.R", sep="/"))
  }
  
  # Save prov.json to file.
  .ddg.json.write()
  if (interactive()) print(paste("Saving prov.json in ", .ddg.path(), sep=""))
  
  # Save debug files to debug directory.
  if (save.debug || .ddg.save.debug()) {
    .ddg.save.debug.files()
  }
  
  invisible()
}

#' .ddg.quit saves and closes the current provenance graph.
#' @param save.debug If TRUE, debug files are saved to the debug directory.
#'   This is intended for developers of the RDataTracker / provR package.
#' @return .ddg.quit writes the current provenance to a file but does not 
#'   return a value.
#' @noRd

.ddg.quit <- function(save.debug = FALSE) {
  #print ("In .ddg.quit")
  if (!.ddg.is.init()) return(invisible())
  
  # If running from the console create a Console finish node.
  # Save the console commands in the provenance directory and also
  # in the console folder.
  if (!.ddg.script.mode()) {
    .ddg.add.finish.node ()
    console.commands <- .ddg.get("ddg.console.commands")
    
    if (length(console.commands) > 0) {
        # Save the console commands inside the provenance directory.
        writeLines(console.commands, paste (.ddg.path.scripts(), "console.R", sep="/"))
    
        # Also save the console commands in the console directory, not within the provenance directory
        # for this session.  This is saved in a timestamped file.
        writeLines(console.commands, paste (.ddg.get("ddg.console.dir"), paste0("console_", .ddg.timestamp(), ".R"), sep="/"))
    }
  }
  
  # If there are any connections still open when the script ends,
  # create nodes and edges for them.
  #print ("Closing connections")
  tryCatch(.ddg.create.file.nodes.for.open.connections (),
           error = function(e) {
             if (.ddg.debug.lib()) {
               print ("Error creating file nodes for open connections when quitting")
             }
           })
  
  # If there is a display device open, grab what is on the display
  #print ("Closing display device")
  if (length(grDevices::dev.list()) >= 1) {
    tryCatch (.ddg.capture.graphics(called.from.save = TRUE),
              error = function (e) print(e))
  }
  
  # If we ran an RMarkdown script, save a copy of the formatted output
  #print ("Rmarkdown stuff")
  rmarkdown.output <- .ddg.get("ddg.markdown.output")
  if (! is.null (rmarkdown.output)) {
    if (file.exists (rmarkdown.output)) {
      .ddg.file.out (rmarkdown.output)
    }
  }
  
  # Turn off the I/O tracing and console tracing.
  #print ("Stopping I/O tracing.")
  .ddg.stop.iotracing()
  if (.ddg.is.set ("ddg.taskCallBack.id")) {
    removeTaskCallback (.ddg.get ("ddg.taskCallBack.id"))
  }
  
  # Delete temporary files.
  #print ("Deleting temporary files")
  .ddg.delete.temp()
  
  # Mark graph as not initialized.
  .ddg.set("ddg.initialized", FALSE)
  
  # Save prov.json to file.
  #print ("Writing json")
  .ddg.json.write()
  if (interactive()) print(paste("Saving prov.json in ", .ddg.path(), sep=""))
  
  # Save debug files to debug directory.
  if (save.debug || .ddg.save.debug()) {
    .ddg.save.debug.files()
  }
  
  # Set script mode to FALSE
  .ddg.set("ddg.script.mode", FALSE)
  
  invisible()
}

#' .ddg.run initiates execution of a script and collects provenance as 
#' the script executes.
#' @param r.script.path the full path to the R script file that is being
#' executed.
#' @return .ddg.run runs a script, collecting provenance as it does so.  
#' It does not return a value. 
#' @noRd

.ddg.run <- function(r.script.path, exprs, ...) {
  

  
  # Execute script and catch any error messages
  tryCatch({
    if(.ddg.get("ddg.is.rmarkdown")){
      #rmarkdown::render(r.script.path)
      .ddg.chunk.source(.ddg.get("ddg.rmd.script.path"),...)
    } else {
      .ddg.source(.ddg.r.script.path(), exprs = exprs, ignore.ddg.calls = FALSE, ...)
    }
  }
  
  # Add finish nodes for anything left open due to errors
  , finally = {
    .ddg.close.blocks()
    .ddg.quit(.ddg.save.debug())
  }
  )
  
  
  invisible()
}

#' .ddg.source reads and executes an R script in the specified
#' environment. .ddg.source mimics the behaviour of the R source command, 
#' with similar input parameters and results, but with the additional 
#' parameter ignore.ddg.calls.
#' @param file the name of the R script file to source.
#' @param local the environment in which to evaluate parsed
#' expressions. If TRUE, the environment from which .ddg.source is
#' called. If FALSE, the user's workspace (global environment).
#' @param echo print each expression after parsing
#' @param print.eval print the result of each evaluation
#' @param verbose print extra diagnostics
#' @param max.deparse.length the maximum number of characters in output
#' for deparse of a single expression
#' @param chdir change the R working directory temporarily to
#' the directory containing the file to be sourced
#' @param encoding encoding to be assumed when file is a
#' character string
#' @param ignore.ddg.calls if TRUE, ignore DDG function calls
#' @param calling.script the number of the calling script
#' @param startLine the line that the source call starts on
#' @param startCol the column that the source call starts on
#' @param endLine the line that the source call ends on
#' @param endCol the column that the source call ends on
#' @return nothing
#' @noRd
.ddg.source <- function (file, local = FALSE, echo = verbose, print.eval = echo, 
                         exprs, spaced = use_file, verbose = getOption("verbose"), 
                         prompt.echo = getOption("prompt"), max.deparse.length = 150, 
                         width.cutoff = 60L, deparseCtrl = "showAttributes", chdir = FALSE, 
                         encoding = getOption("encoding"), continue.echo = getOption("continue"), 
                         skip.echo = 0, 
                         ignore.ddg.calls = TRUE, calling.script = 1, startLine = NA, startCol = NA,
                         endLine = NA, endCol = NA) 
{
  # This function is largely derived from R's source function.  Part of R's
  # source function also appears in .ddg.parse.commands and .ddg.echo.  To
  # simply future maintenance the current version of R's source function is
  # at the bottom of this file in a comment.
  
  # Save provenance information about the script.
  snum <- .ddg.store.script.info(file)
  sname <- basename(file)
  file.copy(file, paste(.ddg.path.scripts(), sname, sep = "/"))
  
  # We always want to keep the source when we collect provenance,
  # so this is not a parameter of .ddg.source.
  keep.source <- TRUE
  
  #### Start section taken from R's source function ####
  envir <- if (isTRUE(local)) 
    parent.frame()
  else if (isFALSE(local)) 
    .GlobalEnv
  else if (is.environment(local)) 
    local
  else stop("'local' must be TRUE, FALSE or an environment")
  if (!missing(echo)) {
    if (!is.logical(echo)) 
      stop("'echo' must be logical")
    if (!echo && verbose) {
      warning("'verbose' is TRUE, 'echo' not; ... coercing 'echo <- TRUE'")
      echo <- TRUE
    }
  }
  if (verbose) {
    cat("'envir' chosen:")
    print(envir)
  }
  if (use_file <- missing(exprs)) {
    ofile <- file
    from_file <- FALSE
    srcfile <- NULL
    if (is.character(file)) {
      have_encoding <- !missing(encoding) && encoding != 
        "unknown"
      if (identical(encoding, "unknown")) {
        enc <- utils::localeToCharset()
        encoding <- enc[length(enc)]
      }
      else enc <- encoding
      if (length(enc) > 1L) {
        encoding <- NA
        # In source function, but CRAN says not to change user's options
        #owarn <- options(warn = 2)
        for (e in enc) {
          if (is.na(e)) 
            next
          zz <- file(file, encoding = e)
          res <- tryCatch(readLines(zz, warn = FALSE), 
                          error = identity)
          close(zz)
          if (!inherits(res, "error")) {
            encoding <- e
            break
          }
        }
        # Reverts to user's options.  Removed for CRAN
        #options(owarn)
      }
      if (is.na(encoding)) 
        stop("unable to find a plausible encoding")
      if (verbose) 
        cat(gettextf("encoding = \"%s\" chosen", encoding), 
            "\n", sep = "")
      if (file == "") {
        filename <- "stdin"  # Line added for provenance
        file <- stdin()
        srcfile <- "<stdin>"
      }
      else {
        filename <- file
        file <- file(filename, "r", encoding = encoding)
        on.exit(close(file))
        lines <- readLines(file, warn = FALSE)
        on.exit()
        close(file)
        srcfile <- srcfilecopy(filename, lines, file.mtime(filename)[1], 
                               isFile = TRUE)
        
        loc <- utils::localeToCharset()[1L]
        encoding <- if (have_encoding) 
          switch(loc, `UTF-8` = "UTF-8", `ISO8859-1` = "latin1", 
                 "unknown")
        else "unknown"
      }
    }
    else {
      filename <- "Connection"  # Line added for provenance
      lines <- readLines(file, warn = FALSE)
      srcfile <- srcfilecopy(deparse(substitute(file)), lines)
    }
    
    # parse calls are changed.  .Internal is not allowed to be called from
    # packages.  This appears to behave the same.
    exprs <- if (!from_file) {
      if (length(lines)) 
        parse(file=stdin(), n = -1, text=lines, prompt="?", 
              keep.source=TRUE, srcfile = srcfile, encoding = encoding)
      
      else expression()
    }
    else parse(file=file, n = -1, text=NULL, prompt="?", 
               keep.source=TRUE, srcfile = srcfile, 
               encoding = encoding)
    on.exit()
    if (from_file) 
      close(file)
    if (verbose) 
      cat("--> parsed", length(exprs), "expressions; now eval(.)ing them:\n")
    if (chdir) {
      if (is.character(ofile)) {
        if (grepl("^(ftp|http|file)://", ofile)) 
          warning("'chdir = TRUE' makes no sense for a URL")
        else if ((path <- dirname(ofile)) != ".") {
          owd <- getwd()
          if (is.null(owd)) 
            stop("cannot 'chdir' as current directory is unknown")
          on.exit(setwd(owd), add = TRUE)
          setwd(path)
        }
      }
      else {
        warning("'chdir = TRUE' makes no sense for a connection")
      }
    }
  }
  else {
    #    Commented out for provenance because if a user passes in expressions
    #    rather than a file to prov.run, we save the expressions in a file and
    #    pass both to .ddg.source.  We report this error in prov.run if the
    #    user calls that function incorrectly.
    #    if (!missing(file)) 
    #      stop("specify either 'file' or 'exprs' but not both")
    if (!is.expression(exprs)) 
      exprs <- as.expression(exprs)
  }
  Ne <- length(exprs)
  yy <- NULL
  lastshown <- 0
  srcrefs <- attr(exprs, "srcref")
  if (verbose && !is.null(srcrefs)) {
    cat("has srcrefs:\n")
    utils::str(srcrefs)
  }
  #### End section copied from R's source function ####
  
  ignores <- c("^library[(]RDataTracker[)]$", "^library[(]provR[)]$", 
               if (ignore.ddg.calls) "^ddg." else c("^prov.init", "^prov.run"))
  if (length(exprs) > 0) {
    .ddg.set("from.source", TRUE)
    if (.ddg.details()) {
      if(calling.script == 1) {
        .ddg.add.start.node(node.name = sname)
      }
      else {
        .ddg.add.start.node(node.name = paste0("source (\"", 
                                               sname, "\")"), script.num = calling.script, 
                            startLine = startLine, startCol = startCol, 
                            endLine = endLine, endCol = endCol)
      }
      
      # We have pulled the evaluation and echo code that is in R's
      # source function into .ddg.parse.commands.
      yy <- .ddg.parse.commands(exprs, sname, snum, environ = envir, 
                                ignore.patterns = ignores, echo = echo, print.eval = print.eval, 
                                max.deparse.length = max.deparse.length, run.commands = TRUE, 
                                continue.echo = continue.echo, skip.echo = skip.echo, prompt.echo = prompt.echo, 
                                spaced = spaced, verbose = verbose, deparseCtrl = deparseCtrl)
      
      if(calling.script == 1) {
        .ddg.add.finish.node()
      }
      else {
        .ddg.add.finish.node(script.num = calling.script, 
                             startLine = startLine, startCol = startCol, 
                             endLine = endLine, endCol = endCol)
      }
    }
    else {
      if(calling.script == 1)
        .ddg.proc.node("Operation", sname)
      yy <- .ddg.evaluate.commands(exprs, environ = envir)
    }
  }
  invisible(yy)
}

#' .ddg.chunk.source is an alternative to .ddg.source which parses an RMarkdown file, collecting provenance on the code. 
#' if details are set to TRUE in the chunk header, then detailed provenance is collected on that chunk. Otherwise, just input/output
#' information is collected.
#' @param file is the path of the RMarkdown file to be parsed
#' @param local the environment in which to evaluate parsed
#' expressions. If TRUE, the environment from which .ddg.source is
#' called. If FALSE, the user's workspace (global environment).
#' @param print.eval print the result of each evaluation
#' @param encoding encoding to be assumed when file is a
#' character string
#' @param ignore.ddg.calls if TRUE, ignore DDG function calls
#' @noRd

.ddg.chunk.source <- function(file, local = FALSE, echo = verbose, print.eval = echo, encoding = getOption("encoding"), 
                              verbose = getOption("verbose"), chdir = FALSE, ignore.ddg.calls = TRUE, ...){

  snum <- .ddg.store.script.info(file)
  sname <- basename(file)
  file.copy(file, paste(.ddg.path.scripts(), sname, sep = "/"))
  
  # We always want to keep the source when we collect provenance,
  # so this is not a parameter of .ddg.source.
  keep.source <- TRUE
  
  #### Start section taken from R's source function ####
  envir <- if (isTRUE(local)) 
    parent.frame()
  else if (isFALSE(local)) 
    .GlobalEnv
  else if (is.environment(local)) 
    local
  else stop("'local' must be TRUE, FALSE or an environment")
  if (!missing(echo)) {
    if (!is.logical(echo)) 
      stop("'echo' must be logical")
    if (!echo && verbose) {
      warning("'verbose' is TRUE, 'echo' not; ... coercing 'echo <- TRUE'")
      echo <- TRUE
    }
  }
  if (verbose) {
    cat("'envir' chosen:")
    print(envir)
  }

  ofile <- file
  srcfile <- NULL
  have_encoding <- !missing(encoding) && encoding != "unknown"
  if (identical(encoding, "unknown")) {
    enc <- utils::localeToCharset()
    encoding <- enc[length(enc)]
  }
  else enc <- encoding
  if (length(enc) > 1L) {
    encoding <- NA
    # In source function, but CRAN says not to change user's options
    #owarn <- options(warn = 2)
    for (e in enc) {
      if (is.na(e)) 
        next
      zz <- file(file, encoding = e)
      res <- tryCatch(readLines(zz, warn = FALSE), 
                          error = identity)
      close(zz)
      if (!inherits(res, "error")) {
        encoding <- e
        break
      }
    }
    # Reverts to user's options.  Removed for CRAN
    #options(owarn)
  }
  if (is.na(encoding)) 
    stop("unable to find a plausible encoding")
  if (verbose) 
    cat(gettextf("encoding = \"%s\" chosen", encoding), 
        "\n", sep = "")
  filename <- file
  file <- file(filename, "r", encoding = encoding)
  on.exit(close(file))
  lines <- readLines(file, warn = FALSE)
  on.exit()
  close(file)
        
  loc <- utils::localeToCharset()[1L]
  encoding <- if (have_encoding) 
      switch(loc, `UTF-8` = "UTF-8", `ISO8859-1` = "latin1", 
                 "unknown")
    else "unknown"
    
  on.exit()
  if (chdir) {
    if (is.character(ofile)) {
      if (grepl("^(ftp|http|file)://", ofile)) 
        warning("'chdir = TRUE' makes no sense for a URL")
      else if ((path <- dirname(ofile)) != ".") {
        owd <- getwd()
        if (is.null(owd)) 
          stop("cannot 'chdir' as current directory is unknown")
        on.exit(setwd(owd), add = TRUE)
        setwd(path)
      }
    }
    else {
      warning("'chdir = TRUE' makes no sense for a connection")
    }
  }
  yy <- NULL

  #### End section copied from R's source function ####
  ignores <-  c("^library[(]RDataTracker[)]$", "^library[(]provR[)]$", 
                if (ignore.ddg.calls) "^ddg." else c("^prov.init", "^prov.run"))
  chunk_num <- 1
  in_chunk <- FALSE
  prov_active <- FALSE
  backticks <- "```"
  cur_chunk <- c()
  for(line in lines){
    if(grepl(backticks,line, fixed = TRUE)){
      if(in_chunk){
        exprs <- parse(file=stdin(), n = -1, text = cur_chunk, keep.source=TRUE, srcfile = srcfile, encoding = encoding,prompt="?")
        # parse(file=stdin(), n = -1, text=lines, prompt="?", 
        #       keep.source=TRUE, srcfile = srcfile, encoding = encoding)
        if(prov_active){
          #print(paste("Chunk", chunk_num))

          .ddg.add.start.node(node.name = paste("chunk", as.character(chunk_num), "(detailed)"))
          yy <- .ddg.parse.commands(exprs, sname, snum, environ = envir,
                              ignore.patterns = ignores, echo = getOption("verbose"), run.commands = TRUE, print.eval = print.eval,
                              max.deparse.length = 150, 
                              continue.echo = getOption("continue"), skip.echo = 0, prompt.echo = getOption("prompt"), verbose = getOption("verbose"), 
                              deparseCtrl = "showAttributes")
          .ddg.add.finish.node()
        }else{
          #print(paste("Chunk", chunk_num))
          
          # Create a procedure node for the chunk and connect it to the ddg
          node.name = paste("chunk", as.character(chunk_num))
          .ddg.proc.node("Operation", node.name, node.name, 
                         functions.called = list(NULL, NULL, NULL, NULL), scriptNum=snum)
          .ddg.proc2proc()
          
          # Parse the statements to find out what variables are used and set
          cmds <- .ddg.create.DDGStatements (exprs, sname, snum)

		  # The variables set in the chunk are the union of the variables set in the statements inside the chunk
		  # The variables used in the chunk are the variables that are used in the chunk but not set by a previous
		  # statement in the chunk
          vars.set.in.chunk <- vector()
          vars.used.in.chunk <- vector()
          for (cmd in cmds) {
          	  vars.used.in.cmd <- setdiff(cmd@vars.used, vars.set.in.chunk)
          	  vars.used.in.chunk <- union(vars.used.in.chunk, vars.used.in.cmd)
          	  vars.set.in.chunk <- union(vars.set.in.chunk, cmd@vars.set)
          }
          	  
          # Create the data use edges
          .ddg.create.data.use.edges(NULL, for.caller=FALSE, env=NULL, vars.used = vars.used.in.chunk, node.name = node.name)

          # This executes the code but does not modify the ddg
          yy <- .ddg.evaluate.commands(exprs,environ = envir)
          
          # Create the nodes and edges for the variables set.  No value is recorded with these nodes.
          .ddg.create.data.set.edges (vars.set.in.chunk, NULL, envir, captured.output = NULL, node.name, save.value=FALSE)
        }
        in_chunk = FALSE
        prov_active = FALSE
        cur_chunk <- c()
        chunk_num <- chunk_num + 1
      
      }else{
        in_chunk = TRUE
        header = substr(line,5,nchar(line)-1) 
        if(grepl("details = TRUE", header, fixed = TRUE) |
           grepl("details= TRUE", header, fixed = TRUE) |
           grepl("details =TRUE", header, fixed = TRUE) |
           grepl("details=TRUE", header, fixed = TRUE)) {#TODO: make better
          prov_active <- TRUE
        } else if (!grepl("details", header, fixed = TRUE)){
          prov_active <- .ddg.details()
        }
      }
    }else if(in_chunk){
      cur_chunk <- c(cur_chunk, line)
    }
  }
  invisible(yy)
}
#_______ SEAN ADD END _________

# To simplify future maintenance, here is the definition of R's source function
# in R 3.6.0.  We should compare this code with the source function in new
# released of R to determine what is changed so we can figure out what we
# need to update.
#
#function (file, local = FALSE, echo = verbose, print.eval = echo, 
#    exprs, spaced = use_file, verbose = getOption("verbose"), 
#    prompt.echo = getOption("prompt"), max.deparse.length = 150, 
#    width.cutoff = 60L, deparseCtrl = "showAttributes", chdir = FALSE, 
#    encoding = getOption("encoding"), continue.echo = getOption("continue"), 
#    skip.echo = 0, keep.source = getOption("keep.source")) 
#{
#  envir <- if (isTRUE(local)) 
#        parent.frame()
#      else if (isFALSE(local)) 
#        .GlobalEnv
#      else if (is.environment(local)) 
#        local
#      else stop("'local' must be TRUE, FALSE or an environment")
#  if (!missing(echo)) {
#    if (!is.logical(echo)) 
#      stop("'echo' must be logical")
#    if (!echo && verbose) {
#      warning("'verbose' is TRUE, 'echo' not; ... coercing 'echo <- TRUE'")
#      echo <- TRUE
#    }
#  }
#  if (verbose) {
#    cat("'envir' chosen:")
#    print(envir)
#  }
#  if (use_file <- missing(exprs)) {
#    ofile <- file
#    from_file <- FALSE
#    srcfile <- NULL
#    if (is.character(file)) {
#      have_encoding <- !missing(encoding) && encoding != 
#          "unknown"
#      if (identical(encoding, "unknown")) {
#        enc <- utils::localeToCharset()
#        encoding <- enc[length(enc)]
#      }
#      else enc <- encoding
#      if (length(enc) > 1L) {
#        encoding <- NA
#        owarn <- options(warn = 2)
#        for (e in enc) {
#          if (is.na(e)) 
#            next
#          zz <- file(file, encoding = e)
#          res <- tryCatch(readLines(zz, warn = FALSE), 
#              error = identity)
#          close(zz)
#          if (!inherits(res, "error")) {
#            encoding <- e
#            break
#          }
#        }
#        options(owarn)
#      }
#      if (is.na(encoding)) 
#        stop("unable to find a plausible encoding")
#      if (verbose) 
#        cat(gettextf("encoding = \"%s\" chosen", encoding), 
#            "\n", sep = "")
#      if (file == "") {
#        file <- stdin()
#        srcfile <- "<stdin>"
#      }
#      else {
#        filename <- file
#        file <- file(filename, "r", encoding = encoding)
#        on.exit(close(file))
#        if (isTRUE(keep.source)) {
#          lines <- readLines(file, warn = FALSE)
#          on.exit()
#          close(file)
#          srcfile <- srcfilecopy(filename, lines, file.mtime(filename)[1], 
#              isFile = TRUE)
#        }
#        else {
#          from_file <- TRUE
#          srcfile <- filename
#        }
#        loc <- utils::localeToCharset()[1L]
#        encoding <- if (have_encoding) 
#              switch(loc, `UTF-8` = "UTF-8", `ISO8859-1` = "latin1", 
#                  "unknown")
#            else "unknown"
#      }
#    }
#    else {
#      lines <- readLines(file, warn = FALSE)
#      srcfile <- if (isTRUE(keep.source)) 
#            srcfilecopy(deparse(substitute(file)), lines)
#          else deparse(substitute(file))
#    }
#    exprs <- if (!from_file) {
#          if (length(lines)) 
#            .Internal(parse(stdin(), n = -1, lines, "?", 
#                    srcfile, encoding))
#          else expression()
#        }
#        else .Internal(parse(file, n = -1, NULL, "?", srcfile, 
#                  encoding))
#    on.exit()
#    if (from_file) 
#      close(file)
#    if (verbose) 
#      cat("--> parsed", length(exprs), "expressions; now eval(.)ing them:\n")
#    if (chdir) {
#      if (is.character(ofile)) {
#        if (grepl("^(ftp|http|file)://", ofile)) 
#          warning("'chdir = TRUE' makes no sense for a URL")
#        else if ((path <- dirname(ofile)) != ".") {
#          owd <- getwd()
#          if (is.null(owd)) 
#            stop("cannot 'chdir' as current directory is unknown")
#          on.exit(setwd(owd), add = TRUE)
#          setwd(path)
#        }
#      }
#      else {
#        warning("'chdir = TRUE' makes no sense for a connection")
#      }
#    }
#  }
#  else {
#    if (!missing(file)) 
#      stop("specify either 'file' or 'exprs' but not both")
#    if (!is.expression(exprs)) 
#      exprs <- as.expression(exprs)
#  }
#  Ne <- length(exprs)
#  if (echo) {
#    sd <- "\""
#    nos <- "[^\"]*"
#    oddsd <- paste0("^", nos, sd, "(", nos, sd, nos, sd, 
#        ")*", nos, "$")
#    trySrcLines <- function(srcfile, showfrom, showto) {
#      tryCatch(suppressWarnings(getSrcLines(srcfile, showfrom, 
#                  showto)), error = function(e) character())
#    }
#  }
#  yy <- NULL
#  lastshown <- 0
#  srcrefs <- attr(exprs, "srcref")
#  if (verbose && !is.null(srcrefs)) {
#    cat("has srcrefs:\n")
#    utils::str(srcrefs)
#  }
#  for (i in seq_len(Ne + echo)) {
#    tail <- i > Ne
#    if (!tail) {
#      if (verbose) 
#        cat("\n>>>> eval(expression_nr.", i, ")\n\t\t =================\n")
#      ei <- exprs[i]
#    }
#    if (echo) {
#      nd <- 0
#      srcref <- if (tail) 
#            attr(exprs, "wholeSrcref")
#          else if (i <= length(srcrefs)) 
#            srcrefs[[i]]
#      if (!is.null(srcref)) {
#        if (i == 1) 
#          lastshown <- min(skip.echo, srcref[3L] - 1)
#        if (lastshown < srcref[3L]) {
#          srcfile <- attr(srcref, "srcfile")
#          dep <- trySrcLines(srcfile, lastshown + 1, 
#              srcref[3L])
#          if (length(dep)) {
#            leading <- if (tail) 
#                  length(dep)
#                else srcref[1L] - lastshown
#            lastshown <- srcref[3L]
#            while (length(dep) && grepl("^[[:blank:]]*$", 
#                dep[1L])) {
#              dep <- dep[-1L]
#              leading <- leading - 1L
#            }
#            dep <- paste0(rep.int(c(prompt.echo, continue.echo), 
#                    c(leading, length(dep) - leading)), dep, 
#                collapse = "\n")
#            nd <- nchar(dep, "c")
#          }
#          else srcref <- NULL
#        }
#      }
#      if (is.null(srcref)) {
#        if (!tail) {
#          dep <- substr(paste(deparse(ei, width.cutoff = width.cutoff, 
#                      control = deparseCtrl), collapse = "\n"), 
#              12L, 1000000L)
#          dep <- paste0(prompt.echo, gsub("\n", paste0("\n", 
#                      continue.echo), dep))
#          nd <- nchar(dep, "c") - 1L
#        }
#      }
#      if (nd) {
#        do.trunc <- nd > max.deparse.length
#        dep <- substr(dep, 1L, if (do.trunc) 
#                  max.deparse.length
#                else nd)
#        cat(if (spaced) 
#              "\n", dep, if (do.trunc) 
#              paste(if (grepl(sd, dep) && grepl(oddsd, dep)) 
#                        " ...\" ..."
#                      else " ....", "[TRUNCATED] "), "\n", sep = "")
#      }
#    }
#    if (!tail) {
#      yy <- withVisible(eval(ei, envir))
#      i.symbol <- mode(ei[[1L]]) == "name"
#      if (!i.symbol) {
#        curr.fun <- ei[[1L]][[1L]]
#        if (verbose) {
#          cat("curr.fun:")
#          utils::str(curr.fun)
#        }
#      }
#      if (verbose >= 2) {
#        cat(".... mode(ei[[1L]])=", mode(ei[[1L]]), "; paste(curr.fun)=")
#        utils::str(paste(curr.fun))
#      }
#      if (print.eval && yy$visible) {
#        if (isS4(yy$value)) 
#          methods::show(yy$value)
#        else print(yy$value)
#      }
#      if (verbose) 
#        cat(" .. after ", sQuote(deparse(ei, control = unique(c(deparseCtrl, 
#                            "useSource")))), "\n", sep = "")
#    }
#  }
#  invisible(yy)
#}

