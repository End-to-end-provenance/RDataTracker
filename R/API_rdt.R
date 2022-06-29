# Copyright (C) President and Fellows of Harvard College and 
# Trustees of Mount Holyoke College, 2014, 2015, 2016, 2017, 2018,
# 2019, 2020, 2021, 2022.

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

############################ API_rdt.R #############################

#' Provenance Collection Functions
#' 
#' prov.init intializes a new provenance graph. This function can be
#' executed in the console or placed inside an R script.
#' 
#' rdt is an R package that collects provenance as an R script 
#' executes. The resulting provenance provides a detailed record of the 
#' execution of the script and includes information on the steps that were 
#' performed and the intermediate data values that were created. The 
#' resulting provenance can be used for a wide variety of applications
#' that include debugging scripts, cleaning code, and reproducing results.
#' 
#' There are two ways in which a user can collect provenance.  To collect
#' provenance from commands stored in a script file, use prov.run.  This
#' will execute the commands that are in the script, collecting provenance
#' as it does so.
#' 
#' The user can also collect provenance while executing commands in the 
#' console.  To do this, first execute prov.init.  Then enter console
#' commands as normal.  When done with the commands for which you want
#' provenance, use prov.quit.  If you want to save the current provenance
#' without turning off provenance collection, call prov.save instead of
#' prov.quit.  You can call prov.save multiple times before calling prov.quit.
#' Each call will append to the same provenance file.
#'
#' The provenance is stored in PROV-JSON format. For immediate use it may
#' be retrieved from memory using the prov.json function. For later use
#' the provenance is also written to the file prov.json. This file and
#' associated files are written by default to the R session temporary
#' directory. The user can change this location by (1) using the optional
#' parameter prov.dir in the prov.run or prov.init functions, or (2) setting
#' the prov.dir option (e.g. by using the R options command or editing the
#' Rprofile.site or .Rprofile file). If prov.dir is set to ".", the current working
#' directory is used.
#' 
#' The level of detail collected by rdt may be set using parameters
#' of the prov.run and prov.init functions. Options include collecting
#' provenance inside functions and inside control constructs and saving
#' snapshots of large intermediate values as separate files. These
#' features are turned off by default to optimize performance. Common
#' settings for the level of detail can also be set and managed using the 
#' prov.set.detail and related functions.

#' @param prov.dir the directory where the provenance graph will be 
#' saved. If not provided, the directory specified by the prov.dir 
#' option is used. Otherwise the R session temporary directory
#' is used.
#' @param overwrite if FALSE, includes a time stamp in the provenance
#'   graph directory name.
#' @param annotate.inside.functions if TRUE, provenance is collected 
#' inside functions.
#' @param first.loop the first loop to collect provenance in a for, 
#' while, or repeat statement.
#' @param max.loops the maximum number of loops to collect
#' provenance in a for, while, or repeat statement. If max.loops = -1,
#' there is no limit. If max.loops = 0, no loops are annotated. 
#' If non-zero, it indicates the number of iterations of each loop for
#' which provenance should be collected.  If max.loops is non-zero, provenance
#' is also collected inside if-statements.
#' @param snapshot.size the maximum size for snapshot files. If 0,
#' no snapshots are saved. If Inf, the complete state of an object is stored
#' in the snapshot file. For other values, the head of the object, truncated
#' to a size near the specified limit, is saved.  The size is in kilobytes. 
#' @param hash.algorithm the hash algorithm to use for files.
#' Choices are md5 (default), sha1, crc32, sha256, sha512, xxhash32, 
#' xxhash64 and murmur32. This feature uses the digest function from 
#' the digest package.
#' @param save.debug If TRUE, debug files are saved to the debug directory.
#' This is intended for developers of the rdt / rdtLite package.
#' @return prov.init initializes the provenance collector.  The prov.init
#' function does not return a value.
#' @export
#' @rdname prov.run
#' @seealso \code{\link{prov.json}} for access to the JSON text of the provenance, 
#'   \code{\link{prov.visualize}} to view the provenance graphically. 
#'   \code{\link{prov.set.detail}} to see an alternative way to set the amount of
#'     provenance collected.
#'   \code{\link{prov.annotate.on}} and \code{\link{prov.annotate.off}} to see how to control
#'     annotation of individual functions

prov.init <- function(prov.dir = NULL, overwrite = TRUE, annotate.inside.functions = FALSE, 
  first.loop = 1, max.loops = 0, snapshot.size = 0, hash.algorithm = "md5", save.debug = FALSE) {

  if (.ddg.is.set("ddg.initialized") && .ddg.get ("ddg.initialized") == TRUE) {
    stop ("Provenance collection is already started.  
			Call prov.quit() to stop the current collection before starting a new one.")
    return()
  }
  
  # Save name of provenance collection tool.
  .ddg.set("ddg.tool.name", "rdt")

  # Save hash algorithm
  .ddg.set("ddg.hash.algorithm", hash.algorithm)
  
  # If this function was not called from prov.run, save arguments
  if(!.ddg.is.set("ddg.run.args")) {
    args.names <- c("overwrite", "annotate.inside.functions", "first.loop", 
                    "max.loops", "snapshot.size", "save.debug")
    args.types <- c("logical", "logical", "integer", "integer", "numeric", "logical")
  
    args.values <- list(overwrite, annotate.inside.functions, first.loop, 
                        max.loops, snapshot.size, save.debug)
    args.values <- as.character(args.values)
    
    ddg.run.args <- data.frame(args.names, args.values, args.types, stringsAsFactors = FALSE)
    .ddg.set("ddg.run.args", ddg.run.args)
  }
  
  # Initialize list of input & output file nodes
  .ddg.init.filenodes()
  
  # Initialize hash table
  .ddg.init.hashtable()
  
  # Set environment constants.
  .ddg.set.details.omitted(FALSE)
  
  # If ddg.detail is not set, use values of annotate.inside, max.loops
  # and snapshot.size.
  if (is.null(prov.get.detail())) {
    # Store value of annotate.inside.
    .ddg.set("ddg.annotate.inside", annotate.inside.functions)
    
    # Store maximum number of loops to annotate.
    if (max.loops < 0) max.loops <- 10^10
    
    # Store maximum snapshot size.
    .ddg.set("ddg.snapshot.size", snapshot.size)
  }
  
  # Intialize loops
  .ddg.init.loops (first.loop, max.loops)
  
  # Set functions to be used for script annotation
  .ddg.set.annotation.functions()
  
  # Functions to be annotated
  .ddg.set("ddg.annotate.on", NULL)
  
  # Functions not to be annotated
  .ddg.set("ddg.annotate.off", NULL)
  
  # Intialize return values
  .ddg.init.return.values()
  
  # Set DGD Explorer port
  .ddg.set("ddg.explorer.port", 6096)
  
  # Initialize the stack of commands and environments being executed in active functions
  .ddg.set("ddg.cur.expr.stack", vector())
  
  .ddg.init (prov.dir, overwrite, save.debug)
}

#' prov.save
#'
#' prov.save saves the current provenance graph to a prov-json file.
#' If more R statements are executed, the provenance for these statements
#' is added to the graph. The graph is finalized with prov.quit.
#' This function can be
#' executed in the console or placed inside an R script.
#' @return prov.save writes the current provenance to a file but does not 
#'   return a value.
#' @export
#' @rdname prov.run

prov.save <- function(save.debug = FALSE) {
  
  # update the value of save.debug in ddg.run.args
  if(.ddg.is.init()) {
    args <- .ddg.get("ddg.run.args")
    args$args.values[args$args.names %in% "save.debug"] <- save.debug
    .ddg.set("ddg.run.args", args)
  }
  
  .ddg.save(save.debug)
}

#' prov.quit
#'
#' prov.quit saves and closes the current provenance graph.
#' This function can be
#' executed in the console or placed inside an R script.
#' @return prov.quit writes the current provenance to a file but does not 
#'   return a value.
#' @export
#' @rdname prov.run

prov.quit <- function(save.debug = FALSE) {
  
  # update the value of save.debug in ddg.run.args
  if(.ddg.is.init()) {
    args <- .ddg.get("ddg.run.args")
    args$args.values[args$args.names %in% "save.debug"] <- save.debug
    .ddg.set("ddg.run.args", args)
  }
  
  .ddg.quit(save.debug)
}

#' prov.run
#'
#' prov.run initiates execution of a script and collects provenance as 
#' the script executes.  This function should be used if you want to 
#' collect provenance for a script that is in an R file and you do not
#' want to modify the R script directly to include calls to prov.init, 
#' prov.save and prov.quit.  It essentially wraps the execution of the 
#' script with calls to prov.init and prov.quit.  
#' @param r.script.path the full path to the R script file that is being 
#' executed. A copy of the script will be saved with the provenance graph.
#' @param details if FALSE, provenance is not collected for top-level
#' statements.
#' @param display if TRUE, the provenance graph is displayed in DDG Explorer
#' @param exprs Instead of specifying file, an expression, call, or list of call's, 
#'   can be passed in to be executed.
#' @param ... parameters passed on to the source function.  See documentation
#'   of source for details.
#' @return prov.run runs a script, collecting provenance as it does so.  
#'   It does not return a value. 
#' @export
#' @rdname prov.run
#' @examples 
#' \dontrun{prov.run ("script.R")}
#' prov.init()
#' a <- 1
#' b <- 2
#' prov.save()
#' ab <- a + b
#' prov.quit()

prov.run <- function(r.script.path, prov.dir = NULL, overwrite = TRUE, details = TRUE, 
  annotate.inside.functions = FALSE, first.loop = 1, max.loops = 0, snapshot.size = 0, 
  hash.algorithm = "md5", save.debug = FALSE, display = FALSE, exprs, ...) {

  # Stop & display message if R script path is missing
  if (missing(r.script.path) && missing(exprs)) {
    stop("Please provide the name of the R script or a list of expressions to execute. If the script
            is not in the working directory, please include the full path.")
  }

  if (!missing(r.script.path) && !missing(exprs)) {
    stop("Please provide either the name of the R script or a list of expressions to execute, but not both.")
  }
  
  # If expressions were passed in rather than a script file,
  # save the expressions in a file.
  if (missing(r.script.path)) {
    r.script.path <- paste0(tempdir(), "/exprs.R")
    source <- sapply (exprs, deparse)
    writeLines(source, r.script.path)
    use_file <- FALSE
  }
  else {
    use_file <- TRUE
  }
  
  # Stop & display message if R script file is not found
  if (!file.exists(r.script.path)) {
    stop("R script file not found.")
  }

  # Store arguments
  args.names <- c("overwrite", "details", "annotate.inside.functions",
                  "first.loop", "max.loops", "snapshot.size", "save.debug")
  args.types <- c("logical", "logical", "logical", 
                  "integer", "integer", "numeric", "logical")
  
  args.values <- list(overwrite, details, annotate.inside.functions,
                      first.loop, max.loops, snapshot.size, save.debug)
  args.values <- as.character(args.values)
  
  ddg.run.args <- data.frame(args.names, args.values, args.types, stringsAsFactors = FALSE)
  .ddg.set("ddg.run.args", ddg.run.args)
  
  # Store R script path
  .ddg.set("ddg.r.script.path", r.script.path)

  # Store details value
  .ddg.set("ddg.details", details)

  # Set script mode to True
  .ddg.set("ddg.script.mode", TRUE)

  # Initialize the provenance graph
  prov.init(prov.dir, overwrite, annotate.inside.functions, first.loop, max.loops, 
    snapshot.size, hash.algorithm, save.debug)
  
  # Execute the script
  .ddg.run(r.script.path, exprs, ...)

  # Display the graph in DDG Explorer
  if (display == TRUE) prov.visualize()
}

#' prov.source
#'
#' prov.source loads an R script and executes it, collecting provenance
#' as it does so.  It assumes that provenance has already been initialized,
#' either via a call to prov.init, or because the R script was executed
#' using prov.run.  If you want to collect provenance inside scripts
#' that are loaded with R's source function, you should replace calls 
#' to source with calls to prov.source.
#' 
#' If prov.source is called when provenance is not initialized, it
#' will just source the file.  No provenance will be collected.
#' 
#' @param file the name of the R script file to source.
#' @return The prov.source function does not return a value.
#' @export
#' @rdname prov.run

prov.source <- function(file, exprs, ...) {
  
  # Stop & display message if argument is missing or in console mode
  if (missing(file) && missing (exprs)) {
    stop("Please provide the name of an R script file or a list of parsed expressions in the call to prov.source.")
  }
    
  if (!missing(file) && !missing (exprs)) {
    stop("Please provide the name of an R script file or a list of parsed expressions, but not both")
  }
  
  if (.ddg.is.init()) {
    .ddg.source(file, exprs = exprs, ...)
  }
  else {
    source (file, exprs = exprs, ...)
  }
  
}

#' Provenance Access Functions
#' 
#' prov.json returns the current provenance graph as a prov-json string.
#' 
#' rdt collects provenance as a script executes.  Once collected,
#' prov.json can be called to access the provenance as a JSON string.  
#' This is useful for applications that operate on the provenance.  The
#' JSON is consistent with the PROV-JSON standard.
#' 
#' One such application is a graphic visualizer built into rdt.
#' To view the provenance graphically, call prov.visualize.  In the provenance
#' graph, the nodes are data values and operations, with edges connecting 
#' them to show data and control flow dependencies.  The visualizer also
#' allows the user to view intermediate values of variables, and to graphically
#' view the lineage of how a value was computed, or to look at how a value 
#' is used moving forward in the computation. The user can also search for specific
#' data or operation nodes, files, or error messages in the provenance.
#' 
#' @return prov.json returns the current provenance graph as a prov-json
#' string
#' @export
#' @rdname prov.json
#' @seealso \code{\link{prov.init}} and \code{\link{prov.run}} for functions to collect provenance
#' @references PROV-JSON standard: \url{https://www.w3.org/Submission/2013/SUBM-prov-json-20130424/}
#' @references PROV-JSON output produced by rdt: \url{https://github.com/End-to-end-provenance/ExtendedProvJson/blob/master/JSON-format.md}
#' @examples
#' prov.init()
#' a <- 1
#' b <- 2
#' ab <- a + b
#' prov.quit()
#' str <- prov.json()
#' pdir <- prov.dir()
#' \dontrun{prov.visualize()} 

prov.json <- function() {
  # This is a wrapper function.
  # Calls and returns the function with the bulk of the code in OutputJSON.R
  return(.ddg.json.string())
}

#' prov.dir
#' 
#' prov.dir returns the current provenance directory.
#' @return prov.dir returns the current provenance directory.
#' @export
#' @rdname prov.json

prov.dir <- function() {

  # Display a message if the provenance directory has not been not set
  if (is.null(.ddg.path())) {
    cat("The provenance directory has not been set. It will be 
      set when prov.init or prov.run is called.\n")
  }

  return(.ddg.path())
}

#' prov.visualize
#'
#' prov.visualize displays the current provenance as a graph.
#' @return prov.visualize loads and displays the current provenance graph
#' in DDG Explorer. The prov.visualize function does not return a value.
#' @export 
#' @rdname prov.json

prov.visualize <- function () {
  provViz::prov.visualize()
  invisible()
}

#' prov.summarize
#'
#' prov.summarize outputs a text summary to the R console
#' 
#' Creating a zip file depends on a zip executable being on the search path.
#' By default, it looks for a program named zip.  To use a program with 
#' a different name, set the value of the R_ZIPCMD environment variable.  This
#' code has been tested with Unix zip and with 7-zip on Windows.  
#' 
#' @param save if true saves the summary to the file prov-summary.txt in the 
#' provenance directory
#' @param create.zip if true all of the provenance data will be packaged up
#'   into a zip file stored in the current working directory.
#' 
#' @export
#' @rdname prov.json

prov.summarize <- function (save=FALSE, create.zip=FALSE, details=FALSE, check=TRUE, console=TRUE, notes=TRUE) {
  if (requireNamespace ("provSummarizeR", quietly=TRUE)) {
  	provSummarizeR::prov.summarize(save=save, create.zip=create.zip, details=details, check=check, console=console, notes=notes)
  }
  else {
  	cat ("You need to install the provSummarizeR package to use this function.")
  }
  invisible()
}


#' .ddg.set.annotation.functions sets the names of the functions used for script
#' annotation in the user's environment.  This is a workaround to exporting
#' these functions and would not be allowed by CRAN.
#' @return nothing
#' @noRd

.ddg.set.annotation.functions <- function () {
  assign(".ddg.details.omitted", rdt:::.ddg.details.omitted, 
      envir = globalenv())
  assign(".ddg.eval", rdt:::.ddg.eval, 
      envir = globalenv())
  assign(".ddg.finish", rdt:::.ddg.finish, 
      envir = globalenv())
  assign(".ddg.first.loop", rdt:::.ddg.first.loop, 
      envir = globalenv())
  assign(".ddg.forloop", rdt:::.ddg.forloop, 
      envir = globalenv())
  assign(".ddg.function", rdt:::.ddg.function, 
      envir = globalenv())
  assign(".ddg.loop.annotate.off", rdt:::.ddg.loop.annotate.off, 
      envir = globalenv())
  assign(".ddg.loop.annotate.on", rdt:::.ddg.loop.annotate.on, 
      envir = globalenv())
  assign(".ddg.loop.count", rdt:::.ddg.loop.count, 
      envir = globalenv())
  assign(".ddg.loop.count.inc", rdt:::.ddg.loop.count.inc, 
      envir = globalenv())
  assign(".ddg.max.loops", rdt:::.ddg.max.loops, 
      envir = globalenv())
  assign(".ddg.snapshot.size", rdt:::.ddg.snapshot.size, 
      envir = globalenv())
  assign(".ddg.not.inside.loop", rdt:::.ddg.not.inside.loop, 
      envir = globalenv())
  assign(".ddg.reset.loop.count", rdt:::.ddg.reset.loop.count, 
      envir = globalenv())
  assign(".ddg.return.value", rdt:::.ddg.return.value, 
      envir = globalenv())
  assign(".ddg.set.inside.loop", rdt:::.ddg.set.inside.loop, 
      envir = globalenv())
  assign(".ddg.source", rdt:::.ddg.source, 
      envir = globalenv())
  assign(".ddg.start", rdt:::.ddg.start, 
      envir = globalenv())
  assign(".ddg.should.run.annotated", rdt:::.ddg.should.run.annotated, 
      envir = globalenv())
  invisible()
}
