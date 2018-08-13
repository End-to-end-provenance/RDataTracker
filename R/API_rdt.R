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

############################ API_rdt.R #############################

#' Provenance Collection Functions
#' 
#' rdt.init intializes a new provenance graph. Called by the user
#' in console mode.
#' 
#' RDataTracker is an R package that collects provenance as an R script 
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
#' The level of detail collected by RDataTracker may be set using parameters
#' of the prov.run and prov.init functions. Options include collecting
#' provenance inside functions and inside control constructs and saving
#' snapshots of large intermediate values as separate files. These
#' features are turned off by default to optimize performance. Common
#' settings for the level of detail can also be set and managed using the 
#' prov.set.detail and related functions.

#' @param r.script.path  the full path to the R script file
#' that is being executed. If provided, a copy of the script will
#' be saved with the provenance graph.
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
#' @param max.snapshot.size the maximum size for snapshot files. 
#' If 0, no snapshot files are saved.
#' If -1, the complete state of an object is stored in the snapshot
#' file. For other values, the head of the object, truncated to a size near
#' the specified limit, is saved.  The size is in kilobytes. 
#' @param hash.algorithm the hash algorithm to use for files.
#' Choices are md5 (default), sha1, crc32, sha256, sha512, xxhash32, 
#' xxhash64 and murmur32. This feature uses the digest function from 
#' the digest package.
#' @return prov.init initializes the provenance collector.  The prov.init
#'   function does not return a value.
#' @export
#' @rdname rdt.run
#' @seealso \code{\link{prov.json}} for access to the JSON text of the provenance, 
#'   \code{\link{prov.display}} to view the provenance graphically. 
#'   \code{\link{prov.set.detail}} to see an alternative way to set the amount of
#'     provenance collected.
#'   \code{\link{prov.annotate.on}} and \code{\link{prov.annotate.off}} to see how to control
#'     annotation of individual functions

rdt.init <- function(r.script.path = NULL, prov.dir = NULL, overwrite = TRUE, 
    annotate.inside.functions = FALSE, first.loop = 1, 
    max.loops = 0, max.snapshot.size = 0,
    hash.algorithm="md5") {
  
  
  #TODO: Would like to remove r.script.path parameter.  run should be 
  # used for scripts, and init for console.
  
  # Save hash algorithm
  .ddg.set (".ddg.hash.algorithm", hash.algorithm)
  .ddg.init.hashtable ()
  
  # Set environment constants.
  .ddg.set.details.omitted(FALSE)
  
  # If ddg.detail is not set, use values of annotate.inside, max.loops
  # and max.snapshot.size.
  if (is.null(prov.get.detail())) {
    # Store value of annotate.inside.
    .ddg.set("ddg.annotate.inside", annotate.inside.functions)
    
    # Store maximum number of loops to annotate.
    if (max.loops < 0) max.loops <- 10^10
    
    # Store maximum snapshot size.
    .ddg.set("ddg.max.snapshot.size", max.snapshot.size)
  }
  
  # Intialize loops
  .ddg.init.loops (first.loop, max.loops)
  
  # Set functions to be used for script annotation
  .ddg.set.annotation.functions()
  
  # Functions to be annotated.
  .ddg.set("ddg.annotate.on", NULL)
  
  # Functions not to be annotated.
  .ddg.set("ddg.annotate.off", NULL)
  
  .ddg.set(".ddg.func.depth", 0)
  .ddg.set(".ddg.explorer.port", 6096)
  
  # Initialize the stack of commands and environments being executed in active functions
  .ddg.set(".ddg.cur.cmd.stack", vector())
  .ddg.set(".ddg.cur.expr.stack", vector())
  
  .ddg.init (r.script.path, prov.dir, overwrite)
  
}

#' rdt.save saves the current provenance graph to a prov-json file.
#' If more R statements are executed, the provenance for these statements
#' is added to the graph. The graph is finalized with prov.quit.
#' Called by the user in console mode.
#' @param save.debug If TRUE, debug files are saved to the debug directory.
#'   This is intended for developers of the RDataTracker package.
#' @return prov.save writes the current provenance to a file but does not 
#'   return a value.
#' @export
#' @rdname rdt.run

rdt.save <- function(save.debug = FALSE) {
  .ddg.save (save.debug)
}

#' rdt.quit saves and closes the current provenance graph.
#' Called by the user in console mode.
#' @return prov.quit writes the current provenance to a file but does not 
#'   return a value.
#' @export
#' @rdname rdt.run

rdt.quit <- function(save.debug = FALSE) {
  .ddg.quit (save.debug)
}

#' rdt.run
#' 
#' rdt.run initiates execution of a script and collects provenance as 
#' the script executes.
#'
#' @param f a function to run. If supplied, the function f is executed 
#' with calls to prov.init and prov.save so that provenance for the 
#' function is captured.  Exactly one of f and r.script.path should be provided.
#' @param display if TRUE, the provenance graph is displayed in DDG Explorer
#'
#' @return prov.run runs a script, collecting provenance as it does so.  
#'   It does not return a value. 
#' @export
#' @rdname rdt.run
#' @examples 
#' \dontrun{rdt.run ("script.R")}
#' rdt.init ()
#' a <- 1
#' b <- 2
#' rdt.save ()
#' ab <- a + b
#' rdt.quit ()

rdt.run <- function(r.script.path = NULL, prov.dir = NULL, overwrite = TRUE, 
    f = NULL, annotate.inside.functions = FALSE, first.loop = 1, 
    max.loops = 0, max.snapshot.size = 0, save.debug = FALSE, 
    display = FALSE, hash.algorithm="md5") {
  
  # Initialize ddg.
  rdt.init(r.script.path, prov.dir, overwrite, annotate.inside.functions, 
      first.loop, max.loops, max.snapshot.size, hash.algorithm)
  
  .ddg.run (r.script.path, save.debug, f)
  
  if (display==TRUE){
    rdt.display()
  }
  
}

#' Provenance Access Functions
#' 
#' rdt.json returns the current provenance graph as a prov-json string.
#' 
#' RDataTracker collects provenance as a script executes.  Once collected,
#' prov.json can be called to access the provenance as a JSON string.  
#' This is useful for applications that operate on the provenance.  The
#' JSON is consistent with the PROV-JSON standard.
#' 
#' One such application is a graphic visualizer built into RDataTracker.
#' To view the provenance graphically, call prov.display.  In the provenance
#' graph, the nodes
#' are data values and operations, with edges connecting them to show 
#' data and control flow dependencies.  The visualizer also
#' allows the user to view intermediate
#' values of variables, and to graphically view the lineage of how a value
#' was computed, or to look at how a value is used moving forward in the computation.
#' The user can also search for specific
#' data or operation nodes, files, or error messages in the provenance.
#' 
#' @return prov.json returns the current provenance graph as a prov-json
#' string
#' @export
#' @rdname rdt.json
#' @seealso \code{\link{prov.init}} and \code{\link{prov.run}} for functions to collect provenance
#' @references PROV-JSON standard: \url{https://www.w3.org/Submission/2013/SUBM-prov-json-20130424/}
#' @references RDataTracker PROV-JSON output: \url{https://github.com/End-to-end-provenance/RDataTracker/blob/export/JSON-format.md}
#' @examples
#' rdt.init ()
#' a <- 1
#' b <- 2
#' ab <- a + b
#' rdt.quit ()
#' str <- rdt.json()

rdt.json <- function()
{
  # This is a wrapper function.
  # Calls and returns the function with the bulk of the code in OutputJSON.R
  return( .ddg.json.string() )
}

#' .ddg.start.ddg.explorer starts DDG Explorer
#' @return nothing

.ddg.start.ddg.explorer <- function () {
  jar.path<- "/RDataTracker/java/DDGExplorer.jar"
  check.library.paths<- file.exists(paste(.libPaths(), jar.path, sep = ""))
  index<- min(which(check.library.paths == TRUE))
  ddgexplorer_path<- paste(.libPaths()[index], jar.path, sep = "")
  ddgjson.path<- paste(.ddg.path(), "prov.json", sep = "/")
  # ddgjson.path<- paste(getwd(), .ddg.path() ,"ddg.json",sep = "/")
  
  # -s flag starts DDG Explorer as a server.  This allows each new ddg to show
  # up in a new tab of an existing running DDG Explorer.
  # print("Starting DDG Explorer server")
  system2("java", 
      c("-jar", ddgexplorer_path, ddgjson.path, "-port", 
          .ddg.get(".ddg.explorer.port")), 
      wait = FALSE)
}

#' rdt.display
#' 
#' rdt.display displays the current provenance as a graph.
#' @return prov.display loads and displays the current provenance graph
#' in DDG Explorer. The prov.display function does not return a value.
#' @export 
#' @rdname rdt.json
#' @examples
#' \dontrun{rdt.display()} 

rdt.display <- function () {
  
  # See if the server is already running
  # print("Opening socket connection")
  tryCatch ({
        con <- socketConnection(host= "localhost", 
            port = .ddg.get(".ddg.explorer.port"), 
            blocking = FALSE,
            server=FALSE, open="w", timeout=1)
        ddgjson.path<- paste(.ddg.path(), "prov.json", sep = "/")
        # ddgjson.path<- paste(getwd(), .ddg.path() ,"ddg.json",sep = "/")
        # print ("Socket open; writing to socket")
        writeLines(ddgjson.path, con)
        # print ("Wrote to socket")
        close(con)
      },
      warning = function(e) {
        # print("Warning!")
        .ddg.start.ddg.explorer()
      }
  )
  
  invisible()
}

#' .ddg.set.annotation.functions sets the names of the functions used for script
#' annotation in the user's environment.  This is a workaround to exporting
#' these functions and would not be allowed by CRAN.
#' @return nothing

.ddg.set.annotation.functions <- function () {
  assign(".ddg.details.omitted", RDataTracker:::.ddg.details.omitted, 
      envir = globalenv())
  assign(".ddg.eval", RDataTracker:::.ddg.eval, 
      envir = globalenv())
  assign(".ddg.finish", RDataTracker:::.ddg.finish, 
      envir = globalenv())
  assign(".ddg.first.loop", RDataTracker:::.ddg.first.loop, 
      envir = globalenv())
  assign(".ddg.forloop", RDataTracker:::.ddg.forloop, 
      envir = globalenv())
  assign(".ddg.function", RDataTracker:::.ddg.function, 
      envir = globalenv())
  assign(".ddg.loop.annotate.off", RDataTracker:::.ddg.loop.annotate.off, 
      envir = globalenv())
  assign(".ddg.loop.annotate.on", RDataTracker:::.ddg.loop.annotate.on, 
      envir = globalenv())
  assign(".ddg.loop.count", RDataTracker:::.ddg.loop.count, 
      envir = globalenv())
  assign(".ddg.loop.count.inc", RDataTracker:::.ddg.loop.count.inc, 
      envir = globalenv())
  assign(".ddg.max.loops", RDataTracker:::.ddg.max.loops, 
      envir = globalenv())
  assign(".ddg.max.snapshot.size", RDataTracker:::.ddg.max.snapshot.size, 
      envir = globalenv())
  assign(".ddg.not.inside.loop", RDataTracker:::.ddg.not.inside.loop, 
      envir = globalenv())
  assign(".ddg.reset.loop.count", RDataTracker:::.ddg.reset.loop.count, 
      envir = globalenv())
  assign(".ddg.return.value", RDataTracker:::.ddg.return.value, 
      envir = globalenv())
  assign(".ddg.set.inside.loop", RDataTracker:::.ddg.set.inside.loop, 
      envir = globalenv())
  assign(".ddg.source", RDataTracker:::.ddg.source, 
      envir = globalenv())
  assign(".ddg.start", RDataTracker:::.ddg.start, 
      envir = globalenv())
  assign(".ddg.should.run.annotated", RDataTracker:::.ddg.should.run.annotated, 
      envir = globalenv())
  invisible()
}

