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

############################ API_prov.R #############################

#' Provenance Collection Functions
#' 
#' prov.init intializes a new provenance graph. This function can be
#' executed in the console or placed inside an R script.
#' 
#' provR is an R package that collects provenance as an R script 
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
#' @param prov.dir the directory where the provenance graph will be 
#' saved. If not provided, the directory specified by the prov.dir 
#' option is used. Otherwise the R session temporary directory
#' is used.
#' @param overwrite if FALSE, includes a time stamp in the provenance
#' graph directory name.
#' @param snapshot.size the maximum size for snapshot files. If 0,
#' no snapshots are saved. If Inf, the complete state of an object is stored
#' in the snapshot file. For other values, the head of the object, truncated
#' to a size near the specified limit, is saved.  The size is in kilobytes. 
#' @param hash.algorithm the hash algorithm to use for files.
#' Choices are md5 (default), sha1, crc32, sha256, sha512, xxhash32, 
#' xxhash64 and murmur32. This feature uses the digest function from 
#' the digest package.
#' @param save.debug If TRUE, debug files are saved to the debug directory.
#' This is intended for developers of the RDataTracker / provR package.
#' @return prov.init initializes the provenance collector.  The prov.init
#' function does not return a value.
#' @export
#' @rdname prov.run
#' @seealso \code{\link{prov.json}} for access to the JSON text of the provenance, 

prov.init <- function(prov.dir = NULL, overwrite = TRUE, snapshot.size = 0, 
  hash.algorithm = "md5", save.debug = FALSE) {
  
  if (.ddg.is.set("ddg.initialized") && .ddg.get ("ddg.initialized") == TRUE) {
    stop ("Provenance collection is already started.  
          Call prov.quit() to stop the current collection before starting a new one.")
    return()
  }

# Save name of provenance collection tool
  .ddg.set("ddg.tool.name", "provR")

  # Save maximum snapshot size
  .ddg.set("ddg.snapshot.size", snapshot.size)
  
  # Save hash algorithm
  .ddg.set("ddg.hash.algorithm", hash.algorithm)
  
  # Initialize list of input & output file nodes
  .ddg.init.filenodes ()

  # Intialize provenance graph
  .ddg.init(prov.dir, overwrite, save.debug)
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
  .ddg.save (save.debug)
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
  .ddg.quit (save.debug)
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
#' @return prov.run runs a script, collecting provenance as it does so.  
#'   It does not return a value. 
#' @export
#' @rdname prov.run
#' @examples 
#' \dontrun{prov.run ("script.R")}
#' \dontrun{prov.source ("script.R")}
#' prov.init()
#' a <- 1
#' b <- 2
#' prov.save()
#' ab <- a + b
#' prov.quit()

prov.run <- function(r.script.path, prov.dir = NULL, overwrite = TRUE, 
  snapshot.size = 0, hash.algorithm = "md5", save.debug = FALSE) {
  
  # Stop & display message if R script path is missing
  if (missing(r.script.path)) {
    stop("Please provide the name of the R script to execute. If the script
      is not in the working directory, please include the full path.")
  }

  # Stop & display message if R script file is not found
  if (!file.exists(r.script.path)) {
    stop("R script file not found.")
  }

  # Store R script path
  .ddg.set("ddg.r.script.path", r.script.path)

  # Set script mode to True.
  .ddg.set("ddg.script.mode", TRUE)

  # Intialize the provenance graph
  prov.init(prov.dir, overwrite, snapshot.size, hash.algorithm, save.debug)
  
  # Execute the script
  .ddg.run(r.script.path)
}

#' prov.source
#'
#' prov.source loads an R script and executes it, collecting provenance
#' as it does so.  It assumes that provenance has already been initialized,
#' either via a call to prov.init, or because the R script was executed
#' using prov.run.  If you want to collect provenance inside scripts
#' that are loaded with R's source function, you should replace calls 
#' to source with calls to prov.source.
#' @param file the name of the R script file to source.
#' @return The prov.source function does not return a value.
#' @export
#' @rdname prov.run

prov.source <- function(file) {

  # Stop & display message if argument is missing or in console mode
  if (missing(file) || !.ddg.script.mode()) {
    stop("The prov.source function is for script annotation only.
      Please use prov.run to execute a script and collect provenance.")
  }

  .ddg.source(file)
}

#' Provenance Access Functions
#' 
#' prov.json returns the current provenance graph as a prov-json string.
#' 
#' provR collects provenance as a script executes.  Once collected,
#' prov.json can be called to access the provenance as a JSON string.  
#' This is useful for applications that operate on the provenance.  The
#' JSON is consistent with the PROV-JSON standard.
#' 
#' @return prov.json returns the current provenance graph as a prov-json
#' string
#' @export
#' @rdname prov.json
#' @seealso \code{\link{prov.init}} and \code{\link{prov.run}} for functions to collect provenance
#' @references PROV-JSON standard: \url{https://www.w3.org/Submission/2013/SUBM-prov-json-20130424/}
#' @references PROV-JSON output produced by provR: \url{https://github.com/End-to-end-provenance/RDataTracker/blob/export/JSON-format.md}
#' @references Applications that use the provenance:  \url{http://provtools.org/analyzes/}
#' @examples
#' prov.init()
#' a <- 1
#' b <- 2
#' ab <- a + b
#' prov.quit()
#' str <- prov.json()
#' pdir <- prov.dir()

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
