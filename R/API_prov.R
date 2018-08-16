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
#' prov.init intializes a new provenance graph. Called by the user
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
#' @param r.script.path the full path to the R script file
#' that is being executed. If provided, a copy of the script will
#' be saved with the provenance graph.
#' @param prov.dir the directory where the provenance graph will be 
#' saved. If not provided, the directory specified by the prov.dir 
#' option is used. Otherwise the R session temporary directory
#' is used.
#' @param overwrite if FALSE, includes a time stamp in the provenance
#'   graph directory name.
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
#' @rdname prov.run
#' @seealso \code{\link{prov.json}} for access to the JSON text of the provenance, 

prov.init <- function(r.script.path = NULL, prov.dir = NULL, overwrite = TRUE, 
    max.snapshot.size = 0, hash.algorithm="md5") {
  
  # Store name of provenance collection tool.
  .ddg.set ("ddg.tool.name", "provR")

  # Save hash algorithm
  .ddg.set (".ddg.hash.algorithm", hash.algorithm)

  # Store maximum snapshot size.
  .ddg.set("ddg.max.snapshot.size", max.snapshot.size)
  
  .ddg.init (r.script.path, prov.dir, overwrite)
}

#' prov.save
#' 
#' prov.save saves the current provenance graph to a prov-json file.
#' If more R statements are executed, the provenance for these statements
#' is added to the graph. The graph is finalized with prov.quit.
#' Called by the user in console mode.
#' @param save.debug If TRUE, debug files are saved to the debug directory.
#'   This is intended for developers of the RDataTracker package.
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
#' Called by the user in console mode.
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
#' the script executes.
#' @param f a function to run. If supplied, the function f is executed 
#' with calls to prov.init and prov.save so that provenance for the 
#' function is captured.  Exactly one of f and r.script.path should be provided.
#' @return prov.run runs a script, collecting provenance as it does so.  
#'   It does not return a value. 
#' @export
#' @rdname prov.run
#' @examples 
#' \dontrun{prov.run ("script.R")}
#' \dontrun{prov.source ("script.R")}
#' prov.init ()
#' a <- 1
#' b <- 2
#' prov.save ()
#' ab <- a + b
#' prov.quit ()

prov.run <- function(r.script.path = NULL, prov.dir = NULL, overwrite = TRUE, 
    f = NULL, max.snapshot.size = 0, save.debug = FALSE, hash.algorithm="md5") {
  
  prov.init(r.script.path, prov.dir, overwrite, max.snapshot.size, hash.algorithm)
  .ddg.run (r.script.path, f = f, save.debug = save.debug)
}

#' prov.source
#'
#' prov.source reads and executes an R script. To collect provenance
#' for scripts sourced by the main script, replace "source" in the
#' main script with "prov.source" before executing the main script
#' with prov.run.
#' @param file the name of the R script file to source.
#' @return The prov.source function does not return a value.
#' @export
#' @rdname prov.run

prov.source <- function(file) {
  .ddg.source(file)
}

#' Provenance Access Functions
#' 
#' prov.json returns the current provenance graph as a prov-json string.
#' 
#' ProvR collects provenance as a script executes.  Once collected,
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
#' @references PROV-JSON output produced by prov: \url{https://github.com/End-to-end-provenance/RDataTracker/blob/export/JSON-format.md}
#' @references Applications that use the provenance:  \url{http://provtools.org/analyzes/}
#' @examples
#' prov.init ()
#' a <- 1
#' b <- 2
#' ab <- a + b
#' prov.quit ()
#' str <- prov.json()

prov.json <- function()
{
  # This is a wrapper function.
  # Calls and returns the function with the bulk of the code in OutputJSON.R
  return( .ddg.json.string() )
}
