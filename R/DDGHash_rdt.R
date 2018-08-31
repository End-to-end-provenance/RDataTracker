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

########################### DDGHash.R ###########################

# This file contains the functions that create and manage the hash table.
# The hash values enable us to detect when a file written by one script 
# is read by another script, so that we can link script executions into 
# a larger workflow.


#' .ddg.init.hashtable initializes the hashtable to be empty
#' @return nothing
#' @noRd

.ddg.init.hashtable <- function() {
  .ddg.set("ddg.hashtable", data.frame())
}

#' .ddg.add.to.hashtable adds information about a file to the hash table.
#' If the flag is set to save hashing information, this calculates the hash value
#' and whether the file is read or written and saves it in the hash table.  
#' @param dname the name of the file
#' @param ddg.dnum the number of the node in the data table
#' @param dloc the absolute path to the data file
#' @param dvalue the relative path to the saved copy of the file
#' @param dtime the timestamp on the file
#' @return nothing
#' @noRd

# EF EDITS - remove drw parameter
.ddg.add.to.hashtable <- function(dname, ddg.dnum, dloc, dvalue, dtime, dhash) {
  if (dhash == "") {
    return()
  }
  
  dhash.algorithm <- .ddg.get(".ddg.hash.algorithm")
  
  dscriptpath <- 
      if (!is.null(.ddg.get("ddg.r.script.path"))) .ddg.get("ddg.r.script.path")
      else ""
  longpath <- paste0(getwd(), substring(.ddg.path(), 2), "/prov.json")
  
  # EF EDITS
  #.ddg.set("ddg.hashtable", 
  #         rbind(.ddg.get("ddg.hashtable"), 
  #               c(dscriptpath, dloc, longpath, 
  #                 paste(.ddg.path(), dvalue, sep="/"), 
  #                 ddg.dnum, dhash, dhash.algorithm, drw, dtime, dvalue), 
  #               stringsAsFactors = FALSE))
  .ddg.set("ddg.hashtable", 
           rbind(.ddg.get("ddg.hashtable"), 
                 c(dscriptpath, dloc, longpath, 
                   paste(.ddg.path(), dvalue, sep="/"), 
                   ddg.dnum, dhash, dhash.algorithm, dtime, dvalue), 
                 stringsAsFactors = FALSE))
}

#' .ddg.set.hash sets the hash and rw fields for a data node
#' and adds this information to the hash table. 
#' @param dname data node name.
#' @param dnum the id of the node to set
#' @param dloc path and name of original file.
#' @param dvalue data node value.
#' @param dtime timestamp of original file.
#' @return nothing 
#' @noRd

.ddg.set.hash <- function (dname, dnum, dloc, dvalue, dtime) {
  
  ddg.data.nodes <- .ddg.data.node.table()
  dhash <- .ddg.calculate.hash(dname)
  ddg.data.nodes$ddg.hash[dnum] <- dhash
  
  # EF EDITS
  #drw <- .ddg.calculate.rw(dname)
  #ddg.data.nodes$ddg.rw[dnum] <- drw
  
  .ddg.set("ddg.data.nodes", ddg.data.nodes)
  
  # EF EDITS - remove drw parameter
  .ddg.add.to.hashtable(dname = dname, ddg.dnum = dnum, dloc = dloc, 
      dvalue = dvalue, dtime = dtime, dhash)
}

#' .ddg.hashtable.write writes relevant information about the ddg
#' to the .ddg directory in the user's home directory. If the
#' function is unable to access or create this directory, then 
#' it will write to the working directory. 
#' @return nothing
#' @noRd

.ddg.hashtable.write <- function() {
  # if (interactive()) print(paste("Saving DDG in ", fileout))
  
  # Determine where to put the hashtable file
  writedir <- paste0(path.expand("~"), "/.ddg/")
  if (!dir.exists(writedir)) {
    tryCatch({
          dir.create(writedir)
        },
        error = function(c){
          writedir <- paste0(getwd(), "/.ddg/")
          if (!dir.exists(writedir)) {
            dir.create(writedir)
          }
        })
  }
  
  # Add column names to the table
  hashtable.json <- paste0(writedir, "/hashtable.json")
  new_hashtable <- .ddg.get("ddg.hashtable")
  columns <- c("ScriptPath", "FilePath", "DDGPath", "NodePath", "NodeNumber", 
               "Hash", "HashAlgo", "ReadWrite", "Timestamp", "Value")
  if (length(new_hashtable) == length(columns)) {
    colnames(new_hashtable) <- columns
  }
  
  # Combine with the existing file, if any
  if (file.exists(hashtable.json)) {
    old_hashtable <- .ddg.hashtable.cleanup(hashtable.json)
    new_hashtable <- rbind(old_hashtable, new_hashtable)
  }
  
  # Write out the updated file
  writejson <- jsonlite::toJSON(new_hashtable, simplifyVector = TRUE, pretty = TRUE)
  writeLines(writejson, hashtable.json)
}

#' .ddg.hashtable.cleanup removes entries in the hashtable that have been
#' overwritten by more recent iterations. It does this by removing entries
#' with matching DDGPaths. 
#' @param hashtable.json the full path to the hashtable file already saved to the file system
#' @return the dataframe containing the contents of the existing hashtable file with 
#' entries corresponding to overwritten files removed
#' @noRd

.ddg.hashtable.cleanup <- function(hashtable.json) {
  old_hashtable <- jsonlite::read_json(hashtable.json, simplifyVector = TRUE)
  longpath <- paste0(getwd(), substring(.ddg.path(), 2), "/prov.json")
  old_hashtable <- old_hashtable[old_hashtable$DDGPath != longpath, ]
  # old_hashtable <- subset(old_hashtable, DDGPath != longpath)
  return(old_hashtable)
}

#' .ddg.save.hashtable saves the hashtable to a file if the script has any file information 
#' to save and the save hashtable flag was set when prov.run/init was called.
#' @return nothing
#' @noRd

.ddg.save.hashtable <- function() {
  if (.ddg.get("ddg.hasfilenodes")) {
    if (interactive()) {
      if (dir.exists(paste0(path.expand("~"), "/.ddg/"))) {
        print("Saving hashtable.json in .ddg directory.")
      } else {
        print("No .ddg directory found in home directory,",
              "saving hashtable.json in local directory.")
      }
    }
    .ddg.hashtable.write()
  }
}
