# This file contains the functions that manipulate the table that records hash function values
# for files read or written by the script.  These hash values enable us to detect when a file
# written by one script is read by another script, so that we can link script executions into
# a larger workflow.
#
# This code was originally written by Connor Gregorich-Trevor, summer REU student, 2017
# 
# Updated by Barbara Lerner to allow for a variety of hash algorithms and refactored
# to create the DDGHash.R file.

library(jsonlite)

#' Initialize the hashtable
#'
#' Initializes the hash table to be empty
#'
#' @return None
.ddg.init.hashtable <- function() {
  .ddg.set("ddg.hashtable", data.frame())
}

#' Adds information about a file to the hash table.
#' 
#' If the flag is set to save hashing information, this calculates the hash value
#' and whether the file is read or written and saves it in the hash table.  
#' 
#' @param dname the name of the file
#' @param ddg.dnum the number of the node in the data table
#' @param dscriptpath the absolute path to the R script
#' @param dloc the absolute path to the data file
#' @param dvalue the relative path to the saved copy of the file
#' @param dtime the timestamp on the file
#' 
#' @return None
.ddg.add.to.hashtable <- function(dname, ddg.dnum, dscriptpath, dloc, dvalue, dtime) {
  if (!.ddg.get (".ddg.save.hashtable")) {
    return
  }
  
  dhash <- .ddg.calculate.hash(dname)
  if (dhash == "") {
    print ("Empty hash value")
    return()
  }
  
  dhash.algorithm <- .ddg.get(".ddg.hash.algorithm")
  
  drw <- .ddg.calculate.rw(dname)
  
  ddg.data.nodes <- .ddg.data.nodes()
  ddg.data.nodes$ddg.hash[ddg.dnum] <- dhash
  ddg.data.nodes$ddg.rw[ddg.dnum] <- drw
  .ddg.set("ddg.data.nodes", ddg.data.nodes)
  
  
  longpath <- paste0(getwd(), substring(.ddg.path(),2),"/ddg.json")
  .ddg.set("ddg.hashtable", rbind(.ddg.get("ddg.hashtable"), c(dscriptpath, dloc, longpath, paste(.ddg.path(), dvalue, sep="/"), ddg.dnum, dhash, dhash.algorithm, drw, dtime, dvalue), stringsAsFactors = FALSE))
  print (.ddg.get("ddg.hashtable"))
  return (ddg.data.nodes)
}

#' Calculate the hash value for the file
#' 
#' @param dname the name of the file
#' 
#' @return the hash value based on the hash algorithm specified when ddg.run or ddg.init was called.
#'   Returns "" if the digest cannot be computed, for example, if the file does not exist.
.ddg.calculate.hash <- function(dname) {
  .ddg.set("ddg.hasfilenodes", TRUE)
  
  # This function will cause certain tests to fail if run with pdf files or
  # other non-text files with internal timestamps. This could also cause these files
  # to sync incorrectly in the workflow, but given that reading in a pdf file is unlikely,
  # this should not be an overly large issue.
  #dhash <- digest(dname, algo=.ddg.get(".ddg.hash.algorithm"), file = TRUE, errormode="silent")
  dhash <- digest(dname, algo=.ddg.get(".ddg.hash.algorithm"))
  if (is.null(dhash)) {
    dhash <- ""
  }
  return (dhash)
  
}

#' Determines whether to record this as a read or write operation in the hash table
#' 
#' @param dname the data file name
#' 
#' @return "read" if the file was read and "write" if the file was written
.ddg.calculate.rw <- function(dname) {
  infiles <- .ddg.get("ddg.infilenodes")
  if (dname %in% infiles) {
    .ddg.set("ddg.infilenodes", infiles[match(infiles, dname, 0) == 0])
    #print (paste("Removing", dname, "from infilenodes"))
    return ("read")
  }
  
  outfiles <- .ddg.get("ddg.outfilenodes")
  if (dname %in% outfiles) {
    .ddg.set("ddg.outfilenodes", outfiles[match(outfiles, dname, 0) == 0])
    #print (paste("Removing", dname, "from outfilenodes"))
    return ("write")
  }

  #print(paste(".ddg.calculate.rw:", dname, "not found in infilenodes or outfilenodes!"))
  return ("")
}


#' .ddg.hashtable.write writes relevant information about the ddg
#' to the .ddg directory in the user's home directory. If the
#' function is unable to access or create this directory, then 
#' it will write to the working directory.
#' 
#' @return None
.ddg.hashtable.write <- function() {
  # if (interactive()) print(paste("Saving DDG in ", fileout))
  
  # Determine where to put the hashtable file
  writedir <- paste0(path.expand("~"),"/.ddg/")
  if (!dir.exists(writedir)) {
    tryCatch({
          dir.create(writedir)
        },
        error = function(c){
          writedir <- paste0(getwd(),"/.ddg/")
          if (!dir.exists(writedir)) {
            dir.create(writedir)
          }
        })
  }
  
  # Add column names to the table
  hashtable.json <- paste0(writedir,"/hashtable.json")
  new_hashtable <- .ddg.get("ddg.hashtable")
  columns <- c("ScriptPath", "FilePath","DDGPath","NodePath","NodeNumber","Hash", "HashAlgo", "ReadWrite","Timestamp","Value")
  if (length(new_hashtable) == length(columns)) {
    colnames(new_hashtable) <- columns
  }
  
  # Combine with the existing file, if any
  if (file.exists(hashtable.json)) {
    old_hashtable <- .ddg.hashtable.cleanup(hashtable.json)
    new_hashtable <- rbind(old_hashtable, new_hashtable)
  }
  
  # Write out the updated file
  writejson <- toJSON(new_hashtable, simplifyVector = TRUE, pretty = TRUE)
  writeLines(writejson, hashtable.json)
}

#' .ddg.hashtable.cleanup removes entries in the hashtable that have been
#' overwritten by more recent iterations. It does this by removing entries
#' with matching DDGPaths.
#' 
#' @param hashtable.json the full path to the hashtable file already saved to the file system
#' 
#' @return the dataframe containing the contents of the existing hashtable file with 
#'    entries corresponding to overwritten files removed
.ddg.hashtable.cleanup <- function(hashtable.json) {
  old_hashtable <- read_json(hashtable.json, simplifyVector = TRUE)
  longpath <- paste0(getwd(), substring(.ddg.path(),2), "/ddg.json")
  old_hashtable <- subset(old_hashtable, DDGPath != longpath)
  return(old_hashtable)
}

#' Saves the hashtable to a file if the script has any file information to save and
#' the save hashtable flag was set when ddg.run/init was called.
#' 
#' @return None
.ddg.save.hashtable <- function() {
  if (.ddg.get (".ddg.save.hashtable") && .ddg.get("ddg.hasfilenodes")) {
    if (interactive()) {
      if (dir.exists(paste0(path.expand("~"),"/.ddg/"))) {
        print("Saving hashtable.json in .ddg directory.")
      } else {
        print("No .ddg directory found in home directory, saving hashtable.json in local directory.")
      }
    }
    .ddg.hashtable.write()
  }
}
