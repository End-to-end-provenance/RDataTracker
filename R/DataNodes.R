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

################  Functions involved in managing the data nodes ################
#
# Data nodes are stored in a data frame, with one row for each node.
# The columns of the data frame are:
#
# type - the type of node, one of Data, File, Exception, URL, Snapshot
# num - a unique number
# name - the label for the node
# value - often this is the same as the name
# val.type - a description of the type of the data value
# scope - the scope that the data value is defined in
# from.env - if the value was set globally before the script ran
# time - a timestamp
# hash - hash value for files
# rw - either "read" or "write", only used for files
# loc - absolute path to the original file
#
# ddg.pnum is the number associated with the last procedure node created
#
# This file also contains functions that create data nodes of various types.

#' Initializes the information needed to manage data nodes.
#' 
#' @return nothing
.ddg.init.data.nodes <- function () {
  .ddg.set("ddg.data.nodes", .ddg.create.data.node.rows()) 
  .ddg.set("ddg.dnum", 0)
}

#' .ddg.is.data.type returns TRUE for any type of data node.
#' This is used for type-checking.
#' 
#' @param type data node type.
#' @returnType logical
#' @return true for any type of data node
.ddg.is.data.type <- function(type) {
  return(type %in% c("Data", "Snapshot", "File", "URL", "Exception"))
}

#' Return the counter used to assign data node ids
#' @returnType integer 
#' @return the data node id of the last data node created
.ddg.dnum <- function() {
  return (.ddg.get("ddg.dnum"))
}

#' Create a data frame of empty rows to put in the data node table.
#' It is faster to add a bunch of empty rows and update them than to 
#' add one row at a time
#' @param size the number of rows to add
#' @returnType a dataframe 
#' @return a data frame with size rows, with all columns being empty vectors
.ddg.create.data.node.rows <- function (size=100) {
  return (      
      data.frame(
          ddg.type = character(size),
          ddg.num = numeric(size),
          ddg.name = character(size),
          ddg.value = character(size),
          ddg.val.type = character(size),
          ddg.scope = character(size),
          ddg.from.env = logical(size),
          ddg.time = character(size),
          ddg.hash = character(size),
          ddg.rw = character(size),
          ddg.loc = character(size),
          stringsAsFactors=FALSE))
}
  

#' Returns the data node table
#' @returnType a data frame
#' @return the data node table
.ddg.data.node.table <- function() {
  return (.ddg.get("ddg.data.nodes"))
}

#' Returns the filled rows of the data node table
#' @returnType a data frame
#' @return the filled rows of the data node table
.ddg.data.nodes <- function() {
  ddg.data.nodes <- .ddg.get("ddg.data.nodes")
  return (ddg.data.nodes [ddg.data.nodes$ddg.num > 0, ])
}


#' Sets the hash and rw fields for a data node
#' 
#' @param dnum the id of the node to set
#' @param hash the hash value to use
#' @param rw the rw value to use
#' @return nothing 
.ddg.set.hash <- function (dnum, hash, rw) {
  ddg.data.nodes <- .ddg.data.node.table()
  ddg.data.nodes$ddg.hash[dnum] <- hash
  ddg.data.nodes$ddg.rw[dnum] <- rw
  .ddg.set("ddg.data.nodes", ddg.data.nodes)
}


#' .ddg.data.node.exists searches the data node table for a matching
#' data node and returns TRUE if a match is found. If no match is found, it 
#' checks if a variable with that name exists in the global environment.
#' If a match is found, it creates a data node for the global and returns TRUE. 
#' If no node exists and it is not a global, it returns FALSE.
#' 
#' @param dname data node name
#' @param dscope data node scope.  If NULL, uses the closest scope in which
#'   dname is defined
#' @returnType logical
#' @return true if a node with the given name exists
.ddg.data.node.exists <- function(dname, dscope=NULL) {
  if (is.null(dscope)) dscope <- .ddg.get.scope(dname)
  
  # Search data nodes table.
  #print (paste (".ddg.data.node.exists: Looking for", dname, "in scope", dscope))
  ddg.data.nodes <- .ddg.data.node.table()
  
  matching <- ddg.data.nodes [ddg.data.nodes$ddg.name == dname & 
          (ddg.data.nodes$ddg.scope == "ddg.library" | ddg.data.nodes$ddg.scope == dscope), ]
  if (nrow (matching) > 0) {
    return(TRUE)
  }
  
  # Search initial environment table.
  if (dscope == "R_GlobalEnv") {
    #print("Searching global environment")
    if (exists(dname, globalenv())) {
      dvalue <- get(dname, envir = globalenv())
      if (!is.function(dvalue)) {
        .ddg.save.data(dname, dvalue, scope=dscope, from.env=TRUE)
        return (TRUE)
      }
    }
  }
  
  #print(".ddg.data.node.exists NOT found")
  return(FALSE)
}

#' .ddg.data.number retrieves the number of the nearest preceding
#' current matching data node. It returns zero if no match is found.
#' 
#' @param dname data node name.
#' @param dscope (optional) data node scope.  If not provided, it uses
#'   the closest scope in which dname is found
#' @returnType integer
#' @return the id of the matching data node, or 0 if none was found
.ddg.data.number <- function(dname, dscope=NULL) {
  if (is.null(dscope)) dscope <- .ddg.get.scope(dname)
  
  ddg.data.nodes <- .ddg.data.node.table()
  matching <- ddg.data.nodes [ddg.data.nodes$ddg.name == dname & 
          (ddg.data.nodes$ddg.scope == "ddg.library" | ddg.data.nodes$ddg.scope == dscope), ]
  if (nrow (matching) > 0) {
    return (matching$ddg.num[nrow(matching)])
  }
  
  # Error message if no match found.
  error.msg <- paste("No data node found for", dname)
  .ddg.insert.error.message(error.msg)
  return(0)
}

#' Write the data nodes to a csv table.  Useful for debugging.
#' The file will be in the debug directory in a file called data-nodes.csv
.ddg.save.debug.data.nodes <- function () {
  # Save data nodes table to file.
  fileout <- paste(.ddg.path.debug(), "/data-nodes.csv", sep="")
  ddg.data.nodes <- .ddg.data.nodes()
  write.csv(ddg.data.nodes, fileout, row.names=FALSE)
}


#' .ddg.record.data records a data node in the data node table.
#' 
#' @param dtype data node type.
#' @param dname data node name.
#' @param dvalue data node value.
#' @param value the value of the data.  For some node types, this will be the same as dvalue;
#'    sometimes it is different.  For example, for snapshot nodes, dvalue is the location
#'    of the snapshot file, while value is the data itself.
#' @param dscope data node scope.
#' @param from.env (optional) if object is from initial environment.  Default is FALSE.
#' @param dtime (optional) timestamp of original file.
#' @param dloc (optional) path and name of original file.
#' @return nothing
.ddg.record.data <- function(dtype, dname, dvalue, value, dscope, from.env=FALSE, dtime="", dloc="") {
  #print("In .ddg.record.data")
  #print(paste("dvalue =", head(dvalue)))
  #print(paste("value =", head(value)))
  #print (sys.calls())
  
  if (!.ddg.is.data.type (dtype)) {
    print (paste (".ddg.record.data: bad value for dtype - ", dtype))
  }
  
  # Increment data node counter.
  ddg.dnum <- .ddg.inc("ddg.dnum")
  
  # If the table is full, make it bigger.
  ddg.data.nodes <- .ddg.data.node.table()
  if (nrow(ddg.data.nodes) < ddg.dnum) {
    ddg.data.nodes <- .ddg.add.rows("ddg.data.nodes", .ddg.create.data.node.rows ())
  }
  
  dvalue2 <- 
      if (length(dvalue) > 1 || !is.atomic(dvalue)) "complex"
      else if (!is.null(dvalue)) dvalue
      else ""
  
  # get value type
  val.type <- .ddg.get.val.type.string(value)
  
  #print(".ddg.record.data: adding info")
  ddg.data.nodes$ddg.type[ddg.dnum] <- dtype
  ddg.data.nodes$ddg.num[ddg.dnum] <- ddg.dnum
  ddg.data.nodes$ddg.name[ddg.dnum] <- dname
  ddg.data.nodes$ddg.value[ddg.dnum] <- dvalue2
  ddg.data.nodes$ddg.val.type[ddg.dnum] <- val.type
  ddg.data.nodes$ddg.scope[ddg.dnum] <- dscope
  ddg.data.nodes$ddg.from.env[ddg.dnum] <- from.env
  ddg.data.nodes$ddg.hash[ddg.dnum] <- ""
  ddg.data.nodes$ddg.rw[ddg.dnum] <- ""
  ddg.data.nodes$ddg.time[ddg.dnum] <- dtime
  ddg.data.nodes$ddg.loc[ddg.dnum] <- dloc
  
  .ddg.set("ddg.data.nodes", ddg.data.nodes)
  
  # Output data node.
  #print(".ddg.record.data outputting data node")
  if (dtype == "File") {
    dhash <- .ddg.calculate.hash(dname)
    drw <- .ddg.calculate.rw(dname)
    .ddg.set.hash (ddg.dnum, dhash, drw)

    # .ddg.add.to.hashtable(dname = dname, ddg.dnum = ddg.dnum, dloc = dloc, dvalue = dvalue, dtime = dtime)
  }
  
  if (.ddg.debug.lib()) {
    if (dtype != "File") {
      print(paste("Adding data node", ddg.dnum, "named", dname, "with scope", dscope, " and value ", ddg.data.nodes$ddg.value[ddg.dnum]))
    } else {
      # Get the table again because .ddg.add.to.hashtable changed it.
      ddg.data.nodes <- .ddg.data.node.table()
      print(paste("Adding data node", ddg.dnum, "named", dname, "with scope", dscope, " and value ", ddg.data.nodes$ddg.value[ddg.dnum], 
              " that hashes to ", ddg.data.nodes$ddg.hash[ddg.dnum], " and performs a file ", ddg.data.nodes$ddg.rw[ddg.dnum]))
    }
  }
}

#' .ddg.data.node creates a data node of type Data. Data nodes are
#' used for single data values. The value (dvalue) is stored in the
#' DDG.
#' 
#' @param dtype type of data node.
#' @param dname name of data node.
#' @param dvalue value of data node.
#' @param dscope scope of data node.
#' @param from.env if object is from initial environment
#' @return nothing
.ddg.data.node <- function(dtype, dname, dvalue, dscope, from.env=FALSE) {
  #print ("In .ddg.data.node")
  #print(paste(".ddg.data.node: dname =", dname))
  #print(paste(".ddg.data.node: typeof(dvalue) =", typeof(dvalue)))
  #print(paste(".ddg.data.node: dvalue =", dvalue))
  #print(paste(".ddg.data.node: dscope =", dscope))
  # If object or a long list, try to create snapshot node.
  
  if (is.object(dvalue)) {
    if (.ddg.is.connection(dvalue)) {
      val <- showConnections(TRUE)[as.character(dvalue[1]), "description"]

      # Record in data node table
      .ddg.record.data(dtype, dname, val, val, dscope, from.env=from.env)
      
      if (.ddg.debug.lib()) print(paste("data.node:", dtype, dname))
      return()
    }
    else {
      tryCatch(
          {
            .ddg.snapshot.node (dname, "txt", dvalue, dscope=dscope, from.env=from.env)
            return()
          },
          error = function(e) {
            error.msg <- paste("Unable to create snapshot node for", dname, "Details:", e)
            .ddg.insert.error.message(error.msg)
            .ddg.data.node (dtype, dname, "complex", dscope, from.env=from.env)
            return ()
          }
      )
    }
    
  }
  
  else if (is.matrix(dvalue) || (is.vector(dvalue) && !is.character(dvalue) && length(dvalue) > 20)) {
    .ddg.snapshot.node (dname, "csv", dvalue, dscope=dscope, from.env=from.env)
    return ()
  }
  
  #print("Converting value to a string")
  # Convert value to a string.
  val <-
      if (is.list(dvalue)) {
        tryCatch(
            {
              .ddg.convert.list.to.string(dvalue)
            },
            error = function(e) {
              error.msg <- paste("Unable to convert value of", dname, "to a string.")
              .ddg.insert.error.message(error.msg)
              "complex"
            }
        )
      }
      else if (typeof(dvalue) == "closure") "#ddg.function"
      else if (length(dvalue) > 1 || !is.atomic(dvalue)) {
        tryCatch(paste(.ddg.replace.quotes(dvalue), collapse=","),
            error = function(e) {"complex"})
      }
      else if (is.null(dvalue)) "NULL"
      else if (length(dvalue) == 0) "Empty"
      else if (is.na(dvalue)) "NA"
      else if (dvalue == "complex" || dvalue == "#ddg.function") dvalue
      else if (is.character(dvalue) && dvalue == "") "NotRecorded"
      else {
        # Replace double quotes with single quotes.
        .ddg.replace.quotes(dvalue)
      }
  
  if (grepl("\n", val)) {
    # Create snapshot node.
    .ddg.snapshot.node (dname, "txt", val, from.env=from.env)
    return ()
  }
  
  else {
    # Get scope if necessary.
    if (is.null(dscope)) dscope <- .ddg.get.scope(dname)
    
    # Record in data node table
    .ddg.record.data(dtype, dname, val, val, dscope, from.env=from.env)
    
    if (.ddg.debug.lib()) print(paste("data.node:", dtype, dname))
  }
  
  invisible()
}

#' .ddg.snapshot.node creates a data node of type Snapshot. Snapshots
#' are used for complex data values not written to file by the main
#' script. The contents of data are written to the file dname.fext
#' in the DDG directory. Snapshots are also used to capture output
#' plots and other graphics generated by the R script.
#' 
#' The user can control the size of the snapshot files by setting the
#' max.snapshot.size parameter when calling ddg.init or ddg.run.  If
#' the user passes in 0, no snapshots are saved.
#' Instead a data node will be created.  If the user passes in -1,
#' there is no limit on the snapshot size.  If the user passes a value > 0,
#' if the R value is larger than this size, only the head of the data will
#' be saved.
#' 
#' @param dname name of data node.
#' @param fext file extension.
#' @param data value of data node.
#' @param save.object (optional) if TRUE, also save as an R object.  Default FALSE
#' @param dscope (optional) scope of data node.  Default NULL
#' @param from.env (optional) true if a value set outside the script.  Default FALSE
#' 
.ddg.snapshot.node <- function(dname, fext, data, save.object = FALSE, dscope=NULL, from.env=FALSE) {
  
  orig.data <- data
  
  # Determine if we should save the entire data
  max.snapshot.size <- ddg.max.snapshot.size()
  
  # Don't save the data
  if (max.snapshot.size == 0) {
    return(.ddg.data.node ("Data", dname, "", dscope, from.env=from.env))
  }
  
  # object.size returns bytes, but max.snapshot.size is in kilobytes
  if (max.snapshot.size == -1 || object.size(data) < max.snapshot.size * 1024) {
    full.snapshot <- TRUE
  }
  
  else if (is.vector(data) || is.list(data) || is.data.frame(data) || is.matrix(data) || is.array(data)) {
    # Decide how much data to save
    
    element.size <- object.size(head(data, 1))
    num.elements.to.save <- ceiling(max.snapshot.size * 1024 / element.size)
    if (num.elements.to.save < length(data)) {
      data <- head(data, num.elements.to.save)
      full.snapshot <- FALSE
    }
    else {
      full.snapshot <- TRUE
    }
  }
  
  else {
    full.snapshot <- FALSE
  }
  
  snapname <-
      if (full.snapshot) dname
      else paste(dname, "-PARTIAL", sep="")
  
  # Snapshot type
  dtype <- "Snapshot"
  
  # If the object is an environment, update the data to be the environment's
  # name followed by a list of the variables bound in the environment.
  if (is.environment (data)) {
    envHeader <- paste0 ("<environemnt: ", environmentName (data), ">")
    data <- c (envHeader, ls(data), recursive=TRUE)
  }
  else if ("XMLInternalDocument" %in% class(data)) {
    fext <- "xml"
  }
  else if (is.vector(data) || is.data.frame(data) || is.matrix(data) || is.array(data) || is.list(data)) {
  }
  else if (!is.character(data)) {
    tryCatch(data <- as.character(data),
        error = function(e){
          # Not sure if str or summary will give us the most useful
          # information.
          #data <- capture.output(str(data));
          data <- summary(data)
        })
  }
  
  # Default file extensions.
  dfile <-
      if (fext == "" || is.null(fext)) paste(.ddg.dnum()+1, "-", snapname, sep="")
      else paste(.ddg.dnum()+1, "-", snapname, ".", fext, sep="")
  
  # Get path plus file name.
  dpfile <- paste(.ddg.path.data(), "/", dfile, sep="")
  if (.ddg.debug.lib()) print(paste("Saving snapshot in ", dpfile))
  
  # Write to file .
  if (fext == "csv") write.csv(data, dpfile, row.names=FALSE)
  
  else if (fext == "xml") saveXML (data, dpfile)
  
  # Capture graphic.
  else if (.ddg.supported.graphic(fext)) .ddg.graphic.snapshot(fext, dpfile)
  
  # Write out RData (this is old code, not sure if we need it).
  else if (fext == "RData") file.rename(paste(.ddg.path.data(), "/", dname, sep=""), dpfile)
  
  # Write out text file for txt or empty fext.
  else if (fext == "txt" || fext == "") {
    file.create(dpfile, showWarnings=FALSE)
    if (is.list(data) && length(data) > 0) {
      list.as.string <- .ddg.convert.list.to.string(data)
      write(list.as.string, dpfile)
    }
    else {
      tryCatch(write(as.character(data), dpfile),
          error = function(e){
            capture.output(data, file=dpfile)
          })
    }
  }
  
  # Write out data node object if the file format is unsupported.
  else {
    error.msg <- paste("File extension", fext, "not recognized")
    .ddg.insert.error.message(error.msg)
    return(NULL)
  }
  
  # Check to see if we want to save the object.
  if (save.object && full.snapshot) save(data, file = paste(.ddg.path.data(), "/", .ddg.dnum()+1, "-", snapname, ".RObject", sep=""), ascii = TRUE)
  
  dtime <- .ddg.timestamp()
  
  # Get scope if necessary.
  if (is.null(dscope)) dscope <- .ddg.get.scope(dname)
  
  # Record in data node table
  .ddg.record.data(dtype, dname, paste(.ddg.data.dir(), dfile, sep="/"), orig.data, dscope, from.env=from.env, dtime)
  
  if (.ddg.debug.lib()) print(paste("snapshot.node: ", dname))
  return(dpfile)
}

#' .ddg.file.copy creates a data node of type File. File nodes are
#' used for files written by the main script. A copy of the file is
#' written to the DDG directory.
#' 
#' @param fname path and name of original file.
#' @param dname name of data node.
#' @param dscope scope of data node.
#' @return nothing
.ddg.file.copy <- function(fname, dname, dscope) {
  # Calculate location of original file.
  file.loc <- normalizePath(fname, winslash="/", mustWork = FALSE)
  
  # Copy file.
  if (file.exists(file.loc)) {
    # Create file node in DDG.
    dpfile.out <- .ddg.file.node("File",fname,dname, dscope)
    file.copy(file.loc, dpfile.out, overwrite=TRUE)
  }
  else {
    # For zipfiles, 
    file.loc <- normalizePath(dname, winslash="/", mustWork = FALSE)
    if (file.exists(file.loc)) {
      # Create file node in DDG.
      dpfile.out <- .ddg.file.node("File",fname,dname, dscope)
      file.copy(file.loc, dpfile.out, overwrite=TRUE)
    }
    else {
      error.msg <- paste("File to copy does not exist:", fname)
      .ddg.insert.error.message(error.msg)
      return()
    }
  }
  
  if (.ddg.debug.lib()) print(paste("file.copy: FILE ", file.loc))
  return ()
}

#' .ddg.file.node creates a node of type File. File nodes are used
#' for files written to the DDG directory by capturing output from
#' the script or by copying a file that is written by the script.
#' Returns the path where the file referenced by the node is stored.
#' 
#' @param dtype - type of data node.
#' @param fname - path and name of original file.
#' @param dname - name of data node.
#' @param dscope (optional) - scope of data node.
#' @returnType string
#' @return the full path to the saved file
.ddg.file.node <- function(dtype, fname, dname, dscope=NULL) {
  
  # Get original file location.
  file.name <- basename(fname)
  file.loc <- normalizePath(fname, winslash="/", mustWork = FALSE)
  
  # Add number to file name.
  dfile <- paste(.ddg.dnum()+1, "-", file.name, sep="")
  
  # Calculate the path to the file relative to the ddg directory.
  # This is the value stored in the node.
  dpfile <- paste(.ddg.data.dir(), dfile, sep="/")
  
  # Set the node label.
  if (is.null(dname)) dname <- file.name
  
  # Get scope if necessary.
  if (is.null(dscope)) dscope <- .ddg.get.scope(dname)
  
  # Record in data node table
  .ddg.record.data(dtype, dname, dpfile, dpfile, dscope, from.env=FALSE, dtime=.ddg.timestamp(), file.loc)
  
  # Get path plus file name to where the file will be copied
  dpath <- paste(.ddg.path.data(), "/", dfile, sep="")
  return(dpath)
}

#' Creates a node of type URL. URL nodes are used
#' for URLs and also for server connections.
#' 
#' @param original the actual url or server connection description
#' @param saved the name of the file where a copy has been saved
#'
#' @return nothing
.ddg.url.node <- function(original, saved) {
  # Record in data node table
  .ddg.record.data("URL", original, saved, saved, 
      dscope=environmentName(.GlobalEnv), from.env=FALSE, dtime=.ddg.timestamp())
}

#' .ddg.save.data takes as input the name and value of a data node
#' that needs to be created. It determines how the data should be
#' output (or saved) and saves it in that format.
#' 
#' @param name name of created node.
#' @param value value of created node.
#' @param graphic.fext (optional) file extension for graphic file.  Default is jpeg
#' @param error (optional) if TRUE, raise an R error rather than a
#'   DDG error.
#' @param scope default is NULL
#' @param from.env if node is from initial environment
#' @param stack (optional) stack to use in determing scope.
#' @param env (optional) default is NULL
#' @return nothing
.ddg.save.data <- function(name, value, graphic.fext='jpeg', error=FALSE, scope=NULL, from.env=FALSE, stack=NULL, env=NULL){
  if (is.null(scope)) {
    scope <- .ddg.get.scope(name, calls=stack, env=env)
  }
  
  # Determine type for value, and save accordingly.
  if (.ddg.is.graphic(value)) .ddg.write.graphic(name, value, graphic.fext, scope=scope, from.env=from.env)
  else if (.ddg.is.simple(value)) .ddg.save.simple(name, value, scope=scope, from.env=from.env)
  else if (.ddg.is.csv(value)) .ddg.write.csv(name, value, scope=scope, from.env=from.env)
  else if (is.list(value) || is.array(value)) .ddg.snapshot.node(name, "txt", value, save.object=TRUE, dscope=scope, from.env=from.env)
  else if (.ddg.is.connection(value)) {.ddg.save.simple(name, value, scope=scope, from.env=from.env)}
  else if (.ddg.is.object(value)) {.ddg.snapshot.node(name, "txt", value, dscope=scope, from.env=from.env) }
  else if (.ddg.is.function(value)) .ddg.save.simple(name, "#ddg.function", scope=scope, from.env=from.env)
  else if (error) stop("Unable to create data (snapshot) node.")
  else {
    error.msg <- paste("Unable to create data (snapshot) node.")
    .ddg.insert.error.message(error.msg)
  }
  invisible()
}

#' .ddg.save.simple takes in a simple name-value pair and creates
#' a data node. Extra long strings are saved as snapshots.
#' 
#' @param name data node name.
#' @param value data node value.
#' @param scope data node scope.
#' @return nothing
.ddg.save.simple <- function(name, value, scope=NULL, from.env=FALSE) {
  # Save extra long strings as snapshot.
  if (is.character(value) && nchar(value) > 200) {
    .ddg.snapshot.node(name, "txt", value, dscope=scope, from.env=from.env)
  } else {
    # Save the true value.
    .ddg.data.node("Data", name, value, scope, from.env=from.env)
  }
}

#' .ddg.write.graphic takes as input the name of a variable as well
#' as its value and attempts to write it out as a graphics file. If
#' all else fails, it writes out the information as a text file and
#' also writes out an RData Object which can later be read back into
#' the system.
#'
#' @param name data node name.
#' @param value data node value.
#' @param fext file extension.
#' @param scope data node scope.
#' @param from.env If TRUE, means the value was assigned outside the script
#' @return nothing
.ddg.write.graphic <- function(name, value=NULL, fext="jpeg", scope=NULL, from.env=FALSE){
  # Try to output graphic value.
  tryCatch({
        .ddg.snapshot.node(name, fext, NULL, dscope=scope, from.env=from.env)
      }, error = function(e) {
        # warning(paste("Attempted to write", name, "as", fext, "snapshot. Trying jpeg", ".", e))
        tryCatch({
              .ddg.snapshot.node(name, "jpeg", NULL, dscope=scope, from.env=from.env)
            }, error = function(e) {
              # warning(paste("Attempted to write", name, "as jpeg snapshot. Failed.", e, "Defaulting to saving RObject and .txt file."))
              .ddg.snapshot.node(name, "txt", value, save.object = TRUE, dscope=scope, from.env=from.env)
            })
      })
}

#' .ddg.write.csv takes as input a name-value pair for a
#' variable and attempts to save the data as a csv file. It does
#' not create any edges but does add the node to the DDG. Edge
#' creation should occur from wherever this function is called.
#' 
#' @param name data node name.
#' @param value data node value.
#' @param scope data node scope.
#' @param from.env TRUE if defined outside the script
#' @return nothing
.ddg.write.csv <- function(name, value, scope=NULL, from.env=FALSE) {
  tryCatch({
        .ddg.snapshot.node(name, "csv", value, dscope=scope, from.env=from.env)
      }, error = function(e) {
        # warning(paste("Attempted to write", name, "as .csv snapshot but failed. Out as RDataObject.", e))
        .ddg.snapshot.node(name, "txt", value, save.object = TRUE, dscope=scope, from.env=from.env)
      })
}


