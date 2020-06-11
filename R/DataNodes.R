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

########################## DataNodes.R ###########################
#
# This file contains functions involved in managing data nodes.
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
# loc - absolute path to the original file
#
# ddg.pnum is the number associated with the last procedure node created
#
# This file also contains functions that create data nodes of various types.


#' .ddg.init.data.nodes initializes the information needed to manage data nodes.
#' @return nothing
#' @noRd

.ddg.init.data.nodes <- function () {
  .ddg.set("ddg.data.nodes", .ddg.create.data.node.rows()) 
  .ddg.set("ddg.dnum", 0)
  
  # Name of the last variable that was a plot created by ggplot
  .ddg.set ("ddg.last.ggplot", "")
  
  # Set snapshot.size.  Make sure it is not already set, 
  # as someone may have called prov.set.detail.
  
  if (!.ddg.is.set("ddg.snapshot.size")) {
    .ddg.set("ddg.snapshot.size", 0)
  }
}

#' .ddg.snapshot.size returns the current maximum size for snapshots 
#' in kilobytes.
#' @return maximum snapshot size in KB
#' @noRd

.ddg.snapshot.size <- function() {
  return(.ddg.get("ddg.snapshot.size"))
}

#' .ddg.is.data.type returns TRUE for any type of data node.
#' This is used for type-checking. 
#' @param type data node type.
#' @return true for any type of data node
#' @noRd

.ddg.is.data.type <- function(type) {
  return(type %in% c("Data", "Device", "Snapshot", "File", "URL", "Exception", 
          "StandardOutput", "StandardOutputSnapshot"))
}

#' .ddg.dnum returns the counter used to assign data node ids
#' @return the data node id of the last data node created
#' @noRd

.ddg.dnum <- function() {
  return (.ddg.get("ddg.dnum"))
}

#' .ddg.create.data.node.rows create a data frame of empty rows to put in
#' the data node table.It is faster to add a bunch of empty rows and update them 
#' than to add one row at a time
#' @param size the number of rows to add
#' @return a data frame with size rows, with all columns being empty vectors
#' @noRd

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
          ddg.loc = character(size),
          stringsAsFactors=FALSE))
}
  
#' .ddg.data.node.table returns the data node table
#' @return the data node table
#' @noRd

.ddg.data.node.table <- function() {
  return (.ddg.get("ddg.data.nodes"))
}

#' .ddg.data.nodes returns the filled rows of the data node table
#' @return the filled rows of the data node table
#' @noRd

.ddg.data.nodes <- function() {
  ddg.data.nodes <- .ddg.get("ddg.data.nodes")
  return (ddg.data.nodes [ddg.data.nodes$ddg.num > 0, ])
}

#' .ddg.data.node.exists searches the data node table for a matching
#' data node and returns TRUE if a match is found. If no match is found, it 
#' checks if a variable with that name exists in the global environment.
#' If a match is found, it creates a data node for the global and returns TRUE. 
#' If no node exists and it is not a global, it returns FALSE.
#' @param dname data node name. For files, this is the entire file name.
#' @param dscope data node scope.  If NULL, uses the closest scope in which
#' dname is defined
#' @param dtype - type of data node.
#' @return true if a node with the given name exists
#' @noRd

.ddg.data.node.exists <- function(dname, dscope=NULL, dtype=NULL) {
  if (is.null(dscope))
    dscope <- .ddg.get.scope(dname)
  
  # Search data nodes table.
  #print (paste (".ddg.data.node.exists: Looking for", dname, "in scope", dscope))
  ddg.data.nodes <- .ddg.data.node.table()
  
  if( identical(dtype, "File") ) {
  	
  	# get basename and full path to file
  	full.path <- normalizePath(dname, winslash="/", mustWork = FALSE)
  	basename <- basename(dname)
    
    matching <- ddg.data.nodes[
                  (ddg.data.nodes$ddg.type == "File" &
                  ddg.data.nodes$ddg.name == basename &
                  ddg.data.nodes$ddg.scope == "undefined" &
                  ddg.data.nodes$ddg.hash == .ddg.calculate.hash(full.path) &
                  ddg.data.nodes$ddg.loc == full.path),
                ]
  }
  else {
    matching <- ddg.data.nodes [ddg.data.nodes$ddg.name == dname & 
            (ddg.data.nodes$ddg.scope == "ddg.library" | 
             ddg.data.nodes$ddg.scope == dscope), ]
  }
  
  if (nrow (matching) > 0) {
    return(TRUE)
  }
  
  # Search initial environment table.
  if (dscope == "R_GlobalEnv") {
    #print("Searching global environment")
    ddg.initial.var <- .ddg.initial.var()
    index <- which(ddg.initial.var$ddg.name == dname)

    if (length(index) > 0) {
      ddg.initial.env <- .ddg.initial.env()
      dvalue <- get(dname, envir = ddg.initial.env)

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
#' This occurs if the variable is in a scope that is not visible at
#' the current line of code.
#' @param dname data node name.
#' @param dscope (optional) data node scope.  If not provided, it uses
#' the closest scope in which dname is found
#' @return the id of the matching data node, or 0 if none was found
#' @noRd

.ddg.data.number <- function(dname, dscope=NULL) {
  if (is.null(dscope)) dscope <- .ddg.get.scope(dname)
  
  ddg.data.nodes <- .ddg.data.node.table()
  matching <- ddg.data.nodes [ddg.data.nodes$ddg.name == dname & 
          (ddg.data.nodes$ddg.scope == "ddg.library" | 
           ddg.data.nodes$ddg.scope == dscope), ]
  if (nrow (matching) > 0) {
    return (matching$ddg.num[nrow(matching)])
  }
  
  return(0)
}

#' .ddg.save.debug.data.nodes writes the data nodes to a csv table. Useful for debugging.
#' The file will be in the debug directory in a file called data-nodes.csv
#' @return nothing
#' @noRd

.ddg.save.debug.data.nodes <- function () {
  # Save data nodes table to file.
  fileout <- paste(.ddg.path.debug(), "/data-nodes.csv", sep="")
  ddg.data.nodes <- .ddg.data.nodes()
  utils::write.csv(ddg.data.nodes, fileout, row.names=FALSE)
}


#' .ddg.record.data records a data node in the data node table. 
#' @param dtype data node type.
#' @param dname data node name.
#' @param dvalue value to store in the data node
#' @param value the value of the data.  For some node types, this will be the same as dvalue;
#' sometimes it is different.  value is used to calculate the val type, so it should be the original
#' data, while dvalue is the string representation to place directly in the node.
#' @param dscope data node scope.
#' @param from.env (optional) if object is from initial environment.  Default is FALSE.
#' @param dtime (optional) timestamp of original file.
#' @param dloc (optional) path and name of original file.
#' @return nothing
#' @noRd

.ddg.record.data <- function(dtype, dname, dvalue, value, dscope, from.env=FALSE, 
                             dtime="", dloc="") {
  #print("In .ddg.record.data")
  #print(paste("dvalue =", utils::head(dvalue)))
  #print(paste("value =", utils::head(value)))
  
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
  
  # get value type
  val.type <- 
      if (dtype == "Device") "Device"
      else .ddg.get.val.type.string(value)
  
  #print(".ddg.record.data: adding info")
  ddg.data.nodes$ddg.type[ddg.dnum] <- dtype
  ddg.data.nodes$ddg.num[ddg.dnum] <- ddg.dnum
  ddg.data.nodes$ddg.name[ddg.dnum] <- dname
  ddg.data.nodes$ddg.value[ddg.dnum] <- dvalue
  ddg.data.nodes$ddg.val.type[ddg.dnum] <- val.type
  ddg.data.nodes$ddg.scope[ddg.dnum] <- dscope
  ddg.data.nodes$ddg.from.env[ddg.dnum] <- from.env
  ddg.data.nodes$ddg.hash[ddg.dnum] <- ""
  ddg.data.nodes$ddg.time[ddg.dnum] <- dtime
  ddg.data.nodes$ddg.loc[ddg.dnum] <- dloc
  
  .ddg.set("ddg.data.nodes", ddg.data.nodes)

  # Output data node.
  #print(".ddg.record.data outputting data node")
  if (dtype == "File") {
    .ddg.set.hash (ddg.dnum, dloc, dvalue, dtime)
  }
  else if (dtype == "URL") {
    .ddg.set.hash (ddg.dnum, paste0(.ddg.get("ddg.path"), "/", dvalue), dvalue, dtime)
  }
  
  if (.ddg.debug.lib()) {
    if (dtype != "File") {
      print(paste("Adding data node", ddg.dnum, "named", dname, 
                  "with scope", dscope, 
                  " and value ", ddg.data.nodes$ddg.value[ddg.dnum]))
    } else {
      # Get the table again because .ddg.add.to.hashtable changed it.
      ddg.data.nodes <- .ddg.data.node.table()
      print(paste("Adding data node", ddg.dnum, "named", dname, 
                  "with scope", dscope, 
                  " and value ", ddg.data.nodes$ddg.value[ddg.dnum]))
    }
  }
}

#' .ddg.get.node.val
#' 
#' Calculates the string value to store in the data node.  
#' 
#' @return If the value is a short value, it returns the value.  
#' If it is a long value and snapshots are not being saved, it is a 
#' 1-line value that is truncated.  If it is a long value
#' and snapshots are being saved, it returns NULL.
#' 
#' @param value the actual data value
#' @param dname the name of the object we are getting the value of
#' @noRd
.ddg.get.node.val <- function (value, dname=NULL, dtype = "") {
  if (is.null (value)) {
    return ("NULL")
  }
  
  # No snapshots, so we want to save a value, perhaps truncated
  if (.ddg.snapshot.size() == 0) {
    #print (".ddg.get.node.val: no snapshots")
    
    # Get a string version of the value
    if (dtype == "StandardOutput") {
      #print (".ddg.get.node.val: StandardOutput")
      print.value <- value
    }
    else if (is.data.frame (value)) {
      #print (".ddg.get.node.val: data frame")
      print.value <- utils::capture.output (print (value[1,]))
      if (length(print.value) > 1) {
        print.value <- paste ("Row", print.value[[2]])
      }
    }
    else if (is.array(value) && length (dim(value)) > 1) {
      #print (".ddg.get.node.val: multi-dimensional array ")
      # Remove dimension names if they exist
      value.copy <- value
      dimnames(value.copy) <- NULL
      print.value <- utils::capture.output (print (value.copy))
      print.value <- Find (function (line) return (startsWith (stringi::stri_trim_left(line), "[1,")), print.value)
    }
    else if (is.list (value)) {
      #print (".ddg.get.node.val: list")
      print.value <- paste (utils::capture.output (print (unlist (value))), collapse="")
      
      # Remove leading spaces
      print.value <- sub ("^ *", "", print.value)
    }
    else if (.ddg.is.connection(value)) {
      #print (".ddg.get.node.val: connection")
      print.value <- showConnections(TRUE)[as.character(value[1]), "description"]
    }
    else   if (is.environment(value)) {
      #print (".ddg.get.node.val: environment")
      env.vars <- ls (value)
      env.name <- environmentName(value)
      if (length (env.vars) == 0) {
        return (paste0 ("Environment ", env.name, ": Empty environment"))
      }
      print.value <- paste0 ("Environment ", env.name, ": ", .ddg.get.node.val (env.vars))
      .ddg.remember.env (dname, env.vars)
    }  
    else {
      #print (".ddg.get.node.val: else")
      #print(value)
      print.value <- utils::capture.output (print (value))
    }
    
    # Strip off the index 
    if (!is.array (value)) {
      print.value <- sub ("^.*?] ", "", print.value)
    }
    
    # Keep just the first line
    if (length (print.value) > 1) {
      print.value <- paste0 (print.value[1], "...")
    }
    
    # Keep just the first line
    if (grepl ("\n", print.value)) {
      print.value <- paste0 (sub ("\\n.*", "", print.value), "...")
    }
    
    # Truncate it if it is long
    if (nchar(print.value) > 80) {
      print.value <- paste0 (substring (print.value, 1, 80), "...")
    } 
    
    return (print.value)
  }
  
  # Saving snapshots
  else {
    if (dtype == "StandardOutput") {
      print.value <- value
    }
    else if (is.list (value) && length(value) > 0) {
      print.value <- paste (utils::capture.output (print (unlist (value))), collapse="")
      
      # Remove leading spaces
      print.value <- sub ("^ *", "", print.value)
    }
    else if (is.environment(value)) {
      env.vars <- ls (value)
      env.name <- environmentName(value)
      if (length (env.vars) == 0) {
        return (paste0 ("Environment ", env.name, ": Empty environment"))
      }
      env.vars.string <- .ddg.get.node.val (env.vars)
      if (is.null (env.vars.string)) {
        return (NULL)
      }
      print.value <- paste0 ("Environment ", env.name, ": ", env.vars.string)
      .ddg.remember.env (dname, env.vars)
    }  
    else {
      print.value <- utils::capture.output (print (value))
    }
    
    # If it is not a 1-liner, we should save the value in a snapshot file
    if (length (print.value) > 1 || grepl ("\n", print.value)) {
      return (NULL)
    }
    
    # Strip off the index 
    if (!is.array (value)) {
      print.value <- sub ("^.*?] ", "", print.value)
    }
    
    # If it is a short 1-line value, store the value
    if (nchar (print.value) <= 80) {
      return (print.value)
    }
    
    # If long, value should go in a snapshot file
    else {
      return (NULL)
    }
  }
}

#' .ddg.remember.env remembers what variables are in an environment
#' @param env.var.name the name of the environment variable
#' @param env.vars the variables within the environment
#' @noRd
.ddg.remember.env <- function (env.var.name, env.vars) {
  ddg.envList <- .ddg.get ("ddg.envList")
  ddg.envList[[env.var.name]] <- env.vars
  .ddg.set("ddg.envList", ddg.envList)
}


#' .ddg.get.val.type.string returns the type information for a given value as a string.
#' "null" for null values. For values of length 1, it is the type of the value.
#' For longer values, the description includes the container (like vector, matrix, ...),
#' the dimensions, and the type of the members of the data structure
#' @param value the value
#' @return the type information as a string
#' @noRd

.ddg.get.val.type.string <- function(value)
{
  val.type <- .ddg.get.val.type(value)
  
  if( is.null(val.type) )
    return( "null" )
  
  # object, environment, function, language
  if( length(val.type) == 1 )
    return( val.type )
  
  # list
  if (length (val.type) == 2) {
    return (paste ('{"container":"', val.type[[1]], 
        '", "dimension":[', val.type[[2]], 
        ']}', sep = ""))
  }
  
  # vector, matrix, array, data frame
  # type information recorded in a list of 3 vectors (container,dimension,type)
  container <- val.type[[1]]
  dimension <- val.type[[2]]
  type <- val.type[[3]]
  
  # matrix: a 2-dimensional array (uniform typing)
  # array: n-dimensional (uniform typing)
  # data frame: list of vectors
  if( !identical(container, "vector"))
  {
    # Record size of each dimension
    dimension <- paste( dimension, collapse = "," )

    # data frame.  Record type of each column
    if (identical(container, "data_frame")) {
      type <- paste( type, collapse = '","' )
    }
  }
  
  return( paste('{"container":"', container, 
                '", "dimension":[', dimension, 
                '], "type":["', type, '"]}', sep = "") )
}

#' .ddg.get.val.type returns the type information for a given value
#' There are several return types possible:
#' For lists, objects, environments, functions and language values,
#' the return type is string.  For vectors, matrices, arrays 
#' and data frames, a list is returned.  The list contains 3 parts: a
#' string representation of the container, dimension information,
#' and the types of values in the structure.  If anything else
#' is found, it returns NULL.
#' @param value the value
#' @return the type information
#' @noRd

.ddg.get.val.type <- function(value)
{
  # data frame: is a type of list
  if(is.data.frame(value))
  {
    types <- unname(sapply(value, .ddg.get.lowest.class))
    return( unname(list("data_frame", dim(value), types)) )
  }
  
  # an object
  if(is.object(value))
    return(.ddg.get.lowest.class(value))
  
  # a list
  if(is.list(value))
    return(list ("list", length(value)))
  
  # vector: a 1-dimensional array (uniform typing).  is.vector also returns
  # true for lists
  if(is.vector(value))
    return( list("vector", length(value), .ddg.get.lowest.class(value)) )
  
  # matrix: a 2-dimensional array (uniform typing)
  if(is.matrix(value))
    return( list("matrix", dim(value), .ddg.get.lowest.class(value[1])) )
  
  # array: n-dimensional (uniform typing)
  if(is.array(value))
    return( list("array", dim(value), .ddg.get.lowest.class(value[1])) )
  
  if (is.factor (value)) {
    return (paste("Factor levels: ", paste (levels (value), collapse=", ")))
  }
  
  # envrionment, function, language
  if(is.environment(value))
    return("environment")
  if(is.function(value))
    return("function")
  if(is.language(value))
    return("language")
  
  # none of the above - null is a character, not NULL or NA
  return(NULL)
}

#' .ddg.get.lowest.class returns the first element that the function class returns.
#' When inheritance is used, this is the lowest type. 
#' @param obj object
#' @return first element returned by class
#' @noRd

.ddg.get.lowest.class <- function( obj )
{
  return( class(obj)[1] )
}


#' .ddg.data.node creates a data node of type Data. Data nodes are
#' used for single data values. The value (dvalue) is stored in the
#' DDG. 
#' @param dtype type of data node.
#' @param dname name of data node.
#' @param dvalue value of data node.
#' @param dscope scope of data node.
#' @param from.env if object is from initial environment
#' @return nothing
#' @noRd

.ddg.data.node <- function(dtype, dname, dvalue, dscope, from.env=FALSE) {
  #print ("In .ddg.data.node")
  #print(paste(".ddg.data.node: dname =", dname))
  #print(paste(".ddg.data.node: str(dvalue) =", utils::str(dvalue)))
  #print(paste(".ddg.data.node: dvalue =", dvalue))
  #print(paste(".ddg.data.node: dscope =", dscope))
  
  # Get scope if necessary.
  if (is.null(dscope)) dscope <- .ddg.get.scope(dname)
  
  val <- 
      if (dtype == "Exception") dvalue
      else .ddg.get.node.val (dvalue, dname, dtype)
  
  # .ddg.get.node.val returns NULL if we should store the value in a snapshot.
  # Otherwise, it returns a string representation of the value to store
  # in the node.
  if (is.null(val)) {
    snapfile <- .ddg.save.snapshot (dname, dvalue, 
                                    dscope, from.env=from.env, dtype)
    dtime <- .ddg.timestamp()
    if (dtype == "Data") {
      .ddg.record.data("Snapshot", dname, snapfile, dvalue, dscope, from.env=from.env, dtime)
    }
    else {
      .ddg.record.data("StandardOutputSnapshot", dname, snapfile, dvalue, dscope, from.env=from.env, dtime)
    }
  }
  
  else {
    # Record in data node table
    .ddg.record.data(dtype, dname, val, dvalue, dscope, from.env=from.env)
    
    if (.ddg.debug.lib()) print(paste("data.node:", dtype, dname))
  }
  
  invisible()
}

#' .ddg.device.node creates a node to represent a graphics device
#' There is no value associated with this node.  It allows us
#' to chain together the lineage of plotting calls.
#' @param dname name of the device node.
#' @return nothing
#' @noRd

.ddg.device.node <- function(dname) {
  # TODO: Need to update DDG Explorer to display these.  Use a new color?
  # Record in data node table
  .ddg.record.data("Device", dname, "", "", "undefined")
  if (.ddg.debug.lib()) print(paste("device.node:", dname))
  
  invisible()
}

#' .ddg.get.element.size estimates the size in memory of the first 
#' row (data frame or matrix) or first element (vector, array, or list)
#' of a complex data object.
#' @param data a complex data object (data frame, matrix, vector, array,
#' or list)
#' @return size in memory of first row or first element
#' @noRd

.ddg.get.element.size <- function(data) {
  element <- list()  
  
  # data frame or matrix
  if (is.data.frame(data) || is.matrix(data)) {
    for (i in 1:ncol(data)) {
      element[[i]] <- data[1, i, drop=TRUE]
      if (is.factor(element[[i]])) element[[i]] <- factor(element[[i]])
    } 

  # vector, list, or array
  } else {
    element <- data[1]
  }

  return(utils::object.size(element))
}

#' .ddg.save.snapshot saves the contents of data to a file
#' in the DDG data directory. The user 
#' can control whether snapshots are saved and the size of snapshot
#' files by setting the parameters snapshots and snapshot.size
#' when calling prov.init or prov.run.  If the user passes in Inf,
#' there is no limit on the snapshot size.  If the user passes a value > 0,
#' if the R value is larger than this size, only the head of the data will
#' be saved.
#' @param dname name of data node.
#' @param data value of data node.
#' @param dscope (optional) scope of data node.  Default NULL
#' @param from.env (optional) true if a value set outside the script.  Default FALSE
#' @return path and name of snapshot file, relative to the ddg directory
#' @noRd

.ddg.save.snapshot <- function (dname, data, dscope, from.env, dtype) {  
  
  # Determine what type of file to create.  We do this before checking
  # the size, because for functions, the type of the data will
  # change from function to text when we determine whether to
  # truncate it.
  if ("XMLInternalDocument" %in% class(data)) {
    fext <- "xml"
  }
  else if (is.data.frame(data) || is.matrix(data)) {
    fext <- "csv"
  }
  else if (is.function (data)) {
    fext <- "R"
  }
  else {
    fext <- "txt"
  }
  
  # Determine if we should save the entire data
  snapshot.size <- .ddg.snapshot.size()
  
  orig.data <- data
  if (is.environment(data)) {
    data <- ls(data)
  }
  
  # object.size returns bytes, but snapshot.size is in kilobytes
  if (snapshot.size == Inf || utils::object.size(data) < snapshot.size * 1024) {
    full.snapshot <- TRUE
  }
  
  else if (is.vector(data) || is.list(data) || is.data.frame(data) || 
           is.matrix(data) || is.array(data)) {
    
    # Decide how much data to save
    element.size <- .ddg.get.element.size(data)
    num.elements.to.save <- ceiling(snapshot.size * 1024 / element.size)

    if (is.data.frame(data) || is.matrix(data) || is.array(data)) {
      total.length <- nrow(data)
    } else {
      total.length <- length(data)
    }

    if (num.elements.to.save < total.length) {
      data <- utils::head(data, num.elements.to.save)
      full.snapshot <- FALSE
    }
    else {
      full.snapshot <- TRUE
    }
  }
  
  else if (is.function (data)) {
    func.text <- deparse (data)
    if (utils::object.size(func.text) < snapshot.size * 1024) {
      data <- func.text
      full.snapshot <- TRUE
    }
    else {
      data <- substr (func.text, 1, snapshot.size * 1024)
      full.snapshot <- FALSE
    }
  }
  
  else {
    full.snapshot <- FALSE
  }
  
  snapname <-
      if (full.snapshot) dname
      else paste(dname, "-PARTIAL", sep="")
  
  # Snapshot type
  # dtype <- "Snapshot"
  
  # Default file extensions.
  dfile <- paste(.ddg.dnum()+1, "-", snapname, ".", fext, sep="")
  
  # Get path plus file name.
  dpfile <- paste(.ddg.path.data(), "/", dfile, sep="")
  if (.ddg.debug.lib()) print(paste("Saving snapshot in ", dpfile))
  
  # Write to file .
  if (fext == "csv") {
    utils::write.csv(data, dpfile, row.names=FALSE)
  }
  
  else if (fext == "xml") XML::saveXML (data, dpfile)
  
  # Capture graphic.
  else if (.ddg.supported.graphic(fext)) .ddg.graphic.snapshot(fext, dpfile)
  
  # Write out RData (this is old code, not sure if we need it).
  else if (fext == "RData") {
    file.rename(paste(.ddg.path.data(), "/", dname, sep=""), dpfile)
  }
  
  # Write out text file for txt or empty fext.
  else if (fext == "txt" || fext == "" || fext == "R") {
    file.create(dpfile, showWarnings=FALSE)
    if ("ggplot" %in% class(data)) {
      write(utils::capture.output(unlist(data)), dpfile)
    }
    else if (is.environment(orig.data)) {
      write(paste0 ("Environment ", environmentName(orig.data), ":\n  ", paste (utils::capture.output(data), collapse="\n  ")), dpfile)
    }
    else if (dtype == "StandardOutput") {
      writeLines(data, dpfile)
    }
    else {
      write(utils::capture.output(data), dpfile)
    }
  }
  
  # Write out text file for txt or empty fext.
  else if (fext == "R") {
    file.create(dpfile, showWarnings=FALSE)
    write(data, dpfile)
  }
  
  # Write out data node object if the file format is unsupported.
  else {
    error.msg <- paste("File extension", fext, "not recognized")
    .ddg.insert.error.message(error.msg)
    return(NULL)
  }
  
  # Check to see if we want to save the object.
  if (full.snapshot) {
    save(data, file = paste(.ddg.path.data(), "/", .ddg.dnum()+1, "-", snapname, 
                            ".RObject", sep=""), ascii = TRUE)
  }
  
  return(paste(.ddg.data.dir(), dfile, sep="/"))
}

#' .ddg.supported.graphic - the sole purpose of this function is
#' to verify that the input file extension is a supported graphic
#' type. Currently supported graphics types inlude: jpg, jpeg,
#' bmp, png, tiff. 
#' @param ext file extension.
#' @return TRUE if the extension passed in is a known graphics type
#' @noRd

.ddg.supported.graphic <- function(ext){
  return(ext %in% c("jpeg", "jpg", "tiff", "png", "bmp", "pdf"))
}

#' .ddg.file.copy creates a data node of type File. File nodes are
#' used for files written by the main script. A copy of the file is
#' written to the DDG directory. 
#' @param fname path and name of original file.
#' @param dname name of data node.
#' @param dscope scope of data node.
#' @return nothing
#' @noRd

.ddg.file.copy <- function(fname, dname=NULL, dscope=NULL) {
  # Calculate location of original file.
  file.loc <- normalizePath(fname, winslash="/", mustWork = FALSE)
  
  # Copy file.
  if (file.exists(file.loc)) {
    # Create file node in DDG.
    dpfile.out <- .ddg.file.node("File", fname, dname, dscope)
    file.copy(file.loc, dpfile.out, overwrite=TRUE, copy.date=TRUE)
  }
  else {
    # For zipfiles, 
    file.loc <- normalizePath(dname, winslash="/", mustWork = FALSE)
    if (file.exists(file.loc)) {
      # Create file node in DDG.
      dpfile.out <- .ddg.file.node("File", fname, dname, dscope)
      file.copy(file.loc, dpfile.out, overwrite=TRUE, copy.date=TRUE)
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
#' @param dtype - type of data node.
#' @param fname - path and name of original file.
#' @param dname - name of data node.
#' @param dscope (optional) - scope of data node.
#' @return the full path to the saved file
#' @noRd

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
  .ddg.record.data(dtype, dname, dpfile, dpfile, dscope, from.env=FALSE, 
                   dtime=.ddg.timestamp(), file.loc)
  
  # Get path plus file name to where the file will be copied
  dpath <- paste(.ddg.path.data(), "/", dfile, sep="")
  return(dpath)
}

#' .ddg.url.node creates a node of type URL. URL nodes are used
#' for URLs and also for server connections.
#' @param original the actual url or server connection description
#' @param saved the name of the file where a copy has been saved
#' @return nothing
#' @noRd

.ddg.url.node <- function(original, saved) {
  # Record in data node table
  .ddg.record.data("URL", original, saved, saved, 
      dscope=environmentName(.GlobalEnv), from.env=FALSE, dtime=.ddg.timestamp())
}

#' .ddg.is.graphic tries to decipher if the value snapshot should be
#' written to file directly from the data or if it is a graphic which
#' can be captured from the image device. This function, as written,
#' is basically a hack. There must be a better way to implement it. 
#' @param value value to test
#' @return TRUE if the value is in the gg or ggplot class
#' @noRd

.ddg.is.graphic <- function(value){
  # Matching any of these classes automatically classifies the
  # object as a graphic.
  graph.classes <- list("gg", "ggplot")
  return(is.object(value) && any(class(value) %in% graph.classes))
}

#' .ddg.save.data takes as input the name and value of a data node
#' that needs to be created. It determines how the data should be
#' output (or saved) and saves it in that format. 
#' @param name name of created node.
#' @param value value of created node.
#' @param graphic.fext (optional) file extension for graphic file.  Default is jpeg
#' @param error (optional) if TRUE, raise an R error rather than a
#' DDG error.
#' @param scope default is NULL
#' @param from.env if node is from initial environment
#' @param stack (optional) stack to use in determing scope.
#' @param env (optional) default is NULL
#' @return nothing
#' @noRd

.ddg.save.data <- function(name, value, graphic.fext="jpeg", error=FALSE, 
                           scope=NULL, from.env=FALSE, stack=NULL, env=NULL){
  #print (paste (".ddg.save.data: name = ", name))
  if (is.null(scope)) {
    scope <- .ddg.get.scope(name, calls=stack, env=env)
  }
  
  # Determine type for value, and save accordingly.
  if (.ddg.is.graphic(value)) {
    .ddg.write.graphic(name, value, graphic.fext, scope=scope, from.env=from.env)
    return(invisible())
  }
  
  else {
    .ddg.data.node ("Data", name, value, dscope=scope, from.env=from.env)
  }
  
  invisible()
}

#' .ddg.write.graphic takes as input the name of a variable as well
#' as its value and attempts to write it out as a graphics file. If
#' all else fails, it writes out the information as a text file and
#' also writes out an RData Object which can later be read back into
#' the system.
#' @param name data node name.
#' @param value data node value.
#' @param fext file extension.
#' @param scope data node scope.
#' @param from.env If TRUE, means the value was assigned outside the script
#' @return nothing
#' @noRd

.ddg.write.graphic <- function(name, value=NULL, fext="jpeg", scope=NULL, from.env=FALSE){
  # Remember the name of the variable so that we can link to it if ggsave is 
  # called later without a plot parameter.
  .ddg.set ("ddg.last.ggplot", name)
  
  # Try to output graphic value.
  #.ddg.snapshot.node(name, "txt", value, save.object = TRUE, dscope=scope, 
  #                   from.env=from.env)
  .ddg.data.node("Data", name, value, dscope=scope, from.env=from.env)
}

#' .ddg.graphic.snapshot copies a graphics value into a snapshot file 
#' @param fext file extension.
#' @param dpfile path and name of file to copy
#' @return nothing
#' @noRd

.ddg.graphic.snapshot <-function(fext, dpfile) {
  # pdfs require a separate procedure.
  if (fext == "pdf") grDevices::dev.copy2pdf(file=dpfile)
  
  # At the moment, all other graphic types can be done by
  # constructing a similar function.
  else {
    # If jpg, we need to change it to jpeg for the function call.
    fext <- ifelse(fext == "jpg", "jpeg", fext)
    
    # First, we create a string, then convert it to an actual R
    # expression and use that as the function.
    strFun <- paste(fext, "(filename=dpfile, width=800, height=500)", sep="")
    parseFun <- 
      function(){
        eval(parse(text=strFun))
      }
    grDevices::dev.copy(parseFun)
    
    # Turn it off (this switches back to prev device).
    if (grDevices::dev.cur() != 1) { 
      grDevices::dev.off()
    }
  }
}
