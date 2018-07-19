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

#################### DDG LIBRARY FOR R ####################

# The functions in this library may be used to annotate an R script
# in order to collect provenance in the form of a data derivation
# graph (DDG) as the script executes. The DDG is saved as a JSON file
# (ddg.json) that may be viewed and queried using DDG Explorer.

# Create DDG environment variable.

.ddg.env <- new.env(parent=emptyenv())

#-------- FUNCTIONS TO MANAGE THE GLOBAL VARIABLES--------#

# Global variables cannot be used directly in a library.  Instead,
# we need to place the variables in our own environment.  These
# functions make that environment easier to use.

.onLoad <- function(libname, pkgname) {
  .ddg.init.tables()
}

#' Reinitialize the ddg
#' @return nothing
.ddg.clear <- function() {
  # reinitialize tables
  .ddg.init.tables()
}

##### Getters for specific variables

#' @return TRUE if debugging information should be saved to the file system
.ddg.save.debug <- function() {
  return(.ddg.get("ddg.save.debug"))
}

#' @return an environment containing the names bound before
#'   the script was executed
.ddg.initial.env <- function() {
  return(.ddg.get("ddg.initial.env"))
}

#' @returnType vector of strings
#' @return the names of functions that the user explicitly said should be annotated
.ddg.annotate.on <- function() {
  return (.ddg.get("ddg.annotate.on"))
}

#' @returnType vector of strings
#' @return the names of functions that user explicitly said should not be annotated
.ddg.annotate.off <- function() {
  return (.ddg.get("ddg.annotate.off"))
}

#' @return TRUE if the commands are coming from a script file
.ddg.enable.source <- function() {
  return(.ddg.is.set("from.source") && .ddg.get("from.source"))
}

#' Keeps track of whether the last loop has all iterations recorded or not.
#' @param value if TRUE, it means that not all iterations are recorded
#' @return nothing
.ddg.set.details.omitted <- function (value) {
  .ddg.set ("details.omitted", value)
}

#' @return TRUE if provenance is incomplete at this point
.ddg.were.details.omitted <- function () {
  .ddg.get ("details.omitted")
}

#' .ddg.set.warning is attached as a handler when we evaluate
#' expressions.  It saves the warning so that a warning
#' node can be created after the procedural node that
#' corresponds to the expression that caused the warning
#'
#' @param w the simplewarning object created by R
#' @return nothing
.ddg.set.warning <- function(w) {
  # Only save warnings if the warn level is set to report them at all.
  # This is important because we do temporarily set the warning level
  # to avoid warnings that RDT might cause that are safe to ignore.
  # Search for calls to the option function to see where that happens.
  if (getOption("warn") >= 0) {
    .ddg.set(".ddg.warning", w)
  }
}

#' Clear the warning 
#' @return nothing
.ddg.clear.warning <- function() {
  .ddg.set(".ddg.warning", NA)
}

#' @returnType an R warning object
#' @return the last saved warning
.ddg.get.warning <- function () {
  return (.ddg.get(".ddg.warning"))
}

#' @returnType logical
#' @return true if there is currently a saved warning
.ddg.warning.occurred <- function() {
  return (.ddg.is.set(".ddg.warning") && !is.na(.ddg.get(".ddg.warning")))
}


#-------------------BASIC FUNCTIONS-----------------------#

#' .ddg.get.initial.env creates a table of non-ddg objects present in the
#' R environment before the script is executed.  This is only used for 
#' debugging.
#' 
.ddg.get.initial.env <- function() {
  e <- globalenv()
  e.ls <- ls(e, all.names=TRUE)

  not.ddg.func <- function (name) {
    return (!grepl("ddg", name) && name != ".onLoad")
  }

  x <- Filter (not.ddg.func, e.ls)

  ddg.initial.env <- data.frame(x)
  colnames(ddg.initial.env) <- "ddg.name"

  .ddg.set("ddg.initial.env", ddg.initial.env)
}


#' .ddg.init.tables creates data frames to store the initial environment,
#' procedure nodes, data nodes, edges, and function return values. 
#' It also initializes selected constants and variables.
#' Tables are used throughout provenance collection and
#' optionally saved as tab-delimited files in ddg.save.
#' 
#' @return nothing
.ddg.init.tables <- function() {
  size <- 100

  .ddg.get.initial.env()
  
  .ddg.init.proc.nodes()
  .ddg.init.data.nodes()
  .ddg.init.edges()
  .ddg.init.function.table()
  .ddg.init.return.values()
  
  # Used to control debugging output.  If already defined, don't
  # change its value.
  if (!.ddg.is.set("ddg.debug.lib")) .ddg.set("ddg.debug.lib", FALSE)

  # Used to control sourcing. If already defined, don't change
  # its value.
  if (!.ddg.is.set("from.source")) .ddg.set("from.source", FALSE)

  # Record last command from the preceding console block.
  .ddg.set(".ddg.last.cmd", NULL)
  
  # Record the current command to be opened during console execution
  # (used when executing a script using ddg.source).
  .ddg.set(".ddg.possible.last.cmd", NULL)

  # Store path of current script.
  .ddg.set("ddg.r.script.path", NULL)

  # Store path of current ddg.
  .ddg.set("ddg.path", NULL)
  
  .ddg.init.console.vars ()

  # Functions to be annotated.
  .ddg.set("ddg.annotate.on", NULL)

  # Functions not to be annotated.
  .ddg.set("ddg.annotate.off", NULL)

  .ddg.init.sourced.scripts ()

  # Save debug files on debug directory
  .ddg.set("ddg.save.debug", FALSE)
  
  .ddg.init.statements ()
  .ddg.init.hashtable ()
  
  .ddg.set(".ddg.func.depth", 0)
  .ddg.set(".ddg.explorer.port", 6096)
  
  # Initialize the stack of commands and environments being executed in active functions
  .ddg.set(".ddg.cur.cmd.stack", vector())
  .ddg.set(".ddg.cur.expr.stack", vector())
}

#' .ddg.init.environ() sets up the filesystem and R environments
#' for use.
#' 
#' @return nothing
.ddg.init.environ <- function() {
  dir.create(.ddg.path(), showWarnings=FALSE)
  dir.create(.ddg.path.data(), showWarnings=FALSE)
  dir.create(.ddg.path.debug(), showWarnings=FALSE)
  dir.create(.ddg.path.scripts(), showWarnings=FALSE)
}

#' .ddg.is.init is called at the beginning of all user accessible
#' functions. It verifies that a DDG has been initialized. If it
#' hasn't, it returns FALSE.
#' 
#' @return true if provenance has been initialized
.ddg.is.init <- function() {
    # Short circuits evaluation.
    return(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized"))
}

<<<<<<< HEAD
#' .ddg.is.nonlocal.assign returns TRUE if the object passed is an
#' expression object containing a non-local assignment.
#' 
#' @param expr input expression.
#' @returnType logical
#' @return TRUE if the expression is an assignment statement using the <<- operator.
=======
# .ddg.format.time reformats time string. Input format is
# yyyy-mm-dd hh:mm:ss. Output format is (yyyy-mm-ddThh.mm.ss).

# time - input time string.

.ddg.format.time <- function(time) {
  formatted.time <- strftime(time, format="%Y-%m-%dT%H.%M.%S",usetz=TRUE)

  # The strftime call leaves a space between time and time
  # zone. We remove that here.
  return (sub(" ", "", formatted.time))
}

# .ddg.timestamp gets the current date and time from the system.

.ddg.timestamp <- function() {
  ts <- Sys.time()
  return (.ddg.format.time(ts))
}

.ddg.elapsed.time <- function(){
  time <- proc.time()
  elapsed <- time[1] +time[2] - .ddg.start.proc.time()
  # time[4] and time[5] are NA under Windows
  # elapsed <- time[1] +time[2] +time[4] +time[5]
  return(elapsed)
}

# .ddg.write.timestamp.to.history writes the current timestamp to
# the R history. The timestamp function does not work properly in
# Windows from within RStudio (the arguments are ignored).  In this
# case we create our own timestamp value and hope that the time
# does not change between when we set .ddg.history.timestamp and
# when the timestamp function inserts the timestamp in the history.

# var - the variable name under which the timestamp is saved.

.ddg.write.timestamp.to.history <- function(var=".ddg.history.timestamp") {
  if (Sys.getenv("RSTUDIO") != "" && Sys.info()['sysname'] == "Windows") {
    .ddg.set(var, paste("##------", date(), "------##"))
    timestamp(quiet=TRUE)
  }
  else {
    .ddg.set(var, timestamp(prefix = "##-ddg-- ", quiet=TRUE))
  }
}

# .ddg.is.graphic tries to decipher if the value snapshot should be
# written to file directly from the data or if it is a graphic which
# can be captured from the image device. This function, as written,
# is basically a hack. There must be a better way to implement it.

# value - input value.

.ddg.is.graphic <- function(value){
  # Matching any of these classes automatically classifies the
  # object as a graphic.
  graph.classes <- list("gg", "ggplot")
  return(is.object(value) && any(class(value) %in% graph.classes))
}

# .ddg.is.simple returns TRUE if the value passed in is a simple
# data value which should be saved locally as opposed to stored
# in a separate file. The assumption is that the value passed in
# has already been declared not to be a graphic.

# value - input value.

.ddg.is.simple <- function(value) {
  # Note that is.vector returns TRUE for lists, so we need to check
  # lists separately.  Since every value in a list can have a
  # different type, if it is a list, we will assume the value is
  # complex. We consider NULL values to be simple.
  return((!.ddg.is.graphic(value) &&
         !is.list(value) &&
         is.vector(value) &&
         length(value) == 1) ||
         is.null(value))
}

# .ddg.is.csv returns TRUE if the value passed in should be saved
# as a csv file, i.e. if it is a vector, matrix, or data frame.
# Note that is.vector returns TRUE for lists.

# value - input value.

.ddg.is.csv <- function(value) {
  return(!.ddg.is.simple(value) && ((is.vector(value) && !is.list(value)) || is.matrix(value) || is.data.frame(value)))
}


# .ddg.is.object returns TRUE if the value is determined to be an
# object by our standards.

# value - input value.

.ddg.is.object <- function(value){
  return(is.object(value) || is.environment(value))
}

# .ddg.is.function returns TRUE if the value is determined to be a
# function or we want to save it as a function.

# value - input value.

.ddg.is.function <- function(value){
  return(is.function(value))
}

# .ddg.dev.change determines whether or not a new graphic device
# has become active and whether or not we should capture the
# previous graphic device. It returns the device number we should
# capture (0 means we shouldn't capture any device).

.ddg.dev.change <- function(){
  prev.device <- .ddg.get("prev.device")
  curr.device <- dev.cur()
  device.list <- dev.list()

  # We've switched devices .
  if (prev.device != curr.device) {
    # Update device.
    .ddg.set("prev.device", curr.device)

    # Previous device still accessible.
    if (prev.device %in% device.list) return(prev.device)
  }

  # No switching, or previous is not accessible (NULL or removed).
  return(0)

}

# .ddg.save.simple takes in a simple name-value pair and saves
# it to the DDG. It does not however create any edges. Extra long
# strings are saved as snapshots.

# name - data node name.
# value - data node value.
# scope - data node scope.

.ddg.save.simple <- function(name, value, scope=NULL, from.env=FALSE) {
  #print(paste("In .ddg.save.simple: name =", name, "value =", value))
  #print(paste("In .ddg.save.simple: scope =", scope))
  # Save extra long strings as snapshot.
  if (is.character(value) && nchar(value) > 200) {
    #print(".ddg.save.simple: saving snapshot")
    #print(head(value))
    .ddg.snapshot.node(name, "txt", value, dscope=scope, from.env=from.env)
  } else {
    # Save the true value.
    #print(".ddg.save.simple: saving data")
    #print(paste(".ddg.save.simple: saving value", value))
    .ddg.data.node("Data", name, value, scope, from.env=from.env)
  }
}

# .ddg.write.graphic takes as input the name of a variable as well
# as its value and attempts to write it out as a graphics file. If
# all else fails, it writes out the information as a text file and
# also writes out an RData Object which can later be read back into
# the system.

# name - data node name.
# value - data node value.
# fext - file extension.
# scope - data node scope.

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

# .ddg.write.csv takes as input a name-value pair for a
# variable and attempts to save the data as a csv file. It does
# not create any edges but does add the node to the DDG. Edge
# creation should occur from wherever this function is called.

# name - data node name.
# value - data node value.
# scope - data node scope.

.ddg.write.csv <- function(name, value, scope=NULL, from.env=FALSE) {
  tryCatch({
    .ddg.snapshot.node(name, "csv", value, dscope=scope, from.env=from.env)
  }, error = function(e) {
    # warning(paste("Attempted to write", name, "as .csv snapshot but failed. Out as RDataObject.", e))
    .ddg.snapshot.node(name, "txt", value, save.object = TRUE, dscope=scope, from.env=from.env)
  })
}

# .ddg.save.data takes as input the name and value of a data node
# that needs to be created. It determines how the data should be
# output (or saved) and saves it in that format.

# name - name of created node.
# value - value of created node.
# from.env - if node is from initial environment
# fname (optional) - name of calling function. Used to generate
#   helpful error messages if something goes wrong.
# graphic.fext (optional) - file extension for graphic file.
# error (optional) - if TRUE, raise an R error rather than a
#   DDG error.
# scope (optional) - scope of node.
# stack (optional) - stack to use in determing scope.

.ddg.save.data <- function(name, value, fname=".ddg.save.data", graphic.fext='jpeg', error=FALSE, scope=NULL, from.env=FALSE, stack=NULL, env=NULL){
  #print (paste (".ddg.save.data: looking for name =", name, "with scope", scope))
  #print(paste(".ddg.save.data saving ", name, "with value structured as", str(value)))
  #if (is.null(value)) print(".ddg.save.data: value is null")
  if (is.null(scope)) {
    scope <- .ddg.get.scope(name, calls=stack, env=env)
  }

  #print (paste (".ddg.save.data: saving", name, "in scope", scope))
  # Determine type for value, and save accordingly.
  if (.ddg.is.graphic(value)) .ddg.write.graphic(name, value, graphic.fext, scope=scope, from.env=from.env)
  else if (.ddg.is.simple(value)) .ddg.save.simple(name, value, scope=scope, from.env=from.env)
  else if (.ddg.is.csv(value)) .ddg.write.csv(name, value, scope=scope, from.env=from.env)
  else if (is.list(value) || is.array(value)) .ddg.snapshot.node(name, "txt", value, save.object=TRUE, dscope=scope, from.env=from.env)
  else if (.ddg.is.connection(value)) {.ddg.save.simple(name, value, scope=scope, from.env=from.env)}
  else if (.ddg.is.object(value)) {.ddg.snapshot.node(name, "txt", value, dscope=scope, from.env=from.env) }
  else if (.ddg.is.function(value)) .ddg.save.simple(name, "#ddg.function", scope=scope, from.env=from.env)
  else if (error) stop("Unable to create data (snapshot) node. Non-Object value to", fname, ".")
  else {
    error.msg <- paste("Unable to create data (snapshot) node. Non-Object value to", fname, ".")
    .ddg.insert.error.message(error.msg)
  }
  #print(".ddg.save.data: Done saving data")
  invisible()
}

# .ddg.record.proc records a procedure node in the procedure node
# table.
# 
# ptype - procedure node type.
# pname - procedure node name.
# pvalue - procedure node value.
# auto.created - TRUE means the node is being created automatically
#   when a return is found
# ptime - elapsed time
# pfunctions - functions called in the procedure
# snum - number of sourced script (main script = 0)
# pos - starting and ending lines and columns in source code (if available)

.ddg.record.proc <- function(ptype, pname, pvalue, auto.created=FALSE, ptime, pfunctions=NULL, snum=NA, pos=NA) {
  # Increment procedure node counter.
  .ddg.inc("ddg.pnum")
  ddg.pnum <- .ddg.pnum()

  # If the table is full, make it bigger.
  ddg.proc.nodes <- .ddg.proc.nodes()
  if (nrow(ddg.proc.nodes) < ddg.pnum) {
    size = 100
    new.rows <- data.frame(ddg.type = character(size),
        ddg.num = numeric(size),
        ddg.name = character(size),
        ddg.value = character(size),
        ddg.return.linked = logical(size),
        ddg.auto.created = logical(size),
        ddg.time = numeric(size),
        ddg.snum = numeric(size),
        ddg.startLine = numeric(size),
        ddg.startCol = numeric(size),
        ddg.endLine = numeric(size),
        ddg.endCol = numeric(size),
        stringsAsFactors=FALSE)
    .ddg.add.rows("ddg.proc.nodes", new.rows)
    ddg.proc.nodes <- .ddg.proc.nodes()
  }

  ddg.proc.nodes$ddg.type[ddg.pnum] <- ptype
  ddg.proc.nodes$ddg.num[ddg.pnum] <- ddg.pnum
  ddg.proc.nodes$ddg.name[ddg.pnum] <- pname
  ddg.proc.nodes$ddg.value[ddg.pnum] <- pvalue
  ddg.proc.nodes$ddg.auto.created[ddg.pnum] <- auto.created
  ddg.proc.nodes$ddg.time[ddg.pnum] <- ptime

  ddg.proc.nodes$ddg.snum[ddg.pnum] <- snum
  if (is.object(pos) && length(pos@startLine == 1)) {
    ddg.proc.nodes$ddg.startLine[ddg.pnum] <- pos@startLine
    ddg.proc.nodes$ddg.startCol[ddg.pnum] <- pos@startCol
    ddg.proc.nodes$ddg.endLine[ddg.pnum] <- pos@endLine
    ddg.proc.nodes$ddg.endCol[ddg.pnum] <- pos@endCol
  }
  else {
    ddg.proc.nodes$ddg.startLine[ddg.pnum] <- NA
    ddg.proc.nodes$ddg.startCol[ddg.pnum] <- NA
    ddg.proc.nodes$ddg.endLine[ddg.pnum] <- NA
    ddg.proc.nodes$ddg.endCol[ddg.pnum] <- NA
  }
  
  .ddg.set("ddg.proc.nodes", ddg.proc.nodes)
  
  # append to function call information to function nodes
  if( ! (is.null(pfunctions) || is.na(pfunctions)) )
  {
    pfunctions <- cbind( "ddg.pnum" = rep(ddg.pnum, nrow(pfunctions)) , pfunctions )
    
    ddg.function.nodes <- rbind( .ddg.function.nodes() , pfunctions )
    row.names(ddg.function.nodes) <- c( 1 : nrow(ddg.function.nodes) )
    
    .ddg.set( "ddg.function.nodes" , ddg.function.nodes )
  }
  
  if (.ddg.debug.lib()) {
    print (paste("Adding procedure node", ddg.pnum, "named", pname))
  }
}

# .ddg.record.data records a data node in the data node table.

# dtype - data node type.
# dname - data node name.
# dvalue - data node value.
# value - the value of the data
# dscope - data node scope.
# from.env - if object is from initial environment.
# dhash - the hash of original file.
# drw - whether the file was read or written.
# dtime (optional) - timestamp of original file.
# dloc (optional) -  path and name of original file.

.ddg.record.data <- function(dtype, dname, dvalue, value, dscope, from.env=FALSE, dtime="", dloc="") {
  #print("In .ddg.record.data")
  #print(paste("dvalue =", head(dvalue)))
  #print(paste("value =", head(value)))
  #print (sys.calls())
  # Increment data node counter.
  .ddg.inc("ddg.dnum")
  ddg.dnum <- .ddg.dnum()

  #Initialize dscriptpath
  if (!is.null(.ddg.get("ddg.r.script.path"))) {
    dscriptpath <- .ddg.get("ddg.r.script.path")
  }
  else {
    dscriptpath <- ""
  }

  # If the table is full, make it bigger.
  ddg.data.nodes <- .ddg.data.nodes()
  if (nrow(ddg.data.nodes) < ddg.dnum) {
    size = 100
    new.rows <- data.frame(ddg.type = character(size),
        ddg.num = numeric(size),
        ddg.name = character(size),
        ddg.path = character(size),
        ddg.value = character(size),
        ddg.val.type = character(size),
        ddg.scope = character(size),
        ddg.from.env = logical(size),
        ddg.time = character(size),
        ddg.hash = character(size),
        ddg.rw = character(size),
        ddg.loc = character(size),
        ddg.current = logical(size), stringsAsFactors=FALSE)
    .ddg.add.rows("ddg.data.nodes", new.rows)
    ddg.data.nodes <- .ddg.data.nodes()
  }

  if (length(dvalue) > 1 || !is.atomic(dvalue)) dvalue2 <- "complex"
  else if (!is.null(dvalue)) dvalue2 <- dvalue
  else dvalue2 <- ""

  # get value type
  val.type <- .ddg.get.val.type.string(value)

  #print(".ddg.record.data: adding info")
  ddg.data.nodes$ddg.type[ddg.dnum] <- dtype
  ddg.data.nodes$ddg.num[ddg.dnum] <- ddg.dnum
  ddg.data.nodes$ddg.path[ddg.dnum] <- dscriptpath
  ddg.data.nodes$ddg.name[ddg.dnum] <- dname
  ddg.data.nodes$ddg.value[ddg.dnum] <- dvalue2
  ddg.data.nodes$ddg.val.type[ddg.dnum] <- val.type
  ddg.data.nodes$ddg.scope[ddg.dnum] <- dscope
  ddg.data.nodes$ddg.from.env[ddg.dnum] <- from.env
  ddg.data.nodes$ddg.hash[ddg.dnum] <- ""
  ddg.data.nodes$ddg.rw[ddg.dnum] <- ""
  ddg.data.nodes$ddg.time[ddg.dnum] <- dtime
  ddg.data.nodes$ddg.loc[ddg.dnum] <- dloc

  ddg.data.nodes$ddg.current[ddg.dnum] <- TRUE
  .ddg.set("ddg.data.nodes", ddg.data.nodes)

  # Output data node.
  #print(".ddg.record.data outputting data node")
  if (dtype == "File") {
    ddg.data.nodes <- .ddg.add.to.hashtable(dname = dname, ddg.dnum = ddg.dnum, dscriptpath = dscriptpath, dloc = dloc, dvalue = dvalue, dtime = dtime)
  }

  if (.ddg.debug.lib()) {
    if (dtype != "File") {
      print(paste("Adding data node", ddg.dnum, "named", dname, "with scope", dscope, " and value ", ddg.data.nodes$ddg.value[ddg.dnum]))
    } else {
      print(paste("Adding data node", ddg.dnum, "named", dname, "with scope", dscope, " and value ", ddg.data.nodes$ddg.value[ddg.dnum], 
                  " that hashes to ", ddg.data.nodes$ddg.hash[ddg.dnum], " and performs a file ", ddg.data.nodes$ddg.rw[ddg.dnum]))
    }
  }
}

# Returns a string representation of the type information of the given value.
.ddg.get.val.type.string <- function(value)
{
  val.type <- .ddg.get.val.type(value)

  if( is.null(val.type) )
  	return( 'null' )

  # list, object, environment, function, language
  if( length(val.type) == 1 )
  	return( paste('"',val.type,'"',sep="") )

  # vector, matrix, array, data frame
  # type information recorded in a list of 3 vectors (container,dimension,type)
  container <- val.type[[1]]
  dimension <- val.type[[2]]
  type <- val.type[[3]]

  # vector: a 1-dimensional array (uniform typing)
  if( identical(container,"vector") )
    return( paste('{"container":"vector", "dimension":[', dimension, '], "type":["' , type, '"]}', sep = "") )

  # matrix: a 2-dimensional array (uniform typing)
  if( identical(container,"matrix") )
  {
	dimension <- paste( dimension , collapse = "," )
	return( paste('{"container":"matrix", "dimension":[', dimension, '], "type":["' , type, '"]}', sep = "") )
  }

  # array: n-dimensional (uniform typing)
  if( identical(container,"array") )
  {
	dimension <- paste( dimension , collapse = "," )
	return( paste('{"container":"array", "dimension":[', dimension, '], "type":["' , type, '"]}', sep = "") )
  }

  # data frame: is a type of list
  dimension <- paste( dimension , collapse = "," )
  type <- paste( type , collapse = '","' )

  return( paste('{"container":"data_frame", "dimension":[', dimension, '], "type":["' , type, '"]}', sep = "") )
}


# Returns the type information of the value of the given variable.
# Does not contain information on dimensions.

.ddg.get.val.type.from.var <- function(var)
{
  val.type <- .ddg.get.val.type( get(var) )

  # remove dimension information, if any
  if( length(val.type) > 1 )
  	val.type[2] <- NULL

  return( val.type )
}


# Returns the type information of the given value,
# broken into its parts and returned in a vecctor or a list.

.ddg.get.val.type <- function(value)
{
	# vector: a 1-dimensional array (uniform typing)
	if(is.vector(value))
		return( list("vector", length(value), .ddg.get.lowest.class(value)) )

	# matrix: a 2-dimensional array (uniform typing)
	if(is.matrix(value))
		return( list("matrix", dim(value), .ddg.get.lowest.class(value[1])) )

	# array: n-dimensional (uniform typing)
	if(is.array(value))
		return( list("array", dim(value), .ddg.get.lowest.class(value[1])) )

	# data frame: is a type of list
	if(is.data.frame(value))
	{
		types <- unname(sapply(value,.ddg.get.lowest.class))
		return( unname(list("data_frame", dim(value), types)) )
	}

	# a list
	if(is.list(value))
		return("list")

	# an object
	if(is.object(value))
		return("object")

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

# returns the first element that the function class returns
# when inheritance is used, this is the lowest type. 
.ddg.get.lowest.class <- function( obj )
{
	return( class(obj)[1] )
}


# .ddg.record.edge records a control flow edge or a data flow edge
# in the edges table.

# etype - type of edge
# node1 - name of first node
# node1 - name of second node

.ddg.record.edge <- function(etype, node1, node2) {
  # Increment edge counter.
  .ddg.inc("ddg.enum")
  ddg.enum <- .ddg.enum()

  # If the table is full, make it bigger.
  ddg.edges <- .ddg.edges()
  if (nrow(ddg.edges) < ddg.enum) {
    size = 100
    new.rows <- data.frame(ddg.num = numeric(size),
        ddg.type = character(size),
        ddg.from = character(size),
        ddg.to = character(size),
        stringsAsFactors=FALSE)
    .ddg.add.rows("ddg.edges", new.rows)
    ddg.edges <- .ddg.edges()
  }

  ddg.edges$ddg.num[ddg.enum] <- ddg.enum
  ddg.edges$ddg.type[ddg.enum] <- etype
  ddg.edges$ddg.from[ddg.enum] <- node1
  ddg.edges$ddg.to[ddg.enum] <- node2
  .ddg.set("ddg.edges", ddg.edges)

  if (.ddg.debug.lib()) {
    if (etype == "cf") etype.long <- "control flow"
    else if (etype == "df.in") etype.long <-"data flow in"
    else etype.long <- "data flow out"
    print (paste("Adding", etype.long, "edge", ddg.enum, "for", node1, "to", node2))
  }
}

# .ddg.is.proc.node returns TRUE if the specified type supports
# input and output edges in an expanded DDG. Currently this
# includes all procedure node types except Start.

# type - procedure node type.

.ddg.is.proc.node <- function(type) {
  return(type == "Operation" |
          type == "Checkpoint" |
          type == "Restore" |
          type == "Start" |
          type == "Finish" |
          type == "Binding")
}

# .ddg.proc.node.exists returns true if there is a
# procedure node with the given name

.ddg.proc.node.exists <- function(pname) {
  ddg.proc.nodes <- .ddg.proc.nodes()
  rows <- nrow(ddg.proc.nodes)
  for (i in rows:1) {
    type <- ddg.proc.nodes$ddg.type[i]
    if (.ddg.is.proc.node(type) & ddg.proc.nodes$ddg.name[i] == pname & !ddg.proc.nodes$ddg.return.linked[i] & !ddg.proc.nodes$ddg.auto.created[i]) {
      return(TRUE)
    }
  }

  return(FALSE)
}

# .ddg.proc.number gets the number of the nearest preceding
# matching Operation, Checkpoint, or Restore node. It returns
# zero if no match is found.

# pname - name of procedure node.
# find.unreturned.function - if true, only return the number if the
#    procedure has not previously been linked to a return value

.ddg.proc.number <- function(pname, find.unreturned.function=FALSE) {
  #print (paste0("Looking for function ", pname))
  ddg.proc.nodes <- .ddg.proc.nodes()
  rows <- nrow(ddg.proc.nodes)
  for (i in rows:1) {
    type <- ddg.proc.nodes$ddg.type[i]
    if (.ddg.is.proc.node(type) & ddg.proc.nodes$ddg.name[i] == pname) {
      #print (paste0("Found a matching function for ", pname))
      if (!find.unreturned.function) {
        #print (paste0("Returning ", ddg.proc.nodes$ddg.num[i]))
        return(ddg.proc.nodes$ddg.num[i])
      }

      if (find.unreturned.function & !ddg.proc.nodes$ddg.return.linked[i]) {
        #print (paste0("Returning ", ddg.proc.nodes$ddg.num[i]))
        return(ddg.proc.nodes$ddg.num[i])
      }
    }
  }

  # Error message if no match is found.
  #print ("Returning error!")
  error.msg <- paste("No procedure node found for", pname)
  if (.ddg.debug.lib()) print (sys.calls())
  .ddg.insert.error.message(error.msg)
  return(0)
}

# .ddg.last.proc.number returns the node number of the last
# procedure node in the ddg procedure node table. Procedure nodes
# are determined as defined in .ddg.is.proc.node above.

.ddg.last.proc.number <- function() {
  ddg.proc.nodes <- .ddg.proc.nodes()
  rows <- nrow(ddg.proc.nodes)
  for (i in rows:1) {
    type <- ddg.proc.nodes$ddg.type[i]
    if (.ddg.is.proc.node(type)) return(i)
  }

  error.msg <- paste("No final procedure nodes")
  .ddg.insert.error.message(error.msg)
  return(0)
}

# .ddg.data.node.exists searches the data node table for a matching
# data node and returns TRUE if a match is found. Otherwise it searches
# the initial environment table and, if a match is found, creates a
# data node and returns TRUE. Otherwise it returns FALSE.

# dname - data node name.
# dscope - data node scope.

.ddg.data.node.exists <- function(dname, dscope=NULL) {
  if (is.null(dscope)) dscope <- .ddg.get.scope(dname)

  # Search data nodes table.
  #print (paste (".ddg.data.node.exists: Looking for", dname, "in scope", dscope))
  ddg.data.nodes <- .ddg.data.nodes()
  rows <- nrow(ddg.data.nodes)
  for (i in rows:1) {
    if (ddg.data.nodes$ddg.current[i]) {
      if (ddg.data.nodes$ddg.name[i] == dname) {
        #print(paste(".ddg.data.node.exist:  found node with name", dname, "in scope", ddg.data.nodes$ddg.scope[i]))
        if (ddg.data.nodes$ddg.scope[i] == "ddg.library" || ddg.data.nodes$ddg.scope[i] == dscope) {
          #print(".ddg.data.node.exists found")
          return (TRUE)
        }
      }
    }
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

# .ddg.data.number retrieves the number of the nearest preceding
# current matching data node. It returns zero if no match is found.

# dname - data node name.
# dscope (optional) - data node scope.

.ddg.data.number <- function(dname, dscope=NULL) {
  if (is.null(dscope)) dscope <- .ddg.get.scope(dname)
  ddg.data.nodes <- .ddg.data.nodes()
  rows <- nrow(ddg.data.nodes)
  for (i in rows:1) {
    if (ddg.data.nodes$ddg.current[i]) {
      if (ddg.data.nodes$ddg.name[i] == dname) {
        if (ddg.data.nodes$ddg.scope[i] == "ddg.library" || ddg.data.nodes$ddg.scope[i] == dscope) return (ddg.data.nodes$ddg.num[i])
      }
    }
  }

  # Error message if no match found.
  error.msg <- paste("No data node found for", dname)
  .ddg.insert.error.message(error.msg)
  return(0)
}

# .ddg.proc.name returns the name of a procedure node. It returns a
# empty string if no match is found.

# pnum - node number in procedure node table.

.ddg.proc.name <- function(pnum) {
  if (pnum < 1 || pnum > .ddg.pnum()) {
    error.msg <- paste("No name found for procedure number", pnum)
    .ddg.insert.error.message(error.msg)
    return ("")
  }

  return(.ddg.proc.nodes()$ddg.name[pnum])
}

# .ddg.proc2proc creates a control flow edge from the preceding
# procedure node to the current procedure node.

.ddg.proc2proc <- function() {
  ddg.pnum <- .ddg.pnum()

  if (ddg.pnum > 1) {
    # Record in edges table
    etype <- "cf"
    node1 <- paste("p", ddg.pnum-1, sep="")
    node2 <- paste("p", ddg.pnum, sep="")
    .ddg.record.edge(etype, node1, node2)

    if (.ddg.debug.lib()) {
      pname1 <- .ddg.proc.name(ddg.pnum-1)
      pname2 <- .ddg.proc.name(ddg.pnum)
      print(paste("proc2proc: ", pname1, " ", pname2))
      print(paste("CF ", node1, " ", node2, sep=""))
    }
  }

  invisible()
}

# .ddg.data2proc creates a data flow edge from a data node to a
# procedure node.

# dname - data node name.
# dscope - data node scope.
# pname - procedure node name.

.ddg.data2proc <- function(dname, dscope, pname = NULL) {
  # Get data & procedure numbers.
  dn <- .ddg.data.number(dname, dscope)
  
  if(is.null(pname) || startsWith(pname,".ddg.") || startsWith(pname,"ddg"))
    pn <- .ddg.last.proc.number()
  else
    pn <- .ddg.proc.number(pname)
  
  # Record in edges table
  etype <- "df.in"
  node1 <- paste("d", dn, sep="")
  node2 <- paste("p", pn, sep="")
  .ddg.record.edge(etype, node1, node2)
  
  if (.ddg.debug.lib()) {
    print(paste("data2proc: ", dname, " ", pname, sep=""))
    print(paste("DF ", node1, " ", node2, sep=""))
  }
  
  invisible()
}

# .ddg.proc2data creates a data flow edge from a procedure node to
# a data node.

# pname - procedure node name.
# dname - data node name.
# dscope (optional) - data node scope.
# return.value (optional) - if true it means we are linking to a return
# value. In this case, we need to be sure that there is not already a
# return value linked.  This is necessary to manage recursive functions
# correctly.

.ddg.proc2data <- function(pname, dname, dscope=NULL, return.value=FALSE) {
  # Get data & procedure numbers.
  #print (paste(".ddg.proc2data: Looking for", dname, "in scope", dscope))
  dn <- .ddg.data.number(dname, dscope)
  
  #print (paste(".ddg.proc2data: Found node", dn))
  
  # attach data node to the last procedure node if pname is NULL.
  if(is.null(pname) || startsWith(pname,".ddg.") || startsWith(pname,"ddg"))
    pn <- .ddg.last.proc.number()
  else
    pn <- .ddg.proc.number(pname, return.value)
  
  # Create data flow edge from procedure node to data node.
  if (dn != 0 && pn != 0) {

    # Record in edges table
    etype <- "df.out"
    node1 <- paste("p", pn, sep="")
    node2 <- paste("d", dn, sep="")
    .ddg.record.edge(etype, node1, node2)

    # Record that the function is linked to a return value.  This
    # is necessary for recursive functions to get linked to their
    # return values correctly.
    if (return.value) {
      ddg.proc.nodes <- .ddg.proc.nodes()
      #print ("Marking return value as being used")
      ddg.proc.nodes$ddg.return.linked[pn] <- TRUE
      .ddg.set("ddg.proc.nodes", ddg.proc.nodes)
    }

    if (.ddg.debug.lib()) {
      print(paste("proc2data: ", pname, " ", dname, sep=""))
      print(paste("DF ", node1, " ", node2, sep=""))
    }
  }

  invisible()
}

# .ddg.lastproc2data creates a data flow edge from the last
# procedure node to a data node.

# dname - data node name.
# all (optional) - whether all nodes should be considered (TRUE)
#   or only procedure nodes (FALSE).
# dscope - the scope in which dname should be looked up

.ddg.lastproc2data <- function(dname, all=TRUE, dscope=NULL) {
  # Get data & procedure numbers.
  dn <- .ddg.data.number(dname, dscope)
  pn <- if(all) .ddg.pnum() else .ddg.last.proc.number()

  # Record in edges table
  etype <- "df.out"
  node1 <- paste("p", pn, sep="")
  node2 <- paste("d", dn, sep="")
  .ddg.record.edge(etype, node1, node2)

  if (.ddg.debug.lib()) {
    print(paste("lastproc2data:", dname))
    print(paste("DF ", node1, " ", node2, sep=""))
  }
}

# .ddg.is.nonlocal.assign returns TRUE if the object passed is an
# expression object containing a non-local assignment.

# expr - input expression.

.ddg.is.nonlocal.assign <- function (expr)
{
  # <<- or ->> means that the assignment is non-local
  if (is.call(expr))
  {
    # This also finds uses of ->>.
    if (identical(expr[[1]], as.name("<<-")))
      return (TRUE)
  }
  return (FALSE)
}

#' .ddg.create.empty.vars.set creates an empty data frame
#' initialized to contain information about variable assignments.
#' The difference between first.writer and possible.first.writer is
#' that first.writer is for simple assignments (like a <- 1), while
#' possible.first.writer is for situations where the assignment might
#' not have occurred, like "if (foo) a <- 1".
#' 
#' @returnType The data frame is structured as follows: \cr
#' - the variable name.\cr
#' - the position of the statement that wrote the variable first.\cr
#' - the position of the statement that wrote the variable last.\cr
#' - the position of the first statement that may have assigned to a
#'   variable .\cr
#' - the position of the last statement that may have assigned to a
#'   variable.\cr
#' 
#' @param var.table.size desired size of the data frame. Negative values
#'   and 0 are coerced to 1.
#' 
#' @return the data frame constructed
.ddg.create.empty.vars.set <- function(var.table.size=1) {

  if (var.table.size <= 0) var.table.size <- 1

  vars.set <- data.frame(variable=character(var.table.size),
      first.writer=numeric(var.table.size),
      last.writer=numeric(var.table.size),
      possible.first.writer=numeric(var.table.size),
      possible.last.writer=numeric(var.table.size),
      stringsAsFactors=FALSE)

  # Initialize first writer.
  vars.set$first.writer <- var.table.size + 1
  vars.set$possible.first.writer <- var.table.size + 1

  return(vars.set)
}

#'.ddg.double.vars.set doubles the size of a variable
#' assignment data frame and returns the new one.
#' 
#' @param vars.set data frame containing variable assignments.
#' @returnType same data frame type as the parameter
#' @return a data frame that is twice the size as the original
.ddg.double.vars.set <- function(vars.set) {
  size=nrow(vars.set)
  
  # Create the right size data frame from input frame.
  new.vars.set <- rbind(vars.set,.ddg.create.empty.vars.set(size))

  # Update first/last writer.
  new.vars.set$first.writer <- 
      ifelse(new.vars.set$first.writer == size + 1, 
             size*2 + 1, 
             new.vars.set$first.writer)
  new.vars.set$possible.first.writer <- 
      ifelse(new.vars.set$possible.first.writer == size + 1, 
             size*2 + 1, 
             new.vars.set$possible.first.writer)

  return(new.vars.set)
}

#' .ddg.add.to.vars.set adds the variables set in the command
#' to the variable assignment data frame. Note that
#' var.num is a global variable! It should be intialized when
#' vars.set is first created.
#' 
#' @param vars.set variable assignment data frame.
#' @param cmd a DDGStatement object
#' @param i position of command in the list of commands
#' @returnType the same data frame type as vars.set
#' @return an updated vars.set data frame with the information from the command
.ddg.add.to.vars.set <- function(vars.set, cmd, i) {
  #print("In .ddg.add.to.vars.set")

  # Find out the variable being assigned to by a simple assignment
  # statement.
  main.var.assigned <- cmd@vars.set

  # Find all the variables that may be assigned in the statement.
  vars.assigned <- cmd@vars.possibly.set

  for (var in vars.assigned) {
    nRow <- which(vars.set$variable == var)

    # If the variable is already in the table, update its entry.
    if (length(nRow) > 0) {
      if (!is.null(main.var.assigned) && var == main.var.assigned) {
        vars.set$last.writer[nRow] <- i
      }
      else {
        vars.set$possible.last.writer[nRow] <- i
      }
    }

    # The variable was not in the table. Add a new line for this
    # variable.
    else {
      # Find the first empty row
      empty.rows <- which(vars.set$variable == "")
      if (length(empty.rows) == 0) {
        vars.set <- .ddg.double.vars.set(vars.set)
        empty.rows <- which(vars.set$variable == "")
      }
      var.num <- empty.rows[1]

      # Set the variable.
      vars.set$variable[var.num] <- var
      if (!is.null(main.var.assigned) && var == main.var.assigned) {
        vars.set$first.writer[var.num] <- i
        vars.set$last.writer[var.num] <- i
      }
      else {
        vars.set$possible.first.writer[var.num] <- i
        vars.set$possible.last.writer[var.num] <- i
      }
    }
  }

  #print (".ddg.add.to.vars.set: returning")
  #print(vars.set)
  return(vars.set)
}


#' .ddg.find.var.assigments finds the possible variable assignments
#' for a fixed set of parsed commands. 
#'
#' @param cmds a list of DDGStatement objects
#' @returnType a data frame as described in .ddg.create.empty.vars.set
#' @return the data frame filled in with the information from all of the commands
.ddg.find.var.assignments <- function(cmds) {
  if (length(cmds) == 0) return (data.frame())

  # Make it big so we don't run out of space.
  var.table.size <- length(cmds)
  vars.set <- .ddg.create.empty.vars.set(var.table.size)

  # Build the table recording where variables are assigned to or may
  # be assigned to.
  for ( i in 1:length(cmds)) {
    #print(paste("Looking for var assignments in", cmd.expr@abbrev))
    vars.set <- .ddg.add.to.vars.set(vars.set, cmds[[i]], i)
  }
  return (vars.set)
}


#' .ddg.create.data.use.edges.for.console.cmd creates a data flow
#' edge from the node for each variable used in cmd to the
#' procedural node labeled cmd, as long as the value would either
#' be one that exists prior to starting the console block, or
#' corresponds to the last setting of this variable in the console
#' block.
#' 
#' @param vars.set variable assignment data frame.
#' @param cmd name of procedure node.
#' @param cmd.pos - position of command.
#' @param for.caller whether the search for the variable's scope should start at the 
#'   current stack frame, or start with its caller
#' @return nothing
.ddg.create.data.use.edges.for.console.cmd <- function (vars.set, cmd, cmd.pos, for.caller) {
  # Find all the variables used in this command.
  #print (paste(".ddg.create.data.use.edges.for.console.cmd: cmd = ", cmd@text))
  vars.used <- cmd@vars.used

  for (var in vars.used) {
    #print(paste(".ddg.create.data.use.edges.for.console.cmd: var =", var))
    # Make sure there is a node we could connect to.
    scope <- .ddg.get.scope(var, for.caller)

    #print(paste(".ddg.create.data.use.edges.for.console.cmd: scope =", scope))

    if (.ddg.data.node.exists(var, scope)) {
      # print(".ddg.create.data.use.edges.for.console.cmd found data node")
      nRow <- which(vars.set$variable == var)

      # Check if the node is written in the console block.
      if (length(nRow) > 0) {
        first.writer <- min(vars.set$first.writer[nRow], vars.set$possible.first.writer[nRow])
        last.writer <- max(vars.set$last.writer[nRow], vars.set$possible.last.writer[nRow])
        
        # Draw the edge if we will connect to a node that exists
        # before the console block or to the last writer of this
        # variable within the console block.

        if (cmd.pos <= first.writer || cmd.pos >= last.writer) {
        # Note: the following line leads to the self-referencing
        # node problem.
        # if (cmd.pos <= first.writer || cmd.pos > last.writer) {
          .ddg.data2proc(var, scope, cmd@abbrev)
        }

        # TODO - add some sort of warning to the user that the node
        # is not being created
      }

      # The variable is not set at all in this console block.
      # Connect to a pre-existing data node.
      else {
        .ddg.data2proc(var, scope, cmd@abbrev)
      }
    }
    else {
      # TODO - add some sort of warning that the data node was NOT
      # found.

      # error.msg <- paste("Unable to find data node for",var, ". Command", parse(text=cmd.expr), "appears to use it for procedure node", cmd, ".")
      # .ddg.insert.error.message(error.msg)
    }
  }
  #print (".ddg.create.data.use.edges.for.console.cmd Done")
}


#' .ddg.create.data.set.edges.for.cmd creates data nodes for 
#' variables being set, saves the data, and creates edges from the
#' procedure node that set the variable to the new data node.  These
#' nodes and edges correspond to the variables set in the command passed in.
#' 
#' @param vars.set variable assignment data frame.
#' @param cmd the command to create edges for
#' @param cmd.pos position of command in the list of commands
#' @param env environment to use for evaluating variables set.
#' @return nothing
.ddg.create.data.set.edges.for.cmd <- function(vars.set, cmd, cmd.pos, env) {
  # print(paste("In .ddg.create.data.set.edges.for.cmd: cmd = ", cmd@abbrev))
  vars.assigned <- cmd@vars.set

  for (var in vars.assigned) {

    #print(paste(".ddg.create.data.set.edges.for.cmd: var = ", var))
    whichRows <- which(vars.set$variable == var)

    # Only create a node edge for the last place that a variable is
    # set within a console block.
    if ((length(whichRows) > 0 && vars.set$last.writer[whichRows] == cmd.pos && 
          vars.set$possible.last.writer[whichRows] <= vars.set$last.writer[whichRows])) {
        if (is.null(env)) {
          env <- .ddg.get.env(var)
        }
        scope <- .ddg.get.scope(var, env=env)

        # Special operators are defined by enclosing the name in `.  However,
        # the R parser drops those characters when we deparse, so when we parse
        # here they are missing and we get an error about unexpected SPECIAL
        # characters.  The first tryCatch, puts the ` back in and parses again.
        # The second tryCatch handles errors associated with evaluating the variable.
        parsed <- tryCatch(parse(text=var),
            error = function(e) parse(text=paste("`",var,"`",sep="")))
        val <- tryCatch(eval(parsed, env),
          error = function(e) {
            eval (parse(text=var), parent.env(env))
          }
        )

        tryCatch(.ddg.save.data(var, val, error=TRUE, scope=scope, env=env),
               error = function(e){.ddg.data.node("Data", var, "complex", scope); print(e)})

        .ddg.proc2data(cmd@abbrev, var, scope)
    }
  }

}


#' .ddg.create.data.node.for.possible.writes creates a data node for
#' each variable that might have been set in something other than a
#' simple assignment.  An edge is created from the last node in the
#' console block.
#' 
#' @param vars.set variable assignment data frame.
#' @param last.command last command in console block.
#' @param env the environment that the command was executed in
#' @return nothing
#' 
.ddg.create.data.node.for.possible.writes <- function (vars.set, last.command, env= NULL) {
  #print("In .ddg.create.data.node.for.possible.writes")
  environment <- if (is.environment(env)) env else .GlobalEnv

  for (i in 1:nrow(vars.set)) {
    # print(paste("Checking ", vars.set$variable[i]))
    if (vars.set$possible.last.writer[i] > vars.set$last.writer[i]) {
      value <- tryCatch(eval(parse(text=vars.set$variable[i]), environment),
          error = function(e) {
            #print(paste("Could not find value for", vars.set$variable[i], "in environment", environment))
            NULL
          }
      )

      # Only create the node and edge if we were successful in
      # looking up the value.
      if (!is.null(value)) {
        envName <- environmentName(environment)
        if (envName == "") envName <- .ddg.get.scope(vars.set$variable[i])
        .ddg.data.node("Data", vars.set$variable[i], value, envName)
        .ddg.proc2data(last.command@abbrev, vars.set$variable[i], envName)
      }
    }
  }
  #print("Done with .ddg.create.data.node.for.possible.writes")

}

#' .ddg.link.function.returns determines if the command calls a
#' function for which ddg.return has created a node for the return
#' value.  If so, a data flow edge is created from the return value
#' data node to the finish node for the command.  Note that if the
#' assignment is an expression, like "d <- f(a) + f(b)", there may
#' be multiple return value nodes to link to.
#' 
#' @param command input command.
#' @return nothing
.ddg.link.function.returns <- function(command) {
  
  return.value.nodes <- .ddg.get.matching.return.value.nodes (command)
  #print (paste (".ddg.link.function.returns: new.uses:", new.uses))
  
  # Create an edge from each of these to the last procedure node.
  lapply (return.value.nodes, function (data.num) {
        .ddg.datanum2lastproc (data.num)
  
        # Set the return value as being used.
        .ddg.set.return.value.used (data.num)
      })
  

  #print ("Returning from .ddg.link.function.returns")
}

#' Creates a start node and its incoming control flow edge.  
#' @param cmd The DDGStatement object for the command being started
#' @param node.name The label to put on the node.  If node.name is not passed in,
#'   the abbreviated label in cmd is used.
#' @returnType string
#' @return the label of the node created, excluding "Start"
.ddg.add.start.node <- function(cmd = NULL, node.name = "") {
  return (.ddg.add.abstract.node ("Start", cmd, node.name))
}
  
#' Creates a finish node and its incoming control flow edge.  
#' @param cmd The DDGStatement object for the command being finished
#' @param node.name The label to put on the node.  If node.name is not passed in,
#'   the abbreviated label in cmd is used.
#' @returnType string
#' @return the label of the node created, excluding "Finish"
.ddg.add.finish.node <- function(cmd = NULL, node.name = "") {
  return (.ddg.add.abstract.node ("Finish", cmd, node.name))
}

#' Creates a start or finish node and its incoming control flow edge.
#' 
#' @param cmd The DDGStatement object for the command being finished
#' @param node.name The label to put on the node.  If node.name is not passed in,
#'   the abbreviated label in cmd is used.
#' @returnType string
#' @return the label of the node created, excluding "Start" or "Finish"
.ddg.add.abstract.node <- function(type, cmd = NULL, node.name = "") {
  #print("In .ddg.add.abstract.node")
  if (node.name == "") {
      node.name <- cmd@abbrev
  }
  if (.ddg.debug.lib()) print(paste("Adding", node.name,  type, "node"))
  .ddg.proc.node(type, node.name, node.name, cmd = cmd, console=(node.name=="Console"))
  .ddg.proc2proc()

  return(node.name)
}

#' .ddg.open.new.command.node opens a new collapsible command
#' node depending on the information stored in .ddg.possible.last.cmd.
#' 
#' @return nothing
.ddg.open.new.command.node <- function() {
  new.command <- .ddg.get(".ddg.possible.last.cmd")
  if (!is.null(new.command)) {
    .ddg.add.start.node(new.command)
    
    # Now the new command becomes the last command, and new command
    # is null.
    #print (paste (".ddg.open.new.command.node: saving .ddg.last.cmd as", new.command))
    .ddg.set(".ddg.last.cmd", new.command)
    .ddg.set(".ddg.possible.last.cmd", NULL)
  }
}

#' .ddg.close.last.command.node closes the last created collapsible
#' node stored in .ddg.last.cmd properly by creating the finish node
#' and linking it in.
#' @return nothing
.ddg.close.last.command.node <- function(){

  # Get both the last command and new commands.
  .ddg.last.cmd <-
    if (.ddg.is.set(".ddg.last.cmd")) {
      .ddg.get(".ddg.last.cmd")
    }
    else {
      NULL
    }
  .ddg.possible.last.cmd <-
    if (.ddg.is.set(".ddg.possible.last.cmd")) {
      .ddg.get(".ddg.possible.last.cmd")
    }
    else {
      NULL
    }

  #print (paste (".ddg.close.last.command.node: .ddg.last.cmd =", .ddg.last.cmd))
  #print (paste (".ddg.close.last.command.node: .ddg.possible.last.cmd =", .ddg.possible.last.cmd))

  # Only create a finish node if a new command exists (i.e., we've
  # parsed some lines of code).
  # TODO: Do we need to check .ddg.possible.last.cmd?  We don't use it here.
  if (!is.null(.ddg.last.cmd) && (!is.null(.ddg.possible.last.cmd))) {
    cmd.abbrev <- .ddg.add.finish.node(.ddg.last.cmd)

    # Add link from a function return node if there is one.
    .ddg.link.function.returns(.ddg.last.cmd)

    # No previous command.
    .ddg.set(".ddg.last.cmd", NULL)
  }
}

#' .ddg.is.procedure.cmd returns TRUE if the command passed in
#' is a call to ddg.procedure, ddg.start, or ddg.finish.
#' These will create a procedure node and therefore
#' initiate the creation of a collapsible console node.
#' 
#' cmd - A DDGStatement object
#' @returnType logical
#' @return true if cmd is a call to ddg.procedure, ddg.start or ddg.finish
.ddg.is.procedure.cmd <- function(cmd) {
  return(grepl("^ddg.(procedure|start|finish)", cmd@text))
}

#' Create the warning node for the saved warning and attach it to the node
#' that created the warning
#' @return nothing
.ddg.record.warning <- function () {
  # Get the saved warning
  w <- .ddg.get.warning()

  # Create a message that looks like the one R creates
  callStr <-
      if (is.null (w$call)) ""
      else paste ("In ", utils::head (deparse(w$call)), ": ")
  warningMessage <- paste (callStr, w$message)

  # Create the warning node
  .ddg.insert.error.message(warningMessage, "warning.msg", doWarn = FALSE)

  # Clear the saved warning
  .ddg.clear.warning()
}

#' .ddg.parse.commands takes as input a list of parsed expressions from
#' an R script and creates DDG nodes for each command. If environ is an
#' environment, it executes the commands in that environment
#' immediately before creating the respective nodes for that
#' command, and then creates the data nodes based on the information
#' available in the environment. If environ is not NULL, calls to
#' ddg.* are not executed so only the clean script is processed.
#' If annotate.inside is TRUE, ddg.function, ddg.eval and ddg.return.value
#' are added to each function definition and ddg.eval is added to control
#' statements before commands are processed. If save.debug is TRUE,
#' changes to the script are saved in the ddg/debug directory.
#' ddg.annotate.on and ddg.annotate.off may be used to limit the
#' functions that are annotated or not annotated, respectively.
#'
#' If run.commands is false, the commands are not executed.  This allows
#' us to build ddgs for commands run from the console as those commands
#' have already been executed.

#' @param exprs list of parsed R statements
#' @param script.name name of the script the statements came from
#' @param script.num the number of the script in the sourced scripts table
#' @param environ environment in which commands should be
#'   executed.
#' @param ignore.patterns (optional) a vector of regular expressions.
#'   Any commands matching these expressions will not be parsed
#'   (i.e. no nodes will be created for them).
#' @param node.name (optional) name for the collapsible node under which
#'   this DDG should be stored.
#' @param run.commands (optional) commands are executed only when environ
#'   is an environment and run.commands is TRUE.
#' @param echo (optional) print each expression after parsing
#' @param print.eval (optional) print result of each evaluation.
#' @param max.deparse.length (optional) maximum number of characters
#'   output for deparse of a single expression.
#' @param called.from.ddg.eval(optional) whether called from ddg.eval
#' @param cmds list of DDG Statements that correspond to the exprs passed in.  This is
#'   currently only used when called from ddg.eval.  Normally, ddg.parse.commands
#'   creates the DDG Statement objects.

.ddg.parse.commands <- function (exprs, script.name="", script.num=NA, environ, ignore.patterns=c('^ddg.'), node.name="Console", run.commands = FALSE, echo=FALSE, print.eval=echo, max.deparse.length=150, called.from.ddg.eval=FALSE, cmds=NULL) {

  return.value <- NULL
  
  # Gather all the information that we need about the statements
  if (is.null(cmds)) {
    cmds <- .ddg.create.DDGStatements (exprs, script.name, script.num)

    if (.ddg.save.debug()) {
      .ddg.save.annotated.script(cmds, script.name)
    }
  }
  num.cmds <- length(cmds)

  # Figure out if we will execute commands or not.
  execute <- run.commands & !is.null(environ) & is.environment(environ)

  # print (paste("ddg.parse.commands: .ddg.func.depth =", .ddg.get(".ddg.func.depth")))
  inside.func <- (.ddg.get(".ddg.func.depth") > 0)

  if (!inside.func) {
    # Attempt to close the previous collapsible command node if a ddg exists
    if (.ddg.is.init()) {
      .ddg.close.last.command.node()
    }
    
    # Get the last command in the new commands and check to see if
    # we need to create a new .ddg.last.cmd node for future reference.
    .ddg.last.cmd <- cmds[[num.cmds]]
    # print(paste(".ddg.parse.commands: setting .ddg.last.cmd to", .ddg.last.cmd$text))

    if (.ddg.last.cmd@isDdgFunc) {
      .ddg.last.cmd <- NULL
      #print(".ddg.parse.commands: setting .ddg.last.cmd to null")
    }
    else if (!execute) {
      cmds <- cmds[1:num.cmds-1]
    }
  }

  # Create start and end nodes to allow collapsing of consecutive
  # console nodes. Don't bother doing this if there is only 1 new
  # command in the history or execution.
  named.node.set <- FALSE

  if (num.cmds > 0 && .ddg.is.init() && !inside.func && !called.from.ddg.eval) {
    # print(paste("ddg.new.parse.commands: Creating Start for", node.name))
    .ddg.add.start.node(node.name = node.name)
    named.node.set <- TRUE
  }

  # Create an operation node for each command.  We can't use lapply
  # here because we need to process the commands in order and
  # lapply does not guarantee an order. Also decide which data nodes
  # and edges to create. Only create a data node for the last
  # write of a variable and only if that occurs after the last
  # possible writer. Create an edge for a data use as long as the
  # use happens before the first writer/possible writer or after
  # the last writer/possible writer. Lastly, if execute is set to
  # true, then execute each command immediately before attempting
  # to create the DDG nodes.

  # Only go through this if  we have at least one command to parse.
  if (num.cmds > 0) {

    # Find where all the variables are assigned for non-environ
    # files.
    if (!execute) {
      vars.set <- .ddg.find.var.assignments(cmds)
    }
    else {
      vars.set <- .ddg.create.empty.vars.set()
    }

    # Loop over the commands as well as their string representations.
    for (i in 1:length(cmds)) {
      cmd <- cmds[[i]]

      if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Processing", cmd@abbrev))

      # print("Checking whether to set last.cmd")
      if (.ddg.enable.source() && grepl("^ddg.eval", cmd@text) && .ddg.enable.console()) {
        if (is.null(.ddg.last.cmd)) {
          .ddg.last.cmd <- cmd
        }
      }

      # Get environment for output data node.
      d.environ <- environ

      if ( .ddg.is.nonlocal.assign(cmd@parsed[[1]]) )
      {
        d.environ <- .ddg.get.env(cmd@vars.set, for.caller=TRUE)
      
        if( identical(d.environ,"undefined") )
          d.environ <- globalenv()
      }


      # Check for control & loop statements.
      st.type <- .ddg.get.statement.type(cmd@parsed[[1]])
      loop.statement <- st.type %in% c("for", "while", "repeat")
      control.statement <- loop.statement || st.type %in% c("if", "{")
      
      # Specifies whether or not a procedure node should be created
      # for this command. Basically, if a ddg exists and the
      # command is not a DDG command or a control statement, it should
      # be created. Note that if control statements are annotated,
      # a procedure node is created for each statement inside a control
      # block, so there is no need to create additional nodes for the
      # control statement itself.

      create <- !cmd@isDdgFunc && .ddg.is.init() && .ddg.enable.console() && !(control.statement && .ddg.loop.annotate() && ddg.max.loops() > 0)
      start.finish.created <- FALSE
      cur.cmd.closed <- FALSE

      # If the command does not match one of the ignored patterns.
      if (!any(sapply(ignore.patterns, function(pattern){grepl(pattern, cmd@text)}))) {

        # If sourcing, we want to execute the command.
        if (execute) {
          # Print command.
          if (echo) {
            cmd.show <- 
                paste0(substr(cmd@text, 
                              1L, 
                              min (max.deparse.length, nchar(cmd@text))), 
                       "\n")
            cat(cmd.show)
          }

          # If we will create a node, then before execution, set
          # this command as a possible abstraction node but only
          # if it's not a call that itself creates abstract nodes.
          if (!cmd@isDdgFunc && cmd@text != "next") {
            .ddg.set(".ddg.possible.last.cmd", cmd)
            .ddg.set (".ddg.cur.cmd", cmd)
            .ddg.push.cmd (cmd)
          }

          else if (.ddg.is.procedure.cmd(cmd)) {
            .ddg.set(".ddg.possible.last.cmd", NULL)
          }

          
          # Capture any warnings that occur when an expression is evaluated.
          # Note that we cannot just use a tryCatch here because it behaves
          # slightly differently and we would lose the value that eval
          # returns.  withCallingHandlers returns the value.
          # withCallingHandlers also re-throws the error after it is caught.

          # EVALUATE.

          if (.ddg.debug.lib()) print (paste (".ddg.parse.commands: Evaluating ", cmd@annotated))

          result <- withCallingHandlers(
          
              {
                for (annot in cmd@annotated) {
                  #print (paste (".ddg.parse.commands: Evaluating ", paste(annot, collapse = " ")))
                  # Don't set return.value if we are calling a ddg function or we are executing an if-statement
                  if (grepl("^ddg", annot) || grepl("^.ddg", annot) || .ddg.get.statement.type(annot) == "if") {
                    eval(annot, environ, NULL)
                  }
                  else {
                    return.value <- eval(annot, environ, NULL)
									  #if (typeof(return.value) != "closure") {
                      #print (paste (".ddg.parse.commands: Done evaluating ", annot))
                      #print(paste(".ddg.parse.commands: setting .ddg.last.R.value to", return.value))
									  #}
                    .ddg.set (".ddg.last.R.value", return.value)
                  }
                }
              },
            warning = .ddg.set.warning ,
            error = function(e)
            {
              # create procedure node for the error-causing operation
              .ddg.proc.node("Operation", cmd@abbrev, cmd@abbrev, functions.called=cmd@functions.called, console=TRUE, cmd=cmd)
              .ddg.proc2proc()

              # create input edges by adding variables to set
              vars.set <- .ddg.add.to.vars.set(vars.set,cmd,i)
              if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding", cmd@abbrev, "information to vars.set, for an error"))
              .ddg.create.data.use.edges.for.console.cmd(vars.set, cmd, i, for.caller=FALSE)

              # Create output exception node.
              .ddg.data.node("Exception", "error.msg", toString(e), "ddg.library")
              
              # Create data flow edge from procedure node to exception node.
              .ddg.proc2data(cmd@abbrev, "error.msg")
              
            }
          )

          if (.ddg.debug.lib()) print (paste (".ddg.parse.commands: Done evaluating ", cmd@annotated))

          if (!cmd@isDdgFunc && cmd@text != "next") {
            # Need to get the stack again because it could have been
            # modified during the eval call.
            .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
            stack.length <- length(.ddg.cur.cmd.stack)
            start.created <- .ddg.cur.cmd.stack[stack.length][[1]]

            # Create a finish node if a start node was created
            # start.created can have one of 3 values: "TRUE", "FALSE",
            # "MATCHES_CALL". Only create the finish node if TRUE.
            if (start.created == "TRUE") {
              .ddg.add.finish.node(cmd)
              start.finish.created <- TRUE
              .ddg.link.function.returns(cmd)

              # If the number of loop iterations exceeds max.loops, add
              # output data nodes containing final values to the finish node.
              if (loop.statement && .ddg.were.details.omitted()) {
                vars.set2 <- .ddg.add.to.vars.set(vars.set, cmd, i)
                .ddg.create.data.node.for.possible.writes(vars.set2, cmd, environ)
                .ddg.set.details.omitted(FALSE)
              }
            }

            # Remove the last command & start.created values pushed
            # onto the stack
            cur.cmd.closed <- (.ddg.cur.cmd.stack[stack.length] == "MATCHES_CALL")
            .ddg.pop.cmd ()
          }

          # Print evaluation.
          if (print.eval) print(result)
        }

        # Figure out if we should create a procedure node for this
        # command. We don't create it if it matches a last command
        # (because that last command has now become a collapsible
        # node). Matching a last command means that the last command
        # is set, is not NULL, and is equal to the current command.

        last.proc.node.created <-
            if (.ddg.is.set (".ddg.last.proc.node.created")).ddg.get(".ddg.last.proc.node.created")
            else ""
        
        create.procedure <- create && (!cur.cmd.closed || !named.node.set) && !start.finish.created  && !grepl("^ddg.source", cmd@text)
        
        # We want to create a procedure node for this command.
        if (create.procedure) {
          
          # Create the procedure node.

          if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding operation node for", cmd@abbrev))
          
          .ddg.proc.node("Operation", cmd@abbrev, cmd@abbrev, functions.called=cmd@functions.called, console=TRUE, cmd=cmd)
          .ddg.proc2proc()

          # If a warning occurred when cmd was evaluated,
          # attach a warning node
          if (.ddg.warning.occurred()) {
            .ddg.record.warning()
          }
          # Store information on the last procedure node in this
          # block.
          last.proc.node <- cmd

          # We want to create the incoming data nodes (by updating
          # the vars.set).
          if (execute) {
            # Add variables to set.
            vars.set <- .ddg.add.to.vars.set(vars.set,cmd,i)

            if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding", cmd@abbrev, "information to vars.set"))
          }

          .ddg.create.data.use.edges.for.console.cmd(vars.set, cmd, i, for.caller=FALSE)

          .ddg.create.file.read.nodes.and.edges()
          .ddg.link.function.returns(cmd)

          if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding input data nodes for", cmd@abbrev))

          .ddg.create.data.set.edges.for.cmd(vars.set, cmd, i, d.environ)

          if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding output data nodes for", cmd@abbrev))

          .ddg.create.file.write.nodes.and.edges ()
          .ddg.create.graphics.nodes.and.edges ()
        }
        # We wanted to create it but it matched a last command node.
        else if (create && execute) {
          .ddg.close.last.command.node()
          if (execute) {
            # Add variables to set.
            vars.set <- .ddg.add.to.vars.set(vars.set,cmd, i)
            if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding", cmd@abbrev, "information to vars.set"))
            .ddg.create.data.set.edges.for.cmd(vars.set, cmd, i, environ)
          }
        }

        if (create.procedure && execute) {
          .ddg.create.data.node.for.possible.writes(vars.set, last.proc.node, env=environ)

          # Update so we don't set these again.
          vars.set$possible.last.writer <- vars.set$last.writer
        }
      }
     }

     # Create a data node for each variable that might have been set in
     # something other than a simple assignment, with an edge from the
     # last node in the console block or source .
     if (!execute) {
       .ddg.create.data.node.for.possible.writes(vars.set, last.proc.node, env=environ)
     }
  }

  #print("Done with ddg.parse.commands loop")

  # Close any node left open during execution.
  if (execute && !inside.func) .ddg.close.last.command.node()

  # Close the console block if we processed anything and the DDG
  # is initialized (also, save).
  #
  if (.ddg.is.init() && named.node.set && !inside.func) {
      .ddg.add.finish.node(node.name = node.name)
  }

  # Open up a new collapsible node in case we need to parse
  # further later.
  if (!execute) {

    .ddg.set(".ddg.possible.last.cmd", .ddg.last.cmd)
    .ddg.set(".ddg.last.cmd", .ddg.last.cmd)
    .ddg.open.new.command.node()
  }

  # Write time stamp to history.
  if (.ddg.is.init() && !.ddg.is.sourced()) .ddg.write.timestamp.to.history()

  if (.ddg.is.set(".ddg.last.R.value")) return (.ddg.get (".ddg.last.R.value"))
  else return ("")
  #return.value <- .ddg.get (".ddg.last.R.value")
  #if (typeof(return.value) != "closure") {
  #  print(paste(".ddg.parse.commands: returning ", return.value))
  #}
}

#' Push a command onto the command stack.  The command stack remembers the 
#' command about to be executed.  It also puts FALSE on the stack to indicate
#' that no start node has (yet) been created for the command.
#' @param cmd The DDGStatement about to be executed
#' @return nothing
.ddg.push.cmd <- function (cmd) {
  
  #print(paste("Pushing onto the stack:", cmd@text))
  
  # Remember the current statement on the stack so that we
  # will be able to create a corresponding Finish node later
  # if needed.
  .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
  
  if (length(.ddg.cur.cmd.stack) == 0) {
    .ddg.cur.cmd.stack <- c(cmd, FALSE)
  }
  else {
    .ddg.cur.cmd.stack <- c(.ddg.get(".ddg.cur.cmd.stack"), cmd, FALSE)
  }
  .ddg.set(".ddg.cur.cmd.stack", .ddg.cur.cmd.stack)
}

#' Remove the top of the command stack, along with the boolean that remembers
#' if the start / finish nodes have been created.
.ddg.pop.cmd <- function () {
  .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
  stack.length <- length(.ddg.cur.cmd.stack)
  if (stack.length == 2) {
    .ddg.set(".ddg.cur.cmd.stack", vector())
  }
  else {
    .ddg.set(".ddg.cur.cmd.stack", .ddg.cur.cmd.stack[1:(stack.length-2)])
  }
}

#' .ddg.get.top.cmd returns the last command on the stack.
#' @returnType DDGStatement
#' @return the last command pushed to the stack
.ddg.get.top.cmd <- function() {
  .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
  stack.length <- length(.ddg.cur.cmd.stack)
  cmd <- .ddg.cur.cmd.stack[stack.length-1][[1]]
}

#' Change the value associated with the current command while keeping 
#' the command at the top of the stack the same
#' 
#' @param value the new value
#' @return nothing
.ddg.change.cmd.top <- function (value) {
  .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
  stack.length <- length(.ddg.cur.cmd.stack)
  .ddg.set (".ddg.cur.cmd.stack", c(.ddg.cur.cmd.stack[1:stack.length-1], value))
}


#' .ddg.lookup.function.name gets the name of the calling function
#' and sets pname to that value. If pname is passed as a string,
#' pname is not changed.  If pname is not a string, it is deparsed.
#' If pname is NULL when called, pname is obtained from the calling environment.
#' 
#' Note that it is important that this be a macro, not a function,
#' due to the use of the substitute function in the body.  expr is
#' the macro body.
#' 
#' @param pname - name of procedure node.
#' 
.ddg.lookup.function.name <- gtools::defmacro (pname,
    expr =
        # If pname is not provided, get from function call.
        if (is.null(pname)) {
          
          # Look up function call.
          # Note: I tried to call .ddg.get.first.non.ddg.call instead
          # of hardwiring the number here but it did not work.  The 
          # call stack contains unexpected entries to eval before
          # getting to the user's function.
          call <- sys.call(-4)

          if (typeof(call[[1]]) == "closure") {
            #print(".ddg.lookup.function.name:  Found a closure!")
            pname <- "FUN"
          }
          else {
            pname <- as.character(call[[1]])
          }
        }

        # Convert pname to a string if necessary.
        else if (!is.character(pname)) {
          pname <- deparse(substitute(pname))
        }
)

#' .ddg.lookup.value is used to determine what value to use when
#' creating data nodes.  
#' 
#' Note that it is important that this be a
#' macro, not a function, due to the use of the substitute function
#' in the body.  expr is the macro body.
#' 
#' @param expr.to.evaluate the expression to be evaluted. This can be a string or
#'   a name.
#' @param value the value that was passed in to the calling function.
#'   If value already exists, nothing happens. If value is NULL,
#'   the expression is evaluated to determine the value.
#' @param env the environment in which the evaluation is done.
#' @param procname the name of the calling procedure, used to produce
#'   an error message if necessary.  Only needed if warn is TRUE.
#' @param warn (optional) if TRUE, warns user that the expression could
#'   not be evaluated if the evaluation failed
#' 
.ddg.lookup.value <- gtools::defmacro(expr.to.evaluate, value, env, procname = "", warn=FALSE,
    expr =
        if (is.null(value)) {
          arg <- substitute(expr.to.evaluate)
          if (is.character(arg)) {
            tryCatch (arg <- parse(text=expr.to.evaluate),
            error = function(e) {})
          }
          else expr.to.evaluate <- deparse(arg)
          value <- tryCatch (
              eval(arg, env),
              error = function(e) {
                if (warn) {
                  error.msg <- paste("Unable to evaluate", expr.to.evaluate, "in call to", procname)
                  .ddg.insert.error.message(error.msg)
                }
                return ("")
              }
          )
        }
)

#' .ddg.delete.temp deletes any temporary files created during
#' the processing of a script. These include the temporary
#' history file.
.ddg.delete.temp <- function() {
  # Delete the temporary history file if we made it.
  if (.ddg.is.set('.ddg.history.file')) unlink(.ddg.get('.ddg.history.file'))

  # Clear the environment.
  .ddg.env <- new.env(parent=emptyenv())
}

#' .ddg.create.output.nodes creates output nodes for ddg.function
#' and ddg.procedure. Outs values must be passed as strings, not
#' names, unless the value is a file name.
#' 
#' @param pname the name of the procedure node.
#' @param outs.graphic - the name of a snapshot node to be used as a
#'    file name.  A graphical snapshot is simply a captured image
#'    of the graphic device active at the time of the call to
#'    ddg.function or ddg.procedure.
#' @param outs.data - a list of names of data nodes.
#' @param outs.exception - a list of names of exception nodes.
#' @param outs.url - a list of names of url nodes.
#' @param outs.file - a list of names of file nodes. Supported file
#'   extensions include: .csv, .jpg, .jpeg, .pdf, and .txt.
#' @param graphic.fext - the file extension to be used when saving the
#'   captured graphic. Supported extensions are .jpg, .jpeg, .pdf.
.ddg.create.output.nodes<- function(pname, outs.graphic, outs.data, outs.exception, outs.url, outs.file, graphic.fext) {
  env <- .ddg.get.first.non.ddg.env()
  
  # Capture graphics device.
  if (is.character(outs.graphic)) {
    name <- outs.graphic
    gfext <- as.character(graphic.fext)
    .ddg.write.graphic(name,"Graphical Plot. Not saved in script.",fext=gfext) # value is ignored
    .ddg.proc2data(pname,name)
  }

  # Create output nodes and edges if outs list provided.

  # Exception node.
  if (!is.null(outs.exception)) {
    stack <- sys.calls()
    # Get scope.
    # scope <- .ddg.get.scope(outs.exception[[1]])

    lapply(outs.exception,
        function(param) {
          # Get value in calling environment.
          name <- param
          value <- NULL
          .ddg.lookup.value(name, value, env)

          # Exception node.
          scope <- .ddg.get.scope(param, calls = stack)
          .ddg.data.node("Exception", name, value, scope)
          .ddg.proc2data(pname, name, scope)
        }
    )
  }

  # URL node.
  if (!is.null(outs.url)) {
    stack <- sys.calls()
    # Get scope.
    # scope <- .ddg.get.scope(outs.url[[1]])

    lapply(outs.url,
        function(param) {
          # Get value in calling environment.
          name <- param
          value <- NULL
          .ddg.lookup.value(name, value, env)

          # URL node.
          scope <- .ddg.get.scope(param, calls=stack)
          .ddg.data.node("URL", name, value, scope)
          .ddg.proc2data(pname, name, scope)
        }
    )
  }

  # Generalized data node (includes simple data values as well as
  # snapshots)
  if (!is.null(outs.data)) {
    # Get scope.
    # scope <- .ddg.get.scope(outs.data[[1]])
    stack <- sys.calls()
    lapply(outs.data,
        function(param) {
          # Get value in calling environment.
          name <- param
          value <- NULL
          .ddg.lookup.value(name, value, env)

          tryCatch({
                if (!is.character(name)) name <- deparse(substitute(name))
                envName <- environmentName(env)
                scope <- .ddg.get.scope(param, calls=stack)
                .ddg.save.data(name, value, error=TRUE, scope=scope)
                .ddg.proc2data(pname, name, scope)
              }, error = function(e) {
                .ddg.insert.error.message(e)
              }
          )
        }
    )
  }

  # File node.
  if (!is.null(outs.file)) {
    # Get scope.
    # scope <- .ddg.get.scope(outs.file[[1]])
    stack <- sys.calls()

    lapply(outs.file,
        function(param) {
          # Get value in calling environment.
          name <- param
          value <- NULL
          .ddg.lookup.value(name, value, env)
          scope <- .ddg.get.scope(param, calls=stack)

          if (value == "") {
            # Filename passed as value.
            .ddg.file.copy(name, name, scope)
            .ddg.proc2data(pname, name, scope)
          }
          else {
            # Filename passed as name.
            .ddg.file.copy(value, name, scope)
            .ddg.proc2data(pname, name, scope)
          }
        }
    )
  }
}

#' Get the environment for the function that called
#' into ddg functions
#'
#' @returnType environment
#' @return the environment of the innermost user's function
.ddg.get.first.non.ddg.env <- function() {
  non.ddg.frame <- .ddg.get.first.non.ddg.frame.number()
  return (sys.frame(non.ddg.frame))
}

#' Get the frame number for the function that called
#' into ddg functions
#'
#' @returnType integer
#' @return the frame number of the innermost user function
.ddg.get.first.non.ddg.frame.number <- function() {
  calls <- sys.calls()
  calls <- as.character (mapply( `[[` , calls , 1 , SIMPLIFY = TRUE ))
  #print(paste("calls =", calls))
  #print(summary(calls))
  
  return ( Position( function (call) {return (!startsWith (call, "ddg") & !startsWith (call, ".ddg"))}, calls, right=TRUE ))
}

#' .ddg.create.function.nodes creates the start node, procedure node, input
#' binding nodes, and output nodes for the function.
#' 
#' @param pname name of procedure node.
#' @param call call as made
#' @param full.call full function call, with full parameter names
#' @param outs.graphic - the name of a snapshot node to be used as a
#'    file name.  A graphical snapshot is simply a captured image
#'    of the graphic device active at the time of the call to
#'    ddg.function or ddg.procedure.
#' @param outs.data - a list of names of data nodes.
#' @param outs.exception - a list of names of exception nodes.
#' @param outs.url - a list of names of url nodes.
#' @param outs.file - a list of names of file nodes. Supported file
#'   extensions include: .csv, .jpg, .jpeg, .pdf, and .txt.
#' @param graphic.fext - the file extension to be used when saving the
#'   captured graphic. Supported extensions are .jpg, .jpeg, .pdf.
#' @param env (optional) - the environment local to the function
#' 
.ddg.create.function.nodes <- function(pname, call, full.call, outs.graphic=NULL, outs.data=NULL, outs.exception=NULL, outs.url=NULL, outs.file=NULL, graphic.fext="jpeg", env=NULL) {
  # Create the start node
  if (typeof(call[[1]]) == "closure") {
    #print(paste(".ddg.create.function.nodes: pname =", pname))
    .ddg.add.start.node (node.name=pname)
  }
  else {
    #print(paste(".ddg.create.function.nodes: deparse(call) =", deparse(call)))
    .ddg.add.start.node (node.name=paste(deparse(call), collapse=""))
  }

  # Tokens will contain the function name and the argument
  # expressions.

  # Get parameters and create edges.
  if (length(full.call) > 1) {
    # args contains the names of the variable that was passed into
    # the function.
    args <- full.call[2:length(full.call)]

    # param,names contains the names of the parameters (this is
    # what the variable is known as inside the function).
    #print(paste(".ddg.create.function.nodes: full.call =", full.call))
    param.names <- names(full.call)
    param.names <- param.names[2:length(param.names)]
    stack <- sys.calls()
    bindings <- list()
    for (i in 1:length(args)) bindings[[i]] <-list(args[[i]], param.names[[i]])
    missing.params <- character()

    lapply(bindings,
        function(binding) {
          # Here, arg is the arguments passed IN.
          arg <- binding[[1]]

          # formal is the paramenter name of the function (what
          # is the variable known as inside?).
          formal <- binding[[2]][[1]]
          if (is.null(formal) || formal == "") formal <- "..."

          # Find all the variables used in this parameter.
          # If the argument is a string constant, don't bother
          # looking for variables.  Also add quotes around it
          # in the node name.
          if (is.character(arg)) {
            vars.used <- character()
            binding.node.name <- paste(formal, " <- \"", arg, "\"", sep="")
          }
          else {
            vars.used <- .ddg.find.var.uses(arg)
            binding.node.name <- paste(formal, " <- ", paste(deparse(arg), collapse=" "))
            #print(paste(".ddg.create.function.nodes: binding.node.name =", binding.node.name))
          }

          .ddg.proc.node("Binding", binding.node.name)
          .ddg.proc2proc()
          
          # Add an input to the binding node for each variable referenced in the argument
          sapply (vars.used, function (var) {
                param.scope <- .ddg.get.scope(var, for.caller = TRUE, calls=stack)
                if (.ddg.data.node.exists(var, param.scope)) {
                  .ddg.data2proc(as.character(var), param.scope, binding.node.name)
                  if (.ddg.debug.lib()) print(paste("param:", var))
                }
              })
          if (formal != "...") {
            formal.scope <- .ddg.get.scope(formal, calls=stack)
            formal.env <- .ddg.get.env(formal, calls=stack)

            # If we can evaluate the argument without an error, we
            # record the value. If an error occurs, we do not record
            # the value as it's possible that the function never
            # actually uses it.
            tryCatch ({
                  .ddg.save.data(formal, eval(parse(text=formal), formal.env), scope=formal.scope, stack=stack)
                  .ddg.proc2data(binding.node.name, formal, formal.scope)},
                error = function(e) {})

          }
        })
  }

  .ddg.proc.node("Operation", pname, pname)

  # Link to the definition of the function if the function is defined in this script.
  if (.ddg.data.node.exists(pname, environmentName(.GlobalEnv))) {
    .ddg.data2proc(pname, environmentName(.GlobalEnv), pname)
  }

  # Create edges from the formal to the operation node for the function
  if (length(full.call) > 1) {
    lapply(bindings, function(binding) {
          formal <- binding[[2]][[1]]

          # Formal will be NULL if declared as ...  Don't create the data node in that case.
          if (!is.null(formal) && formal != "") {
            formal.scope <- .ddg.get.scope(formal, calls=stack)
            if (.ddg.data.node.exists (formal, formal.scope)) {
              .ddg.data2proc(formal, formal.scope, pname)
            }
          }
        })
  }

  # Create control flow edge from preceding procedure node.
  .ddg.proc2proc()

  # create output nodes

  .ddg.create.output.nodes(pname, outs.graphic, outs.data, outs.exception, outs.url, outs.file, graphic.fext)

}

#' .ddg.get.frame.number gets the frame number of the closest
#' non-library calling function.
#' 
#' @param calls call stack to search
#' @param for.caller (optional) if TRUE, return the frame of the caller of the 
#'    first non-ddg function
#' @returnType integer
#' @return If for.caller is FALSE, returns the top-most non-ddg function on the
#'   call stack.  If for.caller is TRUE, returns the second one found.  If none
#'   are found, returns 0. 
.ddg.get.frame.number <- function(calls, for.caller=FALSE) {
  script.func.found <- FALSE
  nframe <- length(calls)
  for (i in nframe:1) {
    call <- calls[[i]][[1]]
    # Guess that if we have a closure it is a user-defined function and not a ddg function
    # Is this a good assumption ????
    if (typeof(call) == "closure") {
      if (for.caller && !script.func.found) {
        script.func.found <- TRUE
      }
      else {
        return(i)
      }
    }
    else {
      call.func <- as.character(call)
      # Ignore calls to ddg functions or to the functions that get called from the outermost tryCatch
      # to ddg code.
      if (!any (startsWith (call.func, c (".ddg", "ddg", "doTryCatch", "tryCatch")))) {
        if (for.caller && !script.func.found) {
          script.func.found <- TRUE
        }
        else {
          return(i)
        }
      }
    }
  }
  return(0)
}


#' .ddg.where looks up the environment for the variable specified
#' by name.  Adapted from Hadley Wickham, Advanced R programming.
#' 
#' @param name - name of variable.
#' @param env (optional) - environment in which to start looking for variable.
#' @param warning (optional) - set to TRUE if a warning should be thrown when a variable is not found.
#' @returnType an environment
#' @return the environment in which the name is found.  Returns "undefined" if the
#'   variable is not found.
.ddg.where <- function( name , env = parent.frame() , warning = TRUE )
{
  stopifnot(is.character(name), length(name) == 1)
  
  if (identical(env, emptyenv()))
  {
    if(warning)
      warning("Can't find ", name)

    return("undefined")
  }
  if (exists(name, env, inherits=FALSE))
  {
    env
  }
  else
  {
    .ddg.where(name, parent.env(env), warning)
  }
}

#'.ddg.get.env gets the environment in which name is declared.
#' 
#' @param name variable name.
#' @param for.caller (optional) if TRUE, go up one level before searching.
#' @param calls (optional) call stack to search
#' 
.ddg.get.env <- function(name, for.caller=FALSE, calls=NULL) {
  if (is.null(calls)) calls <- sys.calls()

  fnum <- .ddg.get.frame.number(calls, for.caller)
  stopifnot(!is.null(fnum))

  tryCatch (
    if(!exists(name, sys.frame(fnum), inherits=TRUE)) return(NULL),
    error = function(e) {}
  )
  env <- .ddg.where(name, sys.frame(fnum))
  return(env)
}

#' .ddg.get.scope converts from an environment object to its name.  If no
#' environment is passed in, it uses the name to find the environment.  One
#' of name or env must be provided.
#' 
#' @param name name of variable.
#' @param for.caller (optional) if TRUE, go up one level before searching.
#' @param calls (optional) call stack to search
#' @param env (optional) the environment to get the scope for
#' 
.ddg.get.scope <- function(name="", for.caller=FALSE, calls=NULL, env=NULL) {
  # Get the environment for the variable call.
  if (is.null(env)) {
    env <- .ddg.get.env(name, for.caller, calls)
  }

  # If no environment found, name does not exist, so scope is
  # undefined.
  if (is.null(env)) return ("undefined")

  scope <- sub('^<environment: (.*)>$', '\\1', utils::capture.output(env)[1])
  if (grepl("undefined", scope)) scope <- "undefined"
  return(scope)
}

#' Creates a start node for the current command if one has not
#' been created already.  Modifies the command stack by setting the 
#' value to TRUE if the start node is created.  If the current command 
#' matches the call, no node is created but the top of the stack is changed
#' to "MATCHES_CALL".
#' 
#' @param call the parsed version of the function call
#' @return nothing
.ddg.create.start.for.cur.cmd <- function (call) {
  if (!.ddg.is.set(".ddg.cur.cmd")) return ()

  .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
  stack.length <- length(.ddg.cur.cmd.stack)
  if (stack.length == 0) return ()
  
  last.created <- .ddg.cur.cmd.stack[stack.length]
  # Only create a start node for the current command if we have not already
  # created one and the command is more than just the call to this function
  if (last.created[[1]] != "FALSE") return ()
  
  .ddg.cur.cmd <- .ddg.get(".ddg.cur.cmd")
  if (.ddg.cur.cmd@text == paste(deparse(call), collapse="")) {
    .ddg.change.cmd.top ("MATCHES_CALL")
  }
  
  else {
    cmd.abbrev <- .ddg.add.start.node (.ddg.cur.cmd)
    .ddg.cur.expr.stack <- .ddg.get(".ddg.cur.expr.stack")
    st.type <- .ddg.get.statement.type(.ddg.cur.cmd@parsed[[1]])
    loop.statement <- st.type %in% c("for", "while", "repeat")
    control.statement <- loop.statement || st.type %in% c("if", "{")
    .ddg.create.data.use.edges.for.console.cmd(vars.set = data.frame(), .ddg.cur.cmd, 0, for.caller=!control.statement)

    # Add Details Omitted node before annotated loops if needed.
    if (loop.statement && ddg.first.loop() > 1) {
      ddg.details.omitted()
    }

    # Mark the start node as created on the stack.  Mark it even if we did not
    # create the abstract node above, because we will create it below.
    .ddg.change.cmd.top (TRUE)
  }
}

#' .ddg.markdown takes a Rmd file and extracts the R code and text through
#' the purl function in the knitr library. It then annotates the R script
#' to insert start and finish nodes based on the chunks the user already
#' created. If eval = false, then the chunk will not be added to the DDG. If
#' the user has a name for the chunk, then that name will be used, else a chunk
#' name "ddg.chunk_1" and higher numbers will be generated.
#'
#' Important: If in a code chunk, there is an empty line followed by "#' ----"
#' or "#''", then an extra finish node will be inserted, causing an error.
#'
#' @param r.script.path the path of the original Rmd file
#' @param output.path the path of the generated R script
#' @returnType string
#' @return the path to the original Rmd file
.ddg.markdown <- function(r.script.path, output.path){

  #generates R script file from markdown file
  knitr::purl(r.script.path, documentation = 2L, quiet = TRUE)

  #moves file to ddg directory
  file.rename(from = paste(getwd(), "/", basename(tools::file_path_sans_ext(r.script.path)), ".R", sep = ""), to = output.path)
  script <- readLines(output.path)

  skip <- FALSE
  name <- "ddg.chunk"
  annotated <- character(0)
  index <- 1


  # This for loop goes through the script line by line and searches for patterns
  # to insert the start and finish nodes
  for(i in 1:length(script)){

    #eval = false means we skip this code chunk, therefore skip = TRUE
    if(regexpr("eval+(\\s*)+=+(\\s*)+FALSE", script[i]) != -1){
      skip <- TRUE
      annotated <- append(annotated, script[i])
    }

    else if(regexpr("## ----", script[i]) != -1){

      #if no options in the line, then generate default name.
      if(regexpr("## -----", script[i]) == -1){
        if(regexpr("=", script[i]) == -1){
          end <- regexpr("-----", script[i])
          name <- substring(script[i], 8, last = end -1)
        }
        else if(regexpr(",", script[i]) != -1){
          comma <- regexpr(",", script[i])
          name <- substring(script[i], 8, last = comma -1)
        }
        else{
          name <- paste("ddg.chunk_", index, sep = "")
          index <- index + 1
        }
      }
      else{
        name <- paste("ddg.chunk_", index, sep = "")
        index <- index + 1
      }
      name <- stringr::str_trim(name, side = "both")
      annotated <- append(annotated, paste("ddg.start(\"", name, "\")", sep = ""))
    }
    else if(nchar(script[i]) == 0 && (regexpr("#'", script[i + 1]) != -1 ||
                                      i == length(script) || regexpr("## ----", script[i + 1]) != -1 )){
      if(skip){
        annotated <- append(annotated, script[i])
        skip <- FALSE
      }
      else{
        annotated <- append(annotated, paste("ddg.finish(\"", name, "\")", sep = ""))
      }
    }
    else{
      annotated <- append(annotated, script[i])
    }
  }
  writeLines(annotated, output.path)
  r.script.path
}


#' .ddg.save.debug.files saves debug files to the debug directory.
#' @return nothing
.ddg.save.debug.files <- function() 
{
	# Save initial environment table to file.
	fileout <- paste(.ddg.path.debug(), "/initial-environment.csv", sep="")
	ddg.initial.env <- .ddg.initial.env()
  utils::write.csv(ddg.initial.env, fileout, row.names=FALSE)

  .ddg.save.debug.proc.nodes ()
  .ddg.save.debug.data.nodes ()
  .ddg.save.debug.edges()
  .ddg.save.function.table ()

	# save library information to file
	fileout <- paste(.ddg.path.debug(), "/libraries.csv", sep="")
  utils::write.csv(.ddg.installedpackages(), fileout, row.names=FALSE)
	
	# save execution environment information to file
	fileout <- paste(.ddg.path.debug(), "/environment.csv", sep="")
  utils::write.csv(.ddg.exec.env(), fileout, row.names=FALSE)
	
  .ddg.save.return.value.table ()
  .ddg.save.sourced.script.table ()
}

#' @return a data frame of information about the current execution environment.
.ddg.exec.env <- function()
{
	env <- data.frame(	"architecture" = character(1), 
						"os" = character(1), 
						"language" = character(1), 
						"rVersion" = character(1), 
						"script" = character(1), 
						"scriptTimeStamp" = character(1),
						"workingDirectory" = character(1), 
						"ddgDirectory" = character(1), 
						"ddgTimeStamp" = character(1),
						"rdtVersion" = character(1), 
						"hashAlgorithm" = character(1),
						stringsAsFactors = FALSE )
	
	# architecture, language, rVersion
	r.version <- R.Version()
	
	env$architecture[1] <- r.version$arch
	env$language[1] <- r.version$language
	env$rVersion[1] <- r.version$version
	
	# operating system
	env$os[1] <- .Platform$OS.type
	
	# script variables
	script.path <- .ddg.get("ddg.r.script.path")
	
	if( ! is.null(script.path) )
	{
		env$script[1] <- script.path
		env$scriptTimeStamp[1] <- .ddg.format.time( file.info(script.path)$mtime )
	}
	else
	{
		env$script[1] <- ""
		env$scriptTimeStamp[1] <- ""
	}
	
	# working directory, ddg. directory
	env$workingDirectory[1] <- getwd()
	env$ddgDirectory[1] <- .ddg.path()
	
	# ddg timestamp
	env$ddgTimeStamp[1] <- .ddg.get("ddg.start.time")
	
	# rdt version
	env$rdtVersion[1] <- toString( utils::packageVersion("RDataTracker") )
	
	# hash algorithm
	env$hashAlgorithm[1] <- .ddg.get(".ddg.hash.algorithm")
	
  return(env)
}


