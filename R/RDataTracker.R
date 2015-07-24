#################### DDG LIBRARY FOR R ####################
# The functions in this library may be used to annotate an R script 
# in order to collect provenance in the form of a data derivation 
# graph (DDG) as the script executes. The DDG is saved as a text file 
# (ddg.txt) that may be viewed and queried using DDG Explorer.

# Copyright (C) 2014 Emery R. Boose & Barbara S. Lerner & Luis Perez
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

# Create DDG environment variable.

.ddg.env <- new.env(parent=emptyenv())

# Set maximum number of checkpoints in DDG.

ddg.MAX_CHECKPOINTS <- 10

# Set the number of lines the history file keeps (and therefore 
# can be analyzed). Note: this setting has no effect on some 
# systems.

ddg.MAX_HIST_LINES <- 2^14

#-------- FUNCTIONS TO MANAGE THE GLOBAL VARIABLES--------#

# Global variables cannot be used directly in a library.  Instead, 
# we need to place the variables in our own environment.  These 
# functions make that environment easier to use.

.onLoad <- function(libname, pkgname) {
  .ddg.init.tables()
}

.ddg.set <- function(var, value) {
  .ddg.env[[var]] <- value
  return(invisible(.ddg.env[[var]]))
}

.ddg.is.set <- function(var) {
  return(exists(var, envir=.ddg.env))
}

.ddg.get <- function(var) {
  if (!.ddg.is.set(var)) {
    error.msg <- paste("No binding for", var, ". DDG may be incorrect!")
    .ddg.insert.error.message(error.msg)
    return(NULL)
  }
  else {
    return(.ddg.env[[var]])
  }
}

# .ddg.clear removes all objects from the .ddg.env environment.

.ddg.clear <- function() {
  # reinitialize tables
  .ddg.init.tables()
}

##### Getters for specific variables

.ddg.debug <- function() {
  return (.ddg.get("ddg.debug"))
}

.ddg.path <- function() {
  return (.ddg.get("ddg.path"))
}

.ddg.dnum <- function() {
  return (.ddg.get("ddg.dnum"))
}

.ddg.pnum <- function() {
  return (.ddg.get("ddg.pnum"))
}

.ddg.data.nodes <- function() {
  return (.ddg.get("ddg.data.nodes"))
}

.ddg.proc.nodes <- function() {
  return (.ddg.get("ddg.proc.nodes"))
}

.ddg.enable.console <- function() {
  return (.ddg.get(".ddg.enable.console"))
}

.ddg.enable.source <- function(){
  return(.ddg.is.set("from.source") && .ddg.get("from.source"))
}

##### Mutators for specific common actions

.ddg.inc <- function(var) {
  value <- .ddg.get(var)
  .ddg.set(var, value + 1)
}

.ddg.dec <- function(var) {
  value <- .ddg.get(var)
  .ddg.set(var, value - 1)
}

.ddg.append <- function(...) {
  text <- .ddg.get("ddg.text")
  .ddg.set("ddg.text", paste(text, ...))
}

.ddg.add.rows <- function(df, new.rows) {
  table <- .ddg.get(df)
  .ddg.set(df, rbind(table, new.rows))
}

#-------------------BASIC FUNCTIONS-----------------------#

# .ddg.init.tables creates data frames to store procedure nodes, 
# data nodes, function return values, and checkpoints. It also 
# initializes selected constants and variables.

.ddg.init.tables <- function() {
# Create tables for procedure and data nodes.  Initially, tables 
# have 100 rows each.
  size <- 100
  
  .ddg.set("ddg.proc.nodes", data.frame(ddg.type = character(size),
          ddg.num = numeric(size),
          ddg.name = character(size),
          ddg.value = character(size), 
          ddg.return.linked = logical(size),
          ddg.auto.created = logical(size),
          stringsAsFactors=FALSE))
  
  .ddg.set("ddg.data.nodes", data.frame(ddg.type = character(size),
          ddg.num = numeric(size),
          ddg.name = character(size),
          ddg.value = character(size),
          ddg.scope = character(size),                                          
          ddg.time = character(size),
          ddg.loc = character(size),
          ddg.current = logical(size), stringsAsFactors=FALSE))
  
  # Create procedure and data node counters.
  .ddg.set("ddg.pnum", 0)
  .ddg.set("ddg.dnum", 0)
  
  # Create DDG string. This string is written to file when ddg.save 
  # is called.
  .ddg.set("ddg.text", "")
  
  # Used to control debugging output.  If already defined, don't 
  # change its value.
	if (!.ddg.is.set("ddg.debug")) .ddg.set("ddg.debug", FALSE)

	# Used to control sourcing. If already defined, don't change 
  # its value.
	if (!.ddg.is.set("from.source")) .ddg.set("from.source", FALSE)

	# Set current number of checkpoints.
	.ddg.set("ddg.checkpoint.num", 0)

  # Create table for checkpoints.
	.ddg.set("ddg.checkpoints", 
          data.frame(filename=character(ddg.MAX_CHECKPOINTS),
          checkpoint.name=character(ddg.MAX_CHECKPOINTS), 
          stringsAsFactors=FALSE))
	
	# Record last command from the preceding console block.
	.ddg.set(".ddg.last.cmd", NULL)

	# Record value returned by calls to ddg.return.
  # ddg.call - the string representing the call, like "f(a)".
  # return.used - remembers if this function return value has been 
  #   linked to the caller.
  # return.node.id - the id of the data node that holds the return 
  #   value.
	.ddg.set(".ddg.return.values",
          data.frame(ddg.call=character(size),  
					return.used = logical(size),
					return.node.id = integer(size),
					stringsAsFactors=FALSE))

  .ddg.set(".ddg.num.returns", 0)
  
	# Record the current command to be opened during console execution
  # (used when executing a script using ddg.source).
	.ddg.set(".ddg.possible.last.cmd", NULL)

	# Used for keeping track of current execution command.
	.ddg.set("var.num", 1)

	# Keep track of history.
	.ddg.set(".ddg.history.timestamp", NULL)
	
	# Keep track of the last device seen (0 implies NULL).
	.ddg.set("prev.device", 0)

	# Store path of current script.
	.ddg.set("ddg.r.script.path", NULL)

	# Store path of current ddg.
	.ddg.set("ddg.path", NULL)

	# No ddg initialized.
	.ddg.set(".ddg.initialized", FALSE)

	# No history file.
	.ddg.set(".ddg.history.file", NULL)

	# Console is disabled.
	.ddg.set(".ddg.enable.console", FALSE)
}

# .ddg.set.history provides a wrapper to change the number of 
# history lines during execution of an R script.

# lines - number of lines in history file.

.ddg.set.history <- function(lines=16384){
	Sys.setenv("R_HISTSIZE" = lines)
}

# .ddg.init.environ() sets up the filesystem and R environments 
# for use.

.ddg.init.environ <- function() {
  dir.create(.ddg.path(), showWarnings = FALSE)
  if (interactive() && .ddg.enable.console()) {
    .ddg.set('ddg.original.hist.size', Sys.getenv('R_HISTSIZE'))
    .ddg.set.history()
  }
}

# ddg.environ gets environment information for the DDG.

.ddg.environ <- function() {
  architecture <- R.Version()$arch
  operating.system <- .Platform$OS.type
  r.version <- R.Version()$version
  lib.version <- packageVersion("RDataTracker")
  time <- .ddg.timestamp()
  environ <- paste("Architecture=\"", architecture, "\"\n", sep="")
  environ <- paste(environ, "OperatingSystem=\"", operating.system, "\"\n", sep="")
  environ <- paste(environ, "Language=\"R\"\n", sep="")
  environ <- paste(environ, "LanguageVersion=\"", r.version, "\"\n", sep="")
  environ <- paste(environ, "RDataTrackerVersion=\"", lib.version, "\"\n", sep="")
  ddg.r.script.path <- .ddg.get("ddg.r.script.path")
  if (!is.null(ddg.r.script.path)) {
    environ <- paste(environ, "Script=\"", ddg.r.script.path, "\"\n", sep="")
    environ <- paste(environ, "ProcessFileTimestamp=\"", .ddg.format.time(file.info(ddg.r.script.path)$mtime), "\"\n", sep="")
  }
  environ <- paste(environ, "WorkingDirectory=\"", getwd(), "\"\n", sep="")
  environ <- paste(environ, "DDGDirectory=\"", .ddg.path(), "\"\n", sep="")
  environ <- paste(environ, "DateTime=\"", time, "\"\n", sep="")
  return (environ)
}

# .ddg.is.init is called at the beginning of all user accessible 
# functions. It verifies that a DDG has been initialized. If it 
# hasn't, it returns FALSE. 

.ddg.is.init <- function() {
		# Short circuits evaluation.
		return(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized"))
}

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

# .ddg.is.csv returns TRUE if the value passed in should be written 
# out as a csv file. No assumptions are made about input.

# value - input value.

.ddg.is.csv <- function(value) {
  return(!(.ddg.is.graphic(value) || .ddg.is.simple(value)) && (
            is.list(value) || is.vector(value) || is.matrix(value) || is.data.frame(value)))
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
# it to the DDG. It does not however create any edges.

# name - data node name.
# value - data node value.
# scope - data node scope.

.ddg.save.simple <- function(name, value, scope=NULL) {
	# Save the true value.
	.ddg.data.node("Data", name, value, scope)
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

.ddg.write.graphic <- function(name, value=NULL, fext="jpeg", scope=NULL){
	# Try to output graphic value.
	tryCatch({
		.ddg.snapshot.node(name, fext, NULL, dscope=scope)
	}, error = function(e) {
		# warning(paste("Attempted to write", name, "as", fext, "snapshot. Trying jpeg", ".", e))
		tryCatch({
			.ddg.dec("ddg.dnum")
			.ddg.snapshot.node(name, "jpeg", NULL, dscope=scope)
		}, error = function(e) {
			 # warning(paste("Attempted to write", name, "as jpeg snapshot. Failed.", e, "Defaulting to saving RObject and .txt file."))
			.ddg.dec("ddg.dnum")
  		.ddg.snapshot.node(name, "txt", value, save.object = TRUE, dscope=scope)
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

.ddg.write.csv <- function(name, value, scope=NULL) {
  tryCatch({
		.ddg.snapshot.node(name, "csv", value, dscope=scope)
	}, error = function(e) {
		# warning(paste("Attempted to write", name, "as .csv snapshot but failed. Out as RDataObject.", e))
		.ddg.dec("ddg.dnum")
		.ddg.snapshot.node(name, "txt", value, save.object = TRUE, dscope=scope)
	})
}

# .ddg.save.data takes as input the name and value of a data node 
# that needs to be created. It determines how the data should be 
# output (or saved) and saves it in that format.

# name - name of created node.
# value - value of created node.
# fname (optional) - name of calling function. Used to generate 
#   helpful error messages if something goes wrong.
# graphic.fext (optional) - file extension for graphic file.
# error (optional) - if TRUE, raise an R error rather than a
#   DDG error.
# scope (optional) - scope of node.
# stack (optional) - stack to use in determing scope.

.ddg.save.data <- function(name, value, fname=".ddg.save.data", graphic.fext='jpeg', error=FALSE, scope=NULL, stack=NULL){
  
  if (is.null(scope)) {
    scope <- .ddg.get.scope(name, calls=stack)
  }
	# Determine type for value, and save accordingly.
	if (.ddg.is.graphic(value)) .ddg.write.graphic(name, value, graphic.fext, scope=scope)
	else if (.ddg.is.simple(value)) .ddg.save.simple(name, value, scope=scope)
	else if (.ddg.is.csv(value)) .ddg.write.csv(name, value, scope=scope)
	else if (.ddg.is.object(value)) .ddg.snapshot.node(name, "txt", value, dscope=scope)
	else if (.ddg.is.function(value)) .ddg.save.simple(name, "#ddg.function", scope=scope)
	else if (error) stop("Unable to create data (snapshot) node. Non-Object value to", fname, ".")
	else {
		error.msg <- paste("Unable to create data (snapshot) node. Non-Object value to", fname, ".")
		.ddg.insert.error.message(error.msg)
	}
	invisible()
}

# .ddg.record.proc records a procedure node in the procedure node 
# table.

# ptype - procedure node type.
# pname - procedure node name.
# pvalue - procedure node value.
# auto.created - TRUE means the node is being created automatically when a return is found

.ddg.record.proc <- function(ptype, pname, pvalue, auto.created=FALSE) {
  # If the table is full, make it bigger.
  ddg.pnum <- .ddg.pnum()
  ddg.proc.nodes <- .ddg.proc.nodes()
  if (nrow(ddg.proc.nodes) < ddg.pnum) {
    size = 100
    new.rows <- data.frame(ddg.type = character(size),
        ddg.num = numeric(size),
        ddg.name = character(size),
        ddg.value = character(size), 
        ddg.return.linked = logical(size),
        ddg.auto.created = logical(size),
        stringsAsFactors=FALSE)
    .ddg.add.rows("ddg.proc.nodes", new.rows)
    ddg.proc.nodes <- .ddg.proc.nodes()
  }
  
  ddg.proc.nodes$ddg.type[ddg.pnum] <- ptype
  ddg.proc.nodes$ddg.num[ddg.pnum] <- ddg.pnum
  ddg.proc.nodes$ddg.name[ddg.pnum] <- pname
  ddg.proc.nodes$ddg.value[ddg.pnum] <- pvalue
  ddg.proc.nodes$ddg.auto.created[ddg.pnum] <- auto.created
  .ddg.set("ddg.proc.nodes", ddg.proc.nodes)
  
  if (.ddg.debug()) print (paste("Adding procedure node", ddg.pnum, "named", pname))
}

# .ddg.record.data records a data node in the data node table.

# dtype - data node type.
# dname - data node name.
# dvalue - data node value.
# dscope - data node scope.
# dtime (optional) - timestamp of original file.
# dloc (optional) -  path and name of original file.

.ddg.record.data <- function(dtype, dname, dvalue, dscope, dtime="", dloc="") {
  # If the table is full, make it bigger.
  ddg.dnum <- .ddg.dnum()
  ddg.data.nodes <- .ddg.data.nodes()
  if (nrow(ddg.data.nodes) < ddg.dnum) {
    size = 100
    new.rows <- data.frame(ddg.type = character(size),
        ddg.num = numeric(size),
        ddg.name = character(size),
        ddg.value = character(size),
        ddg.scope = character(size),                 
        ddg.time = character(size),
        ddg.loc = character(size),
        ddg.current = logical(size), stringsAsFactors=FALSE)
    .ddg.add.rows("ddg.data.nodes", new.rows)
    ddg.data.nodes <- .ddg.data.nodes()
  }
  
  ddg.data.nodes$ddg.type[ddg.dnum] <- dtype
  ddg.data.nodes$ddg.num[ddg.dnum] <- ddg.dnum
  ddg.data.nodes$ddg.name[ddg.dnum] <- dname
  ddg.data.nodes$ddg.value[ddg.dnum] <-
      if (length(dvalue) > 1 || !is.atomic(dvalue)) "complex"
      else if (!is.null(dvalue)) dvalue
      else ""
  ddg.data.nodes$ddg.scope[ddg.dnum] <- dscope
  ddg.data.nodes$ddg.time[ddg.dnum] <- dtime
  ddg.data.nodes$ddg.loc[ddg.dnum] <- dloc
  ddg.data.nodes$ddg.current[ddg.dnum] <- TRUE
  .ddg.set("ddg.data.nodes", ddg.data.nodes)
  
  if (.ddg.debug()) print(paste("Adding data node", ddg.dnum, "named", dname, "with scope", dscope))
}

# .ddg.is.proc.node returns TRUE if the specified type supports 
# input and output edges in an expanded DDG. Currently this 
# includes all procedure node types except Start.

# type - procedure node type.

.ddg.is.proc.node <- function(type) {
  return(type == "Operation" |
          type == "Checkpoint" | 
          type == "Restore" | 
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
    #if (ddg.proc.nodes$ddg.name[i] != "") {
    #  print (paste("Found proc node: ", ddg.proc.nodes$ddg.name[i]))
    #}
    if (.ddg.is.proc.node(type) & ddg.proc.nodes$ddg.name[i] == pname) {
      #print (paste0("Found a matching function for ", pname))
      if (!find.unreturned.function) {
        #print (paste0("Returning ", ddg.proc.nodes$ddg.num[i]))
        #print (sys.calls())
        return(ddg.proc.nodes$ddg.num[i])
      }
      
      if (find.unreturned.function & !ddg.proc.nodes$ddg.return.linked[i]) {
        #print (paste0("Returning ", ddg.proc.nodes$ddg.num[i]))
        #print (sys.calls())
        return(ddg.proc.nodes$ddg.num[i])
      }
    }
  }
  
  # Error message if no match is found.
  #print ("Returning error!")
  #print (sys.calls())
  error.msg <- paste("No procedure node found for", pname)
  .ddg.insert.error.message(error.msg)  
  #stop()
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

# .ddg.data.node.exists searches for a preceding current matching 
# data node. It returns TRUE if a match is found and FALSE 
# otherwise.

# dname - data node name.
# dscope - data node scope.

.ddg.data.node.exists <- function(dname, dscope) {
  if (is.null(dscope)) dscope <- .ddg.get.scope(dname)
  ddg.data.nodes <- .ddg.data.nodes()
  rows <- nrow(ddg.data.nodes)
  for (i in rows:1) {
    if (ddg.data.nodes$ddg.current[i]) {
      # if (ddg.data.nodes$ddg.name[i] == dname) return (TRUE) 
      if (ddg.data.nodes$ddg.name[i] == dname) {
        if (ddg.data.nodes$ddg.scope[i] == "ddg.library" || ddg.data.nodes$ddg.scope[i] == dscope) {
          return (TRUE)
        }
      }
    }
  }
  
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
      # if (ddg.data.nodes$ddg.name[i] == dname) {
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
    .ddg.append("CF p", ddg.pnum-1, " p", ddg.pnum, "\n", sep="")
    
    if (.ddg.debug()) {
      pname1 <- .ddg.proc.name(ddg.pnum-1)
      pname2 <- .ddg.proc.name(ddg.pnum)
      print(paste("proc2proc: ", pname1, " ", pname2))
      print(paste("CF p", ddg.pnum-1, " p", ddg.pnum, sep=""))
    }
  } 
  
  invisible() 
}

# .ddg.data2proc creates a data flow edge from a data node to a 
# procedure node.

# dname - data node name.
# dscope - data node scope.
# pname - procedure node name.

.ddg.data2proc <- function(dname, dscope, pname) {
  # Get data & procedure numbers.
  dn <- .ddg.data.number(dname, dscope)
  pn <- .ddg.proc.number(pname)
  
  # Create data flow edge from data node to procedure node.
  .ddg.append("DF d", dn, " p", pn, "\n", sep="")
  
  if (.ddg.debug()) {
    print(paste("data2proc: ", dname, " ", pname, sep=""))
    print(paste("DF d", dn, " p", pn, sep=""))
  }
  invisible()
}

# .ddg.proc2data creates a data flow edge from a procedure node to 
# a data node.

# pname - procedure node name.
# dname - data node name.
# dscope (optional) - data node scope.
# return.value (optiona) - if true it means we are linking to a return value.
#    In this case, we need to be sure that there is not already a return value
#    linked.  This is necessary to manage recursive functions correctly.

.ddg.proc2data <- function(pname, dname, dscope=NULL, return.value=FALSE) {
  # Get data & procedure numbers.
  dn <- .ddg.data.number(dname, dscope)
  #pn <- .ddg.proc.number(pname, find.unreturned.function=TRUE)
  pn <- .ddg.proc.number(pname, return.value)
  
  # Create data flow edge from procedure node to data node.
  if (dn != 0 && pn != 0) {
    .ddg.append("DF p", pn, " d", dn, "\n", sep="")
    
    # Record that the function is linked to a return value.  This
    # is necessary for recursive functions to get linked to their
    # return values correctly.
    if (return.value) {
      ddg.proc.nodes <- .ddg.proc.nodes()
      #print ("Marking return value as being used")
      #sys.calls()
      ddg.proc.nodes$ddg.return.linked[pn] <- TRUE
      .ddg.set("ddg.proc.nodes", ddg.proc.nodes)
    }
    
    if (.ddg.debug()) {
      print(paste("proc2data: ", pname, " ", dname, sep=""))
      print(paste("DF p", pn, " d", dn, sep=""))
    }
  }
  
  invisible()
}

# .ddg.lastproc2data creates a data flow edge from the last 
# procedure node to a data node.

# dname - data node name.
# all (optional) - whether all nodes should be considered (TRUE) 
#   or only procedure nodes (FALSE).

.ddg.lastproc2data <- function(dname, all=TRUE) {
  # Get data & procedure numbers.
  dn <- .ddg.data.number(dname)
  pn <- if(all) .ddg.pnum() else .ddg.last.proc.number()
  
  # Create data flow edge from procedure node to data node.
  .ddg.append("DF p", pn, " d", dn, "\n", sep="")
  
  if (.ddg.debug()) {
    print(paste("lastproc2data:", dname))
    print(paste("DF p", pn, " d", dn, sep=""))
  }
}

# .ddg.is.assign returns TRUE if the object passed is an expression 
# object containing an assignment statement.

# expr - input expression.

.ddg.is.assign <- function (expr) {
  if (is.call(expr)) {
    # This also finds uses of ->.
    if (identical(expr[[1]], as.name("<-")))
      return (TRUE)
    
    # This also finds uses of ->>.
    else if (identical(expr[[1]], as.name("<<-")))
      return (TRUE)
    else if (identical(expr[[1]], as.name("=")))
      return (TRUE)
    else if (identical(expr[[1]], as.name("assign")))
      return (TRUE)
  }
  return (FALSE)
}

# .ddg.get.var returns the variable being referenced in an 
# expression. It should be passed an expression object that is 
# either a variable, a vector access (like a[1]), a list member 
# (like a[[i]]) or a data frame access (like a$foo[i]).  For all of 
# these examples, it would return "a".

# lvalue - input expression.

.ddg.get.var <- function(lvalue) {
  if (is.symbol(lvalue)) deparse(lvalue)
  else .ddg.get.var(lvalue[[2]])
}

# ddg.is.functiondecl tests to see if an expression is a function 
# declaration.

# expr - input expression.

.ddg.is.functiondecl <- function(expr) {
  if (is.symbol(expr) || !is.language(expr)) return (FALSE)
  if (is.null(expr[[1]]) || !is.language(expr[[1]])) return (FALSE)
  return (expr[[1]] == "function")
}

# .ddg.find.assign returns a vector containing the names of all 
# the variables assigned in an expression.  The parameter should 
# be an expression object. For example, if obj represents the 
# expression "a <- (b <- 2) * 3", the vector returned will contain 
# both a and b.

# obj - input expression.

.ddg.find.assign <- function(obj) {
  # Base case.
  if (!is.recursive(obj)) return(character())
  
  # Assignment statement.  Add the variable being assigned to the 
  # vector and recurse on the expression being assigned.
  if (.ddg.is.assign(obj)) {
    var <- .ddg.get.var(obj[[2]])
    
    # Don't look for assignments in the body of a function as those 
    # won't happen until the function is called.
    # Don't recurse on NULL.
		if (!(is.null(obj[[3]]))) {
			if (.ddg.is.functiondecl(obj[[3]])) var
			else c(var, unlist(lapply(obj[[3]], .ddg.find.assign)))
		}
		else var 
	} 
	
	# Not an assignment statement.  Recurse on the parts of the 
  # expression.
  else {
    unique(unlist(lapply(obj, .ddg.find.assign)))
  }
}

# .ddg.find.simple.assign returns the name of the variable assigned 
# to if the object passed in is an expression representing an 
# assignment statement.  Otherwise, it returns NULL.

# obj - input expression.

.ddg.find.simple.assign <- function(obj) {
  if (.ddg.is.assign(obj)) {
    .ddg.get.var(obj[[2]])
  }
  else {
    NULL
  }
}

# .ddg.find.var.uses returns a vector containing all the variables 
# used in an expression.

# main.object - input expression.
# all (optional) - whether to return a vector of unique values 
#   (FALSE) or all values (TRUE).

.ddg.find.var.uses <- function(main.object, all=FALSE) {
	# Find function to filter results.
	filter <- if (all) identity else unique

	# Recursive helper function.
	.ddg.find.var.uses.rec <- function(obj) {
		# Base cases.
		if (is.name(obj)) return (deparse(obj))
		if (!is.recursive(obj)) return(character())
		##
		if (.ddg.is.functiondecl(obj)) return(character())
		
		tryCatch(
			if (.ddg.is.assign(obj)) {
				# If assigning to a simple variable, recurse on the right 
        # hand side of the assignment.
				if (is.symbol(obj[[2]])) filter(unlist(.ddg.find.var.uses.rec(obj[[3]])))
				else if (is.call(obj[[2]])) filter(c (.ddg.find.var.uses.rec(obj[[2]][[2]]), unlist(.ddg.find.var.uses.rec(obj[[3]]))))
				# If assigning to an expression (like a[b]), recurse on the 
	      # indexing part of the lvalue as well as on the expression.
				else filter(c (.ddg.find.var.uses.rec(obj[[2]][[3]]), unlist(.ddg.find.var.uses.rec(obj[[3]]))))
			} 
			
			# Not an assignment.  Recurse on all parts of the expression 
	    # except the operator.
			else {
				filter(unlist(lapply(obj[2:length(obj)], .ddg.find.var.uses.rec)))
			},
			error = function(e) {
				print (paste(".ddg.find.var.uses.rec:  Error analyzing", deparse(obj)))
				character()
			}
		)
	}

	return(.ddg.find.var.uses.rec(main.object))
}

# .ddg.create.empty.vars.set creates an empty data frame 
# initialized to contain information about variable assignments. 
# The difference between first.writer and possible.first.writer is 
# that first.writer is for simple assignments (like a <- 1), while 
# possible.first.writer is for situations where the assignment might 
# not have occurred, like "if (foo) a <- 1".

# The data frame is structured as follows 
# - the variable name.
# - the position of the statement that wrote the variable first.
# - the position of the statement that wrote the variable last.
# - the position of the first statement that may have assigned to a
#   variable .
# - the position of the last statement that may have assigned to a 
#   variable.

# var.table.size - desired size of the data frame. Negative values 
#   and 0 are coerced to 1.

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

#.ddg.increase.vars.set simply doubles the size of a variable 
# assignment data frame and returns the new one.

# vars.set - data frame containing variable assignments.
# size (optional) - number of rows in data frame.

.ddg.double.vars.set <- function(vars.set, size=nrow(vars.set)) {
	# Create the right size data frame from input frame.
	new.vars.set <- rbind(vars.set,.ddg.create.empty.vars.set(size))

	# Update first/last writer.
	new.vars.set$first.writer <- ifelse(new.vars.set$first.writer == size + 1, size*2 + 1, new.vars.set$first.writer)
	new.vars.set$possible.first.writer <- ifelse(new.vars.set$possible.first.writer == size + 1, size*2 + 1, new.vars.set$possible.first.writer)

	return(new.vars.set)
}

# .ddg.add.to.vars.set parses a command and adds the new variable 
# information to the variable assignment data frame. Note that 
# var.num is a global variable! It should be intialized when 
# vars.set is first created.

# vars.set - variable assignment data frame.
# cmd.expr - command expression.
# i - position of variable in data frame.

.ddg.add.to.vars.set <- function(vars.set, cmd.expr, i) {
  # Find out the variable being assigned to by a simple assignment 
  # statement.
  main.var.assigned <- .ddg.find.simple.assign(cmd.expr)
  
  # Find all the variables that may be assigned in the statement.
  vars.assigned <- .ddg.find.assign(cmd.expr)
  
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
			var.num <- .ddg.get("var.num")
			# Check space.
			size <- nrow(vars.set)
			if (var.num > size) vars.set <- .ddg.double.vars.set(vars.set,size)

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
			.ddg.inc("var.num")
		}
	}

	return(vars.set)
}


# .ddg.find.var.assigments finds the possible variable assignments 
# for a fixed set of parsed commands. See .ddg.create.empty.vars.set 
# for more information on the structure of the returned data frame.

# parsed.commands - a list of parsed commands.

.ddg.find.var.assignments <- function(parsed.commands) {
  if (length(parsed.commands) == 0) return (data.frame())
  
  # Make it big so we don't run out of space.
  var.table.size <- length(parsed.commands)
  vars.set <- .ddg.create.empty.vars.set(var.table.size)
  
  # Build the table recording where variables are assigned to or may 
  # be assigned to.
  .ddg.set("var.num", 1)
  for ( i in 1:length(parsed.commands)) {
    cmd.expr <- parsed.commands[[i]]
    vars.set <- .ddg.add.to.vars.set(vars.set,cmd.expr, i)
  }
  return (vars.set)
}


# .ddg.auto.graphic.node attempts to figure out if a new graphics 
# device has been created and take a snapshot of a previously active 
# device, setting the snapshot node to be the output of the 
# specified command.

# cmd.abbrev (optional) - name of procedure node.
# dev.to.capture (optional) - function specifying which device 
#   should be captured, where zero indicates no device and 
#   negative values are ignored.

.ddg.auto.graphic.node <- function(cmd.abbrev=NULL, dev.to.capture=.ddg.dev.change) {
	
	num.dev.to.capture <- dev.to.capture()
	if (num.dev.to.capture > 1) {
		# Make the capture device active (store info on previous 
    # device).
		prev.device <- dev.cur()
		dev.set(num.dev.to.capture)

		# Capture it as a jpeg.
		name <- if (!is.null(cmd.abbrev) && cmd.abbrev != "") paste0("graphic", substr(cmd.abbrev,1,10)) else "graphic"
		.ddg.snapshot.node(name, fext="jpeg", data=NULL)

		# Make the previous device active again.
		dev.set(prev.device)

		# We're done, so create the edge.
		if(is.null(cmd.abbrev)) .ddg.lastproc2data(name, all=FALSE)
		else .ddg.proc2data(cmd.abbrev, name)
	}
}

# .ddg.create.data.use.edges.for.console.cmd creates a data flow 
# edge from the node for each variable used in cmd.expr to the 
# procedural node labeled cmd, as long as the value would either 
# be one that exists prior to starting the console block, or 
# corresponds to the last setting of this variable in the console 
# block.

# vars.set - variable assignment data frame.
# cmd - name of procedure node.
# cmd.expr - command expression.
# cmd.pos - position of command.

.ddg.create.data.use.edges.for.console.cmd <- function (vars.set, cmd, cmd.expr, cmd.pos) {
  # Find all the variables used in this command.
  vars.used <- .ddg.find.var.uses(cmd.expr)
  
  for (var in vars.used) {
    # Make sure there is a node we could connect to.
    if (.ddg.data.node.exists(var, environmentName(.GlobalEnv))) {
      nRow <- which(vars.set$variable == var)
      
      # Check if the node is written in the console block.
      if (length(nRow) > 0) {
        first.writer <- min(vars.set$first.writer[nRow], vars.set$possible.first.writer[nRow])						
        last.writer <- max(vars.set$last.writer[nRow], vars.set$possible.last.writer[nRow])
        
        # Draw the edge if we will connect to a node that exists 
        # before the console block or to the last writer of this 
        # variable within the console block.
				if (cmd.pos <= first.writer || cmd.pos > last.writer) {
					.ddg.data2proc(var, environmentName(.GlobalEnv), cmd)
				}

				# TODO - add some sort of warning to the user that the node 
        # is not being created
			}
			
			# The variable is not set at all in this console block. 
      # Connect to a pre-existing data node.
			else {
				.ddg.data2proc(var, environmentName(.GlobalEnv), cmd)
			}
		}
		else {
			# TODO - add some sort of warning that the data node was NOT 
      # found.
      
			# error.msg <- paste("Unable to find data node for",var, ". Command", parse(text=cmd.expr), "appears to use it for procedure node", cmd, ".")
    	# .ddg.insert.error.message(error.msg)
		}
	}
}

# .ddg.create.data.set.edges.for.console.cmd creates the nodes and 
# edges that correspond to a console command assigning to a 
# variable. A data node is created for the last write of a variable 
# if that occurs after the last possible writer. A snapshot node 
# is created if the value is a data frame.  Otherwise, a data node 
# is created.

# vars.set - variable assignment data frame.
# cmd.abbrev - name of procedure node.
# cmd.expr - command expression.
# cmd.pos - position of command.
# for.finish.node (optional) - if TRUE, data edge is for finish 
#   node.


.ddg.create.data.set.edges.for.console.cmd <- function(vars.set, cmd.abbrev, cmd.expr, cmd.pos, for.finish.node=FALSE) {

  .ddg.create.data.set.edges.for.cmd(vars.set, cmd.abbrev, cmd.expr, cmd.pos, for.finish.node, scope=environmentName(.GlobalEnv), env=.GlobalEnv)
}

# .ddg.create.data.set.edges.for.cmd creates edgesthat correspond
# to a console command assigning to a variable.

# vars.set - variable assignment data frame.
# cmd.abbrev - name of procedure node.
# cmd.expr - command expression.
# cmd.pos - position of command.
# for.finish.node (optional) - if TRUE, data edge is for finish 
#   node.
# scope (optional) - scope of variable.
# env (optional) - environment to use for evaluating variable.
# stack (optional) - stack to use for evaluating variable.

.ddg.create.data.set.edges.for.cmd <- function(vars.set, cmd.abbrev, cmd.expr, cmd.pos, for.finish.node = FALSE, scope=NULL, env=NULL, stack=NULL) {
  vars.assigned <- .ddg.find.assign (cmd.expr)
  for (var in vars.assigned) {
    
    nRow <- which(vars.set$variable == var)
    
    # Only create a node edge for the last place that a variable is 
    # set within a console block.
		if ((length(nRow) > 0 && vars.set$last.writer[nRow] == cmd.pos && vars.set$possible.last.writer[nRow] <= vars.set$last.writer[nRow]) || for.finish.node) {
		    if (is.null(env)) {
		      env <- .ddg.get.env(var, calls=stack)
		      scope <- .ddg.get.scope(var, calls=stack)
		    }
		    val <- tryCatch(eval(parse(text=var), env),
					error = function(e) {NULL}
			)
			tryCatch(.ddg.save.data(var, val,fname=".ddg.create.data.set.edges.for.console.cmd", error=TRUE, scope=scope, stack=stack),
			         error = function(e){.ddg.data.node("Data", var, "complex", scope)})

#			if (!is.null(val)) {
#				if (is.data.frame(val)) .ddg.snapshot.node(var, "csv", val, dscope=environmentName(.GlobalEnv))
#				else .ddg.data.node("Data", var, val, environmentName(.GlobalEnv))
#			}
#			else {
#				.ddg.data.node("Data", var, "complex", environmentName(.GlobalEnv))
#			}
      .ddg.proc2data(cmd.abbrev, var, scope)
    }
  }
}

# .ddg.create.data.node.for.possible.writes creates a data node for 
# each variable that might have been set in something other than a 
# simple assignment.  An edge is created from the last node in the 
# console block.

# vars.set - variable assignment data frame.
# last.command - last command in console block.

.ddg.create.data.node.for.possible.writes <- function (vars.set, last.command, env= NULL) {
  environment <- if (is.environment(env)) env else .GlobalEnv
  for (i in 1:nrow(vars.set)) {
    if (vars.set$possible.last.writer[i] > vars.set$last.writer[i]) {
      value <- tryCatch(eval(parse(text=vars.set$variable[i]), environment),
          error = function(e) {NULL}
      )
# Only create the node and edge if we were successful in
# looking up the value.
      if (!is.null(value)) {
        .ddg.data.node("Data", vars.set$variable[i], value, environmentName(environment))
        .ddg.proc2data(last.command, vars.set$variable[i], environmentName(environment))
      }
    }
  }
}

# Given a parse tree, this function returns a list containing
# the expressions that correspond to the filename argument
# of the calls to functions that read or write the files.  If there are
# none, it returns NULL.
#
# main.object - the parsed expression to search through
# func.df - the data frame describing the functions with file arguments
.ddg.find.files <- function(main.object, func.df) {
  # Recursive helper function.
  find.files.rec <- function(obj) {
    #print (obj)
    # Base cases.
    if (!is.recursive(obj)) {
      return(NULL)
    }
    
    if (length(obj) == 0) {
      return(NULL)
    }
    ## It might be useful to record somehow that this function
    # reads a file, but we wouldn't actually do the reading
    # until the function is called, not here where it is
    # being declared.
    if (.ddg.is.functiondecl(obj)) return(NULL)
    
    if (is.call(obj)) {
      
      # Call has no arguments, so it can't be reading a function.  Recurse
      # on the first part, in case it is more than just a symbol.
      if (length (obj) == 1) return (find.files.rec (obj[[1]]))
      
      # Call with arguments
      else if (is.symbol (obj[[1]])) {
        # Is this is file reading function?
        read.func.pos <- match (as.character(obj[[1]]), func.df$function.names)
        if (!is.na (read.func.pos)) {
          # Find the file argument.
          arg.name <- func.df$param.names[read.func.pos]
          
          # Find a matching parameter passed by name
          file.name.arg.matches <- unlist(lapply (names(obj), function (arg) {return (pmatch (arg, arg.name))}))
          match.pos <- match (1, file.name.arg.matches)
          
          # If no argument qualified by the file parameter name, use the argument in the 
          # expected position
          if (is.na (match.pos)) {
            file.name <- eval(obj[[func.df$param.pos[read.func.pos]+1]])
          }
          else {
            file.name <- eval(obj[[match.pos]])
          }
          
          # Recurse over the arguments to the function.  We can't just skip over the 2nd
          # element since the filename parameter is not necessarily there if it was passed
          # by name.
          funcs <- find.files.rec (obj[2:length(obj)])
          
          # Add this file name to the list of files being read.
          unique (c (file.name, funcs))
        }
        
        # Not a file reading function.  Recurse over the arguments.
        else {
          find.files.rec (obj[2:length(obj)])
        }
      }
      
      # Function call, but the first list element is not simply a function name.  Recurse
      # over all the list elements.
      else {
        unique (append (find.files.rec (obj[[1]]), find.files.rec (obj[2:length(obj)])))
      }
    } 
    
    # A recursive structure that is not a call.  Not sure if there are any, but just in case...
    else if (length(obj) == 1) {
      unique (find.files.rec (obj[[1]]))
    }
    else {
      unique (append (find.files.rec (obj[[1]]), find.files.rec (obj[2:length(obj)])))      
    }
  }
  
  return(find.files.rec(main.object))
}

# Initialize the information about functions that read from files
.ddg.create.file.read.functions.df <- function () {
  # Functions that read files
  function.names <-
    c ("source", "read.csv", "read.csv2", "read.delim", "read.delim2", "read.table", "read.xls")
  
  # The argument that represents the file name
  param.names <-
    c ("file", "file", "file", "file", "file", "file", "xls")
  
  # Position of the file parameter if it is passed by position
  param.pos <- 
    c (1, 1, 1, 1, 1, 1, 1)
  
  return (data.frame (function.names, param.names, param.pos, stringsAsFactors=FALSE))
}

.ddg.set (".ddg.file.read.functions.df", .ddg.create.file.read.functions.df ())

# Given a parse tree, this function returns a list containing
# the expressions that correspond to the filename argument
# of the calls to functions that read from files.  If there are
# none, it returns NULL.
.ddg.find.files.read <- function(main.object) {
  return (.ddg.find.files (main.object, .ddg.get(".ddg.file.read.functions.df")))
}
  
# Creates file nodes and data in edges for any files that are read in this cmd
# cmd - text command
# cmd.expr - parsed command
.ddg.create.file.read.nodes.and.edges <- function (cmd, cmd.expr) {
  # Find all the files potentially read in this command.
  # This may include files that are not actually read if the 
  # read are within an if-statement, for example.
  files.read <- .ddg.find.files.read(cmd.expr)
  #print ("Files read:")
  #print (files.read)
  
  for (file in files.read) { 
    # Only create the node and edge if there actually is a file
    # Even if the file exists, it is possible that it was not read here
    if (file.exists(file)) {
      # Create the file node and edge      
      ddg.file (file)
      ddg.data.in (basename(file), pname=cmd)
    }
    else if (grepl ("^http", file) || grepl ("^ftp", file)) {
      scope <- environmentName(.GlobalEnv)
      .ddg.data.node("URL", file, file, scope)
      .ddg.data2proc(file, scope, cmd)
    }
  }
}

# Initialize the information about functions that read from files
.ddg.create.file.write.functions.df <- function () {
  # Functions that read files
  function.names <-
    c ("write.csv", "write.csv2", "write.table")
  
  # The argument that represents the file name
  param.names <-
    c ("file", "file", "file")
  
  # Position of the file parameter if it is passed by position
  param.pos <- 
    c (2, 2, 2)
  
  return (data.frame (function.names, param.names, param.pos, stringsAsFactors=FALSE))
}

.ddg.set (".ddg.file.write.functions.df", .ddg.create.file.write.functions.df ())

# Given a parse tree, this function returns a list containing
# the expressions that correspond to the filename argument
# of the calls to functions that write files.  If there are
# none, it returns NULL.
.ddg.find.files.written <- function(main.object) {
  return (.ddg.find.files (main.object, .ddg.get(".ddg.file.write.functions.df")))
}

# Creates file nodes and data in edges for any files that are written in this cmd
# cmd - text command
# cmd.expr - parsed command
.ddg.create.file.write.nodes.and.edges <- function (cmd, cmd.expr) {
  # Find all the files potentially written in this command.
  # This may include files that are not actually written if the 
  # write calls are within an if-statement, for example.
  files.written <- .ddg.find.files.written(cmd.expr)
  #print ("Files written:")
  #print (files.written)
  
  for (file in files.written) {
    # Check that the file exists.  If it does, we will assume that 
    # it was created by the write call that we just found.
    if (file.exists (file)) {
      # Create the file node and edge
      ddg.file.out (file, pname=cmd)
    }
  }
}

# Initialize the information about functions that initialize graphics devices
.ddg.create.graphics.functions.df <- function () {
  # Functions that read files
  function.names <-
    c ("pdf", "postscript", "bmp", "jpeg", "png", "tiff")
  
  # The argument that represents the file name
  param.names <-
    c ("file", "file", "filename", "filename", "filename", "filename")
  
  # Position of the file parameter if it is passed by position
  param.pos <- 
    c (1, 1, 1, 1, 1, 1)
  
  return (data.frame (function.names, param.names, param.pos, stringsAsFactors=FALSE))
}

.ddg.set (".ddg.graphics.functions.df", .ddg.create.graphics.functions.df ())

# Given a parse tree, this function returns a list containing
# the expressions that correspond to the filename argument
# of the calls to functions that create graphics devices.  If there are
# none, it returns NULL.
.ddg.set.graphics.files <- function(main.object) {
  # Find all the graphics files that have potentially been opened.
  # Remember these file names until we find the dev.off call and then
  # determine which was written.
  new.possible.graphics.files.open <- .ddg.find.files (main.object, .ddg.get(".ddg.graphics.functions.df"))
  if (!is.null(new.possible.graphics.files.open)) {
    if (.ddg.is.set ("possible.graphics.files.open")) {
      possible.graphics.files.open <- .ddg.get ("possible.graphics.files.open")
      .ddg.set ("possible.graphics.files.open", 
                c (new.possible.graphics.files.open, possible.graphics.files.open))      
    }
  
    else {
      .ddg.set ("possible.graphics.files.open", new.possible.graphics.files.open)
    }
  
  }
}

.ddg.has.dev.off.call <- function(main.object) {
  # Recursive helper function.
  has.dev.off.call.rec <- function(obj) {
    #print (obj)
    # Base cases.
    if (!is.recursive(obj)) {
      return(FALSE)
    }
    
    if (length(obj) == 0) {
      return(FALSE)
    }
    ## It might be useful to record somehow that this function
    # reads a file, but we wouldn't actually do the reading
    # until the function is called, not here where it is
    # being declared.
    if (.ddg.is.functiondecl(obj)) return(FALSE)
    
    if (is.call(obj)) {
      #print("Found call")
      
      if (is.symbol (obj[[1]])) {
        # Is this a call to dev.off?
        if (as.character(obj[[1]]) == "dev.off") return (TRUE)
        
        else if (length (obj) == 1) return (FALSE)
        
        else return (has.dev.off.call.rec (2:length(obj)))
        }
        
      else return (has.dev.off.call.rec (obj[[1]]) || has.dev.off.call.rec (2:length(obj)))
        }
    
    else return (has.dev.off.call.rec (obj[[1]]) || has.dev.off.call.rec (2:length(obj)))
      }
      
  return(has.dev.off.call.rec(main.object))
    } 
    
.ddg.capture.graphics <- function(cmd) {
  if (!.ddg.is.set ("possible.graphics.files.open")) return
  possible.graphics.files.open <- .ddg.get ("possible.graphics.files.open")
  
  # Find the most recent file
  if (is.null(possible.graphics.files.open)) return

  graphics.file.info <- file.info(possible.graphics.files.open)
  latest.file.date.row <- which.max (graphics.file.info$mtime)
  
  ddg.file.out (possible.graphics.files.open[latest.file.date.row], pname=cmd)
  .ddg.set ("possible.graphics.files.open", NULL)
}

# .ddg.abbrev.cmd abbreviates a command to the specified length.
# Default is 60 characters.

# cmd - command string.
# len (optional) - number of characters.

.ddg.abbrev.cmd <- function(cmd, len=60) {
  if (nchar(cmd) <= len) cmd
  else if (substr(cmd, len, len) != "\\") substr(cmd, 1, len)
  else if (substr(cmd, len-1, len) == "\\\\") substr(cmd, 1, len)
  else substr(cmd, 1, len-1)
}

# .ddg.loadhistory takes in the name of a history file, opens it, 
# scans it for the last occurrence of the string specified by 
# timestamp, and returns the lines from that point forward.

# hist.file - name of history file.
# timestamp - timestamp string.

.ddg.loadhistory <- function(hist.file, timestamp) {
	# Read from specified file.
	history <- readLines(hist.file)
	history.lines <- length(history)
	
	# Find the timestamp specified in the history.  There may be 
  # more than one with the same timestamp, so pick the last of 
  # these.
	history.timestamp.line <- tail(which(history == timestamp), 1)
	
	if (length(history.timestamp.line) == 0) {
		error.msg <- paste("Part of history is missing. DDG may be incomplete! Tried reading from",
		                   hist.file, "but could not find timestamp:", timestamp)

    .ddg.insert.error.message(error.msg)
		history.timestamp.line <- 0
	}

	# Need to check if the timestamp line is the last line in the file
	# explicitly.  If we don't do that and take the vector, we will 
	# get the last line in the file since R will create a descending 
  # sequence for the vector.
	if (history.timestamp.line == history.lines) return (vector())

	# NEED the paren around sum.
	return(history[(history.timestamp.line+1):history.lines])
}

# .ddg.savehistory saves the current and unsaved R command history
# to the specified file if that file matches the DDG history file.
# Note: the commented section of code appends the information to 
# this file.

# hist.file - name of history file.

.ddg.savehistory <- function(hist.file) {

	# USED TO STORE ENTIRE HISTORY IN SEP. FILE.
	# Write history out to temporary file
  
	# ddg.grab.timestamp <- .ddg.get(".ddg.grab.timestamp.history")
	# ddg.tmp.history.file <- paste(hist.file,".tmp", sep="")

	if (.ddg.is.set(".ddg.history.file") && 
	    is.character(.ddg.get(".ddg.history.file")) &&
	    .ddg.get(".ddg.history.file") == hist.file) {
		savehistory(hist.file)
	}

	# USED TO STORE ENTIRE HISTORY IN SEP. FILE.
	# Read in changes and writ eout to extended file.
  
	# newlines <- .ddg.loadhistory(ddg.tmp.history.file,ddg.grab.timestamp)
	# write(newlines, file=hist.file, append=TRUE)
	# insert timestamp to history 
	# .ddg.write.timestamp.to.history(var=".ddg.grab.timestamp.history")
}


# .ddg.link.function.returns determines if the command calls a 
# function for which ddg.return has created a node for the return 
# value.  If so, a data flow edge is created from the return value 
# data node to the finish node for the command.  Note that if the
# assignment is an expression, like "d <- f(a) + f(b)", there may 
# be multiple return value nodes to link to.

# command - input command.

.ddg.link.function.returns <- function(command) {
	# Find the functions that have completed but whose returns have 
  # not been used yet.
	returns <- .ddg.get(".ddg.return.values")
	unused.returns <- returns[!returns$return.used & returns$return.node.id > 0, ]
  if (nrow(unused.returns) == 0) return()
  
	# See which of these are called from the command we are 
  # processing now.
	unused.calls <- unused.returns$ddg.call
  command <- gsub(" ", "", command)
	uses <- sapply(unused.calls, function(call) {grepl(call, command, fixed=TRUE)})
	
	# The following line is here to get around R CMD check, which 
  # otherwise reports:  no visible binding for global variable.
	# Note that return.node.id is not a variable in the subset call, 
  # but the name of a column in the data frame being subsetted.
	return.node.id <- NULL
	
	# Extracts for the return value nodes.
	new.uses <- subset(unused.returns, uses, return.node.id)
  
	# Create an edge from each of these to the last procedure node.
	lapply (new.uses$return.node.id, 
			function(data.num) {
				proc.num <- .ddg.pnum()
				
				# Create data flow edge from procedure node to data node.
				.ddg.append("DF d", data.num, " p", proc.num, "\n", sep="")
				
				if (.ddg.debug()) {
					print(paste(".ddg.link.function.returns:", command))
					print(paste("DF d", data.num, " p", proc.num, sep=""))
				}
				
				# Set the return value as being used.
				returns$return.used[returns$return.node.id == data.num] <- TRUE
				.ddg.set(".ddg.return.values", returns)
			})
}

# ddg.add.abstract.node is exclusively used in .ddg.parse.commands 
# (so far) and simply serves to avoid repetition of code.

# type - type of procedure node.
# cmd - command string.
# called (optional) - name of calling function.

.ddg.add.abstract.node <- function(type, cmd, called=".ddg.parse.commands") {
  cmd.abbrev <- .ddg.abbrev.cmd(cmd)
  if (.ddg.debug()) print(paste(called, ":  Adding", cmd.abbrev,  type, "node"))
  .ddg.proc.node(type, cmd.abbrev, cmd.abbrev, TRUE)
  .ddg.proc2proc()
  
  return(cmd.abbrev)
}

# .ddg.close.last.command.node closes the last created collapsible 
# node stored in .ddg.last.cmd properly.

# called (optional) - used in debugging to identify the function 
#   which called .ddg.close.previous.command.
# initial (optional) - if TRUE, try to close previous command node.

.ddg.close.last.command.node <- function(called=".ddg.parse.commands", initial=FALSE){
	# Get both the last command and new commands.
	.ddg.last.cmd <- .ddg.get(".ddg.last.cmd")
	.ddg.possible.last.cmd <- .ddg.get(".ddg.possible.last.cmd")

	# Only create a finish node if a new command exists (i.e., we've 
  # parsed some lines of code).
	if (!is.null(.ddg.last.cmd) && (!is.null(.ddg.possible.last.cmd) || initial)) {
		cmd.abbrev <- .ddg.add.abstract.node("Finish", .ddg.last.cmd$abbrev,called=paste(called, "-> .ddg.close.last.command.node"))

		# Add link from a function return node if there is one.
		.ddg.link.function.returns(.ddg.last.cmd$text)
		
		# Create outflowing edges.
		vars.set <- .ddg.find.var.assignments(.ddg.last.cmd)
		.ddg.create.data.set.edges.for.console.cmd(vars.set, .ddg.last.cmd$abbrev, .ddg.last.cmd$expr, 1, for.finish.node = TRUE)

		# No previous command.
		.ddg.set(".ddg.last.cmd", NULL)
	}
}

# .ddg.open.new.command.node opens a new collapsible command 
# node depending on the information stored in .ddg.last.cmd.

# called (optional) - name of calling function.

.ddg.open.new.command.node <- function(called=".ddg.parse.commands") {
  new.command <- .ddg.get(".ddg.possible.last.cmd")
	if (!is.null(new.command)) {
		.ddg.add.abstract.node("Start", new.command$abbrev, called=paste(called, "-> .ddg.open.new.command.node"))

		# Now the new command becomes the last command, and new command 
    # is null.
		.ddg.set(".ddg.last.cmd", new.command)
		.ddg.set(".ddg.possible.last.cmd", NULL)
	}
}

# .ddg.is.procedure.cmd returns TRUE if the command passed in 
# (as a string) will create a procedure node and therefore 
# initiate the creation of a collapsible console node.

# cmd.str - command string.

.ddg.is.procedure.cmd <- function(cmd.str) {
  return(grepl("^ddg.(procedure|start|finish|restore|checkpoint)", cmd.str))
}

# .ddg.parse.lines takes as input a set of lines corresponding to 
# the history of an R script or to an R script itself. It parses 
# and converts them to executable commands. Each command might span 
# multiple lines. The function returns a named list of commands.

# The contents of the list are: 
#   text - each entry is the full text string of a single command. 
#   commands - each entry is the parsed command.

# lines - set of lines from command history or R script.

.ddg.parse.lines <- function(lines) {
  # No new lines passed in, so return NULL.
  if (length(lines) == 0) return(NULL)
  
  # Parse the new lines.
  parsed.commands <- parse(text=lines)
  parsed.commands <- Filter(function(cmd) {return (!is.call(cmd) || !grepl("^ddg.eval", cmd[[1]]))}, parsed.commands)
  return(parsed.commands)
}

.ddg.extract.param.from.ddg.eval <- function (cmd.text) {
  # Turn the ddg.eval call into a string
  deparsed.cmd <- cmd.text
  
  # Extract the argument passed to ddg.eval
  deparsed.cmd <- sub("ddg.eval[:space:]*\\([:space:]*\"", "", deparsed.cmd)
  deparsed.cmd <- sub("\"[:space:]*)", "", deparsed.cmd)
  
  # Parse the expression to be evaluated
  updated.cmd <- list("expr" = parse(text=deparsed.cmd)[[1]],
      "abbrev" = .ddg.abbrev.cmd(gsub("\\\"", "\\\\\"", deparsed.cmd)), 
      "text" = deparsed.cmd)
  
  return(updated.cmd)
}

# .ddg.parse.commands takes as input a set of R script commands 
# and creates DDG nodes for each command. If environ is an 
# environment, it executes the commands in that environment
# immediately before creating the respective nodes for that 
# command, and then creates the data nodes based on the information 
# available in the environment. If environ is not NULL, calls to 
# ddg.* are not executed so only the clean script is processed. 

# parsed.commands - set of R script commands.
# environ (optional) - environment in which commands should be
#   executed.
# ignore.patterns (optional) - a vector of regular expressions. 
#   Any commands matching these expressions will not be parsed 
#   (i.e. no nodes will be created for them).
# node.name (optional) - name for the collapsible node under which 
#   this DDG should be stored.
# run.commands (optional) - commands are executed only when environ 
#   is an environment and run.commands is TRUE.
# echo (optional) - print each expression after parsing
# print.eval (optional) - print result of each evaluation.
# max.deparse.length (optional) - maximum number of characters 
#   output for deparse of a single expression.

.ddg.parse.commands <- function(parsed.commands, environ=NULL, ignore.patterns=c('^ddg.'), node.name="Console", run.commands = FALSE, echo=FALSE, print.eval=echo, max.deparse.length=150) {
	
	# Figure out if we will execute commands or not.

  execute <- run.commands & !is.null(environ) & is.environment(environ)

	# It is possible that a command may extend over multiple lines. 
  # new.commands will have one string entry for each parsed command.
	new.commands <- lapply(parsed.commands, function(cmd) {paste(deparse(cmd), collapse="")})
  filtered.commands <- Filter(function(x){return(!grepl("^ddg.", x))}, new.commands)
  
  # Attempt to close the previous collapsible command node if a ddg 
  # exists
  if (.ddg.is.init()) .ddg.close.last.command.node(initial=TRUE)
  
  # Create start and end nodes to allow collapsing of consecutive 
  # console nodes. Don't bother doing this if there is only 1 new 
  # command in the history or execution.
  num.new.commands <- length(new.commands)
  
  # Quote the quotation (") characters so that they will appear in 
  # ddg.txt.
  quoted.commands <- gsub("\\\"", "\\\\\"", new.commands)

  # Get the last command in the new commands and check to see if 
  # we need to create a new .ddg.last.cmd node for future reference.
  .ddg.last.cmd <- list("abbrev" = .ddg.abbrev.cmd(quoted.commands[[num.new.commands]]), 
      "expr" = parsed.commands[[num.new.commands]],
      "text" = new.commands[[num.new.commands]])

  if (substr(.ddg.last.cmd$abbrev, 1, 4) == "ddg.") {
    .ddg.last.cmd <- NULL
  }
  else if (!execute) {
    quoted.commands <- quoted.commands[1:num.new.commands-1]
    parsed.commands <- parsed.commands[1:num.new.commands-1]
    new.commands <- new.commands[1:num.new.commands-1]
  }
  
  filtered.commands <- Filter(function(x){
        return(!grepl("^ddg.", x))}, new.commands)
  
  # Create start and end nodes to allow collapsing of consecutive 
  # console nodes. Don't bother doing this if there is only 1 new 
  # command in the history or execution.
  named.node.set <- FALSE
  start.node.created <- ""
  num.actual.commands <- length(filtered.commands)
  # 
  if (num.actual.commands > 0 && .ddg.is.init()) {
    .ddg.add.abstract.node("Start", node.name)
    named.node.set <- TRUE
    start.node.created <- node.name
  }

  # Don't set .ddg.last.cmd.  We want it to have the value from 
  # the last call. We set it at the end of this function:
  # .ddg.set(".ddg.last.cmd", .ddg.last.cmd)
  
  # We tried to use a data frame to contain new.commands, 
  # quoted.commands and parsed.commands, but it does not seem 
  # possible to put the parsed expressions in a data frame.
  
  # Create an operation node for each command.  We can't use lapply 
  # here because we need to process the commands in order and 
  # lapply does not guarantee an order. Also decide which data nodes 
  # and edges to create. Only create a data node for the last 
  # write of a variable and only if that occurs after the last 
  # possible writer. Create an edge for a data use as long as the 
  # use happens before the first writer/possible writer or after 
  # the last writer/possible writer. Lastly, if environ is set to 
  # true, then execute each command immediately before attempting 
  # to create the DDG nodes. 
  
  # Only go through this if  we have at least one command to parse.
  if (length(parsed.commands) > 0) {
    # Find where all the variables are assigned for non-environ 
    # files.
    if (!execute) {
      vars.set <- .ddg.find.var.assignments(parsed.commands)
    } 
    else {
      .ddg.set("var.num", 1)
      vars.set <- .ddg.create.empty.vars.set()
    }
    
    # Loop over the commands as well as their string representations.
    for (i in 1:length(parsed.commands)) {
      cmd.expr <- parsed.commands[[i]]
      cmd.text <- new.commands[[i]]
      cmd <- quoted.commands[[i]]
      cmd.abbrev <- .ddg.abbrev.cmd(cmd)
      
      if (.ddg.enable.source() && grepl("^ddg.eval", cmd.expr) && .ddg.enable.console()) {    
        update.last.cmd <- is.null(.ddg.last.cmd)
        updated.cmd <- .ddg.extract.param.from.ddg.eval(cmd.text)
        
        cmd <- updated.cmd$abbrev
        cmd.expr <- updated.cmd$expr
        cmd.text <- updated.cmd$text
        
        if (update.last.cmd) {
          .ddg.last.cmd <- list("abbrev"=cmd.abbrev, "expr"=cmd.expr, "text"=cmd)
        }
      }
      
      # Specifies whether or not a procedure node should be created 
      # for this command. Basically, if a ddg exists and the 
      # command is not a DDG command, it should be created.
      
      create <- !grepl("^ddg.", cmd) && .ddg.is.init() && .ddg.enable.console()
      
      # If the command does not match one of the ignored patterns.
      if (!any(sapply(ignore.patterns, function(pattern){grepl(pattern, cmd)}))) {        
        cmd.abbrev <- .ddg.abbrev.cmd(cmd)
        
        # If sourcing, we want to execute the command.
        if (execute) {
          # Print command.
          if (echo) {
            nd <- nchar(cmd)
            do.trunc <- nd > max.deparse.length
            cmd.show <- paste0(substr(cmd, 1L, if (do.trunc) 
                        max.deparse.length
                        else nd), "\n")
            cat(cmd.show)
          }
          
          # If we will create a node, then before execution, set 
          # this command as a possible abstraction node but only 
          # if it's not a call that itself creates abstract nodes.
          if (!grepl("^ddg.", cmd)) {
            .ddg.set(".ddg.possible.last.cmd", list("abbrev"=cmd.abbrev, "expr"=cmd.expr, "text"=cmd.text))
          }
          else if (.ddg.is.procedure.cmd(cmd)) .ddg.set(".ddg.possible.last.cmd", NULL)
          
          # Evaluate.
          result <- eval(cmd.expr, environ, NULL)
          
          # Print evaluation.
          if (print.eval) print(result)
          
          # Check if initialization call. If so, then create a 
          # new console node, but only if the next command is NOT 
          # a DDG command.
          
          # if(grepl("^ddg.init", cmd) && .ddg.enable.console()) { 
          #	.ddg.add.abstract.node("Start", "Console")
          #	.ddg.set(".ddg.last.cmd", list(text="Console",expr="Console"))
          # }
        }
        
        # Figure out if we should create a procedure node for this 
        # command. We don't create it if it matches a last command 
        # (because that last command has now become a collapsible 
        # node). Matching a last command means that the last command 
        # is set, is not NULL, and is equal to the current command.
        
        create.procedure <- create && !(!is.null(.ddg.get(".ddg.last.cmd")) && .ddg.get(".ddg.last.cmd")$text == cmd.text) && (!named.node.set || start.node.created != cmd.text)
        
        # We want to create a procedure node for this command.
        if (create.procedure) {
          
          # Create the procedure node.
          .ddg.proc.node("Operation", cmd.abbrev, cmd.abbrev, console=TRUE)
          .ddg.proc2proc()
          if (.ddg.debug()) print(paste(".ddg.parse.commands: Adding operation node for", cmd.abbrev))
          
          # Store information on the last procedure node in this 
          # block.
          last.proc.node <- cmd.abbrev
          
          # We want to create the incoming data nodes (by updating 
          # the vars.set).
          if (execute) {
            # Add variables to set.
            vars.set <- .ddg.add.to.vars.set(vars.set,cmd.expr,i)
            if (.ddg.debug()) print(paste(".ddg.parse.commands: Adding", cmd.abbrev, "information to vars.set"))
          }
          
          .ddg.create.data.use.edges.for.console.cmd(vars.set, cmd.abbrev, cmd.expr, i)
          .ddg.create.file.read.nodes.and.edges(cmd.abbrev, cmd.expr)          
          .ddg.link.function.returns(cmd.text)
          
          if (.ddg.debug()) print(paste(".ddg.parse.commands: Adding input data nodes for", cmd.abbrev))
          .ddg.create.data.set.edges.for.console.cmd(vars.set, cmd.abbrev, cmd.expr, i)
          if (.ddg.debug()) print(paste(".ddg.parse.commands: Adding output data nodes for", cmd.abbrev))
          .ddg.create.file.write.nodes.and.edges (cmd.abbrev, cmd.expr)
          .ddg.set.graphics.files (cmd.expr)  
          if (.ddg.has.dev.off.call(cmd.expr)) {
            .ddg.capture.graphics(cmd.abbrev)
          }
        }
        # We wanted to create it but it matched a last command node.
        else if (create && execute) .ddg.close.last.command.node(initial=TRUE)
        
        ###### TODO #######
        if (execute) {
          .ddg.create.data.node.for.possible.writes(vars.set, last.proc.node, env=environ)
          
          # Update so we don't set these again.
          vars.set$possible.last.writer <- vars.set$last.writer
        }
      }
    }
    
    # Create a data node for each variable that might have been set in 
    # something other than a simple assignment, with an edge from the 
    # last node in the console block or source .
    if (!execute) .ddg.create.data.node.for.possible.writes(vars.set, last.proc.node, env=environ)
  }
  
  # Close any node left open during execution.
  if (execute) .ddg.close.last.command.node(initial=TRUE)
  
  # Close the console block if we processed anything and the DDG 
  # is initialized (also, save).
  # 
  if (.ddg.is.init() && named.node.set) { 
    .ddg.add.abstract.node("Finish", node.name)
  }
  
  # Open up a new collapsible node in case we need to parse 
  # further later.
  if (!execute) {
		
    .ddg.set(".ddg.possible.last.cmd", .ddg.last.cmd)
    .ddg.set(".ddg.last.cmd", .ddg.last.cmd)
    .ddg.open.new.command.node()
  }
  
  # Write time stamp to history.
  if (.ddg.is.init()) .ddg.write.timestamp.to.history()
  # print(paste("last.commad:",.ddg.get(".ddg.last.cmd")))
  # print(paste("command:", .ddg.get(".ddg.possible.last.cmd")))
  
}

.ddg.parse.source.commands <- function(parsed.commands, environ=NULL, ignore.patterns=c('^ddg.'), node.name="Console", run.commands = FALSE, echo=FALSE, print.eval=echo, max.deparse.length=150) {
  print ("In .ddg.parse.source.commands")
  # Figure out if we will execute commands or not.
  
  execute <- run.commands & !is.null(environ) & is.environment(environ)
  
  # It is possible that a command may extend over multiple lines. 
  # new.commands will have one string entry for each parsed command.
  new.commands <- lapply(parsed.commands, function(cmd) {paste(deparse(cmd), collapse="")})
  filtered.commands <- Filter(function(x){return(!grepl("^ddg.", x))}, new.commands)
  
  # Attempt to close the previous collapsible command node if a ddg 
  # exists
  if (.ddg.is.init()) .ddg.close.last.command.node(initial=TRUE)
  
  # Create start and end nodes to allow collapsing of consecutive 
  # console nodes. Don't bother doing this if there is only 1 new 
  # command in the history or execution.
  num.new.commands <- length(new.commands)
  
  # Quote the quotation (") characters so that they will appear in 
  # ddg.txt.
  quoted.commands <- gsub("\\\"", "\\\\\"", new.commands)
  
  # Get the last command in the new commands and check to see if 
  # we need to create a new .ddg.last.cmd node for future reference.
  .ddg.last.cmd <- list("abbrev" = .ddg.abbrev.cmd(quoted.commands[[num.new.commands]]), 
      "expr" = parsed.commands[[num.new.commands]],
      "text" = new.commands[[num.new.commands]])
  
  if (substr(.ddg.last.cmd$abbrev, 1, 4) == "ddg.") {
    .ddg.last.cmd <- NULL
  }
  else if (!execute) {
    quoted.commands <- quoted.commands[1:num.new.commands-1]
    parsed.commands <- parsed.commands[1:num.new.commands-1]
    new.commands <- new.commands[1:num.new.commands-1]
  }
  
  filtered.commands <- Filter(function(x){
        return(!grepl("^ddg.", x))}, new.commands)
  
  # Create start and end nodes to allow collapsing of consecutive 
  # console nodes. Don't bother doing this if there is only 1 new 
  # command in the history or execution.
  named.node.set <- FALSE
  start.node.created <- ""
  num.actual.commands <- length(filtered.commands)
  # 
  if (num.actual.commands > 0 && .ddg.is.init()) {
    .ddg.add.abstract.node("Start", node.name)
    named.node.set <- TRUE
    start.node.created <- node.name
  }
  
  # Don't set .ddg.last.cmd.  We want it to have the value from 
  # the last call. We set it at the end of this function:
  # .ddg.set(".ddg.last.cmd", .ddg.last.cmd)
  
  # We tried to use a data frame to contain new.commands, 
  # quoted.commands and parsed.commands, but it does not seem 
  # possible to put the parsed expressions in a data frame.
  
  # Create an operation node for each command.  We can't use lapply 
  # here because we need to process the commands in order and 
  # lapply does not guarantee an order. Also decide which data nodes 
  # and edges to create. Only create a data node for the last 
  # write of a variable and only if that occurs after the last 
  # possible writer. Create an edge for a data use as long as the 
  # use happens before the first writer/possible writer or after 
  # the last writer/possible writer. Lastly, if environ is set to 
  # true, then execute each command immediately before attempting 
  # to create the DDG nodes. 
  
  # Only go through this if  we have at least one command to parse.
  if (length(parsed.commands) > 0) {
    # Find where all the variables are assigned for non-environ 
    # files.
    if (!execute) {
      vars.set <- .ddg.find.var.assignments(parsed.commands)
    } 
    else {
      .ddg.set("var.num", 1)
      vars.set <- .ddg.create.empty.vars.set()
    }
    
    # Loop over the commands as well as their string representations.
    for (i in 1:length(parsed.commands)) {
      cmd.expr <- parsed.commands[[i]]
      cmd.text <- new.commands[[i]]
      cmd <- quoted.commands[[i]]
      cmd.abbrev <- .ddg.abbrev.cmd(cmd)
      
      if (.ddg.enable.source() && grepl("^ddg.eval", cmd.expr) && .ddg.enable.console()) {    
        update.last.cmd <- is.null(.ddg.last.cmd)
        updated.cmd <- .ddg.extract.param.from.ddg.eval(cmd.text)
        
        cmd <- updated.cmd$abbrev
        cmd.expr <- updated.cmd$expr
        cmd.text <- updated.cmd$text
        
        if (update.last.cmd) {
          .ddg.last.cmd <- list("abbrev"=cmd.abbrev, "expr"=cmd.expr, "text"=cmd)
        }
      }
      
      # Specifies whether or not a procedure node should be created 
      # for this command. Basically, if a ddg exists and the 
      # command is not a DDG command, it should be created.
      
      create <- !grepl("^ddg.", cmd) && .ddg.is.init() && .ddg.enable.console()
      
      # If the command does not match one of the ignored patterns.
      if (!any(sapply(ignore.patterns, function(pattern){grepl(pattern, cmd)}))) {        
        cmd.abbrev <- .ddg.abbrev.cmd(cmd)
        
        # If sourcing, we want to execute the command.
        if (execute) {
          # Print command.
          if (echo) {
            nd <- nchar(cmd)
            do.trunc <- nd > max.deparse.length
            cmd.show <- paste0(substr(cmd, 1L, if (do.trunc) 
                          max.deparse.length
                        else nd), "\n")
            cat(cmd.show)
          }
          
          # If we will create a node, then before execution, set 
          # this command as a possible abstraction node but only 
          # if it's not a call that itself creates abstract nodes.
          if (!grepl("^ddg.", cmd)) {
            .ddg.set(".ddg.possible.last.cmd", list("abbrev"=cmd.abbrev, "expr"=cmd.expr, "text"=cmd.text))
          }
          else if (.ddg.is.procedure.cmd(cmd)) .ddg.set(".ddg.possible.last.cmd", NULL)
          
          # Evaluate.
          result <- eval(cmd.expr, environ, NULL)
          
          # Print evaluation.
          if (print.eval) print(result)
          
          # Check if initialization call. If so, then create a 
          # new console node, but only if the next command is NOT 
          # a DDG command.
          
          # if(grepl("^ddg.init", cmd) && .ddg.enable.console()) { 
          #	.ddg.add.abstract.node("Start", "Console")
          #	.ddg.set(".ddg.last.cmd", list(text="Console",expr="Console"))
          # }
        }
        
        # Figure out if we should create a procedure node for this 
        # command. We don't create it if it matches a last command 
        # (because that last command has now become a collapsible 
        # node). Matching a last command means that the last command 
        # is set, is not NULL, and is equal to the current command.
        
        create.procedure <- create && !(!is.null(.ddg.get(".ddg.last.cmd")) && .ddg.get(".ddg.last.cmd")$text == cmd.text) && (!named.node.set || start.node.created != cmd.text)
        
        # We want to create a procedure node for this command.
        if (create.procedure) {
          
          # Create the procedure node.
          .ddg.proc.node("Operation", cmd.abbrev, cmd.abbrev, console=TRUE)
          .ddg.proc2proc()
          if (.ddg.debug()) print(paste(".ddg.parse.commands: Adding operation node for", cmd.abbrev))
          
          # Store information on the last procedure node in this 
          # block.
          last.proc.node <- cmd.abbrev
          
          # We want to create the incoming data nodes (by updating 
          # the vars.set).
          if (execute) {
            # Add variables to set.
            vars.set <- .ddg.add.to.vars.set(vars.set,cmd.expr,i)
            if (.ddg.debug()) print(paste(".ddg.parse.commands: Adding", cmd.abbrev, "information to vars.set"))
          }
          
          .ddg.create.data.use.edges.for.console.cmd(vars.set, cmd.abbrev, cmd.expr, i)
          .ddg.create.file.read.nodes.and.edges(cmd.abbrev, cmd.expr)
          .ddg.link.function.returns(cmd.text)
          
          if (.ddg.debug()) print(paste(".ddg.parse.commands: Adding input data nodes for", cmd.abbrev))
          .ddg.create.data.set.edges.for.console.cmd(vars.set, cmd.abbrev, cmd.expr, i)
          if (.ddg.debug()) print(paste(".ddg.parse.commands: Adding output data nodes for", cmd.abbrev))
          .ddg.create.file.write.nodes.and.edges (cmd.abbrev, cmd.expr)          
          .ddg.set.graphics.files (cmd.expr)          
          if (.ddg.has.dev.off.call(cmd.expr)) {
            print ("Found dev.off")
          }
          
        }
        # We wanted to create it but it matched a last command node.
        else if (create && execute) .ddg.close.last.command.node(initial=TRUE)
        
        ###### TODO #######
        if (execute) {
          .ddg.create.data.node.for.possible.writes(vars.set, last.proc.node, env=environ)
          
          # Update so we don't set these again.
          vars.set$possible.last.writer <- vars.set$last.writer
        }
      }
    }
    
    # Create a data node for each variable that might have been set in 
    # something other than a simple assignment, with an edge from the 
    # last node in the console block or source .
    if (!execute) .ddg.create.data.node.for.possible.writes(vars.set, last.proc.node, env=environ)
  }
  
  # Close any node left open during execution.
  if (execute) .ddg.close.last.command.node(initial=TRUE)
  
  # Close the console block if we processed anything and the DDG 
  # is initialized (also, save).
  # 
  if (.ddg.is.init() && named.node.set) { 
    .ddg.add.abstract.node("Finish", node.name)
  }
  
  # Open up a new collapsible node in case we need to parse 
  # further later.
  if (!execute) {
    
    .ddg.set(".ddg.possible.last.cmd", .ddg.last.cmd)
    .ddg.set(".ddg.last.cmd", .ddg.last.cmd)
    .ddg.open.new.command.node()
  }
  
  # Write time stamp to history.
  if (.ddg.is.init()) .ddg.write.timestamp.to.history()
  # print(paste("last.commad:",.ddg.get(".ddg.last.cmd")))
  # print(paste("command:", .ddg.get(".ddg.possible.last.cmd")))
  
}

# .ddg.console.node creates a console node.

# ddg.history.file (optional) - path and name of the history file 
#   which should be loaded.
# ddg.history.timestamp (optional) - timestamp from which the 
#   history file should be parsed (parsing begins at the next line).
#  env (optional) - the environment in which console commands are  
#   evaluated. This is typically the global environment.

.ddg.console.node <- function(ddg.history.file=.ddg.get(".ddg.history.file"),
    ddg.history.timestamp=.ddg.get(".ddg.history.timestamp"),
    env = NULL) {
  # Don't do anything if sourcing, because history isn't necessary 
  # in this case.
  if(.ddg.enable.source()) return(NULL)
  
  # Only continue if these values exists.
  if (!(is.null(ddg.history.file) || is.null(ddg.history.timestamp))) {
    # Grab any new commands that might still be in history.
    .ddg.savehistory(ddg.history.file)
    
    # Load from extended history since last time we wrote out
    # a console node.
    new.lines <- .ddg.loadhistory(ddg.history.file,ddg.history.timestamp)
    
    # Parse the lines into individual commands.
    parsed.commands <- .ddg.parse.lines(new.lines)
    
    # New commands since last timestamp.
    if (!is.null(parsed.commands) && length(parsed.commands) > 0) {
      .ddg.parse.commands(parsed.commands, 
          environ = env,
          run.commands=FALSE)
    }
  }
}

# .ddg.proc.node creates a procedure node.

# ptype - type of procedure node.
# pname - name of procedure node.
# pvalue (optional) - value of procedure node.
# console (optional) - if TRUE, console mode is enabled.
# auto.created - TRUE means that the node is being automatically created when a return call is found

.ddg.proc.node <- function(ptype, pname, pvalue="", console=FALSE, auto.created=FALSE) {
  
  # We're not in a console node but we're capturing data 
  # automatically.
  if (.ddg.enable.console()) {
    
    # Capture graphic output of previous procedure node.
    .ddg.auto.graphic.node()

    if(!console) {
      # We're sourcing, so regardless of interactivity, capture 
      # commands.
      if (.ddg.enable.source()) {
        .ddg.close.last.command.node(called=".ddg.proc.node")
        .ddg.open.new.command.node(called=".ddg.proc.node")
      }
      # Running interactively, so parse command history by making 
      # a console node.
      else if (interactive()) .ddg.console.node()
    }
  }
  
  # Increment procedure counter.
  .ddg.inc("ddg.pnum")
  
  # Include value if available.
  proc.value <- 
      if (pvalue!="") paste(" Value=\"", pvalue, "\"", sep="")
      else ""
  
  # Obtain the timestamp to  use this procedure node.
  proc.time <- paste0(" Time=\"", .ddg.timestamp(), "\"")
  
  # Create procedure node.  
  ddg.pnum <- .ddg.pnum()
  
  if (proc.value != "") {
    .ddg.append(ptype, " p", ddg.pnum, " \"", ddg.pnum, "-", pname, "\"", proc.value, proc.time, ";\n", sep="")
  }
  else {
    .ddg.append(ptype, " p", ddg.pnum, " \"", ddg.pnum, "-", pname, "\"", proc.time, "\n", sep="")
  }
  
  # Record procedure node information.
  .ddg.record.proc(ptype, pname, pvalue, auto.created)
  
  if (.ddg.debug()) print(paste("proc.node:", ptype, pname))
}

# .ddg.replace.quotes quotes quotation characters. It also replaces 
# return, newline and tab characters with spaces.

# str - input string.

.ddg.replace.quotes <- function(str) {
  if (!is.character(str)) return (str)
  
  str <- paste("\"", str, "\"", sep="")
  str <- gsub("\"", "\\\\\"", str)
  
  # Replace returns, new lines, and tabs with spaces.
  str <- gsub("\r", " ", str)
  str <- gsub("\n", " ", str)
  str <- gsub("\t", " ", str)
}

# .ddg.convert.list.to.string converts a list of values to a string
# by calling as.character on each element in the list.

# dvalue - a list of values.

.ddg.convert.list.to.string <- function (dvalue) {
  values <- .ddg.replace.quotes(lapply(dvalue, .ddg.as.character))
  positions <- 1:length(values)
  paste("[[", positions, "]]", values, collapse="\n")
}

# .ddg.as.character wraps an exception handler around as.character
# The exception handler captures the print output for the value and
# returns that instead.
.ddg.as.character <- function (value) {
  tryCatch (as.character(value),
      error=function(e) {capture.output(print(value))})
}

# .ddg.data.node creates a data node of type Data. Data nodes are 
# used for single data values. The value (dvalue) is stored in the 
# DDG.

# dtype - type of data node.
# dname - name of data node.
# dvalue - value of data node.
# dscope - scope of data node.

.ddg.data.node <- function(dtype, dname, dvalue, dscope) {
  # If object, try to create snapshot node.
  if (is.object(dvalue)) {
    tryCatch(
        {
          .ddg.snapshot.node (dname, "txt", as.character(dvalue), dscope=dscope)
          return(NULL)
        },
        error = function(e) {
          error.msg <- paste("Unable to create snapshot node for", dname)
          .ddg.insert.error.message(error.msg)
          .ddg.dec("ddg.dnum")
          return (.ddg.data.node (dtype, dname, "complex", dscope))
        }
    )
    
  }
  
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
      else if (length(dvalue) > 1 || !is.atomic(dvalue)) {
        tryCatch(paste(.ddg.replace.quotes(dvalue), collapse=","),
            error = function(e) {"complex"})
      }
      else if (is.null(dvalue)) "NULL"
      else if (is.na(dvalue)) "NA"
      else if (dvalue == "complex" || dvalue == "#ddg.function") dvalue
      else if (is.character(dvalue) && dvalue == "") "NotRecorded"
      else {
        # Replace double quotes with single quotes.
        .ddg.replace.quotes(dvalue)
      }
  
  if (grepl("\n", val)) {
    # Create snapshot node.
    .ddg.snapshot.node (dname, "txt", val)
    return
  }
  
  else {
    # Get scope if necessary.
    if (is.null(dscope)) dscope <- .ddg.get.scope(dname)
    
    # Create data node.
    data.value <- paste(" Value=\"", val, "\"", sep="")
    
    # Increment data counter.
    .ddg.inc("ddg.dnum")
    
    # Add node to DDG.
    ddg.dnum <- .ddg.dnum()
    .ddg.append(dtype, " d", ddg.dnum, " \"", ddg.dnum, "-", dname, "\"", data.value, ";\n", sep="")
    
    # Record data node information.
    .ddg.record.data(dtype, dname, dvalue, dscope)
    
    if (.ddg.debug()) print(paste("data.node:", dtype, dname))
  }
  
  invisible()
}

# .ddg.supported.graphic - the sole purpose of this function is 
# to verify that the input file extension is a supported graphic 
# type. Currently supported graphics types inlude: jpg, jpeg, 
# bmp, png, tiff.

# ext - file extension.

.ddg.supported.graphic <- function(ext){
  return(ext %in% c("jpeg", "jpg", "tiff", "png", "bmp", "pdf"))
}

# .ddg.graphic.snapshot provides factoring of snapshot code.

# fext - file extension.
# dpfile - path and name of file.

.ddg.graphic.snapshot <-function(fext, dpfile) {
	# pdfs require a separate procedure.
	if (fext == "pdf") dev.copy2pdf(file=dpfile)

	# At the moment, all other graphic types can be done by 
  # constructing a similar function.
	else {
		# If jpg, we need to change it to jpeg for the function call.
		fext = ifelse(fext == "jpg", "jpeg", fext)

		# First, we create a string, then convert it to an actual R 
    # expression and use that as the function.
		strFun <- paste(fext, "(filename=dpfile, width=800, height=500)", sep="")
		parseFun <- function(){eval(parse(text=strFun))}
		dev.copy(parseFun)
	
		# Turn it off (this switches back to prev device).
		dev.off()
	}
}

# .ddg.snapshot.node creates a data node of type Snapshot. Snapshots 
# are used for complex data values not written to file by the main 
# script. The contents of data are written to the file dname.fext 
# in the DDG directory. Snapshots are also used to capture output 
# plots and other graphics generated by the R script.

# The user can control the size of the snapshot files by setting the
# max.snapshot.size parameter when calling ddg.init or ddg.run.  If 
# the user passes in 0, no snapshots are saved.
# Instead a data node will be created.  If the user passes in -1,
# there is no limit on the snapshot size.  If the user passes a value > 0,
# if the R value is larger than this size, only the head of the data will
# be saved.

# dname - name of data node.
# fext - file extension.
# data - value of data node.
# save.object (optional) - if TRUE, also save as an R object.
# dscope (optional) - scope of data node.

.ddg.snapshot.node <- function(dname, fext, data, save.object = FALSE, dscope=NULL) {
  # Determine if we should save the entire data
  max.snapshot.size <- .ddg.get(".ddg.max.snapshot.size") 
  if (max.snapshot.size == 0) {
    return(.ddg.data.node ("Data", dname, "", dscope))
  }
  
  # Increment data counter.
  .ddg.inc("ddg.dnum")
  
  # Get file name.
  ddg.dnum <- .ddg.dnum()
  
  # If the object is an environment, update the data to be the environment's name
  # followed by a list of the variables bound in the environment
  if (is.environment (data)) {
    envHeader <- paste0 ("<environemnt: ", environmentName (data), ">")
    data <- c (envHeader, ls(data), recursive=TRUE)
  }
  
  # object.size returns bytes, but max.snapshot.size is in kilobytes
  if (max.snapshot.size == -1 || object.size(data) < max.snapshot.size * 1024) {
    full.snapshot <- TRUE
    snapname <- dname
  }
  else {
    data <- head(data)
    full.snapshot <- FALSE
    snapname <- paste(dname, "-PARTIAL", sep="")
  }
  
  # Default file extensions.
  dfile <- 
      if (fext == "" || is.null(fext)) paste(ddg.dnum, "-", snapname, sep="")
      else                             paste(ddg.dnum, "-", snapname, ".", fext, sep="")
  
  # Get path plus file name.
  ddg.path <- .ddg.path()
  dpfile <- paste(ddg.path, "/", dfile, sep="")
  if (.ddg.debug()) print(paste("Saving snapshot in ", dpfile))
  
  # Write to file .
  if (fext == "csv") write.csv(data, dpfile, row.names=FALSE)
  
  # Capture graphic.
  else if (.ddg.supported.graphic(fext)) .ddg.graphic.snapshot(fext, dpfile)
  
  # Write out RData (this is old code, not sure if we need it).
  else if (fext == "RData") file.rename(paste(ddg.path, "/", dname, sep=""), dpfile)
  
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
  if (save.object && full.snapshot) save(data, file = paste(dpfile, ".RObject"), ascii = TRUE)
  
  dtime <- .ddg.timestamp()
  
  # Get scope if necessary.
  if (is.null(dscope)) dscope <- .ddg.get.scope(dname)
  
  # Create file node.
  .ddg.append("Snapshot", " d", ddg.dnum, " \"", ddg.dnum, "-", dname, "\" Value=\"", dpfile, "\" Time=\"", dtime, "\";\n", sep="")
  
  # Record data node information.
  .ddg.record.data("Snapshot", dname, dfile, dscope, dtime)
  
  if (.ddg.debug()) print(paste("snapshot.node: ", dname))
  return(dpfile)
}

# .ddg.file.node creates a node of type File. File nodes are used 
# for files written to the DDG directory by capturing output from 
# the script or by copying a file that is written by the script. 
# Returns the path where the file referenced by the node is stored.

# dtype - type of data node.
# fname - path and name of original file.
# dname - name of data node.
# dscope (optional) - scope of data node.

.ddg.file.node <- function(dtype, fname, dname, dscope=NULL) {
	# Increment data counter.
	.ddg.inc("ddg.dnum")
	
	# Get original file location.
	file.name <- basename(fname)
	file.loc <- normalizePath(fname, winslash="/", mustWork = FALSE)
	
	loc.value <- 
		if (dtype == "File") paste(" Location=\"", file.loc, "\"", sep="") 
		else ""

	# Add number to file name.
	ddg.dnum <- .ddg.dnum()
	dfile <- paste(ddg.dnum, "-", file.name, sep="")

	# Get path plus file name.
	dpfile.out <- paste(.ddg.path(), "/", dfile, sep="")

	dtime <- .ddg.timestamp()

	# Set the node label.
	if (is.null(dname)) dname <- file.name
	
	# Get scope if necessary.
	if (is.null(dscope)) dscope <- .ddg.get.scope(dname)
  
  # Create file node.
  .ddg.append(dtype, " d", ddg.dnum, " \"", ddg.dnum, "-", dname, "\" Value=\"", dpfile.out, "\" Time=\"", dtime, "\"", loc.value, ";\n", sep="")
  
  # Record data node information.
  .ddg.record.data(dtype, dname, dfile, dscope, dtime, file.loc)
  
  return(dpfile.out)
}

# .ddg.file.copy creates a data node of type File. File nodes are 
# used for files written by the main script. A copy of the file is 
# written to the DDG directory.

# dtype - type of data node.
# fname - path and name of original file.
# dname - name of data node.
# dscope - scope of data node.
 
.ddg.file.copy <- function(dtype, fname, dname, dscope) {
	# Calculate location of original file.
	file.loc <- normalizePath(fname, winslash="/", mustWork = FALSE)

	# Copy file.
	if (file.exists(file.loc)) {
	  # Create file node in DDG.
	  dpfile.out <- .ddg.file.node(dtype,fname,dname, dscope)    
    file.copy(file.loc, dpfile.out, overwrite=TRUE)
  }
  else {
    error.msg <- paste("File to copy does not exist:", fname) 
    .ddg.insert.error.message(error.msg)
    return(NULL)
  }
  
  if (.ddg.debug()) print(paste("file.copy: ", dtype, " ", file.loc))
  return (dpfile.out)
}

# .ddg.insert.error.message issues a warning and inserts an 
# exception node after the last procedure step. The name of the node 
# is "error.msg" and the value is the error message passed to this 
# function.

# msg - error message.

.ddg.insert.error.message <- function(msg) {
  warning(msg)
  .ddg.data.node("Exception", "error.msg", msg, "ddg.library")
  .ddg.lastproc2data("error.msg")
}

# .ddg.lookup.function.name gets the name of the calling function 
# and returns it as a string. pname may be passed as a string or
# a name. If NULL, pname is obtained from the calling environment.
# Note that it is important that these be macros, not functions, 
# due to the use of the substitute function in the body.

# pname - name of procedure node.

.ddg.lookup.function.name <- gtools::defmacro (pname,
		expr = 
				# If pname is not provided, get from function call.
				if (is.null(pname)) {
					# Look up function call.
					call <- sys.call(-4)
					
					# Discard everything after left parenthesis to get 
          # function name.
					
          # pname <- strsplit (as.character(call), "\\(")[[1]][1]
					pname <- as.character(call[[1]])
				}
				
				# Convert pname to a string if necessary.
				else if (!is.character(pname)) pname <- deparse(substitute(pname))
)

# .ddg.lookup.value is used to determine what value to use when 
# creating data nodes.  Note that it is important that these be 
# macros, not functions, due to the use of the substitute function 
# in the body.

# expr - the expression to be evaluted. This can be a string or 
#   a name.
# value - the value that was passed in to the calling function. 
#   If value already exists, nothing happens. If value is NULL, 
#   the expression is evaluated to determine the value.
# env - the environment in which the evaluation is done. 
# procname - the name of the calling procedure, used to produce 
#   an error message if necessary.
# warn (optional) - if TRUE, warns user that the expression could 
#   not be evaluated.

.ddg.lookup.value <- gtools::defmacro(expr, value, env, procname, warn=TRUE,
		expr = 	
				if (is.null(value)) {
					arg <- substitute(expr)
					if (is.character(arg)) {
						tryCatch (arg <- parse(text=expr),
						error = function(e) {})
					}
					else expr <- deparse(arg)
					value <- tryCatch (
							eval(arg, env),
							error = function(e) {
								# if (is.character(expr)) return (expr)
								if (warn) {
                  error.msg <- paste("Unable to evaluate", expr, "in call to", procname)
                  .ddg.insert.error.message(error.msg)
                }
                return ("")
              }
          )
        }
) 

# .ddg.delete.temp deletes any temporary files created during 
# the processing of a script. These include the temporary 
# history file.

.ddg.delete.temp <- function() {
	# Delete the temporary history file if we made it.
	if (.ddg.is.set('ddg.history.file')) unlink(.ddg.get('ddg.history.file'))

	# Clear the environment.
	.ddg.env <- new.env(parent=emptyenv())
}

# .ddg.create.output.nodes creates output nodes for ddg.function
# and ddg.procedure. Outs values must be passed as strings, not 
# names, unless the value is a file name.

# fname - the name of the function calling .ddg.create.output.nodes.
# pname - the name of the procedure node.
# outs.graphic - the name of a snapshot node to be used as a 
#    file name.  A graphical snapshot is simply a captured image 
#    of the graphic device active at the time of the call to 
#    ddg.function or ddg.procedure.
# outs.data - a list of names of data nodes.
# outs.exception - a list of names of exception nodes.
# outs.url - a list of names of url nodes.
# outs.file - a list of names of file nodes. Supported file 
#   extensions include: .csv, .jpg, .jpeg, .pdf, and .txt.
# graphic.fext - the file extension to be used when saving the 
#   captured graphic. Supported extensions are .jpg, .jpeg, .pdf.

.ddg.create.output.nodes<- function(fname, pname, outs.graphic, outs.data, outs.exception, outs.url, outs.file, graphic.fext, env) {
  
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
          .ddg.lookup.value(name, value, env, fname, warn=FALSE)
          
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
          .ddg.lookup.value(name, value, env, fname, warn=FALSE)
          
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
          .ddg.lookup.value(name, value, env, fname, warn=FALSE)
          
          tryCatch({
                if (!is.character(name)) name <- deparse(substitute(name))
                envName <- environmentName(env)
                scope <- .ddg.get.scope(param, calls=stack)
                .ddg.save.data(name,value, fname, error=TRUE, scope=scope)
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
          .ddg.lookup.value(name, value, env, fname, warn=FALSE)
          scope <- .ddg.get.scope(param, calls=stack)
          
          if (value == "") {
            # Filename passed as value.          
            .ddg.file.copy("File", name, name, scope)
            .ddg.proc2data(pname, name, scope)
          }
          else {
            # Filename passed as name.
            .ddg.file.copy("File", value, name, scope)
            .ddg.proc2data(pname, name, scope)
          }
        }
    )
  }
}

# .ddg.create.function.nodes creates the procedure node, input 
# binding nodes, and output nodes for the function.

# pname - name of procedure node.
# full.call - full function call.
# outs.data, etc (optional) - output nodes.
# auto.created - TRUE if the function node is begin created automatically when a return is found

.ddg.create.function.nodes <- function(pname, full.call, outs.graphic=NULL, outs.data=NULL, outs.exception=NULL, outs.url=NULL, outs.file=NULL, graphic.fext="jpeg", auto.created=FALSE) {
  # Tokens will contain the function name and the argument
  # expressions.
  
  # Get parameters and create edges.
  if (length(full.call) > 1) {
    # args contains the names of the variable that was passed into 
    # the function.
    args <- full.call[2:length(full.call)]

    # param,names contains the names of the parameters (this is 
    # what the variable is known as inside the function).
    param.names <- names(full.call)
    param.names <- param.names[2:length(param.names)]
    stack <- sys.calls()
    # scope <- .ddg.get.scope(args[[1]], for.caller = TRUE)
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
               binding.node.name <- paste(formal, " <- \\\"", arg, "\\\"", sep="")
             }
             else {
               vars.used <- .ddg.find.var.uses(arg)
               binding.node.name <- paste(formal, " <- ", deparse(arg))
             }
             
             .ddg.proc.node("Binding", binding.node.name)
             .ddg.proc2proc()
             for (var in vars.used) {
               param.scope <- .ddg.get.scope(var, for.caller = TRUE, calls=stack)
               if (.ddg.data.node.exists(var, param.scope)) {
                 .ddg.data2proc(as.character(var), param.scope, binding.node.name)
                 if (.ddg.debug()) print(paste("param:", var))
               }
             }
             if (formal != "...") {
             formal.scope <- .ddg.get.scope(formal, calls=stack)
             formal.env <- .ddg.get.env(formal, calls=stack)
             .ddg.save.data(formal, eval(parse(text=formal), formal.env), fname=".ddg.save.data", scope=formal.scope, stack=stack)
             .ddg.proc2data(binding.node.name, formal, formal.scope)
             }
           })
  }
  .ddg.proc.node("Operation", pname, pname, auto.created = auto.created)
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
  
  .ddg.create.output.nodes(fname="ddg.function", pname, outs.graphic, outs.data, outs.exception, outs.url, outs.file, graphic.fext, parent.frame(2))
  
}

# .ddg.get.frame.number gets the frame number of the closest
# non-library calling function.

# calls - system calls.
# for.caller (optional) - if TRUE, go up one level before searching.

.ddg.get.frame.number <- function(calls, for.caller=FALSE) {
  if (is.null(calls)) calls <- sys.calls()
  script.func.found <- FALSE
  nframe <- length(calls)
  for (i in nframe:1) {
    call.func <- as.character(sys.call(i)[[1]])    
    if (substr(call.func, 1, 4) != ".ddg" && substr(call.func, 1, 3) != "ddg") {
      if (for.caller && !script.func.found) {
        script.func.found <- TRUE
      }
      else {
        return(i)
      }
    }
  }
  return(0)
}

# .ddg.where looks up the environment for the variable specified
# by name.  Adapted from Hadley Wickham, Advanced R programming.

# name - name of variable.
# env (optional) - environment in which to look for variable.

.ddg.where <- function(name, env=parent.frame()) {
  stopifnot(is.character(name), length(name) == 1)
  if (identical(env, emptyenv())) {
    # stop("Can't find ", name, call.=FALSE)
    warning("Can't find ", name)
    return("undefined")
  }
  if (exists(name, env, inherits=FALSE)) {
    env
  }
  else {
    .ddg.where(name, parent.env(env))
  }
}

#.ddg.get.env gets the environment in which name is declared.

# name - variable name.
# for.caller (optional) - if TRUE, go up one level before searching.
# calls (optional) - system calls.

.ddg.get.env <- function(name, for.caller=FALSE, calls=NULL) {
  if (is.null(calls)) calls <- sys.calls()
  fnum <- .ddg.get.frame.number(calls, for.caller)
  stopifnot(!is.null(fnum))
  
  # This statement was broken into two statements so that we
  # can add print statements to .ddg.where or step through it
  # with a debugger without breaking it.  If we don't do that
  # the print output gets captured by capture.output and
  # does not display to the user and also causes the subsequent
  # grepl call in this function to fail.
  
  #	scope <- sub('<environment: (.*)>', '\\1', capture.output(.ddg.where(name, sys.frame(fnum))))
  if(!exists(name, sys.frame(fnum), inherits=TRUE)) return(NULL)
  env <- .ddg.where(name, sys.frame(fnum))
  return(env)
}

# .ddg.get.scope gets the id of the closest non-library
# environment.

# name - name of variable.
# for.caller (optional) - if TRUE, go up one level before searching.
# calls (optional) - system calls.

.ddg.get.scope <- function(name, for.caller=FALSE, calls=NULL) {
	# Get the environment for the variable call.
	env <- .ddg.get.env(name, for.caller, calls)

	# If no environment found, name does not exist, so scope is 
  # undefined.
  if (is.null(env)) return ("undefined")
  
  # 
  scope <- sub('^<environment: (.*)>$', '\\1', capture.output(env)[1])
  if (grepl("undefined", scope)) scope <- "undefined"
  return(scope)
}

# .ddg.is.local returns TRUE if the specified name is local in the 
# specified scope.

# name of variable.
# scope of variable.

.ddg.is.local <- function(name, scope) {
  return(exists(name, scope, inherits=FALSE))
}

#--------------------USER FUNCTIONS-----------------------#

# ddg.function creates a procedure node of type Operation for 
# procedures implemented as functions in the original R script. 
# The function name and input parameters are obtained automatically 
# from the calling environment. The outs parameters may be used 
# optionally to create output data nodes. These MUST be passed as 
# a list of strings, not names, unless the value is a file name. 
# Users can right-click on the procedure node in DDG Explorer to 
# see the code for the function in the original script. For more
# details on outs parameters, see .ddg.create.output.nodes.

# outs (optional) - a list of names of data nodes to be created as 
#    outputs to this procedure node. These MUST be passed as
#    a list of strings, not names, unless the value is a file name.
# graphic.fext (optional) - the file extension for a graphics file.

ddg.function <- function(outs.graphic=NULL, outs.data=NULL, outs.exception=NULL, outs.url=NULL, outs.file=NULL, graphic.fext="jpeg") {
  if (!.ddg.is.init()) return(invisible())
  
  pname <- NULL
  .ddg.lookup.function.name(pname)
  
  if (interactive() && .ddg.enable.console()) .ddg.console.node()
  
  # Look up input parameters from calling environment.
  call <- sys.call(-1)
  
  #tokens <- unlist(strsplit (as.character(call), "[(,)]"))
  # match.call expands any argument names to be the 
  # full parameter name
  full.call <- match.call(sys.function(-1), call=call)
  
  .ddg.create.function.nodes(pname, full.call, outs.graphic, outs.data, outs.exception, outs.url, outs.file, graphic.fext)
  
  invisible()
}

# ddg.procedure creates a procedure node of type Operation for 
# procedures not implemented as functions in the original R script.
# For more details on outs parameters, see .ddg.create.output.nodes.

# pname - the label for the node. Can be passed in as a string 
#    or as a name.  
# ins (optional) - a list of names of data nodes to be linked 
#    as inputs to this procedure node. These MUST be passed as 
#    as a list of strings, not names, unless the value is a 
#    file name.
# outs (optional) - a list of names of data nodes to be created as 
#    outputs to this procedure node. These MUST be passed as
#    a list of strings, not names, unless the value is a file name.
# graphic.fext (optional) - file extension for graphics file.

ddg.procedure <- function(pname, ins=NULL, outs.graphic=NULL, outs.data=NULL, outs.exception=NULL, outs.url=NULL, outs.file=NULL, graphic.fext="jpeg") {

  if (!.ddg.is.init()) return(invisible())
  
  .ddg.lookup.function.name(pname)
  
  .ddg.proc.node("Operation", pname, pname)
  
  # Create control flow edge from preceding procedure node.
  .ddg.proc2proc()
  
  # Create the input edges if ins list provided.
  if (!is.null(ins)) {
    # Get scope.  Cannot use lapply because that results in the 
    # creation of a new stack, while .ddg.get.scope assumes that 
    # it is called.
    # scope <- .ddg.get.scope(ins[[1]], for.caller = TRUE)
    stack <- sys.calls()
    function.scope <- parent.frame()
    
    lapply(ins, 
        function(param) {
          # First see if param is defined in the function where 
          # ddg.procedure is called. If that does not find it, 
          # look in the scope of the function that calls the 
          # function that calls ddg.procedure.  The difference 
          # probably depends on whether ddg.procedure is used to 
          # annotate a chunk of code where param is really in the 
          # local scope but used in the annotated chunk (the first 
          # case) or to annotate an entire function and param is 
          # an actual funciton parameter (the second case).
          scope <- .ddg.get.scope(param, calls=stack)
          if (.ddg.is.local(param, function.scope)) {
            if (.ddg.data.node.exists(param, scope)) {
              .ddg.data2proc(param, scope, pname)
              if (.ddg.debug()) print(paste("param:", param))
            }
            else {
              error.msg <- paste("No data node found for local", param)	
              .ddg.insert.error.message(error.msg)
            }
          }
          else if (scope != "undefined" && .ddg.data.node.exists(param, scope)) {
            .ddg.data2proc(param, scope, pname)
            if (.ddg.debug()) print(paste("param:", param))
          }
          else {
            scope <- .ddg.get.scope(param, for.caller = TRUE, calls=stack)
            if (scope != "undefined" && .ddg.data.node.exists(param, scope)) {
              .ddg.data2proc(param, scope, pname)
              if (.ddg.debug()) print(paste("param:", param))
            }
            
            else if (.ddg.data.node.exists(param, "undefined")) {
              # This could be the case if the parameter is the name 
              # of a file rather than a variable in the program.
              .ddg.data2proc(param, "undefined", pname)
              if (.ddg.debug()) print(paste("param:", param))
            }
            
            else {
              # Attempt to allow names, not strings. This does not 
              # work as written because what we get when we call 
              # substitute is the parameter provided by lapply, not 
              # the one provided to ddg.procedure.  We will have 
              # the same problem dealing with outs.
              
              # arg <- substitute(param)
              # if (!is.character(arg) && .ddg.data.node.exists(arg)) {
              #	.ddg.data2proc(deparse(arg), pname)
              #	if (.ddg.debug()) print(paste("param:", deparse(arg)))
              #   else {warning}
              # }
              
              error.msg <- paste("No data node found for", param)	
              .ddg.insert.error.message(error.msg)
            }
          }
        })
  }
  
  # create output nodes
  
  .ddg.create.output.nodes(fname="ddg.procedure", pname, outs.graphic, outs.data, outs.exception, outs.url, outs.file, graphic.fext, parent.frame())
  
  invisible()
}

# ddg.return.value creates a data node for a function's return value. If 
# the function is called from a console command and console mode is
# enabled, a data flow edge will be created linking this node to 
# the console command that uses the value. ddg.return.value returns the 
# same value as the function (expr) and can be used in place of the 
# function's normal return statement(s) if it is the last statement
# in the function.  Otherwise, it should be a parameter to return, 
# like return(ddg.return.value(expr)).

# expr - the value returned by the function.

ddg.return.value <- function (expr=NULL) {
  if (!.ddg.is.init()) return(expr)
  pname <- NULL
  .ddg.lookup.function.name(pname)
  
  # Prints the call & arguments.
  if (.ddg.debug()) {
    # expr forces evaluation of the function early.  I think that causes some 
    # examples to work with debugging on but not off.  Checking.  (6/26/2015 - Barb)
    # Yes, ReturnTest.R fails on the recursive f5 function 
    print(paste("ddg.return:", sys.call(-1))) #, "returns", expr))
  }
  
  ddg.return.values <- .ddg.get(".ddg.return.values")
  ddg.num.returns <- .ddg.get(".ddg.num.returns")
  if (nrow(ddg.return.values) == ddg.num.returns) {
    size = 100
    new.rows <- data.frame(ddg.call = character(size),
        return.used = logical(size),
        return.node.id = integer(size),
        stringsAsFactors=FALSE)
    .ddg.add.rows(".ddg.return.values", new.rows)
    ddg.return.values <- .ddg.get(".ddg.return.values")
  }
  
  # If ddg.function was not called, create the function
  # nodes that it would have created.
  #print (paste0("ddg.return.value looking for function node for ", pname))
  
  if (!.ddg.proc.node.exists(pname)) {
    full.call <- match.call(sys.function(-1), call=call)
    #print (paste0("ddg.return.value creating function node for ", pname))
    .ddg.create.function.nodes(pname, full.call, auto.created = TRUE)
    #print (paste0("ddg.return.value done creating function node for ", pname))
  }
  
  # Create a data node for the return value. We want the scope of 
  # the function that called the function that called ddg.return.
  call <- sys.call(-1)
  call.text <- gsub(" ", "", deparse(call, nlines=1))
  return.node.name <- paste(call.text, "return")
  return.node.name <- gsub("\"", "\\\\\"", return.node.name)
  
  return.node.scope <- 
      environmentName (if (sys.nframe() == 2) .GlobalEnv
              else parent.env(sys.frame(-1)))
  .ddg.save.data(return.node.name, expr, fname="ddg.return", scope=return.node.scope)
  
  # Create an edge from the function to its return value.
  .ddg.proc2data(pname, return.node.name, return.node.scope, return.value=TRUE)
  
  # Update the table.
  ddg.num.returns <- ddg.num.returns + 1
  ddg.return.values$ddg.call[ddg.num.returns] <- call.text
  ddg.return.values$return.used[ddg.num.returns] <- FALSE
  ddg.return.values$return.node.id[ddg.num.returns] <- .ddg.dnum()
  .ddg.set(".ddg.return.values", ddg.return.values)
  .ddg.set(".ddg.num.returns", ddg.num.returns)
  
  #print ("Returning from ddg.return.value")
  return(expr)
}

# ddg.eval evaluates a statement and creates data flow edges from 
# variable and function return nodes that are used in the 
# statement. If the statement is an assignment statement, it also 
# creates a data node for the variable assigned and a corresponding 
# data flow edge.

# statement - the statement to evaluate.

ddg.eval <- function(statement) {
  parsed.statement <- parse(text=statement)
  frame.num <- .ddg.get.frame.number(sys.calls())
  env <- sys.frame(frame.num)
  if (!.ddg.is.init()) {
    eval(parsed.statement, env)
    return(invisible())
  }
  
  if (interactive() && .ddg.enable.console() && !.ddg.enable.source()) {
    .ddg.console.node()
  }
  
  .ddg.parse.commands(parsed.statement, environ=env, run.commands = TRUE, node.name=statement)
  .ddg.link.function.returns(statement)
  
  # Create outflowing edges .
  .ddg.statement <- list("abbrev" = .ddg.abbrev.cmd(gsub("\\\"", "\\\\\"", statement)), 
      "expr" = parsed.statement,
      "text" = statement)
  
  vars.set <- .ddg.find.var.assignments(.ddg.statement$expr)
  .ddg.create.data.set.edges.for.cmd(vars.set, .ddg.statement$abbrev, .ddg.statement$expr, 1, stack=sys.calls())
}

# ddg.data creates a data node for a single or complex data value. 
# If the value is omitted, the argument passed in for dname is  
# evaluated in the calling environment to determine the value. 
# If the value is determined to be complex, the output data is 
# written out to a csv if possible. Otherwise, the data are
# written out as a .txt file if the variable is determined to 
# be an object.

# dname - the label for the node. This can be passed as a string, name, 
#   or expression. 
# dvalue (optional) - the value of the node.
# graphic.fext (optional) - the file extention to be used for 
#   saving the variable if it is a graphical output. Otherwise 
#   ignored. Default is jpeg.


ddg.data <- function(dname, dvalue=NULL, graphic.fext = "jpeg") {
  if (!.ddg.is.init()) return(invisible())
  
  # Look up the value if one was not provided.
  env <- parent.frame()
  .ddg.lookup.value(dname, dvalue, env, "ddg.data")
  
  # Save the value appropriately.  If the name is not a string,
  # use the argument instead of the value.
  if (!is.character(dname)) dname <- deparse(substitute(dname))
  .ddg.save.data(dname, dvalue, "ddg.data", graphic.fext)
}

# ddg.exception creates a data node for an exception.

# dname - the label for the node. This can be passed as a string, 
#   name, or expression. 
# dvalue (optional) - the value of the node.  If the value is 
#   omitted, the argument passed in for dname is evaluated in 
#   the calling environment to determine the value.

ddg.exception <- function(dname, dvalue=NULL) {
  if (!.ddg.is.init()) return(invisible())

	# Look up the value if one was not provided.
	env <- parent.frame()
	.ddg.lookup.value(dname, dvalue, env, "ddg.exception")
	
	if (is.character(dname)) {
    if (exists(dname, env, inherits=TRUE)) {
      dscope = .ddg.get.scope(dname)
    }
    else {
      dscope = environmentName(.GlobalEnv)
    }
  }
  else {
    # If dname is not a string, use its name rather than its value.
    dname <- deparse(substitute(dname))
    dscope <- .ddg.get.scope(dname)
  }
  
  # Create input exception node.
  .ddg.data.node("Exception", dname, dvalue, dscope)
}

# ddg.url creates a data node for a URL.

# dname - the label for the node. 
# dvalue (optional) - the value of the node.  If the value is 
#   omitted, the argument passed in for dname is evaluated in 
#   the calling environment to determine the value.

ddg.url <- function(dname, dvalue=NULL) {
  if (!.ddg.is.init()) return(invisible())

	# Look up the value if one was not provided.
	env <- parent.frame()
	.ddg.lookup.value(dname, dvalue, env, "ddg.url")
	
	if (is.character(dname)) {
		dscope = environmentName(.GlobalEnv)
	}
	else {
		# If dname is not a string, use its name rather than its value.
		dname <- deparse(substitute(dname))
		dscope <- .ddg.get.scope(dname)
	}
	
	# Create input URL node.
	.ddg.data.node("URL", dname, dvalue, dscope)
}

# ddg.file creates a data node of type File by copying an existing 
# file to the DDG directory.

# filename - the name of the file to copy, including path to the file 
#   if it is not in the working directory. 
# dname (optional) - the label for the node. If omitted, the filename, 
#   minus the directory path, is used as the label.

ddg.file <- function(filename, dname=NULL) {
  if (!.ddg.is.init()) return(invisible())

	scope <- if (!is.null(dname)) .ddg.get.scope(dname)
			 else NULL
	invisible(.ddg.file.copy("File", filename, dname, scope))
}

# ddg.data.in creates a data flow edge from data node dname to 
# procedure node pname.

# dname - the name of the data node.  This can be passed as 
#   a string, name, or expression.
# pname (optional) - the name of the procedure that created 
#   this data value.  This can be passed as a string or as 
#   a name. It may be omitted if ddg.data.in is called by a function, 
#   in which case the name of the function will be used.

ddg.data.in <- function(dname, pname=NULL) {
  if (!.ddg.is.init()) return(invisible())
  
  .ddg.lookup.function.name(pname)
  
  arg <- substitute(dname)
  if (!is.character(arg)) {
    argname <- deparse(arg)
    dscope <- .ddg.get.scope(argname)
    if (.ddg.data.node.exists(argname, dscope)) { 
      dname <- argname
    }
    else { 
      dscope <- .ddg.get.scope(argname, for.caller=TRUE)
      if (.ddg.data.node.exists(argname, dscope)) { 
        dname <- argname
      }
      else { 
        # This case is for file names.  The table records the file
        # name, using the scope "undefined".
        dscope <- "undefined"
        if (!is.character(dname) || !.ddg.data.node.exists(dname, dscope)) {
          error.msg <- paste("No data node found for", arg)
          .ddg.insert.error.message(error.msg)
          return()
        }
      }
    }
  }
  else if (exists (arg, envir=parent.frame(), inherits=TRUE)) {
    dscope <- .ddg.get.scope(dname)
  }
  else if (exists (arg, envir=parent.frame(2), inherits=TRUE)) {
    dscope <- .ddg.get.scope(dname, for.caller=TRUE)
  }
  else {
    dscope <- environmentName(.GlobalEnv)
  }
  
  # Create data flow edge from data node to operation node.
  .ddg.data2proc(dname, dscope, pname)
}

# ddg.data.out creates a data or snapshot node of type Data. 
# It also creates a data flow edge from procedure node pname 
# to the output node. Used for simple or complex data values.

# dname - the label for the data node being created. This can
#   be passed as a string, name, or expression. Complex data
#   are written to the file dname.
# dvalue (optional) - the value to associate with the node. 
#   If no value is given, the argument passed in for dname is 
#   evaluated in the calling environment.
# pname (optional) - the name of the procedure that created the 
#   data. This can be passed as a string or name. It may be 
#   omitted if ddg.data.out is called by a function, in which 
#   case the name of the function will be used.
# graphic.fext (optional) - the file extension that should be
#   used when saving a graphics file. Ignored unless the value 
#   to be saved is determined to be a graphic.

ddg.data.out <- function(dname, dvalue=NULL, pname=NULL, graphic.fext="jpeg") {
  if (!.ddg.is.init()) return(invisible())

	# If no value is provided, get value in calling environment.
	env <- parent.frame()
	.ddg.lookup.value(dname, dvalue, env, "ddg.data.out")

	# Convert name to a string if necessary.
	if (!is.character(dname)) dname <- deparse(substitute(dname))

	# Save the complex data in appropriate format.
	.ddg.save.data(dname, dvalue, "ddg.data.out", graphic.fext)
	
	.ddg.lookup.function.name(pname)

	# Create data flow edge from operation node to data node.
	.ddg.proc2data(pname, dname)
}

# ddg.exception.out creates a data node of type Exception. It 
# also creates a data flow edge from the procedure node pname 
# to this node.

# dname - the label for the exception node being created.  
#   This can be passed as a string or name.
# dvalue (optional) - the value to associate  with the node. 
#   If no value is given, the argument passed in for dname is 
#   evaluated in the calling environment.
# pname (optional) - the name of the procedure that created this 
#   exception. This can be passed as a string or as name. It 
#   may be ommited if ddg.exception.out is called by a function, 
#   in which case the name of the function will be used.

ddg.exception.out <- function(dname, dvalue=NULL, pname=NULL) {
  if (!.ddg.is.init()) return(invisible())

	# If no value is provided, get value in calling environment.
	env <- parent.frame()
	.ddg.lookup.value(dname, dvalue, env, "ddg.exception.out")
	
	# Create output exception node.
	.ddg.data.node("Exception", dname, dvalue, "ddg.library")
	
	.ddg.lookup.function.name(pname)

	# Create data flow edge from procedure node to exception node.
	.ddg.proc2data(pname, dname)
}

# ddg.url.out creates a data node of type URL called dname with 
# address dvalue. It also creates a data flow edge from procedure 
# node pname to the URL node dname. Use for URL addresses.

# dname - the label for the data node being created.
# dvalue (optional) - the full URL. If a value is not provided, 
#   the argument passed in for dname is evaluated in the calling 
#   environment to determine the value.
# pname (optional) - the name of the procedure that created this 
#   URL node. This can be passed as a string or as a name. It may 
#   be omitted if ddg.url.out is called by a function, in which 
#   case the name of the function will be used.

ddg.url.out <- function(dname, dvalue=NULL, pname=NULL) {
  if (!.ddg.is.init()) return(invisible())

	# If no value is provided, get value in calling environment.
	env <- parent.frame()
	.ddg.lookup.value(dname, dvalue, env, "ddg.url.out")
  
  # URL labels are not necessarily variables, so make sure
  # it is a variable before trying to determine its scope.
  if (exists (dname, inherits = TRUE)) {
    dscope <- .ddg.get.scope(dname)
  }
  else {
    dscope <- environmentName(.GlobalEnv)
  }
  
  # Create output URL node where dvalue = address.
  .ddg.data.node("URL", dname, dvalue, dscope)
  
  .ddg.lookup.function.name(pname)
  
  # Create data flow edge from operation node to URL node.
  .ddg.proc2data(pname, dname, dscope)
}

# ddg.file.out creates a data node of type File called dname by 
# copying an existing file to the DDG directory. A data flow edge 
# is also created from procedure node pname to data node dname.
# Use for output files already created by the main script. Returns
# the full path to the file that is saved.

# filename - name of the file.  The name should include the path 
#   to the file if it is not in the working directory.
# dname (optional) - the label for the node being created. If 
#   omitted, the filename, minus the directory path, is used as 
#   the label.
# pname (optional) - the name of the procedure that created this 
#   node. This can be passed as a string or as a name. It may be 
#   omitted if ddg.file.out is called by a function, in which 
#   case the name of the function is used.

ddg.file.out <- function(filename, dname=NULL, pname=NULL) {
  if (!.ddg.is.init()) return(invisible())

	if (is.null(dname)) {
		dname <- basename(filename)
		scope <- NULL
	}
	else {
 		scope <- .ddg.get.scope (dname)
	}
	
	# Create output file node called filename and copy file.
	saved.file <- .ddg.file.copy("File", filename, dname, scope)
	
	.ddg.lookup.function.name(pname)
	
	# Create data flow edge from operation node to file node.
	.ddg.proc2data(pname, dname, scope)
	
	return (saved.file)
}

# ddg.graphic.out creates a data node of type Snapshot called 
# dname by capturing the current image in the active graphics 
# device and saving it in the DDG directory. The name of the 
# file is dname plus the extension specified by the fext 
# parameter. Available extensions are bmp, jpeg, png, and tiff. 
# A data flow edge is also created from procedure pname to the 
# data node dname.

# dname - the label for the node being created.
# pname (optional) - the name of the procedure that created this 
#   node. This can be passed as a string or as a name. It may be 
#   omitted if ddg.graphic.out is called by a function, in which 
#   case the name of the function is used.
# fext (optional) - the file extention to be used for the captured 
#   image file. If omitted, this value defaults to jpeg.

ddg.graphic.out <- function(dname, pname=NULL, graphic.fext="jpeg") {
  if(!.ddg.is.init()) return
	# Write out the graphic.
	.ddg.write.graphic(dname, 'Graphical Plot. Not saved in script.', graphic.fext)

	.ddg.lookup.function.name(pname)

	# Create the data flow edge from oepration node to the file node.
	.ddg.proc2data(pname,dname)
}

# ddg.start creates a procedure node of type Start called pname. 
# Users can right-click on a start node in DDG Explorer and see 
# the code between start and finish nodes in the original script.

# pname (optional) - the label for the node.  This can be passed as 
#   a string or as a name. It can be omitted if ddg.start is called 
#   by a function, in which case the name of the function will be
#   used.

ddg.start <- function(pname=NULL) {
	if (!.ddg.is.init()) return(invisible())

	.ddg.lookup.function.name(pname)

	# Check for NULL.
	if(is.null(pname)) {
		msg <- "Cannot call ddg.start with NULL value from top-level."
  	.ddg.insert.error.message(msg)
  }
  
  # Create start non-operational step.
  .ddg.proc.node("Start", pname, pname)
  
  # Create control flow edge from preceding procedure node.
  .ddg.proc2proc()
  
}

# ddg.finish creates a procedure node of type Finish called pname. 
# Users can right-click on a finish node in DDG Explorer and see 
# the code between start and finish nodes in the original script.

# pname (optional) - the label for the node. This can be passed as 
#   a string or as a name. It can be omitted if ddg.finish is called 
#   by a function, in which case the name of the function will be 
#   used.

ddg.finish <- function(pname=NULL) {
	if (!.ddg.is.init()) return(invisible())

	.ddg.lookup.function.name(pname)

	# Check for NULL.
	if(is.null(pname)) {
		msg <- "Cannot call ddg.finish with NULL value from top-level."
  	.ddg.insert.error.message(msg)
  }
  
  # Create finish non-operational step.
  .ddg.proc.node("Finish", pname, pname)
  
  # Create control flow edge from preceding procedure node.
  .ddg.proc2proc()
}


# ddg.init intializes a new DDG.

# r.script.path (optional) - the full path to the R script file 
# that is being executed. If provided, a copy of the script will 
# be saved with the DDG.
# ddgdir (optional) - the directory where the DDG should be saved. 
#   If not provided, the DDG will be saved in a subdirectory called 
#   "ddg" in the current working directory.
# enable.console (optional) - if TRUE, console mode is turned on.
# max.snapshot.size (optional) - the maximum size for objects that 
#   should be output to snapshot files. If 0, no snapshot files are saved. 
#   If -1, all snapshot files are saved. Size in kilobytes.  Note that
#   this tests the size of the object that will be turned into a 
#   snapshot, not the size of the resulting snapshot.

ddg.init <- function(r.script.path = NULL, ddgdir = NULL, enable.console = TRUE, max.snapshot.size = -1) {
  .ddg.init.tables()
  
  .ddg.set("ddg.r.script.path", 
      if (is.null(r.script.path)) NULL
          else normalizePath(r.script.path, winslash="/"))
  .ddg.set("ddg.path", 
      if (is.null(ddgdir)) paste(getwd(), "ddg", sep="/") 
          else normalizePath(ddgdir, winslash="/", mustWork=FALSE))
  
  # Set environment constants.
  .ddg.set(".ddg.enable.console", enable.console)
  .ddg.set(".ddg.max.snapshot.size", max.snapshot.size)
  .ddg.init.environ()
  
  # Mark graph as initilized.
  .ddg.set(".ddg.initialized", TRUE)
  
  # Store the starting graphics device.
  .ddg.set("prev.device", dev.cur())
  
  if (interactive() && .ddg.enable.console()) {
    ddg.history.file <- paste(.ddg.path(), ".ddghistory", sep="/")
    .ddg.set(".ddg.history.file", ddg.history.file)
    
    # Empty file if it already exists, do the same with tmp file.
    file.create(ddg.history.file)
    
    # One timestamp keeps track of last ddg.save (the default).
    .ddg.write.timestamp.to.history()
    
    # Save the history.
    savehistory(ddg.history.file)
  }
  
  invisible()
}

# ddg.run executes a script (r.script.path) or a function (f). 
# If an R error is generated, the error message is captured and 
# saved in the DDG. This function includes calls to ddg.init and 
# ddg.save, so it is not necessary to call those functions from 
# an instrumented script if ddg.run is used. Note that one of f 
# and r.script.path must be given; otherwise an error is generated.

# r.script.path (optional) - the full path to the R script. 
#   If provided, a copy of the script will be saved with the DDG. 
#   If only r.script.path is provided, the script is sourced using 
#   ddg.source and a DDG is created for the script.
# ddgdir (optional) - the directory where the DDG will be saved.
#   If not provided, the DDG will be saved in a directory called
#   "ddg" in the current working directory.
# f (optional) - a function to run. If supplied, the function f 
#   is executed with calls to ddg.init and ddg.save so that 
#   provenance for the function is captured.
# enable.console (optional) - if TRUE, console mode is turned on.
# max.snapshot.size (optional) - the maximum size for objects that 
#   should be output to snapshot files. If 0, no snapshot files are 
#   saved. If -1, all snapshot files are saved.  Size in kilobytes.  Note that
#   this tests the size of the object that will be turned into a 
#   snapshot, not the size of the resulting snapshot.

ddg.run <- function(r.script.path = NULL, ddgdir = NULL, f = NULL, enable.console = TRUE, max.snapshot.size = -1) {
  ddg.init(r.script.path, ddgdir, enable.console, max.snapshot.size)
  
  # If an R error is generated, get the error message and close 
  # the DDG.
  tryCatch(
      if (!is.null(f)) f() 
          else if (!is.null(r.script.path)) ddg.source(r.script.path, 
                ignore.ddg.calls = FALSE,
                ignore.init = TRUE,
                force.console = FALSE)
          else stop("r.script.path and f cannot both be NULL"),
      error=function(e) {
        e.str <- toString(e)
        print(e.str)
        ddg.procedure(pname="tryCatch")
        ddg.exception.out("error.msg", e.str, "tryCatch")
      },
      finally={ddg.save()}
  )
  invisible()
}

# ddg.save inserts attribute information and the number of 
# procedure steps at the top of the DDG. It writes the DDG and 
# the procedure nodes, data nodes, and function return tables
# to the DDG directory.

# quit (optional) - If TRUE, remove all DDG files from memory.

ddg.save <- function(quit=FALSE) {
  
  if (!.ddg.is.init()) return(invisible())
  
  if (interactive() && .ddg.enable.console()) {
    # Get the final commands
    .ddg.console.node()
  }
  
  # Get environment parameters.
  ddg.env <- .ddg.environ()
  
  # Insert the environment parameters and the number of procedures 
  # at the top of the DDG. Use a variable other than ddg.text so 
  # that ddg.save can be called more than once on the same DDG 
  # without generating more than one copy of the attributes.
  output <- paste(ddg.env, .ddg.pnum(), "\n", .ddg.get("ddg.text"), sep="")
  
  # Delete temporary files.
  .ddg.delete.temp()
  
  # Save DDG to file.
  ddg.path <- .ddg.path()
  fileout <- paste(ddg.path, "/ddg.txt", sep="")
  if (interactive()) print(paste("Saving DDG in ", fileout))
  write(output, fileout)
  
  # Save procedure nodes table to file.
  fileout <- paste(ddg.path, "/pnodes.txt", sep="")
  ddg.proc.nodes <- .ddg.proc.nodes()
  write.table(ddg.proc.nodes[ddg.proc.nodes$ddg.num > 0, ], fileout, quote=FALSE, na="", row.names=FALSE, col.names=FALSE)
  
  # Save data nodes table to file.
  fileout <- paste(ddg.path, "/dnodes.txt", sep="")
  ddg.data.nodes <- .ddg.data.nodes()
  write.table(ddg.data.nodes[ddg.data.nodes$ddg.num > 0, ], fileout, quote=FALSE, na="", row.names=FALSE, col.names=FALSE)
  
  # Save the function return table to file.
  fileout <- paste(ddg.path, "/returns.txt", sep="")
  ddg.returns <- .ddg.get(".ddg.return.values")
  write.table(ddg.returns[ddg.returns$return.node.id > 0, ], fileout, quote=FALSE, na="", row.names=FALSE, col.names=TRUE)
  
  # By convention, this is the final call to ddg.save.
  if (quit) {
    # Restore history settings.
    if (.ddg.is.set('ddg.original.hist.size')) Sys.setenv("R_HISTSIZE"=.ddg.get('ddg.original.hist.size'))

    # Delete temporary files.
    .ddg.delete.temp()
    
    # Capture current graphics device.
    .ddg.auto.graphic.node(dev.to.capture=dev.cur)
    
    # Shut down the DDG.
    .ddg.clear()
  }
  
  invisible()
}

# ddg.source reads in an R script and executes it in the provided 
# enviroment. ddg.source essentially mimics the behaviour of the 
# R source command, having similar input parameters and results, 
# but with additional parameters ignore.ddg.calls and ignore.init.

# file - the name of the R script file to source.
# local (optional) - the environment in which to evaluate parsed 
#   expressions. If TRUE, the environment from which ddg.source is 
#   called. If FALSE, the user's workspace (global environment).
# echo (optional) - print each expression after parsing.
# print.eval (optional) - print result of each evaluation.
# verbose (optional) - print extra diagnostics.
# max.deparse.length (optional) - maximum number of characters 
#   output for deparse of a single expression.
# chdir (optional) - change R working directory temporarily to
#   the directory containing the file to be sourced.
# encoding (optional) - encoding to be assumed when file is a
#   character string.
# ignore.ddg.calls (optional) - if TRUE, ignore DDG function calls.
# ignore.init (optional) - if TRUE, ignore ddg.int and ddg.run.
# force.console (optional) - if TRUE, turn console mode on.


ddg.source <- function (file, local = FALSE, echo = verbose, print.eval = echo, 
    verbose = getOption("verbose"), max.deparse.length = 150, chdir = FALSE, encoding = getOption("encoding"),
    ignore.ddg.calls = TRUE, ignore.init = ignore.ddg.calls, force.console=ignore.init){

  ### CODE IN THIS SECTION IS BASICALLY REPLICATION OF source FUNCTION ###
  
  # Get the environment under which the script should be executed.
  envir <- if (isTRUE(local)) {
        parent.frame()
      }
      else if (identical(local, FALSE)) {
        .GlobalEnv
      }
      else if (is.environment(local)) {
        local
      }
      else stop("'local' must be TRUE, FALSE or an environment")
  
  # Parse encoding information.
  have_encoding <- !missing(encoding) && encoding != "unknown"
  if (!missing(echo)) {
    if (!is.logical(echo)) 
      stop("'echo' must be logical")
    if (!echo && verbose) {
      warning("'verbose' is TRUE, 'echo' not; ... coercing 'echo <- TRUE'\n")
      echo <- TRUE
    }
  }

  # Print extra information about environment.
  if (verbose) {
    cat("'envir' chosen:")
    print(envir)
  }

  # Parse input file and figure out encoding.
  ofile <- file
  from_file <- FALSE
  srcfile <- NULL
  if (is.character(file)) {
    if (identical(encoding, "unknown")) {
      enc <- utils::localeToCharset()
      encoding <- enc[length(enc)]
    }
    else enc <- encoding
    if (length(enc) > 1L) {
      encoding <- NA
      owarn <- options("warn")
      options(warn = 2)
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
      options(owarn)
    }
    if (is.na(encoding)) 
      stop("unable to find a plausible encoding")
    if (verbose) 
      cat(gettextf("encoding = \"%s\" chosen", encoding), 
          "\n", sep = "")
    if (file == "") {
      filename <- "stdin"
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
      srcfile <- srcfilecopy(filename, lines, file.info(filename)[1, 
              "mtime"], isFile = TRUE)
    }
    loc <- utils::localeToCharset()[1L]
    encoding <- if (have_encoding) 
          switch(loc, `UTF-8` = "UTF-8", `ISO8859-1` = "latin1", 
              "unknown")
        else "unknown"
  }
  
  else {
    filename <- "Connection"
    lines <- readLines(file, warn = FALSE)
    srcfile <- srcfilecopy(deparse(substitute(file)), lines)
  }

  # Parse the expressions from the file.
  exprs <- if (!from_file) {
        if (length(lines)) 
          parse(stdin(), n = -1, lines, "?", srcfile, 
              encoding)
        else expression()
      }
      else parse(file, n = -1, NULL, "?", srcfile, encoding)
  
  on.exit()
  
  # Set the working directory for the current script and 
  # expressions.
  if (from_file) 
    close(file)
  
  if (verbose) 
    cat("--> parsed", "expressions; now eval(.)ing them:\n")
  if (chdir) {
    if (is.character(ofile)) {
      isURL <- length(grep("^(ftp|http|file)://", ofile)) > 
          0L
      if (isURL) 
        warning("'chdir = TRUE' makes no sense for a URL")
      if (!isURL && (path <- dirname(ofile)) != ".") {
        owd <- getwd()
        if (is.null(owd)) {
          stop("cannot 'chdir' as current directory is unknown")
          on.exit(setwd(owd), add = TRUE)
          setwd(path)
        }
      }
    }
    else {
      warning("'chdir = TRUE' makes no sense for a connection")
    }
  }
  
  ### END OF MODIFIED source CODE SECTION ###
  
  # Calculate the regular expressions for what should be ignored 
  # and what shouldn't.
  if (ignore.ddg.calls && !ignore.init) {
    if(verbose) warning("'ignore.ddg.calls' is TRUE, 'ignore.int' not; ... coercion 'ignore.init <- TRUE'\n")
    ignore.init <- TRUE
  }
  
  # Ignore calculation of certain execution steps.
  ignores <- c("^library[(]RDataTracker[)]$", 	
      if(ignore.ddg.calls) "^ddg."
          else if (ignore.init) c("^ddg.init", "^ddg.run")
          else "a^")
  
  # Now we can parse the commands as we normally would for a DDG.
  if(length(exprs) > 0) {
    
    
    # Turn on the console if forced to, keep track of previous 
    # setting, parse previous commands if necessary.
    prev.on <- .ddg.is.init() && .ddg.enable.console()
    if (prev.on && interactive()) .ddg.console.node()
    if (force.console) ddg.console.on()
    
    # Let library know that we are sourcing a file.
    prev.source <- .ddg.is.init() && .ddg.enable.source()
    
    # Initialize the tables for ddg.capture.
    .ddg.set("from.source", TRUE)
    
    # Parse the commands into a console node.
    .ddg.parse.commands(exprs, environ=envir, ignore.patterns=ignores, node.name=filename,
        echo = echo, print.eval = print.eval, max.deparse.length = max.deparse.length,
        run.commands = TRUE)
    
    # Save the DDG among other things, but don't return any 
    # values, TODO - should we do this?
    ddg.save()
    .ddg.set("from.source", prev.source)
    
    # Turn return console to previous state.
    if (!prev.on) ddg.console.off() else ddg.console.on()
  }
  
  invisible()
}

# ddg.debug.on turns on debugging of DDG construction.

ddg.debug.on <- function () {
  .ddg.set("ddg.debug", TRUE)
}

# ddg.debug.off turns off debugging of DDG construction.

ddg.debug.off <- function () {
  .ddg.set("ddg.debug", FALSE)
}

# ddg.console.off turns off the console mode of DDG construction.

ddg.console.off <- function() {
	if (!.ddg.is.init()) return(invisible())

  # Capture history if console was on up to this point.
	if (interactive() && .ddg.enable.console()) {
		.ddg.console.node()
	}

	# Set the console to off.
	.ddg.set(".ddg.enable.console", FALSE)
}

# ddg.console.on turns on the console mode of DDG construction.

ddg.console.on <- function() {
	if (!.ddg.is.init()) return(invisible())

	# Write a new timestamp if we're turning on the console so 
  # we only capture history from this point forward.
	if (!.ddg.enable.console()) .ddg.write.timestamp.to.history()
	.ddg.set(".ddg.enable.console", TRUE)
}
 
# ddg.flush.ddg removes all files from the DDG directory unless the 
#   DDG directory is the working directory. If no DDG directory is 
#   specified, the current DDG directory is assumed.

# ddg.path (optional) - path to DDG directory.

ddg.flush.ddg <- function (ddg.path=NULL) {
	if (is.null(ddg.path)) {
		if (.ddg.is.init()) ddg.path <- .ddg.path()
		else return(invisible())
	}

	ddg.path <- .ddg.path()
	# Do not remove files unless ddg.path exists and is different 
  # from the working directory.
  if (file.exists(ddg.path) && ddg.path != getwd()) {
    unlink(paste(ddg.path, "ddg.txt", sep="/"))
    unlink(paste(ddg.path, "dnodes.txt", sep="/"))
    unlink(paste(ddg.path, "pnodes.txt", sep="/"))
    unlink(paste(ddg.path,"[1-9]-*.*", sep="/"))
    unlink(paste(ddg.path,"[1-9][0-9]-*.*", sep="/"))
    unlink(paste(ddg.path,"[1-9][0-9][0-9]-*.*", sep="/"))
    unlink(paste(ddg.path,"[1-9][0-9][0-9][0-9]-*.*", sep="/"))
    unlink(paste(ddg.path,"[1-9][0-9][0-9][0-9][0-9]-*.*", sep="/"))
    unlink(paste(ddg.path,"[1-9][0-9][0-9][0-9][0-9][0-9]-*.*", sep="/"))
  }
  
  invisible()
}

# ddg.checkpoint prompts the user to source DDGCheckpoint.R.

# checkpoint.name (optional) - the value associated with the checkpoint 
#   procedure node.

ddg.checkpoint <- function(checkpoint.name=NULL) {
  stop("Call source(DDGCheckpoint.R to load ddg.checkpoint and ddg.restore")
}

# ddg.restore prompts the user to source DDGCheckpoint.R.

# file.path - the name of the checkpoint file to restore.

ddg.restore <- function(file.path) {
  stop("Call source(DDGCheckpoint.R to load ddg.checkpoint and ddg.restore")
}
