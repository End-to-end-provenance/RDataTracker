#################### DDG LIBRARY FOR R ####################

# The functions in this library may be used to annotate an R script 
# in order to collect provenance in the form of a data derivation 
# graph (DDG) as the script executes. The DDG is saved as a text file 
# (ddg.txt) that may be viewed and queried using DDG Explorer.

# Copyright (C) 2014 Emery R. Boose & Barbara S. Lerner
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
#   You should have received a copy of the GNU General Public License 
#   along with this program.  If not, see 
#   <http://www.gnu.org/licenses/>.

# Create DDG environment variable.

.ddg.env <- new.env(parent=emptyenv())

# Set maximum number of checkpoints in DDG.

ddg.MAX_CHECKPOINTS <- 10

# Set the lines the history file keeps (and therefore can be analyized)
ddg.MAX_HIST_LINES <- 2^14

#-------- FUNCTIONS TO MANAGE THE GLOBAL VARIABLES--------#

# Global variables cannot be used directly in a library.  Instead, we 
# need to place the variables in our own environment.  These 
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

# .ddg.clear removes all objects from the .ddg.env environment 
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

# .ddg.init.tables creates data frames to store procedure nodes, data 
# nodes, and checkpoints. It also initializes selected constants and 
# variables.

.ddg.init.tables <- function() {
# Create tables for procedure and data nodes.  Initially, the tables 
# have 100 rows each.
	size <- 100
	
    .ddg.set("ddg.proc.nodes", data.frame(ddg.type = character(size),
          ddg.num = numeric(size),
          ddg.name = character(size),
    			ddg.value = character(size), stringsAsFactors=FALSE))
	
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

	# Used to control sourcing. If already defined, don't change it's value
	if (!.ddg.is.set("from.source")) .ddg.set("from.source", FALSE)

	# Set current number of checkpoints.
	.ddg.set("ddg.checkpoint.num", 0)

  # Create table for checkpoints.
	.ddg.set("ddg.checkpoints", data.frame(filename=character(ddg.MAX_CHECKPOINTS),
          checkpoint.name=character(ddg.MAX_CHECKPOINTS), stringsAsFactors=FALSE))
	
	# Record last command from the preceding console block.
	.ddg.set(".ddg.last.cmd", NULL)

	# Records values returned by calls to ddg.return.
	# ddg.call is the string representing the call, like "f(a)"
	# return.used remembers if this function return value has been linked to the caller
	# return.node.id is the id of the data node that holds the return value
	.ddg.set(".ddg.return.values", data.frame(ddg.call=character(size),  
					return.used = logical(size),
					return.node.id = integer(size),
					stringsAsFactors=FALSE))
	.ddg.set(".ddg.num.returns", 0)
	# Record the current command to be opened during console execution (used 
	# when executing a script using ddg.source)
	.ddg.set(".ddg.possible.last.cmd", NULL)

	# Used for keeping track of current execution command
	.ddg.set("var.num", 1)

	# keept track of history
	.ddg.set(".ddg.history.timestamp", NULL)
	
	# keep track of the last device seen (0 implies NULL)
	.ddg.set("prev.device", 0)

	# store path if current script
	.ddg.set("ddg.r.script.path", NULL)

	# store path of current ddg
	.ddg.set("ddg.path", NULL)

	# no ddg initialized
	.ddg.set(".ddg.initialized", FALSE)

	# no history file
	.ddg.set(".ddg.history.file", NULL)

	# console is disabled
	.ddg.set(".ddg.enable.console", FALSE)
}

# Wrrapper to easily change history lines during execution of script
.ddg.set.history <- function(lines = 16384){
	Sys.setenv("R_HISTSIZE" = lines)
}

# .ddg.init.environ() sets up the filesystem and R environments for use
.ddg.init.environ <- function() {
	dir.create(.ddg.path(), showWarnings = FALSE)
	if (interactive() && .ddg.enable.console()) {
		.ddg.set('ddg.original.hist.size', Sys.getenv('R_HISTSIZE'))
		.ddg.set.history()
	}
}

# ddg.environ gets environment parameters for the DDG.

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

### .ddg.is.init is a function definition to be used at the beginnign of all 
#		user accesible functions. It verifies that the DDG has been initialized.
#   If it hasn't, it returns False. 
###
.ddg.is.init <- function() {
		# || short circuits evaluation
		return(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized"))
}

# Assumes input format is yyyy-mm-dd hh:mm:ss
# Reformat to  (yyyy-mm-ddThh.mm.ss).
.ddg.format.time <- function(time) {
  formatted.time <- strftime(time, format="%Y-%m-%dT%H.%M.%S",usetz=TRUE)
  
  # The strftime call leaves a space between the time and the time zone.
  # We remove that here.
  return (sub(" ", "", formatted.time))
}

# .ddg.timestamp gets the current date and time from the system.

.ddg.timestamp <- function() {
	ts <- Sys.time()
	return (.ddg.format.time(ts))
}

# .ddg.write.timestamp.to.history writes the current timestamp to the 
# R history. The timestamp function does not work properly on Windows 
# from within RStudio (the arguments are ignored).  In this case we 
# create our own timestamp value and hope that the time does not 
# change between when we set .ddg.history.timestamp and when the 
# timestamp function inserts the timestamp in the history.

# @param var - the variable name under which the timestamp is saved
.ddg.write.timestamp.to.history <- function(var = ".ddg.history.timestamp") {
	if (Sys.getenv("RSTUDIO") != "" && Sys.info()['sysname'] == "Windows") {
		.ddg.set(var, paste("##------", date(), "------##"))
		timestamp(quiet=TRUE)
	}
	else {
		.ddg.set(var, timestamp(prefix = "##-ddg-- ", quiet=TRUE))
	}
}

# .ddg.is.viewable tries to decipher if the value snapshot should be written as
# as file directly from the data or if it is a graphic which can be captures 
# from the image device. This function, as written, is basically a hack. There
# must be a better way to implement it.
.ddg.is.graphic <- function(value){
	# matching any of these classes automatically classifies the object as a graphic
	graph.classes <- list("gg", "ggplot")
	return(is.object(value) && any(class(value) %in% graph.classes))
}

# .ddg.is.simple returns true if the value passed in is a simple data value which
# should be saved locally as opposed to stored in a seperate file. The assumption
# is that the value passed in has already been declared not to be a graphic.
.ddg.is.simple <- function(value) {
	# Note that is.vector returns TRUE for lists, so we need to check lists
	# separately.  Since every value in a list can have a different type,
	# if it is a list, we will assume the value is complex. We consider NULL values to be simple.
	return((!.ddg.is.graphic(value) && 
	       !is.list(value) && 
	       is.vector(value) && 
	       length(value) == 1) ||
				 is.null(value))
}

# .ddg.is.csv returns true if the value passed in should be written out as a csv
# file. No assumptions are made about input.
.ddg.is.csv <- function(value) {
  return(!(.ddg.is.graphic(value) || .ddg.is.simple(value)) && (
	       is.list(value) || is.vector(value) || is.matrix(value) || is.data.frame(value)))
}

# .ddg.is.object returns true if the value is determined to be an object, by our standards
.ddg.is.object <- function(value){
	return(is.object(value))
}

# .ddg.is.function returns true if the value is determined to be a function or we
# want to save it as a function
.ddg.is.function <- function(value){
	return(is.function(value))
}

# .ddg.dev.change determined whether or not a new graphic device has become active
# and whether or not we should capture the previous graphic device. It returns the
# device number we should capture (0 means we shouldn't capture any devide)
.ddg.dev.change <- function(){
	prev.device <- .ddg.get("prev.device")
	curr.device <- dev.cur()
	device.list <- dev.list()

	# we've switched devices 
	if (prev.device != curr.device) {
		# update device
		.ddg.set("prev.device", curr.device)

		# previous device still accessible
		if (prev.device %in% device.list) return(prev.device)
	}

	# no switching, or previous is not accessible (null or removed)
	return(0)

}

# .ddg.save.simple takes in a simple name, value pairing and saves it to the ddg.
# It does not however create any edges.
.ddg.save.simple <- function(name,value, scope=NULL) {
	# save the true value
	.ddg.data.node("Data", name, value, scope)
}

# .ddg.write.graphic takes as input the name of a variable as well as the value 
# (the data) associated with it and attempts to write it out as a graphics file
# If all else fails, it writes out the informaion as a text file and also writes
# out an RData Object which can later be read back into the system 
.ddg.write.graphic <- function(name, value = NULL, fext="jpeg", scope=NULL){
	# try to output graphic value
	tryCatch({
		.ddg.snapshot.node(name, fext, NULL, dscope=scope)
	}, error = function(e) {
		# warning(paste("Attempted to write", name, "as", fext, "snapshot. Trying jpeg", ".", e))
		tryCatch({
			.ddg.dec("ddg.dnum")
			.ddg.snapshot.node(name, "jpeg", NULL, dscope=scope)
		}, error = function(e) {
			 #warning(paste("Attempted to write", name, "as jpeg snapshot. Failed.", e, "Defaulting to saving RObject and .txt file."))
			.ddg.dec("ddg.dnum")
  		.ddg.snapshot.node(name, "txt", value, save.object = TRUE, dscope=scope)
  	})
	})
}

# .ddg.write.csv takes as input the name, value pairing for a variable and attempts
# to save the data as a csv. It does not create any edges but does add the node to
# the DDG. Edge creation should occur from wherever this function is called.
.ddg.write.csv <- function(name, value, scope=NULL) {
  tryCatch({
		.ddg.snapshot.node(name, "csv", value, dscope=scope)
	}, error = function(e) {
		#warning(paste("Attempted to write", name, "as .csv snapshot but failed. Out as RDataObject.", e))
		.ddg.dec("ddg.dnum")
		.ddg.snapshot.node(name, "txt", value, save.object = TRUE, dscope=scope)
	})
}

# .ddg.save.data takes as input name and value of a data node that needs to be created
# It determines how the data should be output (or saved) and saves it in that format.
# The name of the created node is name, its value is value. fname is the name of the
# calling function and is used to generate helpful error messages if something goes wrong.
# error indicates whether the function should raise an R Error as opposed to a ddg error.
.ddg.save.data <- function(name, value, fname=".ddg.save.data", graphic.fext = 'jpeg', error=FALSE, scope=NULL, stack=NULL){
  if (is.null(scope)) {
    scope <- .ddg.get.scope(name, calls=stack)
  }
	# Determine type for value, and save accordingly
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

.ddg.record.proc <- function(ptype, pname, pvalue) {
	# If the table is full, make it bigger.
	ddg.pnum <- .ddg.pnum()
	ddg.proc.nodes <- .ddg.proc.nodes()
	if (nrow(ddg.proc.nodes) < ddg.pnum) {
		size = 100
		new.rows <- data.frame(ddg.type = character(size),
        ddg.num = numeric(size),
        ddg.name = character(size),
	      ddg.value = character(size), stringsAsFactors=FALSE)
		.ddg.add.rows("ddg.proc.nodes", new.rows)
		ddg.proc.nodes <- .ddg.proc.nodes()
	}

	ddg.proc.nodes$ddg.type[ddg.pnum] <- ptype
	ddg.proc.nodes$ddg.num[ddg.pnum] <- ddg.pnum
	ddg.proc.nodes$ddg.name[ddg.pnum] <- pname
	ddg.proc.nodes$ddg.value[ddg.pnum] <- pvalue
	.ddg.set("ddg.proc.nodes", ddg.proc.nodes)

  if (.ddg.debug()) print (paste("Adding procedure node", ddg.pnum, "named", pname))
}

# .ddg.record.data records a data node in the data node table.

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

.ddg.copy.data.node <- function(formal, formal.scope, arg, arg.scope) {
    arg.number <- .ddg.data.number (arg, arg.scope) 
    ddg.data.nodes <- .ddg.data.nodes()
	# Increment data counter.
	.ddg.inc("ddg.dnum")
	
	# Add node to DDG.
	ddg.dnum <- .ddg.dnum()
	dtype <- ddg.data.nodes$ddg.type[arg.number]
	dvalue <- ddg.data.nodes$ddg.value[arg.number]
	.ddg.append(dtype, " d", ddg.dnum, " \"", ddg.dnum, "-", formal, "\" Value=\"", dvalue, "\";\n", sep="")
	.ddg.record.data (dtype, 
                      formal, 
                      dvalue, 
                      formal.scope, 
                      dloc=ddg.data.nodes$ddg.loc[arg.number]) 
}

# .ddg.is.proc.node returns true if type is one which is considered a procedure
# node by us. That means, it can have input/output edges in the expanded DDG.
# Current types are: Everything except Start
.ddg.is.proc.node <- function(type) {
	return(type == "Operation" |
	       type == "Checkpoint" | 
	       type == "Restore" | 
	       type == "Finish" | 
	       type == "Binding")
}

# .ddg.proc.number gets the number of the nearest preceding matching 
# Operation, Checkpoint, or Restore node. It returns zero if no match 
# is found.
.ddg.proc.number <- function(pname) {
	ddg.proc.nodes <- .ddg.proc.nodes()
	rows <- nrow(ddg.proc.nodes)
	for (i in rows:1) {
		type <- ddg.proc.nodes$ddg.type[i]
		if (.ddg.is.proc.node(type) & ddg.proc.nodes$ddg.name[i] == pname) {
			return(ddg.proc.nodes$ddg.num[i])
		}
	}

	# Error message if no match is found.
	msg <- paste(if (.ddg.enable.source() && pname == "eval") 
		". eval is called by ddg.source." else ".", "Check  
		ddg.* functions from the top level are called with the pname parameter.")
  error.msg <- paste("No procedure node found for", pname, msg)
  .ddg.insert.error.message(error.msg)  
	return(0)
}

# Function which returns the node number of the last procedure node in the 
# ddg procedure node table. Procedure nodes are determined as defined in 
# .ddg.is.proc.node above.
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
# data node. It returns TRUE if a match is found and FALSE otherwise.

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
# null string if no match is found.

.ddg.proc.name <- function(pnum) {
	if (pnum < 1 || pnum > .ddg.pnum()) {
		error.msg <- paste("No name found for procedure number", pnum)
        .ddg.insert.error.message(error.msg)
		return ("")
	}
	
	return(.ddg.proc.nodes()$ddg.name[pnum])
}

# .ddg.proc2proc creates a control flow edge from the preceding 
# procedure node.
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
.ddg.proc2data <- function(pname, dname, dscope=NULL) {
	# Get data & procedure numbers.
	dn <- .ddg.data.number(dname, dscope)
	pn <- .ddg.proc.number(pname)

	# Create data flow edge from procedure node to data node.
	if (dn != 0 && pn != 0) {
		.ddg.append("DF p", pn, " d", dn, "\n", sep="")
	
		if (.ddg.debug()) {
			print(paste("proc2data: ", pname, " ", dname, sep=""))
			print(paste("DF p", pn, " d", dn, sep=""))
		}
	}

	invisible()
}

# .ddg.lastproc2data creates a data flow edge from the last procedure 
# node to a data node. The all parameter indicated whether 
# all nodes should be considered (TRUE) or only procedure nodes (FALSE)

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

.ddg.get.var <- function(lvalue) {
	if (is.symbol(lvalue)) deparse(lvalue)
	else .ddg.get.var(lvalue[[2]])
}

# ddg.is.functiondecl tests to see if an expression is a function 
# declaration.
.ddg.is.functiondecl <- function(expr) {
	if (is.symbol(expr) || !is.language(expr)) return (FALSE)
	return (expr[[1]] == "function")
}

# .ddg.find.assign returns a vector containing the names of all the 
# variables assigned in an expression.  The parameter should be an 
# expression object. For example, if obj represents the expression 
# "a <- (b <- 2) * 3", the vector returned will contain both a and b.

.ddg.find.assign <- function(obj) {
	# Base case.
	if (!is.recursive(obj)) return(character())
  
	# Assignment statement.  Add the variable being assigned to the 
  # vector and recurse on the expression being assigned.
	if (.ddg.is.assign(obj)) {
		var <- .ddg.get.var(obj[[2]])
		
		# Don't look for assignments in the body of a function as those 
    # won't happen until the function is called.
    # Don't recurse on NULL
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

.ddg.find.simple.assign <- function(obj) {
	if (.ddg.is.assign(obj)) {
		.ddg.get.var(obj[[2]])
	}
	else {
		NULL
	}
}

# .ddg.find.var.uses returns a vector containing all the variables 
# used in an expression. If all is false, then it returns a vector of 
# unique values. Otherwise, it returns all uses.

.ddg.find.var.uses <- function(main.object, all=FALSE) {
	# Find function to filter results
	filter <- if (all) identity else unique

	# Recursive helper function
	.ddg.find.var.uses.rec <- function(obj) {
		# Base cases.
		if (is.name(obj)) return (deparse(obj))
		if (!is.recursive(obj)) return(character())
		##
		if (.ddg.is.functiondecl(obj)) return(character())
		
		tryCatch(
			if (.ddg.is.assign(obj)) {
				# If assigning to a simple variable, recurse on the right hand 
	      # side of the assignment.
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

# .ddg.create.empty.vars.set creates an empty data frame initialized to contain
# information about variable assignments. Parameter is the desired size of the
# data frame; Negative values and 0 are coerced to 1. Return value is the 
# intialized data frame. 

# The data frame is structured as follows 

# - the variable name.
# - the position of the statement that wrote the variable first.
# - the position of the statement that wrote the variable last.
# - the position of the first statement that may have assigned to a
#   variable .
# - the position of the last statement that may have assigned to a 
#   variable.

# The difference between first.writer and possible.first.writer is 
# that first.writer is for simple assignments (like a <- 1), while 
# possible.first.writer is for situations where the assignment might 
# not have occurred, like "if (foo) a <- 1".
.ddg.create.empty.vars.set <- function(var.table.size = 1) {
	if (var.table.size <= 0) var.table.size <- 1

	vars.set <- data.frame(variable=character(var.table.size), 
			first.writer=numeric(var.table.size), 
			last.writer=numeric(var.table.size), 
			possible.first.writer=numeric(var.table.size), 
			possible.last.writer=numeric(var.table.size), stringsAsFactors=FALSE)

	# Initialize first writer
	vars.set$first.writer <- var.table.size + 1
	vars.set$possible.first.writer <- var.table.size + 1

	return(vars.set)
}

#.ddg.increase.vars.set simply double the size of the passed in var set and returns
# the new one
.ddg.double.vars.set <- function(vars.set, size=nrow(vars.set)) {
	# create the right size data frame from input frame
	new.vars.set <- rbind(vars.set,.ddg.create.empty.vars.set(size))

	# update first/last writer
	new.vars.set$first.writer <- ifelse(new.vars.set$first.writer == size + 1, 
	                                    size*2 + 1, new.vars.set$first.writer)
	new.vars.set$possible.first.writer <- ifelse(new.vars.set$possible.first.writer == size + 1, 
	                                             size*2 + 1, new.vars.set$possible.first.writer)

	return(new.vars.set)
}

# .ddg.add.to.vars.set parses expr and adds the new variable information to the 
# data frame. The current vars.set, the command, and the location of the command
# all need to be input parameters. The return value is the new var set.
# Note that var.num is a global variable! It should be intialized when vars.set 
# is first created.
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
			# check space
			size <- nrow(vars.set)
			if (var.num > size) vars.set <- .ddg.double.vars.set(vars.set,size)

			# set the variable
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


# finds the possible variable assignments for a fixed set of parsed commands
# See .ddg.create.empty.vars.set for more information on the structure of the
# returned data frame.
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

# .ddg.create.data.edges.for.cmd creates a data flow edge from the
# node for each variable used in cmd.expr to the procedure node labeled cmd. 
# This is determined by finding nodes referened in cmd which are already assigned.
# ALl unassigned referenced are assumed to be creations (otherwise an error would
# have occured whene executing the commands), therefore these are set as output 
# edges and a data node is created. Additionally, to compensate for something like
# the following a <- a, where a is both input and output, a double occurrence of 
# a variable is automatically assumed to be both input and output.
# The value must exist beforehand.

# THIS IS NOT USED ANYWHERE BECAUSE IT DID NOT WORK AS EXPECTED
.ddg.create.data.edges.for.cmd <- function(cmd.abbrev,cmd.expr, environ=.GlobalEnv) {
	all.vars.used <- .ddg.find.var.uses(cmd.expr,all=TRUE)
	unique.vars.used <- unique(unlist(all.vars.used))

	for (var in unique.vars.used) {
		# count occurrences of variable  
		num.var <- sum(all.vars.used == var)

		# variable exists, so this is an unput
		if (.ddg.data.node.exists(var)) .ddg.data2proc(var,cmd.abbrev)

		# variable does not exist or occurs more than once
		if (!.ddg.data.node.exists(var) || num.var > 1) {
			# find value of this variable
			val <- tryCatch(eval(parse(text=var), environ),
					error = function(e) {NULL}
			)
			tryCatch(.ddg.save.data(var,val,fname=".ddg.create.data.edges.for.cmd",error=TRUE),
			         error = function(e){.ddg.data.node("Data", var, "complex")})


			.ddg.proc2data(cmd.abbrev,var)
		}
	}
}

# .ddg.auto.graphic.node attempts to figure out if a new graphics device has been 
# created and take a snapshot of previously active device, setthing the snapshot node 
# as the output of the specified command. Optionally, it can take as input 
# a function specifying which device should be captured, where 0 values specify
# no device, and negative values are ignored.
.ddg.auto.graphic.node <- function(cmd.abbrev = NULL, dev.to.capture=.ddg.dev.change) {
	
	num.dev.to.capture <- dev.to.capture()
	if (num.dev.to.capture > 1) {
		# make the capture device active (store info on previous device)
		prev.device <- dev.cur()
		dev.set(num.dev.to.capture)

		# capture it as a jpeg
		name <- if (!is.null(cmd.abbrev) && cmd.abbrev != "") paste0("graphic", cmd.abbrev) else "graphic"
		.ddg.snapshot.node(name, fext="jpeg", data=NULL)

		# make the previous device active again
		dev.set(prev.device)

		# we're done, so create the edge
		if(is.null(cmd.abbrev)) .ddg.lastproc2data(name, all=FALSE)
		else .ddg.proc2data(name,cmd.abbrev)
	}
}

# .ddg.create.data.use.edges.for.console.cmd creates a data flow edge 
# from the node for each variable used in cmd.expr to the procedural 
# node labeled cmd, as long as the value would either be one that 
# exists prior to starting the console block, or corresponds to the 
# last setting of this variable in the console block.

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

				# TODO - add some sort of warning to the user that the node is not being created
			}
			
			# The variable is not set at all in this console block.  Connect 
      # to a pre-existing data node.
			else {
				.ddg.data2proc(var, environmentName(.GlobalEnv), cmd)
			}
		}
		else {
			# TODO - add some sort of warning that the data node was NOT found
			#error.msg <- paste("Unable to find data node for",var, ". Command", parse(text=cmd.expr), "appears
			#                  to use it for procedure node", cmd, ".")

    	#.ddg.insert.error.message(error.msg)
		}
	}
}
# .ddg.create.data.set.edges.for.console.cmd creates the nodes and 
# edges that correspond to a console command assigning to a variable. 
# A data node is created for the last write of a variable if that 
# occurs after the last possible writer. A snapshot node is created 
# if the value is a data frame.  Otherwise, a data node is created.

.ddg.create.data.set.edges.for.console.cmd <- function(vars.set, cmd.abbrev, cmd.expr, cmd.pos, for.finish.node = FALSE) {
	.ddg.create.data.set.edges.for.cmd(vars.set, cmd.abbrev, cmd.expr, cmd.pos, for.finish.node, scope=environmentName(.GlobalEnv), env=.GlobalEnv)
}
	


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
			tryCatch(.ddg.save.data(var,val,fname=".ddg.create.data.set.edges.for.console.cmd",error=TRUE, scope=scope, stack=stack),
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

.ddg.create.data.node.for.possible.writes <- function (vars.set, last.command) {

	for (i in 1:nrow(vars.set)) {
		if (vars.set$possible.last.writer[i] > vars.set$last.writer[i]) {
			value <- tryCatch(eval(parse(text=vars.set$variable[i]), .GlobalEnv),
					error = function(e) {NULL}
			)
			
			# Only create the node and edge if we were successful in 
      # looking up the value.
			if (!is.null(value)) {
				.ddg.data.node("Data", vars.set$variable[i], value, environmentName(.GlobalEnv))
				.ddg.proc2data(last.command, vars.set$variable[i], environmentName(.GlobalEnv))
			}
		}
	}
}

# .ddg.abbrev.cmd abbreviates a command to the specified length.
.ddg.abbrev.cmd <- function(cmd, len=60) {
	if (nchar(cmd) <= len) cmd
	else if (substr(cmd, len, len) != "\\") substr(cmd, 1, len)
	else if (substr(cmd, len-1, len) == "\\\\") substr(cmd, 1, len)
	else substr(cmd, 1, len-1)
}

# .ddg.loadhistory takes in the name of a file, open it, scans it for the last
# occurrence of the string specified by timestamp, and returns the lines from 
# that point forward. 
.ddg.loadhistory <- function(hist.file, timestamp) {
	# read from specified file
	history <- readLines(hist.file)
	history.lines <- length(history)
	
	# Find the timestamp specified in the history.  There may be 
  # more than one with the same timestamp, so pick the last of these.
	history.timestamp.line <- tail(which(history == timestamp), 1)
	
	if (length(history.timestamp.line) == 0) {
		error.msg <- paste("Part of history is missing. DDG may be incomplete! Tried reading from",
		                   hist.file, "but could not find timestamp:", timestamp)

    .ddg.insert.error.message(error.msg)
		history.timestamp.line <- 0
	}

	# Need to check if the timestamp line is the last line in the file
	# explicitly.  If we don't do that and take the vector, we will 
	# get the last line in the file since R will create a 
	# descending sequence for the vector.
	if (history.timestamp.line == history.lines) return (vector())

	# NEED the paren around sum
	return(history[(history.timestamp.line+1):history.lines])
}

# .ddg.savehistory saves the current and unsaved R Command history to a file 
# specified as the parementer only if that file matches up with the DDGs history
# file.

# THE FOLLOWING APPLIES TO THE COMMENTED SECTION OF CODE
# It appends the information to this file. 

.ddg.savehistory <- function(hist.file) {

	# USED TO STORE ENTIRE HISTORY IN SEP. FILE
	# write history out to temporary file
	#ddg.grab.timestamp <- .ddg.get(".ddg.grab.timestamp.history")
	#ddg.tmp.history.file <- paste(hist.file,".tmp", sep="")

	if (.ddg.is.set(".ddg.history.file") && 
	    is.character(.ddg.get(".ddg.history.file")) &&
	    .ddg.get(".ddg.history.file") == hist.file) {
		savehistory(hist.file)
	}

	# USED TO STORE ENTIRE HISTORY IN SEP. FILE
	# read in changes and writ eout to extended file
	#newlines <- .ddg.loadhistory(ddg.tmp.history.file,ddg.grab.timestamp)
	#write(newlines, file=hist.file, append=TRUE)
	# insert timestamp to history 
	#.ddg.write.timestamp.to.history(var=".ddg.grab.timestamp.history")
}


# .ddg.link.function.returns determines if the command calls a function
# for which ddg.return has created a node for the return value.  If so,
# a data flow edge is created from the return value data node to the 
# finish node for the command.  Note that if the assignment is an 
# expression, like "d <- f(a) + f(b)", there may be multiple return
# value nodes to link to.
.ddg.link.function.returns <- function(command) {
	# Find the functions that have completed but whose returns have not
	# been used yet.
	returns <- .ddg.get(".ddg.return.values")
	unused.returns <- returns[!returns$return.used & returns$return.node.id > 0, ]
    if (nrow(unused.returns) == 0) return()
	
	# See which of these are called from the command we are processing now
	unused.calls <- unused.returns$ddg.call
	uses <- sapply(unused.calls, function(call) {grepl(call, command, fixed=TRUE)})
	
	# The following line is here to get around R CMD check, which otherwise
	# reports:  no visible binding for global variable
	# Note that return.node.id is not a variable in the subset call, but the
	# name of a column in the data frame being subsetted.
	return.node.id <- NULL
	
	# Extracts for the return value nodes.
	new.uses <- subset(unused.returns, uses, return.node.id)
	
	# Create an edge from each of these to the last procedure node
	lapply (new.uses$return.node.id, 
			function(data.num) {
				proc.num <- .ddg.pnum()
				
				# Create data flow edge from procedure node to data node.
				.ddg.append("DF d", data.num, " p", proc.num, "\n", sep="")
				
				if (.ddg.debug()) {
					print(paste(".ddg.link.function.returns:", command))
					print(paste("DF d", data.num, " p", proc.num, sep=""))
				}
				
				# Set the return value as being used
				returns$return.used[returns$return.node.id == data.num] <- TRUE
				.ddg.set(".ddg.return.values", returns)
			})

}

# This function is exclusively used in .ddg.parse.commands (so far) and simply 
# serves to avoic repetition of code.
.ddg.add.abstract.node <- function(type, cmd, called=".ddg.parse.commands") {
	cmd.abbrev <- .ddg.abbrev.cmd(cmd)
	if (.ddg.debug()) print(paste(called, ":  Adding", cmd.abbrev,  type, "node"))
	  .ddg.proc.node(type, cmd.abbrev, cmd.abbrev, TRUE)
		.ddg.proc2proc()

	return(cmd.abbrev)
}

# .ddg.close.last.command.node closes the last created collapsible node stored
# in .ddg.last.cmd DDG property. The optional parameter called is used when 
# debugging for printing the function which called .ddg.close.previous.command.
.ddg.close.last.command.node <- function(called=".ddg.parse.commands", initial=FALSE){
	# get both the last command and new commands
	.ddg.last.cmd <- .ddg.get(".ddg.last.cmd")
	.ddg.possible.last.cmd <- .ddg.get(".ddg.possible.last.cmd")

	# only create a finish node if a new command exists (ie, we've parsed some lines of code)
	if (!is.null(.ddg.last.cmd) && (!is.null(.ddg.possible.last.cmd) || initial)) {
		cmd.abbrev <- .ddg.add.abstract.node("Finish", .ddg.last.cmd$abbrev,called=paste(called, "-> .ddg.close.last.command.node"))

		# Add link from a function return node if there is one.
		.ddg.link.function.returns(.ddg.last.cmd$text)
		
		# Create outflowing edges 
		vars.set <- .ddg.find.var.assignments(.ddg.last.cmd)
		.ddg.create.data.set.edges.for.console.cmd(vars.set, .ddg.last.cmd$abbrev, .ddg.last.cmd$expr, 1, for.finish.node = TRUE)

		# No previous command
		.ddg.set(".ddg.last.cmd", NULL)
	}
}

# .ddg.open.new.command.node opens a new collapsible command node depending on 
# the informationo stored in .ddg.last.cmd.
# Parameters - (optional) called is the calling function
# new.command - the name of the new command which should be opened
.ddg.open.new.command.node <- function(called=".ddg.parse.commands") {
  new.command <- .ddg.get(".ddg.possible.last.cmd")
	if (!is.null(new.command)) {
		.ddg.add.abstract.node("Start", new.command$abbrev, called=paste(called, "-> .ddg.open.new.command.node"))

		# Now the new command becomes the last command, and new command is null
		.ddg.set(".ddg.last.cmd", new.command)
		.ddg.set(".ddg.possible.last.cmd", NULL)
	}
}

# Returns true if the command passed in (as a string) will create a procedure node
# and therefore initiate the creation of a collapsible console node
.ddg.is.procedure.cmd <- function(cmd.str) {
	return(grepl("^ddg.(procedure|start|finish|restore|checkpoint)", cmd.str))
}

# .ddg.parse.lines takes as input a set of lines corresponding to either the 
# history of an RScript or an RScript itself. It parses and 
# converts them to executable commands. It returns a list of commands. Each command
# might span multiple lines. The function returns a named list. The contents of the list
# are: 	text - each entry is the full text string of a single commands
# 			commands - each entry is the parsed command 
.ddg.parse.lines <- function(lines) {
	# no new lines passed in, so return NULL
	if (length(lines) == 0) return(NULL)

	# Parse the new lines.
	parsed.commands <- parse(text=lines)

	return(parsed.commands)
}

# .ddg.parse.commands takes as input a set of RScript commands command format. 
# It creates DDG nodes for each command. If environ is an environment, it executes 
# the commands in that environment immediately before creating the respective nodes 
# for that command and then creates the data node based on the information available in the 
# environment. This is the parameter used by ddg.environ to execute environ files
# which need to be automatically annotated. Additionally, if environ is not null, calls to 
# ddg.* are not exectuted so only the clean script is processed. The paramenter node.name
# specifies the name for the collapsible node under which this DDG should be stored.
# ignore.patterns is a vector of regular expression patterns. Any commands which match
# any of these regular expressions will not be parsed (ie, no nodes will be created for them.)
.ddg.parse.commands <- function(parsed.commands,environ=NULL, ignore.patterns=c('^ddg.'),
                                        node.name="Console", echo=FALSE, print.eval = echo,
                                        max.deparse.length = 150) {
	# 
	
	# figure out if we will execute commands or not
	execute <- !is.null(environ) & is.environment(environ)

	# It is possidle that a command may extend over multiple lines. 
  # new.commands will have one string entry for each parsed command.

	new.commands <- lapply(parsed.commands, function(cmd) {paste(deparse(cmd), collapse="")})
	filtered.commands <- Filter(function(x){return(!grepl("^ddg.", x))}, new.commands)

	# attempt to close the previous collapsible command node if a ddg exists
	if (.ddg.is.init()) .ddg.close.last.command.node(initial=TRUE)

	# Create start and end nodes to allow collapsing of consecutive 
  # console nodes. Don't bother doing this if there is only 1 new 
  # command in the histpry or execution.
  named.node.set <- FALSE
	num.new.commands <- length(new.commands)
	num.actual.commands <- length(filtered.commands)
	# 
	if (num.actual.commands > 0 && .ddg.is.init()) {
		.ddg.add.abstract.node("Start", node.name)
		named.node.set <- TRUE
	}

	# Quote the quotation (") characters so that they will appear in 
  # ddg.txt.
	quoted.commands <- gsub("\\\"", "\\\\\"", new.commands)
	
	# get the last command in the new commands and check to see if we need to create 
	# a new .ddg.last.cmd node for future reference
	.ddg.last.cmd <- list("abbrev" = .ddg.abbrev.cmd(quoted.commands[[num.new.commands]]), 
	                      "expr" = parsed.commands[[num.new.commands]],
                          "text" = new.commands[[num.new.commands]])
	if (substr(.ddg.last.cmd$abbrev, 1, 4) == "ddg.") {
		.ddg.last.cmd <- NULL
	}
	else if (!execute) {
		quoted.commands <- quoted.commands[1:num.new.commands-1]
		parsed.commands <- parsed.commands[1:num.new.commands-1]
	}

	# We tried to use a data frame to contain new.commands, 
  # quoted.commands and parsed.commands, but it does not seem 
  # possible to put the parsed expressions in a data frame.
	
	# Create an operation node for each command.  We can't use lapply 
  # here because we need to process the commands in order and lapply 
  # does not guarantee an order. 
  # Also decide which data nodes and edges
  # to create.  Only create a data node for the last write of a variable
  # and only if that occurs after the last possible writer. Create an edge
  # for a data use as long as the use happens before the first writer/possible
  # writer or after the last writer/possible writer.
  # Lastly, if environ is set to true, then execute each command immediately
  # before attempting to create the DDG nodes. 

  #Only go through this if  we have at least one command to parse.
  if (length(parsed.commands) > 0) {
  	# Find where all the variables are assigned for non-environ files
  	if (!execute) {
			vars.set <- .ddg.find.var.assignments(parsed.commands)
  	} 
  	else {
  		.ddg.set("var.num", 1)
  		vars.set <- .ddg.create.empty.vars.set()
  	}

  	# loop over the commands as well as their string representations
  	# 
  	for (i in 1:length(parsed.commands)) {
  		cmd.expr <- parsed.commands[[i]]
      cmd.text <- new.commands[[i]]
  		cmd <- quoted.commands[[i]]

			# specifies whether or not a procedure node should be created for this command
  		# Basically, if a ddg exists and the command is not a ddg command, it should
  		# be created.

  		create <- !grepl("^ddg.", cmd) && .ddg.is.init() && .ddg.enable.console()

  		# if the command does not match one of the ignored patterns
  		if (!any(sapply(ignore.patterns, function(pattern){grepl(pattern, cmd)}))) {

  			cmd.abbrev <- .ddg.abbrev.cmd(cmd)
  	
  			# If sourcing, we want to execute the command
  			if (execute) {
  				# print command
  				if (echo) {
  					nd <- nchar(cmd)
						do.trunc <- nd > max.deparse.length
            cmd.show <- substr(cmd, 1L, if (do.trunc) 
              max.deparse.length
            else nd)
		        cat(cmd.show)
         	}

         	# 

         	# if we will create a node, then before execution, set this command as
         	# a possible abstraction node but only if it's not a call that itself creates
         	# abstract nodes
  				if (!grepl("^ddg.", cmd)) .ddg.set(".ddg.possible.last.cmd", list("abbrev"=cmd.abbrev,
  				                                   "expr"=cmd.expr, "text"=cmd.text))
  				else if (.ddg.is.procedure.cmd(cmd)) .ddg.set(".ddg.possible.last.cmd", NULL)

         	# evaluate
  				result <- eval(cmd.expr, environ, NULL)

  				# print evaluation
  				if (print.eval) cat(result)

  				# check if initialization call. If so, then create a new console node,
  				# but only if the next command is NOT a ddg command
  				#if(grepl("^ddg.init", cmd) && .ddg.enable.console()) { 
  				#	.ddg.add.abstract.node("Start", "Console")
  				#	.ddg.set(".ddg.last.cmd", list(text="Console",expr="Console"))
  				#}
  			}

  			# figure out if we should create a procedure node for this command. 
  			# We don't create it if it matches a last command (because that last
  			# command has now become a collapsible node). Matching a last command means
				# that the last command is set, is not null, and is equal to the current
				
  			create.procedure <- create && !(!is.null(.ddg.get(".ddg.last.cmd")) && 
  			                                .ddg.get(".ddg.last.cmd")$expr == cmd.expr)

  			# we want to create a procedure node for this command
  			if (create.procedure) {
  				
					# create the procedure node
					.ddg.proc.node("Operation", cmd.abbrev, cmd.abbrev, console=TRUE)
					.ddg.proc2proc()
					if (.ddg.debug()) print(paste(".ddg.parse.console.node: Adding operation node for", cmd.abbrev))

					# store information on the last procedure node in this block
					last.proc.node <- cmd.abbrev

					# we want to create the incoming data nodes (by updating the vars.set)
					if (execute) {
						# add variables to set
						vars.set <- .ddg.add.to.vars.set(vars.set,cmd.expr,i)
						if (.ddg.debug()) print(paste(".ddg.parse.console.node: Adding", cmd.abbrev, "information to vars.set"))
					}

					.ddg.create.data.use.edges.for.console.cmd(vars.set, cmd.abbrev, cmd.expr, i)
					.ddg.link.function.returns(cmd.expr)
					
					if (.ddg.debug()) print(paste(".ddg.parse.console.node: Adding input data nodes for", cmd.abbrev))
					.ddg.create.data.set.edges.for.console.cmd(vars.set, cmd.abbrev, cmd.expr, i)
					if (.ddg.debug()) print(paste(".ddg.parse.console.node: Adding output data nodes for", cmd.abbrev))
				}
				# we wanted to create it but it matched a last command node
				else if (create && execute) .ddg.close.last.command.node(initial=TRUE)

				###### TODO #######
				if (execute) {
					.ddg.create.data.node.for.possible.writes(vars.set, last.proc.node)

					# update so we don't set these again
					vars.set$possible.last.writer <- vars.set$last.writer
				}
  		}
  	}

  	# Create a data node for each variable that might have been set in 
  	# something other than a simple assignment, with an edge from the 
  	# last node in the console block or source 
		if (!execute) .ddg.create.data.node.for.possible.writes(vars.set, last.proc.node)
	}

	# close any node left open during execution
	if (execute) .ddg.close.last.command.node(initial=TRUE)

	# Close the console block if we processed anything and the ddg is initialized (also, save)
	# 
	if (.ddg.is.init() && named.node.set) { 
		.ddg.add.abstract.node("Finish", node.name)
	}

	# Open up a new collapsible node in case we need to parse further later
	if (!execute) {
		
		.ddg.set(".ddg.possible.last.cmd", .ddg.last.cmd)
		.ddg.set(".ddg.last.cmd", .ddg.last.cmd)
		.ddg.open.new.command.node()
	}

	 # Write time stamp to history.
	.ddg.write.timestamp.to.history()
	#print(paste("last.commad:",.ddg.get(".ddg.last.cmd")))
	#print(paste("command:", .ddg.get(".ddg.possible.last.cmd")))

}

# .ddg.console.node creates a console node.
.ddg.console.node <- function(ddg.history.file=.ddg.get(".ddg.history.file"),
                              ddg.history.timestamp=.ddg.get(".ddg.history.timestamp")) {
	# Don't do anything if sourcing, because history isn't necessary in this case
	if(.ddg.enable.source()) return(NULL)

	# Only continue if these values exists
	if (!(is.null(ddg.history.file) || is.null(ddg.history.timestamp))) {
		# grab any new commands that might still be in history
		.ddg.savehistory(ddg.history.file)

		# load from extended history since last time we wrote out a console node
		new.lines <- .ddg.loadhistory(ddg.history.file,ddg.history.timestamp)

		# Parse the lines into individual commands
		parsed.commands <- .ddg.parse.lines(new.lines)

		# new commands since last timestamp
		if (!is.null(parsed.commands)) .ddg.parse.commands(parsed.commands)
	}
}

# .ddg.proc.node creates a procedure node.
.ddg.proc.node <- function(ptype, pname, pvalue="", console=FALSE) {

	# we're not in a console node but we're capturing data automatically
	if (.ddg.enable.console()) {

		# capture graphic output of previous procedure node
		.ddg.auto.graphic.node()

		if(!console) {
			# we're sourcing, so regardless of interactivity, capcture commands
			if (.ddg.enable.source()) {
				.ddg.close.last.command.node(called=".ddg.proc.node")
				.ddg.open.new.command.node(called=".ddg.proc.node")
			}
			# running interactively, so parse command history by making a console node
			else if (interactive()) .ddg.console.node()
		}
  }

	# Increment procedure counter.
	.ddg.inc("ddg.pnum")

	# Include value if available.
	proc.value <- 
		if (pvalue!="") paste(" Value=\"", pvalue, "\"", sep="")
		else ""

	# obtain the timestamp to  use this procedure node
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
	.ddg.record.proc(ptype, pname, pvalue)
	
	if (.ddg.debug()) print(paste("proc.node:", ptype, pname))
}

# .ddg.replace.quotes quotes quotation characters. It also replaces 
# return, newline and tab characters with spaces.

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
# Assumes that dvalue is a list.

.ddg.convert.list.to.string <- function (dvalue) {
  values <- .ddg.replace.quotes(lapply(dvalue, as.character))
  positions <- 1:length(values)
  paste("[[", positions, "]]", values, collapse="\n")
}
	
# .ddg.data.node creates a data node of type Data. Data nodes are 
# used for single data values. The value (dvalue) is stored in the 
# DDG.

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

# .ddg.supported.graphic - the sole purpose of this function is to verify that 
# the input file extension is a supported graphic type. So far, the list of
# supported graphics inlude:
# jpg, jpeg, bmp, png, tiff
.ddg.supported.graphic <- function(ext){
	return(ext %in% c("jpeg", "jpg", "tiff", "png", "bmp", "pdf"))
}

# Factoring of snapshot code.
.ddg.graphic.snapshot <-function(fext, dpfile) {
	# pdfs require a seperate procedure
	if (fext == "pdf") dev.copy2pdf(file=dpfile)

	# at the moment, all other graphic types can be done by constructing a similar function
	else {
		# if jpg, we need to change it to jpeg for the function call
		fext = ifelse(fext == "jpg", "jpeg", fext)

		#First, we create a string, then convert it to an actual R expression and use that as the function.
		strFun <- paste(fext, "(filename=dpfile, width=800, height=500)", sep="")
		parseFun <- function(){eval(parse(text=strFun))}
		dev.copy(parseFun)
	
		# turn it off (this switches back to prev device)
		dev.off()
	}
}

# .ddg.snapshot.node creates a data node of type Snapshot. Snapshots 
# are used for complex data values not written to file by the main 
# script. The contents of data are written to the file dname.fext 
# in the DDG directory. Snapshots are also used to capture output plots 
# and other graphics generated by the R script.
 
.ddg.snapshot.node <- function(dname, fext, data, save.object = FALSE, dscope=NULL) {
	# Increment data counter.
	.ddg.inc("ddg.dnum")
	
  # Get file name.
	ddg.dnum <- .ddg.dnum()

	# default file extensions
	dfile <- 
		if (fext == "" || is.null(fext)) paste(ddg.dnum, "-", dname, sep="")
		else                             paste(ddg.dnum, "-", dname, ".", fext, sep="")

	# Get path plus file name.
	ddg.path <- .ddg.path()
	dpfile <- paste(ddg.path, "/", dfile, sep="")
  	
	# Write to file .
	if (fext == "csv") write.csv(data, dpfile, row.names=FALSE)

	# Capture graphic
	else if (.ddg.supported.graphic(fext)) .ddg.graphic.snapshot(fext, dpfile)

	# write out RData (this is old code, not sure if we need it)
	else if (fext == "RData") file.rename(paste(ddg.path, "/", dname, sep=""), dpfile)

  # write out text file for txt, empty fext
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

	# write out data node object (use both save and )
	# the file format is unsupported
	else {
    	error.msg <- paste("File extension", fext, "not recognized")
    	.ddg.insert.error.message(error.msg)
    	return(NULL)
	}

	# check to see if we want to save the object
  if (save.object) save(data, file = paste(dpfile, ".RObject"), ascii = TRUE)
	
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

# .ddg.file.node creates a node of tyoe File. File nodes are used for files
# written out either by capturing output from the script or by copying a file
# that is written by the script into the DDG directory. Returns the path 
# where the file referenced by the node should be stored.
.ddg.file.node <- function(dtype,fname,dname, dscope=NULL) {
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
 
.ddg.file.copy <- function(dtype, fname, dname, dscope) {
	# Calculate location of original file
	file.loc <- normalizePath(fname, winslash="/", mustWork = FALSE)

	# Copy file.
	if (file.exists(file.loc)) {
	   # Create file node in DDG
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

.ddg.insert.error.message <- function(msg) {
  	warning(msg)
  	.ddg.data.node("Exception", "error.msg", msg, "ddg.library")
  	.ddg.lastproc2data("error.msg")
}

# .ddg.lookup.function.name gets the name of the calling function and 
# returns it as a string. pname - the name passed in to the DDG 
# function.  It may be NULL, a string, or a name. Note that it is 
# important that these be macros, not functions, due to the use of 
# the substitute function in the body.

.ddg.lookup.function.name <- gtools::defmacro (pname,
		expr = 
				# If pname is not provided, get from function call.
				if (is.null(pname)) {
					# Look up function call.
					call <- sys.call(-4)
					
					# Discard everything after left parenthesis to get function 
          # name.
					# pname <- strsplit (as.character(call), "\\(")[[1]][1]
					pname <- as.character(call[[1]])
				}
				
				# Convert pname to a string if necessary.
				else if (!is.character(pname)) pname <- deparse(substitute(pname))
)

# .ddg.lookup.value is used to determine what value to use when 
# creating data nodes. expr - the expression that needs to be 
# evaluted.  This can be a string or a name. value - the value that 
# was passed in to the calling function. If value already exists, 
# nothing happens.  If value is NULL, the expression is evaluated to 
# determine the value. env - the environment in which evaluation is 
# done. procname - the name of the calling procedure, used to produce 
# an error message if necessary. warn - if true, warns user that the 
# expression could not be evaluated. Note that it is important that 
# these be macros, not functions, due to the use of the substitute 
# function in the body.

.ddg.lookup.value <- gtools::defmacro(expr, value, env, procname, warn = TRUE,
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

# .ddg.delete.temp deletes any temporary files created during the processing 
# of a script. These include:
#	1. The temporary history file
.ddg.delete.temp <- function() {
	# delet the temporary history file if we made it
	if (.ddg.is.set('ddg.history.file')) unlink(.ddg.get('ddg.history.file'))

	# clear the environment
	.ddg.env <- new.env(parent=emptyenv())
}

# .ddg.get.frame.number gets the frame number of the closest
# non-library calling function.

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

#.ddg.get.env gets the environment in which name is declared

.ddg.get.env <- function(name, for.caller=FALSE, calls=NULL) {
  if (is.null(calls)) calls <- sys.calls()
  fnum <- .ddg.get.frame.number(calls, for.caller)
  stopifnot(!is.null(fnum))
  
  
  # I broke this statement up into 2 statements so that we
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

.ddg.get.scope <- function(name, for.caller=FALSE, calls=NULL) {
	# get the environment for the variable call
	env <- .ddg.get.env(name, for.caller, calls)

	# if no environment found, name does not exist, so scope is undefined
  if (is.null(env)) return ("undefined")

  # Evaluation env, capture the output, and look at the first result (this is
  # the result that gives you )
	scope <- sub('^<environment: (.*)>$', '\\1', capture.output(env)[1])
	if (grepl("undefined", scope)) scope <- "undefined"
	return(scope)
}

# .ddg.is.local returns true if the name is local to the scope
.ddg.is.local <- function(name, scope) {
	return(exists(name, scope, inherits=FALSE))
}


#--------------------USER FUNCTIONS-----------------------#

# ddg.procedure creates a procedure node of type Operation. pname 
# (optional) - the label for the node.  If the label corresponds to 
# the name of an R function, the user will be able to right-click on 
# the node and view the definition of the R function. pname be 
# passed in as a string or as a name. If pname is missing, the node 
# is labeled with the name of the function that called ddg.procedure.

# ins (optional) - the names of data nodes that should be linked as 
# inputs to this procedure node. These MUST be passed as strings, 
# not names, unless the value is a file name.

# lookupIns (optional) - if true and ins is NULL, data edges will be 
# created to the arguments of the function that called ddg.procedure, 
# if the corresponding data nodes exist.

# outs.graphic - the name of the snapshot node that should be linked as
# output to this procedure node. The names should be a string, and is used
# as the file name of the saved snapshot. A graphical snapshot is simply a 
# captured image of the graphic device active at the time of call to ddg.procedure.
# outs.data - the name of the data nodes that should be created and linked
# as outputs from this procudure. These MUST be passed as strings, not names.
# outs.exception - the name of exception nodes that should be created and linked 
# as outputs from this procedure. 
# outs.url - the name of the url nodes that should be linked as output to
# this procedure node
# outs.file = the name of the file nodes that should be linked as output to 
# this procedure node, unless the value is a file name. Supported 
# file extensions include: .csv, .jpg, .jpeg, .pdf, and .txt. If 

# graphic.fext - the file extention to be used when saving the captured graphics
# Supported extensions are .jpg, .jpeg, .pdf


ddg.procedure <- function(pname=NULL, ins=NULL, lookup.ins=FALSE, outs.graphic=NULL, outs.data=NULL, 
                          outs.exception=NULL, outs.url=NULL, outs.file=NULL, graphic.fext="jpeg") {
	if (!.ddg.is.init()) return(invisible())
	
	.ddg.lookup.function.name(pname)
	
	if (!lookup.ins) {
		.ddg.proc.node("Operation", pname, pname)
		
		# Create control flow edge from preceding procedure node.
		.ddg.proc2proc()
	}
	else if (interactive() && .ddg.enable.console()) .ddg.console.node()
	
	# Create the input edges if ins list provided.
	if (!is.null(ins)) {
		# Get scope.  Cannot use lapply because that results in the creation of a new 
		# stack, while .ddg.get.scope assumes that it is called 
		#scope <- .ddg.get.scope(ins[[1]], for.caller = TRUE)
		stack <- sys.calls()
		function.scope <- parent.frame()
		
		lapply(ins, 
				function(param) {
					# First see if param is defined in the function where ddg.procedure is called.
					# If that does not find it, look in the scope of the function that calls the function
					# that calls ddg.procedure.  The difference probably depends on whether ddg.procedure
					# is used to annotate a chunk of code where param is really in the local scope
					# but used in the annotated chunk (the first case) or to annotate an entire
					# function and param is an actual funciton parameter (the second case).
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
							# This could be the case if the parameter is the name of a file
							# rather than a variable in the program
							.ddg.data2proc(param, "undefined", pname)
							if (.ddg.debug()) print(paste("param:", param))
						}
						
						else {
							# Attempt to allow names, not strings.  This does not work 
							# as written because what we get when we call substitute is 
							# the parameter provided by lapply, not the one provided to 
							# ddg.procedure.  We will have the same problem dealing 
							# with outs.
							
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
	# Look up input parameters from calling environment.
	else if (lookup.ins) {
		call <- sys.call(-1)
		#tokens <- unlist(strsplit (as.character(call), "[(,)]"))
    
    # match.call expands any argument names to be the 
    # full parameter name
    full.call <- match.call(sys.function(-1), call=call)
    
    # tokens will contain the function name and the argument
    # expressions
		tokens <- as.character(full.call)
    
		# Get parameters and create edges.
		if (length(tokens) > 1) {
			# args contains the names of the variable that was passed into the function
			args <- tokens[2:length(tokens)]
			# param,names contains the names of the parameters (this is what the variable is known as inside the function)
	        param.names <- names(full.call)
	        param.names <- param.names[2:length(param.names)]
			stack <- sys.calls()
			#scope <- .ddg.get.scope(args[[1]], for.caller = TRUE)
			bindings <- list()
			for (i in 1:length(args)) bindings[[i]] <-c(args[[i]], param.names[[i]])
			missing.params <- character()
			
			#lapply(args, 
			lapply(bindings, 
					function(binding) {	
						# here, param is now the arguments passed IN
						param <- binding[1]
						# formal is the paramenter name of the function (what is the variable known as inside?)
						formal <- binding[2]

						# Find all the variables used in this parameter.
						vars.used <- .ddg.find.var.uses(parse(text=param))

						binding.node.name <- paste(formal, " <- ", param)
						.ddg.proc.node("Binding", binding.node.name)
						.ddg.proc2proc()
						for (var in vars.used) {
							param.scope <- .ddg.get.scope(var, for.caller = TRUE, calls=stack)
							if (.ddg.data.node.exists(var, param.scope)) {
								.ddg.data2proc(as.character(var), param.scope, binding.node.name)
								if (.ddg.debug()) print(paste("param:", var))
							}
						}
						formal.scope <- .ddg.get.scope(formal, calls=stack)
						formal.env <- .ddg.get.env(formal, calls=stack)
						.ddg.save.data(formal, eval(parse(text=formal), formal.env), fname=".ddg.save.data", scope=formal.scope, stack=stack)
						.ddg.proc2data(binding.node.name, formal, formal.scope)
					})
		}
		.ddg.proc.node("Operation", pname, pname)
    if (length(tokens) > 1) {
      lapply(bindings, function(binding) {
    					formal <- binding[2]
    					formal.scope <- .ddg.get.scope(formal, calls=stack)
    					if (.ddg.data.node.exists (formal, formal.scope)) {
    						.ddg.data2proc(formal, formal.scope, pname)
    					}
    				})
    }
		
		# Create control flow edge from preceding procedure node.
		.ddg.proc2proc()
	}
	
	# Capture graphics device
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
		#scope <- .ddg.get.scope(outs.exception[[1]])
		
		lapply(outs.exception,
				function(param) {
					# Get value in calling environment.
					name <- param
					value <- NULL
					env <- parent.frame(3)
					.ddg.lookup.value(name, value, env, "ddg.procedure", warn=FALSE)
					
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
		#scope <- .ddg.get.scope(outs.url[[1]])
		
		lapply(outs.url,
				function(param) {
					# Get value in calling environment.
					name <- param
					value <- NULL
					env <- parent.frame(3)
					.ddg.lookup.value(name, value, env, "ddg.procedure", warn=FALSE)
					
					# URL node.
					scope <- .ddg.get.scope(param, calls=stack)
					.ddg.data.node("URL", name, value, scope)
					.ddg.proc2data(pname, name, scope)
				}
		)
	}
	
	# Generalized data node (includes simple data values as well as snapshots)
	if (!is.null(outs.data)) {
		# Get scope.
		#scope <- .ddg.get.scope(outs.data[[1]])
		stack <- sys.calls()
		lapply(outs.data,
				function(param) {
					# Get value in calling environment.
					name <- param
					value <- NULL
					env <- parent.frame(3)
					.ddg.lookup.value(name, value, env, "ddg.procedure", warn=FALSE)
					
					tryCatch({
								if (!is.character(name)) name <- deparse(substitute(name))
								envName <- environmentName(env)
								scope <- .ddg.get.scope(param, calls=stack)
								.ddg.save.data(name,value,".ddg.procedure", error=TRUE, scope=scope)
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
		#scope <- .ddg.get.scope(outs.file[[1]])
		stack <- sys.calls()
		
		lapply(outs.file,
				function(param) {
					# Get value in calling environment.
					name <- param
					value <- NULL
					env <- parent.frame(3)
					.ddg.lookup.value(name, value, env, "ddg.procedure", warn=FALSE)
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
	invisible()
}

# ddg.return creates a data node to hold the return value of a function call.
# It also creates an incoming edge from the function node that is returning.
# It adds information about the call and the node created to a table so
# that it can be linked to later where the return is used.
ddg.return <- function (expr) {
	if (!.ddg.is.init()) return(expr)
	pname <- NULL
	.ddg.lookup.function.name(pname)
	if (length(expr) == 0) return()
	
	# prints the call & arguments
	if (.ddg.debug()) {
		print(paste("ddg.return:", sys.call(-1), "returns", expr))
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
	
	# Create a data node for the return value
	# Want the scope of the function that called the function that called ddg.return
    call <- deparse(sys.call(-1))
	return.node.name <- paste(call, "return")
	return.node.scope <- 
        environmentName (if (sys.nframe() == 2) .GlobalEnv
                         else parent.env(sys.frame(-1)))
	.ddg.save.data(return.node.name, expr, fname="ddg.return", scope=return.node.scope)

	# Create an edge from the function to its return value
	.ddg.proc2data(pname, return.node.name, return.node.scope)
	
	# Update the table
	ddg.num.returns <- ddg.num.returns + 1
	ddg.return.values$ddg.call[ddg.num.returns] <- call
	ddg.return.values$return.used[ddg.num.returns] <- FALSE
	ddg.return.values$return.node.id[ddg.num.returns] <- .ddg.dnum()
	.ddg.set(".ddg.return.values", ddg.return.values)
	.ddg.set(".ddg.num.returns", ddg.num.returns)
	
	return(expr)
}

ddg.eval <- function(statement) {
	parsed.statement <- parse(text=statement)
	frame.num <- .ddg.get.frame.number(sys.calls())
	env <- sys.frame(frame.num)
	if (!.ddg.is.init()) {
		eval(parsed.statement, env)
		return(invisible())
	}
	
	.ddg.parse.commands(parsed.statement, environ=env, node.name=statement)
    .ddg.link.function.returns(parsed.statement)

	# Create outflowing edges 
	.ddg.statement <- list("abbrev" = .ddg.abbrev.cmd(gsub("\\\"", "\\\\\"", statement)), 
					"expr" = parsed.statement,
					"text" = statement)
	
	vars.set <- .ddg.find.var.assignments(.ddg.statement$expr)
	.ddg.create.data.set.edges.for.cmd(vars.set, .ddg.statement$abbrev, .ddg.statement$expr, 1, stack=sys.calls())
}

# ddg.data creates a data node for a single or comple data value. 
# dname - label for the node.  This can be passed as a string, name, or expression. 
# dvalue (optional) - the value of the node.  
# graphic.fext - the file extention to be used for saving the caputure variable 
# if the variable is a graphical output. Otherwise ignored. Default is jpeg.
# If the value is omitted, the argument passed in for dname is evaluated in the 
# calling environment to determine the value. If the value is determined to be complex,
# the output data is written out ot a csv if possible. Otherwise, the data are
# written out as a .txt file if the variable is determined to be an object.

ddg.data <- function(dname, dvalue=NULL, graphic.fext = "jpeg") {
	if (!.ddg.is.init()) return(invisible())

	# Look up the value if one was not provided.
	env <- parent.frame()
	.ddg.lookup.value(dname, dvalue, env, "ddg.data")
  
	# save the value appropriately.  If the name is not a string,
  	# use the argument instead of the value
	if (!is.character(dname)) dname <- deparse(substitute(dname))
	.ddg.save.data(dname, dvalue, "ddg.data", graphic.fext)
}

# ddg.exception creates a data node for an exception. dname - label 
# for the node.  This can be passed as a string, name, or expression. 
# dvalue (optional) - the value of the node.  If the value is 
# omitted, the argument passed in for dname is evaluated in the 
# calling environment to determine the value.

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

# ddg.url creates a data node for a URL. dname - label for the node. 
# dvalue (optional) - the full URL.  If a value is not provided, the 
# argument passed in for dname is evaluated in the calling 
# environment to determine the value.

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
# file to the DDG directory. filename - name of the file to copy, 
# including path to the file if it is not in the working directory. 
# dname (optional) - label for the node.  If omitted, the filename, 
# minus the directory path, is used as the label.

ddg.file <- function(filename, dname=NULL) {
	if (!.ddg.is.init()) return(invisible())

	scope <- if (!is.null(dname)) .ddg.get.scope(dname)
			 else NULL
	invisible(.ddg.file.copy("File", filename, dname, scope))
}

# ddg.data.in creates a data flow edge from data node dname to 
# procedure node pname. pname (optional) - the name of the procedure 
# that created this data value.  This can be passed as a string or as 
# a name.  If omitted, the name of the function that called 
# ddg.data.in is used. dname - the label for the data node.  This 
# can be passed as a string, name, or expression.

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
        # name, using the scope "undefined"
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

# ddg.data.out creates a data/snapshot node of type "Data"/"Snapshot" called 
# dname with value dvalue. It also creates a data flow edge from procedure node 
# pname to data node dname. Use for simple or complex data values. The contents 
# of complex data are written to the file dname (with numerical prefix) on the 
# DDG directory. Complex vs Simple data is automatically determined by the function.
# dname - the label for the data node being created.  This can be passed as a 
# string, name, or expression. 
# dvalue (optional) - the value to associate with the node.  If no value is given, 
# the argument passed in for dname is evaluated in the calling environment. 
# pname (optional) - the name of the procedure that created this data 
# value.  This can be passed as a string or as a name.  If omitted, 
# the name of the function that called ddg.data.out is used.
# graphic.fext (optional) - the file extension which should be used when saving
# a graphics file. This is only used if the value saved is determined to be 
# a graphic, otherwise the parameter goes ignored.

ddg.data.out <- function(dname, dvalue=NULL, pname=NULL, graphic.fext="jpeg") {
	if (!.ddg.is.init()) return(invisible())

	# If no value is provided, get value in calling environment.
	env <- parent.frame()
	.ddg.lookup.value(dname, dvalue, env, "ddg.data.out")

	# convert name to a string if necessary
	if (!is.character(dname)) dname <- deparse(substitute(dname))

	# save the complex data in appropriate format
	.ddg.save.data(dname, dvalue, "ddg.data.out", graphic.fext)
	
	.ddg.lookup.function.name(pname)

	# Create data flow edge from operation node to data node.
	.ddg.proc2data(pname, dname)
}

# ddg.exeception.out creates a data node of type Exception. It also 
# creates a data flow edge from the procedure node pname to this 
# node. dname - the label for the exception node being created.  
# This should be a string. dvalue (optional) - the value to 
# associate  with the node.  If no value is given, the argument 
# passed in for dname is evaluated in the calling environment. pname 
# (optional) - The name of the procedure that created this exception. 
# This can be passed as a string or as a name.  If omitted, the name 
# of the function that called ddg.exception.out is used.

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
# node pname to data node dname. Use for URL addresses. dname - the 
# label for the data node being created. dvalue (optional) - the 
# full URL. If a value is not provided, the argument passed in for 
# dname is evaluated in the calling environment to determine the 
# value. pname (optional) - the name of the procedure that created 
# this URL node.  This can be passed as a string or as a name. If 
# omitted, the name of the function that called ddg.url.out is used.

ddg.url.out <- function(dname, dvalue=NULL, pname=NULL) {
	if (!.ddg.is.init()) return(invisible())

	# If no value is provided, get value in calling environment.
	env <- parent.frame()
	.ddg.lookup.value(dname, dvalue, env, "ddg.url.out")
  
  # URL labels are not necessarily variables, so make sure
  # it is a variable before trying to determine its scope
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
# copying an existing file to the DDG directory. The path to the 
# original file is optionally specified in dloc (otherwise the 
# current working directory is assumed). A data flow edge is also 
# created from procedure node pname to data node dname. The existing 
# output file is copied to a new file called dname (with numerical 
# prefix) on the DDG directory. Use for output files already created 
# by the main script.

# filename - name of the file.  The name should include the path to 
# the file if it is not in the working directory. dname (optional) - 
# the label for the node.  If omitted, the filename, minus the 
# directory path, is used as the label. pname (optional) - the name 
# of the procedure that created this node.  This can be passed as 
# a string or as a name.  If omitted, the name of the function that 
# called ddg.file.out is used. ddg.file.out returns the full path 
# to the file that is saved.

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

# ddg.graphic.out creates a data node of type Snapshot called dname by 
# capturing the current image in the active graphics device and saving it in the
# DDG directory. The file is named under the name filename with numeric prefix 
# and with extention specified by the fext parameter (available extensions are 
# bmp, jpeg, png, tiff).
# A data flow edge is also created from procedure node pname to the data node dame.

# dname - the label for this node. This is also used as the name for the file.
# pname (optional) - the name of the procedure that created this node. This can be
# passed as a string or as a name. If ommited, the name of the function that 
# called ddg.graphic.out is used. This means that this parameter is NOT option
# when using this library function at outside a user-written function
# fext (optional) - the file extention to be used for the graphic device capture.
# This value defaults to jpeg.

ddg.graphic.out <- function(dname, pname=NULL, graphic.fext="jpeg") {
	if(!.ddg.is.init()) return
	# write out the graphic
	.ddg.write.graphic(dname, 'Graphical Plot. Not saved in script.', graphic.fext)

	.ddg.lookup.function.name(pname)

	# Create the data flow edge from oepration node to the file node
	.ddg.proc2data(pname,dname)
}

# ddg.start creates a procedure node of type Start called pname. 
# pname (optional) - the label for the node.  This can be passed as 
# a string or as a name.  If omitted, the name of the function that 
# called ddg.start is used. Users can right-click on a start node 
# in DDG Explorer and see the code between start and finish nodes 
# in the original script.

ddg.start <- function(pname=NULL) {
	if (!.ddg.is.init()) return(invisible())

	.ddg.lookup.function.name(pname)

	# Check null
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
# pname (optional) - the label for the node.  This can be passed as 
# a string or as a name.  If omitted, the name of the function that 
# called ddg.finish is used. Users can right-click on a finish node 
# in DDG Explorer and see the code between start and finish nodes 
# in the original script.

ddg.finish <- function(pname=NULL) {
	if (!.ddg.is.init()) return(invisible())

	.ddg.lookup.function.name(pname)

	# Check null
	if(is.null(pname)) {
		msg <- "Cannot call ddg.finish with NULL value from top-level."
  	.ddg.insert.error.message(msg)
  }

  # Create finish non-operational step.
	.ddg.proc.node("Finish", pname, pname)

	# Create control flow edge from preceding procedure node.
	.ddg.proc2proc()
}

# The function ddg.grabhistory grabs the current console history and creates
# the partial DDG. It does not, however, write this to the file system.
# The function should be called only if more than 512 lines of code need to be 
# interpreted automatically (usually because the script is being annoated with 
# enable.console = TRUE). It should also be called if the error: "Part of history
# is missing. DDG may be incomplete!" is raised. 
# Finally, it should be called periodically to assure no history is missing.
ddg.grabhistory <- function() {
	if (!.ddg.is.init()) return(invisible())

	# only act if in intereactive mode and with enabled console
	if (interactive() && .ddg.enable.console()) {
		.ddg.console.node()
	}

	invisible()
}

# ddg.init intializes a new ddg. r.script.path (optional) - the full 
# path to the R script file.  If provided, a copy of the script will 
# be saved with the DDG. ddgdir (optional) - the directory where the 
# DDG should be saved. If not provided, the DDG will be saved in a 
# subdirectory called "ddg" in the current working directory.

ddg.init <- function(r.script.path = NULL, ddgdir = NULL, enable.console = TRUE) {
	.ddg.init.tables()

	.ddg.set("ddg.r.script.path", 
		if (is.null(r.script.path)) NULL
		else normalizePath(r.script.path, winslash="/"))
	.ddg.set("ddg.path", 
		if (is.null(ddgdir)) paste(getwd(), "ddg", sep="/") 
		else ddgdir)
	
	# set environment constants
	.ddg.set(".ddg.enable.console", enable.console)
	.ddg.init.environ()
	
	# mark graph as initilized
	.ddg.set(".ddg.initialized", TRUE)

	# store the starting graphics device
	.ddg.set("prev.device", dev.cur())
	
	if (interactive() && .ddg.enable.console()) {
		ddg.history.file <- paste(.ddg.path(), ".ddghistory", sep="/")
		.ddg.set(".ddg.history.file", ddg.history.file)
		
		# Empty file if it already exists, do the same with tmp file
    file.create(ddg.history.file)
		
		# one timestamp keeps track of last ddg.save (the default)
 		.ddg.write.timestamp.to.history()

		# save the history
		savehistory(ddg.history.file)
	}

	invisible()
}

# ddg.run executes the function fname which contains the main 
# program. If an R error is generated, the error message is captured 
# and saved in the DDG. Note: this function includes calls to 
# ddg.init() and ddg.save(), so it is not necessary to call those 
# functions from an instrumented script if ddg.run is used. f - the 
# function to run (optional). r.script.path (optional) - the full path to the 
# R script file.  If provided, a copy of the script will be saved 
# with the DDG. ddgdir (optional) - the directory where the DDG 
# should be saved.  If not provided, the DDG will be saved in a 
# subdirectory called "ddg" in the current working directory.
# Note that one of f and r.script.path must given. An error is raised if neither is
# given. If f is given or both are given, then the function f is executed with calls 
# to ddg.init and save so provenance for that function can be captured. If only 
# r.script.path is given, then the r script is sourced as a function and a DDG is
# created for it.

ddg.run <- function(r.script.path = NULL, ddgdir = NULL, f = NULL, enable.console = TRUE) {
    ddg.init(r.script.path, ddgdir, enable.console)
	
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

# ddg.save inserts attribute information and the number of procedure 
# steps at the top of the DDG. It writes the DDG, the procedure nodes 
# table, and the data nodes table to the DDG directory.

# The quit parameter saves and then flushes out the DDG.

ddg.save <- function(quit=FALSE) {

	if (!.ddg.is.init()) return(invisible())
	
	# restore history settings
	if (interactive() && .ddg.enable.console()) {
		Sys.setenv("R_HISTSIZE"=.ddg.get('ddg.original.hist.size'))
	}

	# Get final commands
	ddg.grabhistory()
	
	# Get environment parameters.
	ddg.env <- .ddg.environ()
	
	# Insert the environment parameters and the number of procedures 
  # at the top of the DDG. Use a variable other than ddg.text so 
  # that ddg.save can be called more than once on the same DDG 
  # without generating more than one copy of the attributes.
	output <- paste(ddg.env, .ddg.pnum(), "\n", .ddg.get("ddg.text"), sep="")

	# delete temporary files
	.ddg.delete.temp()
	
	# Save DDG to file.
	ddg.path <- .ddg.path()
	fileout <- paste(ddg.path, "/ddg.txt", sep="")
	write(output, fileout)
	
	# Save procedure nodes table to file.
	fileout <- paste(ddg.path, "/pnodes.txt", sep="")
	ddg.proc.nodes <- .ddg.proc.nodes()
	write.table(ddg.proc.nodes[ddg.proc.nodes$ddg.num > 0, ], fileout, quote=FALSE, na="", row.names=FALSE, col.names=FALSE)
	
	# Save data nodes table to file.
  fileout <- paste(ddg.path, "/dnodes.txt", sep="")
	ddg.data.nodes <- .ddg.data.nodes()
	write.table(ddg.data.nodes[ddg.data.nodes$ddg.num > 0, ], fileout, quote=FALSE, na="", row.names=FALSE, col.names=FALSE)
	
	# Save the function return table to file
	fileout <- paste(ddg.path, "/returns.txt", sep="")
	ddg.returns <- .ddg.get(".ddg.return.values")
	write.table(ddg.returns[ddg.returns$return.node.id > 0, ], fileout, quote=FALSE, na="", row.names=FALSE, col.names=TRUE)

	# by convention, this is the final call to ddg.save
	if (quit) {
		# 
		# delete temporary files
		.ddg.delete.temp()

		# capture current graphics device
		.ddg.auto.graphic.node(dev.to.capture=dev.cur)

		# shut down the ddg
		.ddg.clear()
	}

	invisible()
}

# ddg.source reads in an r script and executes it in the provided enviroment.
# ddg.source essentially mimics the behaviour of the source command, having similar
# input parameters and results, but additionally contains the following parameters:
# @param - ignore.ddg.calls
# @param - ignore.init
ddg.source <- function (file, local = FALSE, echo = verbose, print.eval = echo, 
    verbose = getOption("verbose"), max.deparse.length = 150, chdir = FALSE, encoding = getOption("encoding"),
    ignore.ddg.calls = TRUE, ignore.init = ignore.ddg.calls, force.console=ignore.init){
	### CODE IN THIS SECTION IS BASICALLY REPLICATION OF source FUNCTION ###
	# Get the environment under which the script should be executed
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

  # parse encoding information
  have_encoding <- !missing(encoding) && encoding != "unknown"
  if (!missing(echo)) {
      if (!is.logical(echo)) 
          stop("'echo' must be logical")
      if (!echo && verbose) {
          warning("'verbose' is TRUE, 'echo' not; ... coercing 'echo <- TRUE'")
          echo <- TRUE
      }
  }

  # print extra information about environment
  if (verbose) {
      cat("'envir' chosen:")
      print(envir)
  }

  # parse input file and figure out encoding
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

  # parse the expressions from the file
  exprs <- if (!from_file) {
    if (length(lines)) 
      parse(stdin(), n = -1, lines, "?", srcfile, 
          encoding)
    else expression()
  }
  else parse(file, n = -1, NULL, "?", srcfile, encoding)
  
  on.exit()
  
  # Set the working directory for the current script and expressions
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

  # Calculate the regular expressions for what should be ignored and what shouldn't
	if (ignore.ddg.calls && !ignore.init) {
		if(verbose) warning("'ignore.ddg.calls' is TRUE, 'ignore.int' not; ... coercion 'ignore.init <- TRUE'")
		ignore.init <- TRUE
	}

	# ignores calculation of certain execution steps
	ignores <- c("^library[(]RDataTracker[)]$", 	
		if(ignore.ddg.calls) "^ddg."
		else if (ignore.init) c("^ddg.init", "^ddg.run")
		else "a^")

	# TODO add extra code to exprs, in case it's needed?

  # now we can parse the commands as we normally would for a DDG
  if(length(exprs) > 0) {

  	
  	# Turn on the console if forced to, keep track of previous setting, parse 
  	# previous commands if necessary
  	prev.on <- .ddg.is.init() && .ddg.enable.console()
  	if (prev.on && interactive()) .ddg.console.node()
  	if (force.console) ddg.console.on()
  
  	# Let library know that we are sourcing a file
  	prev.source <- .ddg.is.init() && .ddg.enable.source()

  	# Initialize the tables for ddg.capture (also, we don't like er)
  	.ddg.set("from.source", TRUE)

  	# parse the commands into a console node
  	.ddg.parse.commands(exprs, environ=envir, ignore.patterns=ignores, node.name=filename,
  	                    echo = echo, print.eval = print.eval, max.deparse.length = max.deparse.length)
  	
  	# save the DDG among other things, but don't return any values
  	ddg.save()
  	.ddg.set("from.source", prev.source)

  	# Turn return console to previous state
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

# ddg.console.off turns off the console mode of DDG construction
ddg.console.off <- function() {
	if (!.ddg.is.init()) return(invisible())
	#
	# capture history if console was on up to this point
	if (interactive() && .ddg.enable.console()) {
		.ddg.console.node()
	}

		# set the console to off
	.ddg.set(".ddg.enable.console", FALSE)
}

# ddg.console.on turns on the console mode of DDG construction
ddg.console.on <- function() {
	if (!.ddg.is.init()) return(invisible())
	#
	# write a new timestamp if we're turning on the console so we only capture
	# history from this point forward
	if (!.ddg.enable.console()) .ddg.write.timestamp.to.history()

	.ddg.set(".ddg.enable.console", TRUE)
}
 
# ddg.flush.ddg removes selected files from the DDG directory.
ddg.flush.ddg <- function (ddg.path = NULL) {
	if (is.null(ddg.path)) {
		if (.ddg.is.init()) ddg.path <- .ddg.path()
		else return(invisible())
	}

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

ddg.checkpoint <- function(checkpoint.name=NULL) {
	stop("Call source(DDGCheckpoint.R to load ddg.checkpoint and ddg.restore")
}

ddg.restore <- function(file.path) {
	stop("Call source(DDGCheckpoint.R to load ddg.checkpoint and ddg.restore")
}
