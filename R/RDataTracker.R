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
ddg.MAX_HIST_LINES <- 16384

#-------- FUNCTIONS TO MANAGE THE GLOBAL VARIABLES--------#

# Global variables cannot be used directly in a library.  Instead, we 
# need to place the variables in our own environment.  These 
# functions make that environment easier to use.

.onLoad <- function(libname, pkgname) {
	.ddg.init.tables()
}

.ddg.set <- function(var, value) {
	.ddg.env[[var]] <- value
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
	if (!exists("ddg.debug", envir=.ddg.env)) .ddg.set("ddg.debug", FALSE)

	# Set current number of checkpoints.
	.ddg.set("ddg.checkpoint.num", 0)

  # Create table for checkpoints.
	.ddg.set("ddg.checkpoints", data.frame(filename=character(ddg.MAX_CHECKPOINTS),
          checkpoint.name=character(ddg.MAX_CHECKPOINTS), stringsAsFactors=FALSE))
	
	# Record last command from the preceding console block.
	.ddg.set(".ddg.last.command", NULL)
}

# Wrrapper to easily change history lines during execution of script
.ddg.set.history <- function(lines = 16384){
	Sys.setenv("R_HISTSIZE" = lines)
}

# .ddg.init.environ() sets up the filesystem and R environments for use
.ddg.init.environ <- function() {
	dir.create(.ddg.path(), showWarnings = FALSE)
	.ddg.set('ddg.original.hist.size', Sys.getenv('R_HISTSIZE'))
	if (interactive() && .ddg.enable.console()) .ddg.set.history()
}

# ddg.environ gets environment parameters for the DDG.

.ddg.environ <- function() {
	architecture <- R.Version()$arch
	operating.system <- .Platform$OS.type
	r.version <- R.Version()$version
	time <- .ddg.timestamp()
	environ <- paste("Architecture=\"", architecture, "\"\n", sep="")
	environ <- paste(environ, "OperatingSystem=\"", operating.system, "\"\n", sep="")
	environ <- paste(environ, "Language=\"R\"\n", sep="")
	environ <- paste(environ, "LanguageVersion=\"", r.version, "\"\n", sep="")
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

### .ddg.check.init is a function definition to be used at the beginnign of all 
#		user accesible functions. It verifies that the DDG has been initialized.
#   If it hasn't, it returns False. 
###
.ddg.check.init <- function() {
		# || short circuits evaluation
		return(exists(".ddg.initilized", envir=.ddg.env) && .ddg.get(".ddg.initilized"))
}

# Assumes input format is yyyy-mm-dd hh:mm:ss
# Reformat to  (yyyy-mm-ddThh.mm.ss).
.ddg.format.time <- function(time) {
  time <- paste(substr(time, 1, 10), "T", substr(time, 12, 19), sep="")
  return (gsub(":", ".", time))
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

# .ddg.save.simple takes in a simple name, value pairing and saves it to the ddg.
# It does not however create any edges.
.ddg.save.simple <- function(name,value) {
	# save the true value
	.ddg.data.node("Data", name, value)
}

# .ddg.write.graphic takes as input the name of a variable as well as the value 
# (the data) associated with it and attempts to write it out as a graphics file
# If all else fails, it writes out the informaion as a text file and also writes
# out an RData Object which can later be read back into the system 
.ddg.write.graphic <- function(name, value, fext="jpeg"){
	# try to output graphic value
	tryCatch({
		.ddg.snapshot.node(name, fext, NULL)
	}, error = function(e) {
		warning(paste("Attempted to write", name, "as", fext, "snapshot. Trying jpeg", ".", e))
		tryCatch({
			.ddg.dec("ddg.dnum")
			.ddg.snapshot.node(name, "jpeg", NULL)
		}, error = function(e) {
			warning(paste("Attempted to write", name, "as jpeg snapshot. Failed.", e, 
			        "Defaulting to saving RObject and .txt file."))
			.ddg.dec("ddg.dnum")
  		.ddg.snapshot.node(name, "txt", value, save.object = TRUE)
  	})
	})
}

# .ddg.write.csv takes as input the name, value pairing for a variable and attempts
# to save the data as a csv. It does not create any edges but does add the node to
# the DDG. Edge creation should occur from wherever this function is called.
.ddg.write.csv <- function(name, value) {
  tryCatch({
		.ddg.snapshot.node(name, "csv", value)
	}, error = function(e) {
		#warning(paste("Attempted to write", name, "as .csv snapshot but failed. Out as RDataObject.", e))

		# first decrease node count which has increased above by the call to snapshot.node
		.ddg.dec("ddg.dnum")

		# save the node 
		.ddg.snapshot.node(name, "txt", value, save.object = TRUE)
	})
}

# .ddg.save.data takes as input name and value of a data node that needs to be created
# It determines how the data should be output (or saved) and saves it in that format.
# The name of the created node is name, its value is value. fname is the name of the
# calling function and is used to generate helpful error messages if something goes wrong.
# error indicates whether the function should raise an R Error as opposed to a ddg error.
.ddg.save.data <- function(name, value, fname=".ddg.save.data", graphic.fext = 'jpeg', error=FALSE){
	# Determine type for value, and save accordingly
	if (.ddg.is.graphic(value)) .ddg.write.graphic(name, value, graphic.fext)
	else if (.ddg.is.simple(value)) .ddg.save.simple(name, value)
	else if (.ddg.is.csv(value)) .ddg.write.csv(name, value)
	else if (is.object(value)) .ddg.snapshot.node(name, "txt", value)
	else if (error) stop("Unable to create data (snapshot) node. Non-Object value to", fname, ".")
	else {
		error.msg <- paste("Unable to create data (snapshot) node. Non-Object value to", fname, ".")
		.ddg.insert.error.message(error.msg)
	}
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

.ddg.record.data <- function(dtype, dname, dvalue, dtime="", dloc="") {
	# If the table is full, make it bigger.
	ddg.dnum <- .ddg.dnum()
	ddg.data.nodes <- .ddg.data.nodes()
	if (nrow(ddg.data.nodes) < ddg.dnum) {
		size = 100
		new.rows <- data.frame(ddg.type = character(size),
          ddg.num = numeric(size),
          ddg.name = character(size),
          ddg.value = character(size),
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
	ddg.data.nodes$ddg.time[ddg.dnum] <- dtime
	ddg.data.nodes$ddg.loc[ddg.dnum] <- dloc
	ddg.data.nodes$ddg.current[ddg.dnum] <- TRUE
	.ddg.set("ddg.data.nodes", ddg.data.nodes)

	if (.ddg.debug()) print(paste("Adding data node", ddg.dnum, "named", dname))
}

# .ddg.proc.number gets the number of the nearest preceding matching 
# Operation, Checkpoint, or Restore node. It returns zero if no match 
# is found.

.ddg.proc.number <- function(pname) {
	ddg.proc.nodes <- .ddg.proc.nodes()
	rows <- nrow(ddg.proc.nodes)
	for (i in rows:1) {
		type <- ddg.proc.nodes$ddg.type[i]
		if ((type == "Operation" | type == "Checkpoint" | type == "Restore" | type == "Finish" ) & ddg.proc.nodes$ddg.name[i] == pname) {
			return(ddg.proc.nodes$ddg.num[i])
		}
	}

	# Error message if no match is found.
    error.msg <- paste("No procedure node found for", pname)
    .ddg.insert.error.message(error.msg)  
	return(0)
}

# .ddg.data.node.exists searches for a preceding current matching 
# data node. It returns TRUE if a match is found and FALSE otherwise.

.ddg.data.node.exists <- function(dname) {
	ddg.data.nodes <- .ddg.data.nodes()
	rows <- nrow(ddg.data.nodes)
	for (i in rows:1) {
		if (ddg.data.nodes$ddg.current[i]) {
			if (ddg.data.nodes$ddg.name[i] == dname) return (TRUE) 
		}
	}
	
	return(FALSE)
}

# .ddg.data.number retrieves the number of the nearest preceding 
# current matching data node. It returns zero if no match is found.

.ddg.data.number <- function(dname) {
	ddg.data.nodes <- .ddg.data.nodes()
	rows <- nrow(ddg.data.nodes)
	for (i in rows:1) {
		if (ddg.data.nodes$ddg.current[i]) {
			if (ddg.data.nodes$ddg.name[i] == dname) {
				return (ddg.data.nodes$ddg.num[i])	
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
}

# .ddg.data2proc creates a data flow edge from a data node to a 
# procedure node.
.ddg.data2proc <- function(dname, pname) {
	# Get data & procedure numbers.
	dn <- .ddg.data.number(dname)
	pn <- .ddg.proc.number(pname)

	# Create data flow edge from data node to procedure node.
	.ddg.append("DF d", dn, " p", pn, "\n", sep="")

	if (.ddg.debug()) {
		print(paste("data2proc: ", dname, " ", pname, sep=""))
		print(paste("DF d", dn, " p", pn, sep=""))
	}
}

# .ddg.proc2data creates a data flow edge from a procedure node to 
# a data node.
.ddg.proc2data <- function(pname, dname) {
	# Get data & procedure numbers.
	dn <- .ddg.data.number(dname)
	pn <- .ddg.proc.number(pname)

	# Create data flow edge from procedure node to data node.
	.ddg.append("DF p", pn, " d", dn, "\n", sep="")

	if (.ddg.debug()) {
		print(paste("proc2data: ", pname, " ", dname, sep=""))
		print(paste("DF p", pn, " d", dn, sep=""))
	}
}

# .ddg.lastproc2data creates a data flow edge from the last procedure 
# node to a data node.

.ddg.lastproc2data <- function(dname) {
	# Get data & procedure numbers.
	dn <- .ddg.data.number(dname)
	pn <- .ddg.pnum()
	
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
# used in an expression.

.ddg.find.var.uses <- function(obj) {
	# Base cases.
	if (is.name(obj)) return (deparse(obj))
	if (!is.recursive(obj)) return(character())
	if (.ddg.is.functiondecl(obj)) return(character())
	
	tryCatch(
		if (.ddg.is.assign(obj)) {
			# If assigning to a simple variable, recurse on the right hand 
      # side of the assignment.
			if (is.symbol(obj[[2]])) unique(unlist(.ddg.find.var.uses(obj[[3]])))
			
			# If assigning to an expression (like a[b]), recurse on the 
      # indexing part of the lvalue as well as on the expression.
			else unique(c (.ddg.find.var.uses(obj[[2]][[3]]), unlist(.ddg.find.var.uses(obj[[3]]))))
		} 
		
		# Not an assignment.  Recurse on all parts of the expression 
    # except the operator.
		else {
			unique(unlist(lapply(obj[2:length(obj)], .ddg.find.var.uses)))
		},
		error = function(e) {
			print (paste(".ddg.find.var.uses:  Error analyzing", deparse(obj)))
			character()
		}
	)
}

# .ddg.find.var.assignments creates a data frame containing 
# information about variable assignments:

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

.ddg.find.var.assignments <- function(parsed.commands) {
	if (length(parsed.commands) == 0) return (data.frame())
	
	# Make it big so we don't run out of space.
	var.table.size <- length(parsed.commands) * 5  
	vars.set <- data.frame(variable=character(var.table.size), 
			first.writer=numeric(var.table.size), 
			last.writer=numeric(var.table.size), 
			possible.first.writer=numeric(var.table.size), 
			possible.last.writer=numeric(var.table.size), stringsAsFactors=FALSE)
	vars.set$first.writer <- length(parsed.commands)+1
	vars.set$possible.first.writer <- length(parsed.commands)+1
  
	# Build the table recording where variables are assigned to or may 
  # be assigned to.
	var.num <- 1
	for (i in 1:length(parsed.commands)) {
		cmd.expr <- parsed.commands[[i]]
		
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
			
			# The variable was not in the table.  Add a new line for this 
      # variable.
			else {
				vars.set$variable[var.num] <- var
				if (!is.null(main.var.assigned) && var == main.var.assigned) {
					vars.set$first.writer[var.num] <- i
					vars.set$last.writer[var.num] <- i
				}
				else {
					vars.set$possible.first.writer[var.num] <- i
					vars.set$possible.last.writer[var.num] <- i
				}
				var.num <- var.num + 1
			}
		}
	}
	return (vars.set)
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
		if (.ddg.data.node.exists(var)) {
			nRow <- which(vars.set$variable == var)
			
			# Check if the node is written in the console block.
			if (length(nRow) > 0) {
				first.writer <- min(vars.set$first.writer[nRow], vars.set$possible.first.writer[nRow])						
				last.writer <- max(vars.set$last.writer[nRow], vars.set$possible.last.writer[nRow])
        
				# Draw the edge if we will connect to a node that exists 
        # before the console block or to the last writer of this 
        # variable within the console block.
				if (cmd.pos <= first.writer || cmd.pos > last.writer) {
					.ddg.data2proc(var, cmd)
				}

				# TODO - add some sort of warning to the user that the node is not being created
			}
			
			# The variable is not set at all in this console block.  Connect 
      # to a pre-existing data node.
			else {
				.ddg.data2proc(var, cmd)
			}
		}
	}
}

# .ddg.create.data.set.edges.for.console.cmd creates the nodes and 
# edges that correspond to a console command assigning to a variable. 
# A data node is created for the last write of a variable if that 
# occurs after the last possible writer. A snapshot node is created 
# if the value is a data frame.  Otherwise, a data node is created.

.ddg.create.data.set.edges.for.console.cmd <- function(vars.set, cmd, cmd.expr, cmd.pos, for.finish.node = FALSE) {
	vars.assigned <- .ddg.find.assign (cmd.expr)
	for (var in vars.assigned) {
		
		nRow <- which(vars.set$variable == var)
		
		# Only create a node edge for the last place that a variable is 
    # set within a console block.
		if ((length(nRow) > 0 && vars.set$last.writer[nRow] == cmd.pos && vars.set$possible.last.writer[nRow] <= vars.set$last.writer[nRow]) || for.finish.node) {
			val <- tryCatch(eval(parse(text=var), .GlobalEnv),
					error = function(e) {NULL}
			)
			tryCatch(.ddg.save.data(var,val,fname=".ddg.create.data.set.edges.for.console.cmd",error=TRUE),
			         error = function(e){.ddg.data.node("Data", var, "complex")})

			#if (!is.null(val)) {
			#	if (is.data.frame(val)) .ddg.snapshot.node(var, "csv", val)
			#	else .ddg.data.node("Data", var, val)
			#}
			#else {
			#	.ddg.data.node("Data", var, "complex")
			#}
			.ddg.proc2data(cmd, var)
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
				.ddg.data.node("Data", vars.set$variable[i], value)
				.ddg.proc2data(last.command, vars.set$variable[i])
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
# specified as the parementer.
# THE FOLLOWING APPLIES TO THE COMMENTED SECTION OF CODE
# It appends the information to this file. 

.ddg.savehistory <- function(hist.file) {

	# USED TO STORE ENTIRE HISTORY IN SEP. FILE
	# write history out to temporary file
	#ddg.grab.timestamp <- .ddg.get(".ddg.grab.timestamp.history")
	#ddg.tmp.history.file <- paste(hist.file,".tmp", sep="")

	savehistory(hist.file)

	# USED TO STORE ENTIRE HISTORY IN SEP. FILE
	# read in changes and writ eout to extended file
	#newlines <- .ddg.loadhistory(ddg.tmp.history.file,ddg.grab.timestamp)
	#write(newlines, file=hist.file, append=TRUE)
	# insert timestamp to history 
	#.ddg.write.timestamp.to.history(var=".ddg.grab.timestamp.history")
}

# .ddg.console.node creates a console node.
.ddg.console.node <- function() {
	# Load our extended history file and the last timestamp
	ddg.history.file <- .ddg.get("ddg.history.file")
	ddg.history.timestamp <- .ddg.get(".ddg.history.timestamp")

	# grab any new commands that might still be in history
	.ddg.savehistory(ddg.history.file)
	
	# load from extended history since last time we wrote out a console node
	new.lines <- .ddg.loadhistory(ddg.history.file,ddg.history.timestamp)
	
	# no new history since last timestamp
	if (length(new.lines) == 0) return (NULL)	

	# Add a procedure node for each new command, data nodes for 
  # variables set and used, and the corresponding edges.
	.ddg.last.command <- .ddg.get(".ddg.last.command")
	if (!is.null(.ddg.last.command)) {
		cmd.abbrev <- .ddg.abbrev.cmd(.ddg.last.command)
		if (.ddg.debug()) print(paste(".ddg.console.node:  Adding finish node for last command", cmd.abbrev))
		.ddg.proc.node("Finish", cmd.abbrev, .ddg.last.command, console=TRUE)
		.ddg.proc2proc()
		vars.set <- .ddg.find.var.assignments(.ddg.last.command)
		.ddg.create.data.set.edges.for.console.cmd(vars.set, cmd.abbrev, parse(text=.ddg.last.command), 0, for.finish.node = TRUE)
	}
	
	# Parse the new lines.
	parsed.commands <- parse(text=new.lines)
	
	# It is possible that a command may extend over multiple lines. 
  # new.commands will have one entry for each parsed command, whereas 
  # new.lines, which is read from the file, will have one entry for 
  # each line, which is not very useful for us.
	new.commands <- lapply(parsed.commands, function(cmd) {paste(deparse(cmd), collapse="")})
	
	# Create start and end nodes to allow collapsing of consecutive 
  # console nodes. Don't bother doing this if there is only 1 new 
  # line in the history.
	num.new.commands <- length(new.commands)
	if (num.new.commands > 1) {
	  if (.ddg.debug()) print(paste(".ddg.console.node:  Adding console start node"))
	  .ddg.proc.node("Start", "Console", "Console", console=TRUE)
		
		.ddg.proc2proc()
	}
	
	# Quote the quotation (") characters so that they will appear in 
  # ddg.txt.
	quoted.commands <- gsub("\\\"", "\\\\\"", new.commands)
	
	.ddg.last.command <- quoted.commands[[num.new.commands]]
	.ddg.set(".ddg.last.command", .ddg.last.command)
	if (substr(.ddg.last.command, 1, 4) == "ddg.") {
		.ddg.set(".ddg.last.command", NULL)
		.ddg.last.command <- NULL
	}
	else {
		quoted.commands <- quoted.commands[1:num.new.commands-1]
		parsed.commands <- parsed.commands[1:num.new.commands-1]
	}
	
	# We tried to use a data frame to contain new.commands, 
  # quoted.commands and parsed.commands, but it does not seem 
  # possible to put the parsed expressions in a data frame.
	
	# Create an operation node for each command.  We can't use lapply 
  # here because we need to process the commands in order and lapply 
  # does not guarantee an order.
	for (cmd in quoted.commands) {
		if (substr(cmd, 1, 4) != "ddg.") {
			cmd.abbrev <- .ddg.abbrev.cmd(cmd)
			last.proc.node <- cmd.abbrev
			if (.ddg.debug()) print(paste(".ddg.console.node: Adding operation node for", cmd.abbrev))
			.ddg.proc.node("Operation", cmd.abbrev, cmd, console=TRUE)
			.ddg.proc2proc()
		}
	}
	
	# Close the console block
	if (num.new.commands > 1) {
	  if (.ddg.debug()) print(paste(".ddg.console.node:  Adding finish console node"))
	  .ddg.proc.node("Finish", "Console", "Console", console=TRUE)
		.ddg.proc2proc()
	}
	
	if (!is.null(.ddg.last.command)) {
		cmd.abbrev <- .ddg.abbrev.cmd(.ddg.last.command)
		if (.ddg.debug()) print(paste(".ddg.console.node:  Adding start node for last command", cmd.abbrev))
		.ddg.proc.node("Start", cmd.abbrev, .ddg.last.command, console=TRUE)
		.ddg.proc2proc()
	}
	
	if (length(parsed.commands) > 0) {
		# Find where all the variables are assigned to.
		vars.set <- .ddg.find.var.assignments(parsed.commands)
	
		# Decide which data nodes and edges to create.  Only create a data 
  	# node for the last write of a variable and only if that occurs 
  	# after the last possible writer. Create an edge for a data use as 
  	# long as the use happens before the first writer/possible writer 
  	# or after the last writer/possible writer.
		for (i in 1:length(parsed.commands)) {
			cmd.expr <- parsed.commands[[i]]
			cmd <- quoted.commands[i]
		
			if (substr(cmd, 1, 4) != "ddg.") {
				cmd.abbrev <- .ddg.abbrev.cmd(cmd)
				.ddg.create.data.use.edges.for.console.cmd(vars.set, cmd.abbrev, cmd.expr, i)
				.ddg.create.data.set.edges.for.console.cmd(vars.set, cmd.abbrev, cmd.expr, i)
			}
		}
	
		# Create a data node for each variable that might have been set in 
  	# something other than a simple assignment, with an edge from the 
  	# last node in the console block.
		.ddg.create.data.node.for.possible.writes(vars.set, last.proc.node)
	}
	
    # Write time stamp to history.
	.ddg.write.timestamp.to.history()
}

# .ddg.proc.node creates a procedure node.
.ddg.proc.node <- function(ptype, pname, pvalue="", console=FALSE) {
	if (interactive() && !console && .ddg.enable.console()) .ddg.console.node()
  
	# Increment procedure counter.
	.ddg.inc("ddg.pnum")
	
	# Include value if available.
	proc.value <- 
		if (pvalue!="") paste(" Value=\"", pvalue, "\"", sep="")
		else ""

	# Create procedure node.  
	ddg.pnum <- .ddg.pnum()
  
	if (proc.value != "") {
        .ddg.append(ptype, " p", ddg.pnum, " \"", ddg.pnum, "-", pname, "\"", proc.value, ";\n", sep="")
	}
	else {
        .ddg.append(ptype, " p", ddg.pnum, " \"", ddg.pnum, "-", pname, "\"\n", sep="")
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

.ddg.data.node <- function(dtype, dname, dvalue) {
	# If object, try to create snapshot node.
	if (is.object(dvalue)) {
		tryCatch(
			{
			  .ddg.snapshot.node (dname, "txt", as.character(dvalue))
        return(NULL)
		    },
			error = function(e) {
				error.msg <- paste("Unable to create snapshot node for", dname)
        .ddg.insert.error.message(error.msg)
        .ddg.dec("ddg.dnum")
				return (.ddg.data.node (dtype, dname, "complex"))
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
		else if (dvalue == "complex") "complex"
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
	   # Create data node.
		data.value <- paste(" Value=\"", val, "\"", sep="")
	
		# Increment data counter.
		.ddg.inc("ddg.dnum")
			
		# Add node to DDG.
		ddg.dnum <- .ddg.dnum()
		.ddg.append(dtype, " d", ddg.dnum, " \"", ddg.dnum, "-", dname, "\"", data.value, ";\n", sep="")
	
		# Record data node information.
		.ddg.record.data(dtype, dname, dvalue)
	
		if (.ddg.debug()) print(paste("data.node:", dtype, dname))
	}
}

# .ddg.supported.graphic - the sole purpose of this function is to verify that 
# the input file extension is a supported graphic type. So far, the list of
# supported graphics inlude:
# jpg, jpeg, bmp, png, tiff
.ddg.supported.graphic <- function(ext){
	return(ext %in% c("jpeg", "jpg", "tiff", "png", "bmp"))
}

# .ddg.snapshot.node creates a data node of type Snapshot. Snapshots 
# are used for complex data values not written to file by the main 
# script. The contents of data are written to the file dname.fext 
# in the DDG directory. Snapshots are also used to capture output plots 
# and other graphics generated by the R script.
 
.ddg.snapshot.node <- function(dname, fext, data, save.object = FALSE) {
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
	else if (.ddg.supported.graphic(fext)){
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
      write(as.character(data), dpfile)
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

	# Create file node.
	.ddg.append("Snapshot", " d", ddg.dnum, " \"", ddg.dnum, "-", dname, "\" Value=\"", dpfile, "\" Time=\"", dtime, "\";\n", sep="")

	# Record data node information.
	.ddg.record.data("Snapshot", dname, dfile, dtime)

	if (.ddg.debug()) print(paste("snapshot.node: ", dname))
	return(dpfile)
}

# .ddg.file.node creates a node of tyoe File. File nodes are used for files
# written out either by capturing output from the script or by copying a file
# that is written by the script into the DDG directory. Returns the path 
# where the file referenced by the node should be stored.
.ddg.file.node <- function(dtype,fname,dname) {
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
	
	# Create file node.
	.ddg.append(dtype, " d", ddg.dnum, " \"", ddg.dnum, "-", dname, "\" Value=\"", dpfile.out, "\" Time=\"", dtime, "\"", loc.value, ";\n", sep="")

	# Record data node information.
	.ddg.record.data(dtype, dname, dfile, dtime, file.loc)

	return(dpfile.out)
}

# .ddg.file.copy creates a data node of type File. File nodes are 
# used for files written by the main script. A copy of the file is 
# written to the DDG directory.
 
.ddg.file.copy <- function(dtype, fname, dname) {
	# Calculate location of original file
	file.loc <- normalizePath(fname, winslash="/", mustWork = FALSE)

	# Copy file.
	if (file.exists(file.loc)) {
	   # Create file node in DDG
	   dpfile.out <- .ddg.file.node(dtype,fname,dname)

	   file.copy(file.loc, dpfile.out, overwrite=TRUE)
	}
	else {
    	error.msg <- paste("File to copy does not exist:", file.loc) 
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
  	.ddg.data.node("Exception", "error.msg", msg)
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

# .ddg.checkpoint.file.node creates a checkpoint file node.
.ddg.checkpoint.file.node <- function(fname, dname, checkpoint.name) {
	# Increment data counter.
	.ddg.inc("ddg.dnum")
	
	# Get checkpoint file location.
	file.name <- basename(fname)
	file.loc <- dirname(normalizePath(fname, winslash="/", mustWork = FALSE))

	# Add number to file name.
	ddg.dnum <- .ddg.dnum()
	dfile <- paste(ddg.dnum, "-", file.name, sep="")

	# Get path plus file name.
	dpfile.out <- paste(.ddg.path(), "/", dfile, sep="")
	
	if (.ddg.debug()) print(paste("Saving checkpoint in", dpfile.out))

	# Create checkpoint.
	save.image(dpfile.out)
	# toSave <- ls(envir=.GlobalEnv)
	# save (list = toSave, file = dpfile.out, envir = parent.frame(3))
	
	# Create the node.
	dtime <- .ddg.timestamp()
	.ddg.append("CheckpointFile", " d", ddg.dnum, " \"", ddg.dnum, "-", file.name, "\" Value=\"", dpfile.out, "\" Time=\"", dtime, "\";\n", sep="")

	# Record data node information.
	.ddg.record.data("Checkpoint", file.name, dfile, dtime, file.loc)

	if (.ddg.debug()) print(paste("file.copy: Checkpoint", fname))
	return (dpfile.out)
}

# .ddg.checkpoint.out creates a checkpoint node and data flow edge.
.ddg.checkpoint.out <- function(checkpoint.name, filename) {
	dname <- basename(filename)
	
	# Create output file node where name = filename.
	saved.file <- .ddg.checkpoint.file.node(filename, dname, checkpoint.name)
	
	# Create data flow edge from operation node to file node.
	.ddg.proc2data(checkpoint.name, dname)
	
	return (saved.file)
}

# .ddg.mark.stale.data updates the "current" attribute of the data 
# nodes. The current attribute is used to determine which value in 
# the data node table corresponds to the use of a data item. We want 
# the latest value before the checkpoint to be found.

# For data that was in the table when the checkpoint was made, the 
# current attribute should be the same as in the checkpointed table. 
# For data that was created after the checkpoint was made, the 
# current attribute should be FALSE.

# The entries for files are examined to determine which was the 
# current version of each file at the time the checkpoint was taken. 
# If that file no longer exists, or has been modified, the version 
# of the file current at the time of the checkpoint is restored.

.ddg.mark.stale.data <- function(saved.env) {
	ddg.files.to.restore <- matrix('', nrow=0, ncol=2, dimnames=list(NULL, c("filename", "original")))
	
	# Mark the data that was in the checkpointed table to be consistent 
  # with the checkpoint.
	ddg.dnum <- .ddg.dnum()
	ddg.data.nodes <- .ddg.data.nodes()
	ddg.saved.data.nodes <- saved.env[["ddg.data.nodes"]]
	for (i in 1:ddg.dnum) {
		ddg.saved.data.nodes$ddg.current[i] <- ddg.data.nodes$ddg.current[i]
		
		# Determine the most recent version of each file at the time the 
    # checkpoint was taken.
		if (ddg.saved.data.nodes$ddg.type[i] == "File") {
			nRow<-which(ddg.files.to.restore[,"original"] == ddg.saved.data.nodes$ddg.loc[i])
			if (length(nRow) > 0) {
				ddg.files.to.restore[nRow,"filename"] <- paste(ddg.saved.data.nodes$ddg.num[i], "-", ddg.saved.data.nodes$ddg.name[i], sep="")
			}
			else {
				newfile = c(paste(ddg.saved.data.nodes$ddg.num[i], "-", ddg.saved.data.nodes$ddg.name[i], sep=""), ddg.saved.data.nodes$ddg.loc[i])
				ddg.files.to.restore <- rbind(ddg.files.to.restore, newfile)
			}
		}
		
		else if (ddg.saved.data.nodes$ddg.type[i] == "Checkpoint") {
			ddg.saved.data.nodes$ddg.current[i] <- TRUE
		}
	}
	
	# Mark the data entries made after the checkpoint to be not current.
	ddg.saved.dnum <- saved.env[["ddg.dnum"]]
	if (ddg.dnum < ddg.saved.dnum) {
		for (i in (ddg.dnum+1):ddg.saved.dnum) {
			if (ddg.saved.data.nodes$ddg.type[i] != "Checkpoint") {
				ddg.saved.data.nodes$ddg.current[i] <- FALSE
			}
		}
	}
	saved.env[["ddg.data.nodes"]] <- ddg.saved.data.nodes
	
	# Restore files that have been modified or deleted since the 
  # checkpoint.
	num.files.to.restore <- nrow(ddg.files.to.restore)
	if (num.files.to.restore > 0) {
		# ddg.path <- .ddg.path()
		ddg.path <- saved.env[["ddg.path"]]
		for (i in 1:num.files.to.restore) {
			original <- ddg.files.to.restore[i, 2]
			saved <- paste(ddg.path, ddg.files.to.restore[i, 1], sep="/")
			
			# Original file is newer than the one to be restored from the 
      # DDG. Save the original file in a special place before 
      # restoring the DDG file.
			if (file.exists(original)) {
				modTime.original <- file.info(original)$mtime[1]
				modTime.saved <- file.info(saved)$mtime[1]
				if (modTime.original >= modTime.saved) {
					rescue.dir <- paste(getwd(), "/ddg.rescued.files/", sep="")
					dir.create(rescue.dir, showWarnings=FALSE)
					rescue.filename <- paste(rescue.dir, modTime.original, "-", basename(original), sep="")
					warning("Saving ", original, " in ", rescue.filename)
					file.copy(original, rescue.filename, overwrite=TRUE)
					file.copy(saved, original, overwrite=TRUE)
				}
			}
			
			# Original file no longer exists. Just copy in the DDG file.
			else {
				file.copy(saved, original)
			}
		}
	}
}

# .ddg.restore.ddg.state replaces the current informatin with the 
# saved DDG information.

.ddg.restore.ddg.state <- function(saved.env) {	
	
	saved.names <- ls (saved.env, all.names = TRUE)
	for (saved.name in saved.names) {
		saved.value <- saved.env[[saved.name]]
		.ddg.env[[saved.name]] <- saved.value
	}
}

# .ddg.record.checkpoint records the procedure node information for 
# a checkpoint.
.ddg.record.checkpoint <- function(filename, checkpoint.name) {
	ddg.checkpoint.num <- .ddg.get("ddg.checkpoint.num")
	ddg.checkpoints <- .ddg.get("ddg.checkpoints")
	ddg.checkpoints$filename[ddg.checkpoint.num] <- filename
	ddg.checkpoints$checkpoint.name[ddg.checkpoint.num] <- checkpoint.name
	.ddg.set("ddg.checkpoints", ddg.checkpoints)
}

# .ddg.lookup.checkpoint.name looks up the name of a checkpoint.
.ddg.lookup.checkpoint.name <- function(filename) {
	ddg.checkpoints <- .ddg.get("ddg.checkpoints")
	nRow<-which(ddg.checkpoints$filename == filename)
	return(ddg.checkpoints$checkpoint.name[nRow])
}

# .ddg.delete.temp deletes any temporary files created during the processing 
# of a script. These include:
#	1. The temporary history file
.ddg.delete.temp <- function() {
	# delet the temporary history file if we made it
	if (.ddg.is.set('ddg.history.file')) unlink(.ddg.get('ddg.history.file'))
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
	if (!.ddg.check.init()) return(NULL)

	.ddg.lookup.function.name(pname)
	.ddg.proc.node("Operation", pname, pname)
	
	# Create control flow edge from preceding procedure node.
	.ddg.proc2proc()

	# Create the input edges if ins list provided.
	if (!is.null(ins)) {
		lapply(ins, 
			function(param) {
				if (.ddg.data.node.exists(param)) {
					.ddg.data2proc(param, pname)
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
			})
	}
	# Look up input parameters from calling environment.
  else if (lookup.ins) {
		call <- sys.call(-1)
		tokens <- unlist(strsplit (as.character(call), "[(,)]"))
		
		# Get parameters and create edges.
		if (length(tokens) > 1) {
			args <- tokens[2:length(tokens)]
			lapply(args, 
					function(param) {	
						if (.ddg.data.node.exists(param)) {
							.ddg.data2proc(as.character(param), pname)
							if (.ddg.debug()) print(paste("param:", param))
						}
						else {
							error.msg <- paste("Skipping parameter", param)
              				.ddg.insert.error.message(error.msg)
						}
					})
		}
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
	  lapply(outs.exception,
	    function(param) {
	      # Get value in calling environment.
	      name <- param
	      value <- NULL
	      env <- parent.frame(3)
	      .ddg.lookup.value(name, value, env, "ddg.procedure", warn=FALSE)
 
        # Exception node.
        .ddg.data.node("Exception", name, value)
        .ddg.proc2data(pname, name)
	    }
	  )
  }
	
	# URL node.
	if (!is.null(outs.url)) {
	  lapply(outs.url,
      function(param) {
	      # Get value in calling environment.
	      name <- param
	      value <- NULL
	      env <- parent.frame(3)
	      .ddg.lookup.value(name, value, env, "ddg.procedure", warn=FALSE)

        # URL node.
        .ddg.data.node("URL", name, value)
	      .ddg.proc2data(pname, name)
      }
	  )
	}

	# Generalized data node (includes simple data values as well as snapshots)
	if (!is.null(outs.data)) {
	  lapply(outs.data,
			  function(param) {
				  # Get value in calling environment.
				  name <- param
				  value <- NULL
				  env <- parent.frame(3)
				  .ddg.lookup.value(name, value, env, "ddg.procedure", warn=FALSE)
				  
				  tryCatch({
							  if (!is.character(name)) name <- deparse(substitute(name))
							  .ddg.save.data(name,value,".ddg.procedure", error=TRUE)
							  .ddg.proc2data(pname, name)
						  }, error = function(e) {
							  .ddg.insert.error.message(e)
						  }
				  )  
			  }
	  )
	}
	
	# File node.
	if (!is.null(outs.file)) {
	  lapply(outs.file,
	    function(param) {
	      # Get value in calling environment.
	      name <- param
	      value <- NULL
	      env <- parent.frame(3)
	      .ddg.lookup.value(name, value, env, "ddg.procedure", warn=FALSE)

        if (value == "") {
          # Filename passed as value.          
          .ddg.file.copy("File", name, name)
          .ddg.proc2data(pname, name)
        }
        else {
          # Filename passed as name.
          .ddg.file.copy("File", value, name)
          .ddg.proc2data(pname, name)
        }
	    }
	  )
	}
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
	if (!.ddg.check.init()) return(NULL)

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
	if (!.ddg.check.init()) return(NULL)

	# Look up the value if one was not provided.
	env <- parent.frame()
	.ddg.lookup.value(dname, dvalue, env, "ddg.exception")
	
	# If dname is not a string, use its name rather than its value.
	if (!is.character(dname)) dname <- deparse(substitute(dname))

	# Create input exception node.
    .ddg.data.node("Exception", dname, dvalue)
}

# ddg.url creates a data node for a URL. dname - label for the node. 
# dvalue (optional) - the full URL.  If a value is not provided, the 
# argument passed in for dname is evaluated in the calling 
# environment to determine the value.

ddg.url <- function(dname, dvalue=NULL) {
	if (!.ddg.check.init()) return(NULL)

	# Look up the value if one was not provided.
	env <- parent.frame()
	.ddg.lookup.value(dname, dvalue, env, "ddg.url")
	
	# If dname is not a string, use its name rather than its value.
	if (!is.character(dname)) dname <- deparse(substitute(dname))
	
	# Create input URL node.
	.ddg.data.node("URL", dname, dvalue)
}

# ddg.file creates a data node of type File by copying an existing 
# file to the DDG directory. filename - name of the file to copy, 
# including path to the file if it is not in the working directory. 
# dname (optional) - label for the node.  If omitted, the filename, 
# minus the directory path, is used as the label.

ddg.file <- function(filename, dname=NULL) {
	if (!.ddg.check.init()) return(NULL)

	.ddg.file.copy("File", filename, dname)
}

# ddg.data.in creates a data flow edge from data node dname to 
# procedure node pname. pname (optional) - the name of the procedure 
# that created this data value.  This can be passed as a string or as 
# a name.  If omitted, the name of the function that called 
# ddg.data.in is used. dname - the label for the data node.  This 
# can be passed as a string, name, or expression.

ddg.data.in <- function(dname, pname=NULL) {
	if (!.ddg.check.init()) return(NULL)

	.ddg.lookup.function.name(pname)
	
	arg <- substitute(dname)
	if (!is.character(arg) && .ddg.data.node.exists(arg)) dname <- deparse(arg)
	
	# Create data flow edge from data node to operation node.
	.ddg.data2proc(dname, pname)
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
	if (!.ddg.check.init()) return(NULL)

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
	if (!.ddg.check.init()) return(NULL)

	# If no value is provided, get value in calling environment.
	env <- parent.frame()
	.ddg.lookup.value(dname, dvalue, env, "ddg.exception.out")
	
	# Create output exception node.
	.ddg.data.node("Exception", dname, dvalue)
	
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
	if (!.ddg.check.init()) return(NULL)

	# If no value is provided, get value in calling environment.
	env <- parent.frame()
	.ddg.lookup.value(dname, dvalue, env, "ddg.url.out")
	
	# Create output URL node where dvalue = address.
	.ddg.data.node("URL", dname, dvalue)

	.ddg.lookup.function.name(pname)
	
	# Create data flow edge from operation node to URL node.
	.ddg.proc2data(pname, dname)
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
	if (!.ddg.check.init()) return(NULL)

	if (is.null(dname)) dname <- basename(filename)
	
	# Create output file node called filename and copy file.
	saved.file <- .ddg.file.copy("File", filename, dname)
	
	.ddg.lookup.function.name(pname)
	
	# Create data flow edge from operation node to file node.
	.ddg.proc2data(pname, dname)
	
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
	if(!.ddg.check.init()) return

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
	if (!.ddg.check.init()) return(NULL)

	.ddg.lookup.function.name(pname)
  
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
	if (!.ddg.check.init()) return(NULL)

	.ddg.lookup.function.name(pname)
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
	if (!.ddg.check.init()) return(NULL)

	# only act if in intereactive mode and with enabled console
	if (interactive() && .ddg.enable.console()) {
		.ddg.console.node()
	}
}

# ddg.checkpoint saves the current R state in a file and adds a 
# checkpoint node to the DDG. It creates a procedural node labeled 
# with the checkpoint name and a snapshot node labeled with the name 
# of the checkpoint file created. It returns the full path to the 
# file that the checkpoint is saved in. checkpoint.name (optional) - 
# the value associated with the checkpoint procedure node.

ddg.checkpoint <- function(checkpoint.name=NULL) {
	if (!.ddg.check.init()) return(NULL)

	if (.ddg.debug()) print("Creating checkpoint")
	ddg.checkpoint.num <- .ddg.get("ddg.checkpoint.num")
	file.name <- paste(ddg.checkpoint.num, ".RData", sep="")
	file.path <- paste(.ddg.path(), "/", file.name, sep="")
	if (is.null(checkpoint.name)) checkpoint.name <- file.name
	.ddg.proc.node("Checkpoint", checkpoint.name)	

	# Create control flow edge from preceding procedure node.
	.ddg.proc2proc()
	checkpoint.file <- .ddg.checkpoint.out(checkpoint.name, file.path)
	.ddg.set("ddg.checkpoint.num", (ddg.checkpoint.num + 1) %% ddg.MAX_CHECKPOINTS)
	.ddg.record.checkpoint(file.name, checkpoint.name)
	return (checkpoint.file)
}

# ddg.restore restores the state saved in a checkpoint file. It 
# creates a procedure node in the DDG labeled ddg.restore and a link 
# from the file node representing the checkpointed file to the 
# restore node.  The DDG tables are updated so the DDG will be 
# extended to include the actions between the checkpoint and restore 
# but the data flow edges will link to the data that existed when 
# the checkpoint was made. file.path - the name of the checkpoint 
# file to restore.

ddg.restore <- function(file.path) {
	if (!.ddg.check.init()) return(NULL)

	# Remove the directories.
	file.name <- basename(file.path)

	# Remove the node number.
	file.name.start <- regexpr("-", file.name) + 1
	file.name.end <- nchar(file.name)
	file.name <- substr(file.name, file.name.start, file.name.end)
	checkpoint.name <- .ddg.lookup.checkpoint.name(file.name)
	.ddg.proc.node("Restore", checkpoint.name)	

	# Create control flow edge from preceding procedure node.
	.ddg.proc2proc()
	ddg.data.in(file.name, checkpoint.name)
	
	if (.ddg.debug()) print(paste("Restoring from", file.path))
	
	# Update the ddg tables so that the ddg will get extended to include 
  # the actions between the checkpoint and restore, but the data edges 
  # will link to the data that existed at the time the checkpoint was 
  # made.

	saved.ddg.env <- .ddg.env
	load (file.path, parent.env(environment()))
	# load (file.path, .GlobalEnv, verbose=TRUE)
	.ddg.mark.stale.data(saved.ddg.env)
	.ddg.restore.ddg.state(saved.ddg.env)
}

# ddg.init intializes a new ddg. r.script.path (optional) - the full 
# path to the R script file.  If provided, a copy of the script will 
# be saved with the DDG. ddgdir (optional) - the directory where the 
# DDG should be saved. If not provided, the DDG will be saved in a 
# subdirectory called "ddg" in the current working directory.

ddg.init <- function(r.script.path = NULL, ddgdir = NULL, enable.console = FALSE) {
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
	.ddg.set(".ddg.initilized", TRUE)
	
	if (interactive() && .ddg.enable.console()) {
		ddg.history.file <- paste(.ddg.path(), ".ddghistory", sep="/")
		.ddg.set("ddg.history.file", ddg.history.file)
		
		# Empty file if it already exists, do the same with tmp file
    file.create(ddg.history.file)
		
		# one timestamp keeps track of last ddg.save (the default)
 		.ddg.write.timestamp.to.history()

		# save the history
		savehistory(ddg.history.file)
		history <- readLines(ddg.history.file)
	}
}

# ddg.run executes the function fname which contains the main 
# program. If an R error is generated, the error message is captured 
# and saved in the DDG. Note: this function includes calls to 
# ddg.init() and ddg.save(), so it is not necessary to call those 
# functions from an instrumented script if ddg.run is used. f - the 
# function to run. r.script.path (optional) - the full path to the 
# R script file.  If provided, a copy of the script will be saved 
# with the DDG. ddgdir (optional) - the directory where the DDG 
# should be saved.  If not provided, the DDG will be saved in a 
# subdirectory called "ddg" in the current working directory.

ddg.run <- function(f, r.script.path = NULL, ddgdir = NULL, enable.console = FALSE) {
    ddg.init(r.script.path, ddgdir, enable.console)
	
    # If an R error is generated, get the error message and close 
    # the DDG.
    tryCatch(
		f(),
		error=function(e) {
			e.str <- toString(e)
			print(e.str)
			ddg.procedure(pname="tryCatch")
			ddg.exception.out("error.msg", e.str, "tryCatch")
		},
		finally={ddg.save()}
	)
}

# ddg.save inserts attribute information and the number of procedure 
# steps at the top of the DDG. It writes the DDG, the procedure nodes 
# table, and the data nodes table to the DDG directory.

ddg.save <- function() {
	if (!.ddg.check.init()) return(NULL)

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

	# delete temporary files
	.ddg.delete.temp()
}

# ddg.debug.on turns on debugging of DDG construction.
ddg.debug.on <- function () {
	if (!.ddg.check.init()) return(NULL)

	.ddg.set("ddg.debug", TRUE)
}

# ddg.debug.off turns off debugging of DDG construction.
ddg.debug.off <- function () {
	if (!.ddg.check.init()) return(NULL)

	.ddg.set("ddg.debug", FALSE)
}

# ddg.console.off turns off the console mode of DDG construction
ddg.console.off <- function() {
	if (!.ddg.check.init()) return(NULL)

	# capture history if console was on up to this point
	if (interactive() && .ddg.enable.console()) {
		.ddg.console.node()

		# set the console to off
		.ddg.set(".ddg.enable.console", FALSE)
	}
}

# ddg.console.on turns on the console mode of DDG construction
ddg.console.on <- function() {
	if (!.ddg.check.init()) return(NULL)

	# write a new timestamp if we're turning on the console so we only capture
	# history from this point forward
	if (!.ddg.enable.console()) {
		.ddg.write.timestamp.to.history()

		.ddg.set(".ddg.enable.console", TRUE)
	}
}
 
# ddg.flush.ddg removes selected files from the DDG directory.
ddg.flush.ddg <- function () {
	if (!.ddg.check.init()) return(NULL)

	ddg.path <- .ddg.path()
	# Do not remove files unless ddg.path exists and is different 
  # from the working directory.
  	if (file.exists(ddg.path) && ddg.path != getwd()) {
    	unlink(paste(ddg.path, "ddg.txt", sep="/"))
    	unlink(paste(ddg.path, "dnodes.txt", sep="/"))
    	unlink(paste(ddg.path, "pnodes.txt", sep="/"))
    	unlink(paste(ddg.path, ".ddghistory", sep="/"))
    	unlink(paste(ddg.path,"[1-9]-*.*", sep="/"))
    	unlink(paste(ddg.path,"[1-9][0-9]-*.*", sep="/"))
    	unlink(paste(ddg.path,"[1-9][0-9][0-9]-*.*", sep="/"))
    	unlink(paste(ddg.path,"[1-9][0-9][0-9][0-9]-*.*", sep="/"))
    	unlink(paste(ddg.path,"[1-9][0-9][0-9][0-9][0-9]-*.*", sep="/"))
    	unlink(paste(ddg.path,"[1-9][0-9][0-9][0-9][0-9][0-9]-*.*", sep="/"))
  	}
}

