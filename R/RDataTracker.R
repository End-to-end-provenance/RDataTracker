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

.ddg.get <- function(var) {
	if (!exists(var, envir=.ddg.env)) {
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

.ddg.write.timestamp.to.history <- function() {
	if (Sys.getenv("RSTUDIO") != "" && Sys.info()['sysname'] == "Windows") {
		.ddg.set(".ddg.history.timestamp", paste("##------ ", date(), " ------##", sep=""))
		timestamp(quiet=TRUE)
	}
	else {
		.ddg.set(".ddg.history.timestamp", timestamp(prefix = "##-ddg-- ", quiet=TRUE))
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
		if ((type == "Operation" | type == "Checkpoint" | type == "Restore") & ddg.proc.nodes$ddg.name[i] == pname) {
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
	if (is.symbol(expr)) return (FALSE)
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
		if (.ddg.is.functiondecl(obj[[3]])) var
		else c(var, unlist(lapply(obj[[3]], .ddg.find.assign)))
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
# - the position of the statement that the variable last.
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

.ddg.create.data.set.edges.for.console.cmd <- function(vars.set, cmd, cmd.expr, cmd.pos) {
	vars.assigned <- .ddg.find.assign (cmd.expr)
	for (var in vars.assigned) {
		
		nRow <- which(vars.set$variable == var)
		
		# Only create a node edge for the last place that a variable is 
    # set within a console block.
		if (length(nRow) > 0 && vars.set$last.writer[nRow] == cmd.pos && vars.set$possible.last.writer[nRow] <= vars.set$last.writer[nRow]) {
			val <- tryCatch(eval(parse(text=var), .GlobalEnv),
					error = function(e) {NULL}
			)
			if (!is.null(val)) {
				if (is.data.frame(val)) .ddg.snapshot.node(var, "csv", val)
				else .ddg.data.node("Data", var, val)
			}
			else {
				.ddg.data.node("Data", var, "complex")
			}
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

# .ddg.console.node creates a console node.
.ddg.console.node <- function() {
	# Find the new console commands since the last console node was 
  # created.
	ddg.history.file <- .ddg.get("ddg.history.file")
	savehistory(ddg.history.file)
	
	# Add a procedure node for each new command, data nodes for 
  # variables set and used, and the corresponding edges.
	history <- readLines(ddg.history.file)
	history.lines <- length(history)
	
	# Find the last timestamp in the history so we know.  There may be 
  # more than one with the same timestamp, so pick the last of these.
	history.timestamp.line <- tail(which(history == .ddg.get(".ddg.history.timestamp")), 1)
	
	if (length(history.timestamp.line) == 0) {
		error.msg <- "Part of history is missing. DDG may be incomplete!"
    .ddg.insert.error.message(error.msg)
		history.timestamp.line <- 0
	}
	
	if (history.timestamp.line == length(history)) return (NULL)
		
	.ddg.last.command <- .ddg.get(".ddg.last.command")
	if (!is.null(.ddg.last.command)) {
		cmd.abbrev <- .ddg.abbrev.cmd(.ddg.last.command)
		.ddg.proc.node("Finish", cmd.abbrev, .ddg.last.command, console=TRUE)
		.ddg.proc2proc()
	}

	new.lines <- history[(history.timestamp.line + 1):history.lines]
	
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
			# print(paste("Adding operation node for", cmd.abbrev))
			.ddg.proc.node("Operation", cmd.abbrev, cmd, console=TRUE)
			.ddg.proc2proc()
		}
	}
	
	# Close the console block
	if (num.new.commands > 1) {
		.ddg.proc.node("Finish", "Console", "Console", console=TRUE)
		.ddg.proc2proc()
	}
	
	if (!is.null(.ddg.last.command)) {
		cmd.abbrev <- .ddg.abbrev.cmd(.ddg.last.command)
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
	
# .ddg.data.node creates a data node of type Data. Data nodes are 
# used for single data values. The value (dvalue) is stored in the 
# DDG.

.ddg.data.node <- function(dtype, dname, dvalue) {
	# If object, try to create snapshot node.
	if (is.object(dvalue)) {
		tryCatch(
			{
        .ddg.snapshot.node (dname, "txt", toString(dvalue))
				return(NULL)
		    },
			error = function(e) {
				error.msg <- paste("Unable to create snapshot node for", dname)
        .ddg.insert.error.message(error.msg)
				.ddg.set("ddg.dnum", .ddg.dnum() - 1)
				return (.ddg.data.node (dtype, dname, "complex"))
    	}
		)

	}
	
	# Convert value to a string.
	val <-
		if (is.list(dvalue)) {
			tryCatch(
				{
					values <- .ddg.replace.quotes(lapply(dvalue, as.character))
					positions <- 1:length(values)
					paste("[[", positions, "]]", values, collapse="\n")
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
		else if (is.na(dvalue)) "NA"
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

# .ddg.snapshot.node creates a data node of type Snapshot. Snapshots 
# are used for complex data values not written to file by the main 
# script. The contents of data are written to the file dname.fext 
# in the DDG directory.
 
.ddg.snapshot.node <- function(dname, fext, data) {
	# Increment data counter.
	.ddg.inc("ddg.dnum")
	
  # Get file name.
	ddg.dnum <- .ddg.dnum()
	dfile <- 
		if (fext == "" || is.null(fext)) paste(ddg.dnum, "-", dname, sep="")
		else                             paste(ddg.dnum, "-", dname, ".", fext, sep="")

	# Get path plus file name.
	ddg.path <- .ddg.path()
	dpfile <- paste(ddg.path, "/", dfile, sep="")
	
	# Write to file .
	if (fext == "csv") write.csv(data, dpfile, row.names=FALSE)
	else if (fext == "jpeg" || fext == "jpg") jpeg(filename=dpfile, width=800, height=500, quality=100)
	else if (fext == "pdf") dev.copy2pdf(file=dpfile)
    else if (fext == "txt" || fext == "") {
	    fileConn <- file(dpfile)
	    write(as.character(data), fileConn)
	    close(fileConn)
	}
	else if (fext == "RData") file.rename(paste(ddg.path, "/", dname, sep=""), dpfile)
	else {
    	error.msg <- paste("File extension", fext, "not recognized")
    	.ddg.insert.error.message(error.msg)
    	return(NULL)
	}

	dtime <- .ddg.timestamp()

	# Create file node.
	.ddg.append("Snapshot", " d", ddg.dnum, " \"", ddg.dnum, "-", dname, "\" Value=\"", dpfile, "\" Time=\"", dtime, "\";\n", sep="")

	# Record data node information.
	.ddg.record.data("Snapshot", dname, dfile, dtime)

	if (.ddg.debug()) print(paste("snapshot.node: ", dname))
	return(dpfile)
}

# .ddg.file.copy creates a data node of type File. File nodes are 
# used for files written by the main script. A copy of the file is 
# written to the DDG directory.
 
.ddg.file.copy <- function(dtype, fname, dname) {
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
	
	# Copy file.
	if (file.exists(file.loc)) file.copy(file.loc, dpfile.out, overwrite=TRUE)
	else {
    	error.msg <- paste("File to copy does not exist:", file.loc) 
    	.ddg.insert.error.message(error.msg)
    	return(NULL)
	}

	dtime <- .ddg.timestamp()

	# Set the node label.
	if (is.null(dname)) dname <- file.name
	
	# Create file node.
	.ddg.append(dtype, " d", ddg.dnum, " \"", ddg.dnum, "-", dname, "\" Value=\"", dpfile.out, "\" Time=\"", dtime, "\"", loc.value, ";\n", sep="")

	# Record data node information.
	.ddg.record.data(dtype, dname, dfile, dtime, file.loc)

	if (.ddg.debug()) print(paste("file.copy: ", dtype, " ", fname))
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

# outs (optional) - the names of data nodes that should be created 
# and linked as outputs from this procedure.  These MUST be passed 
# as strings, not names, unless the value is a file name. Supported 
# file extensions include: .csv, .jpg, .jpeg, .pdf, and .txt. If 
# the value ends in .url, a URL node is created.

# lookupIns (optional) - if true and ins is NULL, data edges will be 
# created to the arguments of the function that called ddg.procedure, 
# if the corresponding data nodes exist.

ddg.procedure <- function(pname=NULL, ins=NULL, lookup.ins=FALSE, outs.data=NULL, outs.exception=NULL, outs.url=NULL, outs.snapshot=NULL, outs.file=NULL) {
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
	
	# Create output nodes and edges if outs list provided.

  # Data node.
  if (!is.null(outs.data)) {
    lapply(outs.data,
      function(param) {
        # Get value in calling environment.
        name <- param
        value <- NULL
        env <- parent.frame(3)
        .ddg.lookup.value(name, value, env, "ddg.procedure", warn=FALSE)

        if (length(value) == 1 && is.vector(value) && is.na(value)) {
          # Data node with value NA.
          .ddg.data.node("Data", name, NA)            
          .ddg.proc2data(pname, name)
        }
        else if (length(value) == 1) {
          # Data node.
          .ddg.data.node("Data", name, value)
          .ddg.proc2data(pname, name)
        }
        else {
          # Not a simple value.
          error.msg = paste(name,"is not a simple value")
          .ddg.insert.error.message(error.msg)
        }
      }
    )  
  }    
  
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
	
	# Snapshot node.
	if (!is.null(outs.snapshot)) {
	  lapply(outs.snapshot,
	    function(param) {
	      # Get value in calling environment.
	      name <- param
	      value <- NULL
	      env <- parent.frame(3)
	      .ddg.lookup.value(name, value, env, "ddg.procedure", warn=FALSE)
 
	      if (is.vector(value) || is.matrix(value) || is.data.frame(value)) {
	        # Vector, matrix, or data frame.
	        .ddg.snapshot.node(name, "csv", value)
	        .ddg.proc2data(pname, name)
	      }
	      else if (length(value) == 1 && is.object(value)) {
	         # Class.
	         .ddg.snapshot.node(name, "txt", value)
	         .ddg.proc2data(pname, name)
	      }
	      else {
          # Unable to create Snapshot node.
          error.msg <- "Unable to create Snapshot node"
          .ddg.insert.error.message(error.msg)
	      }  
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

# ddg.data creates a data node for a single data value. dname - label 
# for the node.  This can be passed as a string, name, or expression. 
# dvalue (optional) - the value of the node.  If the value is 
# omitted, the argument passed in for dname is evaluated in the 
# calling environment to determine the value.

ddg.data <- function(dname, dvalue=NULL) {
	
	# Look up the value if one was not provided.
	env <- parent.frame()
	.ddg.lookup.value(dname, dvalue, env, "ddg.data")
	
	# If dname is not a string, use its name rather than its value.
	if (!is.character(dname)) dname <- deparse(substitute(dname))

	# Create input data node.
	.ddg.data.node("Data", dname, dvalue)
}

# ddg.exception creates a data node for an exception. dname - label 
# for the node.  This can be passed as a string, name, or expression. 
# dvalue (optional) - the value of the node.  If the value is 
# omitted, the argument passed in for dname is evaluated in the 
# calling environment to determine the value.

ddg.exception <- function(dname, dvalue=NULL) {
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
	# Look up the value if one was not provided.
	env <- parent.frame()
	.ddg.lookup.value(dname, dvalue, env, "ddg.url")
	
	# If dname is not a string, use its name rather than its value.
	if (!is.character(dname)) dname <- deparse(substitute(dname))
	
	# Create input URL node.
	.ddg.data.node("URL", dname, dvalue)
}

# ddg.snapshot creates a data node of type Snapshot. The contents 
# of data (or, if data is missing, the contents of dname in the 
# calling environment) are written to file on the DDG directory. The 
# name of the file is dname.fext. If fext is missing, the file 
# extension is assumed to be "csv".

ddg.snapshot <- function(dname, fext="csv", data=NULL) {
	# If data is not provided, get value of dname in the calling 
  # environment.
	env <- parent.frame()
	.ddg.lookup.value(dname, data, env, "ddg.snapshot")

	# Convert dname to string if necessary.
	if (!is.character(dname)) dname <- deparse(substitute(dname))  

	# Create snapshot node.
	.ddg.snapshot.node(dname, fext, data)
}

# ddg.file creates a data node of type File by copying an existing 
# file to the DDG directory. filename - name of the file to copy, 
# including path to the file if it is not in the working directory. 
# dname (optional) - label for the node.  If omitted, the filename, 
# minus the directory path, is used as the label.

ddg.file <- function(filename, dname=NULL) {
	.ddg.file.copy("File", filename, dname)
}

# ddg.data.in creates a data flow edge from data node dname to 
# procedure node pname. pname (optional) - the name of the procedure 
# that created this data value.  This can be passed as a string or as 
# a name.  If omitted, the name of the function that called 
# ddg.data.in is used. dname - the label for the data node.  This 
# can be passed as a string, name, or expression.

ddg.data.in <- function(dname, pname=NULL) {
	.ddg.lookup.function.name(pname)
	
	arg <- substitute(dname)
	if (!is.character(arg) && .ddg.data.node.exists(arg)) dname <- deparse(arg)
	
	# Create data flow edge from data node to operation node.
	.ddg.data2proc(dname, pname)
}

# ddg.data.out creates a data node of type "Data" called dname with 
# value dvalue. It also creates a data flow edge from procedure node 
# pname to data node dname. Use for simple data values. dname - the 
# label for the data node being created.  This can be passed as a 
# string, name, or expression. dvalue (optional) - the value to 
# associate with the node.  If no value is given, the argument passed 
# in for dname is evaluated in the calling environment. pname 
# (optional) - the name of the procedure that created this data 
# value.  This can be passed as a string or as a name.  If omitted, 
# the name of the function that called ddg.data.out is used.

ddg.data.out <- function(dname, dvalue=NULL, pname=NULL) {
	# If no value is provided, get value in calling environment.
	env <- parent.frame()
	.ddg.lookup.value(dname, dvalue, env, "ddg.data.out")
	
	# Create output data node.
	.ddg.data.node("Data", dname, dvalue)
	
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
	# If no value is provided, get value in calling environment.
	env <- parent.frame()
	.ddg.lookup.value(dname, dvalue, env, "ddg.url.out")
	
	# Create output URL node where dvalue = address.
	.ddg.data.node("URL", dname, dvalue)

	.ddg.lookup.function.name(pname)
	
	# Create data flow edge from operation node to URL node.
	.ddg.proc2data(pname, dname)
}

# ddg.snapshot.out creates a data node of type Snapshot called dname. 
# It also creates a data flow edge from procedure node pname to data 
# node dname. The contents of data are written to the file dname 
# (with numerical prefix) on the DDG directory. Use for complex data 
# values stored in memory. dname - the label for the data node being 
# created. fext (optional) - the file extension for the file 
# (dname.fext) that is created. fext should be a string. csv is used 
# if no value is provided. data (optional) - the data to write to 
# the file.  If omitted, dname will be evaluated in the calling 
# environment to get the data to be saved to file. pname (optional) 
# - the name of the procedure that created this node.  This can be 
# passed as a string or as a name.  If omitted, the name of the 
# function that called ddg.snapshot.out is used.

ddg.snapshot.out <- function(dname, fext="csv", data=NULL, pname=NULL) {
  # If data is not provided, get value of dname in calling 
  # environment.
	env <- parent.frame()
	.ddg.lookup.value(dname, data, env, "ddg.snapshot.out")
	
    # Convert dname to string if necessary.
    if (!is.character(dname)) dname <- deparse(substitute(dname))  
    
	# Create snapshot node.
	.ddg.snapshot.node(dname, fext, data)
	
	.ddg.lookup.function.name(pname)

	# Create data flow edge from operation node to snapshot node.
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
	if (is.null(dname)) dname <- basename(filename)
	
	# Create output file node called filename and copy file.
	saved.file <- .ddg.file.copy("File", filename, dname)
	
	.ddg.lookup.function.name(pname)
	
	# Create data flow edge from operation node to file node.
	.ddg.proc2data(pname, dname)
	
	return (saved.file)
}

# ddg.start creates a procedure node of type Start called pname. 
# pname (optional) - the label for the node.  This can be passed as 
# a string or as a name.  If omitted, the name of the function that 
# called ddg.start is used. Users can right-click on a start node 
# in DDG Explorer and see the code between start and finish nodes 
# in the original script.

ddg.start <- function(pname=NULL) {
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
	.ddg.lookup.function.name(pname)
	
	# Create finish non-operational step.
	.ddg.proc.node("Finish", pname, pname)

	# Create control flow edge from preceding procedure node.
	.ddg.proc2proc()
}

# ddg.checkpoint saves the current R state in a file and adds a 
# checkpoint node to the DDG. It creates a procedural node labeled 
# with the checkpoint name and a snapshot node labeled with the name 
# of the checkpoint file created. It returns the full path to the 
# file that the checkpoint is saved in. checkpoint.name (optional) - 
# the value associated with the checkpoint procedure node.

ddg.checkpoint <- function(checkpoint.name=NULL) {
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
	print("Set saved.ddg.env")
	load (file.path, parent.env(environment()))
	print("Loaded checkpoint file")
	# load (file.path, .GlobalEnv, verbose=TRUE)
	.ddg.mark.stale.data(saved.ddg.env)
	print("Marked stale data")
	.ddg.restore.ddg.state(saved.ddg.env)
	print("Restored ddg state")
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
	
	.ddg.set(".ddg.enable.console", enable.console)
	if (interactive() && .ddg.enable.console()) {
		ddg.history.file <- paste(.ddg.path(), ".ddghistory", sep="/")
		.ddg.set("ddg.history.file", ddg.history.file)
		# Empty file if it already exists.
    	file.create(ddg.history.file)
		
		.ddg.write.timestamp.to.history()
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
	if (interactive() && .ddg.enable.console()) {
		.ddg.console.node()
		# Sys.setenv("R_HISTSIZE"=.ddg.original.hist.size)
	}
	
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

	
}

# ddg.debug.on turns on debugging of DDG construction.
ddg.debug.on <- function () {
	.ddg.set("ddg.debug", TRUE)
}

# ddg.debug.off turns off debugging of DDG construction.
ddg.debug.off <- function () {
	.ddg.set("ddg.debug", FALSE)
}

# ddg.flush.ddg removes selected files from the DDG directory.
ddg.flush.ddg <- function () {
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

