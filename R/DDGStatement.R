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

############################ DDGStatement.R #############################

# This file contains definitions of S4 classes to manage information about
# individual R statements and functions that operate on individual statements
#
# All of these functions are internal to the RDataTracker library and
# not called from user code.
#
# Author: blerner
# August 2016
#
#########################################################################

# Needed to work with S4 classes.  Normally, this library is automatically
# loaded.  However, it is not loaded when running non-interactively, as
# in our test cases or if a user uses RScript to run R files.
library (methods)

# Information about where in the source code this statement appears.
setClass("DDGStatementPos",
	slots = list(
		startLine = "numeric",
		startCol = "numeric",
		endLine = "numeric",
		endCol = "numeric"
	)
)

# This is called automatically when there is a call to create a new
# DDGStatementPos object.
setMethod ("initialize",
	"DDGStatementPos",
	function(.Object, parseData){
		# If the parse data is missing, we set all the fields to -1
		if (length(parseData) == 1 && is.na(parseData)) {
			.Object@startLine <- -1
			.Object@startCol <- -1
			.Object@endLine <- -1
			.Object@endCol<- -1
		}
		
		# If we have parseData, we extract the information into oure
		# object.
		else {
			.Object@startLine <- parseData$line1
			.Object@startCol <- parseData$col1
			.Object@endLine <- parseData$line2
			.Object@endCol<- parseData$col2
		}
		
		#print(.Object)
		return (.Object)
	}
)

# This class contains all the information that we need when building a ddg.
# We create this when we parse the statement so that it is only done once
# and then look up the information we need when the statement executes.
setClass("DDGStatement",
	slots = list(
		text = "character",		# The original text in the file
		parsed = "expression",	# The parse tree for the statement
		abbrev = "character",	# A shortened version of the text to use in node names
		annotated = "expression",	# An annotated version of the statement.  This is
									# what we actually execute.
		
		# Note that vars.used through has.dev.off do not apply to a situation where
		# the statement is a function declaration, since declaring the statement
		# does not read from files, etc.  That happens when the function is called,
		# at which point we will refer to the information in the contained statements.
		
		vars.used = "character",	# A list of the variables that are used in the statement
		vars.set = "character",		# If this is an assignment statement, this is the variable assigned
		vars.possibly.set = "character",	# If this contains any internal assignment statements,
											# like an if-statement might, for example, these are the
											# variables assigned within the statement.
		isDdgFunc = "logical",	# True if this is a call to a ddg function
		readsFile = "logical",	# True if this statement contains a call to a function that
								# reads from a file
		writesFile = "logical",	# True if this statement contains a call to a function that
								# writes to a file
		createsGraphics = "logical",	# True if this is a function that creates a graphics
										# object, like a call to pdf, for example
		updatesGraphics = "logical",	# True if this is a function that updates a graphics
										# object, like a call to a function in the graphics package, for example
		has.dev.off = "logical",	# True if this statement contains a call to dev.off
		pos = "DDGStatementPos",	# The location of this statement in the source code.
									# Has the value null.pos() if it is not available.
		script.num = "numeric",	# The number for the script this statement comes from.
								# Has the value -1 if it is not available
		is.breakpoint = "logical",	# True if a breakpoint has been set for this statement
		contained = "list",			# If this is a function declaration, this will be a list of
									# DDGStatement objects for the statements it contains.
		functions.called = "list"	# A list of the statement's function calls and potential function calls.]
	)
)

# This is called when a new DDG Statement is created.  It initializes all of the slots.
setMethod ("initialize", "DDGStatement",
	function(.Object, parsed, pos, script.name, script.num, breakpoints, parseData){
		.Object@parsed <- parsed
		
		# deparse can return a vector of strings.  We convert that into
		# one long string.
		.Object@text <- paste(deparse(.Object@parsed[[1]]), collapse="")
		#print(.Object@text)
		
		.Object@abbrev <-
			# If this is a call to ddg.eval, we only want the argument to ddg.eval
			# (which is a string) to appear in the node label
			if (grepl("^ddg.eval", .Object@text)) {
				.ddg.abbrev.cmd(.Object@parsed[[1]][[2]])
			}
			else {
				.ddg.abbrev.cmd(.Object@text)
			}
			
		vars.used <- .ddg.find.var.uses(.Object@parsed[[1]])
		
		# Remove index variable in for statement (handled separately in ddg.forloop).
		if (length(parsed) > 0 && !is.symbol(parsed[[1]]) && parsed[[1]][[1]] == "for") {
			index.var <- c(parsed[[1]][[2]])
			vars.used <- vars.used[! vars.used %in% index.var]
		}
		
		.Object@vars.used <- vars.used
		
		.Object@vars.set <- .ddg.find.simple.assign(.Object@parsed[[1]])
		
		.Object@vars.possibly.set <- .ddg.find.assign(.Object@parsed[[1]])

		# ddg.eval is treated differently than other calls to ddg functions since
		# we will execute the parameter as a command and want a node for it.
		.Object@isDdgFunc <- grepl("^ddg.", .Object@text) & !grepl("^ddg.eval", .Object@text)
		
		.Object@readsFile <- .ddg.reads.file (.Object@parsed[[1]])
		.Object@writesFile <- .ddg.writes.file (.Object@parsed[[1]])
		.Object@createsGraphics <- .ddg.creates.graphics (.Object@parsed[[1]])
		.Object@updatesGraphics <- .ddg.updates.graphics (.Object@parsed[[1]])
		.Object@has.dev.off <- .ddg.has.call.to (.Object@parsed[[1]], "dev.off")
		
		.Object@pos <-
			if (is.object(pos)) {
				pos
			}
			else {
				null.pos()
			}
			
		.Object@script.num <-
			if (is.na(script.num)) 
				-1
			else 
				script.num
		
		.Object@is.breakpoint <-
			if (is.object(breakpoints)) {
				# If this statement is a function declaration, set a breakpoint on the declaration
				# if the breakpoint is for the first line of the function.  Otherwise, we will
				# want the breakpoint to be on one of the contained lines.
				if (.ddg.is.assign(.Object@parsed[[1]]) && .ddg.is.functiondecl(.Object@parsed[[1]][[3]])) {
					is.breakpoint <- any(breakpoints$lnum == .Object@pos@startLine)
				}
				
				# If this is not a function declaration, then set a breakpoint if it is on any line
				# within the statement.
				else {
					is.breakpoint <- any(breakpoints$lnum >= .Object@pos@startLine & breakpoints$lnum <= .Object@pos@endLine)
				}
			}
			# No breakpoints are set in the script.
			else {
				is.breakpoint <- FALSE
			}
		
		.Object@contained <-
			# The contained field is a list of DDGStatements for all statements inside
			# the function or control statement.  If we are collecting
			# provenance inside functions or control statements, we will execute
			# annotated versions of these statements.
			.ddg.parse.contained(.Object, script.name, parseData)
		
		.Object@annotated <-
			# If this is a call to ddg.eval, we only want to execute
			# the argument to ddg.eval
			if (grepl("^ddg.eval", .Object@text)) {
				parse(text=.Object@parsed[[1]][[2]])
			}
			else {
				.ddg.add.annotations(.Object)
			}
		
		#print(paste ("annotated statement", .Object@annotated))
		
		# find the list of the names of the function calls in the statement
		.Object@functions.called <- .ddg.find.calls( .Object@parsed[[1]] )
		
		return(.Object)
	}
)

# Finds the function calls and potential function calls in an expression.
# This function wraps the last two vectors in the returned value of 
# .ddg.find.calls.rec into a data frame, keeping the other two vectors the same,
# before returning the resulting list.
#
# @param expr The parse tree for the statement
#
# @return
#	[1]: functions from unknown libraries (character vector)
#	[2]: variable names, which may refer to functions (character vector)
#	[3]: known function calls with their respective libraries (data frame)

.ddg.find.calls <- function(expr) 
{
	# The returned list of .ddg.find.calls.rec(expr) contains:
	# 	[1]: functions from unknown libraries
	# 	[2]: variable names, which may refer to functions
	# 	[3]: functions with known libraries
	# 	[4]: libraries which the functions in [3] are from
	result <- .ddg.find.calls.rec(expr)
	
	# [3] and [4] of the returned value of .ddg.find.calls.rec(expr) are paired.
	# As this function wraps [3] and [4] into a data frame before returning the resulting list, 
	# if [3] is null (the statement does not contain `::` or `:::` operators),
	# then we can just return the result from the recursive function without [4].
	if( is.null(result[[3]]) )
		return( result[-4] )
	
	# wraps [3] and [4] into a data frame, changing the column names to match
	# the column names for ddg.function.nodes to enable rbind.
	fn.known.lib <- data.frame( result[[3]] , result[[4]] , stringsAsFactors = FALSE)
	names(fn.known.lib) <- c("ddg.fun","ddg.lib")
	
	return( list(result[[1]], result[[2]], unique(fn.known.lib)) )
}


# The recursive helper function for .ddg.find.calls.
# This function WILL FAIL if the user overwrites `::` or `:::`
#
# @return A named list of the following character vectors:
#		  [1]: functions from unknown libraries
#		  [2]: variable names, which may refer to functions
#		  [3]: functions with known libraries
#		  [4]: libraries which the functions in [3] are from

.ddg.find.calls.rec <- function(expr)
{
	# base case: a name or a constant
	if( (! is.call(expr) || .ddg.is.functiondecl(expr)) )
	{
		elem <- toString(expr)
		
		if( is.name(expr) && ! identical(elem, "") )
			return( list(NULL, elem, NULL, NULL) )
		else
			return( list(NULL, NULL, NULL, NULL) )
	}
	
	# expr[[1]] is a call: recurse on all parts of expr
	if( is.call(expr[[1]]) )
	{
		recursion.result <- lapply(expr, .ddg.find.calls.rec)
		
		fn.unknown.lib <- unlist( mapply(`[`, recursion.result, 1) )
		var.names <- unlist( mapply(`[`, recursion.result, 2) )
		
		fn.known.lib <- unlist( mapply(`[`, recursion.result, 3) )
		libraries <- unlist( mapply(`[`, recursion.result, 4) )
	}
	else
	{
		elem1 <- toString(expr[[1]])
		
		# general case for `::` or `:::`
		if( identical(elem1, "::") || identical(elem1, ":::") )
		{
			fn.unknown.lib <- elem1
			var.names <- NULL
			
			fn.known.lib <- toString(expr[[3]])
			libraries <- toString(expr[[2]])
		}
		else 	# general case
		{
			# case: function call with no parameters
			if( is.null(expr[-1]) )
				return( list(elem1, NULL, NULL, NULL) )
			
			recursion.result <- lapply(expr[-1], .ddg.find.calls.rec)
			
			fn.unknown.lib <- unlist( mapply(`[`, recursion.result, 1) )
			fn.unknown.lib <- append(elem1, fn.unknown.lib)
			
			var.names <- unlist( mapply(`[`, recursion.result, 2) )
			
			fn.known.lib <- unlist( mapply(`[`, recursion.result, 3) )
			libraries <- unlist( mapply(`[`, recursion.result, 4) )
		}
	}
	
	fn.unknown.lib <- unique(fn.unknown.lib)
	var.names <- unique(var.names)
	
	return( list(fn.unknown.lib, var.names, fn.known.lib, libraries) )
}


# A special null value for when source code position information is missing.
null.pos <- function() {
  return (new (Class = "DDGStatementPos", NA))
}


# Create a DDGStatement.
# expr - the parsed expression
# pos - the DDGStatementPos object for this statement
# script.name - the name of the script the statement is from
# script.num - the script number used to find the script in the sourced script table
# breakpoints - all the breakpoints currently set
# parseData - the object created by the parser that gives us source position information
.ddg.construct.DDGStatement <- function (expr, pos, script.name, script.num, breakpoints, parseData) {
	#print(paste(".ddg.construct.DDGStatement: expr =", expr))
	# Surprisingly, if a statement is just a number, like 1 (which could be the last statement in a function, for example),
	# the parser returns a number, rather than a parse tree!
	if (is.numeric(expr))
		expr <- parse(text=expr)
	
	return (new (Class = "DDGStatement", parsed = expr, pos, script.name, script.num, breakpoints, parseData))
}

# .ddg.abbrev.cmd abbreviates a command to the specified length.
# Default is 60 characters.

# cmd - command string.
# len (optional) - number of characters.

.ddg.abbrev.cmd <- function(cmd, len=60) {
	if (length(cmd) > 1) {
		cmd <- paste (cmd, collapse = " ")
	}
	
	if (file.exists(cmd))
		basename(cmd)
	else if (nchar(cmd) <= len)
		cmd
	else if (substr(cmd, len, len) != "\\")
		substr(cmd, 1, len)
	else if (substr(cmd, len-1, len) == "\\\\")
		substr(cmd, 1, len)
	else
		substr(cmd, 1, len-1)
}

# .ddg.find.var.uses returns a vector containing all the variables
# used in an expression.  Each value is unique in the returned
# vector, so that if a variable is used more than once, it
# only appears once.

# main.object - input expression.

.ddg.find.var.uses <- function(main.object) {
  # Recursive helper function.
  .ddg.find.var.uses.rec <- function(obj) {

    # Base cases.
    if (is.atomic(obj)) {
      return(character())  # A name is not atomic!
    }

    if (is.name(obj)) {
      if (nchar(obj) == 0) return (character())

      # Operators also pass the is.name test.  Make sure that if it is a
      # single character, then it is alpha-numeric.
      if (nchar(obj) == 1 && !grepl("[[:alpha:]]", obj)) return (character())
      #print(paste(".ddg.find.var.uses found", deparse(obj)))
      return (deparse(obj))
    }

    if (!is.recursive(obj)) return(character())

    if (.ddg.is.functiondecl(obj)) return(character())

    tryCatch(
      {
        if (.ddg.is.assign(obj)) {

          # If assigning to a simple variable, recurse on the right
          # hand side of the assignment.

          # covers cases: '=', '<-', '<<-' for simple variable assignments
          # e.g.  a <- 2
          if (is.symbol(obj[[2]])) {
            unique(unlist(.ddg.find.var.uses.rec(obj[[3]])))
          }

          # If assigning to an expression (like a[b]), recurse on the
          # indexing part of the lvalue as well as on the expression.
          # covers cases:
          # storage.mode(z)
          # a[1] <- 2, a[b] <- 3
          else if (is.call(obj[[2]])) {
            variables <- c( .ddg.find.var.uses.rec(obj[[2]][[2]]) , unlist(.ddg.find.var.uses.rec(obj[[3]])) )

            # for array index cases like a[b] <- 3,
            # where there could be a variable in the brackets
            if( obj[[2]][[1]] == '[' )
              append( variables , .ddg.find.var.uses.rec(obj[[2]][[3]]) )

            unique( variables )
          }

          # covers cases where there is a string literal.
          # for assign function
          else if (is.character(obj[[2]])) {
            unique( c(unlist(.ddg.find.var.uses.rec(parse(text = obj[[2]])[[1]])) , unlist(.ddg.find.var.uses.rec(parse(text = obj[[3]])[[1]]))) )
          }

          # not entirely sure what this catches
          else {
            unique(c (.ddg.find.var.uses.rec(obj[[2]]), unlist(.ddg.find.var.uses.rec(obj[[3]]))))
          }
        }

        # Not an assignment.  Recurse on all parts of the expression
        # except the operator.
        else {
          unique(unlist(lapply(obj[1:length(obj)], .ddg.find.var.uses.rec)))
        }
      },
      error = function(e)
      {
        print (paste(".ddg.find.var.uses.rec:  Error analyzing", deparse(obj)))
        character()
      }
    )
  }

  return(.ddg.find.var.uses.rec(main.object))
}


# .ddg.find.simple.assign returns the name of the variable assigned
# to if the object passed in is an expression representing an
# assignment statement.  Otherwise, it returns NULL.

# obj - input expression.

.ddg.find.simple.assign <- function(obj)
{
	if (.ddg.is.assign(obj)) {
		.ddg.get.var(obj[[2]])
	}
	else {
		""
	}
}


# .ddg.is.assign returns TRUE if the object passed is an expression
# object containing an assignment statement.

# expr - a parsed expression.

.ddg.is.assign <- function (expr)
{
	if (is.call(expr))
	{
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

# lvalue - a parsed expression.

.ddg.get.var <- function(lvalue)
{
  if (is.symbol(lvalue))
    deparse(lvalue)

  # for string literals
  # e.g. when the assign function is used
  else if ( is.character(lvalue) )
    .ddg.get.var( parse(text = lvalue)[[1]] )

  else
    .ddg.get.var(lvalue[[2]])
}


# .ddg.find.assign returns a vector containing the names of all
# the variables assigned in an expression.  The parameter should
# be an expression object. For example, if obj represents the
# expression "a <- (b <- 2) * 3", the vector returned will contain
# both a and b.

# obj - a parsed expression.

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


# ddg.is.functiondecl tests to see if an expression is a function
# declaration.

# expr - a parsed expression.

.ddg.is.functiondecl <- function(expr) {
	if (is.symbol(expr) || !is.language(expr))
		return (FALSE)
	if (is.null(expr[[1]]) || !is.language(expr[[1]]))
		return (FALSE)
	
	return (expr[[1]] == "function")
}

# .ddg.get.statement.type returns the control type (if applicable) of a
# parsed statement.

.ddg.get.statement.type <- function(parsed.command) {
  if (length(parsed.command) > 1) return(parsed.command[[1]])
  return(0)
}

# .ddg.add.annotations accepts a DDGStatement and returns an expression.
# The returned expression is annotated as needed.

.ddg.add.annotations <- function(command) {
  #print("In .ddg.add.annotations")
  #print(paste("command@text =", command@text))
  parsed.command <- command@parsed[[1]]

  # Return if statement is empty.
  if (length(parsed.command) == 0) return(command@parsed)

  # Replace source with ddg.source.
  if (is.call(parsed.command) && parsed.command[[1]] == "source") {
    return(.ddg.add.ddg.source(parsed.command))
  }

  # Annotate user-defined functions.
  # Note that this will not annotate anonymous functions, like ones that might be passed to lapply, for example
  # Is that what we want?
  if (.ddg.is.assign(parsed.command) && .ddg.is.functiondecl(parsed.command[[3]])) {
      return(.ddg.add.function.annotations(command))
    }
  
  statement.type <- as.character(.ddg.get.statement.type(parsed.command))
  loop.types <- list("for", "while", "repeat")
  if (length(statement.type > 0) && !is.null(statement.type)) { # Move into funcs below && ddg.max.loops() > 0) {

    # Annotate if statement.
    if (statement.type == "if"){
      return(.ddg.annotate.if.statement(command))
    }

    # Annotate for, while, repeat statement.
    else if (statement.type %in% loop.types) {
      return(.ddg.annotate.loop.statement(command, statement.type))
    }

    # Annotate simple block.
    else if (statement.type == "{") {
      return(.ddg.annotate.simple.block(command))
    }
  }

  # Not a function or control construct.  No annotation required.
  return(command@parsed)
}

# .ddg.parse.contained creates the DDGStatement objects that correspond to
# statements inside a function or control block (or blocks).
#
# cmd - the DDGStatement being considered
# script.name - the name of the script the statement is from
# parseData - the data returned by the parser that is used to extract
#     source position information
#
# Returns a list of DDTStatements or an empty list if this is not a function
# declaration or a control construct.

.ddg.parse.contained <- function (cmd, script.name, parseData) {
  # print("In .ddg.parse.contained")
  parsed.cmd <- cmd@parsed[[1]]
  #print(paste(".ddg.parse.contained: cmd@parsed =", deparse(cmd@parsed)))
  #print(paste(".ddg.parse.contained: parsed.cmd =", deparse(parsed.cmd)))

  # Function declaration
  if (.ddg.is.assign(parsed.cmd) && .ddg.is.functiondecl(parsed.cmd[[3]])) {
      # Create the DDGStatement objects for the statements in the function
      return (.ddg.parse.contained.function(cmd, script.name, parseData, parsed.cmd[[3]][[3]]))
  }
  
  # Check if we want to go inside loop and if-statements
  else if (ddg.max.loops() == 0) {
    return (list())
  }

  # Control statements.
  st.type <- as.character(.ddg.get.statement.type(parsed.cmd))

  # If statement.
  if (st.type == "if") {
    return (.ddg.parse.contained.if(cmd, script.name, parseData, parsed.cmd))
  }

  # Other control statements
  else {
    control.types <- list("for", "while", "repeat", "{")
    if (length(st.type) > 0 && !is.null(st.type) && (st.type %in% control.types)) {
      return (.ddg.parse.contained.control(cmd, script.name, parseData, parsed.cmd, st.type))
    }
  }

  # Not a function declaration or control construct.
  return(list())
}

.ddg.parse.contained.function <- function (cmd, script.name, parseData, func.body) {
  #print(paste(".ddg.parse.contained.function: func.body =", deparse(func.body)))
  # The function body is a block.  Extract the statements inside the block
  if (func.body[[1]] == "{") {
    func.stmts <- func.body[2:length(func.body)]
  }

  # The function body is a single statement.
  else {
    func.stmts <- list(func.body)
  }

  # Create the DDGStatement objects for the statements in the function
  return (.ddg.create.DDGStatements (func.stmts, script.name, cmd@script.num, parseData, cmd@pos))
}

.ddg.parse.contained.if <- function (cmd, script.name, parseData, parent) {
  block.stmts <- list()

  # If and else if blocks.
  while(!is.symbol(parent) && parent[[1]] == "if") {
    # Get block
    block <- parent[[3]]
    block <- .ddg.ensure.in.block(block)

    # Get statements for this block.
    for (i in 2:(length(block))) {
      block.stmts <- c(block.stmts, block[[i]])
    }

    # Check for possible final else.
    if (length(parent) == 4) {
      final.else <- TRUE
    } else {
      final.else <- FALSE
    }

    # Get next parent
    parent <- parent[[(length(parent))]]
  }

  # Final else block (if any).
  if (final.else) {
    # Get block.
    block <- parent
    block <- .ddg.ensure.in.block(block)

    # Get statements for this block.
    for (i in 2:(length(block))) {
      block.stmts <- c(block.stmts, block[[i]])
    }
  }

  # Create the DDGStatement objects for statements in block
  return (.ddg.create.DDGStatements (block.stmts, script.name, cmd@script.num, parseData, cmd@pos))
}

# If there is a singleton statement inside a control construct nest it inside a block.
# Return the block.
.ddg.ensure.in.block <- function(block) {
  if (is.symbol(block) || block[[1]] != "{") call("{", block)
  else block
}


.ddg.parse.contained.control <- function(cmd, script.name, parseData, parsed.cmd, st.type) {
  block.stmts <- list()

  if (st.type == "for") block <- parsed.cmd[[4]]
  else if (st.type == "while") block <- parsed.cmd[[3]]
  else if (st.type == "repeat") block <- parsed.cmd[[2]]
  else if (st.type == "{") block <- parsed.cmd

  block <- .ddg.ensure.in.block(block)

  for (i in 2:length(block)) {
    block.stmts <- c(block.stmts, block[[i]])
  }

  # Create the DDGStatement objects for statements in block
  return (.ddg.create.DDGStatements (block.stmts, script.name, cmd@script.num, parseData, cmd@pos))
}

# .ddg.add.ddg.source replaces source with ddg.source.
#
# parsed.command must be a parsed expression that is a call
# to the source function.

.ddg.add.ddg.source <- function(parsed.command) {
  script.name <- deparse(parsed.command[[2]])
  parsed.command.txt <- paste("ddg.source(", script.name, ")", sep="")
  return(parse(text=parsed.command.txt))
}

# .ddg.add.function.annotations is passed a command that corresponds
# to a function declaration.  It returns a parsed command corresponding
# to the same function declaration but with calls to ddg.function,
# ddg.eval and ddg.return.value inserted if they are not already present.
# The functions ddg.annotate.on and ddg.annotate.off may be used to provide
# a list of functions to annotate or not to annotate, respectively.
#
# function.decl should be a command that contains an assignment statement where the value
# being bound is a function declaration

.ddg.add.function.annotations <- function(function.decl) {
  #print("In .ddg.add.function.annotations")
  parsed.function.decl <- function.decl@parsed[[1]]

  # Get function name.
  func.name <- toString(parsed.function.decl[[2]])
  #print(paste("Annotating", func.name))

    # Get function definition.
    func.definition <- parsed.function.decl[[3]]

    # Create function block if necessary.
    if (func.definition[[3]][[1]] != "{") {
      func.definition <- .ddg.create.function.block(func.definition)
    }

    # Create new function body with an if-then statement for annotations.
    func.definition <- .ddg.add.conditional.statement(func.definition, func.name)

    # Insert call to ddg.function if not already added.
    if (!.ddg.has.call.to(func.definition[[3]], "ddg.function")) {
      func.definition <- .ddg.insert.ddg.function(func.definition)
    }

    # Insert calls to ddg.return.value if not already added.
    if (!.ddg.has.call.to(func.definition[[3]], "ddg.return.value")) {
      func.definition <- .ddg.wrap.all.return.parameters(func.definition, function.decl@contained)
    }

    # Wrap last statement with ddg.return.value if not already added
    # and if last statement is not a simple return or a ddg function.
    last.statement <- .ddg.find.last.statement(func.definition)
    
   if (!.ddg.is.call.to(last.statement, "ddg.return.value") & !.ddg.is.call.to(last.statement, "return") & !.ddg.is.call.to.ddg.function(last.statement)) {
     func.definition <- .ddg.wrap.last.line(func.definition, function.decl@contained)
   }

    # Wrap statements with ddg.eval if not already added and if
    # statements are not calls to a ddg function and do not contain
    # ddg.return.value.
    if (!.ddg.has.call.to(func.definition, "ddg.eval")) {
      func.definition <- .ddg.wrap.with.ddg.eval(func.definition, function.decl@contained)
    }

    # Reassemble parsed.command.
    #print(paste("Done annotating", func.name))
    return (as.expression (call ("<-", as.name(func.name), func.definition)))
  }

# .ddg.create.function.block creates a function block.
#
# func.definition is a parsed expression for a function declaration (not
#   the full assignment statement in which it is declared)
#
# Returns a parse tree for the same function declaration but with
# the function statements inside a block.

.ddg.create.function.block <- function(func.definition) {
  # Get the function parameters.
  func.params <- func.definition[[2]]

  # Get the body of the function.
  func.body <- func.definition[[3]]

  # Add block and reconstruct the call.
  new.func.body <- call("{", func.body)
  return(call("function", func.params, as.call(new.func.body)))
}

# .ddg.add.conditional.statement creates a new function definition
# containing an if-then statement used to control annotation.
#
# func.definition - original function definition.

.ddg.add.conditional.statement <- function(func.definition, func.name) {
  # Get the function parameters.
  func.params <- func.definition[[2]]

  # Get the body of the function.
  func.body <- func.definition[[3]]

  pos <- length(func.body)

  # Create new function definition containing if-then statement.
  # This will prevent us from collecting provenance inside
  # functions that are inside control structures when we 
  # are not collecting provenance in control structures.
  new.func.body.txt <-
    c(paste("if (ddg.should.run.annotated(\"", func.name, "\")) {", sep=""),
    as.list(func.body[2:pos]),
    paste("} else {", sep=""),
    as.list(func.body[2:pos]),
    paste("}", sep=""))

  new.func.expr <- parse(text=new.func.body.txt)
  new.func.body <- new.func.expr[[1]]

  return(call("function", func.params, call("{", new.func.body)))
}

# .ddg.insert.ddg.function inserts ddg.function before the first line
# in the annotated block of a function body.
#
# func.definition is a parsed expression for a function declaration (not
#   the full assignment statement in which it is declared)
#
# Returns a parse tree for the same function declaration but with
# a call to ddg.function() as the first statement.

.ddg.insert.ddg.function <- function(func.definition) {
  # Get the function parameters.
  func.params <- func.definition[[2]]

  # Get the body of the function.
  func.body <- func.definition[[3]]

  # Get annotated block.
  block <- func.body[[2]][[3]]
  pos <- length(block)

  # Insert ddg.function.
  inserted.statement <- call("ddg.function")
  new.statements.txt <- c(as.list("{"), inserted.statement, as.list(block[2:pos]), as.list("}"))
  block <- parse(text=new.statements.txt)[[1]]

  func.body[[2]][[3]] <- block

  return(call("function", func.params, as.call(func.body)))
}

# .ddg.wrap.return.parameters wraps parameters of return functions
# with ddg.return.value in the annotated block of a function body.
#
# block is the parse tree corresponding to the statements within
#   the annotated block of a function
# parsed.stmts is the list of DDGStatement objects contained in the
#   function
#
# Returns a parse tree for the same function body but with
# a call to ddg.return.value wrapped around all expressions that are
# returned.

.ddg.wrap.return.parameters <- function(block, parsed.stmts) {
  # Check each statement in the annotated block to see if it
  # contains a return.
  pos <- length(block)
  #print(paste(".ddg.wrap.return.parameters: pos =", pos))

  for (i in 1:pos) {
    statement <- block[[i]]
    #print(paste("statement", i, "=", deparse(statement)))
    #print(paste("parsed.stmts", i, "=", parsed.stmts[[i]]@abbrev))
    if (.ddg.has.call.to(statement, "return")) {

      #print(".ddg.wrap.return.parameters: found return call")
      
      # If statement is a return, wrap parameters with ddg.return.value.
      if (.ddg.is.call.to(statement, "return")) {
        #print(".ddg.wrap.return.parameters:  IS a return call")
        # Need to handle empty parameter separately.
        if (length(statement) == 1) {
          ret.params <- ""
        } else {
          ret.params <- statement[[2]]
        }
        
        #print(paste(".ddg.wrap.return.parameters: ret.params =", ret.params))

        #for (i in 1:length(parsed.stmts)) {
        #  print(paste(".ddg.wrap.return.parameters: parsed.stmts =", parsed.stmts[[i]]@abbrev))
        #}
        if (is.list(parsed.stmts)) {
          #print(".ddg.wrap.return.parameters: parsed.stmts is a list")
          #print(paste("str(parsed.stmts) =", str(parsed.stmts)))
          parsed.stmt <- parsed.stmts[[i-2]]
        }
        else {
          #print(".ddg.wrap.return.parameters: parsed.stmts is NOT a list")
          #print(paste("str(parsed.stmts) =", str(parsed.stmts)))
          parsed.stmt <- parsed.stmts
        }
        
        #print(paste(".ddg.wrap.return.parameters: parsed.stmt =", parsed.stmt@abbrev))
        
        # If parameters contain a return, recurse on parameters.
        if (.ddg.has.call.to(ret.params, "return")) {
          ret.params <- .ddg.wrap.return.parameters(ret.params, parsed.stmt)
        }

        new.ret.params <- .ddg.create.ddg.return.call(ret.params, parsed.stmt)
        new.statement <- call("return", new.ret.params)
        block[[i]] <- new.statement

        # If statement contains a return, recurse on statement.
      } else {
        #print(".ddg.wrap.return.parameters:  CONTAINS a return call")
        if (is.list(parsed.stmts)) {
          #print(".ddg.wrap.return.parameters: parsed.stmts is a list")
          #print(paste("str(parsed.stmts) =", str(parsed.stmts)))
          #print(paste("@contained[[1]] =", parsed.stmts[[1]]@contained[[1]]@text))
          parsed.stmt <- parsed.stmts[[i-2]]
        }
        else {
          #print(".ddg.wrap.return.parameters: parsed.stmts is NOT a list")
          #parsed.stmt <- parsed.stmts@contained[[i-2]]
          parsed.stmt <- parsed.stmts
        }

        #print("Recursing")
        #print(paste("Passing for parsed.stmt:", str(parsed.stmt)))
        block[[i]] <- .ddg.wrap.return.parameters(statement, parsed.stmt)
        #print("Returned from recursion")
      }
    }
    #print(paste(".ddg.wrap.return.parameters: after annotation, block[[", i, "]] =", paste(deparse(block[[i]]), collapse="\n")))
  }

  return(block)
}

# .ddg.wrap.all.return.parameters wraps parameters of all return
# functions with ddg.return.value in the annotated block of a function
# definition.
#
# func.definition is a parsed expression for a function declaration (not
#   the full assignment statement in which it is declared)
# parsed.stmts is the list of DDGStatement objects contained in the
#   function
#
# Returns a parse tree for the same function declaration but with
# a call to ddg.return.value wrapped around all expressions that are
# returned.

.ddg.wrap.all.return.parameters <- function(func.definition, parsed.stmts) {
  # Get function parameters.
  func.params <- func.definition[[2]]

  # Get the body of the function.
  func.body <- func.definition[[3]]

  # Get annotated block.
  block <- func.body[[2]][[3]]
  pos <- length(block)

  # Wrap individual return functions.
  block <- .ddg.wrap.return.parameters(block, parsed.stmts)

  # Get new function body
  func.body[[2]][[3]] <- block

  # Reconstruct function.
  return(call("function", func.params, as.call(func.body)))
}

# .ddg.find.last.statement finds the last statement in the annotated
# block of a function.
#
# func.definition is a parsed expression for a function declaration (not
#   the full assignment statement in which it is declared)
#
# Returns the parse tree corresponding to the last statement in the
# function definition.

.ddg.find.last.statement <- function (func.definition) {
  # Get function body.
  func.body <- func.definition[[3]]

  # Get annotated block.
  block <- func.body[[2]][[3]]
  pos <- length(block)

  # Return final statement in block.
  return(block[[pos]])
}

# .ddg.wrap.last.line wraps the last line of the annotated block of a
# function with ddg.return.value.
#
# func.definition is a parsed expression for a function declaration (not
#   the full assignment statement in which it is declared)
# parsed.stmts is the list of DDGStatement objects contained in the
#   function
#
# Returns a parse tree for the same function declaration but with
# a call to ddg.return.value wrapped around the last line in the
# body.

.ddg.wrap.last.line <- function(func.definition, parsed.stmts) {
  # Get function parameters.
  func.params <- func.definition[[2]]

  # Get the body of the function.
  func.body <- func.definition[[3]]

  # Get annotated block.
  block <- func.body[[2]][[3]]
  pos <- length(block)

  last.statement <- block[[pos]]
  parsed.stmt <- parsed.stmts[[length(parsed.stmts)]]

  wrapped.statement <- .ddg.create.ddg.return.call(last.statement, parsed.stmt)
  func.body[[2]][[3]][[pos]] <- wrapped.statement

  return(call("function", func.params, as.call(func.body)))
}

# Creates a call to ddg.return.value using a closure so that we
# will be able to refer to the correct DDGStatement object when the
# return call is executed.
#
# last.statement is the parse tree for the expression being returned
# parsed.stmt is the DDGStatement object corresponding to the last statement
#
# Returns a parse tree with a call to ddg.return.value.  The arguments to
# ddg.return.value are the parsed statement and the DDGStatement object.

.ddg.create.ddg.return.call <- function (last.statement, parsed.stmt) {
  # We need to force the evaluation of parsed.stmt for the closure to
  # return the value that parsed.stmt has at the time the ddg.eval
  # call is created.
  force(parsed.stmt)
  #print(paste(".ddg.create.ddg.return.call: parsed.stmt =", parsed.stmt@abbrev))
  #print(paste(".ddg.create.ddg.return.call: last.statement =", last.statement))
  if (.ddg.has.call.to(last.statement, "return")) {
    #print(".ddg.create.ddg.return.call: has call to return")
    return (call ("ddg.return.value", last.statement, function() parsed.stmt))
  }
  else {
    #print(".ddg.create.ddg.return.call: NO call to return")
    
    # If there is no return call, we will use ddg.eval to execute the
    # statement and then ddg.return.value to create the necessary return
    # structure.  We cannot use this technique if there is a return call
    # because we if tried to eval a return call, we would end up returning
    # from some code inside RDT, instead of the user's function.
    eval.cmd <- .ddg.construct.DDGStatement (parse(text=deparse(last.statement)), pos=NA, script.num=NA, breakpoints=NA, parseData=NULL)
    new.statement <- .ddg.create.ddg.eval.call(last.statement, parsed.stmt)
    return (call ("ddg.return.value", new.statement, function() parsed.stmt))
  }
}

# Creates a call to ddg.eval using a closure so that we
# will be able to refer to the correct DDGStatement object when the
# return call is executed.
#
# statement is the parse tree for the expression being returned
# parsed.stmt is the DDGStatement object corresponding to the last statement
#
# Returns a parse tree with a call to ddg.eval.  The arguments to
# ddg.eval are the original statement and the DDGStatement object.

.ddg.create.ddg.eval.call <- function (statement, parsed.stmt) {
  # We need to force the evaluation of parsed.stmt for the closure to
  # return the value that parsed.stmt has at the time the ddg.eval
  # call is created.
  force(parsed.stmt)

  return (call("ddg.eval", paste(deparse(statement), collapse=""), function() parsed.stmt))
}

# .ddg.wrap.with.ddg.eval wraps each statement in the annotated block
# of a function body with ddg.eval if the statement is not a call to a ddg
# function and does not contain a call to ddg.return.value. The statement
# is enclosed in quotation marks.
#
# func.definition is a parsed expression for a function declaration (not
#   the full assignment statement in which it is declared)
# parsed.stmts is the list of DDGStatement objects contained in the
#   function
#
# Returns a parse tree for the same function declaration but with
# the calls to ddg.eval inserted.

.ddg.wrap.with.ddg.eval <- function(func.definition, parsed.stmts) {
  # Get the function parameters.
  func.params <- func.definition[[2]]

  # Get the body of the function.
  func.body <- func.definition[[3]]

  # Get annotated block.
  block <- func.body[[2]][[3]]
  pos <- length(block)

  # Process each statement in block.
  for (i in 2:pos) {
    # Wrap with ddg.eval if statement is not a call to a ddg function and
    # does not contain a call to ddg.return.value. Enclose statement in
    # quotation marks.
    statement <- block[[i]]
    if (!grepl("^ddg.", statement[1]) & !.ddg.has.call.to(statement, "ddg.return.value")) {
      parsed.stmt <- parsed.stmts[[i-2]]
      new.statement <- .ddg.create.ddg.eval.call(statement, parsed.stmt)
      func.body[[2]][[3]][[i]] <- new.statement
    }
  }

  return(call("function", func.params, as.call(func.body)))
}

# Creates a call to ddg.eval using the number of the DDGStatement
# stored in the list ddg.statements in the ddg environment.
#
# statement is the parse tree for the expression being returned and
# parsed.stmt is the corresponding DDGStatement object.
#
# Returns a parse tree with a call to ddg.eval.  The arguments to
# ddg.eval are the original statement and the number of the DDGStatement object.

.ddg.create.block.ddg.eval.call <- function (statement, parsed.stmt) {
  # Get the next DDGStatement number and store parsed.stmt at this location.
  .ddg.inc("ddg.statement.num")
  num <- .ddg.statement.num()
  .ddg.add.ddgstatement(parsed.stmt)

  return (call("ddg.eval", paste(deparse(statement), collapse=""), num))
}

#.ddg.wrap.block.with.ddg.eval wraps each statement in a block with
# ddg.eval unless the statement is a ddg function or contains a call
# to ddg.return.value.

.ddg.wrap.block.with.ddg.eval <- function(block, parsed.stmts) {
  # Ignore initial brace.
  for (i in 2:length(block)) {
    # Enclose statement in quotation marks and wrap with ddg.eval.
    statement <- block[[i]]
    if (!grepl("^ddg.", statement) && !.ddg.has.call.to(statement, "ddg.return.value")) {
      parsed.stmt <- parsed.stmts[[i-1]]
      # print(statement)
      # print(parsed.stmt@text)

      new.statement <- .ddg.create.block.ddg.eval.call(statement, parsed.stmt)
      block[[i]] <- new.statement
    }
  }
  return(block)
}

# .ddg.add.block.start.finish adds start and finish nodes to blocks in control
# statements.

.ddg.add.block.start.finish <- function(block, pname) {
  # Create ddg.start & ddg.finish statements.
  start.statement <- deparse(call("ddg.start", pname))
  finish.statement <- deparse(call("ddg.finish", pname))

  # Get internal statements.
  pos <- length(block)
  statements <- deparse(block[[2]])
  if (pos > 2) {
    for (i in 3:pos) {
      statements <- append(statements, deparse(block[[i]]))
    }
  }

  # Create new block.
  block.txt <- paste(c("{", start.statement, statements, finish.statement, "}"), collapse="\n")
  block.parsed <- parse(text=block.txt)

  return(block.parsed[[1]])
}

# .ddg.insert.ddg.forloop inserts a ddg.forloop statement at the top of a block.

.ddg.insert.ddg.forloop <- function(block, index.var) {
  pos <- length(block)
  inserted.statement <- call("ddg.forloop", index.var)

  # Block with single statement.
  if (pos == 2) {
    new.statements <- c(as.list(block[[1]]),  inserted.statement, as.list(block[2]))
    return(as.call(new.statements))
  }

  # Block with multiple statements.
  else {
    new.statements <- c(as.list(block[[1]]), inserted.statement, as.list(block[2:pos]))
    return(as.call(new.statements))
  }
}

# .ddg.insert.ddg.loop.annotate inserts a ddg.loop.annotate.on or
# ddg.loop.annotate.off statement at the beginning of a block.

.ddg.insert.ddg.loop.annotate <- function(block, var) {
  pos <- length(block)
  if (var == "on") inserted.statement <- call("ddg.loop.annotate.on")
  else if (var == "off") inserted.statement <- call("ddg.loop.annotate.off")

  # Block with single statement.
  if (pos == 2) {
    new.statements <- c(as.list(block[[1]]),  inserted.statement, as.list(block[2]))
    return(as.call(new.statements))
  }

  # Block with multiple statements.
  else {
    new.statements <- c(as.list(block[[1]]), inserted.statement, as.list(block[2:pos]))
    return(as.call(new.statements))
  }
}

# .ddg.annotate.if.statement adds annotations to if statements.

.ddg.annotate.if.statement <- function(command) {
  #print(paste(".ddg.annotate.if.statement annotating", command@text))
  if (ddg.max.loops() == 0) {
    parsed.command.txt <- deparse(command@parsed[[1]])
  }
  
  else  {
    # Get parsed command & contained statements
    parsed.command <- command@parsed[[1]]
    parsed.stmts <- command@contained
  
    # Set initial values.
    bnum <- 1
    ptr <- 0
    parent <- parsed.command
    parsed.command.txt <- vector()
  
    # If & else if blocks.
    while(!is.symbol(parent) && parent[[1]] == "if") {
      # Get block
      block <- parent[[3]]
      block <- .ddg.ensure.in.block(block)
  
      # Get statements for this block.
      block.stmts<- list()
      for (i in 1:(length(block)-1)) {
        block.stmts <- c(block.stmts, parsed.stmts[[i+ptr]])
      }
  
      # Advance pointer for next block.
      ptr <- ptr + length(block) - 1
      
      # Wrap each statement with ddg.eval.
      block <- .ddg.wrap.block.with.ddg.eval(block, block.stmts)
  
      # Add start and finish nodes.
      block <- .ddg.add.block.start.finish(block, "if")
  
      # Reconstruct original statement.
      cond <- paste(deparse(parent[[2]]), collapse="")
      if (bnum == 1) {
        statement.txt <- paste(c(paste("if (", cond, ")", sep=""), deparse(block), collapse="\n"))
      } else {
        statement.txt <- paste(c(paste("} else if (", cond, ")", sep=""), deparse(block), collapse="\n"))
      }
  
      # Remove final brace & new line.
      if (bnum > 1) {
        last <- length(parsed.command.txt) - 2
        parsed.command.txt <- parsed.command.txt[c(1:last)]
      }
      parsed.command.txt <- append(parsed.command.txt, statement.txt)
  
      # Check for possible final else.
      if (length(parent) == 4) {
        final.else <- TRUE
      } else {
        final.else <- FALSE
      }
  
      # Get next parent
      bnum <- bnum + 1
      parent <- parent[[(length(parent))]]
    }
  
    # Final else block (if any).
    if (final.else) {
      # Get block.
      block <- parent
      block <- .ddg.ensure.in.block(block)
  
      # Get statements for this block
      block.stmts <- list()
      for (i in 1:(length(block)-1)) {
        block.stmts <- c(block.stmts, parsed.stmts[[i+ptr]])
      }
  
      # Wrap each statement with ddg.eval.
      block <- .ddg.wrap.block.with.ddg.eval(block, block.stmts)
  
      # Add start and finish nodes.
      block <- .ddg.add.block.start.finish(block, "if")
  
      # Reconstruct original statement
      statement.txt <- paste(c(paste("} else", sep=""), deparse(block), collapse=""))
  
      # Remove final brace.
        last <- length(parsed.command.txt) - 2
        parsed.command.txt <- parsed.command.txt[c(1:last)]
        parsed.command.txt <- append(parsed.command.txt, statement.txt)
    }
  }
  
  parsed.command.txt <- 
      append(parsed.command.txt, "ddg.set.inside.loop()", after = 0)
  parsed.command.txt <-
      append(parsed.command.txt, "ddg.not.inside.loop()")

  #print(paste(".ddg.annotate.if.statement annotated version:", parsed.command.txt))
  return(parse(text=parsed.command.txt))
}

# .ddg.annotate.loop.statement adds annotations to for, while and repeat
# statements. Provenance is collected for the number of iterations
# specified in the parameter max.loops, beginning with the iteration
# specified in the parameter first.loop. A Details Omitted node may be
# added before and after the annotated section, as needed.

.ddg.annotate.loop.statement <- function(command, loop.type) {
  if (ddg.max.loops() == 0) {
    # Note that I can't just use command@text because it does not separate statements with newlines
    parsed.command.txt <- deparse(command@parsed[[1]])
  }
  
  else  {
    
    # Get parsed command
    parsed.command <- command@parsed[[1]]
  
    # Add new loop & get loop number.
    .ddg.add.loop()
    .ddg.inc("ddg.loop.num")
    ddg.loop.num <- .ddg.loop.num()
  
    # Get statements in block.
    if (loop.type == "for") {
      block <- parsed.command[[4]]
    }
    else if (loop.type == "while") {
      block <- parsed.command[[3]]
    }
    else {  # repeat
      block <- parsed.command[[2]]
    }
  
    # Add braces if necessary.
    block <- .ddg.ensure.in.block(block)
  
    # Wrap each statement with ddg.eval.
    annotated.block <- .ddg.wrap.block.with.ddg.eval(block, command@contained)
  
    # Insert ddg.forloop statement.
    if (loop.type == "for") {
      index.var <- parsed.command[[2]]
      annotated.block <- .ddg.insert.ddg.forloop(annotated.block, index.var)
    }
  
    # Add start and finish nodes.
    annotated.block <- .ddg.add.block.start.finish(annotated.block, paste(loop.type, "loop"))
  
    # Insert ddg.loop.annotate statements.
    block <- .ddg.insert.ddg.loop.annotate(block, "off")
  
    # Reconstruct for statement.
    block.txt <- deparse(block)
    annotated.block.txt <- deparse(annotated.block)
  
    # Calculate the control line of the annotated code
    if (loop.type == "for") {
      firstLine <- paste("for (", deparse(parsed.command[[2]]), " in ", deparse(parsed.command[[3]]), ") {", sep="")
    }
    else if (loop.type == "while") {
      firstLine <- paste("while (", deparse(parsed.command[[2]]), ") {", sep="")
    }
    else {  # repeat
      firstLine <- paste("repeat {", sep="")
    }
  
    parsed.command.txt <- paste(c(firstLine,
      paste("if (ddg.loop.count.inc(", ddg.loop.num, ") >= ddg.first.loop() && ddg.loop.count(", ddg.loop.num, ") <= ddg.first.loop() + ddg.max.loops() - 1)", sep=""),
      annotated.block.txt,
      paste("else", sep = ""),
      block.txt,
      paste("}", sep=""),
      paste("if (ddg.loop.count(", ddg.loop.num, ") > ddg.first.loop() + ddg.max.loops() - 1) ddg.details.omitted()", sep=""),
      paste("ddg.reset.loop.count(", ddg.loop.num, ")", sep=""),
      paste("if (ddg.max.loops() != 0) ddg.loop.annotate.on()"),  # Turn loop annotations back on in case we reached the max.
      collapse="\n"))
  }

  parsed.command.txt <- 
      append(parsed.command.txt, "ddg.set.inside.loop()", after = 0)
  parsed.command.txt <-
      append(parsed.command.txt, "ddg.not.inside.loop()")
  
  #print(parse(text=parsed.command.txt))

  return(parse(text=parsed.command.txt))
}

# .ddg.annotate.simple.block adds annotations to simple blocks.

.ddg.annotate.simple.block <- function(command) {
  # Get parsed command
  parsed.command <- command@parsed[[1]]

  # Get statements in block.
  block <- parsed.command

  # Wrap each statement with ddg.eval.
  block <- .ddg.wrap.block.with.ddg.eval(block, command@contained)

  # Add start and finish nodes.
  block <- .ddg.add.block.start.finish(block, "block")

  # Reconstruct block.
  block.txt <- deparse(block)

  return(parse(text=block.txt))
}

# .ddg.is.call.to returns TRUE if the parsed expression passed
# in is a call to the specified function.
#
# parsed.expr - a parse tree
# func.name - the name of a function

.ddg.is.call.to <- function(parsed.expr, func.name) {
  # Check if a function call.
  if (is.call(parsed.expr)) {
    # Check if the function called is the specified function.
    if (parsed.expr[[1]] == func.name) {
      return (TRUE)
    }
  }
  return (FALSE)
}

# .ddg.has.call.to returns TRUE if the parsed expression passed
# in contains a call to the specified function.
#
# parsed.expr - a parse tree
# func.name - the name of a function

.ddg.has.call.to <- function(parsed.expr, func.name) {
  # Base case.
  if (!is.recursive(parsed.expr)) return(FALSE)

  # If this is a function declaration, skip it
  if (.ddg.is.functiondecl(parsed.expr)) return(FALSE)

  # A call to the specified function.
  if (.ddg.is.call.to(parsed.expr, func.name)) {
    return (TRUE)
  }
  # Not a call to the specified function.  Recurse on the parts of
  # the expression.
  else {
    return (any(sapply(parsed.expr, function(parsed.expr) {return(.ddg.has.call.to(parsed.expr, func.name))})))
  }
}

# .ddg.is.call.to.ddg.function returns TRUE if the parsed expression
# passed in is a call to a ddg function.
#
# parsed.expr - a parse tree

.ddg.is.call.to.ddg.function <- function(parsed.expr) {
  # Check if a function call.
  if (is.call(parsed.expr)) {
    # Check if the function called is a ddg function.
    if (grepl("^ddg.", parsed.expr[1])) {
      return (TRUE)
    }
  }
  return (FALSE)
}

# Returns true if the statement contains a call to a function that read from a file
#
# parsed.statement - a parse tree
#
.ddg.reads.file <- function (parsed.statement) {
  .ddg.file.read.functions.df <- .ddg.get (".ddg.file.read.functions.df")
  reading.functions <- .ddg.file.read.functions.df$function.names
  return (TRUE %in% (lapply (reading.functions, function(fun.name) {return (.ddg.has.call.to(parsed.statement, fun.name))})))
}

# Returns true if the statement contains a call to a function that writes to a file
#
# parsed.statement - a parse tree
#
.ddg.writes.file <- function (parsed.statement) {
  .ddg.file.write.functions.df <- .ddg.get (".ddg.file.write.functions.df")
  writing.functions <- .ddg.file.write.functions.df$function.names
  return (TRUE %in% (lapply (writing.functions, function(fun.name) {return (.ddg.has.call.to(parsed.statement, fun.name))})))
}

# Returns true if the statement contains a call to a function that creates a graphics object
#
# parsed.statement - a parse tree
#
.ddg.creates.graphics <- function (parsed.statement) {
  .ddg.graphics.functions.df <- .ddg.get (".ddg.graphics.functions.df")
  graphics.functions <- .ddg.graphics.functions.df$function.names
  if (TRUE %in% (lapply (graphics.functions, function(fun.name) {return (.ddg.has.call.to(parsed.statement, fun.name))}))) {
    #print(paste("Setting @creates.graphics in", deparse(parsed.statement)))
    return(TRUE)
  }
  return (FALSE)
}

# Returns true if the statement contains a call to a function that updates a graphics object
#
# parsed.statement - a parse tree
#
.ddg.updates.graphics <- function (parsed.statement) {
  graphics.update.functions <- .ddg.get(".ddg.graphics.update.functions")
  if (TRUE %in% (lapply (graphics.update.functions, function(fun.name) {return (.ddg.has.call.to(parsed.statement, fun.name))}))) {
    #print("Found a graphics update function")
    return (TRUE)
  }
  return (FALSE)
}
