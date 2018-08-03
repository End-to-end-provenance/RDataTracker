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


#' .ddg.init.statements initialize the data used to manage the statements
#' @return nothing

.ddg.init.statements <- function() {
  .ddg.set("ddg.statement.num", 0)
  .ddg.set("ddg.statements", list())
}

#' .ddg.statement.num returns the number of DDG Statements created
#' @return the number of DDG statements created

.ddg.statement.num <- function() {
  return(.ddg.get("ddg.statement.num"))
}

#' .ddg.statements returns a list of DDG Statements created
#' @return the list of DDG statements created

.ddg.statements <- function() {
  return(.ddg.get("ddg.statements"))
}

#' .ddg.statement returns the ith DDG Statement
#' @param i the index of the statement to return
#' @return the ith DDGStatement

.ddg.statement <- function(i) {
  ddg.statements <- .ddg.statements()
  return(ddg.statements[[i]])
}

#' .ddg.add.ddgstatement adds a DDGStatement to the end of the list
#' @param stmt a DDGStatement object 
#' @return nothing

.ddg.add.ddgstatement <- function(stmt) {
  ddg.statements <- c(.ddg.statements(), stmt)
  .ddg.set("ddg.statements", ddg.statements)
}

# Information about where in the source code this statement appears.
methods::setClass("DDGStatementPos",
    slots = list(
        startLine = "numeric",
        startCol = "numeric",
        endLine = "numeric",
        endCol = "numeric")
)

# This is called automatically when there is a call to create a new
# DDGStatementPos object.
methods::setMethod ("initialize",
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
methods::setClass("DDGStatement",
    slots = list(
        text = "character",     # The original text in the file
        parsed = "expression",  # The parse tree for the statement
        abbrev = "character",   # A shortened version of the text to use in node names
        annotated = "expression",  # An annotated version of the statement.  This is
                                   # what we actually execute.

        # Note that vars.used through has.dev.off do not apply to a situation where
        # the statement is a function declaration, since declaring the statement
        # does not read from files, etc.  That happens when the function is called,
        # at which point we will refer to the information in the contained statements.

        vars.used = "character", # A list of the variables that are used in the statement
        vars.set = "character",  # If this is an assignment statement, this is the 
                                 # variable assigned
        vars.possibly.set = "character",  # If this contains any internal
                                          # assignment statements, like an 
                                          # if-statement might, for example, these 
                                          # are the variables assigned within the 
                                          # statement.
        isDdgFunc = "logical",   # True if this is a call to a ddg function
        pos = "DDGStatementPos",  # The location of this statement in the source code.
                                  # Has the value null.pos() if it is not available.
        script.num = "numeric",   # The number for the script this statement comes from.
                                  # Has the value -1 if it is not available
        contained = "list",        # If this is a function declaration, this will 
                                   # be a list of DDGStatement objects for the 
                                   # statements it contains.
        functions.called = "list" # A list of the statement's function calls and 
                                  # potential function calls.]
      )
)

# This is called when a new DDG Statement is created.  It initializes all of the slots.
methods::setMethod ("initialize",
  "DDGStatement",
    function(.Object, parsed, pos, script.name, script.num, parseData){
      .Object@parsed <- parsed

      # deparse can return a vector of strings.  We convert that into
      # one long string.
      .Object@text <- paste(deparse(.Object@parsed[[1]]), collapse="")
      #print(.Object@text)

      .Object@abbrev <-
          # If this is a call to .ddg.eval, we only want the argument to .ddg.eval
          # (which is a string) to appear in the node label
          if (grepl("^.ddg.eval", .Object@text)) {
            .ddg.abbrev.cmd(.Object@parsed[[1]][[2]])
          }
          else {
            .ddg.abbrev.cmd(.Object@text)
          }

      vars.used <- .ddg.find.var.uses(.Object@parsed[[1]])

      # Remove index variable in for statement (handled separately in .ddg.forloop).
      if (length(parsed) > 0 && !is.symbol(parsed[[1]]) && parsed[[1]][[1]] == "for") {
        index.var <- c(parsed[[1]][[2]])
        vars.used <- vars.used[! vars.used %in% index.var]
      }

      .Object@vars.used <- vars.used

      .Object@vars.set <- .ddg.find.simple.assign(.Object@parsed[[1]])

      .Object@vars.possibly.set <- .ddg.find.assign(.Object@parsed[[1]])

      # .ddg.eval is treated differently than other calls to ddg functions since
      # we will execute the parameter as a command and want a node for it.
      .Object@isDdgFunc <- grepl("^ddg.", .Object@text) & 
                           !grepl("^.ddg.eval", .Object@text)

      .Object@pos <-
          if (is.object(pos)) {
            pos
          }
          else {
            .ddg.null.pos()
          }

      .Object@script.num <-
          if (is.na(script.num)) -1
          else script.num

      .Object@contained <-
        # The contained field is a list of DDGStatements for all statements inside
        # the function or control statement.  If we are collecting
        # provenance inside functions or control statements, we will execute
        # annotated versions of these statements.
        .ddg.parse.contained(.Object, script.name, parseData)

      .Object@annotated <-
          # If this is a call to .ddg.eval, we only want to execute
          # the argument to .ddg.eval
          if (grepl("^.ddg.eval", .Object@text)) {
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

#' .ddg.create.DDGStatements creates the DDGStatement list for a list of parsed expressions.
#' @param exprs - a list of parsed expressions
#' @param script.name - the name of the script the expressions come from
#' @param script.num - the number of the script the expressions came from
#' @param parseData - information provided by the parser that we use to find line numbers
#' @param enclosing.pos - if exprs are statements within a function definition, enclosing.pos
#' is the source position information of the entire function declaration
#' @return a list of DDGStatement objects

.ddg.create.DDGStatements <- function (exprs, script.name, script.num, 
                                       parseData = NULL, enclosing.pos = NULL) {
  # The parse data gives us line number information
  if (is.null(parseData)) {
    parseData <- utils::getParseData(exprs, includeText=TRUE)
    
    if (is.null(parseData)) {
      # In this case there is no line number information available
      cmds <- vector("list", (length(exprs)))
      for (i in 1:length(exprs)) {
        expr <- as.expression(exprs[i])
        cmds[[i]] <- .ddg.construct.DDGStatement(expr, NA, script.name, 
                                                 script.num, parseData)
      }
      return(cmds)
    }
    
    non.comment.parse.data <- parseData[parseData$token != "COMMENT", ]
    if (nrow(non.comment.parse.data) == 0) {
      return(list())
    }
    
    # Start at the first non-comment expression in parseData
    next.parseData <- 1
  }
  
  else {
    non.comment.parse.data <- parseData[parseData$token != "COMMENT", ]
    
    # Start at the first entry in parse data that begins after the enclosing 
    # function begins, ends before the enclosing function ends, and matches the 
    # text of the first expression.
    next.parseData <- 
      which(non.comment.parse.data$line1 >= enclosing.pos@startLine & 
            non.comment.parse.data$line2 <= enclosing.pos@endLine & 
            non.comment.parse.data$text == paste(deparse(exprs[[1]]), collapse="\n") )[1]
  }
  
  # Create the DDGStatements
  cmds <- vector("list", (length(exprs)))
  next.cmd <- 1
  for (i in 1:length(exprs)) {
    expr <- as.expression(exprs[i][[1]])
    next.expr.pos <- methods::new (Class = "DDGStatementPos", 
                                   non.comment.parse.data[next.parseData, ])
    cmds[[next.cmd]] <- .ddg.construct.DDGStatement(expr, next.expr.pos, 
                                                    script.name, script.num, 
                                                    parseData)
    next.cmd <- next.cmd + 1
    
    # If there are more expressions, determine where to look next in the parseData
    if (i < length(exprs)) {
      last.ending.line <- non.comment.parse.data[next.parseData, ]$line2
      last.parent <- non.comment.parse.data[next.parseData, "parent"]
      last.id <- non.comment.parse.data[next.parseData, "id"]
      
      # Find the first entry in parseData that has the same parent as the
      # previous expression and starts after the previous expression.
      next.parseData <- which(non.comment.parse.data$parent == last.parent & 
                              non.comment.parse.data$line1 >= last.ending.line & 
                              non.comment.parse.data$id > last.id) [1]
    }
  }
  
  return (cmds)
}

#' .ddg.find.calls finds the function calls and potential function calls in an expression.
#' This function wraps the last two vectors in the returned value of 
#' .ddg.find.calls.rec into a data frame, keeping the other two vectors the same,
#' before returning the resulting list.
#' @param expr The parse tree for the statement
#' @return A list containing the following:
#' [1]: functions from unknown libraries (character vector)
#' [2]: variable names, which may refer to functions (character vector)
#' [3]: known function calls with their respective libraries (data frame)

.ddg.find.calls <- function(expr) 
{
  # The returned list of .ddg.find.calls.rec(expr) contains:
  #   [1]: functions from unknown libraries
  #   [2]: variable names, which may refer to functions
  #   [3]: functions with known libraries
  #   [4]: libraries which the functions in [3] are from
  result <- .ddg.find.calls.rec(expr)

  # [3] and [4] of the returned value of .ddg.find.calls.rec(expr) are paired.
  # As this function wraps [3] and [4] into a data frame before returning the 
  # resulting list, if [3] is null (the statement does not contain `::` or `:::` 
  # operators), then we can just return the result from the recursive function 
  # without [4].
  if( is.null(result[[3]]) )
    return( result[-4] )
  
  # wraps [3] and [4] into a data frame, changing the column names to match
  # the column names for ddg.function.nodes to enable rbind.
  fn.known.lib <- data.frame( result[[3]] , result[[4]] , stringsAsFactors = FALSE)
  names(fn.known.lib) <- c("ddg.fun","ddg.lib")
  
  return( list(result[[1]], result[[2]], unique(fn.known.lib)) )
}


#' .dgg.find.calls.rec is a recursive helper function for .ddg.find.calls.
#' This function WILL FAIL if the user overwrites `::` or `:::`
#' @param expr an expression
#' @return A named list of the following character vectors:
#' [1]: functions from unknown libraries
#' [2]: variable names, which may refer to functions
#' [3]: functions with known libraries
#' [4]: libraries which the functions in [3] are from

.ddg.find.calls.rec <- function(expr)
{
  # base case: a name or a constant
  if( ! is.call(expr) || .ddg.is.functiondecl(expr) )
  {
    elem <- toString(expr)
    
    # parameter names could be "", as is the case in a[2, ]
    # the if branch places such function parameter names that are not ""
    # into the var.names list for checking if they are function names at runtime.
    if( is.name(expr) && ! identical(elem, "") )
      return( list(NULL, elem, NULL, NULL) )
    else
      return( list(NULL, NULL, NULL, NULL) )
  }
  
  # expr is a call && expr[[1]] is a call: recurse on all parts of expr
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
    # e.g. stringi::stri_join
    # The parse tree is:  `::`, stringi, stri_join
    if( identical(elem1, "::") || identical(elem1, ":::") )
{
      fn.unknown.lib <- elem1
      var.names <- NULL
      
      fn.known.lib <- toString(expr[[3]])
      libraries <- toString(expr[[2]])
    }
    else  # general case
  {
      # If expr is a call, expr[[1]] is not a call and not `::` or `:::`,
      # then expr[[1]] is a function name.
      #
      # This recurses on all parts of expr but the first element, then
      # appending expr[[1]] to the list of functions with unknown libraries
      # (fn.unknown.lib) after combining the result of the recursive calls.
      #
      # e.g. 
      # let expr be 'as.character(a)'
      # expr is a call, its parse tree is:  as.character, a
      # expr[[1]], as.character, is the function name (not a call)
      
      # edge case: function call with no parameters
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

# .ddg.null.pos provides a special null value for when source code position 
#' information is missing.
#' @return a special null value

.ddg.null.pos <- function() {
  return (methods::new (Class = "DDGStatementPos", NA))
}


#' .ddg.construct.DDGStatement creates a DDGStatement.
#' @param expr - the parsed expression
#' @param pos - the DDGStatementPos object for this statement
#' @param script.name - the name of the script the statement is from
#' @param script.num - the script number used to find the script in the sourced script table
#' @param parseData - the object created by the parser that gives us source position information
#' @return a DDG statement

.ddg.construct.DDGStatement <- function (expr, pos, script.name, script.num, parseData) {
  #print(paste(".ddg.construct.DDGStatement: expr =", expr))
  # Surprisingly, if a statement is just a number, like 1 (which could be the last 
  # statement in a function, for example), the parser returns a number, rather 
  # than a parse tree!
  if (is.numeric(expr)) expr <- parse(text=expr)

  return (methods::new (Class = "DDGStatement", parsed = expr, pos, script.name, 
                        script.num, parseData))
}

#' .ddg.abbrev.cmd abbreviates a command to the specified length.
#' Default is 60 characters.
#' @param cmd - command string.
#' @param len (optional) - number of characters.
#' @return abbreviated command

.ddg.abbrev.cmd <- function(cmd, len=60) {
  if (length(cmd) > 1) {
    cmd <- paste (cmd, collapse = " ")
  }

  if (file.exists(cmd)) basename(cmd)
  else if (nchar(cmd) <= len) cmd
  else if (substr(cmd, len, len) != "\\") substr(cmd, 1, len)
  else if (substr(cmd, len-1, len) == "\\\\") substr(cmd, 1, len)
  else substr(cmd, 1, len-1)
}

#' .ddg.find.var.uses returns a vector containing all the variables
#' used in an expression.  Each value is unique in the returned
#' vector, so that if a variable is used more than once, it
#' only appears once.
#' @param main.object - input expression.
#' @return vector of variables used in the expression

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
            variables <- c( .ddg.find.var.uses.rec(obj[[2]][[2]]), 
                            unlist(.ddg.find.var.uses.rec(obj[[3]])) )

            # for array index cases like a[b] <- 3,
            # where there could be a variable in the brackets
            if( obj[[2]][[1]] == "[" )
              append( variables, .ddg.find.var.uses.rec(obj[[2]][[3]]) )

            unique( variables )
          }

          # covers cases where there is a string literal.
          # for assign function
          else if (is.character(obj[[2]])) {
            unique( c(unlist(.ddg.find.var.uses.rec(parse(text = obj[[2]])[[1]])), 
                      unlist(.ddg.find.var.uses.rec(parse(text = obj[[3]])[[1]]))) )
          }

          # not entirely sure what this catches
          else {
            unique(c (.ddg.find.var.uses.rec(obj[[2]]), 
                      unlist(.ddg.find.var.uses.rec(obj[[3]]))))
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


#' .ddg.find.simple.assign returns the name of the variable assigned
#' to if the object passed in is an expression representing an
#' assignment statement.  Otherwise, it returns NULL.
#' @param obj - input expression.
#' @return name of variable assigned to

.ddg.find.simple.assign <- function(obj)
{
  if (.ddg.is.assign(obj)) {
    .ddg.get.var(obj[[2]])
  }
  else {
    ""
  }
}


#' .ddg.is.assign returns TRUE if the object passed is an expression
#' object containing an assignment statement.
#' @param expr - a parsed expression.
#' @return True if an expression object containing an assignment

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

#' .ddg.get.var returns the variable being referenced in an
#' expression. It should be passed an expression object that is
#' either a variable, a vector access (like a[1]), a list member
#' (like a[[i]]) or a data frame access (like a$foo[i]).  For all of
#' these examples, it would return "a".
#' @param lvalue - a parsed expression.
#' @return name of variable referenced in the expression

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


#' .ddg.find.assign returns a vector containing the names of all
#' the variables assigned in an expression.  The parameter should
#' be an expression object. For example, if obj represents the
#' expression "a <- (b <- 2) * 3", the vector returned will contain
#' both a and b.
#' @param obj - a parsed expression.
#' @return a vector containing all variables assigned in the expression

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


#' ddg.is.functiondecl tests to see if an expression is a function
#' declaration.
#' @param expr - a parsed expression.
#' @return True if expression is a function declaration

.ddg.is.functiondecl <- function(expr) {
  if (is.symbol(expr) || !is.language(expr)) return (FALSE)
  if (is.null(expr[[1]]) || !is.language(expr[[1]])) return (FALSE)
  return (expr[[1]] == "function")
}

#' .ddg.get.statement.type returns the control type (if applicable) of a
#' parsed statement.
#' @param parsed.command a parsed statement
#' @return the control type of the statement

.ddg.get.statement.type <- function(parsed.command) {
  if (length(parsed.command) > 1) return(as.character(parsed.command[[1]]))
  return("")
}

#' .ddg.parse.contained creates the DDGStatement objects that correspond to
#' statements inside a function or control block (or blocks).
#' @param cmd - the DDGStatement being considered
#' @param script.name - the name of the script the statement is from
#' @param parseData - the data returned by the parser that is used to extract
#' source position information
#' @return a list of DDTStatements or an empty list if this is not a function
#' declaration or a control construct.

.ddg.parse.contained <- function (cmd, script.name, parseData) {
  # print("In .ddg.parse.contained")
  parsed.cmd <- cmd@parsed[[1]]
  #print(paste(".ddg.parse.contained: cmd@parsed =", deparse(cmd@parsed)))
  #print(paste(".ddg.parse.contained: parsed.cmd =", deparse(parsed.cmd)))

  # Function declaration
  if (.ddg.is.assign(parsed.cmd) && .ddg.is.functiondecl(parsed.cmd[[3]])) {
      # Create the DDGStatement objects for the statements in the function
      return (.ddg.parse.contained.function(cmd, script.name, parseData, 
                                            parsed.cmd[[3]][[3]]))
  }
  
  # Check if we want to go inside loop and if-statements
  else if (ddg.max.loops() == 0) {
    return (list())
  }

  # Control statements.
  st.type <- .ddg.get.statement.type(parsed.cmd)

  # If statement.
  if (st.type == "if") {
    return (.ddg.parse.contained.if(cmd, script.name, parseData, parsed.cmd))
  }

  # Other control statements
  else {
    control.types <- list("for", "while", "repeat", "{")
    if (length(st.type) > 0 && !is.null(st.type) && (st.type %in% control.types)) {
      return (.ddg.parse.contained.control(cmd, script.name, parseData, 
                                           parsed.cmd, st.type))
    }
  }

  # Not a function declaration or control construct.
  return(list())
}

#' .ddg.parse.contained.function creates DDG statement objects for statements
#' contained in a function
#' @param cmd a list of parsed expressions
#' @param script.name name of script
#' @param parseData information from the parser used to find line numbers
#' @param func.body body of function
#' @return DDG statement objects for statements in the function

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
  return (.ddg.create.DDGStatements (func.stmts, script.name, cmd@script.num, 
                                     parseData, cmd@pos))
}

#' .ddg.parse.contained.if creates DDG statement objects for statements in an
#' if statement
#' @param cmd a list of parsed expressions
#' @param script.name name of script
#' @param parseData information from the parser used to find line numbers
#' @param parent parent statement
#' @return DDG statement objects for statements in if statement

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
  return (.ddg.create.DDGStatements (block.stmts, script.name, cmd@script.num, 
                                     parseData, cmd@pos))
}

#' .ddg.ensure.in.block ensures that if there is a singleton statement inside a control 
#' construct it is enclosed in a block
#' @param block input statement(s)
#' @return a block containing the statement(s)

.ddg.ensure.in.block <- function(block) {
  if (is.symbol(block) || block[[1]] != "{") call("{", block)
  else block
}

#' .ddg.parse.contained.control creates DDG statements for statements contained in a
#' control construct.
#' @param cmd a list of parsed expressions
#' @param script.name name of script
#' @param parseData information from the parser used to find line numbers
#' @param parsed.cmd control construct statement
#' @param st.type statement type (for, while, repeat, simple block)
#' @return DDG statement objects for statements in the control construct

.ddg.parse.contained.control <- function(cmd, script.name, parseData, parsed.cmd, 
                                         st.type) {
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
  return (.ddg.create.DDGStatements (block.stmts, script.name, cmd@script.num, 
                                     parseData, cmd@pos))
}

#' .ddg.is.call.to returns TRUE if the parsed expression passed
#' in is a call to the specified function.
#' @param parsed.expr - a parse tree
#' @param func.name - the name of a function
#' @return True if the expression is a call to the specified function

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

#' .ddg.has.call.to returns TRUE if the parsed expression passed
#' in contains a call to the specified function.
#' @param parsed.expr - a parse tree
#' @param func.name - the name of a function
#' @return True if the parsed expression contains a call to the specified function

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
    return (any(sapply(parsed.expr, 
                       function(parsed.expr) {
                         return(.ddg.has.call.to(parsed.expr, func.name))
                       }
                       )))
  }
}
