# An S4 class to maintain information about a command
# 
# Author: blerner
###############################################################################

#.ddg.DDGStatement.init <- function() {

  setClass("DDGStatementPos",
      slots = list(
          startLine = "numeric",
          startCol = "numeric",
          endLine = "numeric", 
          endCol = "numeric")
  )
  
  setMethod ("initialize",
      "DDGStatementPos",
      function(.Object, parseData){
        if (length(parseData) == 1 && is.na(parseData)) {
          .Object@startLine <- -1
          .Object@startCol <- -1
          .Object@endLine <- -1
          .Object@endCol<- -1
        }
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
  
  setClass("DDGStatement",
      slots = list(
          text = "character",
          parsed = "language",
          quoted = "character",
          abbrev = "character",
          annotated = "language",
          vars.used = "character",
          vars.set = "character",
          vars.possibly.set = "character",
          isDdgFunc = "logical",
          readsFile = "logical",
          writesFile = "logical",
          createsGraphics = "logical",
          has.dev.off = "logical",
          pos = "DDGStatementPos",
          script.num = "numeric",
          is.breakpoint = "logical")
  )
  
  setMethod ("initialize",
      "DDGStatement",
      function(.Object, parsed, pos, script.num, is.breakpoint, annotate.functions){
        if (typeof(parsed) == "expression") {
          .Object@parsed <- parsed[[1]]
        }
        else {
          .Object@parsed <- parsed
        }
        .Object@text <- paste(deparse(.Object@parsed), collapse="")
        .Object@quoted <- gsub("\\\"", "\\\\\"", .Object@text)
        .Object@abbrev <- 
            if (grepl("^ddg.eval", .Object@text)) {
              .ddg.abbrev.cmd(.Object@parsed[[2]])
            }
            else {
              .ddg.abbrev.cmd(.Object@text)
            }
        
        .Object@annotated <- 
            if (grepl("^ddg.eval", .Object@text)) {
              parse(text=.Object@parsed[[2]])[[1]]
            }
            else {
              .ddg.add.annotations(.Object@parsed, annotate.functions)
            }
        .Object@vars.used <- .ddg.find.var.uses(.Object@annotated)
        .Object@vars.set <- .ddg.find.simple.assign(.Object@annotated)
        .Object@vars.possibly.set <- .ddg.find.assign(.Object@annotated)
        .Object@isDdgFunc <- grepl("^ddg.", .Object@text) & !grepl("^ddg.eval", .Object@text)
        .Object@readsFile <- .ddg.reads.file (.Object@parsed)
        .Object@writesFile <- .ddg.writes.file (.Object@parsed)
        .Object@createsGraphics <- .ddg.creates.graphics (.Object@parsed)
        .Object@has.dev.off <- .ddg.has.call.to (.Object@parsed, "dev.off")
        .Object@pos <- 
            if (is.object(pos)) {
              pos
            }
            else {
              null.pos()
            }
        .Object@script.num <- 
            if (is.na(script.num)) -1
            else script.num
        .Object@is.breakpoint <- is.breakpoint
        #print (.Object)
        return (.Object)
      }
  )
#}

null.pos <- function() {
  return (new (Class = "DDGStatementPos", NA))
}

.ddg.construct.DDGStatement <- function (expr, pos, script.num, is.breakpoint, annotate.functions) {
  #print(expr)
  #print(typeof(expr))
  return (new (Class = "DDGStatement", parsed = expr, pos, script.num, is.breakpoint, annotate.functions))
}


# .ddg.abbrev.cmd abbreviates a command to the specified length.
# Default is 60 characters.

# cmd - command string.
# len (optional) - number of characters.

.ddg.abbrev.cmd <- function(cmd, len=60) {
  if (length(cmd) > 1) {
    cmd <- paste (cmd, collapse = " ")
  }
  if(file.exists(cmd)){
    basename(cmd);
  } else {
    if (nchar(cmd) <= len) cmd
    else if (substr(cmd, len, len) != "\\") substr(cmd, 1, len)
    else if (substr(cmd, len-1, len) == "\\\\") substr(cmd, 1, len)
    else substr(cmd, 1, len-1)
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
    ##
    if (.ddg.is.functiondecl(obj)) return(character())
    
    tryCatch(
        if (.ddg.is.assign(obj)) {
              # If assigning to a simple variable, recurse on the right
              # hand side of the assignment.
              if (is.symbol(obj[[2]])) {
                filter(unlist(.ddg.find.var.uses.rec(obj[[3]])))
              }
              else if (is.call(obj[[2]])) {
                filter(c (.ddg.find.var.uses.rec(obj[[2]][[2]]), unlist(.ddg.find.var.uses.rec(obj[[3]]))))
              }
              # If assigning to an expression (like a[b]), recurse on the
              # indexing part of the lvalue as well as on the expression.
              else {
                filter(c (.ddg.find.var.uses.rec(obj[[2]][[3]]), unlist(.ddg.find.var.uses.rec(obj[[3]]))))
              }
            }
            
            # Not an assignment.  Recurse on all parts of the expression
            # except the operator.
            else {
              filter(unlist(lapply(obj[1:length(obj)], .ddg.find.var.uses.rec)))
            },
        error = function(e) {
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

.ddg.find.simple.assign <- function(obj) {
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

# lvalue - a parsed expression.

.ddg.get.var <- function(lvalue) {
  if (is.symbol(lvalue)) deparse(lvalue)
  else .ddg.get.var(lvalue[[2]])
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
  if (is.symbol(expr) || !is.language(expr)) return (FALSE)
  if (is.null(expr[[1]]) || !is.language(expr[[1]])) return (FALSE)
  return (expr[[1]] == "function")
}

# .ddg.add.annotations accepts and returns a parsed command.
# The returned command is annotated as needed.

.ddg.add.annotations <- function(parsed.command, annotate.functions) {
  # Return if statement is empty.
  if (length(parsed.command) == 0) return(parsed.command)
  
  # Replace source with ddg.source.
  if (is.call(parsed.command) && parsed.command[[1]] == "source") {
    return(.ddg.add.ddg.source(parsed.command))
  }
  
  # Annotate user-defined functions.
  # Note that this will not annotate anonymous functions, like ones that might be passed to lapply, for example
  # Is that what we want?
  if (annotate.functions && .ddg.is.assign(parsed.command) && .ddg.is.functiondecl(parsed.command[[3]])) {
    return(.ddg.add.function.annotations(parsed.command))
  }
  
  # Add other annotations here.
  
  # No annotation required.
  return(parsed.command)
}

# .ddg.add.ddg.source replaces source with ddg.source
#
# parsed.source.call must be a parsed expression that is a call
# to the source function

.ddg.add.ddg.source <- function(parsed.source.call) {
  script.name <- deparse(parsed.source.call[[2]])
  new.command.txt <- paste("ddg.source(", script.name, ")", sep="")
  parsed.ddg.source.call <- parse(text=new.command.txt)
  return(parsed.ddg.source.call[[1]])
}

# .ddg.add.function.annotations accepts and returns a parsed command.
# If the command is a function declaration, calls to ddg.function, ddg.eval
# and ddg.return.value are added, if not already present. Otherwise the
# command is returned unchanged. The functions ddg.annotate.on and
# ddg.annotate.off may be used to provide a list of functions to annotate
# or not to annotate, respectively.
#
# parsed.function.decl should be an assignment statement where the value
# being bound is a function declaration

.ddg.add.function.annotations <- function(parsed.function.decl) {
  # Get function name.
  func.name <- toString(parsed.function.decl[[2]])
  
  # Return if a list of functions to annotate is provided and this
  # function is not on the list.
  if (!is.null(.ddg.annotate.on()) & !(func.name %in% .ddg.annotate.on())) return(parsed.function.decl)
  
  # Return if a list of functions not to annotate is provided and this
  # function is on the list.
  else if (!is.null(.ddg.annotate.off()) & func.name %in% .ddg.annotate.off()) return(parsed.function.decl)
  
  # Add function annotations.
  else {
    # Get function definition.
    func.definition <- parsed.function.decl[[3]]
    
    # Create function block if necessary.
    if (func.definition[[3]][[1]] != "{") {
      func.definition <- .ddg.create.function.block(func.definition)
    }
    
    # Insert call to ddg.function if not already added.
    if (!.ddg.has.call.to(func.definition, "ddg.function")) {
      func.definition <- .ddg.insert.ddg.function(func.definition)
    }
    
    # Insert calls to ddg.return.value if not already added.
    if (!.ddg.has.call.to(func.definition, "ddg.return.value")) {
      func.definition <- .ddg.wrap.all.return.parameters(func.definition)
    }
    
    # Wrap last statement with ddg.return.value if not already added
    # and if last statement is not a simple return or a ddg function.
    last.statement <- .ddg.find.last.statement(func.definition)
    if (!.ddg.is.call.to(last.statement, "ddg.return.value") & !.ddg.is.call.to(last.statement, "return") & !.ddg.is.call.to.ddg.function(last.statement)) {
      func.definition <- .ddg.wrap.last.line(func.definition)
    }
    
    # Wrap statements with ddg.eval if not already added and if
    # statements are not calls to a ddg function and do not contain
    # ddg.return.value.
    if (!.ddg.has.call.to(func.definition, "ddg.eval")) {
      func.definition <- .ddg.wrap.with.ddg.eval(func.definition)
    }
    
    # Reassemble parsed.command.
#    func.definition.txt <- deparse(func.definition)
#    annotated.funcdecl.txt <- paste(c(paste(func.name, "<-", sep=" "), func.definition.txt, collapse="\n"))
#    annotated.funcdecl.parsed <- parse(text=annotated.funcdecl.txt)
#    # Return modified parsed command
#    return(annotated.funcdecl.parsed)
    
    return (call ("<-", as.name(func.name), func.definition))
  }
}

# .ddg.create.function.block creates a function block.

.ddg.create.function.block <- function(func.definition) {
  # Get the function parameters.
  func.params <- func.definition[[2]]
  
  # Get the body of the function.
  func.body <- func.definition[[3]]
  
  # Add block and reconstruct the call.
  new.func.body <- call("{", func.body)
  return(call("function", func.params, as.call(new.func.body)))
}

# .ddg.insert.ddg.function inserts ddg.function before the first line
# in a function body.

.ddg.insert.ddg.function <- function(func.definition) {
  # Get the function parameters.
  func.params <- func.definition[[2]]
  
  # Get the body of the function.
  func.body <- func.definition[[3]]
  
  pos <- length (func.body)
  
  # If the function body contains a single statement, insert
  # ddg.function and reconstruct the call.
  if (pos == 2) {
    inserted.statement <- call("ddg.function")
    new.statements <- c(as.list(func.body[1]), inserted.statement, as.list(func.body[2]))
    return(call("function", func.params, as.call(new.statements)))
  }
  
  # If the function body contains more than one statement, insert
  # ddg.function and reconstruct the call.
  else {
    inserted.statement <- call("ddg.function")
    new.statements <- c(as.list(func.body[1]), inserted.statement, as.list(func.body[2:pos]))
    return(call("function", func.params, as.call(new.statements)))
  }
}

# .ddg.wrap.return.parameters wraps parameters of return functions
# with ddg.return.value in a function body.

.ddg.wrap.return.parameters <- function(func.body) {
  pos <- length(func.body)
  # Check each statement in the function body to see if it
  # contains a return.
  #
  for (i in 1:pos) {
    statement <- func.body[[i]]
    if (.ddg.has.call.to(statement, "return")) {
      
      # If statement is a return, wrap parameters with ddg.return.value.
      if (.ddg.is.call.to(statement, "return")) {
        # Need to handle empty parameter separately.
        if (length(statement) == 1) {
          ret.params <- ""
        } else {
          ret.params <- statement[[2]]
        }
        
        # If parameters contain a return, recurse on parameters.
        if (.ddg.has.call.to(ret.params, "return")) {
          ret.params <- .ddg.wrap.return.parameters(ret.params)
        }
        
        new.ret.params <- call("ddg.return.value", ret.params)
        new.statement <- call("return", new.ret.params)
        func.body[[i]] <- new.statement
        
        # If statement contains a return, recurse on statement.
      } else {
        func.body[[i]] <- .ddg.wrap.return.parameters(statement)
      }
    }
  }
  return(func.body)
}

# .ddg.wrap.all.return.parameters wraps parameters of all return
# functions with ddg.return.value in a function definition.

.ddg.wrap.all.return.parameters <- function(func.definition) {
  # Get function parameters.
  func.params <- func.definition[[2]]
  
  # Get the body of the function.
  func.body <- func.definition[[3]]
  
  # Wrap individual return functions.
  new.func.body <- .ddg.wrap.return.parameters(func.body)
  
  # Reconstruct function.
  return(call("function", func.params, as.call(new.func.body)))
}

# .ddg.wrap.last.line wraps the last line of a function with
# ddg.return.value.

.ddg.wrap.last.line <- function(func.definition) {
  # Get function parameters.
  func.params <- func.definition[[2]]
  
  # Get the body of the function.
  func.body <- func.definition[[3]]
  
  # Check to see if the function body is a block.
  pos <- length (func.body)
  
  # If the function body contains a single statement, wrap that
  # statement and reconstruct the call.
  if (pos == 2) {
    last.statement <- func.body[[pos]]
    wrapped.statement <- call ("ddg.return.value", last.statement)
    new.func.body <- call("{", wrapped.statement)
    return(call("function", func.params, new.func.body))
  }
  
  # If the function body contains more than one statement, find the
  # last statement, wrap it, and reconstruct the call.
  else {
    last.statement <- func.body[[pos]]
    wrapped.statement <- call ("ddg.return.value", last.statement)
    new.statements <- c(as.list(func.body[2:pos-1]), wrapped.statement)
    return(call("function", func.params, as.call(new.statements)))
  }
}

# .ddg.wrap.with.ddg.eval wraps each statement in a function body
# with ddg.eval if the statement is not a call to a ddg function and
# does not contain a call to ddg.return.value. The statement is enclosed
# in quotation marks.

.ddg.wrap.with.ddg.eval <- function(func.definition) {
  # Get the function parameters.
  func.params <- func.definition[[2]]
  
  # Get the body of the function.
  func.body <- func.definition[[3]]
  
  pos <- length(func.body)
  
  # Process each statement in the function body
  for (i in 2:pos) {
    # Wrap with ddg.eval if statement is not a call to a ddg function and
    # does not contain a call to ddg.return.value. Enclose statement in
    # quotation marks.
    statement <- func.body[[i]]
    if (!grepl("^ddg.", statement[1]) & !.ddg.has.call.to(statement, "ddg.return.value")) {
      new.statement <- call("ddg.eval", deparse(statement))
      func.body[[i]] <- new.statement
    }
  }
  
  # Reassemble function definition.
  func.definition <- call("function", func.params, as.call(func.body))
  
  return(func.definition)
}

# .ddg.find.last.statement finds the last statement of a function.

.ddg.find.last.statement <- function (func.definition) {
  # Get function body.
  func.body <- func.definition[[3]]
  # Check to see if the function body is a block.
  if (func.body[[1]] == "{") {
    # Return the last statement in the block.
    pos <- length(func.body)
    return(func.body[[pos]])
  }
  # Not a block. Return the single statement that is the body.
  else {
    return(func.body)
  }
}

# .ddg.is.call.to returns TRUE if the parsed expression passed
# in is a call to the specified function.

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

.ddg.has.call.to <- function(parsed.expr, func.name) {
  # Base case.
  if (!is.recursive(parsed.expr)) return(FALSE)
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

.ddg.reads.file <- function (parsed.statement) {
  .ddg.file.read.functions.df <- .ddg.get (".ddg.file.read.functions.df")
  reading.functions <- .ddg.file.read.functions.df$function.names
  return (TRUE %in% (lapply (reading.functions, function(fun.name) {return (.ddg.has.call.to(parsed.statement, fun.name))})))
}

.ddg.writes.file <- function (parsed.statement) {
  .ddg.file.write.functions.df <- .ddg.get (".ddg.file.write.functions.df")
  writing.functions <- .ddg.file.write.functions.df$function.names
  return (TRUE %in% (lapply (writing.functions, function(fun.name) {return (.ddg.has.call.to(parsed.statement, fun.name))})))
}

.ddg.creates.graphics <- function (parsed.statement) {
  .ddg.graphics.functions.df <- .ddg.get (".ddg.graphics.functions.df")
  graphics.functions <- .ddg.graphics.functions.df$function.names
  return (TRUE %in% (lapply (graphics.functions, function(fun.name) {return (.ddg.has.call.to(parsed.statement, fun.name))})))
}

