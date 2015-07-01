# It assumes the caller has already found the function text.  When integrated into RDataTracker, it is much more likely that we would have the parsed function.
# It does not add the ddg.return.value calls yet.

#library("RDataTracker")
#source (("/Users/blerner/Documents/Process/DataProvenance/RDataTracker-9.R"))
#ddg.run("FunctionAnnotationTest.R")

options(warn=2)
f1 <- function () { 
  a <-1
  b <- 2 
}

f2 <- function (x) {
  return (x + 1)
}

f3 <- function (x) {
  if (x > 0) return (x + 1)
  else return (x - 1)
}

f4 <- function () return (1)

f5 <- function (x, y) {
  return (x + y)
}

f6 <- function () {
  returned <- TRUE
}

f7 <- function () {
  x <- "return"
}

f8 <- function () {
  returned <- TRUE
  return (returned)
}

f9 <- function(x) {  
  return(2*return(x^2)) 
}

f10 <- function(x) {
  if (x > 0) return (x + 1)
}

x <- 2:6
lapply (x, function(num) {ddg.function(); ddg.return.value(num+1)})

insert.ddg.function <- function (function.text) {
  print ("Original function")
  print (function.text)
  
  # Parse the function declaration
  function.parsed <- parse(text=function.text)
  
  # Get the expression that corresponds to the parameters of the function
  function.params <- function.parsed[[1]][[2]]
  if (is.null (function.params)) {
    function.params.text = ""
  }
  else {
    function.params.text = paste0 (names(function.params), collapse=", ")
  }
  
  # Get the expression that corresponds to the body of the function
  function.body <- function.parsed[[1]][[3]]
  
  # Turn the body back into text
  function.body.text <- deparse (function.body)
  
  # Check to see if the function body is a block
  if (function.body[[1]] == "{") {
    # Insert the call to ddg.function()
    modified.function.body.text <- append (function.body.text, "    ddg.function()", after=1)
  }
  
  # Not a block.  Insert the { }
  else {
    # Insert the call to ddg.function()
    modified.function.body.text <- c ("{\n    ddg.function () ", paste0("    ", function.body.text), "}")
  }
  
  # Recreate the function text
  function.header <- paste0 ("function (", function.params.text, ")")
  modified.function.text <- paste (c (function.header, modified.function.body.text), collapse="\n")
  
  # Return the modified function text
  return (modified.function.text)
}

is.call.to <- function (parsed.expr, func.name) {
#  if (is.expression (parsed.expr)) {
#    parsed.expr <- parsed.expr[[1]]
#  }
  
  # Check if it is a function call
  if (is.call(parsed.expr)) {
    # Check that the function called is the desired function
    if (parsed.expr[[1]] == func.name) {
      return (TRUE)
    }
  }
  return (FALSE)
}

# Returns true if the expression passed in contains the type of function 
# call requested
has.call.to <- function (obj, func.name) {
  # Base case.
  if (!is.recursive(obj)) return(FALSE)
  
  # Return statement.
  if (is.call.to(obj, func.name)) {
    return (TRUE)
  } 
  
  # Not a return statement.  Recurse on the parts of the expression.
  else {
    return (any(sapply(obj, function(obj) {return(has.call.to(obj, func.name))})))
  }
  
}

# Determine if an expression is a return statement
is.return.statement <- function (parsed.expr) {
  return (is.call.to (parsed.expr, "return"))
}

# Returns true if the expression passed in contains a call to the return
# function
has.return.statement <- function (obj) {
  return (has.call.to (obj, "return"))
}

# Determine if an expression is a function definition
is.function.definition <- function (parsed.expr) {
  return (is.call.to (parsed.expr, "function"))  
}

has.function.definition <- function (obj) {
  return (has.call.to (obj, "function"))
}

# Helper function to recursively find all the function definitions
# in a parse tree
find.function.definitions.rec <- function (obj) {
  if (!has.function.definition (obj)) return (NULL)
  
  if (is.function.definition (obj)) {
    defs <- unlist(sapply (obj, find.function.definitions.rec))
    return (c (obj, Filter (function (obj) {return (!is.null(obj))}, defs)))
  }
  
  return (sapply (obj, find.function.definitions.rec))
}

# Returns a vector of all the function definitions in a parse tree
find.function.definitions <- function (obj) {
  defs <- unlist(find.function.definitions.rec (obj))
  return (Filter (function (obj) {return (!is.null(obj))}, defs))
}

# Helper function that recursively finds return calls in a function
find.return.calls.rec <- function (obj) {
  if (!has.return.statement (obj)) return (NULL)
  
  if (is.return.statement (obj)) {
    defs <- sapply (obj, find.return.calls.rec)
    return (c (obj, defs))
  }
  
  return (sapply (obj, find.return.calls.rec))
}

# Find all the return calls in a parse tree and return a vector of them.
find.return.calls <- function (obj) {
  return (unlist(find.return.calls.rec (obj)))
}

# Replace one return statement in a function with a call to ddg.return.value
replace.one.return.statement <- function (func.text, return.stmt) {
  return.text <- deparse (return.stmt)
  if (length(return.stmt) == 1) {
    modified.return.text <- c("return (ddg.return.value ())")
  }
  else {
    modified.return.text <- paste0("return (ddg.return.value (", deparse(return.stmt[[2]]), "))", collapse="")
  }
  #print(func.text)
  #print(return.text)
  #print (modified.return.text)
  
  return (sub (return.text, modified.return.text, func.text, fixed=TRUE))
}

# Replace all return statements in a function with calls to ddg.return.value
replace.return.statement <- function (func.def, return.stmts) {
  func.text <- paste0 (deparse (func.def), collapse="\n")
  
  # A function may have multiple return statements.  Repeat for each of them.
  for (return.stmt in return.stmts) {
    func.text <- replace.one.return.statement (func.text, return.stmt)
  }
  return (func.text)
}

# Find the last statement of a function.  
# function.parsed should be a parse tree for a function definition.
find.last.statement <- function (function.parsed) {
  # Get the expression that corresponds to the body of the function
  function.body <- function.parsed[[3]]
  
  # Check to see if the function body is a block
  if (function.body[[1]] == "{") {
    # Return the last statement in the block.
    pos <- length (function.body)
    return (function.body[[pos]])
  }
  
  # Not a block.  Return the single statement that is the body.
  else {
    return (function.body)
  }
}

wrap.last.statement <- function (function.parsed) {
  # Get the parameters
  params <- function.parsed[[2]]
  
  # Get the expression that corresponds to the body of the function
  function.body <- function.parsed[[3]]
  
  # Check to see if the function body is a block
  if (function.body[[1]] == "{") {
    pos <- length (function.body)
    
    # If the function body contains a single statement, wrap that statement
    # and reconstruct the function.
    if (pos == 2) {
      last.statement <- function.body[[pos]]
      wrapped.statement <- call ("ddg.return.value", last.statement)
      new.function.body <- call ("{", wrapped.statement)
      return (call ("function", params, new.function.body))
    }
    
    # If the function body contains more than one statement, find the
    # last statement, wrap that and reconstruct the call.
    else {
      last.statement <- function.body[[pos]]
      wrapped.statement <- call ("ddg.return.value", last.statement)
      new.statements <- c(as.list(function.body[2:pos-1]), wrapped.statement)
      return (call ("function", params, as.call(new.statements)))
    }
  }
  
  # Not a block.  Wrap the single statement that is the body and
  # reconstruct the call.
  else {
    wrapped.statement <- call ("ddg.return.value", function.body)
    return (call ("function", params, wrapped.statement))
  }
}

#replace.return.statements <- function (obj) {
  # Base case.
#  if (!is.recursive(obj)) return(obj)
  
  # Return statement.
#  if (is.return.statement(obj)) {
#    if (length(obj) == 1) {
#      return (call("ddg.return.value"))
#    }
#    return (call("ddg.return.value", obj[[2]]))
#  } 
  
  # Not a return statement.  Recurse on the parts of the expression.
  # This does not work because I end up with a simple list, not
  # a modified parse tree!
#  else {
#    return (lapply(obj, replace.return.statements))
#  } 
  
#}



# Works - only trying to insert ddg.function calls so far.
parse(text=insert.ddg.function(deparse(f1)))
parse(text=insert.ddg.function(deparse(f2)))
parse(text=insert.ddg.function(deparse(f3)))
parse(text=insert.ddg.function(deparse(f4)))
parse(text=insert.ddg.function(deparse(f5)))
parse(text=insert.ddg.function(deparse(f6)))
parse(text=insert.ddg.function(deparse(f7)))
parse(text=insert.ddg.function(deparse(f8)))


# Works
has.return.statement(parse (text=deparse(f1)))
has.return.statement(parse (text=deparse(f2)))
has.return.statement(parse (text=deparse(f3)))
has.return.statement(parse (text=deparse(f4)))
has.return.statement(parse (text=deparse(f5)))
has.return.statement(parse (text=deparse(f6)))
has.return.statement(parse (text=deparse(f7)))
has.return.statement(parse (text=deparse(f8)))

# Works
has.function.definition(parse (text=deparse(f1)))
has.function.definition(parse (text=deparse(f2)))
has.function.definition(parse (text=deparse(f3)))
has.function.definition(parse (text=deparse(f4)))
has.function.definition(parse (text=deparse(f5)))
has.function.definition(parse (text=deparse(f6)))
has.function.definition(parse (text=deparse(f7)))
has.function.definition(parse (text=deparse(f8)))
has.function.definition(parse (text="x <- 1"))
has.function.definition(parse (text="lapply (x, function(num) num+1)"))

# Works
find.function.definitions(parse (text = deparse(f1)))
find.function.definitions(parse (text="x <- 1"))
find.function.definitions(parse (text="lapply (x, function(num) num+1)"))
find.function.definitions(parse (text="lapply (x, function(num) num+1)"))

wrap.last.statement(parse (text=deparse(f1))[[1]])
wrap.last.statement(parse (text=deparse(f2))[[1]])
wrap.last.statement(parse (text=deparse(f3))[[1]])
wrap.last.statement(parse (text=deparse(f4))[[1]])
wrap.last.statement(parse (text=deparse(f5))[[1]])
wrap.last.statement(parse (text=deparse(f6))[[1]])
wrap.last.statement(parse (text=deparse(f7))[[1]])
wrap.last.statement(parse (text=deparse(f8))[[1]])
wrap.last.statement(parse (text=deparse(f9))[[1]])
wrap.last.statement(parse (text=deparse(f10))[[1]])

# Works
# Find all the functions defined in a file
defs <- find.function.definitions(parse ("FunctionAnnotationTest.R"))

# For each function, make a list of its return statements
returns <- lapply (defs, find.return.calls)

# Sample call that updates all the return calls of one function.
replace.return.statement (defs[[9]], returns[[9]])

# Find last statement of each function
lapply (defs, find.last.statement)
