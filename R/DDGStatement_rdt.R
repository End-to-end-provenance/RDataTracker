# Extension of DDGStatement that keeps track of individual statements
# within functions.
setClass("RDTStatement",
    slots = list(
        contained = "list"         # If this is a function declaration, this will 
                                   # be a list of DDGStatement objects for the 
                                   # statements it contains.
        ),
    contains = "DDGStatement")

# This is called when a new RDTStatement is created.  It initializes all of the slots.
methods::setMethod ("initialize",
    "RDTStatement",
    function(.Object, parsed, pos, script.name, script.num, parseData){
      .Object <- methods::callNextMethod(.Object, parsed, pos, script.num)
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
      return(.Object)
    }
)

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
  
  return (methods::new (Class = "RDTStatement", parsed = expr, pos, script.name, 
          script.num, parseData))
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
  else if (.ddg.max.loops() == 0) {
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

#' .ddg.ensure.in.block ensures that if there is a singleton statement inside a control 
#' construct it is enclosed in a block
#' @param block input statement(s)
#' @return a block containing the statement(s)

.ddg.ensure.in.block <- function(block) {
  if (is.symbol(block) || block[[1]] != "{") call("{", block)
  else block
}
