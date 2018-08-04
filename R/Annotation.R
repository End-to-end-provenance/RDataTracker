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

######################## Annotation.R ########################

# This file contains functions that are called within annotations
# that are added to collect provenance within functions and 
# control constructs.
#
# It also contains helper functions that are used only for
# annotations internal to functions and control constructs.

#--------------------USER FUNCTIONS-----------------------#

#' .ddg.function creates a procedure node of type Operation for
#' procedures implemented as functions in the original R script.
#' The function name and input parameters are obtained automatically
#' from the calling environment. The outs parameters may be used
#' optionally to create output data nodes. These MUST be passed as
#' a list of strings, not names, unless the value is a file name.
#' Users can right-click on the procedure node in DDG Explorer to
#' see the code for the function in the original script. For more
#' details on outs parameters, see .ddg.create.output.nodes.
#' The outs parameters should be a list of names of data nodes to be created as
#' outputs to this procedure node. These MUST be passed as
#' a list of strings, not names, unless the value is a file name
#' (internal use only)
#' 
#' @param outs.graphic (optional) the name of a snapshot node to be used as a
#'    file name.  A graphical snapshot is simply a captured image
#'    of the graphic device active at the time of the call to
#'    .ddg.function or .ddg.procedure.
#' @param outs.data (optional) a list of names of data nodes.
#' @param outs.exception (optional) a list of names of exception nodes.
#' @param outs.url (optional) a list of names of url nodes.
#' @param outs.file (optional) a list of names of file nodes. Supported file
#'   extensions include: .csv, .jpg, .jpeg, .pdf, and .txt.
#' @param graphic.fext (optional) the file extension for a graphics file, defaults to jpeg.
#' @return nothing

.ddg.function <- function(outs.graphic=NULL, outs.data=NULL, 
                         outs.exception=NULL, outs.url=NULL, outs.file=NULL, 
                         graphic.fext="jpeg") {
  #print("In .ddg.function")
  if (!.ddg.is.init()) return(invisible())
  
  .ddg.inc(".ddg.func.depth")
  pname <- NULL
  .ddg.lookup.function.name(pname)
    
  # Look up input parameters from calling environment.
  call <- sys.call(-1)
  
  # Try to find the full call so that we can bind the parameters
  # by name in the DDG.  In the case that the function being executed
  # has been passed as a parameter to another function and is being
  # called from the context (for example, with lapply and other higher-order
  # functions), the match.call will fail.  In that case, we will use the
  # call as it appears in side the higher-order function.
  full.call <- tryCatch (match.call(sys.function(-1), call=call),
      error = function(e) call)
  
  # Create start node for the calling statement if one is not already created.
  .ddg.create.start.for.cur.cmd (call)
  .ddg.create.function.nodes(pname, call, full.call, outs.graphic, outs.data, 
                          outs.exception, outs.url, outs.file, graphic.fext,
                          env = sys.frame(.ddg.get.frame.number(sys.calls())))
  invisible()
}

#' .ddg.return.value creates a data node for a function's return value. If
#' the function is called from a console command and console mode is
#' enabled, a data flow edge will be created linking this node to
#' the console command that uses the value. .ddg.return.value returns the
#' same value as the function (expr) and can be used in place of the
#' function's normal return statement(s) if it is the last statement
#' in the function.  Otherwise, it should be a parameter to return,
#' as in return(.ddg.return.value(expr)). If expr is an assignment, nodes
#' and edges are created for the assignment (internal use only)
#' 
#' @param expr the value returned by the function.
#' @param cmd.func the DDGStatement object for the return statement
#' @return the value that the function whose return value we are capturing 
#'    returns

.ddg.return.value <- function (expr=NULL, cmd.func=NULL) {
  if (!.ddg.is.init()) return(expr)
  
  #print("In .ddg.return.value")
  
  parsed.stmt <- NULL
  
  if (!is.null(cmd.func)) {
    parsed.stmt <- cmd.func()
  }
  
  # If expr is an assignment, create nodes and edges for the assignment.
  orig.expr <- substitute(expr)
  #print(paste(".ddg.return.value: expr =", paste(deparse(orig.expr), 
  #            collapse="\n")))
  
  frame.num <- .ddg.get.frame.number(sys.calls())
  env <- sys.frame(frame.num)
  
  orig.return <- paste("return(", deparse(orig.expr), ")", sep="")
  
  pname <- NULL
  .ddg.lookup.function.name(pname)
  #print(paste(".ddg.return.value: pname =", pname))
  
  # If this is a recursive call to .ddg.return.value, find
  # the caller of the first .ddg.return.value
  if (grepl("^ddg|^.ddg|^prov", pname)) {
    #print(".ddg.return.value: Found a recursive call")
    caller.frame <- .ddg.find.ddg.return.value.caller.frame.number ()
    pname <- as.character(sys.call(caller.frame)[[1]])
    #print(paste(".ddg.return.value: updated pname =", pname))
  }
  else {
    #print(".ddg.return.value: NOT a recursive call")
    caller.frame <- -1
  }
  
  # Prints the call & arguments.
  # expr forces evaluation of the function early.  I think that
  # causes some examples to work with debugging on but not off.
  # Checking.  (6/26/2015 - Barb).
  # Yes, ReturnTest.R fails on the recursive f5 function
  #print(paste(".ddg.return.value:", sys.call(caller.frame))) #, "returns", 
  #            expr))
  
  # If this is not a recursive call to .ddg.return.value and
  # .ddg.function was not called, create the function nodes that
  # it would have created.
  call <- sys.call(caller.frame)
  if (!.ddg.proc.node.exists(pname)) {
    #print(".ddg.return.value creating function nodes")
    full.call <- match.call(sys.function(caller.frame), call=call)
    .ddg.create.function.nodes(pname, call, full.call, 
                               env = sys.frame(.ddg.get.frame.number(sys.calls()))
    )
  }
  else {
    #print(".ddg.return.value decrementing func.depth")
    .ddg.dec (".ddg.func.depth")
  }
  
  if (is.null(cmd.func)) {
    #print(".ddg.return.value constructing DDG statement for the return call")
    return.stmt <- .ddg.construct.DDGStatement (parse(text=orig.return), 
                                                pos=NA, script.num=NA)
  }
  else {
    #print(".ddg.return.value using existing DDG statement for the return call")
    return.stmt <- cmd.func()
  }
  
  # Create a data node for the return value. We want the scope of
  # the function that called the function that called ddg.return.
  call.text <- gsub(" ", "", deparse(call, nlines=1))
  return.node.name <- paste(call.text, "return")
  
  #print(paste(".ddg.return.value: sys.nframe =", sys.nframe()))
  #print(paste(".ddg.return.value: caller.frame =", caller.frame))
  return.node.scope <-
      environmentName (if (sys.nframe() == 2) .GlobalEnv
              else parent.env(sys.frame(caller.frame)))
  #print(paste(".ddg.return.value: return.node.scope =", return.node.scope))
  .ddg.save.data(return.node.name, expr, scope=return.node.scope)
  
  # Check if there is a return call within this call to ddg.return.
  if (.ddg.has.call.to(parsed.stmt, "return")) {
    .ddg.proc.node("Operation", return.stmt@abbrev, return.stmt@abbrev, cmd=return.stmt)
    
    # Create control flow edge from preceding procedure node.
    .ddg.proc2proc()
    
    # Create an edge from the return statement to its return value.
    .ddg.proc2data(return.stmt@abbrev, return.node.name, return.node.scope, 
                   return.value=TRUE)
  }
  else {
    .ddg.lastproc2data(return.node.name, dscope=return.node.scope)
  }
  .ddg.add.to.return.values (call.text)
  
  # If it does not have return, then its parameter was a call to .ddg.eval
  # and this stuff has been done already.
  if (.ddg.has.call.to(parsed.stmt, "return")) {
    # Create edges from variables used in the return statement
    vars.used <- return.stmt@vars.used
    for (var in vars.used) {
      # Make sure there is a node we could connect to.
      scope <- .ddg.get.scope(var)
      if (.ddg.data.node.exists(var, scope)) {
        .ddg.data2proc(var, scope, return.stmt@abbrev)
      }
    }
    
    for (var in return.stmt@vars.set)
    {
      if (var != "")
      {
        # Create output data node.
        dvalue <- eval(as.symbol(var), envir=env)
        
        # Check for non-local assignment
        if ( .ddg.is.nonlocal.assign(return.stmt@parsed[[1]]) )
        {
          env <- .ddg.where( var, env = parent.env(parent.frame()), warning = FALSE )
          
          if ( identical(env, "undefined") )
            env <- globalenv()
        }
        
        dscope <- .ddg.get.scope(var, env=env)
        .ddg.save.data(var, dvalue, scope=dscope)
        
        # Create an edge from procedure node to data node.
        .ddg.proc2data(return.stmt@abbrev, var, dscope=dscope, return.value=FALSE)
      }
    }
    
    
    # Create nodes and edges dealing with reading and writing files
    .ddg.create.file.read.nodes.and.edges()
    .ddg.create.file.write.nodes.and.edges ()
    .ddg.create.graphics.nodes.and.edges ()
    
  }
  
  # Create the finish node for the function
  #print(".ddg.return.value: creating finish node")
  .ddg.add.finish.node()
  
  #print(paste (".ddg.return.value: returning", expr))
  return(expr)
}

#' .ddg.find.ddg.return.value.caller.frame.number returns the frame
#' number of the first caller to .ddg.return.value.  If .ddg.return.value
#' is called recursively, this will give us the position of the
#' earliest one called.
#' 
#' @return the frame number of the user function that called .ddg.return.value 

.ddg.find.ddg.return.value.caller.frame.number <- function() {
  # Get the stack
  calls <- sys.calls()
  
  # Find the calls to .ddg.return.value
  ddg.funcs <- unlist(lapply (calls, 
    function (call) return (grepl("^ddg|^.ddg|^prov", deparse(call)[[1]]))))
  calls.to.ddg.return.value <- unlist(lapply(calls, 
    function (call) 
      return(.ddg.is.call.to(call, as.name(".ddg.return.value")))))
  non.ddg.calls.to.ddg.return.value <- !(ddg.funcs[1:length(ddg.funcs)-1]) & 
    calls.to.ddg.return.value[2:length(calls.to.ddg.return.value)]
  which.frame <- Position (function (call) return (call), 
                           non.ddg.calls.to.ddg.return.value, right=TRUE)
  
  # Return the frame number of the caller to .ddg.return.value
  return (which.frame)
}

#' .ddg.annotate.inside returns True if we should be annotating inside 
#' control constructs
#' @return true if annotating inside control constructs

.ddg.annotate.inside <- function() {
  return(.ddg.get("ddg.annotate.inside"))
}

#' .ddg.details.omitted inserts an operational node called "Details Omitted"
#' in cases where not all iterations of a loop are annotated.  This may
#' happen if the number of the first loop to be annotaed (first.loop) is
#' greater than 1 and/or if the total number of loops to be annotated is
#' less than the actual number of iterations.
#' It also sets a variable to remember that the last construct is incomplete
#' so that the right data nodes get created.
#' NOTE:  This might be useful outside of the context of loops, but is
#' currently only used within loops (internal use only)
#' 
#' @return nothing

.ddg.details.omitted <- function() {
  pnode.name <- "Details Omitted"
  .ddg.proc.node("Incomplete", pnode.name, pnode.name)
  .ddg.proc2proc()
  .ddg.set.details.omitted(TRUE)
  
  if (.ddg.debug.lib()) {
    print("Adding Details Omitted node")
  }
}

#' .ddg.should.run.annotated returns True if we should run the annotated
#' version of a function (internal use only)
#' @param func.name name of function
#' @return True if we should run annotated version

.ddg.should.run.annotated <- function (func.name) {
  
  # Check if we are in a loop and loop annotations are off
  if (!.ddg.loop.annotate() && .ddg.inside.loop() > 0) return (FALSE)
  
  # Make sure this specific function has not been disabled
  if (!is.null(.ddg.annotate.off()) & func.name %in% .ddg.annotate.off()) return(FALSE)
  
  # Not annotating functions in general
  # Check if this specific function should be annotated
  if (!is.null(.ddg.annotate.on()) & func.name %in% .ddg.annotate.on()) return(TRUE)
  
  # If we do not know anything specific about this function, follow the 
  # general rule
  return (.ddg.annotate.inside()) 
}

#' .ddg.eval evaluates a statement and creates data flow edges from
#' variable and function return nodes that are used in the
#' statement. If the statement is an assignment statement, it also
#' creates a data node for the variable assigned and a corresponding
#' data flow edge. If .ddg.eval is called from inside a function, cmd.func
#' is a function that returns the corresponding DDGStatement object.
#' If .ddg.eval is called from inside a control block, cmd.func is an
#' integer that points to the corresponding DDGStatement object stored
#' in the list .ddg.statements (internal use only)
#' 
#' @param statement a string version of the statement to evaluate.
#' @param cmd.func the corresponding DDGStatement if inside a function,
#'    or an integer identifying the position of the statement in a list
#'    if inside a control construct

.ddg.eval <- function(statement, cmd.func=NULL) {
  
  # Statement at top level.
  if (is.null(cmd.func)) {
    parsed.statement <- parse(text=statement)
    cmd <- NULL
  }
  
  # Statement inside control block.
  else if (is.numeric(cmd.func)) {
    num <- cmd.func
    cmd <- .ddg.statement(num)
    parsed.statement <- cmd@parsed
  } 
  
  # Statement inside function.
  else {
    cmd <- cmd.func()
    parsed.statement <- cmd@parsed
  }
  
  if (.ddg.debug.lib()) print (paste(".ddg.eval: statement =", statement))
  
  frame.num <- .ddg.get.frame.number(sys.calls())
  env <- sys.frame(frame.num)
  
  if (!.ddg.is.init()) {
    return(eval(parsed.statement, env))
  }
  
  # If break statement, create procedure node and close open start nodes.
  if (!is.null(cmd) && cmd@text == "break") {
    .ddg.break.statement()
  }
  
  # If next statement, create procedure node and close open start nodes.
  if (!is.null(cmd) && cmd@text == "next") {
    .ddg.next.statement()
  }
  
  return.value <- .ddg.parse.commands(parsed.statement, environ=env, 
                                      run.commands = TRUE, 
                                      called.from.ddg.eval=TRUE, 
                                      cmds=list(cmd))
  
  if (.ddg.get(".ddg.func.depth")) {
    if (!is.null(cmd)) {
      .ddg.link.function.returns(cmd)
    }
  }
  
  return (return.value)
}

#' prov.start creates a procedure node of type Start called pname.
#' Users can right-click on a start node in DDG Explorer and see
#' the code between start and finish nodes in the original script.
#' @param pname the label for the node.  This can be passed as
#' a string or as a name.
#' @export

prov.start <- function(pname=NULL) {
  if (!.ddg.is.init()) return(invisible())
  
  .ddg.lookup.function.name(pname)
  
  # Check for NULL.
  if (is.null(pname)) {
    msg <- "Cannot call prov.start with NULL value from top-level."
    .ddg.insert.error.message(msg)
    return
  }
  
  # Create start node for the calling statement if one is not already created.
  frame.number <- .ddg.get.frame.number(sys.calls())
  env <- sys.frame(frame.number)
  .ddg.create.start.for.cur.cmd (env)
  
  # Create start non-operational step.
  .ddg.proc.node("Start", pname, pname)
  
  # Create control flow edge from preceding procedure node.
  .ddg.proc2proc()
  
}

#' prov.finish creates a procedure node of type Finish called pname.
#' Users can right-click on a finish node in DDG Explorer and see
#' the code between start and finish nodes in the original script.
#' @param pname the label for the node. This can be passed as
#' a string or as a name. It can be omitted if prov.finish is called
#' by a function, in which case the name of the function will be used.
#' @export

prov.finish <- function(pname=NULL) {
  if (!.ddg.is.init()) return(invisible())
  
  .ddg.lookup.function.name(pname)
  
  # Check for NULL.
  if (is.null(pname)) {
    msg <- "Cannot call prov.finish with NULL value from top-level."
    .ddg.insert.error.message (msg)
  }
  
  # Create finish non-operational step.
  .ddg.proc.node("Finish", pname, pname)
  
  # Create control flow edge from preceding procedure node.
  .ddg.proc2proc()
  
  # prov.finish is added to the end of blocks.  We want the block to
  # return the value of the last R statement.
  return(.ddg.get (".ddg.last.R.value"))
}

#' prov.annotate.on enables annotation for the specified functions. Functions not on
#' this list are not annotated. If fnames is NULL, all functions will be annotated.
#' @param fnames - a list of one or more function names passed in as strings.
#' @export

prov.annotate.on <- function (fnames=NULL){
  if (is.null(fnames)) {
    .ddg.set("ddg.annotate.off", vector())
    .ddg.set("ddg.annotate.inside", TRUE)
    return()
  }
  
  # Add to the on list
  on.list <- .ddg.get("ddg.annotate.on")
  on.list <- union (on.list, fnames)
  .ddg.set("ddg.annotate.on", on.list)
  
  # Remove from the off list
  off.list <- .ddg.annotate.off()
  off.list <- Filter (function(off) !(off %in% fnames), off.list)
  .ddg.set("ddg.annotate.off", off.list) 
  
}

#' prov.annotate.off disables annotation for the specified functions. Functions not on 
#' this list are annotated. If fnames is NULL, no functions will be annotated.
#' @param fnames a list of one or more function names passed in as strings.
#' @export

prov.annotate.off <- function (fnames=NULL) {
  if (is.null(fnames)) {
    .ddg.set("ddg.annotate.on", vector())
    .ddg.set("ddg.annotate.inside", FALSE)
    return()
  }
  
  # Add to the off list
  off.list <- .ddg.annotate.off()
  off.list <- union (off.list, fnames)
  .ddg.set("ddg.annotate.off", off.list)
  
  # Remove from the on list
  on.list <- .ddg.annotate.on()
  on.list <- Filter (function(on) !(on %in% fnames), on.list)
  .ddg.set("ddg.annotate.on", on.list) 
  
}

#' .ddg.save.annotated.script saves a copy of the annotated script to
#' the debug directory.
#' @param cmds set of parsed commands
#' @param script.name name of script
#' @return nothing

.ddg.save.annotated.script <- function(cmds, script.name) {
  for (i in 1:length(cmds)) {
    expr <- cmds[[i]]@annotated
    for (j in 1:length(expr)) {
      line <- deparse(expr[[j]])
      if (i == 1 && j == 1) script <- line else script <- append(script, line)
    }
  }
  
  fileout <- file(paste(.ddg.path.debug(), "/", "annotated-", script.name, sep=""))
  write(script, fileout)
  close(fileout)
}

#' .ddg.add.annotations accepts a DDGStatement and returns an expression.
#' The returned expression is annotated as needed.
#' @param command a DDGStatement
#' @return an expression with annotations

.ddg.add.annotations <- function(command) {
  #print("In .ddg.add.annotations")
  #print(paste("command@text =", command@text))
  parsed.command <- command@parsed[[1]]
  
  # Return if statement is empty.
  if (length(parsed.command) == 0) return(command@parsed)
  
  # Replace source with prov.source.
  if (is.call(parsed.command) && parsed.command[[1]] == "source") {
    return(.ddg.add.prov.source(parsed.command, command))
  }
  
  # Annotate user-defined functions.
  # Note that this will not annotate anonymous functions, like ones that might 
  # be passed to lapply, for example.  Is that what we want?
  if (.ddg.is.assign(parsed.command) && .ddg.is.functiondecl(parsed.command[[3]])) {
    return(.ddg.add.function.annotations(command))
  }
  
  statement.type <- .ddg.get.statement.type(parsed.command)
  loop.types <- list("for", "while", "repeat")
  
  # Move into funcs below && prov.max.loops() > 0) {
  if (length(statement.type > 0) && !is.null(statement.type)) { 
    
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

#' .ddg.add.prov.source replaces source with prov.source.
#' @param parsed.command a parsed expression that is a call to the source function.
#' @param command the DDGStatement object for the source call
#' @return a parsed expression with source replaced by prov.source

.ddg.add.prov.source <- function(parsed.command, cmd) {
  script.name <- deparse(parsed.command[[2]])
  parsed.command.txt <- paste("prov.source(", script.name, 
                              ", calling.script =", cmd@script.num, 
                              ", startLine=", cmd@pos@startLine,
                              ", startCol=", cmd@pos@startCol, 
                              ", endLine=", cmd@pos@endLine, 
                              ",endCol=", cmd@pos@endCol, ")", sep="")
  return(parse(text=parsed.command.txt))
}

#' .ddg.add.function.annotations is passed a command that corresponds
#' to a function declaration.  It returns a parsed command corresponding
#' to the same function declaration but with calls to .ddg.function,
#' .ddg.eval and .ddg.return.value inserted if they are not already present.
#' The functions prov.annotate.on and prov.annotate.off may be used to provide
#' a list of functions to annotate or not to annotate, respectively.
#' @param function.decl a command that contains an assignment statement where the value
#' being bound is a function declaration
#' @return a parsed command with annotations added

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
  
  # Insert call to .ddg.function if not already added.
  if (!.ddg.has.call.to(func.definition[[3]], ".ddg.function")) {
    func.definition <- .ddg.insert.ddg.function(func.definition)
  }
  
  # Insert calls to .ddg.return.value if not already added.
  if (!.ddg.has.call.to(func.definition[[3]], ".ddg.return.value")) {
    func.definition <- .ddg.wrap.all.return.parameters(func.definition, 
                                                       function.decl@contained)
  }
  
  # Wrap last statement with .ddg.return.value if not already added
  # and if last statement is not a simple return or a ddg function.
  last.statement <- .ddg.find.last.statement(func.definition)
  
  if (!.ddg.is.call.to(last.statement, ".ddg.return.value") & 
      !.ddg.is.call.to(last.statement, "return") & 
      !.ddg.is.call.to.ddg.function(last.statement)) {
    func.definition <- .ddg.wrap.last.line(func.definition, function.decl@contained)
  }
  
  # Wrap statements with .ddg.eval if not already added and if
  # statements are not calls to a ddg function and do not contain
  # .ddg.return.value.
  if (!.ddg.has.call.to(func.definition, ".ddg.eval")) {
    func.definition <- .ddg.wrap.with.ddg.eval(func.definition, function.decl@contained)
  }
  
  # Reassemble parsed.command.
  #print(paste("Done annotating", func.name))
  return (as.expression (call ("<-", as.name(func.name), func.definition)))
}

#' .ddg.create.function.block creates a function block.
#' @param func.definition a parsed expression for a function declaration (not
#'   the full assignment statement in which it is declared)
#' @return a parse tree for the same function declaration but with
#' the function statements inside a block.

.ddg.create.function.block <- function(func.definition) {
  # Get the function parameters.
  func.params <- func.definition[[2]]
  
  # Get the body of the function.
  func.body <- func.definition[[3]]
  
  # Add block and reconstruct the call.
  new.func.body <- call("{", func.body)
  return(call("function", func.params, as.call(new.func.body)))
}

#' .ddg.add.conditional.statement creates a new function definition
#' containing an if-then statement used to control annotation.
#' @param func.definition the original function definition.
#' @return the function definition with annotations added

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
      c(paste("if (.ddg.should.run.annotated(\"", func.name, "\")) {", sep=""),
          as.list(func.body[2:pos]),
          paste("} else {", sep=""),
          as.list(func.body[2:pos]),
          paste("}", sep=""))
  
  new.func.expr <- parse(text=new.func.body.txt)
  new.func.body <- new.func.expr[[1]]
  
  return(call("function", func.params, call("{", new.func.body)))
}

#' .ddg.insert.ddg.function inserts .ddg.function before the first line
#' in the annotated block of a function body.
#' @param func.definition a parsed expression for a function declaration (not
#'   the full assignment statement in which it is declared)
#' @return a parse tree for the same function declaration but with
#' a call to .ddg.function as the first statement.

.ddg.insert.ddg.function <- function(func.definition) {
  # Get the function parameters.
  func.params <- func.definition[[2]]
  
  # Get the body of the function.
  func.body <- func.definition[[3]]
  
  # Get annotated block.
  block <- func.body[[2]][[3]]
  pos <- length(block)
  
  # Insert .ddg.function.
  inserted.statement <- call(".ddg.function")
  new.statements.txt <- c(as.list("{"), inserted.statement, 
                          as.list(block[2:pos]), as.list("}"))
  block <- parse(text=new.statements.txt)[[1]]
  
  func.body[[2]][[3]] <- block
  
  return(call("function", func.params, as.call(func.body)))
}

#' .ddg.wrap.return.parameters wraps parameters of return functions
#' with .ddg.return.value in the annotated block of a function body.
#'
#' @param block the parse tree corresponding to the statements within
#'   the annotated block of a function
#' @param parsed.stmts the list of DDGStatement objects contained in the
#'   function
#' @return a parse tree for the same function body but with
#' a call to .ddg.return.value wrapped around all expressions that are
#' returned.

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
      
      # If statement is a return, wrap parameters with .ddg.return.value.
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
        #  print(paste(".ddg.wrap.return.parameters: parsed.stmts =", 
        #              parsed.stmts[[i]]@abbrev))
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
    #print(paste(".ddg.wrap.return.parameters: after annotation, block[[", i, 
    #            "]] =", paste(deparse(block[[i]]), collapse="\n")))
  }
  
  return(block)
}

#' .ddg.wrap.all.return.parameters wraps parameters of all return
#' functions with .ddg.return.value in the annotated block of a function
#' definition.
#' @param func.definition a parsed expression for a function declaration (not
#'   the full assignment statement in which it is declared)
#' @param parsed.stmts the list of DDGStatement objects contained in the
#'   function
#' @return a parse tree for the same function declaration but with
#' a call to .ddg.return.value wrapped around all expressions that are
#' returned.

.ddg.wrap.all.return.parameters <- function(func.definition, parsed.stmts) {
  # Get function parameters.
  func.params <- func.definition[[2]]
  
  # Get the body of the function.
  func.body <- func.definition[[3]]
  
  # Get annotated block.
  block <- func.body[[2]][[3]]
  
  # Wrap individual return functions.
  block <- .ddg.wrap.return.parameters(block, parsed.stmts)
  
  # Get new function body
  func.body[[2]][[3]] <- block
  
  # Reconstruct function.
  return(call("function", func.params, as.call(func.body)))
}

#' .ddg.find.last.statement finds the last statement in the annotated
#' block of a function.
#' @param func.definition a parsed expression for a function declaration (not
#'   the full assignment statement in which it is declared)
#' @return the parse tree corresponding to the last statement in the
#' function definition.

.ddg.find.last.statement <- function (func.definition) {
  # Get function body.
  func.body <- func.definition[[3]]
  
  # Get annotated block.
  block <- func.body[[2]][[3]]
  pos <- length(block)
  
  # Return final statement in block.
  return(block[[pos]])
}

#' .ddg.wrap.last.line wraps the last line of the annotated block of a
#' function with .ddg.return.value.
#' @param func.definition a parsed expression for a function declaration (not
#'   the full assignment statement in which it is declared)
#' @param parsed.stmts the list of DDGStatement objects contained in the
#'   function
#' @return a parse tree for the same function declaration but with
#' a call to .ddg.return.value wrapped around the last line in the body.

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

#' Creates a call to .ddg.return.value using a closure so that we
#' will be able to refer to the correct DDGStatement object when the
#' return call is executed.
#' @param last.statement the parse tree for the expression being returned
#' @param parsed.stmt the DDGStatement object corresponding to the last statement
#' @return a parse tree with a call to .ddg.return.value.  The arguments to
#' .ddg.return.value are the parsed statement and the DDGStatement object.

.ddg.create.ddg.return.call <- function (last.statement, parsed.stmt) {
  # We need to force the evaluation of parsed.stmt for the closure to
  # return the value that parsed.stmt has at the time the .ddg.eval
  # call is created.
  force(parsed.stmt)
  #print(paste(".ddg.create.ddg.return.call: parsed.stmt =", parsed.stmt@abbrev))
  #print(paste(".ddg.create.ddg.return.call: last.statement =", last.statement))
  if (.ddg.has.call.to(last.statement, "return")) {
    #print(".ddg.create.ddg.return.call: has call to return")
    return (call (".ddg.return.value", last.statement, function() parsed.stmt))
  }
  else {
    #print(".ddg.create.ddg.return.call: NO call to return")
    
    # If there is no return call, we will use .ddg.eval to execute the
    # statement and then .ddg.return.value to create the necessary return
    # structure.  We cannot use this technique if there is a return call
    # because we if tried to eval a return call, we would end up returning
    # from some code inside RDT, instead of the user's function.
    new.statement <- .ddg.create.ddg.eval.call(last.statement, parsed.stmt)
    return (call (".ddg.return.value", new.statement, function() parsed.stmt))
  }
}

#' .ddg.wrap.with.ddg.eval wraps each statement in the annotated block
#' of a function body with .ddg.eval if the statement is not a call to a ddg
#' function and does not contain a call to .ddg.return.value. The statement
#' is enclosed in quotation marks.
#' @param func.definition a parsed expression for a function declaration (not
#'   the full assignment statement in which it is declared)
#' @param parsed.stmts the list of DDGStatement objects contained in the
#'   function
#' @return a parse tree for the same function declaration but with
#' the calls to .ddg.eval inserted.

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
    # Wrap with .ddg.eval if statement is not a call to a ddg function and
    # does not contain a call to .ddg.return.value. Enclose statement in
    # quotation marks.
    statement <- block[[i]]
    if (!grepl("^ddg", statement[1]) && !grepl("^.ddg", statement[1]) && !grepl("^prov", statement[1]) &&
        !.ddg.has.call.to(statement, ".ddg.return.value")) {
      parsed.stmt <- parsed.stmts[[i-2]]
      new.statement <- .ddg.create.ddg.eval.call(statement, parsed.stmt)
      func.body[[2]][[3]][[i]] <- new.statement
    }
  }
  
  return(call("function", func.params, as.call(func.body)))
}

#' .ddg.annotate.if.statement adds annotations to if statements.
#' @param command original parsed command
#' @return parsed command with annotations added

.ddg.annotate.if.statement <- function(command) {
  #print(paste(".ddg.annotate.if.statement annotating", command@text))
  if (prov.max.loops() == 0) {
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
    while (!is.symbol(parent) && parent[[1]] == "if") {
      # Get block
      block <- parent[[3]]
      block <- .ddg.ensure.in.block(block)
      
      # Get statements for this block.
      block.stmts<- list ()
      for (i in 1:(length(block)-1)) {
        block.stmts <- c(block.stmts, parsed.stmts[[i+ptr]])
      }
      
      # Advance pointer for next block.
      ptr <- ptr + length(block) - 1
      
      # Wrap each statement with .ddg.eval.
      block <- .ddg.wrap.block.with.ddg.eval(block, block.stmts)
      
      # Add start and finish nodes.
      block <- .ddg.add.block.start.finish(block, "if")
      
      # Reconstruct original statement.
      cond <- paste(deparse(parent[[2]]), collapse="")
      if (bnum == 1) {
        statement.txt <- paste(c(paste("if (", cond, ")", sep=""), 
                                 deparse(block), collapse="\n"))
      } else {
        statement.txt <- paste(c(paste("} else if (", cond, ")", sep=""), 
                                 deparse(block), collapse="\n"))
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
      
      # Wrap each statement with .ddg.eval.
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
      append(parsed.command.txt, ".ddg.set.inside.loop()", after = 0)
  parsed.command.txt <-
      append(parsed.command.txt, ".ddg.not.inside.loop()")
  
  #print(paste(".ddg.annotate.if.statement annotated version:", parsed.command.txt))
  return(parse(text=parsed.command.txt))
}

#' .ddg.annotate.loop.statement adds annotations to for, while and repeat
#' statements. Provenance is collected for the number of iterations
#' specified in the parameter max.loops, beginning with the iteration
#' specified in the parameter first.loop. A Details Omitted node may be
#' added before and after the annotated section, as needed.
#' @param command original parsed command
#' @param loop.type loop type (for, while, repeat)
#' @return parsed command with annotationa added

.ddg.annotate.loop.statement <- function(command, loop.type) {
  if (prov.max.loops() == 0) {
    # Note that I can't just use command@text because it does not separate 
    # statements with newlines
    parsed.command.txt <- deparse(command@parsed[[1]])
  }
  
  else  {
    
    # Get parsed command
    parsed.command <- command@parsed[[1]]
    
    # Add new loop & get loop number.
    ddg.loop.num <- .ddg.add.loop()
    
    
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
    
    # Wrap each statement with .ddg.eval.
    annotated.block <- .ddg.wrap.block.with.ddg.eval(block, command@contained)
    
    # Insert .ddg.forloop statement.
    if (loop.type == "for") {
      index.var <- parsed.command[[2]]
      annotated.block <- .ddg.insert.ddg.forloop(annotated.block, index.var)
    }
    
    # Add start and finish nodes.
    annotated.block <- .ddg.add.block.start.finish(annotated.block, 
                                                   paste(loop.type, "loop"))
    
    # Insert ddg.loop.annotate statements.
    block <- .ddg.insert.ddg.loop.annotate(block, "off")
    
    # Reconstruct for statement.
    block.txt <- deparse(block)
    annotated.block.txt <- deparse(annotated.block)
    
    # Calculate the control line of the annotated code
    if (loop.type == "for") {
      firstLine <- paste("for (", deparse(parsed.command[[2]]), " in ", 
                         deparse(parsed.command[[3]]), ") {", sep="")
    }
    else if (loop.type == "while") {
      firstLine <- paste("while (", deparse(parsed.command[[2]]), ") {", sep="")
    }
    else {  # repeat
      firstLine <- paste("repeat {", sep="")
    }
    
    parsed.command.txt <- paste(c(firstLine,
            paste("if (.ddg.loop.count.inc(", ddg.loop.num, 
                  ") >= .ddg.first.loop() && .ddg.loop.count(", ddg.loop.num, 
                  ") <= .ddg.first.loop() + prov.max.loops() - 1)", sep=""),
            annotated.block.txt,
            paste("else", sep = ""),
            block.txt,
            paste("}", sep=""),
            paste("if (.ddg.loop.count(", ddg.loop.num, 
                  ") > .ddg.first.loop() + prov.max.loops() - 1)",
                  " .ddg.details.omitted()", sep=""),
            paste(".ddg.reset.loop.count(", ddg.loop.num, ")", sep=""),
            
            # Turn loop annotations back on in case we reached the max.
            paste("if (prov.max.loops() != 0) .ddg.loop.annotate.on()"),  
            collapse="\n"))
  }
  
  parsed.command.txt <- 
      append(parsed.command.txt, ".ddg.set.inside.loop()", after = 0)
  parsed.command.txt <-
      append(parsed.command.txt, ".ddg.not.inside.loop()")
  
  #print(parse(text=parsed.command.txt))
  
  return(parse(text=parsed.command.txt))
}

#' .ddg.annotate.simple.block adds annotations to simple blocks.
#' @param command original parsed command
#' @return parsed command with annotations added

.ddg.annotate.simple.block <- function(command) {
  # Get parsed command
  parsed.command <- command@parsed[[1]]
  
  # Get statements in block.
  block <- parsed.command
  
  # Wrap each statement with .ddg.eval.
  block <- .ddg.wrap.block.with.ddg.eval(block, command@contained)
  
  # Add start and finish nodes.
  block <- .ddg.add.block.start.finish(block, "block")
  
  # Reconstruct block.
  block.txt <- deparse(block)
  
  return(parse(text=block.txt))
}

#' .ddg.is.call.to.ddg.function returns TRUE if the parsed expression
#' passed in is a call to a ddg function.
#' @param parsed.expr a parse tree
#' @return True if a call to a ddg function

.ddg.is.call.to.ddg.function <- function(parsed.expr) {
  # Check if a function call.
  if (is.call(parsed.expr)) {
    # Check if the function called is a ddg function.
    if (grepl("^ddg|^.ddg|^prov", parsed.expr[1])) {
      return (TRUE)
    }
  }
  return (FALSE)
}

#' Creates a call to .ddg.eval using a closure so that we
#' will be able to refer to the correct DDGStatement object when the
#' return call is executed.
#' @param statement the parse tree for the expression being returned
#' @param parsed.stmt the DDGStatement object corresponding to the last statement
#' @return a parse tree with a call to .ddg.eval.  The arguments to
#' .ddg.eval are the original statement and the DDGStatement object.

.ddg.create.ddg.eval.call <- function (statement, parsed.stmt) {
  # We need to force the evaluation of parsed.stmt for the closure to
  # return the value that parsed.stmt has at the time the .ddg.eval
  # call is created.
  force(parsed.stmt)
  
  return (call(".ddg.eval", paste(deparse(statement), collapse=""), 
               function() parsed.stmt))
}

#' Creates a call to .ddg.eval using the number of the DDGStatement
#' stored in the list ddg.statements in the ddg environment.
#' @param statement the parse tree for the expression being returned and
#' @param parsed.stmt the corresponding DDGStatement object.
#' @return a parse tree with a call to .ddg.eval.  The arguments to
#' .ddg.eval are the original statement and the number of the DDGStatement object.

.ddg.create.block.ddg.eval.call <- function (statement, parsed.stmt) {
  # Get the next DDGStatement number and store parsed.stmt at this location.
  .ddg.inc("ddg.statement.num")
  num <- .ddg.statement.num()
  .ddg.add.ddgstatement(parsed.stmt)
  
  return (call(".ddg.eval", paste(deparse(statement), collapse=""), num))
}

#'.ddg.wrap.block.with.ddg.eval wraps each statement in a block with
#' .ddg.eval unless the statement is a ddg function or contains a call
#' to .ddg.return.value.
#' @param block original block
#' @param parsed.stmts parsed statements in block
#' @return block with annotations added

.ddg.wrap.block.with.ddg.eval <- function(block, parsed.stmts) {
  # Ignore initial brace.
  for (i in 2:length(block)) {
    # Enclose statement in quotation marks and wrap with .ddg.eval.
    statement <- block[[i]]
    if (!grepl("^ddg", statement) && !grepl("^.ddg", statement) && !grepl("^prov", statement) &&
      !.ddg.has.call.to(statement, ".ddg.return.value")) {
      parsed.stmt <- parsed.stmts[[i-1]]
      # print(statement)
      # print(parsed.stmt@text)
      
      new.statement <- .ddg.create.block.ddg.eval.call(statement, parsed.stmt)
      block[[i]] <- new.statement
    }
  }
  return(block)
}

#' .ddg.add.block.start.finish adds start and finish nodes to blocks in control
#' statements.
#' @param block original block
#' @param pname label for start and finish nodes
#' @return block with annotations added

.ddg.add.block.start.finish <- function(block, pname) {
  # Create prov.start & prov.finish statements.
  start.statement <- deparse(call("prov.start", pname))
  finish.statement <- deparse(call("prov.finish", pname))
  
  # Get internal statements.
  pos <- length(block)
  statements <- deparse(block[[2]])
  if (pos > 2) {
    for (i in 3:pos) {
      statements <- append(statements, deparse(block[[i]]))
    }
  }
  
  # Create new block.
  block.txt <- paste(c("{", start.statement, statements, finish.statement, "}"), 
                     collapse="\n")
  block.parsed <- parse(text=block.txt)
  
  return(block.parsed[[1]])
}

#' .ddg.insert.ddg.forloop inserts a .ddg.forloop statement at the top of a block.
#' @param block original block
#' @param index.var index variable
#' @return block with annotations added

.ddg.insert.ddg.forloop <- function(block, index.var) {
  pos <- length(block)
  inserted.statement <- call(".ddg.forloop", index.var)
  
  # Block with single statement.
  if (pos == 2) {
    new.statements <- c(as.list(block[[1]]),  inserted.statement, 
                        as.list(block[2]))
    return(as.call(new.statements))
  }
  
  # Block with multiple statements.
  else {
    new.statements <- c(as.list(block[[1]]), inserted.statement,
                        as.list(block[2:pos]))
    return(as.call(new.statements))
  }
}

#' .ddg.insert.ddg.loop.annotate inserts a .ddg.loop.annotate.on or
#' .ddg.loop.annotate.off statement at the beginning of a block.
#' @param block original block
#' @param var whether loop annotation is on or off
#' @return block with annotations added

.ddg.insert.ddg.loop.annotate <- function(block, var) {
  pos <- length(block)
  if (var == "on") inserted.statement <- call(".ddg.loop.annotate.on")
  else if (var == "off") inserted.statement <- call(".ddg.loop.annotate.off")
  
  # Block with single statement.
  if (pos == 2) {
    new.statements <- c(as.list(block[[1]]),  inserted.statement, 
                        as.list(block[2]))
    return(as.call(new.statements))
  }
  
  # Block with multiple statements.
  else {
    new.statements <- c(as.list(block[[1]]), inserted.statement, 
                        as.list(block[2:pos]))
    return(as.call(new.statements))
  }
}

#' prov.set.detail sets the level of provenance detail to be collected.
#' If ddg.detail is not set, the values of annotate.inside, max.loops,
#' and max.snapshot.size passed to prov.run are used instead.
#' 0 = no internal annotation, no snapshots.
#' 1 = 1 loop, snapshots < 10k.
#' 2 = 10 loops, snapshots < 100k.
#' 3 = all loops, all snapshots.
#' @param detail.level level of detail to set (0-3)
#' @return nothing
#' @export

prov.set.detail <- function(detail.level) {
  if (detail.level == 0) {
    .ddg.set("ddg.annotate.inside", FALSE)
    .ddg.set("ddg.max.loops", 0)
    .ddg.set("ddg.max.snapshot.size", 0)
    .ddg.set("ddg.detail", 0)
  } else if (detail.level == 1) {
    .ddg.set("ddg.annotate.inside", TRUE)
    .ddg.set("ddg.max.loops", 1)
    .ddg.set("ddg.max.snapshot.size", 10)
    .ddg.set("ddg.detail", 1)
  } else if (detail.level == 2) {
    .ddg.set("ddg.annotate.inside", TRUE)
    .ddg.set("ddg.max.loops", 10)
    .ddg.set("ddg.max.snapshot.size", 100)
    .ddg.set("ddg.detail", 2)
  } else if (detail.level == 3) {
    .ddg.set("ddg.annotate.inside", TRUE)
    .ddg.set("ddg.max.loops", 10^10)
    .ddg.set("ddg.max.snapshot.size", -1)
    .ddg.set("ddg.detail", 3)
  } else {
    print("0 = no internal annotation, no snapshots")
    print("1 = 1 loop, snapshots < 10k")
    print("2 = 10 loops, snapshots < 100k")
    print("3 = all loops, all snapshots")
  }
}

#' ddg.detail returns the current level of provenance detail.
#' @return the current level of detail (0-3)
#' @export 

prov.get.detail <- function() {
  if (!.ddg.is.set("ddg.detail")) .ddg.set("ddg.detail", NULL)
  return(.ddg.get("ddg.detail"))
}

#' prov.clear.detail clears the current value of provenance detail.
#' @return nothing
#' @export

prov.clear.detail <- function() {
  .ddg.set("ddg.detail", NULL)
}
