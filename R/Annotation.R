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

#' ddg.function creates a procedure node of type Operation for
#' procedures implemented as functions in the original R script.
#' The function name and input parameters are obtained automatically
#' from the calling environment. The outs parameters may be used
#' optionally to create output data nodes. These MUST be passed as
#' a list of strings, not names, unless the value is a file name.
#' Users can right-click on the procedure node in DDG Explorer to
#' see the code for the function in the original script. For more
#' details on outs parameters, see .ddg.create.output.nodes.
#' 
#' The outs parameters should be a list of names of data nodes to be created as
#' outputs to this procedure node. These MUST be passed as
#' a list of strings, not names, unless the value is a file name.
#' 
#' @param outs.graphic (optional) the name of a snapshot node to be used as a
#'    file name.  A graphical snapshot is simply a captured image
#'    of the graphic device active at the time of the call to
#'    ddg.function or ddg.procedure.
#' @param outs.data (optional) a list of names of data nodes.
#' @param outs.exception (optional) a list of names of exception nodes.
#' @param outs.url (optional) a list of names of url nodes.
#' @param outs.file (optional) a list of names of file nodes. Supported file
#'   extensions include: .csv, .jpg, .jpeg, .pdf, and .txt.
#' @param graphic.fext (optional) the file extension for a graphics file, defaults to jpeg.
#' @return nothing
#' @export
ddg.function <- function(outs.graphic=NULL, outs.data=NULL, outs.exception=NULL, outs.url=NULL, outs.file=NULL, graphic.fext="jpeg") {
  #print("In ddg.function")
  if (!.ddg.is.init()) return(invisible())
  
  .ddg.inc(".ddg.func.depth")
  pname <- NULL
  .ddg.lookup.function.name(pname)
  
  .ddg.console.node()
  
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
  .ddg.create.function.nodes(pname, call, full.call, outs.graphic, outs.data, outs.exception, outs.url, outs.file, graphic.fext,
      env = sys.frame(.ddg.get.frame.number(sys.calls())))
  invisible()
}

#' ddg.return.value creates a data node for a function's return value. If
#' the function is called from a console command and console mode is
#' enabled, a data flow edge will be created linking this node to
#' the console command that uses the value. ddg.return.value returns the
#' same value as the function (expr) and can be used in place of the
#' function's normal return statement(s) if it is the last statement
#' in the function.  Otherwise, it should be a parameter to return,
#' as in return(ddg.return.value(expr)). If expr is an assignment, nodes
#' and edges are created for the assignment.
#' 
#' @param expr the value returned by the function.
#' @param cmd.func the DDGStatement object for the return statement
#' @return the value that the function whose return value we are capturing 
#'    returns
ddg.return.value <- function (expr=NULL, cmd.func=NULL) {
  if (!.ddg.is.init()) return(expr)
  
  #print("In ddg.return.value")
  
  parsed.stmt <- NULL
  
  if (!is.null(cmd.func)) {
    parsed.stmt <- cmd.func()
  }
  
  # If expr is an assignment, create nodes and edges for the assignment.
  orig.expr <- substitute(expr)
  #print(paste("ddg.return.value: expr =", paste(deparse(orig.expr), collapse="\n")))
  
  frame.num <- .ddg.get.frame.number(sys.calls())
  env <- sys.frame(frame.num)
  
  orig.return <- paste("return(", deparse(orig.expr), ")", sep="")
  
  pname <- NULL
  .ddg.lookup.function.name(pname)
  #print(paste("ddg.return.value: pname =", pname))
  
  # If this is a recursive call to ddg.return.value, find
  # the caller of the first ddg.return.value
  if (grepl("(^ddg|.ddg)", pname)) {
    #print("ddg.return.value: Found a recursive call")
    caller.frame <- .ddg.find.ddg.return.value.caller.frame.number ()
    pname <- as.character(sys.call(caller.frame)[[1]])
    #print(paste("ddg.return.value: updated pname =", pname))
  }
  else {
    #print("ddg.return.value: NOT a recursive call")
    caller.frame <- -1
  }
  
  # Prints the call & arguments.
  # expr forces evaluation of the function early.  I think that
  # causes some examples to work with debugging on but not off.
  # Checking.  (6/26/2015 - Barb).
  # Yes, ReturnTest.R fails on the recursive f5 function
  #print(paste("ddg.return.value:", sys.call(caller.frame))) #, "returns", expr))
  
  # If this is not a recursive call to ddg.return.value and
  # ddg.function was not called, create the function nodes that
  # it would have created.
  call <- sys.call(caller.frame)
  if (!.ddg.proc.node.exists(pname)) {
    #print("ddg.return.value creating function nodes")
    full.call <- match.call(sys.function(caller.frame), call=call)
    .ddg.create.function.nodes(pname, call, full.call, env = sys.frame(.ddg.get.frame.number(sys.calls()))
    )
  }
  else {
    #print("ddg.return.value decrementing func.depth")
    .ddg.dec (".ddg.func.depth")
  }
  
  if (is.null(cmd.func)) {
    #print("ddg.return.value constructing DDG statement for the return call")
    return.stmt <- .ddg.construct.DDGStatement (parse(text=orig.return), pos=NA, script.num=NA)
  }
  else {
    #print("ddg.return.value using existing DDG statement for the return call")
    return.stmt <- cmd.func()
    parsed.statement <- return.stmt@parsed
    #print(paste("ddg.return.value: parsed.statement =", deparse(parsed.statement)))
  }
  
  # Create a data node for the return value. We want the scope of
  # the function that called the function that called ddg.return.
  call.text <- gsub(" ", "", deparse(call, nlines=1))
  return.node.name <- paste(call.text, "return")
  
  #print(paste("ddg.return.value: sys.nframe =", sys.nframe()))
  #print(paste("ddg.return.value: caller.frame =", caller.frame))
  return.node.scope <-
      environmentName (if (sys.nframe() == 2) .GlobalEnv
              else parent.env(sys.frame(caller.frame)))
  #print(paste("ddg.return.value: return.node.scope =", return.node.scope))
  .ddg.save.data(return.node.name, expr, scope=return.node.scope)
  
  # Create a return proc node
  
  caller.env = sys.frame(caller.frame)
  
  # Check if there is a return call within this call to ddg.return.
  if (.ddg.has.call.to(parsed.stmt, "return")) {
    .ddg.proc.node("Operation", return.stmt@abbrev, return.stmt@abbrev, console = TRUE, cmd=return.stmt)
    
    # Create control flow edge from preceding procedure node.
    .ddg.proc2proc()
    
    # Create an edge from the return statement to its return value.
    .ddg.proc2data(return.stmt@abbrev, return.node.name, return.node.scope, return.value=TRUE)
  }
  else {
    .ddg.lastproc2data(return.node.name, dscope=return.node.scope)
  }
  .ddg.add.to.return.values (call.text)
  
  # If it does not have return, then its parameter was a call to ddg.eval
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
          env <- .ddg.where( var, env = parent.env(parent.frame()) , warning = FALSE )
          
          if( identical(env,"undefined") )
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
  #print("ddg.return.value: creating finish node")
  if (typeof(call[[1]]) == "closure") {
    .ddg.add.finish.node (node.name=pname)
  }
  else {
    .ddg.add.finish.node (node.name=paste(deparse(call),collapse=""))
  }
  
  #print(paste ("ddg.return.value: returning", expr))
  return(expr)
}

#' .ddg.find.ddg.return.value.caller.frame.number returns the frame
#' number of the first caller to ddg.return.value.  If ddg.return.value
#' is called recursively, this will give us the position of the
#' earliest one called.
#' 
#' @returnType integer
#' @return the frame number of the user function that called ddg.return.value 
.ddg.find.ddg.return.value.caller.frame.number <- function() {
  # Get the stack
  calls <- sys.calls()
  
  # Find the calls to ddg.return.value
  ddg.funcs <- unlist(lapply (calls, function (call) return (grepl("^ddg|.ddg", deparse(call)[[1]]))))
  calls.to.ddg.return.value <- unlist(lapply(calls, function (call) return(.ddg.is.call.to(call, as.name("ddg.return.value")))))
  non.ddg.calls.to.ddg.return.value <- !(ddg.funcs[1:length(ddg.funcs)-1]) & calls.to.ddg.return.value[2:length(calls.to.ddg.return.value)]
  which.frame <- Position (function (call) return (call), non.ddg.calls.to.ddg.return.value, right=TRUE)
  
  # Return the frame number of the caller to ddg.return.value
  return (which.frame)
}

#' @returnType logical
#' @return true if we should be annotating inside control constructs
.ddg.annotate.inside <- function() {
  return(.ddg.get("ddg.annotate.inside"))
}

#' Inserts an operational node called "Details Omitted"
#' in cases where not all iterations of a loop are annotated.  This may
#' happen if the number of the first loop to be annotaed (first.loop) is
#' greater than 1 and/or if the total number of loops to be annotated is
#' less than the actual number of iterations.
#'
#' It also sets a variable to remember that the last construct is incomplete
#' so that the right data nodes get created.
#' 
#' NOTE:  This might be useful outside of the context of loops, but is
#' currently only used within loops.
#' 
#' @return nothing 
ddg.details.omitted <- function() {
  pnode.name <- "Details Omitted"
  .ddg.proc.node("Incomplete", pnode.name, pnode.name)
  .ddg.proc2proc()
  .ddg.set.details.omitted(TRUE)
  
  if (.ddg.debug.lib()) {
    print("Adding Details Omitted node")
  }
}

#' @returnType logical
#' @return true if we should run the annotated version of a function and
#' false if we should run the unannotated version.
#' 
ddg.should.run.annotated <- function (func.name) {
  
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

#' ddg.eval evaluates a statement and creates data flow edges from
#' variable and function return nodes that are used in the
#' statement. If the statement is an assignment statement, it also
#' creates a data node for the variable assigned and a corresponding
#' data flow edge. If ddg.eval is called from inside a function, cmd.func
#' is a function that returns the corresponding DDGStatement object.
#' If ddg.eval is called from inside a control block, cmd.func is an
#' integer that points to the corresponding DDGStatement object stored
#' in the list .ddg.statements.
#' 
#' @param statement a string version of the statement to evaluate.
#' @param cmd.func the corresponding DDGStatement if inside a function,
#'    or an integer identifying the position of the statement in a list
#'    if inside a control construct
ddg.eval <- function(statement, cmd.func=NULL) {
  
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
  
  if (.ddg.debug.lib()) print (paste("ddg.eval: statement =", statement))
  
  frame.num <- .ddg.get.frame.number(sys.calls())
  env <- sys.frame(frame.num)
  
  if (!.ddg.is.init()) {
    return(eval(parsed.statement, env))
  }
  
  if (!.ddg.enable.source()) {
    .ddg.console.node()
  }
  
  # If break statement, create procedure node and close open start nodes.
  if (!is.null(cmd) && cmd@text == "break") {
    .ddg.break.statement()
  }
  
  # If next statement, create procedure node and close open start nodes.
  if (!is.null(cmd) && cmd@text == "next") {
    .ddg.next.statement()
  }
  
  return.value <- .ddg.parse.commands(parsed.statement, environ=env, run.commands = TRUE, node.name=statement, called.from.ddg.eval=TRUE, cmds=list(cmd))
  
  if (.ddg.get(".ddg.func.depth")) {
    if (!is.null(cmd)) {
      .ddg.link.function.returns(cmd)
    }
  }
  
  return (return.value)
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
    return
  }
  
  # Create start node for the calling statement if one is not already created.
  frame.number <- .ddg.get.frame.number(sys.calls())
  env <- sys.frame(frame.number)
  call <- sys.call(frame.number)
  .ddg.create.start.for.cur.cmd (env)
  
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
  
  # ddg.finish is added to the end of blocks.  We want the block to
  # return the value of the last R statement.
  return(.ddg.get (".ddg.last.R.value"))
}

.ddg.start.ddg.explorer <- function () {
  jar.path<- "/RDataTracker/java/DDGExplorer.jar"
  check.library.paths<- file.exists(paste(.libPaths(),jar.path,sep = ""))
  index<- min(which(check.library.paths == TRUE))
  ddgexplorer_path<- paste(.libPaths()[index],jar.path,sep = "")
  ddgjson.path<- paste(.ddg.path() ,"ddg.json",sep = "/")
  # ddgjson.path<- paste(getwd(), .ddg.path() ,"ddg.json",sep = "/")
  
  # -s flag starts DDG Explorer as a server.  This allows each new ddg to show
  # up in a new tab of an existing running DDG Explorer.
  # print("Starting DDG Explorer server")
  systemResult <- system2("java", c("-jar", ddgexplorer_path, ddgjson.path, "-port", .ddg.get(".ddg.explorer.port")), wait = FALSE)
  # print(paste("Starting java server return code:", systemResult))
}

# ddg.display loads & displays the current DDG.

ddg.display <- function () {
  
  # See if the server is already running
  # print("Opening socket connection")
  tryCatch ({
        con <- socketConnection(host= "localhost", port = .ddg.get(".ddg.explorer.port"), blocking = FALSE,
            server=FALSE, open="w", timeout=1)
        ddgjson.path<- paste(.ddg.path() ,"ddg.json",sep = "/")
        # ddgjson.path<- paste(getwd(), .ddg.path() ,"ddg.json",sep = "/")
        # print ("Socket open; writing to socket")
        writeLines(ddgjson.path, con)
        # print ("Wrote to socket")
        close(con)
      },
      warning = function(e) {
        # print("Warning!")
        .ddg.start.ddg.explorer()
      }
  )
  
  invisible()
}

# ddg.debug.lib.on turns on debugging of DDG construction.

ddg.debug.lib.on <- function () {
  .ddg.set("ddg.debug.lib", TRUE)
}

# ddg.debug.lib.off turns off debugging of DDG construction.

ddg.debug.lib.off <- function () {
  .ddg.set("ddg.debug.lib", FALSE)
}

# ddg.set.detail sets the level of provenance detail to be collected.
# If ddg.detail is not set, the values of annotate.inside, max.loops,
# and max.snapshot.size passed to ddg.run are used instead.

#   0 = no internal annotation, no snapshots.
#   1 = 1 loop, snapshots < 10k.
#   2 = 10 loops, snapshots < 100k.
#   3 = all loops, all snapshots.

ddg.set.detail <- function(detail.level) {
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

# ddg.detail returns the current level of provenance detail.

ddg.get.detail <- function() {
  if (!.ddg.is.set("ddg.detail")) .ddg.set("ddg.detail", NULL)
  return(.ddg.get("ddg.detail"))
}

# ddg.clear.detail clears the current value of provenance detail.

ddg.clear.detail <- function() {
  .ddg.set("ddg.detail", NULL)
}

# ddg.annotate.on enables annotation for the specified functions. Functions
# not on this list are not annotated.
# 
# If fnames is NULL, all functions will be annotated

# fnames - a list of one or more function names passed in as strings.

ddg.annotate.on <- function (fnames=NULL){
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
  off.list <- .ddg.get("ddg.annotate.off")
  off.list <- Filter (function(off) !(off %in% fnames), off.list)
  .ddg.set("ddg.annotate.off", off.list) 
  
}

# ddg.annotate.off disables annotation for the specified functions.
# Functions not on this list are annotated.
# 
# If fnames is NULL, no functions will be annotated
#
# fnames - a list of one or more function names passed in as strings.

ddg.annotate.off <- function (fnames=NULL) {
  if (is.null(fnames)) {
    .ddg.set("ddg.annotate.on", vector())
    .ddg.set("ddg.annotate.inside", FALSE)
    return()
  }
  
  # Add to the off list
  off.list <- .ddg.get("ddg.annotate.off")
  off.list <- union (off.list, fnames)
  .ddg.set("ddg.annotate.off", off.list)
  
  # Remove from the on list
  on.list <- .ddg.get("ddg.annotate.on")
  on.list <- Filter (function(on) !(on %in% fnames), on.list)
  .ddg.set("ddg.annotate.on", on.list) 
  
}

# ddg.flush.ddg removes all files from the DDG directories unless the
#   the DDG directory is the working directory. If no DDG directory is
#   specified, the current DDG directory is assumed.

# ddg.path (optional) - path to DDG directory.

ddg.flush.ddg <- function(ddg.path=NULL) {
  # Use current DDG directories if no directory is specified.
  if (is.null(ddg.path)) {
    ddg.path <- .ddg.path()
    ddg.path.data <- .ddg.path.data()
    ddg.path.debug <- .ddg.path.debug()
    ddg.path.scripts <- .ddg.path.scripts()
  }
  
  # Remove files unless the DDG directory is the working directory.
  if (ddg.path != getwd()) {
    unlink(paste(ddg.path, "*.*", sep="/"))
    unlink(paste(ddg.path.data, "*.*", sep="/"))
    unlink(paste(ddg.path.data, ".ddghistory", sep="/"))
    unlink(paste(ddg.path.debug, "*.*", sep="/"))
    unlink(paste(ddg.path.scripts, "*.*", sep="/"))
  }
  
  invisible()
}

# ddg.checkpoint prompts the user to source DDGCheckpoint.R.

# checkpoint.name (optional) - the value associated with the checkpoint
#   procedure node.

# ddg.checkpoint <- function(checkpoint.name=NULL) {
#   stop("Call source(DDGCheckpoint.R to load ddg.checkpoint and ddg.restore")
# }

# ddg.restore prompts the user to source DDGCheckpoint.R.

# file.path - the name of the checkpoint file to restore.

# ddg.restore <- function(file.path) {
#   stop("Call source(DDGCheckpoint.R to load ddg.checkpoint and ddg.restore")
# }
