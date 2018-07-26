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

#################### DDG LIBRARY FOR R ####################

# The functions in this library may be used to annotate an R script
# in order to collect provenance in the form of a data derivation
# graph (DDG) as the script executes. The DDG is saved as a JSON file
# (ddg.json) that may be viewed and queried using DDG Explorer.

# Create DDG environment variable.

.ddg.env <- new.env(parent=emptyenv())

#-------- FUNCTIONS TO MANAGE THE GLOBAL VARIABLES--------#

# Global variables cannot be used directly in a library.  Instead,
# we need to place the variables in our own environment.  These
# functions make that environment easier to use.

.onLoad <- function(libname, pkgname) {
  .ddg.init.tables()
}

#' .ddg.clear reinitializes the ddg
#' @return nothing
.ddg.clear <- function() {
  # reinitialize tables
  .ddg.init.tables()
}

##### Getters for specific variables

#' .ddg.save.debug returns True if debugging information should be saved
#' to the file system
#' @return TRUE if saving debugging information

.ddg.save.debug <- function() {
  return(.ddg.get("ddg.save.debug"))
}

#' .ddg.initial.env returns an environment containing names bound before
#'   the script was executed
#' @return an environment containing names previously bound

.ddg.initial.env <- function() {
  return(.ddg.get("ddg.initial.env"))
}

#' .ddg.annotate.on returns the names of functions that the user explicity said
#' should be annotated
#' @return the names of functions to be annotated

.ddg.annotate.on <- function() {
  return (.ddg.get("ddg.annotate.on"))
}

#' .ddg.annotate.off returns the names of functions that the user explicitly said
#' should not be annotaed
#' @return the names of functions not be annotated

.ddg.annotate.off <- function() {
  return (.ddg.get("ddg.annotate.off"))
}

#' .ddg.enable.source returns True if the commands are coming from a script file
#' @return TRUE if commands are from a script

.ddg.enable.source <- function() {
  return(.ddg.is.set("from.source") && .ddg.get("from.source"))
}

#' .ddg.set.details.omitted keeps track of whether the last loop has all iterations 
#' recorded or not.
#' @param value if TRUE, it means that not all iterations are recorded
#' @return nothing
.ddg.set.details.omitted <- function (value) {
  .ddg.set ("details.omitted", value)
}

#' .ddg.were.details.omitted returns True if provenance is incomplete at this point
#' @return TRUE if provenance is incomplete at this point
.ddg.were.details.omitted <- function () {
  .ddg.get ("details.omitted")
}

#' .ddg.set.warning is attached as a handler when we evaluate
#' expressions.  It saves the warning so that a warning
#' node can be created after the procedural node that
#' corresponds to the expression that caused the warning
#' @param w the simplewarning object created by R
#' @return nothing
.ddg.set.warning <- function(w) {
  # Only save warnings if the warn level is set to report them at all.
  # This is important because we do temporarily set the warning level
  # to avoid warnings that RDT might cause that are safe to ignore.
  # Search for calls to the option function to see where that happens.
  if (getOption("warn") >= 0) {
    .ddg.set(".ddg.warning", w)
  }
}

#' .ddg.clear.warning clears the warning 
#' @return nothing
.ddg.clear.warning <- function() {
  .ddg.set(".ddg.warning", NA)
}

#' .ddg.get.warning returns the last saved warning
#' @return the last saved warning
.ddg.get.warning <- function () {
  return (.ddg.get(".ddg.warning"))
}

#' .ddg.warning.occurred returns True if there is a currrently saved warning
#' @return true if there is currently a saved warning
.ddg.warning.occurred <- function() {
  return (.ddg.is.set(".ddg.warning") && !is.na(.ddg.get(".ddg.warning")))
}


#-------------------BASIC FUNCTIONS-----------------------#

#' .ddg.get.initial.env creates a table of non-ddg objects present in the
#' R environment before the script is executed.  This is only used for 
#' debugging.
#' @return nothing

.ddg.get.initial.env <- function() {
  e <- globalenv()
  e.ls <- ls(e, all.names=TRUE)

  not.ddg.func <- function (name) {
    return (!grepl("ddg", name) && name != ".onLoad")
  }

  x <- Filter (not.ddg.func, e.ls)

  ddg.initial.env <- data.frame(x)
  colnames(ddg.initial.env) <- "ddg.name"

  .ddg.set("ddg.initial.env", ddg.initial.env)
}


#' .ddg.init.tables creates data frames to store the initial environment,
#' procedure nodes, data nodes, edges, and function return values. 
#' It also initializes selected constants and variables.
#' Tables are used throughout provenance collection and
#' optionally saved as tab-delimited files in ddg.save.
#' @return nothing
.ddg.init.tables <- function() {
  size <- 100

  .ddg.get.initial.env()
  
  .ddg.init.proc.nodes()
  .ddg.init.data.nodes()
  .ddg.init.edges()
  .ddg.init.function.table()
  .ddg.init.return.values()
  
  # Used to control debugging output.  If already defined, don't
  # change its value.
  if (!.ddg.is.set("ddg.debug.lib")) .ddg.set("ddg.debug.lib", FALSE)

  # Used to control sourcing. If already defined, don't change
  # its value.
  if (!.ddg.is.set("from.source")) .ddg.set("from.source", FALSE)

  # Record last command from the preceding console block.
  .ddg.set(".ddg.last.cmd", NULL)
  
  # Record the current command to be opened during console execution
  # (used when executing a script using ddg.source).
  .ddg.set(".ddg.possible.last.cmd", NULL)

  # Store path of current script.
  .ddg.set("ddg.r.script.path", NULL)

  # Store path of current ddg.
  .ddg.set("ddg.path", NULL)
  
  # Functions to be annotated.
  .ddg.set("ddg.annotate.on", NULL)

  # Functions not to be annotated.
  .ddg.set("ddg.annotate.off", NULL)

  .ddg.init.sourced.scripts ()

  # Save debug files on debug directory
  .ddg.set("ddg.save.debug", FALSE)
  
  .ddg.init.statements ()
  .ddg.init.hashtable ()
  
  .ddg.set(".ddg.func.depth", 0)
  .ddg.set(".ddg.explorer.port", 6096)
  
  # Initialize the stack of commands and environments being executed in active functions
  .ddg.set(".ddg.cur.cmd.stack", vector())
  .ddg.set(".ddg.cur.expr.stack", vector())
}

#' .ddg.init.environ() sets up the filesystem and R environments for use. 
#' @return nothing
.ddg.init.environ <- function() {
  dir.create(.ddg.path(), showWarnings=FALSE)
  dir.create(.ddg.path.data(), showWarnings=FALSE)
  dir.create(.ddg.path.debug(), showWarnings=FALSE)
  dir.create(.ddg.path.scripts(), showWarnings=FALSE)
}

#' .ddg.is.init is called at the beginning of all user accessible
#' functions. It verifies that a DDG has been initialized. If it
#' hasn't, it returns FALSE.
#' @return true if provenance has been initialized
.ddg.is.init <- function() {
    # Short circuits evaluation.
    return(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized"))
}

#' .ddg.is.nonlocal.assign returns TRUE if the object passed is an
#' expression object containing a non-local assignment.
#' @param expr input expression.
#' @return TRUE if the expression is an assignment statement using the <<- operator.
.ddg.is.nonlocal.assign <- function (expr)
{
  # <<- or ->> means that the assignment is non-local
  if (is.call(expr))
  {
    # This also finds uses of ->>.
    if (identical(expr[[1]], as.name("<<-")))
      return (TRUE)
  }
  return (FALSE)
}

#' .ddg.create.empty.vars.set creates an empty data frame
#' initialized to contain information about variable assignments.
#' 
#' @return The data frame is structured as follows: \cr
#' - the variable name.\cr
#' - the position of the statement that wrote the variable last.\cr
#' - the position of the last statement that may have assigned to a
#'   variable.\cr
#' 
#' @param var.table.size desired size of the data frame. Negative values
#'   and 0 are coerced to 1.
#' 
.ddg.create.empty.vars.set <- function(var.table.size=1) {

  if (var.table.size <= 0) var.table.size <- 1

  vars.set <- data.frame(variable=character(var.table.size),
      last.writer=numeric(var.table.size),
      possible.last.writer=numeric(var.table.size),
      stringsAsFactors=FALSE)

  return(vars.set)
}

#'.ddg.double.vars.set doubles the size of a variable
#' assignment data frame and returns the new one.
#' @param vars.set data frame containing variable assignments.
#' @return a data frame that is twice the size as the original
.ddg.double.vars.set <- function(vars.set) {
  size=nrow(vars.set)
  
  # Create the right size data frame from input frame.
  new.vars.set <- rbind(vars.set,.ddg.create.empty.vars.set(size))

  return(new.vars.set)
}

#' .ddg.add.to.vars.set adds the variables set in the command
#' to the variable assignment data frame. Note that
#' var.num is a global variable! It should be intialized when
#' vars.set is first created.
#' @param vars.set variable assignment data frame.
#' @param cmd a DDGStatement object
#' @param i position of command in the list of commands
#' @return an updated vars.set data frame with the information from the command
.ddg.add.to.vars.set <- function(vars.set, cmd, i) {
  #print("In .ddg.add.to.vars.set")

  # Find out the variable being assigned to by a simple assignment
  # statement.
  main.var.assigned <- cmd@vars.set

  # Find all the variables that may be assigned in the statement.
  vars.assigned <- cmd@vars.possibly.set

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
      # Find the first empty row
      empty.rows <- which(vars.set$variable == "")
      if (length(empty.rows) == 0) {
        vars.set <- .ddg.double.vars.set(vars.set)
        empty.rows <- which(vars.set$variable == "")
      }
      var.num <- empty.rows[1]

      # Set the variable.
      vars.set$variable[var.num] <- var
      if (!is.null(main.var.assigned) && var == main.var.assigned) {
        vars.set$last.writer[var.num] <- i
      }
      else {
        vars.set$possible.last.writer[var.num] <- i
      }
    }
  }

  #print (".ddg.add.to.vars.set: returning")
  #print(vars.set)
  return(vars.set)
}


#' .ddg.find.var.assigments finds the possible variable assignments
#' for a fixed set of parsed commands. 
#' @param cmds a list of DDGStatement objects
#' @return the data frame filled in with the information from all of the commands
.ddg.find.var.assignments <- function(cmds) {
  if (length(cmds) == 0) return (data.frame())

  # Make it big so we don't run out of space.
  var.table.size <- length(cmds)
  vars.set <- .ddg.create.empty.vars.set(var.table.size)

  # Build the table recording where variables are assigned to or may
  # be assigned to.
  for ( i in 1:length(cmds)) {
    #print(paste("Looking for var assignments in", cmd.expr@abbrev))
    vars.set <- .ddg.add.to.vars.set(vars.set, cmds[[i]], i)
  }
  return (vars.set)
}


#' .ddg.create.data.use.edges creates a data flow
#' edge from the node for each variable used in cmd to the
#' procedural node labeled cmd.
#' @param cmd name of procedure node.
#' @param for.caller whether the search for the variable's scope should start at the 
#'   current stack frame, or start with its caller
#' @return nothing
.ddg.create.data.use.edges <- function (cmd, for.caller) {
  # Find all the variables used in this command.
  #print (paste(".ddg.create.data.use.edges: cmd = ", cmd@text))
  vars.used <- cmd@vars.used

  for (var in vars.used) {
    #print(paste(".ddg.create.data.use.edges: var =", var))
    # Make sure there is a node we could connect to.
    scope <- .ddg.get.scope(var, for.caller)

    #print(paste(".ddg.create.data.use.edges: scope =", scope))

    if (.ddg.data.node.exists(var, scope)) {
      # print(".ddg.create.data.use.edges found data node")
      .ddg.data2proc(var, scope, cmd@abbrev)
    }
    else {
      # print ("Did not find data node")
      # TODO - add some sort of warning that the data node was NOT
      # found.

      # error.msg <- paste("Unable to find data node for",var, ". Command", parse(text=cmd.expr), "appears to use it for procedure node", cmd, ".")
      # .ddg.insert.error.message(error.msg)
    }
  }
  #print (".ddg.create.data.use.edges Done")
}


#' .ddg.create.data.set.edges creates data nodes for 
#' variables being set, saves the data, and creates edges from the
#' procedure node that set the variable to the new data node.  These
#' nodes and edges correspond to the variables set in the command passed in.
#' @param vars.set variable assignment data frame.
#' @param cmd the command to create edges for
#' @param env environment to use for evaluating variables set.
#' @return nothing
.ddg.create.data.set.edges <- function(vars.set, cmd, env) {
  # print(paste("In .ddg.create.data.set.edges.for.cmd: cmd = ", cmd@abbrev))
  vars.assigned <- cmd@vars.set
  
  for (var in vars.assigned) {

    #print(paste(".ddg.create.data.set.edges.for.cmd: var = ", var))
    
    # Check for a new ggplot that was not assigned to a variable
    if (.ddg.get (".ddg.ggplot.created")) {
      if (var == "") {      
        # Add a data node for the plot and link it in.
        # Set .ddg.last.ggplot to the name of this node
        .ddg.data.node("Data", ".ggplot", "graph", NULL)
        .ddg.lastproc2data(".ggplot")
        .ddg.set(".ddg.last.ggplot", ".ggplot")
      }
      .ddg.set (".ddg.ggplot.created", FALSE)
    }
    
    if (var != "") {
        if (is.null(env)) {
          env <- .ddg.get.env(var)
        }
        scope <- .ddg.get.scope(var, env=env)

        # Special operators are defined by enclosing the name in `.  However,
        # the R parser drops those characters when we deparse, so when we parse
        # here they are missing and we get an error about unexpected SPECIAL
        # characters.  The first tryCatch, puts the ` back in and parses again.
        # The second tryCatch handles errors associated with evaluating the variable.
        parsed <- tryCatch(parse(text=var),
            error = function(e) parse(text=paste("`",var,"`",sep="")))
        val <- tryCatch(eval(parsed, env),
          error = function(e) {
            eval (parse(text=var), parent.env(env))
          }
        )

        tryCatch(.ddg.save.data(var, val, error=TRUE, scope=scope, env=env),
               error = function(e){.ddg.data.node("Data", var, "complex", scope); print(e)})

        .ddg.proc2data(cmd@abbrev, var, scope)
    }
  }
  

}


#' .ddg.create.data.node.for.possible.writes creates a data node for
#' each variable that might have been set in something other than a
#' simple assignment.  An edge is created from the last node in the
#' console block.
#' @param vars.set variable assignment data frame.
#' @param last.command last command in console block.
#' @param env the environment that the command was executed in
#' @return nothing

.ddg.create.data.node.for.possible.writes <- function (vars.set, last.command, env= NULL) {
  #print("In .ddg.create.data.node.for.possible.writes")
  environment <- if (is.environment(env)) env else .GlobalEnv

  for (i in 1:nrow(vars.set)) {
    # print(paste("Checking ", vars.set$variable[i]))
    if (vars.set$possible.last.writer[i] > vars.set$last.writer[i]) {
      value <- tryCatch(eval(parse(text=vars.set$variable[i]), environment),
          error = function(e) {
            #print(paste("Could not find value for", vars.set$variable[i], "in environment", environment))
            NULL
          }
      )

      # Only create the node and edge if we were successful in
      # looking up the value.
      if (!is.null(value)) {
        envName <- environmentName(environment)
        if (envName == "") envName <- .ddg.get.scope(vars.set$variable[i])
        .ddg.data.node("Data", vars.set$variable[i], value, envName)
        .ddg.proc2data(last.command@abbrev, vars.set$variable[i], envName)
      }
    }
  }
  #print("Done with .ddg.create.data.node.for.possible.writes")

}

#' .ddg.link.function.returns determines if the command calls a
#' function for which ddg.return has created a node for the return
#' value.  If so, a data flow edge is created from the return value
#' data node to the finish node for the command.  Note that if the
#' assignment is an expression, like "d <- f(a) + f(b)", there may
#' be multiple return value nodes to link to.
#' @param command input command.
#' @return nothing
.ddg.link.function.returns <- function(command) {
  
  return.value.nodes <- .ddg.get.matching.return.value.nodes (command)
  #print (paste (".ddg.link.function.returns: new.uses:", new.uses))
  
  # Create an edge from each of these to the last procedure node.
  lapply (return.value.nodes, function (data.num) {
        .ddg.datanum2lastproc (data.num)
  
        # Set the return value as being used.
        .ddg.set.return.value.used (data.num)
      })
  

  #print ("Returning from .ddg.link.function.returns")
}

#' .ddg.add.start.node creates a start node and its incoming control flow edge.  
#' @param cmd The DDGStatement object for the command being started
#' @param node.name The label to put on the node.  If node.name is not passed in,
#'   the abbreviated label in cmd is used.
#' @return the label of the node created, excluding "Start"
.ddg.add.start.node <- function(cmd = NULL, node.name = "") {
  return (.ddg.add.abstract.node ("Start", cmd, node.name))
}
  
#' .ddg.add.finish.node creates a finish node and its incoming control flow edge.  
#' @param cmd The DDGStatement object for the command being finished
#' @param node.name The label to put on the node.  If node.name is not passed in,
#'   the abbreviated label in cmd is used.
#' @return the label of the node created, excluding "Finish"
.ddg.add.finish.node <- function(cmd = NULL, node.name = "") {
  return (.ddg.add.abstract.node ("Finish", cmd, node.name))
}

#' .ddg.add.abstract.node creates a start or finish node and its 
#' incoming control flow edge.
#' @param cmd The DDGStatement object for the command being finished
#' @param node.name The label to put on the node.  If node.name is not passed in,
#'   the abbreviated label in cmd is used.
#' @return the label of the node created, excluding "Start" or "Finish"
.ddg.add.abstract.node <- function(type, cmd = NULL, node.name = "") {
  #print("In .ddg.add.abstract.node")
  if (node.name == "") {
      node.name <- cmd@abbrev
  }
  if (.ddg.debug.lib()) print(paste("Adding", node.name,  type, "node"))
  .ddg.proc.node(type, node.name, node.name, cmd = cmd)
  .ddg.proc2proc()

  return(node.name)
}

#' .ddg.open.new.command.node opens a new collapsible command
#' node depending on the information stored in .ddg.possible.last.cmd.
#' @return nothing
.ddg.open.new.command.node <- function() {
  new.command <- .ddg.get(".ddg.possible.last.cmd")
  if (!is.null(new.command)) {
    .ddg.add.start.node(new.command)
    
    # Now the new command becomes the last command, and new command
    # is null.
    #print (paste (".ddg.open.new.command.node: saving .ddg.last.cmd as", new.command))
    .ddg.set(".ddg.last.cmd", new.command)
    .ddg.set(".ddg.possible.last.cmd", NULL)
  }
}

#' .ddg.close.last.command.node closes the last created collapsible
#' node stored in .ddg.last.cmd properly by creating the finish node
#' and linking it in.
#' @return nothing
.ddg.close.last.command.node <- function(){

  # Get both the last command and new commands.
  .ddg.last.cmd <-
    if (.ddg.is.set(".ddg.last.cmd")) {
      .ddg.get(".ddg.last.cmd")
    }
    else {
      NULL
    }
  .ddg.possible.last.cmd <-
    if (.ddg.is.set(".ddg.possible.last.cmd")) {
      .ddg.get(".ddg.possible.last.cmd")
    }
    else {
      NULL
    }

  #print (paste (".ddg.close.last.command.node: .ddg.last.cmd =", .ddg.last.cmd))
  #print (paste (".ddg.close.last.command.node: .ddg.possible.last.cmd =", .ddg.possible.last.cmd))

  # Only create a finish node if a new command exists (i.e., we've
  # parsed some lines of code).
  # TODO: Do we need to check .ddg.possible.last.cmd?  We don't use it here.
  if (!is.null(.ddg.last.cmd) && (!is.null(.ddg.possible.last.cmd))) {
    cmd.abbrev <- .ddg.add.finish.node(.ddg.last.cmd)

    # Add link from a function return node if there is one.
    .ddg.link.function.returns(.ddg.last.cmd)

    # No previous command.
    .ddg.set(".ddg.last.cmd", NULL)
  }
}

#' .ddg.is.procedure.cmd returns TRUE if the command passed in
#' is a call to ddg.procedure, ddg.start, or ddg.finish.
#' These will create a procedure node and therefore
#' initiate the creation of a collapsible console node.
#' 
#' @param cmd - A DDGStatement object
#' @return true if cmd is a call to ddg.procedure, ddg.start or ddg.finish
.ddg.is.procedure.cmd <- function(cmd) {
  return(grepl("^ddg.(procedure|start|finish)", cmd@text))
}

#' .ddg.record.warning creates the warning node for the saved warning and 
#' attaches it to the node that created the warning
#' @return nothing
.ddg.record.warning <- function () {
  # Get the saved warning
  w <- .ddg.get.warning()

  # Create a message that looks like the one R creates
  callStr <-
      if (is.null (w$call)) ""
      else paste ("In ", utils::head (deparse(w$call)), ": ")
  warningMessage <- paste (callStr, w$message)

  # Create the warning node
  .ddg.insert.error.message(warningMessage, "warning.msg", doWarn = FALSE)

  # Clear the saved warning
  .ddg.clear.warning()
}

#' .ddg.parse.commands takes as input a list of parsed expressions from
#' an R script and creates DDG nodes for each command. If environ is an
#' environment, it executes the commands in that environment
#' immediately before creating the respective nodes for that
#' command, and then creates the data nodes based on the information
#' available in the environment. If environ is not NULL, calls to
#' ddg.* are not executed so only the clean script is processed.
#' If annotate.inside is TRUE, ddg.function, ddg.eval and ddg.return.value
#' are added to each function definition and ddg.eval is added to control
#' statements before commands are processed. If save.debug is TRUE,
#' changes to the script are saved in the ddg/debug directory.
#' ddg.annotate.on and ddg.annotate.off may be used to limit the
#' functions that are annotated or not annotated, respectively.
#' If run.commands is false, the commands are not executed.  This allows
#' us to build ddgs for commands run from the console as those commands
#' have already been executed.

#' @param exprs list of parsed R statements
#' @param script.name name of the script the statements came from
#' @param script.num the number of the script in the sourced scripts table
#' @param environ environment in which commands should be
#'   executed.
#' @param ignore.patterns (optional) a vector of regular expressions.
#'   Any commands matching these expressions will not be parsed
#'   (i.e. no nodes will be created for them).
#' @param node.name (optional) name for the collapsible node under which
#'   this DDG should be stored.
#' @param run.commands (optional) commands are executed only when environ
#'   is an environment and run.commands is TRUE.
#' @param echo (optional) print each expression after parsing
#' @param print.eval (optional) print result of each evaluation.
#' @param max.deparse.length (optional) maximum number of characters
#'   output for deparse of a single expression.
#' @param called.from.ddg.eval(optional) whether called from ddg.eval
#' @param cmds list of DDG Statements that correspond to the exprs passed in.  This is
#'   currently only used when called from ddg.eval.  Normally, ddg.parse.commands
#'   creates the DDG Statement objects.
#' @return nothing
.ddg.parse.commands <- function (exprs, script.name="", script.num=NA, environ, 
    ignore.patterns=c('^ddg.'), node.name="Console", run.commands = FALSE, echo=FALSE, 
    print.eval=echo, max.deparse.length=150, called.from.ddg.eval=FALSE, cmds=NULL) {

  return.value <- NULL
  
  # Gather all the information that we need about the statements
  if (is.null(cmds)) {
    cmds <- .ddg.create.DDGStatements (exprs, script.name, script.num)
    
    if (.ddg.save.debug()) {
      .ddg.save.annotated.script(cmds, script.name)
    }
  }
  num.cmds <- length(cmds)

  # print (paste("ddg.parse.commands: .ddg.func.depth =", .ddg.get(".ddg.func.depth")))
  inside.func <- (.ddg.get(".ddg.func.depth") > 0)

  if (!inside.func) {
    # Attempt to close the previous collapsible command node if a ddg exists
    if (.ddg.is.init()) {
      .ddg.close.last.command.node()
    }
    
    # Get the last command in the new commands and check to see if
    # we need to create a new .ddg.last.cmd node for future reference.
    .ddg.last.cmd <- cmds[[num.cmds]]
    # print(paste(".ddg.parse.commands: setting .ddg.last.cmd to", .ddg.last.cmd$text))

    if (.ddg.last.cmd@isDdgFunc) {
      .ddg.last.cmd <- NULL
      #print(".ddg.parse.commands: setting .ddg.last.cmd to null")
    }
  }

  # Create start and end nodes to allow collapsing of consecutive
  # console nodes. Don't bother doing this if there is only 1 new
  # command in the history or execution.
  named.node.set <- FALSE

  if (num.cmds > 1 && .ddg.is.init() && !inside.func && !called.from.ddg.eval) {
    #print(paste("ddg.parse.commands: Creating Start for", node.name))
    .ddg.add.start.node(node.name = node.name)
    named.node.set <- TRUE
  }

  # Create an operation node for each command.  We can't use lapply
  # here because we need to process the commands in order and
  # lapply does not guarantee an order. Also decide which data nodes
  # and edges to create. Only create a data node for the last
  # write of a variable and only if that occurs after the last
  # possible writer. Create an edge for a data use as long as the
  # use happens before the first writer/possible writer or after
  # the last writer/possible writer. Lastly, if run.commands is set to
  # true, then execute each command immediately before attempting
  # to create the DDG nodes.

  # Only go through this if  we have at least one command to parse.
  if (num.cmds > 0) {

    # Find where all the variables are assigned for non-environ
    # files.
    if (!run.commands) {
      vars.set <- .ddg.find.var.assignments(cmds)
    }
    else {
      vars.set <- .ddg.create.empty.vars.set()
    }

    # Loop over the commands as well as their string representations.
    for (i in 1:length(cmds)) {
      cmd <- cmds[[i]]

      if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Processing", cmd@abbrev))
      
      # print("Checking whether to set last.cmd")
      if (grepl("^ddg.eval", cmd@text)) {
        if (is.null(.ddg.last.cmd)) {
          .ddg.last.cmd <- cmd
        }
      }

      # Get environment for output data node.
      d.environ <- environ

      if ( .ddg.is.nonlocal.assign(cmd@parsed[[1]]) )
      {
        d.environ <- .ddg.get.env(cmd@vars.set, for.caller=TRUE)
      
        if( identical(d.environ,"undefined") )
          d.environ <- globalenv()
      }


      # Check for control & loop statements.
      st.type <- .ddg.get.statement.type(cmd@parsed[[1]])
      loop.statement <- st.type %in% c("for", "while", "repeat")
      control.statement <- loop.statement || st.type %in% c("if", "{")
      
      # Specifies whether or not a procedure node should be created
      # for this command. Basically, if a ddg exists and the
      # command is not a DDG command or a control statement, it should
      # be created. Note that if control statements are annotated,
      # a procedure node is created for each statement inside a control
      # block, so there is no need to create additional nodes for the
      # control statement itself.

      create <- !cmd@isDdgFunc && .ddg.is.init() &&  
          (!(control.statement && .ddg.loop.annotate() && ddg.max.loops() > 0) || !run.commands)
      start.finish.created <- FALSE
      cur.cmd.closed <- FALSE

      # If the command does not match one of the ignored patterns.
      if (!any(sapply(ignore.patterns, function(pattern){grepl(pattern, cmd@text)}))) {

        # If sourcing, we want to execute the command.
        if (run.commands) {
          # Print command.
          if (echo) {
            cmd.show <- 
                paste0(substr(cmd@text, 
                              1L, 
                              min (max.deparse.length, nchar(cmd@text))), 
                       "\n")
            cat(cmd.show)
          }

          # If we will create a node, then before execution, set
          # this command as a possible abstraction node but only
          # if it's not a call that itself creates abstract nodes.
          if (!cmd@isDdgFunc && cmd@text != "next") {
            .ddg.set(".ddg.possible.last.cmd", cmd)
            .ddg.set (".ddg.cur.cmd", cmd)
            .ddg.push.cmd (cmd)
          }

          else if (.ddg.is.procedure.cmd(cmd)) {
            .ddg.set(".ddg.possible.last.cmd", NULL)
          }

          
          # Capture any warnings that occur when an expression is evaluated.
          # Note that we cannot just use a tryCatch here because it behaves
          # slightly differently and we would lose the value that eval
          # returns.  withCallingHandlers returns the value.
          # withCallingHandlers also re-throws the error after it is caught.

          # EVALUATE.

          if (.ddg.debug.lib()) print (paste (".ddg.parse.commands: Evaluating ", cmd@annotated))

          result <- withCallingHandlers(
          
              {
                for (annot in cmd@annotated) {
                  #print (paste (".ddg.parse.commands: Evaluating ", paste(annot, collapse = " ")))
                  # Don't set return.value if we are calling a ddg function or we are executing an if-statement
                  if (grepl("^ddg", annot) || grepl("^.ddg", annot) || .ddg.get.statement.type(annot) == "if") {
                    eval(annot, environ, NULL)
                  }
                  else {
                    return.value <- eval(annot, environ, NULL)
									  #if (typeof(return.value) != "closure") {
                      #print (paste (".ddg.parse.commands: Done evaluating ", annot))
                      #print(paste(".ddg.parse.commands: setting .ddg.last.R.value to", return.value))
									  #}
                    .ddg.set (".ddg.last.R.value", return.value)
                  }
                }
              },
            warning = .ddg.set.warning ,
            error = function(e)
            {
              # create procedure node for the error-causing operation
              .ddg.proc.node("Operation", cmd@abbrev, cmd@abbrev, functions.called=cmd@functions.called, cmd=cmd)
              .ddg.proc2proc()

              # create input edges by adding variables to set
              if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding", cmd@abbrev, "information to vars.set, for an error"))
              .ddg.create.data.use.edges(cmd, for.caller=FALSE)

              # Create output exception node.
              .ddg.data.node("Exception", "error.msg", toString(e), "ddg.library")
              
              # Create data flow edge from procedure node to exception node.
              .ddg.proc2data(cmd@abbrev, "error.msg")
              
            }
          )

          if (.ddg.debug.lib()) print (paste (".ddg.parse.commands: Done evaluating ", cmd@annotated))

          if (!cmd@isDdgFunc && cmd@text != "next") {
            # Need to get the stack again because it could have been
            # modified during the eval call.
            .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
            stack.length <- length(.ddg.cur.cmd.stack)
            start.created <- .ddg.cur.cmd.stack[stack.length][[1]]

            # Create a finish node if a start node was created
            # start.created can have one of 3 values: "TRUE", "FALSE",
            # "MATCHES_CALL". Only create the finish node if TRUE.
            if (start.created == "TRUE") {
              .ddg.add.finish.node(cmd)
              start.finish.created <- TRUE
              .ddg.link.function.returns(cmd)

              # If the number of loop iterations exceeds max.loops, add
              # output data nodes containing final values to the finish node.
              if (loop.statement && .ddg.were.details.omitted()) {
                vars.set2 <- .ddg.add.to.vars.set(vars.set, cmd, i)
                .ddg.create.data.node.for.possible.writes(vars.set2, cmd, environ)
                .ddg.set.details.omitted(FALSE)
              }
            }

            # Remove the last command & start.created values pushed
            # onto the stack
            cur.cmd.closed <- (.ddg.cur.cmd.stack[stack.length] == "MATCHES_CALL")
            .ddg.pop.cmd ()
          }

          # Print evaluation.
          if (print.eval) print(result)
        }

        # Figure out if we should create a procedure node for this
        # command. We don't create it if it matches a last command
        # (because that last command has now become a collapsible
        # node). Matching a last command means that the last command
        # is set, is not NULL, and is equal to the current command.

        last.proc.node.created <-
            if (.ddg.is.set (".ddg.last.proc.node.created")).ddg.get(".ddg.last.proc.node.created")
            else ""
        
        create.procedure <- create && (!cur.cmd.closed || !named.node.set) && !start.finish.created  && !grepl("^ddg.source", cmd@text)
        
        # We want to create a procedure node for this command.
        if (create.procedure) {
          
          # Create the procedure node.

          if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding operation node for", cmd@abbrev))
          
          .ddg.proc.node("Operation", cmd@abbrev, cmd@abbrev, functions.called=cmd@functions.called, cmd=cmd)
          .ddg.proc2proc()

          # If a warning occurred when cmd was evaluated,
          # attach a warning node
          if (.ddg.warning.occurred()) {
            .ddg.record.warning()
          }
          # Store information on the last procedure node in this
          # block.
          last.proc.node <- cmd

          # We want to create the incoming data nodes (by updating
          # the vars.set).
          if (run.commands) {
            # Add variables to set.
            vars.set <- .ddg.add.to.vars.set(vars.set,cmd,i)

            if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding", cmd@abbrev, "information to vars.set"))
          }

          .ddg.create.data.use.edges(cmd, for.caller=FALSE)

          .ddg.create.file.read.nodes.and.edges()
          .ddg.link.function.returns(cmd)

          if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding input data nodes for", cmd@abbrev))

          .ddg.create.data.set.edges(vars.set, cmd, d.environ)

          if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding output data nodes for", cmd@abbrev))

          .ddg.create.file.write.nodes.and.edges ()
          .ddg.create.graphics.nodes.and.edges ()
        }
        # We wanted to create it but it matched a last command node.
        else if (create && run.commands) {
          .ddg.close.last.command.node()
          if (run.commands) {
            # Add variables to set.
            vars.set <- .ddg.add.to.vars.set(vars.set,cmd, i)
            if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding", cmd@abbrev, "information to vars.set"))
            .ddg.create.data.set.edges(vars.set, cmd, environ)
          }
        }

        if (create.procedure && run.commands) {
          .ddg.create.data.node.for.possible.writes(vars.set, last.proc.node, env=environ)

          # Update so we don't set these again.
          vars.set$possible.last.writer <- vars.set$last.writer
        }
      }
     }

     # Create a data node for each variable that might have been set in
     # something other than a simple assignment, with an edge from the
     # last node in the console block or source .
     if (!run.commands) {
       .ddg.create.data.node.for.possible.writes(vars.set, last.proc.node, env=environ)
     }
  }

  #print("Done with ddg.parse.commands loop")

  # Close any node left open during execution.
  if (run.commands && !inside.func) .ddg.close.last.command.node()

  # Close the console block if we processed anything and the DDG
  # is initialized (also, save).
  #
  if (.ddg.is.init() && named.node.set && !inside.func) {
      .ddg.add.finish.node(node.name = node.name)
  }

  if (.ddg.is.set(".ddg.last.R.value")) return (.ddg.get (".ddg.last.R.value"))
  else return ("")
  #return.value <- .ddg.get (".ddg.last.R.value")
  #if (typeof(return.value) != "closure") {
  #  print(paste(".ddg.parse.commands: returning ", return.value))
  #}
}

#' .ddg.push.cmd pushes a command onto the command stack.  The command stack 
#' remembers the command about to be executed.  It also puts FALSE on the stack 
#' to indicate that no start node has (yet) been created for the command.
#' @param cmd The DDGStatement about to be executed
#' @return nothing
.ddg.push.cmd <- function (cmd) {
  
  #print(paste("Pushing onto the stack:", cmd@text))
  
  # Remember the current statement on the stack so that we
  # will be able to create a corresponding Finish node later
  # if needed.
  .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
  
  if (length(.ddg.cur.cmd.stack) == 0) {
    .ddg.cur.cmd.stack <- c(cmd, FALSE)
  }
  else {
    .ddg.cur.cmd.stack <- c(.ddg.get(".ddg.cur.cmd.stack"), cmd, FALSE)
  }
  .ddg.set(".ddg.cur.cmd.stack", .ddg.cur.cmd.stack)
}

#' .ddg.pop.cmd removes the top of the command stack, along with the boolean 
#' that remembers if the start / finish nodes have been created.
#' @return nothing

.ddg.pop.cmd <- function () {
  .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
  stack.length <- length(.ddg.cur.cmd.stack)
  if (stack.length == 2) {
    .ddg.set(".ddg.cur.cmd.stack", vector())
  }
  else {
    .ddg.set(".ddg.cur.cmd.stack", .ddg.cur.cmd.stack[1:(stack.length-2)])
  }
}

#' .ddg.get.top.cmd returns the last command on the stack.
#' @return the last command pushed to the stack
.ddg.get.top.cmd <- function() {
  .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
  stack.length <- length(.ddg.cur.cmd.stack)
  cmd <- .ddg.cur.cmd.stack[stack.length-1][[1]]
}

#' .ddg.change.cmd.top changes the value associated with the current command 
#' while keeping the command at the top of the stack the same
#' @param value the new value
#' @return nothing
.ddg.change.cmd.top <- function (value) {
  .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
  stack.length <- length(.ddg.cur.cmd.stack)
  .ddg.set (".ddg.cur.cmd.stack", c(.ddg.cur.cmd.stack[1:stack.length-1], value))
}


#' .ddg.lookup.function.name gets the name of the calling function
#' and sets pname to that value. If pname is passed as a string,
#' pname is not changed.  If pname is not a string, it is deparsed.
#' If pname is NULL when called, pname is obtained from the calling environment.
#' Note that it is important that this be a macro, not a function,
#' due to the use of the substitute function in the body.  expr is
#' the macro body.
#' 
#' @param pname - name of procedure node.

.ddg.lookup.function.name <- gtools::defmacro (pname,
    expr =
        # If pname is not provided, get from function call.
        if (is.null(pname)) {
          
          # Look up function call.
          # Note: I tried to call .ddg.get.first.non.ddg.call instead
          # of hardwiring the number here but it did not work.  The 
          # call stack contains unexpected entries to eval before
          # getting to the user's function.
          call <- sys.call(-4)

          if (typeof(call[[1]]) == "closure") {
            #print(".ddg.lookup.function.name:  Found a closure!")
            pname <- "FUN"
          }
          else {
            pname <- as.character(call[[1]])
          }
        }

        # Convert pname to a string if necessary.
        else if (!is.character(pname)) {
          pname <- deparse(substitute(pname))
        }
)

#' .ddg.lookup.value is used to determine what value to use when
#' creating data nodes.  
#' Note that it is important that this be a macro, not a function, 
#' due to the use of the substitute function in the body. expr is the macro body. 
#' @param expr.to.evaluate the expression to be evaluted. This can be a string or
#'   a name.
#' @param value the value that was passed in to the calling function.
#'   If value already exists, nothing happens. If value is NULL,
#'   the expression is evaluated to determine the value.
#' @param env the environment in which the evaluation is done.
#' @param procname the name of the calling procedure, used to produce
#'   an error message if necessary.  Only needed if warn is TRUE.
#' @param warn (optional) if TRUE, warns user that the expression could
#'   not be evaluated if the evaluation failed
 
.ddg.lookup.value <- gtools::defmacro(expr.to.evaluate, value, env, procname = "", warn=FALSE,
    expr =
        if (is.null(value)) {
          arg <- substitute(expr.to.evaluate)
          if (is.character(arg)) {
            tryCatch (arg <- parse(text=expr.to.evaluate),
            error = function(e) {})
          }
          else expr.to.evaluate <- deparse(arg)
          value <- tryCatch (
              eval(arg, env),
              error = function(e) {
                if (warn) {
                  error.msg <- paste("Unable to evaluate", expr.to.evaluate, "in call to", procname)
                  .ddg.insert.error.message(error.msg)
                }
                return ("")
              }
          )
        }
)

#' .ddg.delete.temp deletes any temporary files created during
#' the processing of a script. These include the temporary
#' history file.
#' @return nothing

.ddg.delete.temp <- function() {
  # Delete the temporary history file if we made it.
  if (.ddg.is.set('.ddg.history.file')) unlink(.ddg.get('.ddg.history.file'))

  # Clear the environment.
  .ddg.env <- new.env(parent=emptyenv())
}

#' .ddg.create.output.nodes creates output nodes for ddg.function
#' and ddg.procedure. Outs values must be passed as strings, not
#' names, unless the value is a file name.
#' @param pname the name of the procedure node.
#' @param outs.graphic - the name of a snapshot node to be used as a
#'    file name.  A graphical snapshot is simply a captured image
#'    of the graphic device active at the time of the call to
#'    ddg.function or ddg.procedure.
#' @param outs.data - a list of names of data nodes.
#' @param outs.exception - a list of names of exception nodes.
#' @param outs.url - a list of names of url nodes.
#' @param outs.file - a list of names of file nodes. Supported file
#'   extensions include: .csv, .jpg, .jpeg, .pdf, and .txt.
#' @param graphic.fext - the file extension to be used when saving the
#'   captured graphic. Supported extensions are .jpg, .jpeg, .pdf.
#' @return nothing

.ddg.create.output.nodes<- function(pname, outs.graphic, outs.data, outs.exception, outs.url, outs.file, graphic.fext) {
  env <- .ddg.get.first.non.ddg.env()
  
  # Capture graphics device.
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
    # scope <- .ddg.get.scope(outs.exception[[1]])

    lapply(outs.exception,
        function(param) {
          # Get value in calling environment.
          name <- param
          value <- NULL
          .ddg.lookup.value(name, value, env)

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
    # scope <- .ddg.get.scope(outs.url[[1]])

    lapply(outs.url,
        function(param) {
          # Get value in calling environment.
          name <- param
          value <- NULL
          .ddg.lookup.value(name, value, env)

          # URL node.
          scope <- .ddg.get.scope(param, calls=stack)
          .ddg.data.node("URL", name, value, scope)
          .ddg.proc2data(pname, name, scope)
        }
    )
  }

  # Generalized data node (includes simple data values as well as
  # snapshots)
  if (!is.null(outs.data)) {
    # Get scope.
    # scope <- .ddg.get.scope(outs.data[[1]])
    stack <- sys.calls()
    lapply(outs.data,
        function(param) {
          # Get value in calling environment.
          name <- param
          value <- NULL
          .ddg.lookup.value(name, value, env)

          tryCatch({
                if (!is.character(name)) name <- deparse(substitute(name))
                envName <- environmentName(env)
                scope <- .ddg.get.scope(param, calls=stack)
                .ddg.save.data(name, value, error=TRUE, scope=scope)
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
    # scope <- .ddg.get.scope(outs.file[[1]])
    stack <- sys.calls()

    lapply(outs.file,
        function(param) {
          # Get value in calling environment.
          name <- param
          value <- NULL
          .ddg.lookup.value(name, value, env)
          scope <- .ddg.get.scope(param, calls=stack)

          if (value == "") {
            # Filename passed as value.
            .ddg.file.copy(name, name, scope)
            .ddg.proc2data(pname, name, scope)
          }
          else {
            # Filename passed as name.
            .ddg.file.copy(value, name, scope)
            .ddg.proc2data(pname, name, scope)
          }
        }
    )
  }
}

#' .ddg.get.first.non.ddg.env gets the environment for the function that called
#' into ddg functions
#' @return the environment of the innermost user's function
.ddg.get.first.non.ddg.env <- function() {
  non.ddg.frame <- .ddg.get.first.non.ddg.frame.number()
  return (sys.frame(non.ddg.frame))
}

#' .ddg.get.first.non.ddg.frame.number gets the frame number for the function 
#' that called into ddg functions
#' @return the frame number of the innermost user function
.ddg.get.first.non.ddg.frame.number <- function() {
  calls <- sys.calls()
  calls <- as.character (mapply( `[[` , calls , 1 , SIMPLIFY = TRUE ))
  #print(paste("calls =", calls))
  #print(summary(calls))
  
  return ( Position( function (call) {return (!startsWith (call, "ddg") & !startsWith (call, ".ddg"))}, calls, right=TRUE ))
}

#' .ddg.create.function.nodes creates the start node, procedure node, input
#' binding nodes, and output nodes for the function.
#' @param pname name of procedure node.
#' @param call call as made
#' @param full.call full function call, with full parameter names
#' @param outs.graphic - the name of a snapshot node to be used as a
#'    file name.  A graphical snapshot is simply a captured image
#'    of the graphic device active at the time of the call to
#'    ddg.function or ddg.procedure.
#' @param outs.data - a list of names of data nodes.
#' @param outs.exception - a list of names of exception nodes.
#' @param outs.url - a list of names of url nodes.
#' @param outs.file - a list of names of file nodes. Supported file
#'   extensions include: .csv, .jpg, .jpeg, .pdf, and .txt.
#' @param graphic.fext - the file extension to be used when saving the
#'   captured graphic. Supported extensions are .jpg, .jpeg, .pdf.
#' @param env (optional) - the environment local to the function
#' @return nothing

.ddg.create.function.nodes <- function(pname, call, full.call, outs.graphic=NULL, outs.data=NULL, outs.exception=NULL, outs.url=NULL, outs.file=NULL, graphic.fext="jpeg", env=NULL) {
  # Create the start node
  if (typeof(call[[1]]) == "closure") {
    #print(paste(".ddg.create.function.nodes: pname =", pname))
    .ddg.add.start.node (node.name=pname)
  }
  else {
    #print(paste(".ddg.create.function.nodes: deparse(call) =", deparse(call)))
    .ddg.add.start.node (node.name=paste(deparse(call), collapse=""))
  }

  # Tokens will contain the function name and the argument
  # expressions.

  # Get parameters and create edges.
  if (length(full.call) > 1) {
    # args contains the names of the variable that was passed into
    # the function.
    args <- full.call[2:length(full.call)]

    # param,names contains the names of the parameters (this is
    # what the variable is known as inside the function).
    #print(paste(".ddg.create.function.nodes: full.call =", full.call))
    param.names <- names(full.call)
    param.names <- param.names[2:length(param.names)]
    stack <- sys.calls()
    bindings <- list()
    for (i in 1:length(args)) bindings[[i]] <-list(args[[i]], param.names[[i]])
    missing.params <- character()

    lapply(bindings,
        function(binding) {
          # Here, arg is the arguments passed IN.
          arg <- binding[[1]]

          # formal is the paramenter name of the function (what
          # is the variable known as inside?).
          formal <- binding[[2]][[1]]
          if (is.null(formal) || formal == "") formal <- "..."

          # Find all the variables used in this parameter.
          # If the argument is a string constant, don't bother
          # looking for variables.  Also add quotes around it
          # in the node name.
          if (is.character(arg)) {
            vars.used <- character()
            binding.node.name <- paste(formal, " <- \"", arg, "\"", sep="")
          }
          else {
            vars.used <- .ddg.find.var.uses(arg)
            binding.node.name <- paste(formal, " <- ", paste(deparse(arg), collapse=" "))
            #print(paste(".ddg.create.function.nodes: binding.node.name =", binding.node.name))
          }

          .ddg.proc.node("Binding", binding.node.name)
          .ddg.proc2proc()
          
          # Add an input to the binding node for each variable referenced in the argument
          sapply (vars.used, function (var) {
                param.scope <- .ddg.get.scope(var, for.caller = TRUE, calls=stack)
                if (.ddg.data.node.exists(var, param.scope)) {
                  .ddg.data2proc(as.character(var), param.scope, binding.node.name)
                  if (.ddg.debug.lib()) print(paste("param:", var))
                }
              })
          if (formal != "...") {
            formal.scope <- .ddg.get.scope(formal, calls=stack)
            formal.env <- .ddg.get.env(formal, calls=stack)

            # If we can evaluate the argument without an error, we
            # record the value. If an error occurs, we do not record
            # the value as it's possible that the function never
            # actually uses it.
            tryCatch ({
                  .ddg.save.data(formal, eval(parse(text=formal), formal.env), scope=formal.scope, stack=stack)
                  .ddg.proc2data(binding.node.name, formal, formal.scope)},
                error = function(e) {})

          }
        })
  }

  .ddg.proc.node("Operation", pname, pname)

  # Link to the definition of the function if the function is defined in this script.
  if (.ddg.data.node.exists(pname, environmentName(.GlobalEnv))) {
    .ddg.data2proc(pname, environmentName(.GlobalEnv), pname)
  }

  # Create edges from the formal to the operation node for the function
  if (length(full.call) > 1) {
    lapply(bindings, function(binding) {
          formal <- binding[[2]][[1]]

          # Formal will be NULL if declared as ...  Don't create the data node in that case.
          if (!is.null(formal) && formal != "") {
            formal.scope <- .ddg.get.scope(formal, calls=stack)
            if (.ddg.data.node.exists (formal, formal.scope)) {
              .ddg.data2proc(formal, formal.scope, pname)
            }
          }
        })
  }

  # Create control flow edge from preceding procedure node.
  .ddg.proc2proc()

  # create output nodes

  .ddg.create.output.nodes(pname, outs.graphic, outs.data, outs.exception, outs.url, outs.file, graphic.fext)

}

#' .ddg.get.frame.number gets the frame number of the closest
#' non-library calling function.
#' @param calls call stack to search
#' @param for.caller (optional) if TRUE, return the frame of the caller of the 
#'    first non-ddg function
#' @return If for.caller is FALSE, returns the top-most non-ddg function on the
#'   call stack.  If for.caller is TRUE, returns the second one found.  If none
#'   are found, returns 0. 
.ddg.get.frame.number <- function(calls, for.caller=FALSE) {
  script.func.found <- FALSE
  nframe <- length(calls)
  for (i in nframe:1) {
    call <- calls[[i]][[1]]
    # Guess that if we have a closure it is a user-defined function and not a ddg function
    # Is this a good assumption ????
    if (typeof(call) == "closure") {
      if (for.caller && !script.func.found) {
        script.func.found <- TRUE
      }
      else {
        return(i)
      }
    }
    else {
      call.func <- as.character(call)
      # Ignore calls to ddg functions or to the functions that get called from the outermost tryCatch
      # to ddg code.
      if (!any (startsWith (call.func, c (".ddg", "ddg", "doTryCatch", "tryCatch")))) {
        if (for.caller && !script.func.found) {
          script.func.found <- TRUE
        }
        else {
          return(i)
        }
      }
    }
  }
  return(0)
}


#' .ddg.where looks up the environment for the variable specified
#' by name.  Adapted from Hadley Wickham, Advanced R programming.
#' @param name - name of variable.
#' @param env (optional) - environment in which to start looking for variable.
#' @param warning (optional) - set to TRUE if a warning should be thrown when a variable is not found.
#' @return the environment in which the name is found.  Returns "undefined" if the
#'   variable is not found.
.ddg.where <- function( name , env = parent.frame() , warning = TRUE )
{
  stopifnot(is.character(name), length(name) == 1)
  
  if (identical(env, emptyenv()))
  {
    if(warning)
      warning("Can't find ", name)

    return("undefined")
  }
  if (exists(name, env, inherits=FALSE))
  {
    env
  }
  else
  {
    .ddg.where(name, parent.env(env), warning)
  }
}

#'.ddg.get.env gets the environment in which name is declared.
#' @param name variable name.
#' @param for.caller (optional) if TRUE, go up one level before searching.
#' @param calls (optional) call stack to search
#' @return the environment in which name is declated

.ddg.get.env <- function(name, for.caller=FALSE, calls=NULL) {
  if (is.null(calls)) calls <- sys.calls()

  fnum <- .ddg.get.frame.number(calls, for.caller)
  stopifnot(!is.null(fnum))

  tryCatch (
    if(!exists(name, sys.frame(fnum), inherits=TRUE)) return(NULL),
    error = function(e) {}
  )
  env <- .ddg.where(name, sys.frame(fnum))
  return(env)
}

#' .ddg.get.scope converts from an environment object to its name.  If no
#' environment is passed in, it uses the name to find the environment.  One
#' of name or env must be provided.
#' @param name name of variable.
#' @param for.caller (optional) if TRUE, go up one level before searching.
#' @param calls (optional) call stack to search
#' @param env (optional) the environment to get the scope for
#' @return the scope of the variable

.ddg.get.scope <- function(name="", for.caller=FALSE, calls=NULL, env=NULL) {
  # Get the environment for the variable call.
  if (is.null(env)) {
    env <- .ddg.get.env(name, for.caller, calls)
  }

  # If no environment found, name does not exist, so scope is
  # undefined.
  if (is.null(env)) return ("undefined")

  scope <- sub('^<environment: (.*)>$', '\\1', utils::capture.output(env)[1])
  if (grepl("undefined", scope)) scope <- "undefined"
  return(scope)
}

#' .ddg.create.start.for.cur.cmd creates a start node for the current command 
#' if one has not been created already.  Modifies the command stack by setting 
#' the value to TRUE if the start node is created.  If the current command 
#' matches the call, no node is created but the top of the stack is changed
#' to "MATCHES_CALL".
#' @param call the parsed version of the function call
#' @return nothing
.ddg.create.start.for.cur.cmd <- function (call) {
  if (!.ddg.is.set(".ddg.cur.cmd")) return ()

  .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
  stack.length <- length(.ddg.cur.cmd.stack)
  if (stack.length == 0) return ()
  
  last.created <- .ddg.cur.cmd.stack[stack.length]
  # Only create a start node for the current command if we have not already
  # created one and the command is more than just the call to this function
  if (last.created[[1]] != "FALSE") return ()
  
  .ddg.cur.cmd <- .ddg.get(".ddg.cur.cmd")
  if (.ddg.cur.cmd@text == paste(deparse(call), collapse="")) {
    .ddg.change.cmd.top ("MATCHES_CALL")
  }
  
  else {
    cmd.abbrev <- .ddg.add.start.node (.ddg.cur.cmd)
    .ddg.cur.expr.stack <- .ddg.get(".ddg.cur.expr.stack")
    st.type <- .ddg.get.statement.type(.ddg.cur.cmd@parsed[[1]])
    loop.statement <- st.type %in% c("for", "while", "repeat")
    control.statement <- loop.statement || st.type %in% c("if", "{")
    .ddg.create.data.use.edges(.ddg.cur.cmd, for.caller=!control.statement)

    # Add Details Omitted node before annotated loops if needed.
    if (loop.statement && ddg.first.loop() > 1) {
      ddg.details.omitted()
    }

    # Mark the start node as created on the stack.  Mark it even if we did not
    # create the abstract node above, because we will create it below.
    .ddg.change.cmd.top (TRUE)
  }
}

#' .ddg.markdown takes a Rmd file and extracts the R code and text through
#' the purl function in the knitr library. It then annotates the R script
#' to insert start and finish nodes based on the chunks the user already
#' created. If eval = false, then the chunk will not be added to the DDG. If
#' the user has a name for the chunk, then that name will be used, else a chunk
#' name "ddg.chunk_1" and higher numbers will be generated.
#' Important: If in a code chunk, there is an empty line followed by "#' ----"
#' or "#''", then an extra finish node will be inserted, causing an error.
#' @param r.script.path the path of the original Rmd file
#' @param output.path the path of the generated R script
#' @return the path to the original Rmd file
.ddg.markdown <- function(r.script.path, output.path){

  #generates R script file from markdown file
  knitr::purl(r.script.path, documentation = 2L, quiet = TRUE)

  #moves file to ddg directory
  file.rename(from = paste(getwd(), "/", basename(tools::file_path_sans_ext(r.script.path)), ".R", sep = ""), to = output.path)
  script <- readLines(output.path)

  skip <- FALSE
  name <- "ddg.chunk"
  annotated <- character(0)
  index <- 1


  # This for loop goes through the script line by line and searches for patterns
  # to insert the start and finish nodes
  for(i in 1:length(script)){

    #eval = false means we skip this code chunk, therefore skip = TRUE
    if(regexpr("eval+(\\s*)+=+(\\s*)+FALSE", script[i]) != -1){
      skip <- TRUE
      annotated <- append(annotated, script[i])
    }

    else if(regexpr("## ----", script[i]) != -1){

      #if no options in the line, then generate default name.
      if(regexpr("## -----", script[i]) == -1){
        if(regexpr("=", script[i]) == -1){
          end <- regexpr("-----", script[i])
          name <- substring(script[i], 8, last = end -1)
        }
        else if(regexpr(",", script[i]) != -1){
          comma <- regexpr(",", script[i])
          name <- substring(script[i], 8, last = comma -1)
        }
        else{
          name <- paste("ddg.chunk_", index, sep = "")
          index <- index + 1
        }
      }
      else{
        name <- paste("ddg.chunk_", index, sep = "")
        index <- index + 1
      }
      name <- stringr::str_trim(name, side = "both")
      annotated <- append(annotated, paste("ddg.start(\"", name, "\")", sep = ""))
    }
    else if(nchar(script[i]) == 0 && (regexpr("#'", script[i + 1]) != -1 ||
                                      i == length(script) || regexpr("## ----", script[i + 1]) != -1 )){
      if(skip){
        annotated <- append(annotated, script[i])
        skip <- FALSE
      }
      else{
        annotated <- append(annotated, paste("ddg.finish(\"", name, "\")", sep = ""))
      }
    }
    else{
      annotated <- append(annotated, script[i])
    }
  }
  writeLines(annotated, output.path)
  r.script.path
}


#' .ddg.save.debug.files saves debug files to the debug directory.
#' @return nothing
.ddg.save.debug.files <- function() 
{
	# Save initial environment table to file.
	fileout <- paste(.ddg.path.debug(), "/initial-environment.csv", sep="")
	ddg.initial.env <- .ddg.initial.env()
  utils::write.csv(ddg.initial.env, fileout, row.names=FALSE)

  .ddg.save.debug.proc.nodes ()
  .ddg.save.debug.data.nodes ()
  .ddg.save.debug.edges()
  .ddg.save.function.table ()

	# save library information to file
	fileout <- paste(.ddg.path.debug(), "/libraries.csv", sep="")
  utils::write.csv(.ddg.installedpackages(), fileout, row.names=FALSE)
	
	# save execution environment information to file
	fileout <- paste(.ddg.path.debug(), "/environment.csv", sep="")
  utils::write.csv(.ddg.exec.env(), fileout, row.names=FALSE)
	
  .ddg.save.return.value.table ()
  .ddg.save.sourced.script.table ()
}

#' .ddg.exec.env returns a dataframe of information about the current
#' execution environment
#' @return a data frame of information about the current environment.

.ddg.exec.env <- function()
{
	env <- data.frame(	"architecture" = character(1), 
						"os" = character(1), 
						"language" = character(1), 
						"rVersion" = character(1), 
						"script" = character(1), 
						"scriptTimeStamp" = character(1),
						"workingDirectory" = character(1), 
						"ddgDirectory" = character(1), 
						"ddgTimeStamp" = character(1),
						"rdtVersion" = character(1), 
						"hashAlgorithm" = character(1),
						stringsAsFactors = FALSE )
	
	# architecture, language, rVersion
	r.version <- R.Version()
	
	env$architecture[1] <- r.version$arch
	env$language[1] <- r.version$language
	env$rVersion[1] <- r.version$version
	
	# operating system
	env$os[1] <- .Platform$OS.type
	
	# script variables
	script.path <- .ddg.get("ddg.r.script.path")
	
	if( ! is.null(script.path) )
	{
		env$script[1] <- script.path
		env$scriptTimeStamp[1] <- .ddg.format.time( file.info(script.path)$mtime )
	}
	else
	{
		env$script[1] <- ""
		env$scriptTimeStamp[1] <- ""
	}
	
	# working directory, ddg. directory
	env$workingDirectory[1] <- getwd()
	env$ddgDirectory[1] <- .ddg.path()
	
	# ddg timestamp
	env$ddgTimeStamp[1] <- .ddg.get("ddg.start.time")
	
	# rdt version
	env$rdtVersion[1] <- toString( utils::packageVersion("RDataTracker") )
	
	# hash algorithm
	env$hashAlgorithm[1] <- .ddg.get(".ddg.hash.algorithm")
	
  return(env)
}


