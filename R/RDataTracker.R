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

# Set maximum number of checkpoints in DDG.

# ddg.MAX_CHECKPOINTS <- 10

# Set the number of lines the history file keeps (and therefore
# can be analyzed). Note: this setting has no effect on some
# systems.

ddg.MAX_HIST_LINES <- 2^14

#-------- FUNCTIONS TO MANAGE THE GLOBAL VARIABLES--------#

# Global variables cannot be used directly in a library.  Instead,
# we need to place the variables in our own environment.  These
# functions make that environment easier to use.

.onLoad <- function(libname, pkgname) {
  .ddg.init.tables()
}

.ddg.set <- function(var, value) {
  .ddg.env[[var]] <- value
  return(invisible(.ddg.env[[var]]))
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

# .ddg.clear removes all objects from the .ddg.env environment.

.ddg.clear <- function() {
  # reinitialize tables
  .ddg.init.tables()
}

##### Getters for specific variables

.ddg.debug.lib <- function() {
  return (.ddg.get("ddg.debug.lib"))
}

.ddg.path <- function() {
  return (.ddg.get("ddg.path"))
}

.ddg.data.dir <- function() {
  return ("data")
}

.ddg.path.data <- function() {
  return(paste(.ddg.path(), .ddg.data.dir() , sep="/"))
}

.ddg.path.debug <- function() {
  return(paste(.ddg.path(), "/debug", sep=""))
}

.ddg.path.scripts <- function() {
  return(paste(.ddg.path(), "/scripts", sep=""))
}

.ddg.save.debug <- function() {
  return(.ddg.get("ddg.save.debug"))
}

.ddg.initial.env <- function() {
  return(.ddg.get("ddg.initial.env"))
}

.ddg.annotate.on <- function() {
  return (.ddg.get("ddg.annotate.on"))
}

.ddg.annotate.off <- function() {
  return (.ddg.get("ddg.annotate.off"))
}

.ddg.enable.source <- function() {
  return(.ddg.is.set("from.source") && .ddg.get("from.source"))
}

# value should be TRUE or FALSE
# Keeps track of whether the last loop has all iterations
# recorded or not.
.ddg.set.details.omitted <- function (value) {
  .ddg.set ("details.omitted", value)
}

.ddg.were.details.omitted <- function () {
  .ddg.get ("details.omitted")
}

# Functions that allow us to save warnings when they occur
# so that we can create the warning node after the node
# that caused the warning is created.

# .ddg.set.warning is attached as a handler when we evaluate
# expressions.  It saves the warning so that a warning
# node can be created after the procedural node that
# corresponds to the expression that caused the warning
#
# w - the simplewarning object created by R

.ddg.set.warning <- function(w) {
  # Only save warnings if the warn level is set to report them at all.
  # This is important because we do temporarily set the warning level
  # to avoid warnings that RDT might cause that are safe to ignore.
  # Search for calls to the option function to see where that happens.
  if (getOption("warn") >= 0) {
    .ddg.set(".ddg.warning", w)
  }
}

.ddg.clear.warning <- function() {
  .ddg.set(".ddg.warning", NA)
}

.ddg.get.warning <- function () {
  return (.ddg.get(".ddg.warning"))
}

.ddg.warning.occurred <- function() {
  return (.ddg.is.set(".ddg.warning") && !is.na(.ddg.get(".ddg.warning")))
}

##### Mutators for specific common actions

.ddg.inc <- function(var) {
  value <- .ddg.get(var)
  return (.ddg.set(var, value + 1))
}

.ddg.dec <- function(var) {
  value <- .ddg.get(var)
  .ddg.set(var, value - 1)
}

.ddg.add.rows <- function(df, new.rows) {
  table <- .ddg.get(df)
  return (.ddg.set(df, rbind(table, new.rows)))
}

.ddg.push <- function(x, value) {
  return(assign(as.character(substitute(x)), c(x, value), parent.frame()))
}

.ddg.pop <- function(x) {
  return(assign(as.character(substitute(x)), x[-length(x)], parent.frame()))
}


#-------------------BASIC FUNCTIONS-----------------------#

#' .ddg.get.initial.env creates a table of non-ddg objects present in the
#' R environment before the script is executed.  This is only used for 
#' debugging.
#' 
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
#' 
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
  
  .ddg.init.console.vars ()

  # Functions to be annotated.
  .ddg.set("ddg.annotate.on", NULL)

  # Functions not to be annotated.
  .ddg.set("ddg.annotate.off", NULL)

  .ddg.init.sourced.scripts ()

  # Save debug files on debug directory
  .ddg.set("ddg.save.debug", FALSE)
  
  .ddg.init.statements ()

  # Set max.snapshot.size for console mode.
  if (!.ddg.is.set("ddg.max.snapshot.size")) {
    .ddg.set("ddg.max.snapshot.size", 100)
  }
  
  .ddg.init.hashtable ()
}

#' .ddg.init.environ() sets up the filesystem and R environments
#' for use.
#' 
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
#' 
#' @return true if provenance has been initialized
.ddg.is.init <- function() {
    # Short circuits evaluation.
    return(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized"))
}

#' .ddg.is.nonlocal.assign returns TRUE if the object passed is an
#' expression object containing a non-local assignment.
#' 
#' @param expr input expression.
#' @returnType logical
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
#' The difference between first.writer and possible.first.writer is
#' that first.writer is for simple assignments (like a <- 1), while
#' possible.first.writer is for situations where the assignment might
#' not have occurred, like "if (foo) a <- 1".
#' 
#' @returnType The data frame is structured as follows: \cr
#' - the variable name.\cr
#' - the position of the statement that wrote the variable first.\cr
#' - the position of the statement that wrote the variable last.\cr
#' - the position of the first statement that may have assigned to a
#'   variable .\cr
#' - the position of the last statement that may have assigned to a
#'   variable.\cr
#' 
#' @param var.table.size desired size of the data frame. Negative values
#'   and 0 are coerced to 1.
#' 
#' @return the data frame constructed
.ddg.create.empty.vars.set <- function(var.table.size=1) {

  if (var.table.size <= 0) var.table.size <- 1

  vars.set <- data.frame(variable=character(var.table.size),
      first.writer=numeric(var.table.size),
      last.writer=numeric(var.table.size),
      possible.first.writer=numeric(var.table.size),
      possible.last.writer=numeric(var.table.size),
      stringsAsFactors=FALSE)

  # Initialize first writer.
  vars.set$first.writer <- var.table.size + 1
  vars.set$possible.first.writer <- var.table.size + 1

  return(vars.set)
}

#'.ddg.double.vars.set doubles the size of a variable
#' assignment data frame and returns the new one.
#' 
#' @param vars.set data frame containing variable assignments.
#' @returnType same data frame type as the parameter
#' @return a data frame that is twice the size as the original
.ddg.double.vars.set <- function(vars.set) {
  size=nrow(vars.set)
  
  # Create the right size data frame from input frame.
  new.vars.set <- rbind(vars.set,.ddg.create.empty.vars.set(size))

  # Update first/last writer.
  new.vars.set$first.writer <- 
      ifelse(new.vars.set$first.writer == size + 1, 
             size*2 + 1, 
             new.vars.set$first.writer)
  new.vars.set$possible.first.writer <- 
      ifelse(new.vars.set$possible.first.writer == size + 1, 
             size*2 + 1, 
             new.vars.set$possible.first.writer)

  return(new.vars.set)
}

#' .ddg.add.to.vars.set adds the variables set in the command
#' to the variable assignment data frame. Note that
#' var.num is a global variable! It should be intialized when
#' vars.set is first created.
#' 
#' @param vars.set variable assignment data frame.
#' @param cmd a DDGStatement object
#' @param i position of command in the list of commands
#' @returnType the same data frame type as vars.set
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
        vars.set$first.writer[var.num] <- i
        vars.set$last.writer[var.num] <- i
      }
      else {
        vars.set$possible.first.writer[var.num] <- i
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
#'
#' @param cmds a list of DDGStatement objects
#' @returnType a data frame as described in .ddg.create.empty.vars.set
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


#' .ddg.create.data.use.edges.for.console.cmd creates a data flow
#' edge from the node for each variable used in cmd to the
#' procedural node labeled cmd, as long as the value would either
#' be one that exists prior to starting the console block, or
#' corresponds to the last setting of this variable in the console
#' block.
#' 
#' @param vars.set variable assignment data frame.
#' @param cmd name of procedure node.
#' @param cmd.pos - position of command.
#' @param for.caller whether the search for the variable's scope should start at the 
#'   current stack frame, or start with its caller
#' @return nothing
.ddg.create.data.use.edges.for.console.cmd <- function (vars.set, cmd, cmd.pos, for.caller) {
  # Find all the variables used in this command.
  #print (paste(".ddg.create.data.use.edges.for.console.cmd: cmd = ", cmd@text))
  vars.used <- cmd@vars.used

  for (var in vars.used) {
    #print(paste(".ddg.create.data.use.edges.for.console.cmd: var =", var))
    # Make sure there is a node we could connect to.
    scope <- .ddg.get.scope(var, for.caller)

    #print(paste(".ddg.create.data.use.edges.for.console.cmd: scope =", scope))

    if (.ddg.data.node.exists(var, scope)) {
      # print(".ddg.create.data.use.edges.for.console.cmd found data node")
      nRow <- which(vars.set$variable == var)

      # Check if the node is written in the console block.
      if (length(nRow) > 0) {
        first.writer <- min(vars.set$first.writer[nRow], vars.set$possible.first.writer[nRow])
        last.writer <- max(vars.set$last.writer[nRow], vars.set$possible.last.writer[nRow])
        
        # Draw the edge if we will connect to a node that exists
        # before the console block or to the last writer of this
        # variable within the console block.

        if (cmd.pos <= first.writer || cmd.pos >= last.writer) {
        # Note: the following line leads to the self-referencing
        # node problem.
        # if (cmd.pos <= first.writer || cmd.pos > last.writer) {
          .ddg.data2proc(var, scope, cmd@abbrev)
        }

        # TODO - add some sort of warning to the user that the node
        # is not being created
      }

      # The variable is not set at all in this console block.
      # Connect to a pre-existing data node.
      else {
        .ddg.data2proc(var, scope, cmd@abbrev)
      }
    }
    else {
      # TODO - add some sort of warning that the data node was NOT
      # found.

      # error.msg <- paste("Unable to find data node for",var, ". Command", parse(text=cmd.expr), "appears to use it for procedure node", cmd, ".")
      # .ddg.insert.error.message(error.msg)
    }
  }
  #print (".ddg.create.data.use.edges.for.console.cmd Done")
}


#' .ddg.create.data.set.edges.for.cmd creates data nodes for 
#' variables being set, saves the data, and creates edges from the
#' procedure node that set the variable to the new data node.  These
#' nodes and edges correspond to the variables set in the command passed in.
#' 
#' @param vars.set variable assignment data frame.
#' @param cmd the command to create edges for
#' @param cmd.pos position of command in the list of commands
#' @param env environment to use for evaluating variables set.
#' @return nothing
.ddg.create.data.set.edges.for.cmd <- function(vars.set, cmd, cmd.pos, env) {
  # print(paste("In .ddg.create.data.set.edges.for.cmd: cmd = ", cmd@abbrev))
  vars.assigned <- cmd@vars.set

  for (var in vars.assigned) {

    #print(paste(".ddg.create.data.set.edges.for.cmd: var = ", var))
    whichRows <- which(vars.set$variable == var)

    # Only create a node edge for the last place that a variable is
    # set within a console block.
    if ((length(whichRows) > 0 && vars.set$last.writer[whichRows] == cmd.pos && 
          vars.set$possible.last.writer[whichRows] <= vars.set$last.writer[whichRows])) {
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
#' 
#' @param vars.set variable assignment data frame.
#' @param last.command last command in console block.
#' @param env the environment that the command was executed in
#' @return nothing
#' 
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

# .ddg.link.function.returns determines if the command calls a
# function for which ddg.return has created a node for the return
# value.  If so, a data flow edge is created from the return value
# data node to the finish node for the command.  Note that if the
# assignment is an expression, like "d <- f(a) + f(b)", there may
# be multiple return value nodes to link to.

# command - input command.

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

# ddg.add.abstract.node is exclusively used in .ddg.parse.commands
# (so far) and simply serves to avoid repetition of code.

# type - type of procedure node.
# cmd - command string.
# called (optional) - name of calling function.

.ddg.add.abstract.node <- function(type, cmd = NULL, env, called=".ddg.parse.commands", node.name = "") {
  #print("In .ddg.add.abstract.node")
  #print("cmd =")
  #print(cmd)
  #print("node.name =")
  #print(node.name)
  if (node.name == "") {
    if (is.null(cmd)) {
      node.name <- .ddg.abbrev.cmd(cmd)
    }
    else {
      node.name <- cmd@abbrev
    }
  }
  if (.ddg.debug.lib()) print(paste(called, ":  Adding", node.name,  type, "node"))
  .ddg.proc.node(type, node.name, node.name, cmd = cmd, console=(node.name=="Console"))
  .ddg.proc2proc()

  return(node.name)
}

# .ddg.close.last.command.node closes the last created collapsible
# node stored in .ddg.last.cmd properly.

# env - the environment in which the close is occurring
# called (optional) - used in debugging to identify the function
#   which called .ddg.close.previous.command.
# initial (optional) - if TRUE, try to close previous command node.

.ddg.close.last.command.node <- function(env, called=".ddg.parse.commands", initial=FALSE){

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
  if (!is.null(.ddg.last.cmd) && (!is.null(.ddg.possible.last.cmd) || initial)) {
    cmd.abbrev <- .ddg.add.abstract.node("Finish", .ddg.last.cmd, env=env, called=paste(called, "-> .ddg.close.last.command.node"))

    # Add link from a function return node if there is one.
    .ddg.link.function.returns(.ddg.last.cmd)
    # .ddg.link.function.returns(.ddg.last.cmd$text)

    # Create outflowing edges.
    # Has the assignment happened yet???
    #vars.set <- .ddg.find.var.assignments(.ddg.last.cmd)
    #print (".ddg.close.last.command.node calling .ddg.create.data.set.edges.for.cmd")
    #.ddg.create.data.set.edges.for.cmd(vars.set, .ddg.last.cmd$abbrev, .ddg.last.cmd$expr, 1, env, for.finish.node = TRUE)
    #print (".ddg.close.last.command.node call to .ddg.create.data.set.edges.for.cmd returned")

    # No previous command.
    #print (".ddg.close.last.command.node: created finish node; saving .ddg.last.cmd as null")
    .ddg.set(".ddg.last.cmd", NULL)
  }
}

# .ddg.open.new.command.node opens a new collapsible command
# node depending on the information stored in .ddg.last.cmd.

# env - the environment in which the command occurs
# called (optional) - name of calling function.

.ddg.open.new.command.node <- function(env, called=".ddg.parse.commands") {
  new.command <- .ddg.get(".ddg.possible.last.cmd")
  if (!is.null(new.command)) {
    .ddg.add.abstract.node("Start", new.command, env, called=paste(called, "-> .ddg.open.new.command.node"))

    # Now the new command becomes the last command, and new command
    # is null.
    #print (paste (".ddg.open.new.command.node: saving .ddg.last.cmd as", new.command))
    .ddg.set(".ddg.last.cmd", new.command)
    .ddg.set(".ddg.possible.last.cmd", NULL)
  }
}

# .ddg.is.procedure.cmd returns TRUE if the command passed in
# (as a string) will create a procedure node and therefore
# initiate the creation of a collapsible console node.

# cmd.str - command string.

.ddg.is.procedure.cmd <- function(cmd) {
  return(grepl("^ddg.(procedure|start|finish)", cmd@text))
  # return(grepl("^ddg.(procedure|start|finish|restore|checkpoint)", cmd@text))
}

# Create the warning node for the saved warning and attach it to the node
# that created the warning

.ddg.record.warning <- function () {
  # Get the saved warning
  w <- .ddg.get.warning()

  # Create a message that looks like the one R creates
  callStr <-
      if (is.null (w$call)) ""
      else paste ("In ", head (deparse(w$call)), ": ")
  warningMessage <- paste (callStr, w$message)

  # Create the warning node
  .ddg.insert.error.message(warningMessage, "warning.msg", doWarn = FALSE)

  # Clear the saved warning
  .ddg.clear.warning()
}

# Create the DDGStatement list for a list of parsed expressions.

# exprs - a list of parsed expressions
# script.name - the name of the script the expressions come from
# parseData - information provided by the parser that we use to find line numbers
# enclosing.pos - if exprs are statements within a function definition, enclosing.pos
#   is the source position information of the entire function declaration

# Returns a list of DDGStatement objects

.ddg.create.DDGStatements <- function (exprs, script.name, script.num, parseData = NULL, enclosing.pos = NULL) {
  # The parse data gives us line number information
  if (is.null(parseData)) {
    parseData <- getParseData(exprs, includeText=TRUE)

    if (is.null(parseData)) {
      # In this case there is no line number information available
      cmds <- vector("list", (length(exprs)))
      for (i in 1:length(exprs)) {
        expr <- as.expression(exprs[i])
        cmds[[i]] <- .ddg.construct.DDGStatement(expr, NA, script.name, script.num, parseData)
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

    # Start at the first entry in parse data that begins after the enclosing function begins,
    # ends before the enclosing function ends, and matches the text of the first expression.
    next.parseData <- which(non.comment.parse.data$line1 >= enclosing.pos@startLine & non.comment.parse.data$line2 <= enclosing.pos@endLine & non.comment.parse.data$text == paste(deparse(exprs[[1]]), collapse="\n") )[1]
  }

  # Create the DDGStatements
  cmds <- vector("list", (length(exprs)))
  next.cmd <- 1
  for (i in 1:length(exprs)) {
      expr <- as.expression(exprs[i][[1]])
    next.expr.pos <- new (Class = "DDGStatementPos", non.comment.parse.data[next.parseData, ])
    cmds[[next.cmd]] <- .ddg.construct.DDGStatement(expr, next.expr.pos, script.name, script.num, parseData)
    next.cmd <- next.cmd + 1

    # If there are more expressions, determine where to look next in the parseData
    if (i < length(exprs)) {
       last.ending.line <- non.comment.parse.data[next.parseData,]$line2
       last.parent <- non.comment.parse.data[next.parseData,"parent"]
       last.id <- non.comment.parse.data[next.parseData,"id"]

       # Find the first entry in parseData that has the same parent as the
       # previous expression and starts after the previous expression.
       next.parseData <- which(non.comment.parse.data$parent == last.parent & non.comment.parse.data$line1 >= last.ending.line & non.comment.parse.data$id > last.id) [1]
    }
  }

  return (cmds)
}

# .ddg.save.annotated.script saves a copy of the annotated script to
# the debug directory.

.ddg.save.annotated.script <- function(cmds, script.name) {
  for (i in 1:length(cmds)) {
    expr <- cmds[[i]]@annotated
    for (j in 1:length(expr)) {
      line <- deparse(expr[[j]])
      if (i == 1 && j == 1) script <- line else script <- append(script, line)
    }
  }

  fileout <- file(paste(.ddg.path.debug(), "/annotated-", script.name, sep=""))
  write(script, fileout)
  close(fileout)
}

# .ddg.parse.commands takes as input a list of parsed expressions from
# an R script and creates DDG nodes for each command. If environ is an
# environment, it executes the commands in that environment
# immediately before creating the respective nodes for that
# command, and then creates the data nodes based on the information
# available in the environment. If environ is not NULL, calls to
# ddg.* are not executed so only the clean script is processed.
# If annotate.inside is TRUE, ddg.function, ddg.eval and ddg.return.value
# are added to each function definition and ddg.eval is added to control
# statements before commands are processed. If save.debug is TRUE,
# changes to the script are saved in the ddg/debug directory.
# ddg.annotate.on and ddg.annotate.off may be used to limit the
# functions that are annotated or not annotated, respectively.
#
# If run.commands is false, the commands are not executed.  This allows
# us to build ddgs for commands run from the console as those commands
# have already been executed.

# exprs - list of parsed R statements
# script.name - name of the script the statements came from
# script.num - the number of the script in the sourced scripts table
# environ - environment in which commands should be
#   executed.
# ignore.patterns (optional) - a vector of regular expressions.
#   Any commands matching these expressions will not be parsed
#   (i.e. no nodes will be created for them).
# node.name (optional) - name for the collapsible node under which
#   this DDG should be stored.
# run.commands (optional) - commands are executed only when environ
#   is an environment and run.commands is TRUE.
# echo (optional) - print each expression after parsing
# print.eval (optional) - print result of each evaluation.
# max.deparse.length (optional) - maximum number of characters
#   output for deparse of a single expression.
# called.from.ddg.eval(optional) - whether called from ddg.eval
# cmds - list of DDG Statements that correspond to the exprs passed in.  This is
#   currently only used when called from ddg.eval.  Normally, ddg.parse.commands
#   creates the DDG Statement objects.

.ddg.parse.commands <- function (exprs, script.name="", script.num=NA, environ, ignore.patterns=c('^ddg.'), node.name="Console", run.commands = FALSE, echo=FALSE, print.eval=echo, max.deparse.length=150, called.from.ddg.eval=FALSE, cmds=NULL) {

  return.value <- NULL
  
  # Gather all the information that we need about the statements
  if (is.null(cmds)) {
    cmds <- .ddg.create.DDGStatements (exprs, script.name, script.num)

    if (.ddg.save.debug()) {
      .ddg.save.annotated.script(cmds, script.name)
    }
  }
  num.cmds <- length(cmds)

  # Figure out if we will execute commands or not.
  execute <- run.commands & !is.null(environ) & is.environment(environ)

  # print (paste("ddg.parse.commands: .ddg.func.depth =", .ddg.get(".ddg.func.depth")))
  inside.func <- (.ddg.get(".ddg.func.depth") > 0)

  # Attempt to close the previous collapsible command node if a ddg
  # exists
  if (.ddg.is.init() && !inside.func) .ddg.close.last.command.node(environ, initial=TRUE)

  # Get the last command in the new commands and check to see if
  # we need to create a new .ddg.last.cmd node for future reference.
  if (!inside.func) {
      .ddg.last.cmd <- cmds[[num.cmds]]
    # print(paste(".ddg.parse.commands: setting .ddg.last.cmd to", .ddg.last.cmd$text))
    if (.ddg.last.cmd@isDdgFunc) {
      .ddg.last.cmd <- NULL
      #print(".ddg.parse.commands: setting .ddg.last.cmd to null")
    }

    else if (!execute) {
      cmds <- cmds[1:num.cmds-1]
    }
  }

  # Create start and end nodes to allow collapsing of consecutive
  # console nodes. Don't bother doing this if there is only 1 new
  # command in the history or execution.
  named.node.set <- FALSE
  start.node.created <- ""

  if (num.cmds > 0 && .ddg.is.init() && !inside.func && !called.from.ddg.eval) {
    # print(paste("ddg.new.parse.commands: Creating Start for", node.name))
    .ddg.add.abstract.node("Start", node.name = node.name, env = environ)
    named.node.set <- TRUE
    start.node.created <- node.name
  }

  # Don't set .ddg.last.cmd.  We want it to have the value from
  # the last call. We set it at the end of this function:
  # .ddg.set(".ddg.last.cmd", .ddg.last.cmd)

  # Create an operation node for each command.  We can't use lapply
  # here because we need to process the commands in order and
  # lapply does not guarantee an order. Also decide which data nodes
  # and edges to create. Only create a data node for the last
  # write of a variable and only if that occurs after the last
  # possible writer. Create an edge for a data use as long as the
  # use happens before the first writer/possible writer or after
  # the last writer/possible writer. Lastly, if execute is set to
  # true, then execute each command immediately before attempting
  # to create the DDG nodes.

  # Only go through this if  we have at least one command to parse.
  if (num.cmds > 0) {

    # Find where all the variables are assigned for non-environ
    # files.
    if (!execute) {
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
      if (.ddg.enable.source() && grepl("^ddg.eval", cmd@text) && .ddg.enable.console()) {
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

      control.statement <- (st.type == "if" || st.type == "for" || st.type == "while" || st.type == "repeat" || st.type == "{")

      loop.statement <- (st.type == "for" || st.type == "while" || st.type == "repeat")

      # Specifies whether or not a procedure node should be created
      # for this command. Basically, if a ddg exists and the
      # command is not a DDG command or a control statement, it should
      # be created. Note that if control statements are annotated,
      # a procedure node is created for each statement inside a control
      # block, so there is no need to create additional nodes for the
      # control statement itself.

      create <- !cmd@isDdgFunc && .ddg.is.init() && .ddg.enable.console() && !(control.statement && .ddg.loop.annotate() && ddg.max.loops() > 0)
      # create <- !cmd@isDdgFunc && .ddg.is.init() && .ddg.enable.console()
      start.finish.created <- FALSE
      cur.cmd.closed <- FALSE

      # If the command does not match one of the ignored patterns.
      if (!any(sapply(ignore.patterns, function(pattern){grepl(pattern, cmd@text)}))) {

        # If sourcing, we want to execute the command.
        if (execute) {
          # Print command.
          if (echo) {
            nd <- nchar(cmd@text)
            do.trunc <- nd > max.deparse.length
            cmd.show <- paste0(substr(cmd@text, 1L, if (do.trunc)
                          max.deparse.length
                        else nd), "\n")
            cat(cmd.show)
          }

          # If we will create a node, then before execution, set
          # this command as a possible abstraction node but only
          # if it's not a call that itself creates abstract nodes.
          if (!cmd@isDdgFunc && cmd@text != "next") {
            .ddg.set(".ddg.possible.last.cmd", cmd)
            .ddg.set (".ddg.cur.cmd", cmd)

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
          #print (paste (".ddg.parse.commands: Evaluating ", cmd@annotated))
          #print (paste ("length(cmd@annotated) =", length(cmd@annotated)))

          result <- withCallingHandlers(
          
              {
                for (annot in cmd@annotated) {
                  #print (paste (".ddg.parse.commands: Evaluating ", paste(annot, collapse = " ")))
                  # Don't set return.value if we are calling a ddg function or we are executing an if-statement
                  if (grepl("^ddg", annot) || grepl("^.ddg", annot) || as.character(.ddg.get.statement.type(annot)) == "if") {
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
              .ddg.proc.node("Operation", cmd@abbrev, cmd@abbrev, functions.called=cmd@functions.called, console=TRUE, cmd=cmd)
              .ddg.proc2proc()

              # create input edges by adding variables to set
              vars.set <- .ddg.add.to.vars.set(vars.set,cmd,i)
              if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding", cmd@abbrev, "information to vars.set, for an error"))
              .ddg.create.data.use.edges.for.console.cmd(vars.set, cmd, i, for.caller=FALSE)

              # create and link to an error node
              ddg.exception.out("error.msg", toString(e) , cmd@abbrev)
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
              .ddg.add.abstract.node("Finish", cmd, environ)
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
            if (stack.length == 2) {
              .ddg.set(".ddg.cur.cmd.stack", vector())
            }
            else {
              .ddg.set(".ddg.cur.cmd.stack", .ddg.cur.cmd.stack[1:(stack.length-2)])
            }
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
          
          .ddg.proc.node("Operation", cmd@abbrev, cmd@abbrev, functions.called=cmd@functions.called, console=TRUE, cmd=cmd)
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
          if (execute) {
            # Add variables to set.
            vars.set <- .ddg.add.to.vars.set(vars.set,cmd,i)

            if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding", cmd@abbrev, "information to vars.set"))
          }

          .ddg.create.data.use.edges.for.console.cmd(vars.set, cmd, i, for.caller=FALSE)

          .ddg.create.file.read.nodes.and.edges()
          .ddg.link.function.returns(cmd)

          if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding input data nodes for", cmd@abbrev))

          .ddg.create.data.set.edges.for.cmd(vars.set, cmd, i, d.environ)

          if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding output data nodes for", cmd@abbrev))

          .ddg.create.file.write.nodes.and.edges ()
          .ddg.create.graphics.nodes.and.edges ()
        }
        # We wanted to create it but it matched a last command node.
        else if (create && execute) {
          .ddg.close.last.command.node(environ, initial=TRUE)
          if (execute) {
            # Add variables to set.
            vars.set <- .ddg.add.to.vars.set(vars.set,cmd, i)
            if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding", cmd@abbrev, "information to vars.set"))
            .ddg.create.data.set.edges.for.cmd(vars.set, cmd, i, environ)
          }
        }

        if (create.procedure && execute) {
          .ddg.create.data.node.for.possible.writes(vars.set, last.proc.node, env=environ)

          # Update so we don't set these again.
          vars.set$possible.last.writer <- vars.set$last.writer
        }
      }
     }

     # Create a data node for each variable that might have been set in
     # something other than a simple assignment, with an edge from the
     # last node in the console block or source .
     if (!execute) {
       .ddg.create.data.node.for.possible.writes(vars.set, last.proc.node, env=environ)
     }
  }

  #print("Done with ddg.parse.commands loop")

  # Close any node left open during execution.
  if (execute && !inside.func) .ddg.close.last.command.node(environ, initial=TRUE)

  # Close the console block if we processed anything and the DDG
  # is initialized (also, save).
  #
  if (.ddg.is.init() && named.node.set && !inside.func) {
      .ddg.add.abstract.node("Finish", node.name = node.name, env=environ)
  }

  # Open up a new collapsible node in case we need to parse
  # further later.
  if (!execute) {

    .ddg.set(".ddg.possible.last.cmd", .ddg.last.cmd)
    .ddg.set(".ddg.last.cmd", .ddg.last.cmd)
    .ddg.open.new.command.node(environ)
  }

  # Write time stamp to history.
  if (.ddg.is.init() && !.ddg.is.sourced()) .ddg.write.timestamp.to.history()

  return.value <- .ddg.get (".ddg.last.R.value")
  #if (typeof(return.value) != "closure") {
  #  print(paste(".ddg.parse.commands: returning ", return.value))
  #}
  return(return.value)
}


# Finds and returns the names of function calls to external packages,
# as well as the names of the packages used.
#
# @param function.names 
# @return
#   ddg.fun | ddg.lib

.ddg.get.function.info <- function( function.names )
{
  # edge case: no functions/potential function calls
  if( all(sapply(function.names, is.null)) )
    return(NA)
  
  # functions with unknown libraries
  ddg.fun <- function.names[[1]]
  ddg.lib <- NULL
  
  # identify which of the variable names are functions
  if( ! is.null(function.names[[2]]) )
  {
    vars <- sapply( function.names[[2]] , 
                    function(name) {
                      if( ! .ddg.is.set(name) )
                        return(NULL)
                      else
                        return( get(name) )
                    } )
    vars <- sapply( vars , is.function )
    
    # append to list of functions with unknown libraries
    ddg.fun <- append( ddg.fun , names(vars[vars == TRUE]) )
  }
  
  # obtain library information from functions
  fn.frame <- function.names[[3]]
  
  if( length(ddg.fun) > 0 )
  {
    ddg.lib <- sapply( ddg.fun , .ddg.where )
    ddg.lib <- sapply( ddg.lib , environmentName )
    
    ddg.lib <- ddg.lib[ grepl("package:", ddg.lib) ]
    
    # combine with functions with known library calls into data frame
    if( length(ddg.lib) > 0 )
    {
      ddg.lib <- mapply( substring , ddg.lib , 9 )
      
      ddg.fun <- names(ddg.lib)
      ddg.lib <- unname(ddg.lib)
      
      fn.frame <- rbind( fn.frame , data.frame(ddg.fun, ddg.lib, stringsAsFactors=FALSE) )
    }
  }
  
  # return
  fn.frame <- unique(fn.frame)
  return( fn.frame )
}


# Returns TRUE if the value of the given variable name is a data frame
# containing at least one factor. Returns FALSE otherwise.
# var - the variable name

.ddg.var.contains.factor <- function( var )
{
  value <- get(var)

  if( is.data.frame(value) )
    return( is.element("factor",sapply(value,class)) )

  return(FALSE)
}

# .ddg.replace.quotes quotes quotation characters. It also replaces
# return, newline and tab characters with spaces.

# str - input string.

.ddg.replace.quotes <- function(str) {
  #print(paste(".ddg.replace.quotes start, str =", str))
  if (!is.character(str)) return (str)

  # Replace returns, new lines, and tabs with spaces.
  str <- gsub("\r", " ", str)
  str <- gsub("\n", " ", str)
  str <- gsub("\t", " ", str)
  #print(paste(".ddg.replace.quotes end, str =", str))
  return(str)
}

# .ddg.convert.list.to.string converts a list of values to a string
# by calling as.character on each element in the list.

# dvalue - a list of values.

.ddg.convert.list.to.string <- function (dvalue) {
  values <- .ddg.replace.quotes(lapply(dvalue, .ddg.as.character))
  positions <- 1:length(values)
  paste("[[", positions, "]]", values, collapse="\n")
}

# .ddg.as.character wraps an exception handler around as.character
# The exception handler captures the print output for the value and
# returns that instead.
.ddg.as.character <- function (value) {
  tryCatch (as.character(value),
            error=function(e) {capture.output(print(value))})
}

# .ddg.supported.graphic - the sole purpose of this function is
# to verify that the input file extension is a supported graphic
# type. Currently supported graphics types inlude: jpg, jpeg,
# bmp, png, tiff.

# ext - file extension.

.ddg.supported.graphic <- function(ext){
  return(ext %in% c("jpeg", "jpg", "tiff", "png", "bmp", "pdf"))
}

# .ddg.graphic.snapshot provides factoring of snapshot code.

# fext - file extension.
# dpfile - path and name of file.

.ddg.graphic.snapshot <-function(fext, dpfile) {
  # pdfs require a separate procedure.
  if (fext == "pdf") dev.copy2pdf(file=dpfile)

  # At the moment, all other graphic types can be done by
  # constructing a similar function.
  else {
    # If jpg, we need to change it to jpeg for the function call.
    fext = ifelse(fext == "jpg", "jpeg", fext)

    # First, we create a string, then convert it to an actual R
    # expression and use that as the function.
    strFun <- paste(fext, "(filename=dpfile, width=800, height=500)", sep="")
    parseFun <- function(){eval(parse(text=strFun))}
    dev.copy(parseFun)

    # Turn it off (this switches back to prev device).
    dev.off()
  }
}

#' Saves the contents of a web page referenced by a URL in the data
#' directory
#'
#' @param url the URL as a string
#'
#' @return the name of the file where the copy is stored.  This is 
#'   a relative path beginning with the data directory.
#'
.ddg.url.copy <- function (url) {
  # Get last part of the url.
  file.name <- basename(url)
  
  # Add number to file name.
  dfile <- paste(.ddg.dnum()+1, "-", file.name, sep="")
  
  # Get path plus file name to where the file will be copied
  dpath <- paste(.ddg.path.data(), "/", dfile, sep="")
  
  # Download and save the webpage
  curl_download (url, dpath)
    
  if (.ddg.debug.lib()) print(paste("url.copy: ", url))
  return (paste(.ddg.data.dir(), dfile, sep="/"))
}

# .ddg.insert.error.message issues a warning and inserts an
# exception node after the last procedure step. The name of the node
# is "error.msg" and the value is the error message passed to this
# function.

# msg - error message.
# msg.type - error or warning
# scope - scope for evaluating any data
# doWarn - if true, this function displays a warning

.ddg.insert.error.message <- function(msg, msg.type="error.msg", scope="ddg.library", doWarn = TRUE) {
  if (doWarn) {
    warning(msg)
  }
  .ddg.data.node("Exception", msg.type, msg, scope)
  .ddg.lastproc2data(msg.type, dscope=scope)
}

# .ddg.lookup.function.name gets the name of the calling function
# and returns it as a string. pname may be passed as a string or
# a name. If NULL, pname is obtained from the calling environment.
# Note that it is important that these be macros, not functions,
# due to the use of the substitute function in the body.

# pname - name of procedure node.

.ddg.lookup.function.name <- gtools::defmacro (pname,
    expr =
        # If pname is not provided, get from function call.
        if (is.null(pname)) {
          
          #print(".ddg.lookup.function.name: sys.calls() =")
          #print(sys.calls())

          # Look up function call.
          call <- sys.call(-4)

          # Discard everything after left parenthesis to get
          # function name.

          # pname <- strsplit (as.character(call), "\\(")[[1]][1]
          #print(paste(".ddg.lookup.function.name: typeof(call[[1]] =", typeof(call[[1]])))
          #print(paste(".ddg.lookup.function.name: str(call[[1]] =", str(call[[1]])))
          # If the call uses a closure rather than a function name, we will
          # call the name FUN.
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

# .ddg.lookup.value is used to determine what value to use when
# creating data nodes.  Note that it is important that these be
# macros, not functions, due to the use of the substitute function
# in the body.

# expr - the expression to be evaluted. This can be a string or
#   a name.
# value - the value that was passed in to the calling function.
#   If value already exists, nothing happens. If value is NULL,
#   the expression is evaluated to determine the value.
# env - the environment in which the evaluation is done.
# procname - the name of the calling procedure, used to produce
#   an error message if necessary.
# warn (optional) - if TRUE, warns user that the expression could
#   not be evaluated.

.ddg.lookup.value <- gtools::defmacro(expr, value, env, procname, warn=TRUE,
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

# .ddg.delete.temp deletes any temporary files created during
# the processing of a script. These include the temporary
# history file.

.ddg.delete.temp <- function() {
  # Delete the temporary history file if we made it.
  if (.ddg.is.set('.ddg.history.file')) unlink(.ddg.get('.ddg.history.file'))

  # Clear the environment.
  .ddg.env <- new.env(parent=emptyenv())
}

# .ddg.create.output.nodes creates output nodes for ddg.function
# and ddg.procedure. Outs values must be passed as strings, not
# names, unless the value is a file name.

# fname - the name of the function calling .ddg.create.output.nodes.
# pname - the name of the procedure node.
# outs.graphic - the name of a snapshot node to be used as a
#    file name.  A graphical snapshot is simply a captured image
#    of the graphic device active at the time of the call to
#    ddg.function or ddg.procedure.
# outs.data - a list of names of data nodes.
# outs.exception - a list of names of exception nodes.
# outs.url - a list of names of url nodes.
# outs.file - a list of names of file nodes. Supported file
#   extensions include: .csv, .jpg, .jpeg, .pdf, and .txt.
# graphic.fext - the file extension to be used when saving the
#   captured graphic. Supported extensions are .jpg, .jpeg, .pdf.

.ddg.create.output.nodes<- function(fname, pname, outs.graphic, outs.data, outs.exception, outs.url, outs.file, graphic.fext, env) {

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
          .ddg.lookup.value(name, value, env, fname, warn=FALSE)

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
          .ddg.lookup.value(name, value, env, fname, warn=FALSE)

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
          .ddg.lookup.value(name, value, env, fname, warn=FALSE)

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
          .ddg.lookup.value(name, value, env, fname, warn=FALSE)
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

# .ddg.create.function.nodes creates the procedure node, input
# binding nodes, and output nodes for the function.

# pname - name of procedure node.
# full.call - full function call.
# outs.data, etc (optional) - output nodes.
# auto.created - TRUE if the function node is created automatically
# when a return is found
# env (optional) - the environment local to the function

.ddg.create.function.nodes <- function(pname, call, full.call, outs.graphic=NULL, outs.data=NULL, outs.exception=NULL, outs.url=NULL, outs.file=NULL, graphic.fext="jpeg", env=NULL) {
  # Create the start node
  if (typeof(call[[1]]) == "closure") {
    #print(paste(".ddg.create.function.nodes: pname =", pname))
    .ddg.add.abstract.node ("Start", node.name=pname, env=env)
  }
  else {
    #print(paste(".ddg.create.function.nodes: deparse(call) =", deparse(call)))
    .ddg.add.abstract.node ("Start", node.name=paste(deparse(call), collapse=""), env=env)
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
    # scope <- .ddg.get.scope(args[[1]], for.caller = TRUE)
    bindings <- list()
    for (i in 1:length(args)) bindings[[i]] <-list(args[[i]], param.names[[i]])
    missing.params <- character()

    lapply(bindings,
        function(binding) {
          # Here, arg is the arguments passed IN.
          #print(paste(".ddg.create.function.nodes: binding =", binding))
          arg <- binding[[1]]
          #print(paste(".ddg.create.function.nodes: arg =", arg))

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
          for (var in vars.used) {
            param.scope <- .ddg.get.scope(var, for.caller = TRUE, calls=stack)
            if (.ddg.data.node.exists(var, param.scope)) {
              .ddg.data2proc(as.character(var), param.scope, binding.node.name)
              if (.ddg.debug.lib()) print(paste("param:", var))
            }
          }
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
  #print (".ddg.create.function.nodes creating Operation node")
  .ddg.proc.node("Operation", pname, pname)

  # Link to the definition of the function if the function is defined in this script.
  if (.ddg.data.node.exists(pname, environmentName(.GlobalEnv))) {
    .ddg.data2proc(pname, environmentName(.GlobalEnv), pname)
  }

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

  .ddg.create.output.nodes(fname="ddg.function", pname, outs.graphic, outs.data, outs.exception, outs.url, outs.file, graphic.fext, parent.frame(2))

}

# .ddg.get.frame.number gets the frame number of the closest
# non-library calling function.

# calls - system calls.
# for.caller (optional) - if TRUE, go up one level before searching.

.ddg.get.frame.number <- function(calls, for.caller=FALSE) {
  #print (".ddg.get.frame.number: for.caller =", for.caller)
  if (is.null(calls)) calls <- sys.calls()
  script.func.found <- FALSE
  nframe <- length(calls)
  for (i in nframe:1) {
    call <- sys.call(i)[[1]]
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
      #print(paste(".ddg.get.frame.number: call.func =", call.func))
      # Ignore calls to ddg functions or to the functions that get called from the outermost tryCatch
      # to ddg code.
      if (substr(call.func, 1, 4) != ".ddg" && substr(call.func, 1, 3) != "ddg"
          && substr(call.func, 1, 10) != "doTryCatch" && substr(call.func, 1, 11) != "tryCatchOne"
          && substr(call.func, 1, 12) != "tryCatchList" && substr(call.func, 1, 8) != "tryCatch") {
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


# .ddg.where looks up the environment for the variable specified
# by name.  Adapted from Hadley Wickham, Advanced R programming.

# name - name of variable.
# env (optional) - environment in which to look for variable.
# warning (optional) - set to TRUE if a warning should be thrown when a variable is not found.

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


#.ddg.get.env gets the environment in which name is declared.

# name - variable name.
# for.caller (optional) - if TRUE, go up one level before searching.
# calls (optional) - system calls.

.ddg.get.env <- function(name, for.caller=FALSE, calls=NULL) {
  #print (paste(".ddg.get.env: for.caller =", for.caller))
  if (is.null(calls)) calls <- sys.calls()
  #print(".ddg.get.env getting the frame number")
  fnum <- .ddg.get.frame.number(calls, for.caller)
  #print(paste(".ddg.get.env: fnum =", fnum))
  stopifnot(!is.null(fnum))

  # This statement was broken into two statements so that we
  # can add print statements to .ddg.where without breaking it.  If we don't do that
  # the print output gets captured by capture.output and
  # does not display to the user and also causes the subsequent
  # grepl call in this function to fail.

  # scope <- sub('<environment: (.*)>', '\\1', capture.output(.ddg.where(name, sys.frame(fnum))))
  tryCatch (
    if(!exists(name, sys.frame(fnum), inherits=TRUE)) return(NULL),
    error = function(e) {}
  )
  #print(".ddg.get.env calling .ddg.where")
  env <- .ddg.where(name, sys.frame(fnum))
  #print(".ddg.get.env Done")
  return(env)
}

# .ddg.get.scope gets the id of the closest non-library
# environment.

# name - name of variable.
# for.caller (optional) - if TRUE, go up one level before searching.
# calls (optional) - system calls.
# env (optional) - the environment to get the scope for

.ddg.get.scope <- function(name, for.caller=FALSE, calls=NULL, env=NULL) {
  # Get the environment for the variable call.
  if (is.null(env)) {
    #print (".ddg.get.scope getting the environment")
    env <- .ddg.get.env(name, for.caller, calls)
    #print (".ddg.get.scope getting the environment got env")
  }

  # If no environment found, name does not exist, so scope is
  # undefined.
  if (is.null(env)) return ("undefined")

  #
  scope <- sub('^<environment: (.*)>$', '\\1', capture.output(env)[1])
  if (grepl("undefined", scope)) scope <- "undefined"
  return(scope)
}

# .ddg.is.local returns TRUE if the specified name is local in the
# specified scope.

# name of variable.
# scope of variable.

.ddg.is.local <- function(name, scope) {
  return(exists(name, scope, inherits=FALSE))
}

# Creates a start node for the current command if one has not
# been created already.
.ddg.create.start.for.cur.cmd <- function (call, caller.env) {
  if (.ddg.is.set(".ddg.cur.cmd")) {
    # print("In .ddg.create.start.for.cur.cmd")
    .ddg.cur.cmd <- .ddg.get(".ddg.cur.cmd")
    .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
    stack.length <- length(.ddg.cur.cmd.stack)
    if (stack.length >= 1) {
      last.created <- .ddg.cur.cmd.stack[stack.length]
      # Only create a start node for the current command if we have not already
      # created one and the command is more than just the call to this function
      if (last.created[[1]] == "FALSE") {
        if (.ddg.cur.cmd@text != paste(deparse(call), collapse="")) {
          cmd.abbrev <- .ddg.add.abstract.node ("Start", .ddg.cur.cmd, caller.env)
          .ddg.cur.expr.stack <- .ddg.get(".ddg.cur.expr.stack")
          st.type <- .ddg.get.statement.type(.ddg.cur.cmd@parsed[[1]])
          loop.statement <- (st.type == "for" || st.type == "while" || st.type == "repeat")
          control.statement <- loop.statement || st.type == "if"
          .ddg.create.data.use.edges.for.console.cmd(vars.set = data.frame(), .ddg.cur.cmd, 0, for.caller=!control.statement)

          # Add Details Omitted node before annotated loops if needed.
          if (loop.statement && ddg.first.loop() > 1) {
            ddg.details.omitted()
          }

          # Mark the start node as created on the stack.  Mark it even if we did not
          # create the abstract node above, because we will create it below.
          .ddg.set (".ddg.cur.cmd.stack", c(.ddg.cur.cmd.stack[1:stack.length-1], TRUE))
        }
        else {
          .ddg.set (".ddg.cur.cmd.stack", c(.ddg.cur.cmd.stack[1:stack.length-1], "MATCHES_CALL"))
        }
      }
    }
    #print("Done .ddg.create.start.for.cur.cmd")
  }
}

# .ddg.get.last.cmd returns the last command on the stack.

.ddg.get.last.cmd <- function() {
  .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
  stack.length <- length(.ddg.cur.cmd.stack)
  cmd <- .ddg.cur.cmd.stack[stack.length-1][[1]]
}

# .ddg.remove.last.cmd.start.created removes the last command and
# start.created from the stack.

.ddg.remove.last.cmd.start.created <- function () {
  .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
  stack.length <- length(.ddg.cur.cmd.stack)
  #print(paste(".ddg.remove.last.cmd.start.created: Popping from stack:", .ddg.cur.cmd.stack[stack.length-1]))

  if (stack.length == 2) {
    .ddg.set(".ddg.cur.cmd.stack", vector())
  }
  else {
    .ddg.set(".ddg.cur.cmd.stack", .ddg.cur.cmd.stack[1:(stack.length-2)])
  }
}

# .ddg.break.statement creates a procedure node for a break statement in
# a for, repeat, or while statement. It also adds a finish node for the
# if statement (if any) where the break occurs, adds a finish node
# for the for, repeat, or while loop where the break occurs, and adds a
# finish node for the for, repeat, or while statement.

.ddg.break.statement <- function() {
  # Create procedure node for break statement.
  .ddg.proc.node("Operation", "break", "break")
  .ddg.proc2proc()

  # Get last command from stack.
  cmd <- .ddg.get.last.cmd()
  # Get loop type.
  loop.type <- as.character(cmd@parsed[[1]][[1]])

  # Create finish nodes if break occurs in if statement.
  if (loop.type == "if") {
    # Create finish node for if loop.
    ddg.finish("if")
    # Create finish node for if statement.
    .ddg.add.abstract.node("Finish", cmd, parent.frame())

    # Remove last command & start.created from stack.
    .ddg.remove.last.cmd.start.created()
    # Get last command from stack.
    cmd <- .ddg.get.last.cmd()
    # Get loop type.
    loop.type <- as.character(cmd@parsed[[1]][[1]])
  }

  # Create finish node for for, repeat, or while loop.
  loop.name <- paste(loop.type, "loop")
  ddg.finish(loop.name)

  # Create finish node for for, while, or repeat statement.
  .ddg.add.abstract.node("Finish", cmd, parent.frame())

  # Remove last command & start.created from stack.
  .ddg.remove.last.cmd.start.created()
}

# .ddg.next.statement creates a procedure node for a next statement in
# a for, repeat, or while statement. It also adds a finish node for the
# if statement (if any) where the next occurs and adds a finish node for
# the for, while, or repeat loop where the next occurs.

.ddg.next.statement <- function() {
  # Create procedure node for next statement.
  .ddg.proc.node("Operation", "next", "next")
  .ddg.proc2proc()

  # Get last command from stack.
  cmd <- .ddg.get.last.cmd()
  # Get loop type.
  loop.type <- as.character(cmd@parsed[[1]][[1]])

  # Create finish nodes if break occurs in if statement.
  if (loop.type == "if") {
    # Create finish node for if loop.
    ddg.finish("if")
    # Create finish node for if statement.
    .ddg.add.abstract.node("Finish", cmd, parent.frame())

    # Remove last command & start.created from stack.
    .ddg.remove.last.cmd.start.created()
    # Get last command from stack.
    cmd <- .ddg.get.last.cmd()
    # Get loop type.
    loop.type <- as.character(cmd@parsed[[1]][[1]])
  }

  # Create finish node for for, repeat, or while loop.
  loop.name <- paste(loop.type, "loop")
  ddg.finish(loop.name)
}

# .ddg.markdown takes a Rmd file and extracts the R code and text through
# the purl function in the knitr library. It then annotates the R script
# to insert start and finish nodes based on the chunks the user already
# created. If eval = false, then the chunk will not be added to the DDG. If
# the user has a name for the chunk, then that name will be used, else a chunk
# name "ddg.chunk_1" and higher numbers will be generated.
#
# Important: If in a code chunk, there is an empty line followed by "# ----"
# or "#'", then an extra finish node will be inserted, causing an error.
#
# r.script.path is the path of the original Rmd file
# output.path is the path of the generated R script

.ddg.markdown <- function(r.script.path = NULL, output.path = NULL){

  #generates R script file from markdown file
  knitr::purl(r.script.path, documentation = 2L, quiet = TRUE)

  #moves file to ddg directory
  file.rename(from = paste(getwd(), "/", basename(file_path_sans_ext(r.script.path)), ".R", sep = ""), to = output.path)
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


# .ddg.save.debug.files saves debug files to the debug directory.

.ddg.save.debug.files <- function() 
{
	# Save initial environment table to file.
	fileout <- paste(.ddg.path.debug(), "/initial-environment.csv", sep="")
	ddg.initial.env <- .ddg.initial.env()
	write.csv(ddg.initial.env, fileout, row.names=FALSE)

  .ddg.save.debug.proc.nodes ()
  .ddg.save.debug.data.nodes ()
  .ddg.save.debug.edges()
  .ddg.save.function.table ()

	# save library information to file
	fileout <- paste(.ddg.path.debug(), "/libraries.csv", sep="")
	write.csv(.ddg.installedpackages(), fileout, row.names=FALSE)
	
	# save execution environment information to file
	fileout <- paste(.ddg.path.debug(), "/environment.csv", sep="")
	write.csv(.ddg.exec.env(), fileout, row.names=FALSE)
	
  .ddg.save.return.value.table ()
  .ddg.save.sourced.script.table ()

}

# Returns a data frame of information about the current execution environment.
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
	env$rdtVersion[1] <- toString( packageVersion("RDataTracker") )
	
	# hash algorithm
	env$hashAlgorithm[1] <- .ddg.get(".ddg.hash.algorithm")
	
	# RETURN
	return(env)
}


#--------------------USER FUNCTIONS-----------------------#

# ddg.function creates a procedure node of type Operation for
# procedures implemented as functions in the original R script.
# The function name and input parameters are obtained automatically
# from the calling environment. The outs parameters may be used
# optionally to create output data nodes. These MUST be passed as
# a list of strings, not names, unless the value is a file name.
# Users can right-click on the procedure node in DDG Explorer to
# see the code for the function in the original script. For more
# details on outs parameters, see .ddg.create.output.nodes.

# outs (optional) - a list of names of data nodes to be created as
#    outputs to this procedure node. These MUST be passed as
#    a list of strings, not names, unless the value is a file name.
# graphic.fext (optional) - the file extension for a graphics file.

ddg.function <- function(outs.graphic=NULL, outs.data=NULL, outs.exception=NULL, outs.url=NULL, outs.file=NULL, graphic.fext="jpeg") {
  #print("In ddg.function")
  if (!.ddg.is.init()) return(invisible())

  .ddg.inc(".ddg.func.depth")
  pname <- NULL
  #print("ddg.function: looking up function name")
  .ddg.lookup.function.name(pname)
  #print(paste("ddg.function: pname =", pname))

  .ddg.console.node()

  # Look up input parameters from calling environment.
  call <- sys.call(-1)

  #tokens <- unlist(strsplit (as.character(call), "[(,)]"))
  # match.call expands any argument names to be the
  # full parameter name
  #print(paste("ddg.function: pname =", pname))
  #print(paste("ddg.function: caller =", sys.call(-2)[[1]]))

  # Try to find the full call so that we can bind the parameters
  # by name in the DDG.  In the case that the function being executed
  # has been passed as a parameter to another function and is being
  # called from the context (for example, with lapply and other higher-order
  # functions), the match.call will fail.  In that case, we will use the
  # call as it appears in side the higher-order function.
  full.call <- tryCatch (match.call(sys.function(-1), call=call),
         error = function(e) call)

  # Create start node for the calling statement if one is not already created.
  #print("ddg.function creating start node")
  .ddg.create.start.for.cur.cmd (call, sys.frame(-1))
  #print ("ddg.function creating other nodes")
  .ddg.create.function.nodes(pname, call, full.call, outs.graphic, outs.data, outs.exception, outs.url, outs.file, graphic.fext,
      env = sys.frame(.ddg.get.frame.number(sys.calls())))
  #print("ddg.function ending")
  invisible()
}

# .ddg.find.ddg.return.value.caller.frame.number returns the frame
# number of the first caller to ddg.return.value.  If ddg.return.value
# is called recursively, this will give us the position of the
# earliest one called.

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

# ddg.return.value creates a data node for a function's return value. If
# the function is called from a console command and console mode is
# enabled, a data flow edge will be created linking this node to
# the console command that uses the value. ddg.return.value returns the
# same value as the function (expr) and can be used in place of the
# function's normal return statement(s) if it is the last statement
# in the function.  Otherwise, it should be a parameter to return,
# as in return(ddg.return.value(expr)). If expr is an assignment, nodes
# and edges are created for the assignment.

# expr - the value returned by the function.

ddg.return.value <- function (expr=NULL, cmd.func=NULL) {
  if (!.ddg.is.init()) return(expr)
  
  #print("In ddg.return.value")

  dev.file <- NULL
  parsed.stmt <- NULL
  
  # Capture graphics if dev.off is about to be called.
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
  
    if (!is.null(dev.file)) {
      ddg.file.out (dev.file, pname=return.stmt@abbrev)
      
      # Remove the temporary file
      file.remove(dev.file)
      
      # Add an input edge from the current device
      .ddg.data2proc(dev.node.name, NULL, return.stmt@abbrev)
    }
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
    .ddg.add.abstract.node ("Finish", node.name=pname, env=caller.env)
  }
  else {
    .ddg.add.abstract.node ("Finish", node.name=paste(deparse(call),collapse=""), env=caller.env)
  }

  #print(paste ("ddg.return.value: returning", expr))
  return(expr)
}

# ddg.annotate.inside returns the value of the parameter
# annotate.inside.

.ddg.annotate.inside <- function() {
  return(.ddg.get("ddg.annotate.inside"))
}

# ddg.first.loop returns the value of the parameter first.loop.

ddg.first.loop <- function() {
  return(.ddg.get("ddg.first.loop"))
}

# ddg.max.loops returns the value of the parameter max.loops.

ddg.max.loops <- function() {
  return(.ddg.get("ddg.max.loops"))
}

# ddg.max.snapshot.size returns the value of the parameter
# max.snapshot.size.

ddg.max.snapshot.size <- function() {
  return(.ddg.get("ddg.max.snapshot.size"))
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

# Returns true if we should run the annotated version of a function and
# false if we should run the unannotated version.

ddg.should.run.annotated <- function (func.name) {
  #print("In ddg.should.run.annotated")
  
  # Check if we are in a loop and loop annotations are off
  #print(paste("loop annotate?", .ddg.loop.annotate()))
  #print(paste("inside loop?", .ddg.inside.loop()))
  if (!.ddg.loop.annotate() && .ddg.inside.loop() > 0) return (FALSE)
  
  # Make sure this specific function has not been disabled
  if (!is.null(.ddg.annotate.off()) & func.name %in% .ddg.annotate.off()) return(FALSE)
  
  #print(paste(func.name, "is not in off list"))
  
  # Not annotating functions in general
  # Check if this specific function should be annotated
  if (!is.null(.ddg.annotate.on()) & func.name %in% .ddg.annotate.on()) return(TRUE)
  
  #print(paste(func.name, "is not in on list"))
  
  # If we do not know anything specific about this function, follow the 
  # general rule
  return (.ddg.annotate.inside()) 
}

# ddg.eval evaluates a statement and creates data flow edges from
# variable and function return nodes that are used in the
# statement. If the statement is an assignment statement, it also
# creates a data node for the variable assigned and a corresponding
# data flow edge. If ddg.eval is called from inside a function, cmd.func
# is a function that returns the corresponding DDGStatement object.
# If ddg.eval is called from inside a control block, cmd.func is an
# integer that points to the corresponding DDGStatement object stored
# in the list .ddg.statements.

# statement - the statement to evaluate.

ddg.eval <- function(statement, cmd.func=NULL) {
  #print(paste("ddg.eval: statement =", statement))
  #print(paste("ddg.eval: cmd.func =", cmd.func))

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
    #print("ddg.eval evaluating cmd inside control block")
    # print(paste("ddg.eval:", cmd@text))

  # Statement inside function.
  } else {
    cmd <- cmd.func()
    parsed.statement <- cmd@parsed
    #print("ddg.eval evaluating cmd inside function")
    # print(paste("ddg.eval:", cmd@text))
    # print(paste("pos ="))
    # print(cmd@pos)
  }

  if (.ddg.debug.lib()) print (paste("ddg.eval: statement =", statement))

  frame.num <- .ddg.get.frame.number(sys.calls())
  env <- sys.frame(frame.num)

  if (!.ddg.is.init()) {
    # print ("ddg.eval:  no ddg!")
    return(eval(parsed.statement, env))
  }

  #print (paste("ddg.eval: statement =", statement))
  
  if (!.ddg.enable.source()) {
    # print("ddg.eval:  Creating console node")
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

  #print(paste("ddg.eval: Passing to .ddg.parse.commands as node.name:", statement))
  #print(paste("ddg.eval: cmd@abbrev =", cmd@abbrev))
  #print(paste("ddg.eval: Calling .ddg.parse.commands with ", deparse(parsed.statement)))
  return.value <- .ddg.parse.commands(parsed.statement, environ=env, run.commands = TRUE, node.name=statement, called.from.ddg.eval=TRUE, cmds=list(cmd))
  # cmd <- .ddg.parse.commands(parsed.statement, environ=env, run.commands = TRUE, node.name=statement, called.from.ddg.eval=TRUE, cmds=list(cmd))

  if (.ddg.get(".ddg.func.depth")) {
    if (!is.null(cmd)) {
      .ddg.link.function.returns(cmd)
    }
    # .ddg.link.function.returns(statement)
  }

  # Create outflowing edges .
  # .ddg.create.data.set.edges.for.cmd(cmd@vars.set, cmd, 1, env)

  #print(paste("ddg.eval: returning from", deparse(parsed.statement), "with", return.value))
  return (return.value)
}

# ddg.data creates a data node for a single or complex data value.
# If the value is omitted, the argument passed in for dname is
# evaluated in the calling environment to determine the value.
# If the value is determined to be complex, the output data is
# written out to a csv if possible. Otherwise, the data are
# written out as a .txt file if the variable is determined to
# be an object.

# dname - the label for the node. This can be passed as a string, name,
#   or expression.
# dvalue (optional) - the value of the node.
# graphic.fext (optional) - the file extention to be used for
#   saving the variable if it is a graphical output. Otherwise
#   ignored. Default is jpeg.


ddg.data <- function(dname, dvalue=NULL, graphic.fext = "jpeg") {
  if (!.ddg.is.init()) return(invisible())

  # Look up the value if one was not provided.
  env <- parent.frame()
  .ddg.lookup.value(dname, dvalue, env, "ddg.data")

  # Save the value appropriately.  If the name is not a string,
  # use the argument instead of the value.
  if (!is.character(dname)) dname <- deparse(substitute(dname))
  .ddg.save.data(dname, dvalue, graphic.fext, env=env)
}

# ddg.file creates a data node of type File by copying an existing
# file to the DDG directory.

# filename - the name of the file to copy, including path to the file
#   if it is not in the working directory.
# dname (optional) - the label for the node. If omitted, the filename,
#   minus the directory path, is used as the label.

ddg.file <- function(filename, dname=NULL) {
  if (!.ddg.is.init()) return(invisible())

  scope <- if (!is.null(dname)) .ddg.get.scope(dname)
       else NULL
  invisible(.ddg.file.copy(filename, dname, scope))
}

# ddg.data.in creates a data flow edge from data node dname to
# procedure node pname.

# dname - the name of the data node.  This can be passed as
#   a string, name, or expression.
# pname (optional) - the name of the procedure that created
#   this data value.  This can be passed as a string or as
#   a name. It may be omitted if ddg.data.in is called by a function,
#   in which case the name of the function will be used.

ddg.data.in <- function(dname, pname=NULL) {
  if (!.ddg.is.init()) return(invisible())

  .ddg.lookup.function.name(pname)

  arg <- substitute(dname)
  if (!is.character(arg)) {
    argname <- deparse(arg)
    dscope <- .ddg.get.scope(argname)
    if (.ddg.data.node.exists(argname, dscope)) {
      dname <- argname
    }
    else {
      dscope <- .ddg.get.scope(argname, for.caller=TRUE)
      if (.ddg.data.node.exists(argname, dscope)) {
        dname <- argname
      }
      else {
        # This case is for file names.  The table records the file
        # name, using the scope "undefined".
        dscope <- "undefined"
        if (!is.character(dname) || !.ddg.data.node.exists(dname, dscope)) {
          error.msg <- paste("No data node found for", arg)
          .ddg.insert.error.message(error.msg)
          return()
        }
      }
    }
  }
  else if (exists (arg, envir=parent.frame(), inherits=TRUE)) {
    dscope <- .ddg.get.scope(dname)
  }
  else if (exists (arg, envir=parent.frame(2), inherits=TRUE)) {
    dscope <- .ddg.get.scope(dname, for.caller=TRUE)
  }
  else {
    dscope <- environmentName(.GlobalEnv)
  }

  # Create data flow edge from data node to operation node.
  .ddg.data2proc(dname, dscope, pname)
}

# ddg.data.out creates a data or snapshot node of type Data.
# It also creates a data flow edge from procedure node pname
# to the output node. Used for simple or complex data values.

# dname - the label for the data node being created. This can
#   be passed as a string, name, or expression. Complex data
#   are written to the file dname.
# dvalue (optional) - the value to associate with the node.
#   If no value is given, the argument passed in for dname is
#   evaluated in the calling environment.
# pname (optional) - the name of the procedure that created the
#   data. This can be passed as a string or name. It may be
#   omitted if ddg.data.out is called by a function, in which
#   case the name of the function will be used.
# graphic.fext (optional) - the file extension that should be
#   used when saving a graphics file. Ignored unless the value
#   to be saved is determined to be a graphic.

ddg.data.out <- function(dname, dvalue=NULL, pname=NULL, graphic.fext="jpeg") {
  if (!.ddg.is.init()) return(invisible())

  # If no value is provided, get value in calling environment.
  env <- parent.frame()
  .ddg.lookup.value(dname, dvalue, env, "ddg.data.out")

  # Convert name to a string if necessary.
  if (!is.character(dname)) dname <- deparse(substitute(dname))

  # Save the complex data in appropriate format.
  .ddg.save.data(dname, dvalue, graphic.fext, env=env)

  .ddg.lookup.function.name(pname)

  # Create data flow edge from operation node to data node.
  .ddg.proc2data(pname, dname)
}

# ddg.exception.out creates a data node of type Exception. It
# also creates a data flow edge from the procedure node pname
# to this node.

# dname - the label for the exception node being created.
#   This can be passed as a string or name.
# dvalue (optional) - the value to associate  with the node.
#   If no value is given, the argument passed in for dname is
#   evaluated in the calling environment.
# pname (optional) - the name of the procedure that created this
#   exception. This can be passed as a string or as name. It
#   may be ommited if ddg.exception.out is called by a function,
#   in which case the name of the function will be used.

ddg.exception.out <- function(dname, dvalue=NULL, pname=NULL) {
  if (!.ddg.is.init()) return(invisible())

  # If no value is provided, get value in calling environment.
  env <- parent.frame()
  .ddg.lookup.value(dname, dvalue, env, "ddg.exception.out")

  # Create output exception node.
  .ddg.data.node("Exception", dname, dvalue, "ddg.library")

  .ddg.lookup.function.name(pname)

  # Create data flow edge from procedure node to exception node.
  .ddg.proc2data(pname, dname)
}

# ddg.url.out creates a data node of type URL called dname with
# address dvalue. It also creates a data flow edge from procedure
# node pname to the URL node dname. Use for URL addresses.

# dname - the label for the data node being created.
# dvalue (optional) - the full URL. If a value is not provided,
#   the argument passed in for dname is evaluated in the calling
#   environment to determine the value.
# pname (optional) - the name of the procedure that created this
#   URL node. This can be passed as a string or as a name. It may
#   be omitted if ddg.url.out is called by a function, in which
#   case the name of the function will be used.

ddg.url.out <- function(dname, dvalue=NULL, pname=NULL) {
  if (!.ddg.is.init()) return(invisible())

  # If no value is provided, get value in calling environment.
  env <- parent.frame()
  .ddg.lookup.value(dname, dvalue, env, "ddg.url.out")

  # URL labels are not necessarily variables, so make sure
  # it is a variable before trying to determine its scope.
  if (exists (dname, inherits = TRUE)) {
    dscope <- .ddg.get.scope(dname)
  }
  else {
    dscope <- environmentName(.GlobalEnv)
  }

  # Create output URL node where dvalue = address.
  .ddg.data.node("URL", dname, dvalue, dscope)

  .ddg.lookup.function.name(pname)

  # Create data flow edge from operation node to URL node.
  .ddg.proc2data(pname, dname, dscope)
}

# ddg.file.out creates a data node of type File called dname by
# copying an existing file to the DDG directory. A data flow edge
# is also created from procedure node pname to data node dname.
# Use for output files already created by the main script. Returns
# the full path to the file that is saved.

# filename - name of the file.  The name should include the path
#   to the file if it is not in the working directory.
# dname (optional) - the label for the node being created. If
#   omitted, the filename, minus the directory path, is used as
#   the label.
# pname (optional) - the name of the procedure that created this
#   node. This can be passed as a string or as a name. It may be
#   omitted if ddg.file.out is called by a function, in which
#   case the name of the function is used.

ddg.file.out <- function(filename, dname=NULL, pname=NULL) {
  if (!.ddg.is.init()) return(invisible())
  
  # Adds the files written to ddg.outfilenodes for use in determining reads
  # and writes in the hashtable.
  .ddg.add.outfiles (filename)
  
  if (is.null(dname)) {
    dname <- basename(filename)
    scope <- NULL
  }
  else {
    scope <- .ddg.get.scope (dname)
  }
  
  # Create output file node called filename and copy file.
  #print(paste("ddg.file.out copying ", filename))
  saved.file <- .ddg.file.copy(filename, dname, scope)
  #print(paste("ddg.file.out done copying ", filename))
  
  .ddg.lookup.function.name(pname)
  
  # Create data flow edge from operation node to file node.
  .ddg.proc2data(pname, dname, scope)
  
  return (saved.file)
}

# ddg.graphic.out creates a data node of type Snapshot called
# dname by capturing the current image in the active graphics
# device and saving it in the DDG directory. The name of the
# file is dname plus the extension specified by the fext
# parameter. Available extensions are bmp, jpeg, png, and tiff.
# A data flow edge is also created from procedure pname to the
# data node dname.

# dname - the label for the node being created.
# pname (optional) - the name of the procedure that created this
#   node. This can be passed as a string or as a name. It may be
#   omitted if ddg.graphic.out is called by a function, in which
#   case the name of the function is used.
# fext (optional) - the file extention to be used for the captured
#   image file. If omitted, this value defaults to jpeg.

ddg.graphic.out <- function(dname, pname=NULL, graphic.fext="jpeg") {
  if(!.ddg.is.init()) return
  # Write out the graphic.
  .ddg.write.graphic(dname, 'Graphical Plot. Not saved in script.', graphic.fext)

  .ddg.lookup.function.name(pname)

  # Create the data flow edge from oepration node to the file node.
  .ddg.proc2data(pname,dname)
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
