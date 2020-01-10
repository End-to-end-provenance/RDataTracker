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

######################### RDataTracker.R #########################

# This file contains core functions for RDataTracker / provR.


# Create DDG environment variable.

.ddg.env <- new.env(parent=emptyenv())

#-------- FUNCTIONS TO MANAGE THE GLOBAL VARIABLES--------#

##### Getters for specific variables

#' .ddg.tool.name returns the name of the provenance collection tool.
#' @return the name of the provenance collection tool
#' @noRd

.ddg.tool.name <- function() {
  return(.ddg.get("ddg.tool.name"))
}

#' .ddg.save.debug returns True if debugging information should be saved
#' to the file system
#' @return TRUE if saving debugging information
#' @noRd

.ddg.save.debug <- function() {
  return(.ddg.get("ddg.save.debug"))
}

#' .ddg.initial.env returns an environment containing names bound before
#'   the script was executed
#' @return an environment containing names previously bound
#' @noRd

.ddg.initial.env <- function() {
  return(.ddg.get("ddg.initial.env"))
}

#' .ddg.annotate.on returns the names of functions that the user explicity said
#' should be annotated
#' @return the names of functions to be annotated
#' @noRd

.ddg.annotate.on <- function() {
  return (.ddg.get("ddg.annotate.on"))
}

#' .ddg.annotate.off returns the names of functions that the user explicitly said
#' should not be annotaed
#' @return the names of functions not be annotated
#' @noRd

.ddg.annotate.off <- function() {
  return (.ddg.get("ddg.annotate.off"))
}

#' .ddg.enable.source returns True if the commands are coming from a script file
#' @return TRUE if commands are from a script
#' @noRd

.ddg.enable.source <- function() {
  return(.ddg.is.set("from.source") && .ddg.get("from.source"))
}

#' .ddg.set.details.omitted keeps track of whether the last loop has all iterations 
#' recorded or not.
#' @param value if TRUE, it means that not all iterations are recorded
#' @return nothing
#' @noRd

.ddg.set.details.omitted <- function (value) {
  .ddg.set ("details.omitted", value)
}

#' .ddg.were.details.omitted returns True if provenance is incomplete at this point
#' @return TRUE if provenance is incomplete at this point
#' @noRd

.ddg.were.details.omitted <- function () {
  .ddg.get ("details.omitted")
}

#' .ddg.run.args returns the run arguments for this provenance in a named list.
#' These are the arguments from prov.run, prov.init, prov.save, prov.quit.
#' @return The run arguments for this provenance.
#' @noRd

.ddg.run.args <- function() {
  .ddg.get("ddg.run.args")
}

#' .ddg.set.warning is attached as a handler when we evaluate
#' expressions.  It saves the warning so that a warning
#' node can be created after the procedural node that
#' corresponds to the expression that caused the warning
#' @param w the simplewarning object created by R
#' @return nothing
#' @noRd

.ddg.set.warning <- function(w) {
  # Only save warnings if the warn level is set to report them at all.
  # This is important because we do temporarily set the warning level
  # to avoid warnings that RDT might cause that are safe to ignore.
  # Search for calls to the option function to see where that happens.
  if (getOption("warn") >= 0) {
    .ddg.set("ddg.warning", w)
  }
}

#' .ddg.clear.warning clears the warning 
#' @return nothing
#' @noRd

.ddg.clear.warning <- function() {
  .ddg.set("ddg.warning", NA)
}

#' .ddg.get.warning returns the last saved warning
#' @return the last saved warning
#' @noRd

.ddg.get.warning <- function () {
  return (.ddg.get("ddg.warning"))
}

#' .ddg.warning.occurred returns True if there is a currrently saved warning
#' @return true if there is currently a saved warning
#' @noRd

.ddg.warning.occurred <- function() {
  return (.ddg.is.set("ddg.warning") && !is.na(.ddg.get("ddg.warning")))
}


#-------------------BASIC FUNCTIONS-----------------------#

#' .ddg.get.initial.env creates a table of non-ddg objects present in the
#' R environment before the script is executed.  This is only used for 
#' debugging.
#' @return nothing
#' @noRd

.ddg.get.initial.env <- function() {
  e <- globalenv()
  e.ls <- ls(e, all.names=TRUE)

  not.ddg.func <- function (name) {
    return (!grepl("^ddg", name) && !grepl("^.ddg", name) && !grepl("^prov", name) 
      && name != ".onLoad")
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
#' optionally saved as tab-delimited files in prov.save.
#' @return nothing
#' @noRd

.ddg.init.tables <- function() {
  .ddg.get.initial.env()
  
  .ddg.init.proc.nodes()
  .ddg.init.data.nodes()
  .ddg.init.edges()
  .ddg.init.function.table()
  
  # Used to control debugging output.  If already defined, don't
  # change its value.
  if (!.ddg.is.set("ddg.debug.lib")) .ddg.set("ddg.debug.lib", FALSE)

  # Used to control sourcing. If already defined, don't change
  # its value.
  if (!.ddg.is.set("from.source")) .ddg.set("from.source", FALSE)

  # Record last command from the preceding console block.
  .ddg.set("ddg.last.cmd", NULL)
  
  # Record the current command to be opened during console execution
  # (used when executing a script using .ddg.source).
  .ddg.set("ddg.possible.last.cmd", NULL)

  # Store path of current ddg
  .ddg.set("ddg.path", NULL)
  
  # Initialize sourced scripts information
  .ddg.init.sourced.scripts ()

  # Save debug files on debug directory
  .ddg.set("ddg.save.debug", FALSE)
  
  # Initialize DDG Statements
  .ddg.init.statements ()
  
  # Initialize the stack corresponding to the names of start/finish nodes.
  .ddg.set("ddg.start.stack", vector())
  
  # Remember if an error node has been created during the processing
  # of the last statement so we only create one.
  .ddg.set ("ddg.error.node.created", FALSE)
}

#' .ddg.init.environ() sets up the filesystem and R environments for use. 
#' @return nothing
#' @noRd

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
#' @noRd

.ddg.is.init <- function() {
    # Short circuits evaluation.
    return(.ddg.is.set("ddg.initialized") && .ddg.get("ddg.initialized"))
}

#' .ddg.is.nonlocal.assign returns TRUE if the object passed is an
#' expression object containing a non-local assignment.
#' @param expr input expression.
#' @return TRUE if the expression is an assignment statement using the <<- operator.
#' @noRd

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
#' @param var.table.size desired size of the data frame. Negative values
#'   and 0 are coerced to 1.
#' @return The data frame is structured as follows: \cr
#' - the variable name.\cr
#' - the position of the statement that wrote the variable last.\cr
#' - the position of the last statement that may have assigned to a
#'   variable.\cr 
#' @noRd

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
#' @noRd

.ddg.double.vars.set <- function(vars.set) {
  size <- nrow(vars.set)
  
  # Create the right size data frame from input frame.
  new.vars.set <- rbind(vars.set, .ddg.create.empty.vars.set(size))

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
#' @noRd

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
#' @noRd

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
#' @noRd

.ddg.create.data.use.edges <- function (cmd, for.caller, env=NULL) {
  # Find all the variables used in this command.
  #print (paste(".ddg.create.data.use.edges: cmd = ", cmd@text))
  vars.used <- cmd@vars.used

  for (var in vars.used) {
    var <- .ddg.extract.var (var, env)
    
    #print(paste(".ddg.create.data.use.edges: var =", var))
    # Make sure there is a node we could connect to.
    scope <- .ddg.get.scope(var, for.caller, env=env)

    #print(paste(".ddg.create.data.use.edges: scope =", scope))

    if (.ddg.data.node.exists(var, scope)) {
      #print(".ddg.create.data.use.edges found data node")
      .ddg.data2proc(var, scope, cmd@abbrev)
    }
    else {
      scope <- .ddg.get.scope(var, for.caller)
      if (.ddg.data.node.exists(var, scope)) {
        .ddg.data2proc(var, scope, cmd@abbrev)
      }
    }
    
    # If the variable is inside an environment, like env$var, we also
    # create nodes and edges for using the env.
    var.env <- .ddg.extract.env (var, env)
    if (!is.null(var.env)){
      if (.ddg.data.node.exists(var.env, scope)) {
        #print(".ddg.create.data.use.edges found data node for the environment")
        .ddg.data2proc(var.env, scope, cmd@abbrev)
      }
      else {
        scope <- .ddg.get.scope(var.env, for.caller)
        if (.ddg.data.node.exists(var.env, scope)) {
          .ddg.data2proc(var.env, scope, cmd@abbrev)
        }
      }
    }
    
  }
  #print (".ddg.create.data.use.edges Done")
}

#' .ddg.extract.var is given an expression that is either a variable
#' or an expression with $ in it, like env$var or df$col and finds the part
#' of it that represents a variable, returning the variable as
#' a string.  If the $ operator is being used with an environment, like
#' env$var, "env$var" is returned.  If it is the column of a data frame,
#' like df$col, df is returned.
#' @param var a string holding either a variable or an expression including
#'   the $ operator
#' @param env the environment in which var could be evaluated.  If NULL,
#'   the environment is searched for.
#' @return the portion of var that actually represents the variable
#' @noRd 

.ddg.extract.var <- function (var, env=NULL) {
  # $ is used both to access variables in an environment and 
  # columns in a data frame.  In the former case, we want the 
  # qualified name env$var to be the thing being used.  For a 
  # dataframe, we want to note that the data frame is being used.
  #print (paste ("In .ddg.extract.var, var =", var))
  if (stringi::stri_detect_fixed(var, "$")) {
    #print ("Found $ in var")
    parsed <- parse (text=var)[[1]]
    if (parsed[[1]] == "$") {
      #print ("First operator is $")
      if (is.null(env)) {
        env <- .ddg.get.env(deparse(parsed[[2]]))
      }
      
      if (!is.environment(eval(parsed[[2]], env))) {
        #print ("Not an environment")
        var = deparse(parsed[[2]])
      }
    }
  }
  return(var)

}

#' .ddg.extract.env is given an expression that is either a variable
#' or an expression with $ in it, like env$var or df$col and finds the part
#' of it that represents the environment, returning it as
#' a string.  If the $ operator is being used with an environment, like
#' env$var, "env" is returned.  Otherwise NULL is returned.
#' @param var a string holding either a variable or an expression including
#'   the $ operator
#' @param env the environment in which var could be evaluated.  If NULL,
#'   the environment is searched for.
#' @return the portion of var that actually represents the environment, or 
#'   NULL if no environment is involved
#' @noRd 

.ddg.extract.env <- function (var, env=NULL) {
  if (stringi::stri_detect_fixed(var, "$")) {
    parsed <- parse (text=var)[[1]]
    if (parsed[[1]] == "$") {
      if (is.null(env)) {
        env <- .ddg.get.env(deparse(parsed[[2]]))
      }
      
      if (is.environment(eval(parsed[[2]], env))) {
        return(deparse(parsed[[2]]))
      }
    }
  }
  return(NULL)
}


#.ddg.changing.env <- function (cmd, env) {
#  print ("In .ddg.changing.env")
#  vars.assigned <- cmd@vars.set
#  env.to.set <- character()
#  
#  for (var in vars.assigned) {
#    
#    if (stringi::stri_detect_fixed(var, "$")) {
#      print (paste ("Found $ in var", var))
#      parsed <- parse (text=var)[[1]]
#      if (parsed[[1]] == "$") {
#        print ("First operator is $")
#        arg2String <- deparse(parsed[[2]])
#        if (is.null(env)) {
#          env <- .ddg.get.env(arg2String)
#        }
#        
#        arg2 <- eval(parsed[[2]], env)
#        
#        if (is.environment(arg2)) {
#          print ("Found environment")
#          if (!exists (deparse(parsed[[3]]), envir = arg2)) {
#            print (paste (deparse(parsed[[3]], "is a new var in the environment")))
#             env.to.set <- c(env.to.set, arg2String)  
#          } 
#        }
#      }
#    }
#  }
#  
#  print (".ddg.changing.env is returning")
#  print (env.to.set)
#  return (env.to.set)
#  
#}

#' .ddg.create.data.set.edges creates data nodes for 
#' variables being set, saves the data, and creates edges from the
#' procedure node that set the variable to the new data node.  These
#' nodes and edges correspond to the variables set in the command passed in.
#' @param vars.set variable assignment data frame.
#' @param cmd the command to create edges for
#' @param env environment to use for evaluating variables set.
#' @return nothing
#' @noRd

.ddg.create.data.set.edges <- function(vars.set, cmd, env, captured.output = NULL) {
  #print(paste("In .ddg.create.data.set.edges.for.cmd: cmd = ", cmd@abbrev))
  vars.assigned <- cmd@vars.set
  
  for (var in vars.assigned) {

    #print(paste(".ddg.create.data.set.edges.for.cmd: var = ", var))
    
    # Check for a new ggplot that was not assigned to a variable
    if (.ddg.get ("ddg.ggplot.created")) {
      if (var == "") {      
        # Add a data node for the plot and link it in.
        # Set ddg.last.ggplot to the name of this node
        .ddg.data.node("Data", ".ggplot", ggplot2::last_plot(), NULL)
        .ddg.lastproc2data(".ggplot")
        .ddg.set("ddg.last.ggplot", ".ggplot")
      }
      .ddg.set ("ddg.ggplot.created", FALSE)
    }
    
    if (var != "") {
      # Create the nodes and edges for setting the variable
      var <- .ddg.extract.var (var, env)
      scope <- .ddg.get.scope(var, env=env)
      .ddg.save.var(var, env, scope)
      .ddg.proc2data(cmd@abbrev, var, scope)
      
      # If the variable is inside an environment, like env$var
      # and var was not previously in the environment, we also
      # create nodes and edges for change env.
      var.env <- .ddg.extract.env (var, env)
      if (!is.null(var.env)){
        envList <- .ddg.get ("ddg.envList")
        oldEnvContents <- envList[[var.env]]
        
        # Checks if this is the first variable set
        if (is.null(oldEnvContents)) {
          .ddg.save.var(var.env, env, scope)
          .ddg.proc2data(cmd@abbrev, var.env, scope)
        }
        else {
          # Checks if the variable being set was already in the environment
          internal.var <- substring (var, stringi::stri_locate_first_fixed(var, "$")[1,"start"]+1)
          if (!(internal.var %in% oldEnvContents)) {
            .ddg.save.var(var.env, env, scope)
            .ddg.proc2data(cmd@abbrev, var.env, scope)
          }
        }
      }
    }
  }
  
  if (!is.null(captured.output) && length(captured.output) > 0) {
    #cat(captured.output)
    #print(paste("length(captured.output)",length(captured.output)))
    .ddg.data.node("StandardOutput", "output", captured.output, "Standard output");
    .ddg.proc2data(cmd@abbrev, "output", "Standard output")
  }
}

#' .ddg.save.var
#' 
#' Creates a node for a variable
#' 
#' @param var the variable to create a node for
#' @param env the environment in which the variable lives.  If NULL, it
#'   finds the closest environment with that variable
#' @param scope the scope for the environment.  If NULL, it looks it up.
#' @return nothing
#' @noRd
.ddg.save.var <- function(var, env=NULL, scope=NULL) {
  if (is.null(env)) {
    env <- .ddg.get.env(var)
    
    # If no environment is found defining this variable, do not 
    # save it.  It means that the variable is from a scope that
    # is not available at the current line of code, such as a 
    # non-local, yet not global scope.
    if (is.null(env)) {
      return()
    }
  }
  if (is.null(scope)) {
    scope <- .ddg.get.scope(var, env=env)
  }

  
  # Special operators are defined by enclosing the name in `.  However,
  # the R parser drops those characters when we deparse, so when we parse
  # here they are missing and we get an error about unexpected SPECIAL
  # characters.  The first tryCatch, puts the ` back in and parses again.
  # The second tryCatch handles errors associated with evaluating the variable.
  parsed <- tryCatch(parse(text=var),
      error = function(e) parse(text=paste("`", var, "`", sep="")))
  val <- tryCatch(eval(parsed, env),
      error = function(e) {
        eval (parse(text=var), parent.env(env))
      }
  )
  
  tryCatch(.ddg.save.data(var, val, error=TRUE, scope=scope, env=env),
      error = 
          function(e){
        .ddg.data.node("Data", var, "complex", scope); 
        print(e)
      }
  )
}

#' .ddg.create.data.node.for.possible.writes creates a data node for
#' each variable that might have been set in something other than a
#' simple assignment.  An edge is created from the last node in the
#' console block.
#' @param vars.set variable assignment data frame.
#' @param last.command last command in console block.
#' @param env the environment that the command was executed in
#' @return nothing
#' @noRd

.ddg.create.data.node.for.possible.writes <- function (vars.set, last.command, 
                                                       env= NULL) {
  #print("In .ddg.create.data.node.for.possible.writes")
  environment <- if (is.environment(env)) env else .GlobalEnv

  for (i in 1:nrow(vars.set)) {
    # print(paste("Checking ", vars.set$variable[i]))
    if (vars.set$possible.last.writer[i] > vars.set$last.writer[i]) {
      value <- tryCatch(eval(parse(text=vars.set$variable[i]), environment),
          error = function(e) {
            #print(paste("Could not find value for", vars.set$variable[i], 
            #            "in environment", environment))
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

#' .ddg.add.start.node creates a start node and its incoming control flow edge.  
#' @param cmd The DDGStatement object for the command being started
#' @param node.name The label to put on the node.  If node.name is not passed in,
#'   the abbreviated label in cmd is used.
#' @param script.num (optional) - the number of the script that the operation is in
#' @param startLine (optional) - the line that the operation starts on
#' @param startCol (optional) - the column that the operation starts on
#' @param endLine (optional) - the line that the operation ends on
#' @param endCol (optional) - the column that the operation ends on
#' @return the label of the node created, excluding "Start"
#' @noRd

.ddg.add.start.node <- function(cmd = NULL, node.name = "",
    script.num=NA, startLine=NA, startCol=NA, endLine=NA, endCol=NA) {
  node.name <- .ddg.add.abstract.node ("Start", cmd, node.name,
      script.num, startLine, startCol, endLine, endCol)
  .ddg.push.start (node.name)
  return (node.name)
}
  
#' .ddg.add.finish.node creates a finish node and its incoming control flow edge.  
#' @param script.num (optional) - the number of the script that the operation is in
#' @param startLine (optional) - the line that the operation starts on
#' @param startCol (optional) - the column that the operation starts on
#' @param endLine (optional) - the line that the operation ends on
#' @param endCol (optional) - the column that the operation ends on
#' @return the label of the node created, excluding "Finish"
#' @noRd

.ddg.add.finish.node <- function(cmd = NULL,
    script.num=NA, startLine=NA, startCol=NA, endLine=NA, endCol=NA) {
  popped <- .ddg.pop.start ()
  node.name <- .ddg.add.abstract.node ("Finish", cmd, node.name = popped,
      script.num, startLine, startCol, endLine, endCol)
  return (node.name)
}

#' .ddg.close.blocks creates finish nodes and incoming control flow edges
#' for each node on the start/finish stack.
#' @return nothing
#' @noRd

.ddg.close.blocks <- function () {
  ddg.start.stack <- .ddg.get("ddg.start.stack")
  #print (paste ("start.stack =", .ddg.start.stack))
  stack.length <- length(ddg.start.stack)
  if (stack.length == 0) {
    return()
  }
  
  for (i in stack.length:1) {
    .ddg.add.finish.node ()
  }
}

#' .ddg.push.start pushes a node to the start/finish stack.
#' @param node.name name of node to push
#' @return nothing
#' @noRd

.ddg.push.start <- function (node.name) {
  ddg.start.stack <- .ddg.get("ddg.start.stack")
  
  if (length(ddg.start.stack) == 0) {
    ddg.start.stack <- node.name
  }
  else {
    ddg.start.stack <- c(ddg.start.stack, node.name)
  }
  .ddg.set("ddg.start.stack", ddg.start.stack)
}

#' .ddg.pop.start pops a node from the start/finish stack.
#' @return the top node on the start/finish stack.
#' @noRd

.ddg.pop.start <- function () {
  ddg.start.stack <- .ddg.get("ddg.start.stack")
  #print (paste ("Before pop, start.stack =", ddg.start.stack))
  stack.length <- length(ddg.start.stack)
  top <- 
      if (stack.length == 0) NULL
      else ddg.start.stack[stack.length]

  if (stack.length == 1) {
    .ddg.set("ddg.start.stack", vector())
  }
  else if (stack.length > 1){
    .ddg.set("ddg.start.stack", ddg.start.stack[1:(stack.length-1)])
  }
  #print (paste ("pop returning", top))
  return (top)
}

#' .ddg.top.start returns the number of nodes on the start/finish
#' stack.
#' @return the number of nodes on the start/finish stack
#' @noRd

.ddg.top.start <- function () {
  ddg.start.stack <- .ddg.get("ddg.start.stack")
  stack.length <- length(ddg.start.stack)
  if (stack.length == 0) {
    return (NULL)
  }
  else {
    return (ddg.start.stack[stack.length])
  }
}

#' .ddg.add.abstract.node creates a start or finish node and its 
#' incoming control flow edge.
#' @param cmd The DDGStatement object for the command being finished
#' @param node.name The label to put on the node.  If node.name is not passed in,
#'   the abbreviated label in cmd is used.
#' @param scriptNum (optional) - the number of the script that the operation is in
#' @param startLine (optional) - the line that the operation starts on
#' @param startCol (optional) - the column that the operation starts on
#' @param endLine (optional) - the line that the operation ends on
#' @param endCol (optional) - the column that the operation ends on
#' @return the label of the node created, excluding "Start" or "Finish"
#' @noRd

.ddg.add.abstract.node <- function(type, cmd = NULL, node.name = "",
    scriptNum=NA, startLine=NA, startCol=NA, endLine=NA, endCol=NA) {
  #print("In .ddg.add.abstract.node")
  
  if (node.name == "") {
      node.name <- cmd@abbrev
  }
  if (.ddg.debug.lib()) print(paste("Adding", node.name,  type, "node"))
  .ddg.proc.node(type, node.name, node.name, cmd = cmd, 
      scriptNum=scriptNum, startLine=startLine, startCol=startCol, 
      endLine=endLine, endCol=endCol)
  .ddg.proc2proc()

  return(node.name)
}

#' .ddg.open.new.command.node opens a new collapsible command
#' node depending on the information stored in ddg.possible.last.cmd.
#' @return nothing
#' @noRd

.ddg.open.new.command.node <- function() {
  new.command <- .ddg.get("ddg.possible.last.cmd")
  if (!is.null(new.command)) {
    .ddg.add.start.node(new.command)
    
    # Now the new command becomes the last command, and new command
    # is null.
    #print (paste (".ddg.open.new.command.node: saving ddg.last.cmd as", new.command))
    .ddg.set("ddg.last.cmd", new.command)
    .ddg.set("ddg.possible.last.cmd", NULL)
  }
}

#' .ddg.close.last.command.node closes the last created collapsible
#' node stored in ddg.last.cmd properly by creating the finish node
#' and linking it in.
#' @return nothing
#' @noRd

.ddg.close.last.command.node <- function(){

  # Get last command
  ddg.last.cmd <-
    if (.ddg.is.set("ddg.last.cmd")) {
      .ddg.get("ddg.last.cmd")
    }
    else {
      NULL
    }

  # print (paste (".ddg.close.last.command.node: ddg.last.cmd =", ddg.last.cmd))
  
  # Only create a finish node if a new command exists (i.e., we've
  # parsed some lines of code).
  if (!is.null(ddg.last.cmd)) {
    .ddg.add.finish.node()

    # Add link from a function return node if there is one.
    .ddg.link.function.returns(ddg.last.cmd)

    # No previous command.
    .ddg.set("ddg.last.cmd", NULL)
  }
}

#' .ddg.is.procedure.cmd returns TRUE if the command passed in
#' is a call to .ddg.procedure, .ddg.start, or .ddg.finish.
#' These will create a procedure node and therefore
#' initiate the creation of a collapsible console node.
#' 
#' @param cmd - A DDGStatement object
#' @return true if cmd is a call to .ddg.procedure, .ddg.start or .ddg.finish
#' @noRd

.ddg.is.procedure.cmd <- function(cmd) {
  return(grepl("^ddg.procedure", cmd@text) || grepl("^.ddg.start", cmd@text) 
    || grepl("^.ddg.finish", cmd@text))
}

#' .ddg.record.warning creates the warning node for the saved warning and 
#' attaches it to the node that created the warning
#' @return nothing
#' @noRd

.ddg.record.warning <- function () {
  # Get the saved warning
  w <- .ddg.get.warning()

  # Create a message that looks like the one R creates
  callStr <-
      if (is.null (w$call)) ""
      else paste ("In ", utils::head (deparse(w$call)), ": ")
  warningMessage <- paste (callStr[1], w$message)

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
#' If annotate.inside is TRUE, .ddg.function, .ddg.eval and .ddg.return.value
#' are added to each function definition and .ddg.eval is added to control
#' statements before commands are processed. If save.debug is TRUE,
#' changes to the script are saved in the ddg/debug directory.
#' prov.annotate.on and prov.annotate.off may be used to limit the
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
#' @param run.commands (optional) commands are executed only when environ
#'   is an environment and run.commands is TRUE.
#' @param echo (optional) print each expression after parsing
#' @param print.eval (optional) print result of each evaluation.
#' @param max.deparse.length (optional) maximum number of characters
#'   output for deparse of a single expression.
#' @param called.from.ddg.eval(optional) whether called from .ddg.eval
#' @param cmds list of DDG Statements that correspond to the exprs passed in.  This is
#'   currently only used when called from .ddg.eval.  Normally, ddg.parse.commands
#'   creates the DDG Statement objects.
#' @return nothing
#' @noRd

.ddg.parse.commands <- function (exprs, script.name="", script.num=NA, environ, 
    ignore.patterns=c('^ddg.'), run.commands = FALSE, echo=FALSE, 
    print.eval = echo, 
    max.deparse.length=150, called.from.ddg.eval=FALSE, cmds=NULL, 
    continue.echo=getOption("continue"), skip.echo = 0, prompt.echo=getOption("prompt"), 
    spaced=FALSE, verbose=getOption("verbose"),
    deparseCtrl = "showAttributes") {

  
  #print (paste ("In .ddg.parse.commands, exprs =", exprs))
  return.value <- NULL
  
  # Gather all the information that we need about the statements
  if (is.null(cmds)) {
    cmds <- .ddg.create.DDGStatements (exprs, script.name, script.num)
    
    if (.ddg.save.debug()) {
      .ddg.save.annotated.script(cmds, script.name)
    }
  }
  num.cmds <- length(cmds)

  # print (paste("ddg.parse.commands: ddg.func.depth =", .ddg.get("ddg.func.depth")))
  inside.func <- (.ddg.get("ddg.func.depth") > 0)

  if (!inside.func) {
    # Attempt to close the previous collapsible command node if a ddg exists
    if (.ddg.is.init()) {
      .ddg.close.last.command.node()
    }
    
    # Get the last command in the new commands and check to see if
    # we need to create a new ddg.last.cmd node for future reference.
    ddg.last.cmd <- cmds[[num.cmds]]
    # print(paste(".ddg.parse.commands: setting ddg.last.cmd to", ddg.last.cmd$text))

    if (ddg.last.cmd@isDdgFunc) {
      ddg.last.cmd <- NULL
      #print(".ddg.parse.commands: setting ddg.last.cmd to null")
    }
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
      # if-statement taken from R's source function
      if (verbose) 
        cat("\n>>>> eval(expression_nr.", i, ")\n\t\t =================\n")

      cmd <- cmds[[i]]

      if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Processing", cmd@abbrev))
      
      # print("Checking whether to set last.cmd")
      if (grepl("^.ddg.eval", cmd@text)) {
        if (is.null(ddg.last.cmd)) {
          ddg.last.cmd <- cmd
        }
      }

      # Get environment for output data node.
      d.environ <- environ

      if ( .ddg.is.nonlocal.assign(cmd@parsed[[1]]) )
      {
        d.environ <- .ddg.get.env(cmd@vars.set, for.caller=TRUE)
      
        if( identical(d.environ, "undefined") )
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
          (!(control.statement && .ddg.loop.annotate() && .ddg.max.loops() > 0) || 
             !run.commands)
      start.finish.created <- FALSE
      cur.cmd.closed <- FALSE

      # If the command does not match one of the ignored patterns.
      if (!any(sapply(ignore.patterns, function(pattern){grepl(pattern, cmd@text)}))) {
        # Set to NULL so it is bound even if we are in console mode.
        captured.output <- NULL

        # If sourcing, we want to execute the command.
        if (run.commands) {
          # Print command.  The echo code is adapted from R's source function.
          if (echo) {
            # lastshown holds the last line number shown to the user.
            # When executing the first command, we can skip the number
            # of lines at the beginning of the file, as specified
            # by skip.echo.  This allows skipping over header comments.
            if (i == 1) 
              lastshown <- min(skip.echo, cmd@pos@startLine - 1)
            if (lastshown < cmd@pos@endLine) {
              # Look up the lines to display in the source file
              srcrefs <- attr(exprs, "srcref")
              srcref <- srcrefs[[i]]
              srcfile <- attr(srcref, "srcfile")
              dep <- getSrcLines(srcfile, lastshown + 1, cmd@pos@endLine)
              lastshown <- cmd@pos@endLine
            
             .ddg.echo (dep, max.deparse.length, continue.echo, prompt.echo, spaced,
                     cmd@pos@endLine - cmd@pos@startLine + 1)
           }
          }

          # If we will create a node, then before execution, set
          # this command as a possible abstraction node but only
          # if it's not a call that itself creates abstract nodes.
          if (!cmd@isDdgFunc && cmd@text != "next") {
            .ddg.set("ddg.possible.last.cmd", cmd)
            .ddg.set ("ddg.cur.cmd", cmd)
            .ddg.push.cmd (cmd)
          }

          else if (.ddg.is.procedure.cmd(cmd)) {
            .ddg.set("ddg.possible.last.cmd", NULL)
          }

          
          # Capture any warnings that occur when an expression is evaluated.
          # Note that we cannot just use a tryCatch here because it behaves
          # slightly differently and we would lose the value that eval
          # returns.  withCallingHandlers returns the value.
          # withCallingHandlers also re-throws the error after it is caught.

          # EVALUATE.

          if (.ddg.debug.lib()) {
            print (paste (".ddg.parse.commands: Evaluating ", cmd@annotated))
          }
          
          result <- withCallingHandlers(
          
              {
                for (annot in cmd@annotated) {
                  #print (paste (".ddg.parse.commands: Evaluating ", 
                  #             paste(annot, collapse = " ")))
                  # Don't set return.value if we are calling a ddg function or we 
                  # are executing an if-statement
                  if (grepl("^ddg|^.ddg|^prov", annot) 
                    || .ddg.get.statement.type(annot) == "if") {
                      captured.output <- .ddg.capture.output (returnWithVisible <- withVisible (eval(annot, environ, NULL)))
                      .ddg.set ("ddg.error.node.created", FALSE)
                  }
                  else {
                    captured.output <- .ddg.capture.output (returnWithVisible <- withVisible (eval(annot, environ, NULL)))
                    #if (typeof(return.value) != "closure") {
                      #print (paste (".ddg.parse.commands: Done evaluating ", annot))
                      #print(paste(".ddg.parse.commands: setting ddg.last.R.value to", 
                      #            return.value))
                    #}
                    .ddg.set ("ddg.last.R.value", returnWithVisible$value)
                    .ddg.set ("ddg.error.node.created", FALSE)
                  }
                  
                  #### Start code taken from R's source function. ####
                  i.symbol <- mode(annot) == "name"
                  if (!i.symbol) {
                    curr.fun <- annot[[1L]]
                    if (verbose) {
                      cat("curr.fun:")
                      utils::str(curr.fun)
                    }
                  }
                  if (verbose >= 2) {
                    cat(".... mode(ei[[1L]])=", mode(annot), "; paste(curr.fun)=")
                    utils::str(paste(curr.fun))
                  }
                  # Print evaluation.
                  if (print.eval && returnWithVisible$visible) {
                    if (isS4(returnWithVisible$value))
                      methods::show(returnWithVisible$value)
                    else print(returnWithVisible$value)
                  }
                  if (verbose) 
                    cat(" .. after ", sQuote(deparse(cmd@annotated, control = unique(c(deparseCtrl, 
                                        "useSource")))), "\n", sep = "")
                  #### End code taken from R's source function ####
                }
                
                returnWithVisible
              },
            warning = .ddg.set.warning,
            error = function(e)
            {
              # Only create an error node if there has not been one created
              # for the statement.  Since the error is re-thrown by withCallingHandlers,
              # we will see this for every recursive call to ddg.parse.commands unless
              # the error gets handled, but we only want to record it the
              # first time.
              if (!.ddg.get("ddg.error.node.created")) {
                # create procedure node for the error-causing operation
                .ddg.proc.node("Operation", cmd@abbrev, cmd@abbrev, 
                               functions.called=cmd@functions.called, cmd=cmd)
                .ddg.proc2proc()
  
                # create input edges by adding variables to set
                if (.ddg.debug.lib()) {
                  print(paste(".ddg.parse.commands: Adding", cmd@abbrev, 
                              "information to vars.set, for an error"))
                }
                .ddg.create.data.use.edges(cmd, for.caller=FALSE)
  
                # Create output exception node.
        
                .ddg.data.node("Exception", "error.msg", toString(e), "ddg.library")
                
                # Create data flow edge from procedure node to exception node.
                .ddg.proc2data(cmd@abbrev, "error.msg")
                .ddg.set ("ddg.error.node.created", TRUE)
              }
            }
          )

          if (.ddg.debug.lib()) {
            print (paste (".ddg.parse.commands: Done evaluating ", cmd@annotated))
          }

          if (!cmd@isDdgFunc && cmd@text != "next") {
            # Need to get the stack again because it could have been
            # modified during the eval call.
            ddg.cur.cmd.stack <- .ddg.get("ddg.cur.cmd.stack")
            stack.length <- length(ddg.cur.cmd.stack)
            start.created <- ddg.cur.cmd.stack[stack.length][[1]]

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
            cur.cmd.closed <- (ddg.cur.cmd.stack[stack.length] == "MATCHES_CALL")
            .ddg.pop.cmd ()
          }
        }  

        # Figure out if we should create a procedure node for this
        # command. We don't create it if it matches a last command
        # (because that last command has now become a collapsible
        # node). Matching a last command means that the last command
        # is set, is not NULL, and is equal to the current command.

        last.proc.node.created <-
            if (.ddg.is.set ("ddg.last.proc.node.created"))
              .ddg.get("ddg.last.proc.node.created")
            else ""
        
        create.procedure <- create && !cur.cmd.closed && 
                            !start.finish.created  && 
                            !grepl("^.ddg.source", cmd@annotated)
        
        # We want to create a procedure node for this command.
        if (create.procedure) {
          
          # Create the procedure node.

          if (.ddg.debug.lib()) {
            print(paste(".ddg.parse.commands: Adding operation node for", 
                        cmd@abbrev))
          }
          
          .ddg.proc.node("Operation", cmd@abbrev, cmd@abbrev, 
                         functions.called=cmd@functions.called, cmd=cmd)
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
            vars.set <- .ddg.add.to.vars.set(vars.set, cmd, i)

            if (.ddg.debug.lib()) {
              print(paste(".ddg.parse.commands: Adding", cmd@abbrev, 
                          "information to vars.set"))
            }
          }

          .ddg.create.data.use.edges(cmd, for.caller=FALSE, d.environ)

          .ddg.create.file.read.nodes.and.edges()
          .ddg.link.function.returns(cmd)

          if (.ddg.debug.lib()) {
            print(paste(".ddg.parse.commands: Adding input data nodes for", 
                        cmd@abbrev))
          }
          .ddg.create.data.set.edges(vars.set, cmd, d.environ, captured.output)

          if (.ddg.debug.lib()) {
            print(paste(".ddg.parse.commands: Adding output data nodes for", 
                        cmd@abbrev))
          }

          .ddg.create.file.write.nodes.and.edges ()
          .ddg.create.graphics.nodes.and.edges ()
        }
        # We wanted to create it but it matched a last command node.
        else if (create && run.commands) {
          .ddg.close.last.command.node()
          if (run.commands) {
            # Add variables to set.
            vars.set <- .ddg.add.to.vars.set(vars.set, cmd, i)
            if (.ddg.debug.lib()) {
              print(paste(".ddg.parse.commands: Adding", cmd@abbrev, 
                          "information to vars.set"))
            }
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
     
     if (echo) {
       # Output any extra lines from the file that are after the last line executed
	     # This code is adapted from R's source function.  In the source function
	     # they use the tail variable to identify statements done
	     # after the last line of code is executed.
       srcref <- attr(exprs, "wholeSrcref")
       srcfile <- attr(srcref, "srcfile")
        dep <- getSrcLines(srcfile, lastshown + 1, srcref[3])
        .ddg.echo (dep, max.deparse.length, continue.echo, prompt.echo, spaced, 0)
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

  if (.ddg.is.set("ddg.last.R.value")) return (.ddg.get ("ddg.last.R.value"))
  else return ("")
  #return.value <- .ddg.get ("ddg.last.R.value")
  #if (typeof(return.value) != "closure") {
  #  print(paste(".ddg.parse.commands: returning ", return.value))
  #}
}

#' .ddg.capture.output is a simple re-write of R's capture.output
#'  that allows us to capture the exception that occurs if the 
#' connection is closed by the code whose output is being captured
#' as is the case if that code includes a call to closeAllConnections.
#' See capture.output for more details.
#' @noRd 
.ddg.capture.output <- function (...) 
{
  args <- substitute(list(...))[-1L]
  type <- c("output", "message")
  rval <- NULL
  file <- textConnection("rval", "w", local = TRUE)
  sink(file, type = type, split = TRUE)
  on.exit({
        sink(type = type, split = TRUE)
        close(file)
      })
  pf <- parent.frame()
  evalVis <- function(expr) withVisible(eval(expr, pf))
  for (i in seq_along(args)) {
    expr <- args[[i]]
    tmp <- switch(mode(expr), expression = lapply(expr, evalVis), 
        call = , name = list(evalVis(expr)), stop("bad argument"))
    for (item in tmp) if (item$visible) 
        print(item$value)
  }
  on.exit()
  tryCatch(
      {
        sink(type = type, split = TRUE)
        close(file)
      },
      warning = function (e) {}
  )
  if (is.null(rval)) 
    invisible(NULL)
  else rval
}

#' .ddg.echo prints the command to the screen
#' 
#' @param cmdText the lines to display.  This can include blank lines and comments that
#'    preceded the command itself.
#' @param max.deparse.length the maximum length to display
#' @param continue.echo the prompt to put at the start of the condinuation lines of a multi-line statement
#' @param prompt.echo the prompt to put at the start of the first line of a statement
#' @param spaced if true, blank lines are inserted between statements executed
#' @param curCmdLength the number of lines in the statement being executed.  This is the
#'     length of the command itself, not including leading blank lines or comments.
#' @noRd 
.ddg.echo <- function (cmdText, max.deparse.length, continue.echo, prompt.echo, spaced, curCmdLength) {
  # This function is extracted and adapted from R's source function.
  dep <- cmdText
  
  # Remove leading blank lines
  while (length(dep) && grepl("^[[:blank:]]*$", dep[1])) {
    dep <- dep[-1]
  }
  
  # No actual command.  This happens if we have reached the end of the file
  # and there is a trailing comment.
  if (curCmdLength == 0) {
    # Print the trailing comments, with each line beginning with 
    # the prompt character.
    dep <- paste0(rep.int(prompt.echo, length(dep)), dep, 
      collapse = "\n")
  }
  
  else {
    # Print the comments and first line of the command beginning with the
    # prompt character.  If there is more than one line to the command,
    # begin each of those with the continue character.
    dep <- paste0(rep.int(c(prompt.echo, continue.echo), 
            c(length(dep) - curCmdLength + 1, curCmdLength - 1)), dep, 
            collapse = "\n")
  }
  
  nd <- nchar(dep, "c")
  
  if (nd) {
    # Truncate the output if it is too long
    do.trunc <- nd > max.deparse.length
    dep <- substr(dep, 1L, if (do.trunc) 
              max.deparse.length
            else nd)
    sd <- "\""
    nos <- "[^\"]*"
    oddsd <- paste0("^", nos, sd, "(", nos, sd, nos, sd, 
        ")*", nos, "$")
    
    # Insert blank line if spaced is true
    cat(if (spaced) "\n", 
        dep, 
        if (do.trunc) 
          # Make sure any open quotes are closed if the string
          # is truncated.
          paste(if (grepl(sd, dep) && grepl(oddsd, dep)) 
                    " ...\" ..."
                else " ....", 
                "[TRUNCATED] "), "\n", sep = "")
  }
} 

#' .ddg.evaluate.commands evaluates a list of parsed R statements. Provenance is 
#' collected for inputs and outputs only. If an error or warning is generated 
#' when a statement is evaluated, an output exception node is created containing 
#' the error or warning message.

#' @param exprs list of parsed R statements
#' @param environ environment in which commands should be executed.
#' @return nothing
#' @noRd

.ddg.evaluate.commands <- function (exprs, environ) {
  for (expr in exprs) {
    # Evaluate each statement in turn.
    result <- withCallingHandlers(
      {
        return.value <- eval(expr, environ, NULL)
        .ddg.set ("ddg.error.node.created", FALSE)
      },
      warning = .ddg.set.warning,
      error = function(e)
        {
          # If an error occurred, create an error node if not already created.
          if (!.ddg.get("ddg.error.node.created")) {
            .ddg.data.node("Exception", "error.msg", toString(e), "ddg.library")
            .ddg.lastproc2data ("error.msg")
            .ddg.set ("ddg.error.node.created", TRUE)
          }
        }
    )

    # If a warning occurred, create a warning node.
    if (.ddg.warning.occurred()) {
      .ddg.record.warning()
    }

    # Create nodes & edges for inputs & outputs.
    .ddg.create.file.read.nodes.and.edges ()
    .ddg.create.file.write.nodes.and.edges ()
    .ddg.create.graphics.nodes.and.edges ()
  }
}

#' .ddg.push.cmd pushes a command onto the command stack.  The command stack 
#' remembers the command about to be executed.  It also puts FALSE on the stack 
#' to indicate that no start node has (yet) been created for the command.
#' @param cmd The DDGStatement about to be executed
#' @return nothing
#' @noRd

.ddg.push.cmd <- function (cmd) {
  
  #print(paste("Pushing onto the stack:", cmd@text))
  
  # Remember the current statement on the stack so that we
  # will be able to create a corresponding Finish node later
  # if needed.
  ddg.cur.cmd.stack <- .ddg.get("ddg.cur.cmd.stack")
  
  if (length(ddg.cur.cmd.stack) == 0) {
    ddg.cur.cmd.stack <- c(cmd, FALSE)
  }
  else {
    ddg.cur.cmd.stack <- c(.ddg.get("ddg.cur.cmd.stack"), cmd, FALSE)
  }
  .ddg.set("ddg.cur.cmd.stack", ddg.cur.cmd.stack)
}

#' .ddg.pop.cmd removes the top of the command stack, along with the boolean 
#' that remembers if the start / finish nodes have been created.
#' @return nothing
#' @noRd

.ddg.pop.cmd <- function () {
  ddg.cur.cmd.stack <- .ddg.get("ddg.cur.cmd.stack")
  stack.length <- length(ddg.cur.cmd.stack)
  if (stack.length == 2) {
    .ddg.set("ddg.cur.cmd.stack", vector())
  }
  else {
    .ddg.set("ddg.cur.cmd.stack", ddg.cur.cmd.stack[1:(stack.length-2)])
  }
}

#' .ddg.get.top.cmd returns the last command on the stack.
#' @return the last command pushed to the stack
#' @noRd

.ddg.get.top.cmd <- function() {
  ddg.cur.cmd.stack <- .ddg.get("ddg.cur.cmd.stack")
  stack.length <- length(ddg.cur.cmd.stack)
  return (ddg.cur.cmd.stack[stack.length-1][[1]])
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
#' @noRd

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
#' @noRd
 
.ddg.lookup.value <- gtools::defmacro(expr.to.evaluate, value, env, 
                                      procname = "", warn=FALSE,
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
                  error.msg <- paste("Unable to evaluate", expr.to.evaluate, 
                                     "in call to", procname)
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
#' @noRd

.ddg.delete.temp <- function() {
  # Delete the temporary history file if we made it.
  if (.ddg.is.set('ddg.history.file')) unlink(.ddg.get('ddg.history.file'))
}

#' .ddg.get.frame.number gets the frame number of the closest
#' non-library calling function.
#' @param calls call stack to search
#' @param for.caller (optional) if TRUE, return the frame of the caller of the 
#'    first non-ddg function
#' @return If for.caller is FALSE, returns the top-most non-ddg function on the
#'   call stack.  If for.caller is TRUE, returns the second one found.  If none
#'   are found, returns 0. 
#' @noRd

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
      # Ignore calls to ddg functions or to the functions that get called from 
      # the outermost tryCatch to ddg code.  Seems like a hack.  If provenance
      # tools call prov.run, we need to add them to the list (like debug.init).
      # We also could capture functions that actually are defined in a script if
      # they start with "prov"!  
      if (!any (startsWith (call.func, c (".ddg", "ddg", "prov", "doTryCatch", 
        "tryCatch", "debug.init")))) {
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
#' @noRd

.ddg.where <- function( name, env = parent.frame(), warning = TRUE )
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
#' @noRd

.ddg.get.env <- function(name, for.caller=FALSE, calls=NULL) {
  if (is.null(calls)) calls <- sys.calls()

  fnum <- .ddg.get.frame.number(calls, for.caller)
  stopifnot(!is.null(fnum))

  tryCatch (
    if(!exists(name, sys.frame(fnum), inherits=TRUE)) {
      return(NULL)
    },
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
#' @noRd

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

#' .ddg.save.debug.files saves debug files to the debug directory.
#' @return nothing
#' @noRd

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
  
  # save run arguments to file
  fileout <- paste(.ddg.path.debug(), "/run-args.csv", sep="")
  utils::write.csv(.ddg.run.args(), fileout, row.names=FALSE)
  
  if (interactive()) print(paste("Saving debug files in ", .ddg.path.debug(), sep=""))
}

#' .ddg.exec.env returns a dataframe of information about the current
#' execution environment
#' @return a data frame of information about the current environment.
#' @noRd

.ddg.exec.env <- function()
{
  env <- data.frame(  "architecture" = character(1), 
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
  script.path <- .ddg.r.script.path()
  
  if(!is.null(script.path) )
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
  
  # tool version
  tool.name <- .ddg.tool.name()
  env$rdtVersion[1] <- toString( utils::packageVersion(tool.name) )
  
  # hash algorithm
  env$hashAlgorithm[1] <- .ddg.get("ddg.hash.algorithm")
  
  return(env)
}
