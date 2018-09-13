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


#' .ddg.create.start.for.cur.cmd creates a start node for the current command 
#' if one has not been created already.  Modifies the command stack by setting 
#' the value to TRUE if the start node is created.  If the current command 
#' matches the call, no node is created but the top of the stack is changed
#' to "MATCHES_CALL".
#' @param call the parsed version of the function call
#' @return nothing
#' @noRd

.ddg.create.start.for.cur.cmd <- function (call) {
  if (!.ddg.is.set("ddg.cur.cmd")) return ()
  
  ddg.cur.cmd.stack <- .ddg.get("ddg.cur.cmd.stack")
  stack.length <- length(ddg.cur.cmd.stack)
  if (stack.length == 0) return ()
  
  last.created <- ddg.cur.cmd.stack[stack.length]
  # Only create a start node for the current command if we have not already
  # created one and the command is more than just the call to this function
  if (last.created[[1]] != "FALSE") return ()
  
  ddg.cur.cmd <- .ddg.get("ddg.cur.cmd")
  if (ddg.cur.cmd@text == paste(deparse(call), collapse="")) {
    .ddg.change.cmd.top ("MATCHES_CALL")
  }
  
  else {
    .ddg.add.start.node (ddg.cur.cmd)
    st.type <- .ddg.get.statement.type(ddg.cur.cmd@parsed[[1]])
    loop.statement <- st.type %in% c("for", "while", "repeat")
    control.statement <- loop.statement || st.type %in% c("if", "{")
    .ddg.create.data.use.edges(ddg.cur.cmd, for.caller=!control.statement)
    
    # Add Details Omitted node before annotated loops if needed.
    if (loop.statement && .ddg.first.loop() > 1) {
      .ddg.details.omitted()
    }
    
    # Mark the start node as created on the stack.  Mark it even if we did not
    # create the abstract node above, because we will create it below.
    .ddg.change.cmd.top (TRUE)
  }
}

#' .ddg.link.function.returns determines if the command calls a
#' function for which ddg.return has created a node for the return
#' value.  If so, a data flow edge is created from the return value
#' data node to the finish node for the command.  Note that if the
#' assignment is an expression, like "d <- f(a) + f(b)", there may
#' be multiple return value nodes to link to.
#' @param command input command.
#' @return nothing
#' @noRd

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

#' .ddg.change.cmd.top changes the value associated with the current command 
#' while keeping the command at the top of the stack the same
#' @param value the new value
#' @return nothing
#' @noRd

.ddg.change.cmd.top <- function (value) {
  ddg.cur.cmd.stack <- .ddg.get("ddg.cur.cmd.stack")
  stack.length <- length(ddg.cur.cmd.stack)
  .ddg.set ("ddg.cur.cmd.stack", c(ddg.cur.cmd.stack[1:stack.length-1], value))
}

#' .ddg.create.function.nodes creates the start node, procedure node, input
#' binding nodes, and output nodes for the function.
#' @param pname name of procedure node.
#' @param call call as made
#' @param full.call full function call, with full parameter names
#' @param outs.graphic - the name of a snapshot node to be used as a
#'    file name.  A graphical snapshot is simply a captured image
#'    of the graphic device active at the time of the call to
#'    .ddg.function or .ddg.procedure.
#' @param outs.data - a list of names of data nodes.
#' @param outs.exception - a list of names of exception nodes.
#' @param outs.url - a list of names of url nodes.
#' @param outs.file - a list of names of file nodes. Supported file
#'   extensions include: .csv, .jpg, .jpeg, .pdf, and .txt.
#' @param graphic.fext - the file extension to be used when saving the
#'   captured graphic. Supported extensions are .jpg, .jpeg, .pdf.
#' @param env (optional) - the environment local to the function
#' @return nothing
#' @noRd

.ddg.create.function.nodes <- function(pname, call, full.call, outs.graphic=NULL, 
    outs.data=NULL, outs.exception=NULL, 
    outs.url=NULL, outs.file=NULL, 
    graphic.fext="jpeg", env=NULL) {
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
            #print(paste(".ddg.create.function.nodes: binding.node.name =", 
            #            binding.node.name))
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
                  .ddg.save.data(formal, eval(parse(text=formal), formal.env), 
                      scope=formal.scope, stack=stack)
                  .ddg.proc2data(binding.node.name, formal, formal.scope)
                },
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
          
          # Formal will be NULL if declared as ...  Don't create the data node in 
          # that case.
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
  
  .ddg.create.output.nodes(pname, outs.graphic, outs.data, outs.exception, 
      outs.url, outs.file, graphic.fext)
  
}

#' .ddg.create.output.nodes creates output nodes for .ddg.function
#' and .ddg.procedure. Outs values must be passed as strings, not
#' names, unless the value is a file name.
#' @param pname the name of the procedure node.
#' @param outs.graphic - the name of a snapshot node to be used as a
#'    file name.  A graphical snapshot is simply a captured image
#'    of the graphic device active at the time of the call to
#'    .ddg.function or .ddg.procedure.
#' @param outs.data - a list of names of data nodes.
#' @param outs.exception - a list of names of exception nodes.
#' @param outs.url - a list of names of url nodes.
#' @param outs.file - a list of names of file nodes. Supported file
#'   extensions include: .csv, .jpg, .jpeg, .pdf, and .txt.
#' @param graphic.fext - the file extension to be used when saving the
#'   captured graphic. Supported extensions are .jpg, .jpeg, .pdf.
#' @return nothing
#' @noRd

.ddg.create.output.nodes<- function(pname, outs.graphic, outs.data, 
    outs.exception, outs.url, outs.file, 
    graphic.fext) {
  env <- .ddg.get.first.non.ddg.env()
  
  # Capture graphics device.
  if (is.character(outs.graphic)) {
    name <- outs.graphic
    gfext <- as.character(graphic.fext)
    
    # value is ignored
    .ddg.write.graphic(name, "Graphical Plot. Not saved in script.", fext=gfext) 
    .ddg.proc2data(pname, name)
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
                scope <- .ddg.get.scope(param, calls=stack)
                .ddg.save.data(name, value, error=TRUE, scope=scope)
                .ddg.proc2data(pname, name, scope)
              }
              , error = function(e) {
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
#' @noRd

.ddg.get.first.non.ddg.env <- function() {
  non.ddg.frame <- .ddg.get.first.non.ddg.frame.number()
  return (sys.frame(non.ddg.frame))
}

#' .ddg.get.first.non.ddg.frame.number gets the frame number for the function 
#' that called into ddg functions
#' @return the frame number of the innermost user function
#' @noRd

.ddg.get.first.non.ddg.frame.number <- function() {
  calls <- sys.calls()
  calls <- as.character (mapply( `[[`, calls, 1, SIMPLIFY = TRUE ))
  #print(paste("calls =", calls))
  #print(summary(calls))
  
  return ( Position( 
          function (call) {
            return (!startsWith (call, "ddg") & !startsWith (call, ".ddg"))
          }
          , calls, right=TRUE ))
}
