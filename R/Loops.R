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

######## Loops.R ##############

# This file contains code related to collecting provenance inside of loops
#

#' .ddg.init.loops initialize the variables used to manage loop annotation
#' @param first.loop the first loop iteration to collect
#' @param max.loops the maximum number of iterations to collect
#' @return nothing

.ddg.init.loops <- function (first.loop, max.loops) {
  # Control loop number
  .ddg.set("ddg.loop.num", 0)
  
  # Control loop list
  .ddg.set("ddg.loops", numeric())
  
  # If loops are not annotated, do not annotate functions called from inside a loop.
  .ddg.set("ddg.loop.annotate", max.loops > 0)
  
  .ddg.set("ddg.max.loops", max.loops)
  
  # Initialize the counter that keeps track of nested levels
  # of ifs and loops
  .ddg.set("ddg.inside.loop", 0)
  
  # Set number of first loop.
  .ddg.set("ddg.first.loop", first.loop)
}

#' .ddg.clear.loops reinitialize some variables.  (Not sure why this is needed.)
#' @return nothing

.ddg.clear.loops <- function () {
  .ddg.set("ddg.loop.num", 0)
  .ddg.set("ddg.loops", list())
}

#' .ddg.loops returns the number of iterations of each loop.
#' Each loop has a unique id.  The vector returned contains an entry
#' for each loop.
#' @return the vector contains the number of iterations of each loop

.ddg.loops <- function() {
  return(.ddg.get("ddg.loops"))
}

#' .ddg.add.loop adds an entry to the ddg.loops vector that tracks the 
#' number of iterations of each loop, initializing it to 0. 
#' @return the unique id for the new loop

.ddg.add.loop <- function() {
  ddg.loops <- c(.ddg.loops(), 0)
  .ddg.set("ddg.loops", ddg.loops)
  return (.ddg.inc("ddg.loop.num"))
}

#' .ddg.loop.annotate returns True if the loop should be annotated
#' @return TRUE if the loop should be annotated

.ddg.loop.annotate <- function() {
  return(.ddg.get("ddg.loop.annotate"))
}

#' ddg.loop.annotate.on turns on loop annotation (internal use only)
#' @return nothing
#' @export

ddg.loop.annotate.on <- function() {
  .ddg.set("ddg.loop.annotate", TRUE)
}

#' ddg.loop.annotate.off turns off loop annotation (internal use only)
#' @return nothing
#' @export

ddg.loop.annotate.off <- function() {
  .ddg.set("ddg.loop.annotate", FALSE)
}

#' .ddg.inside.loop returns the level of loop nesting
#' @return the level of loop nesting

.ddg.inside.loop <- function() {
  return (.ddg.get("ddg.inside.loop"))
}

#' ddg.set.inside.loop increases the counter of the level of nesting of loops
#' (internal use only)
#' @return nothing 
#' @export

ddg.set.inside.loop <- function() {
  .ddg.set("ddg.inside.loop", .ddg.get("ddg.inside.loop") + 1)    
}

#' ddg.not.inside.loop decreases the counter of the level of nesting of loops
#' (internal use only)
#' @return nothing
#' @export

ddg.not.inside.loop <- function() {
  .ddg.set("ddg.inside.loop", .ddg.get("ddg.inside.loop") - 1)
}

#' ddg.loop.count returns the number of times a loop has iterated so far
#' (internal use only)
#' @param loop.num the id of the loop to look up
#' @return the number of times the loop has iterated
#' @export

ddg.loop.count <- function(loop.num) {
  ddg.loops <- .ddg.loops()
  return(ddg.loops[loop.num])
}

#' ddg.loop.count.inc increments the current count for the specified loop
#' (internal use only)
#' @param loop.num the id of the loop being executed
#' @return the updated value of the counter.
#' @export

ddg.loop.count.inc <- function(loop.num) {
  ddg.loops <- .ddg.loops()
  ddg.loops[loop.num] <- ddg.loops[loop.num] + 1
  .ddg.set("ddg.loops", ddg.loops)
  return(ddg.loops[loop.num])
}

#' ddg.reset.loop.count sets the current count for the specified loop to zero
#' (internal use only)
#' @param loop.num the id of the loop to reset
#' @return nothing
#' @export

ddg.reset.loop.count <- function(loop.num) {
  ddg.loops <- .ddg.loops()
  ddg.loops[loop.num] <- 0
  .ddg.set("ddg.loops", ddg.loops)
}

#' ddg.first.loop returns the loop iteration at which we should start
#' collecting provenance (internal use only)
#' @return the first iteration to collect provenance 
#' @export

ddg.first.loop <- function() {
  return(.ddg.get("ddg.first.loop"))
}

#' ddg.max.loops returns the maximum number of iterations of a loop for
#' which provenance should be collected
#' @return the maximum number of iterations to collect provenance
#' @export

ddg.max.loops <- function() {
  return(.ddg.get("ddg.max.loops"))
}

#' ddg.forloop inserts a procedure node and a data node in a for loop,
#' indicating the value currently assigned to the index variable
#' (internal use only)
#' @param index.var a parsed expression containing the index variable
#' @return nothing
#' @export 

ddg.forloop <- function(index.var) {
  index.name <- as.character(deparse(substitute(index.var)))
  pnode.name <- paste(index.name, "<-", index.var)
  dscope <- .ddg.get.scope(index.name)
  
  .ddg.proc.node("Operation", pnode.name, pnode.name)
  .ddg.proc2proc()
  
  .ddg.data.node("Data", index.name, index.var, dscope, from.env=FALSE)
  .ddg.proc2data(pnode.name, index.name)
}

#' .ddg.break.statement creates a procedure node for a break statement in
#' a for, repeat, or while statement. It also adds a finish node for the
#' if statement (if any) where the break occurs, adds a finish node
#' for the for, repeat, or while loop where the break occurs, and adds a
#' finish node for the for, repeat, or while statement.
#' @return nothing

.ddg.break.statement <- function() {
  .ddg.end.loop ("break")
}

#' .ddg.next.statement creates a procedure node for a next statement in
#' a for, repeat, or while statement. It also adds a finish node for the
#' if statement (if any) where the next occurs and adds a finish node for
#' the for, while, or repeat loop where the next occurs.

.ddg.next.statement <- function() {
  .ddg.end.loop ("next")
}

#' .ddg.end.loop creates the nodes necessary to end a loop
#' @param op One of "break" or "next" which determines the operation node that is built
#' @return nothing

.ddg.end.loop <- function (op) {
  .ddg.proc.node("Operation", op, op)
  .ddg.proc2proc()
  
  # Get last command from stack.
  cmd <- .ddg.get.top.cmd()
  # Get loop type.
  loop.type <- as.character(cmd@parsed[[1]][[1]])
  
  # Create finish nodes if break occurs in if statement.
  while (loop.type == "if") {
    # Create finish node for if loop.
    ddg.finish("if")
    # Create finish node for if statement.
    .ddg.add.finish.node(cmd)
    
    # Remove last command & start.created from stack.
    .ddg.pop.cmd()
    
    # Get preceding command from stack.
    cmd <- .ddg.get.top.cmd()
    # Get loop type.
    loop.type <- as.character(cmd@parsed[[1]][[1]])
  }
  
  # Create finish node for for, repeat, or while loop.
  loop.name <- paste(loop.type, "loop")
  ddg.finish(loop.name)
}
