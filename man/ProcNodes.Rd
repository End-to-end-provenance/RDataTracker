\name{ProcedureNodes}
\alias{ddg.procedure}
\alias{ddg.start}
\alias{ddg.finish}
\title{RDataTracker Procedural Nodes}
\description{These functions are used to create procedural nodes and abstraction nodes 
  in the provenance graph created by RDataTracker.}
\usage{
ddg.procedure(pname=NULL, ins=NULL, outs=NULL, 
    lookup.ins = FALSE)
ddg.start(pname=NULL)
ddg.finish(pname=NULL)
}
\arguments{
  \item{pname}{ the label for the node }
  \item{ins}{ a list of names of data nodes that should be linked as inputs to the node being created.
    These must be passed as quoted strings. }
  \item{outs}{ a list of names of data nodes that should be created and linked as outputs to the node being created.
    These must be passed as quoted strings. }
  \item{lookup.ins}{ if true and ins is null, ddg.procedure will try to create the data in edges automatically }
}
\details{
  ddg.procedure creates a node of type Operation.  If pname is NULL, the name of the procedure
  that called ddg.procedure() will be used as the label of the node.  If pname is the name of 
  a function, the user can right-click on it in DDG Explorer to see the corresponding function 
  definition.
  
  If pname and ins are both NULL and lookup.ins is TRUE, ddg.procedure will create a data flow edge for each
  argument that was passed to the function that called ddg.procedure.
  
  ddg.start() and ddg.finish() should come in matching pairs.  ddg.start() should precede ddg.finish().  
  For each call made to ddg.start() when the script is executed, there should be exactly one call made
  to ddg.finish() using the same pname.  For example, ddg.start() might be called at the beginning of
  a function with ddg.finish() called at each place where the function might return.
  
  ddg.start creates a Start node labeled with pname.  ddg.finish() creates a Finish node labeled with 
  the same name.  If pname is NULL, the name of the function that called ddg.start() or ddg.finish() 
  will be used for pname.  In DDG Explorer, ddg.start() ... ddg.finish() pairs can be collapsed 
  into a single node to make the visualization smaller and easier to understand.  Also, in DDG Explorer, 
  the user can right-click on a Start of Finish or collapsed Start/Finish node to see all of the R code 
  between the start and finish.
}
\author{Emery Boose and Barbara Lerner}
\examples{
dir.create("ddg", showWarnings=FALSE)
ddg.init()
a <- 0
myfunc <- function() {
  b <<- a + 1
  ddg.procedure(ins=list("a"), outs=list("b"))
}

secondfunc <- function() {
  ddg.start()
  myfunc()
  if (a == 1) {
  	ddg.finish()
  	return(10)
  }
  else {
    ddg.finish()
    return(1)
  }
}
secondfunc()
ddg.procedure("myfunc") 
ddg.save()
}
\keyword{provenance}