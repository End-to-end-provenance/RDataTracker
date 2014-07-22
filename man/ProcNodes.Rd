\name{ProcNodes}
\alias{ddg.procedure}
\alias{ddg.return}
\alias{ddg.start}
\alias{ddg.finish}
\title{RDataTracker Procedural Nodes}
\description{These functions create procedural nodes and abstraction nodes within a provenance 
  graph created by RDataTracker.}
\usage{
ddg.procedure(pname = NULL, ins = NULL, lookup.ins = FALSE, outs.graphic = NULL, 
              outs.data = NULL, outs.exception = NULL, outs.url = NULL, outs.file =NULL,
              graphic.fext = "jpeg")
ddg.return(expr)
ddg.start(pname = NULL)
ddg.finish(pname = NULL)
}
\arguments{
  \item{pname}{ the label for the node. }
  \item{lookup.ins}{ if TRUE and ins is NULL, ddg.procedure will try to create the input data edges automatically. }
  \item{ins}{ a list of names of data nodes to be linked as inputs to the node being created. }
  \item{outs.data}{ a list of names of data nodes to be created and linked as outputs to the node being created. }
  \item{outs.file}{ a list of names of file nodes to be created and linked as outputs to the node being created. }
  \item{outs.url}{ a list of names of url nodes to be created and linked as outputs to the node being created. }
  \item{outs.exception}{ a list of names of exception nodes to be created and linked as outputs to the node being created. }
  \item{outs.graphic}{the name of the snapshot node to be created and linked as output to the procedural node being created}
  \item{graphic.fext}{the file extension for the capturd graphics file}
  \item{expr}{the value the function returns}
}
\details{
  ddg.procedure creates a node of type Operation.  If pname is NULL, the name of the function
  that called ddg.procedure() will be used as the name of the node.  If pname is the name of 
  a function, the user can right-click on it in DDG Explorer to see the corresponding function 
  definition.
  
  If ins is NULL and lookup.ins is TRUE, ddg.procedure will create a data flow edge for each
  argument that was passed to the function that called ddg.procedure.
  
  If ins is not NULL, ddg.procedure will create a data flow edge from each data node in the list.  
  Data nodes may be of type data, file, url, or exception.  Node names must be passed 
  as quoted strings.  For files, the node name may be the name of the file or the name of a variable that 
  holds the name of the file.
  
  If outs.data, outs.file, etc is not NULL, ddg.procedure will create a node of the specified type (data, file,
  url, or exception) for each name in the list along with a data flow edge to that node.  Node names must be 
  passed as quoted strings.  For files, the node name may be the name of the file or the name of a variable 
  that holds the name of the file.

  If outs.graphic is not NULL, ddg.procedure will create a node of type snapshot (these nodes hold complex data types) with the value of outs.graphic as the name. The node will
  store a snapshot of the current graphics device in the file (outs.graphic).(graphic.fext). The file will be linked to the DDG and will be viewable using DDG Explorer.

  The parameter graphic.fext is ignored if outs.graphic is NULL. 
  
  ddg.return creates a data node for a function's return value.  If the function is called from a console
  command and the console processing is enabled, the node will link to the console command
  that uses the value.  ddg.return should be used in place of the function's normal return statement(s).
  
  ddg.start() and ddg.finish() should come in matching pairs.  ddg.start() should precede ddg.finish().  
  For each call made to ddg.start() when the script is executed, there should be exactly one call made
  to ddg.finish() using the same pname.  For example, ddg.start() might be called at the beginning of
  a function with ddg.finish() called at each place where the function might return.
  
  ddg.start creates a Start node labeled with pname.  ddg.finish() creates a Finish node labeled with 
  the same name.  If pname is NULL, the name of the function that called ddg.start() or ddg.finish() 
  will be used for pname.  In DDG Explorer, ddg.start() ... ddg.finish() pairs can be collapsed 
  into a single node to make the visualization smaller and easier to understand.  The user can also 
  right-click in DDG Explorer on a Start or Finish or collapsed Start/Finish node to see all of the R code 
  between the start and finish.
}
\author{Emery Boose and Barbara Lerner}
\examples{
dir.create("ddg", showWarnings=FALSE)
ddg.init()
a <- 0
myfunc <- function() {
  b <- a + 1
  ddg.procedure(ins=list("a"))
  ddg.return(b)
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
