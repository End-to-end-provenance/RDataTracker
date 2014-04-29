\name{DataFlowEdges}
\alias{ddg.data.in}
\alias{ddg.data.out}
\alias{ddg.exception.out}
\alias{ddg.url.out}
\alias{ddg.snapshot.out}
\alias{ddg.file.out}
\title{RDataTracker Data Flow Edges}
\description{These functions create data flow edges in the provenance graphs created using
  RDataTracker.}
\usage{
ddg.data.in (dname, pname=NULL) 
ddg.data.out (dname,dvalue=NULL,pname=NULL)
ddg.exception.out (dname,dvalue=NULL, pname=NULL)
ddg.url.out (dname,dvalue=NULL, pname=NULL) 
ddg.file.out (filename,dname=NULL,pname=NULL) 
ddg.snapshot.out (dname,fext="csv",data=NULL,pname=NULL)
}
\arguments{
  \item{dname}{ the label for the data node }
  \item{pname}{ the name of the procedure node that this edge connects to }
  \item{dvalue}{ the value to associate with the node being created }
  \item{filename}{ name of the file to copy, including path if not in the working directory }
  \item{fext}{ the file extension for the snapshot file that is created }
  \item{data}{ the data to write to a snapshot file }
}\value{
  ddg.file.out returns the full path to the copied file
}
\details{
  If pname is NULL and the function is called from within an R function, pname will be set to the
  name of the function that the ddg function is called from.
  
  If dvalue is NULL, the value passed to dname will be evaluated and that value will be used 
  as the value for the node.

  ddg.data.in creates a data flow edge from an existing data node (created by an earlier call
  to one of ddg.data, ddg.exception, ddg.url, or ddg.snapshot) and an existing procedure node 
  (created by an earlier call to ddg.procedure).
  
  ddg.data.out creates a Data node and a control flow edge from an existing procedure node 
  (created by an earlier call to ddg.procedure) and the Data node.  It should be used when
  the value of the node is either a simple value, or the user does not want to save the value
  in the ddg.
  
  ddg.exception.out behaves similarly to ddg.data.out, except that the node created is an
  Exception node.  This function should be used to record where an error message is created.
  
  ddg.url.out behaves similarly to ddg.data.out, except that the node created is a URL node.
  This should be used if to record that the script has created a document or Web page 
  accessible via a URL.
  
  ddg.snapshot.out behaves similarly to ddg.data.out, except that the node created is a
  Snapshot node.  The value that is passed in to ddg.snapshot.out is written to the file
  specified.  This function should be called when the user wants to save a complex value
  that the script does not write to a file in the provenance graph.  For example, this would
  be useful to save intermediate values of data frames.
  
  ddg.file.out behaves similarly to ddg.data.out, except that the node created is a File node.
  This should be used when the script writes a file and the user wants to save this version
  of the file in the provenance graph.  Files saved in this way can be restored by using the
  ddg.checkpoint and ddg.restore functions, described elsewhere.  If dname is NULL, the
  filename will be used as the label for the node.
}
\author{Emery Boose and Barbara Lerner}
\examples{
dir.create("ddg", showWarnings=FALSE)
ddg.init()
number <- 3
ddg.data("number")
a <- number + 1
ddg.procedure("f")
ddg.data.in("number", "f")
ddg.data.out("a", pname="f")
if (a < 0) {
  warning ("a is negative!")
  ddg.procedure("a < 0")
  ddg.exception.out("negative", "a is negative!", "a < 0")
}
writeLines("Hello, world", "hello.txt")
ddg.procedure("Say hello")
ddg.file.out("hello.txt", pname="Say hello")
year <- c(1991, 1998, 2011)
name <- c("Sterling", "Smuggles", "Snickers")
male <- c(FALSE, FALSE, FALSE) 
pets.df <- data.frame(year, name, male)
ddg.procedure("data.frame(year, name, male)")
ddg.data.in("year", pname="data.frame(year, name, male)")
ddg.data.in("name", pname="data.frame(year, name, male)")
ddg.data.in("male", pname="data.frame(year, name, male)")
ddg.snapshot.out("pets.df", pname="data.frame(year, name, male)")
ddg.save()
}
\keyword{provenance}