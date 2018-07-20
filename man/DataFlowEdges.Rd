\name{DataFlowEdges}
\alias{ddg.data.in}
\alias{ddg.data.out}
\alias{ddg.exception.out}
\alias{ddg.url.out}
\alias{ddg.file.out}
\alias{ddg.graphic.out}
\title{RDataTracker Data Flow Edges}
\description{These functions create data flow edges within a provenance graph created by RDataTracker.}
\usage{
ddg.data.in (dname, pname = NULL) 
ddg.data.out (dname, dvalue = NULL, pname = NULL, graphic.fext = 'jpeg')
ddg.exception.out (dname, dvalue = NULL, pname = NULL)
ddg.url.out (dname, dvalue = NULL, pname = NULL) 
ddg.file.out (filename, dname = NULL, pname = NULL)
ddg.graphic.out(dname, pname = NULL, graphic.fext = "jpeg")
}
\arguments{
  \item{dname}{ the label for the data node. }
  \item{pname}{ the name of the procedure node that this edge connects to. }
  \item{dvalue}{ the value to associate with the node being created. }
  \item{filename}{ the name of the file to copy, including the path if not in the working directory. }
  \item{graphic.fext}{ the file extension for the captured image file to be created. }
  \item{data}{ the data to write to the snapshot file. }
}\value{
  ddg.file.out returns the full path to the copied file
}
\details{
  If pname is NULL and the DDG function is called from within an R function, pname will be set to the
  name of the R function.
  
  If dvalue is NULL, the value passed to dname will be evaluated and that value will be used 
  as the value for the node.

  ddg.data.in creates a data flow edge from an existing data node (created by an earlier call
  to ddg.data, ddg.data.out, etc) to an existing procedure node (created by an earlier call 
  to ddg.function).
  
  ddg.data.out creates a Data or Snapshot node (depending on the input) and a data flow edge from an 
  existing procedure node (created by an earlier call to ddg.function).  Complex data 
  values are written to the file system in the DDG directory.
     
  ddg.exception.out behaves similarly to ddg.data.out, except that the node created is an
  Exception node.  This function should be used to record where an error message is created.
  
  ddg.url.out behaves similarly to ddg.data.out, except that the node created is a URL node.
  This should be used to record that the script has created a document or Web page 
  accessible via a URL.
  
  ddg.file.out behaves similarly to ddg.data.out, except that the node created is a File node.
  This should be used when the script writes a file and the user wants to save this version
  of the file in the provenance graph.  If dname is omitted, the
  filename (minus the directory path) will be used as the label for the node.

  ddg.graphic.out behaves similary to ddg.data.out with an input variable of type
  graphic, but does not require a variable to exist in the namespace. Instead, it directly copies 
  the active graphics devices and saves it as specified by the input parameters.
  Available file extensions are jpeg, jpg, tiff, png, and bmp.
}
\author{Emery Boose and Barbara Lerner}
\examples{
# create the directory and initialize the DDG
dir.create("ddg", showWarnings=FALSE)
ddg.init()
# store simple data (note call to ddg.data)
number <- 3
ddg.data("number")
a <- number + 1

# annoate conditional procedure
if (a < 0) {
  warning ("a is negative!")
}

# file output example
writeLines("Hello, world", "hello.txt")

# complex data example (note you use the same function ddg.data*. The library 
# automatically determined complexity of input value)
year <- c(1991, 1998, 2011)
name <- c("Sterling", "Smuggles", "Snickers")
male <- c(FALSE, FALSE, FALSE) 
pets.df <- data.frame(year, name, male)

# save ddg
ddg.save()
}
\keyword{provenance}
