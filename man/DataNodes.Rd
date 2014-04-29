\name{DataNodes}
\alias{ddg.data}
\alias{ddg.file}
\alias{ddg.snapshot}
\alias{ddg.url}
\alias{ddg.exception}
\title{RDataTracker Data Nodes}
\description{The functions described here are used to create data nodes within a provenance graph
  created by RDataTracker.}
\usage{
ddg.data(dname, dvalue=NULL)
ddg.file(filename, dname=NULL)
ddg.snapshot(dname, fext="csv", data=NULL)
ddg.url(dname, dvalue=NULL)
ddg.exception(dname, dvalue=NULL)
}
\arguments{
  \item{dname}{ the label for the node }
  \item{dvalue}{ data's value }
  \item{filename}{ name of the file to copy }
  \item{fext}{ the extension to use for the file name }
  \item{data}{ the value to save in the file }
  
}
\details{ddg.data creates a data node for a single data value.

dname can either be a quoted string or an expression.  If an expression is passed in, 
the label on the node will be the unevaluated expression. 

If dvalue is omitted, the argument passed in for dname will be evaluated and its value used in place
of dvalue.

ddg.file creates a data node of type File and copies an existing file to the DDG directory.
If dname is omitted, the filename, minus directory path, will be used as the label

ddg.snapshot creates a data node of type Snapshot.  It saves data in a file named by 
  dname.fext in the ddg directory.  This should be used to save complex values, like data frames,
  in the DDG.

ddg.url creates a data node of type URL.  Use this to remember a URL that is used in a script.  
dvalue should contain the full URL.  If dvalue is omitted, dname is used as the value.

ddg.exception creates a data node of type Exception.  Use this to record an error that
  occurs in the script.
}

\author{Emery Boose and Barbara Lerner}
\examples{
dir.create("ddg", showWarnings=FALSE)
ddg.init()
ddg.data("number", 15)
x <- 1
ddg.data(x)
writeLines("Hello, world", "hello.txt")
ddg.file("hello.txt")
year <- c(1991, 1998, 2011)
name <- c("Sterling", "Smuggles", "Snickers")
male <- c(FALSE, FALSE, FALSE) 
pets.df <- data.frame(year, name, male)
ddg.snapshot("pets", "csv", pets.df)
ddg.url("hf", "http://harvardforest.fas.harvard.edu")
ddg.exception("FileNotFound", "File not found")
ddg.save()
}
\keyword{provenance}