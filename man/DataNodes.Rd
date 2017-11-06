\name{DataNodes}
\alias{ddg.data}
\alias{ddg.file}
\alias{ddg.url}
\alias{ddg.exception}
\title{RDataTracker Data Nodes}
\description{These functions create data nodes within a provenance graph created by RDataTracker.}
\usage{
ddg.data(dname, dvalue = NULL, graphic.fext = "jpeg")
ddg.file(filename, dname = NULL)
ddg.url(dname, dvalue = NULL)
ddg.exception(dname, dvalue = NULL)
}
\arguments{
  \item{dname}{ the label for the node. }
  \item{dvalue}{ the value of the data. }
  \item{filename}{ the name of the file to copy. }
  \item{graphic.fext}{ the extension to use for the captured image file name. }
  \item{data}{ the value to save in the file. }
  
}
\details{ddg.data creates a data node for single or complex data values. dname can either be a quoted string, 
  a name, or an expression containing the data value. If an expression is passed in, the label on 
  the node will be the unevaluated expression.  If dvalue is omitted, the argument passed in for dname will 
  be evaluated and its value used in place of dvalue. Complex data values (such as data frames) will be save 
  in a file named by dname. The file extentison will have an appropriate extension based on the data stored. 
  (for example, data frames are stored as .csv files). If dvalue is a graphical object (type plot or ggplot, etc.), 
  an image will be captured of the active graphic device and saved in a file called dname with extension
  graphic.fext.  Available graphics formats are jpeg, jpg, tiff, png, and bmp.

  ddg.file creates a data node of type File and copies an existing file to the DDG directory.
  If dname is omitted, the filename, minus the directory path, will be used as the label.
  Use to save a copy of an input file.

  ddg.url creates a data node of type URL.  Use to save a URL that appears in the R script. 

  ddg.exception creates a data node of type Exception.  Use to record an error that
  occurs in the script.
}

\author{Emery Boose and Barbara Lerner}
% \examples{
% dir.create("ddg", showWarnings=FALSE)
% ddg.init()
% ddg.data("number", 15)
% x <- 1
% ddg.data(x)
% writeLines("Hello, world", "hello.txt")
% ddg.file("hello.txt")
% year <- c(1991, 1998, 2011)
% name <- c("Sterling", "Smuggles", "Snickers")
% male <- c(FALSE, FALSE, FALSE) 
% pets.df <- data.frame(year, name, male)
% ddg.data("pets", "csv", pets.df)
% ddg.url("hf", "http://harvardforest.fas.harvard.edu")
% ddg.exception("FileNotFound", "File not found")
% ddg.save()
% }
\keyword{provenance}
