\name{InitSave}
\alias{ddg.init}
\alias{ddg.run}
\alias{ddg.save}
\alias{ddg.flush.ddg}
\title{Creating Provenance Graphs with RDataTracker}
\description{These functions are needed to create provenance graphs.}
\usage{
ddg.init(r.script.path = NULL, ddgdir = NULL, 
    enable.console = FALSE) 
ddg.save()
ddg.run(f, r.script.path = NULL, ddgdir = NULL, 
    enable.console = FALSE)
ddg.flush.ddg()
}
\arguments{
  \item{r.script.path}{ the full path to the file containing the R script that is being executed }
  \item{ddgdir}{the directory where the DDG should be saved }
  \item{enable.console}{ If true, any commands executed in the console, either by typing, copy-and-paste
    or selecting and running will result in a procedure node created in the provenance graph, with
    data nodes created for each variable assigned, and data flow edges for variables used and set. }
  \item{f}{ A function to run, collecting the provenance within that function. }
}
\details{
  In order to use RDataTracker to collect provenance data, the user must either call ddg.init() 
  at the beginning of execution and ddg.save() at the end, or the user must call ddg.run().  
  When using ddg.init(), it is possible to call ddg.save() multiple times.  Each call will 
  save the current provenance graph in a file, overwriting the previous version that was saved.
  
  ddg.init initializes the provenance graph.  If r.script.path is not NULL, the script is copied
  into the ddg directory, becoming a permanent part of the provenance record.  If ddgdir is NULL,
  the provenance graph will be saved in a directory called "ddg" within the current working directory.
  The ddg directory must be created before calling ddg.init().
  
  ddg.save writes the provenance graph along with additional provenance information to a file named
  ddg.txt.  The extra information includes the version of R being used, the operating system, and 
  a timestamp of the execution.  ddg.save() can be called multiple times for a single call to 
  ddg.init(), where each call will extend the previous provenance graph, overwriting the file
  containing the previous version.
  
  ddg.run provides a short cut for ddg.init()...ddg.save().  It initializes the provenance graph,
  calls the function provided as a parameter and then saves the provenance graph.  The provenance
  will contain just the provenance created by calls to the ddg functions.  If an R error occurs 
  during the execution of the function, the error will be captured in an Exception node in the 
  provenance graph.
  
  ddg.flush.ddg removes all files from the DDG directory unless the DDG directory is also the working 
  directory.  In the latter case, it does nothing.
}
\author{Emery Boose and Barbara Lerner}
\examples{
dir.create("ddg", showWarnings=FALSE)
ddg.init()
ddg.save()
myfunc <- function() {
  a <- 1
}
ddg.run(myfunc)
}
\keyword{provenance}