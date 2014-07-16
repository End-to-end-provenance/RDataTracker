\name{InitSave}
\alias{ddg.init}
\alias{ddg.run}
\alias{ddg.grabhistory}
\alias{ddg.save}
\alias{ddg.flush.ddg}
\alias{ddg.console.off}
\alias{ddg.console.on}
\title{Creating Provenance Graphs with RDataTracker}
\description{These functions are needed to create provenance graphs.}
\usage{
ddg.init(r.script.path = NULL, ddgdir = NULL, enable.console = FALSE) 
ddg.grabhistory()
ddg.save()
ddg.run(f, r.script.path = NULL, ddgdir = NULL, enable.console = FALSE)
ddg.console.on()
ddg.console.off()
ddg.flush.ddg()
}
\arguments{
  \item{r.script.path}{ the full path to the file containing the R script that is being executed. }
  \item{ddgdir}{the directory where the DDG should be saved. }
  \item{enable.console}{ If TRUE, any commands executed in the console, either by typing, copying and 
    pasting, or selecting and running, will result in a procedure node created in the provenance graph, with
    data nodes created for each variable assigned and data flow edges for variables used and set. }
  \item{f}{ A function to run.  Data provenance is collected within the function. }
}
\details{
  In order to use RDataTracker to collect data provenance, the user must either call ddg.init() 
  at the beginning of execution and ddg.save() at the end, or the user must call ddg.run().  
  When using ddg.init(), it is possible to call ddg.save() multiple times.  Each call will 
  save the current provenance graph in a file, overwriting the previous version that was saved. 

  For longer scripts, it might be necessary to call ddg.grabhistory() instead of ddg.save(). This function call saves the current ddg to memory, analyzing variable names under the current environemnt (current meaning the environment when ddg.grabhistory() is called). It does not write out the information to disk. A final call to ddg.save() is still required to do this.
  
  ddg.init initializes the provenance graph.  If r.script.path is not NULL, the R script is copied
  into the DDG directory, becoming a permanent part of the provenance record.  If ddgdir is NULL,
  the provenance graph will be saved in a subdirectory called "ddg" within the current working directory.
  The DDG directory must be created before calling ddg.init().
  
  ddg.save writes the provenance graph along with additional provenance information to a file named
  ddg.txt.  The extra information includes the version of R being used, the operating system, and 
  a timestamp of the execution.  ddg.save() can be called multiple times for a single call to 
  ddg.init(), where each call will extend the previous provenance graph, overwriting the file
  containing the previous version.
  
  ddg.run provides a short cut for ddg.init()...ddg.save().  It initializes the provenance graph,
  calls the function provided as a parameter, and then saves the provenance graph.  The graph
  will contain just the provenance created by calls to the DDG functions.  If an R error occurs 
  during the execution of the function, the error will be captured in an Exception node in the 
  provenance graph.

  ddg.console.on and ddg.console.off toggle the console parameter for DDG creation. When the console is enabled, all commands sent to the R
  console are captured as provenance by the RDataTracker library. The functions allow for an intermix of automatic annotations
  as well as manual, more detailed, annotations of a script. 
  Note that a call to ddg.console.off will create a collapsible console node with data provenence of the previous console session,
  while ddg.console.on will initiate a console session. No action is performed if the console is already in the desired state. 
  
  ddg.flush.ddg removes all files from the DDG directory unless the DDG directory is also the working 
  directory, in which case it does nothing.
}
\author{Emery Boose and Barbara Lerner}
\examples{
dir.create("ddg", showWarnings=FALSE)
ddg.init()
ddg.save()
myfunc <- function() {
  a <- 1
}
ddg.grabhistory()
ddg.run(myfunc)
}
\keyword{provenance}
