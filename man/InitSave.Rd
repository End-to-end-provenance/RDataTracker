\name{InitSave}
\alias{ddg.init}
\alias{ddg.run}
\alias{ddg.annotate.on}
\alias{ddg.annotate.off}
\alias{ddg.save}
\alias{ddg.flush.ddg}
\alias{ddg.console.off}
\alias{ddg.console.on}
\alias{ddg.display}
\title{Creating Provenance Graphs with RDataTracker}
\description{These functions are needed to create provenance graphs.}
\usage{
ddg.init(r.script.path = NULL, ddgdir = NULL, overwrite = TRUE, enable.console = TRUE, 
    max.snapshot.size = 100) 
ddg.save(r.script.path = NULL, save.debug=FALSE, quit=FALSE)
ddg.run(r.script.path = NULL, ddgdir = NULL, overwrite = TRUE, f = NULL, enable.console = TRUE, 
    annotate.inside = TRUE, max.snapshot.size = 100, debug = FALSE, save.debug=FALSE, display = FALSE)
ddg.annotate.on(fnames = NULL)
ddg.annotate.off(fnames = NULL)
ddg.console.on()
ddg.console.off()
ddg.flush.ddg(ddg.path = NULL)
ddg.display()
}
\arguments{
  \item{r.script.path}{ the full path to the file containing the R script that is being executed. }
  \item{ddgdir}{the directory where the DDG should be saved. }
  \item{overwrite}{Defaults is TRUE, if FALSE, adds timestamp to ddg directory to prevent overwriting}
  \item{enable.console}{ If TRUE, any commands executed in the console, either by typing, copying and 
    pasting, or selecting and running, will result in a procedure node created in the provenance graph, with
    data nodes created for each variable assigned and data flow edges for variables used and set. }
  \item{annotate.inside}{specifies whether automatic annotation of functions and control constructs should be enabled. }
  \item{max.snapshot.size}{ The maximum size for objects that should be output in snapshot
    files.  If max.snapshot.size is -1, there is no limit. If max.snapshot.size is 0, snapshot nodes
    are created but no snapshot files are saved. }
  \item{debug}{ If TRUE, enable script debugging. }
  \item{save.debug}{ If TRUE, save debug files to debug directory. }
  \item{display}{ If TRUE, display the DDG when the R script completes.}
  \item{f}{ A function to run.  Data provenance is collected within the function. }
  \item{fnames}{ A list of one or more function names. }
  \item{quit}{ If TRUE, all DDG files are removed from memory. }
  \item{ddg.path}{The path to the DDG directory which needs to be flushed. }
}
\details{
  In order to use RDataTracker to collect data provenance, the user must either call ddg.init 
  at the beginning of execution and ddg.save at the end, or the user must call ddg.run.  
  When using ddg.init, it is possible to call ddg.save multiple times.  Each call will 
  save the current provenance graph in a file, overwriting the previous version that was saved. 

  ddg.init initializes the provenance graph.  If r.script.path is not NULL, the R script is copied
  into the DDG directory, becoming a permanent part of the provenance record.  If ddgdir is NULL,
  the provenance graph will be saved in a subdirectory called "ddg_[script name]" in the script's directory.
  Further changes can be made to the save directory by specifying overwrite = FALSE, which will add a
  timestamp to the directory to prevent overwriting, i.e. "ddg_script.name_2016-06-09T15.41.02EDT"
   
  ddg.save writes the provenance graph along with additional provenance information to a file named
  ddg.txt.  The extra information includes the platform and operating system, the R version, the name
  of the R script, and a timestamp of the execution.  ddg.save can be called multiple times for a 
  single call to ddg.init, where each call will extend the previous provenance graph, overwriting the file
  containing the previous version. When the final save procedure is wanted, the parameter "quit" can 
  be set to TRUE, causing all temporary files in memory to be cleared out.  While not strictly 
  necessary, this prevents issues when creating multiple DDGs from the same script. If save.debug
  is set to TRUE, debug files are saved to the debug directory.
  
  ddg.run provides a short cut for ddg.init...ddg.save.  It initializes the provenance graph,
  calls the script or function provided as a parameter, and then saves the provenance graph.  
  If a script is provided, the script is sourced using ddg.source (see \code{\link{ddg.source}}), 
  a DDG is created for the script, and a copy of the script is saved with the DDG.  If a function 
  is provided, the function is executed with calls to ddg.init and ddg.save so that provenance 
  for the function is captured. In either case, if an R error occurs during execution, the error will be
  captured in an Exception node in the provenance graph. If annotate.inside is set to TRUE, 
  provenance is collected for statements inside functions and inside control constructs
  (if, for, while, repeat, and simple block).  ddg.annotate.on and ddg.annotate.off may be used to limit 
  the functions that will be annotated or not annotated, respectively. If break is set to TRUE, script debugging  is enabled. This has the same effect as inserting ddg.breakpoint() at the top of the script. 
  If display is set to TRUE, the DDG is displayed after the R script finished executing. If save.debug 
  is set to TRUE, debug files are saved to the debug directory.
  
  ddg.console.on and ddg.console.off toggle the console parameter for DDG creation. 
  When the console is enabled, all commands sent to the R console are captured as provenance 
  by the RDataTracker library. These functions allow for an intermixing of automatic and more detailed 
  manual annotations of a script. Note that a call to ddg.console.off will create a collapsible console 
  node with data provenence of the previous console session, while ddg.console.on will initiate
  a console session. No action is performed if the console is already in the desired state. 

  ddg.flush.ddg removes all files from the DDG directory specified unless the DDG directory is also 
  the working directory, in which case it does nothing. If no DDG directory is specified, the current
  DDG directory (if any) is assumed.
  
  The last DDG created (if any) can be displayed with ddg.display. This function starts DDG Explorer
  and loads the most recent ddg.txt file (if any). If the DDG path is not set or a ddg.txt file is not available,
  the function returns "DDG not available".
}
\author{Emery Boose and Barbara Lerner}
\examples{
dir.create("ddg", showWarnings=FALSE)
ddg.init()
ddg.save()
myfunc <- function() {
  a <- 1
}
ddg.run(f = myfunc)
}
\keyword{provenance}
