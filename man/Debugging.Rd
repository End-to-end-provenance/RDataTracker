\name{Debugging}
\alias{ddg.debug.lib.on}
\alias{ddg.debug.lib.off}
\alias{ddg.breakpoint}
\alias{ddg.set.breakpoint}
\alias{ddg.list.breakpoints}
\alias{ddg.clear.breakpoints}
\title{RDataTracker Debugging}
\description{These functions turn debugging output on or off during the collection of data provenance 
  by RDataTracker. }
\usage{
ddg.debug.lib.on()
ddg.debug.lib.off()
ddg.breakpoint()
ddg.set.breakpoint(script.name, line.num)
ddg.list.breakpoints()
ddg.clear.breakpoints()
}
\arguments{
  \item{script.name}{ the name of the script for the new breakpoint. }
  \item{line.num}{ the line number for the new breakpoint. }
}
\details{
  With debugging on, RDataTracker displays output in the console as the provenance graph is created, 
  showing each node and edge that is created.  Debugging is off by default.
  
  Script debugging is enabled by (1) setting debug to TRUE in ddg.run, (2) inserting one or more 
  breakpoints in the script with ddg.breakpoint, and/or (3) inserting breakpoints at the command line
  using ddg.set.breakpoint with the script name and line number. Breakpoints inserted at the command
  line can be displayed with ddg.list.breakpoints and cleared with ddg.clear.breakpoints.
  
  Once in debugging mode, the user can execute the  next command by pressing Enter, continue execution 
  until the next breakpoint by entering "C", view the ddg text created for the last command by entering "D",
  or quit debugging and complete execution by entering "Q".
}
\author{Emery Boose and Barbara Lerner}
% \examples{
% dir.create("ddg", showWarnings=FALSE)
% ddg.init()
% ddg.debug.lib.on()
% ddg.debug.lib.off()
% ddg.save()
% }
\keyword{provenance}