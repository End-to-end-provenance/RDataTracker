\name{Debugging}
\alias{ddg.debug.lib.on}
\alias{ddg.debug.lib.off}
\alias{ddg.breakpoint}
\title{RDataTracker Debugging}
\description{These functions turn debugging output on or off during the collection of data provenance 
  by RDataTracker. }
\usage{
ddg.debug.lib.on()
ddg.debug.lib.off()
ddg.breakpoint()
}
\details{
  With debugging on, RDataTracker displays output in the console as the provenance graph is created, 
  showing each node and edge that is created.  Debugging is off by default.
  
  Script debugging is enabled by setting debug to TRUE in ddg.run and/or by inserting one or more 
  breakpoints in the script with ddg.breakpoint(). Once in debugging mode, the user can execute the 
  next command by pressing Enter, continue execution until the next breakpoint by entering "C", 
  or quit debugging and complete execution by entering "Q".
}
\author{Emery Boose and Barbara Lerner}
\examples{
dir.create("ddg", showWarnings=FALSE)
ddg.init()
ddg.debug.lib.on()
ddg.debug.lib.off()
ddg.save()
}
\keyword{provenance}