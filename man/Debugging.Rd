\name{Debugging}
\alias{ddg.debug.on}
\alias{ddg.debug.off}
\title{RDataTracker Debugging}
\description{These functions turn debugging output on or off during the collection of data provenance 
  by RDataTracker. }
\usage{
ddg.debug.on()
ddg.debug.off()
}
\details{
  With debugging on, RDataTracker displays output in the console as the provenance graph is created, 
  showing each node and edge that is created.  Debugging is off by default.
}
\author{Emery Boose and Barbara Lerner}
\examples{
dir.create("ddg", showWarnings=FALSE)
ddg.init()
ddg.debug.on()
ddg.debug.off()
ddg.save()
}
\keyword{provenance}