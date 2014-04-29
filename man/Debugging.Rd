\name{Debugging}
\alias{ddg.debug.on}
\alias{ddg.debug.off}
\title{RDataTracker Debugging}
\description{These functions allow the user to turn debugging output on during the collection of 
  provenance data by RDataTracker}
\usage{
ddg.debug.on()
ddg.debug.off()
}
\details{
  With debugging on, RDataTracker displays output as the provenance graph is created, showing 
  each node and edge that is created.
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