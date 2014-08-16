\name{Checkpoint}
\alias{ddg.checkpoint}
\alias{ddg.restore}
\title{RDataTracker Checkpoint and Restore}
\description{These functions work with RDataTracker to save and restore the R state and the file 
  system state.  They also record in the provenance graph where the checkpoints and restore 
  occur and their effect on the data used.  Note that these functions are not included
  in the RDataTracker library since they change the user's global environment when a restore is
  performed. To use these functions, you must download and source DDGCheckpoint.R.}
\usage{
ddg.checkpoint(checkpoint.name = NULL)
ddg.restore(file.path)
}
\arguments{
  \item{checkpoint.name}{ the value associated with the checkpoint procedure node. }
  \item{file.path}{ the name of the checkpoint file to restore. }
} \value{
  ddg.checkpoint returns the full path to the file in which the checkpoint is saved.  This value should
  be passed to any subsequent calls to ddg.restore that want to restore that particular checkpoint.
}
\details{ddg.checkpoint saves the current R state in a file and returns the path to the saved file.  It 
  adds a Checkpoint node and a Snapshot node to the provenance graph created by RDataTracker.  
  The Snapshot node references the file that contains the saved state.
  
  ddg.restore can be called later by passing in the name of a checkpoint file.  It will restore the
  state of the R workspace to the state of the checkpoint file.  It will also revert any files that were saved 
  by an earlier call to ddg.file or ddg.file.out to the state they were in at the time that the checkpoint was taken.
  
  ddg.restore also adds a Restore node to the provenance graph, with an input edge from the saved
  checkpoint file.  The provenance graph will reflect the variable state following the restore.
}
\author{Emery Boose and Barbara Lerner}
\examples{
\dontrun{source("DDGCheckpoint.R")}
dir.create("ddg", showWarnings=FALSE)
ddg.init()
\dontrun{checkpoint <- ddg.checkpoint()}
\dontshow{# Following results in a "cannot add bindings to a locked environment" error}
\dontrun{ddg.restore(checkpoint)}
ddg.save()
}
\keyword{provenance}