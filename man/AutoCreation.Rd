\name{AutomaticCreation}
\alias{ddg.source}
\title{Automatic DDG Creation From Script}
\description{These functions replace commonly used R functions which retooled 
versios which automatically capture provenance data. }
\usage{
ddg.source(file, local = FALSE, echo = verbose, print.eval = echo, 
    verbose = getOption("verbose"), max.deparse.length = 150, chdir = FALSE, 
    encoding = getOption("encoding"), ignore.ddg.calls = TRUE, 
    ignore.init = ignore.ddg.calls, force.console=ignore.init)
}
\arguments{
  \item{file}{see \code{\link{source}}}
  \item{local}{see \code{\link{source}}}
  \item{echo}{see \code{\link{source}}}
  \item{print.eval}{see \code{\link{source}}}
  \item{verbose}{see \code{\link{source}}}
  \item{max.deparse.length}{see \code{\link{source}}}
  \item{chdir}{see \code{\link{source}}}
  \item{encoding}{see \code{\link{source}}}
  \item{ignore.ddg.calls}{specifies whether commands in the source script 
  pertaining to DDGs should be ignored.}
  \item{ignore.init}{specifies whether suppresion of source script DDG Initialization
  should occur.}
  \item{force.console}{specifies whether console parsing should be forced on the source script}
} 
\seealso{
  \code{\link{source}}.
}
\details{
  This is a rewriting of the \code{\link{source}} function to support provenance
  collection for sourced files. For more information on source pertinant  
  parameters and functions, see the source help page.

  Data Provenance is collected from the sourced file and a DDG is created.

  If ignore.ddg.calls is set to TRUE, then all code statement beginning with 
  "ddg." are ignored. They are not executed nor incorporated into the created 
  DDG. Note that this only holds true for "ddg." calls at the top level, as nested
  calls within functions are executed with a call to the function.This parameter
  is used if an existing RScript is partly annotated, but when sourcing, 
  automatic console annotation are required. 

  This is also useful if automatic annotations of a sourced script should be 
  included in the DDG of the script sourcing file. Ignoring "ddg." statements 
  leads to the automatically created DDG being incorporated into the current DDG
  (if one is already initialized).

  Note that ignore.ddg.calls set to TRUE implies ignore.init must also be true. 
  If this is not the case, then ignore.init will be coerced to TRUE.

  The paramenter ignore.init is used when a sourced script is already annotated,
  but instead of creating a new DDG, the annotated DDG of the source script 
  should be incorporated into the current DDG. When ignore.ddg.calls is set to 
  FALSE and ignore.init is set to TRUE, all calls beginning with "ddg.init" will
  be ignored but others will be executed.

  This prevents the initialization of a new DDG. Therefore, the provenance is 
  stored in the currently initialized DDG (if one exists).

  Further, note that all calls to "library(RDataTracker)" are ignored in the
  sourced file.

  The parameter of force.console turns on automatic node creation for the sourced
  R Script. More precisely, a call to \code{\link{ddg.console.on}} is made before
  any processing of the script. However, note that changes executed within the sourced
  script to the console mode will make changes to the environment.
  For example, if init.ignore is FALSE but force.console is set to TRUE, then 
  the ddg.init call in the source script will overwrite the console mode. In a sense,
  script changes prevail, but under ambiguous situations, force.console will determine
  the state of command capturing.

  In any case, the console is returned to the mode set before the call to ddg.source.

  Note that a call to ddg.save is always made once the script has been sourced.
}
\author{Luis Perez}
\keyword{provenance}
