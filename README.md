What is RDataTracker?
=====================

RDataTracker is a library of R functions that can be used to collect
data provenance in the form of a Data Derivation Graph (DDG) as
an R script executes. The DDG is saved in PROV-JSON format to
the file ddg.json. The DDG can be viewed and queried with DDG
Explorer, a separate visualizing tool written in Java, which is 
currently included in the RDataTracker install package.

What is a DDG?
==============

A Data Derivation Graph (DDG) is a mathematical graph that captures the
detailed history of a data analysis. The DDG consists of nodes and edges
that represent steps or operations (procedural nodes), intermediate data
values (data nodes), the order in which steps are executed (control flow
edges), and how data values are used or created (data flow edges). Note
that the DDG always records a particular execution and a new DDG is 
created every time an R script is run.

In DDG Explorer, nodes and edges are shown as ovals and arrows
(respectively), with the node or edge type indicated in different colors
(as explained in the legend). Collapsible and expandable nodes provide
a level of abstraction by allowing sections of the DDG to be expanded
or collapsed. Clicking on a procedural node displays the corresponding 
lines in the source code while clicking on a data node displays the 
intermediate data value. Warnings and errors from the R interpreter are
captured in error nodes at the point in the execution where they occur.

Installing RDataTracker
=======================

RDataTracker currently requires the following R packages: gtools, XML, 
knitr, stringr, utils, devtools, and methods.

RDataTracker is easily installed from GitHub using devtools:

> library(devtools)
> install_github("End-to-end-provenance/RDataTracker")

Once installed, use the R library command to load RDataTracker:

> library(RDataTracker)

Note that all RDataTracker functions begin with "ddg." to avoid confusion
with variable or function names in the main script or other libraries.

Using RDataTracker
==================

To capture data provenance for an R script, set the working directory, 
load the RDataTracker package (as above), and enter the following command:

> ddg.run("script.R")

where "script.R" is an an R script on the working directory. The ddg.run
command will execute the script and save the provenance information in
a subdirectory called "script_ddg" under the working directory.

To view the DDG, enter the following command:

> ddg.display()

This command will call up DDG Explorer and display the last DDG created.

The ddg.run command has numerous parameters that can be used to control
how it works. For example:

- r.script.path = the full path to the R script (if not in the working directory)

- annotate.inside =  whether to collect provenance for statements inside functions 
and control constructs (default = TRUE)
   
- first.loop = the number of the first iteration of a loop to annotate (default = 1)

- max.loops = the maximum number of iterations of a loop to annotate (default = 1)

- max.snapshot.size = the maximum size (in kilobytes) for objects saved in 
snapshot files (default = 10)

- debug = execute the script one line at a time with an option to view the DDG
(default = FALSE)

- display = display the DDG in DDG Explorer once the script completes
execution (default = FALSE)

For example, to collect provenance for the first 10 iterations of control construct
loops and display the DDG immediately after the script completes execution, enter:

> ddg.run("script.R", max.loops=10, display=TRUE)

For more information on ddg.run, see the help pages for RDataTracker.

Additional Features
==================

RDataTracker includes numerous other features, several of which are outlined
below.  For more details, see the help pages for RDataTracker.

Level of Detail Collected

The following commands may be used at the R command line to set 
level of detail collected by RDataTracker:

> ddg.set.detail(n) = sets the level of detail from no annotation of internal statements
and no snapshot files (n = 0) to annotation of all loop iterations and creation of
snapshot files of any size (n = 3).

> ddg.get.detail() = returns the current level of detail

> ddg.clear.detail() = clears the current level of detail

These commands provide an alternative to setting individual parameters in ddg.run.

Breakpoints

The following commands can be used at the R command line to set one or more
breakpoints in a script (or sourced script):

> ddg.set.breakpoint(script.name, line.num) =  set a breakpoint at the specified line
number of the specified script

> ddg.list.breakpoints() = display all current breakpoints

> ddg.clear.breakpoints() = clear all current breakponts

When the script is run with ddg.run, execution is paused at the breakpoint with an
option to view the DDG created up to that point.
