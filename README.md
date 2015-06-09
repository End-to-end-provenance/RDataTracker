What is RDataTracker?
=====================

RDataTracker is a library of R functions that can be used to collect
data provenance in the form of a Data Derivation Graph (DDG) during R
console sessions or when executing an R script. The level of detail in
the DDG can be increased by annotating the session or script with
additional calls to the library. The DDG is saved as a text file
(ddg.txt) in a special DDG directory, where it can be viewed, stored,
and queried using a separate tool, DDG Explorer.

What is a DDG?
==============

A Data Derivation Graph (DDG) is a mathematical graph that captures the
history of a data analysis. The DDG consists of nodes and edges. In DDG
Explorer, nodes are shown as ovals and edges are shown as arrows (see
below for examples). Different colors (explained in the legend for DDG
Explorer) are used to indicate different types of nodes and edges.

There are two major types of nodes—procedural nodes and data nodes—and
two major types of edges—control flow edges and data flow edges. Control
flow edges indicate how control passes from one procedural node to the
next procedural node as the script executes. Data flow edges indicate
how input data pass from a data node to a procedural node or how output
data pass from a procedural node to a data node.

Procedural nodes include Operational, Binding, Collapsible, Expandable,
Checkpoint, and Restore nodes. Operational nodes perform an operation.
Binding nodes indicate how actual parameters are bound to formal
parameters in R functions. Collapsible nodes and expandable nodes
provide a level of abstraction by allowing a section of the DDG to be
expanded or collapsed. Checkpoint nodes indicate creation of a
checkpoint. Restore nodes indicate that a previous checkpoint was
restored.

Data nodes include Data, File, Snapshot, URL, and Exception nodes. Data
nodes are used for simple values. File nodes are used for files that are
inputs to the R script or created by the R script. Snapshot nodes are
used for complex data values such as vectors, arrays, and data frames,
as well as graphical outputs. URL nodes are used for URL addresses.
Exception nodes are used for error messages. The values of Data, URL,
and Exception nodes are stored in the DDG text file. The values of File
and Snapshot nodes are stored as files in the DDG directory.

Note that the DDG always records a *particular* execution. A new DDG is
created every time a console session is completed or an R script is run.

For more details on DDGs and how to view, store, and query them, please
see *Using DDG Explorer*.

Installing RDataTracker
=======================

The following instructions assume you are using RStudio.

RDataTracker is distributed as an R package. Note that R packages must
be *installed* to your computer (normally just once) and then *loaded*
for use in a particular session.

To install RDataTracker, copy or download the package file to your
computer, open RStudio, and use the Tools / Install Packages option to
install from a Package Archive File (alternatively you can use the
**install.packages** command at the R prompt). The library depends on
**gtools**, so make sure this package is installed before attempting
installation of RDataTracker. Once the library has been installed,
select Packages / RDataTracker to see a list of help pages for the
various functions. Note that all functions begin with **ddg.** to avoid
confusion with function names in the main script or other libraries.

To load RDataTracker, use the **library** command at the R prompt or at
the top of your script. Alternatively you can click on the checkbox for
RDataTracker in the list of packages in RStudio.

Because they may alter the user’s environment, the checkpoint and
restore features are implemented in a separate R script
(DDGCheckpoint.R) that must be downloaded separately and loaded using
the R **source** function.

Using RDataTracker
==================

Getting Started
---------------

If you like to work in the R console, see [sec:console] Console
Sessions.\
If you like to source your R scripts, see [sec:source] Sourcing R
Scripts.\
If you like to select and run R scripts, see [sec:selectrun] Select and
Run.

For information on how to create abstraction levels, see
[sec:abstraction] Abstraction.\
For information on individual functions, see [sec:details] Detailed
Annotations.\
For information on checkpoint and restore, see [sec:checkpoint]
Checkpoint and Restore.

For help with troubleshooting problems, see [sec:troubleshoot]
Troubleshooting.

**Please note:** Example scripts are shown in code boxes followed by the
DDG as it is displayed in DDG Explorer.

Console Sessions 
----------------

RDataTracker can be used to collect data provenance during R console
sessions. Consider the following session:

    # CONSOLE SESSION

    # Load the library.
    > library(RDataTracker)

    # Set the working directory.
    > setwd("c:/data/r/test")

    # Initialize DDG collection.
    > ddg.init()

    # Console session.
    > x <- 3
    > y <- 7
    > z <- x + y

    # Save the DDG.
    > ddg.save()

![Console Session](vignettes/UsingRDataTracker-img/UsingRDataTracker-console.jpg?raw=TRUE)

Here line 4 loads the RDataTracker package. Line 7 sets the working
directory. Line 10 initiates a DDG. Lines 13-15 are user commands
entered at the console. Line 18 saves the DDG to file.

Because no DDG directory is specified, the DDG is saved by default to a
directory called “ddg” in the working directory. In the DDG, the user’s
commands are enclosed within start and finish Console nodes, which can
be expanded or collapsed in DDG Explorer. If the user entered more
commands followed by another **ddg.save**, the DDG would be expanded to
include the new commands enclosed within a second set of Console nodes.
To quit and clear the DDG from memory, the user could enter
**ddg.save(quit=TRUE)** Note that calling **ddg.init** again would start
a new DDG, overwriting the current DDG if the same DDG directory were
used.

In console sessions RDataTracker creates procedure nodes for each
top-level assignment statement along with the corresponding data nodes.
More details can be captured by adding calls to library functions as
described below.

There are two limitations to collecting data provenance in console
sessions. First, the DDG is built from the R history file, which on most
system is limited to 512 lines. For long R console sessions, the
**ddg.save** function must be called every 500 lines or so to avoid loss
of provenance information. Second, RDataTracker can only evaluate
variables when calls are made to library functions. If the value of a
variable is changed and you would like to record the earlier value, call
**ddg.save** before the value is changed.

Sourcing R Scripts 
------------------

RDataTracker can be used to collect data provenance while sourcing an R
script. Consider the following simple script:

    # R SCRIPT

    f <- function(x) {
      x <- x^2
      return(x)
    }

    a <- 10
    b <- f(a)

The following commands at the console can be used to execute this script
and collect data provenance without having to add any annotations to the
script itself:

    # CONSOLE SESSION

    # Load the library.
    > library(RDataTracker)

    # Set the working directory.
    > setwd("c:/data/r/test")

    # Execute the script.
    > ddg.run("test.r")

![Sourcing a Script with
**ddg.run**](vignettes/UsingRDataTracker-img/UsingRDataTracker-source1.jpg?raw=TRUE)

Here line 4 loads the RDataTracker package. Line 7 sets the working
directory. Line 10 uses the **ddg.run** function to execute the script
and create a DDG. Since a path for the R script is not provided in
**ddg.run**, the script is assumed to be in the working directory. Since
a DDG directory is not specified, the DDG is saved by default in a
directory called “ddg” in the working directory.

Note that many library commands can accept as input either a name or a
value. So in this case we could define a variable to hold the name of
the script and use that variable to execute the script:

    r.script.path <- "test.r"
    ddg.run(r.script.path)

**ddg.run** initializes and saves the DDG so there is no need for the
user to do this. Because console mode is enabled, RDataTracker creates
procedure nodes for each top-level assignment statement along with the
corresponding data nodes. Since **ddg.run** does not rely on the R
history file, intermediate data values are saved in the DDG.

More information may be collected by adding function calls to the
script. For example, the above script might be annotated to show
function parameter bindings:

    # R SCRIPT

    f <- function(x) {
      x <- x^2
      ddg.function()
      return(ddg.return.value(x))
    }

    a <- 10
    b <- f(a)

Running this script in the console using **ddg.run** (as above) yields
the following DDG:

![Sourcing a Script with Function
Annotations](vignettes/UsingRDataTracker-img/UsingRDataTracker-source2.jpg?raw=TRUE)

Here **ddg.function** in line 5 causes RDataTracker to create a data
node for formal parameter **x** in function **f** and to show the
binding of actual parameter **a** to **x**; while **ddg.return.value**
in line 6 creates a data node for the return value of **f** which is
then assigned to variable **b**.

Select and Run 
--------------

RDataTracker can be used to collect data provenance by annotating an R
script and then selecting and running the script (or a portion of it).
For example:

    # R SCRIPT

    library(RDataTracker)
    setwd("c:/data/r/test")

    ddg.init("test.r")

    f <- function(x) {
      x <- x^2
      ddg.function()
      return(ddg.return.value(x))
    }

    a <- 10
    b <- f(a)

    ddg.save()

A data flow edge to an existing data node may also be created using
**ddg.data.in**. The name of operational node may be omitted if
**ddg.data.in **is called from within an R function.

Here is the resulting DDG as displayed in DDG Explorer:

![Select and Run in Console
Mode](vignettes/UsingRDataTracker-img/UsingRDataTracker-selectrun1.jpg?raw=TRUE)

This script is identical to the preceding one except for lines 3-4,
which load the library and set the working directory, and the calls to
**ddg.init** and **ddg.save** in lines 6 and 17. When this script is
copied and pasted into the console, it produces a DDG that is identical
to the one above except for the start and finish nodes.

To ensure completeness of the DDG we generally recommend that the
**enable.console** parameter in **ddg.run** or **ddg.init** retain its
default value (TRUE). However setting this parameter to FALSE may be
useful if you wish to specify exactly what provenance is collected or
wish to collect provenance for only a portion of a script. For example:

    # R SCRIPT

    library(RDataTracker)
    setwd("c:/data/r/test")

    ddg.init("test.r", enable.console=FALSE)

    i <- 10
    j <- 2*i

    f <- function(x) {
      x <- x^2
      ddg.function()
      return(ddg.return.value(x))
    }

    a <- 10
    ddg.data(a)

    ddg.eval("b <- f(a)")

    ddg.save()

![Select and Run Not in Console
Mode](vignettes/UsingRDataTracker-img/UsingRDataTracker-selectrun2.jpg?raw=TRUE)

Here **enable.console** in **ddg.init** is set to FALSE. Because the
assignment statements in lines 8, 9, and 11 are not annotated, they are
not captured in the DDG.

Abstraction 
-----------

Expandable and collapsible nodes, implemented through calls to
**ddg.start** and **ddg.finish**, may be used to create levels of
abstraction in the DDG. These functions must be correctly nested with
matching arguments. The node name may be omitted if **ddg.start** and
**ddg.finish** are called within an R function, in which case the
function name will be used to label the nodes. Clicking on the start or
finish node in DDG Explorer will collapse the intervening nodes to a
single node, while clicking on a collapsed node will expand to reveal
the intervening nodes.

    # R SCRIPT

    ddg.start("create.data.frame")

    x <- c(1, 2, 3)
    y <- c(4, 5, 6)
    z <- data.frame(x, y)

    ddg.finish("create.data.frame")

![Abstraction Levels](vignettes/UsingRDataTracker-img/UsingRDataTracker-abstract.jpg?raw=TRUE)

When this script is executed with **ddg.run**, data nodes are created
for vectors **x** and **y** and data frame **z**. These data nodes are
enclosed within collapsible start and finish nodes called
**create.data.frame**, which in turn are enclosed within start and
finish nodes for the script itself (**test.r**).

Detailed Annotations 
--------------------

The functions described in this section provide additional flexibility
and control in deciding what data provenance to capture in the DDG. For
more details and a complete list of DDG functions, please see the help
pages for RDataTracker in RStudio. The examples below were executed
using **ddg.run** with console mode enabled. The resulting DDGs are
shown as displayed in DDG Explorer.

1.  To create an Operational node for an R function in the original
    script, use **ddg.function**. Input data nodes (if any) are assumed
    to exist when this function is called. The R function name and input
    parameters are looked up in the calling environment and a binding
    node is created for each pair of actual and formal parameters. The
    **outs** parameters may be used with lists of values to create
    output data nodes for global variables assigned in the function.

    To create a data node for the function return value, use
    **ddg.return.value** in each function **return** statement as
    illustrated below. If there is no **return** statement, use
    **ddg.return.value** with the last function statement.

        f <- function(x) {
          d <<- 20
          x <- x^2
          ddg.function(outs.data=list("d"))
          return(ddg.return.value(x))
        }

        a <- 10
        b <- f(a)

    ![Operational Node with
    **ddg.function**](vignettes/UsingRDataTracker-img/UsingRDataTracker-function.jpg?raw=TRUE)

    This example creates a data node called **a** with value 10, a
    procedural node for the function **f**, a binding node indicating
    that actual parameter **a** is bound to formal parameter **x**, a
    data node called **d** with value 20, and data nodes **f(a) return**
    and **b** with value 100, with corresponding data flow edges.

2.  To create an Operational node for a procedure not implemented as an
    R function in the original script, use **ddg.procedure**. Input data
    nodes (if any) are assumed to exist when this function is called.
    The procedure name must be supplied. The **ins** parameter may be
    used with a list of values to create input data flow edges. The
    **outs** parameters may be used with lists of values to create
    output data nodes and corresponding data flow edges.

    The separate function **ddg.data.in** may be used in place of
    **ins**; while the separate functions **ddg.data.out**,
    **ddg.file.out**, **ddg.url.out**, **ddg.exception.out**, and
    **ddg.graphic.out**, depending on the data type, may be used in
    place of **outs**. Note that, unlike **ddg.data.in** and
    **ddg.data.out**, the **ins** and **outs** parameters require quoted
    strings unless the name is the name of a file.

    The following two scripts produce identical DDGs:

        x <- 0

        for (i in 1:3) {
          ddg.data(i)
          x <- x + i^2
          ddg.procedure("sum.squares", ins=list("i"), outs.data=list("x"))
        }

        x <- 0

        for (i in 1:3) {
          ddg.data(i)
          x <- x + i^2
          ddg.procedure("sum.squares")
          ddg.data.in(i, pname="sum.squares")
          ddg.data.out(x, pname="sum.squares")
        }

    ![Operational Node with
    **ddg.procedure**](vignettes/UsingRDataTracker-img/UsingRDataTracker-procedure.jpg?raw=TRUE)

    This example creates a procedural node called **sum.squares** with
    input data node **i** and output data node **x** for each iteration
    of the **while** loop.

3.  To create an input data node not created by a procedural node, use
    **ddg.data**, **ddg.file**, **ddg.url**, or **ddg.exception**,
    depending on the data type. If only the variable name is supplied,
    the library will look up its value in the current environment. Note
    that these functions are generally not required for top-level
    statements if console mode is enabled. See preceding example.

4.  To evaluate a statement and create associated data nodes and edges,
    use **ddg.eval**. If the statement is an assignment, a data node for
    the variable assigned and a corresponding data flow edge are
    created. Note that this function is not required for top-level
    statements if console mode is enabled.

    Compare the DDGs created by the following two scripts:

        x <- 0

        for (i in 1:2) {
          x <- x + i^2
        }

    ![Loop without
    **ddg.eval**](vignettes/UsingRDataTracker-img/UsingRDataTracker-eval1.jpg?raw=TRUE)

    Here data nodes are created for **x** before and after the **while**
    loop is executed.

        x <- 0

        for (i in 1:2) {
          ddg.eval("x <- x + i^2")
        }

    ![Loop with
    **ddg.eval**](vignettes/UsingRDataTracker-img/UsingRDataTracker-eval2.jpg?raw=TRUE)

    Here **start** and **finish** nodes for the assignment statement and
    a data node for **x** are created for each iteration of the
    **while** loop as a result of the **ddg.eval** function.

5.  To turn on and off automatic capture of data provenance in console
    mode use **ddg.console.on** and **ddg.console.off**. Commands
    captured while console mode is enabled are enclosed in start and
    finish Console nodes. Note that console mode is enabled by default
    in **ddg.run** and **ddg.init**.

        a <- 10
        ddg.console.off()
        b <- 20

    ![Turning Console Mode On and
    Off](vignettes/UsingRDataTracker-img/UsingRDataTracker-consoleoff.jpg?raw=TRUE)

    This example creates a data node called **a** with value 10 enclosed
    by Console start and finish nodes. A data node for **b** is not
    created.

6.  To insert the DDG from a sourced script into the larger DDG of the
    main script, use **ddg.source** in place of **source**. The DDG from
    the sourced script is encompassed within start and finish nodes
    named after the sourced script.

        # SCRIPT-1.R

        b <- 20

        # TEST.R

        a <- 10
        ddg.source("script-1.r")
        d <- a + b

    ![Inserting the DDG for a Sourced
    Script](vignettes/UsingRDataTracker-img/UsingRDataTracker-source.jpg?raw=TRUE)

    This example inserts the DDG for **script-1.r** into the DDG for
    **test.r**. A data flow edge connects data node **b** created in the
    sourced script to the assignment statement for **d** in the main
    script.

7.  To initiate a DDG, use **ddg.init**; to save a DDG, use
    **ddg.save**. The parameter **max.snapshot.size** in **ddg.init**
    may be used to limit the size of snapshot files in order to improve
    performance or to save file space. Note that **ddg.save** may be
    called multiple times in the same console session or script, while
    **ddg.init** creates a new DDG each time it is called. Both
    functions are called by **ddg.run**.

        ddg.init(max.snapshot.size=0)
        ddg.save(quit=TRUE)

    In this example the first line initiates creation of a DDG. Snapshot
    nodes are created but snapshot values are not saved in the DDG. The
    second line saves the DDG and removes DDG objects from memory.

The following examples illustrate the use of some of these functions.
Both use an iterative process to estimate the square root of 10 to six
decimal places. The first script is not annotated and is run with
**ddg.run**. The second script is annotated and is run by copying and
pasting to the console with console mode turned off.

    # R SCRIPT

    number <- 10
    estimate <- 3
    tolerance <- 0.000001

    while (abs(number - estimate^2) > tolerance) {
      estimate <- (number/estimate + estimate)/2
    }

![Example Script with
**ddg.run**](vignettes/UsingRDataTracker-img/UsingRDataTracker-sqrroot1.jpg?raw=TRUE)

    # R SCRIPT

    library(RDataTracker)
    setwd("c:/data/r/test")

    ddg.init("test.r", enable.console=FALSE)

    number <- 10
    ddg.data(number)

    estimate <- 3
    ddg.data(estimate)

    tolerance <- 0.000001
    ddg.data(tolerance)

    while (abs(number - estimate^2) > tolerance) {
      ddg.start("iteration")

      estimate <- (number/estimate + estimate)/2

      ddg.procedure("calc.sqr.root", ins=list("number", "estimate", "tolerance"), outs.data=list("estimate"))

      ddg.finish("iteration")
    }

    ddg.save()

![Example Script with
Annotations](vignettes/UsingRDataTracker-img/UsingRDataTracker-sqrroot2.jpg?raw=TRUE)

The first DDG captures the essentials of the process, including the
assignment statements and the final value of **estimate**. The second
DDG captures additional details, including the results of each iteration
within collapsible **start** and **finish** nodes for the procedure
**calc.sqr.root**.

Checkpoint and Restore 
----------------------

A separate R script called **DDGCheckpoint.R** contains functions that
may be used to save and restore the R state and file system state,
allowing a user to return to the environment and data files present at
an earlier point in the data analysis. **ddg.checkpoint** creates a
procedural node for the checkpoint operation, a snapshot node for the
checkpoint file, and the corresponding data flow edge, and returns the
full path to this file. **ddg.restore** creates a procedural node for
the restore operation with a data flow edge from the snapshot node for
the saved checkpoint file. A given checkpoint can be restored more than
once.

    # R SCRIPT

    library(RDataTracker)
    source("c:/data/r/DDGCheckpoint.R")

    setwd("c:/data/r/test")

    ddg.init("test.r")

    a <- 10

    checkpoint1 <- ddg.checkpoint()

    a <- 20

    ddg.restore(checkpoint1)

    b <- a

    ddg.save()

Here a checkpoint called **checkpoint1** is created in line 12 and
restored in line 16. In line 18, **b** is assigned the original value of
**a** (10). The checkpoint information is stored in and retrieved from
the file **0.RData**.

Note that the DDGCheckpoint.R file is not included in RDataTracker and
must be separately sourced.

![Checkpoint and
Restore](vignettes/UsingRDataTracker-img/UsingRDataTracker-checkpoint.jpg?raw=TRUE)

Troubleshooting 
---------------

Annotation errors are generally captured by RDataTracker and stored as
error nodes in the DDG. The same is true for error messages from the R
interpreter if an R script is executed with **ddg.run**. These features
may be useful for troubleshooting the original script and the associated
annotations.

The **ddg.debug.on** and **ddd.debug.off** functions may be used in an R
script or at the console to turn debugging on and off. Debugging is off
by default. When debugging is turned on, details related to creation of
the DDG are displayed in the console as the script executes.

The contents of the current DDG directory (if different from the working
directory) may be deleted by calling the **ddg.flush.ddg** function at
the R prompt.

Trapping of DDG and R error messages is illustrated below. These scripts
were run in console mode using **ddg.run**. In both cases the error
message is stored in the DDG as the value of the error node
**error.msg** and may be viewed by right clicking on that node in DDG
Explorer.

    # R SCRIPT

    a <- 10
    ddg.data(b)
    b <- 20

![DDG Error](vignettes/UsingRDataTracker-img/UsingRDataTracker-error1.jpg?raw=TRUE)

Here a DDG error is generated (“In .ddg.insert.error.message(error.msg):
Unable to evaluate b in call to ddg.data”) but the R script continues to
execute.

    # R SCRIPT

    a <- 10
    c <- a * b

![R Error](vignettes/UsingRDataTracker-img/UsingRDataTracker-error2.jpg?raw=TRUE)

Here an R error (“Error in eval(expr, envir, enclos): object ’b’ not
found”) terminates execution of the R script.

Technical Details
=================

How RDataTracker Works
----------------------

RDataTracker collects data provenance and stores it in memory during a
console session or while an R script is executing. This information is
written to the DDG file (**ddg.txt**) in the DDG directory whenever the
function **ddg.save** is called. The DDG file contains information about
the computing environment, the number of procedural steps, and the
specifications for individual nodes and edges of the DDG. Procedural
nodes and data nodes are each numbered in sequence beginning with one.
Simple data values (e.g. numbers and strings) are stored in the DDG
itself. More complex data values (e.g. vectors, arrays, data frames, and
graphics) are stored as pointers to files created in the DDG directory.
Input and output files of the main script are stored as pointers to
copies of those files created in the DDG directory. While the DDG file
can be viewed using a text editor, its primary purpose is to support
exchange of information and it should normally be viewed and queried
using DDG Explorer.

RDataTracker maintains internal tables of data nodes, procedural nodes,
and function return values as a console session proceeds or a script
executes. These tables are used to look up nodes when creating control
flow edges and data flow edges. RDataTracker keeps track of variable
scope and a data node of the same name is not considered a match unless
it also has the same scope. Selected values from these tables are
written to the DDG directory in the files **dnodes.txt**,
**pnodes.txt**, and **returns.txt** whenever **ddg.save** is called.

Known Issues
------------

**Timestamp in history file**. The timestamp function in RStudio for
Windows does not work correctly. A workaround was devised to create our
own timestamps in such cases.

**Return nodes in recursive functions**. If the recursive function does not 
explicitly call ddg.function, values recorded with ddg.return.value will not
get linked in correctly.

**S3 objects and reference classes**. RDataTracker has not been tested
with S3 objects or reference classes.

Acknowledgements
================

The authors acknowledge intellectual contributions from collaborators
Leon Osterweil (University of Massachusetts Amherst), Aaron Ellison
(Harvard University), and Harvard Forest REU students listed below. The
work was supported by NSF grants DEB-0620443, DEB-1237491, and
DBI-1003938, the Charles Bullard Fellowship at Harvard University, and a
faculty fellowship from Mount Holyoke College and is a contribution from
the Harvard Forest Long-Term Ecological Research (LTER) program. Any
opinions, findings, and conclusions or recommendations expressed in this
material are those of the authors and do not necessarily reflect the
views of the National Science Foundation, Harvard University, or Mount
Holyoke College.

Numerous students have been involved in related research and tool
development through the Harvard Forest Summer Research (REU) program,
including Shaylyn Adams (2013), Vasco Carinhas (2013), Andrew Galdunski
(2011), Nicole Hoffler (2014), Antonia Miruna Oprescu (2012), Luis
Antonio Perez (2014), Garrett Rosenblatt (2011), Cory Teshera-Sterne
(2009), Sofiya Toskova (2010-2011), Morgan Vigil (2010), and Yujia Zhou
(2012).

