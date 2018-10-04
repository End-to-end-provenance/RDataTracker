What is provR?
==============

provR is an R package that collects provenance as an R script 
executes. The resulting provenance provides a detailed record of the 
execution of the script and includes information on the steps that were 
performed and the intermediate data values that were created. The 
resulting provenance can be used for a wide variety of applications
that include debugging scripts, cleaning code, and reproducing results.
provR can also be used to collect provenance during console sessions.

The provenance is stored in PROV-JSON format (for details see JSON-format.md).
For immediate use it maybe retrieved from memory using the prov.json function. 
For later use the provenance is also written to the file prov.json. This file 
and associated files are written by default to the R session temporary
directory. The user may change this location by (1) using the optional
parameter prov.dir in the prov.run or prov.init functions, or (2) setting
the prov.dir option (e.g. by using the R options command or editing the
Rprofile.site or .Rprofile file). If prov.dir is set to ".", the current working
directory is used.

provR provides two modes of operation. In script mode, the prov.run
function is used to execute a script and collect provenance as the script 
executes. In console mode, provenance is collected during a console session.
Here the prov.init function is used to initiate provenance collection,
prov.save is used to save provenance collected since the last time prov.save
was used, and prov.quit is used to save and close the provenance file.

Simple data values are stored in the PROV-JSON file. "Snapshots" of complex
data values (e.g. data frames) are optionally stored by adjusting
the value of the parameter max.snapshot.size in prov.run or prov.init.

Installing provR
================

provR currently requires R version 3.5.0 (or later) and the 
following R packages: curl, devtools, digest, ggplot2, grDevices, 
gtools, jsonlite, knitr, methods, stringr, tools, utils, XML.

provR is easily installed from GitHub using devtools:
```
library(devtools)
install_github("End-to-end-provenance/provR")
```

Once installed, use the R library command to load provR:
```
library(provR)
```

Note that all exported provR functions begin with "prov." to 
avoid confusion with variable or function names in the main script 
or other libraries.

Using provR
===========

To capture provenance for an R script, set the working directory, 
load the provR package (as above), and enter the following:
```
prov.run("my-script.R")
```

where "my-script.R" is an R script in the working directory. The 
prov.run command will execute the script and save the provenance in 
a subdirectory called "prov_my-script" under the current provenance
directory (as above).

To capture provenance for a console session, enter the following:
```
prov.init()
```
and enter commands at the R console. To save the provenance collected 
so far to a subdirectory called "prov_console" under the current
provenance directory (as above), enter the following:
```
prov.save()
```
To save the provenance and quit provenance collection, enter the 
following:
```
prov.quit()
```
Note that various parameters of the prov.run and prov.init functions
may be used to control where the provenance is stored and whether earlier 
provenance at the same location should be overwritten.
