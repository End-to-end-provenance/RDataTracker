# Tests of ddg functions using ddg.source command.  This is a template file 
# which is copied to examples/TEST/local directory, changed in format, and then
# executed. The expected output template for each test should be found in 
# examples/TEST/EXPECTED_SOURCE_OUPUT 

# In theory, the output of this script should be the exact same as the normal 
# output for scripts being sourced. The only difference is that when automatically 
# testing, this script should produce the manual ddg.txt as IF it had been run from the
# console.
 
# Author @Luis Perez

library(methods)

# determine which prov-collection library to run
tool <- commandArgs(TRUE)[1]

if( identical(tool, "prov") ) {
	library(provR)
} else if( identical(tool, "rdt") ) {
	library(RDataTracker)
} else {
	stop("Provenance collection library is not specified.", call. = FALSE)
}

## Directories
testDir <- "[DIR_LOCAL]/"
setwd(testDir)

# get initial time
startTime <- Sys.time()
invisible(force(startTime))

# turn off usage of fancy quotes
options(useFancyQuotes=FALSE)

# Run the script
if( identical(tool, "prov") ) {
	prov.run("[SCRIPT]", "[DIR_DDG]", snapshot.size=10)
} else if( identical(tool, "rdt") ) {
	prov.run("[SCRIPT]", "[DIR_DDG]", annotate.inside.functions=TRUE, 
		max.loops=1, snapshot.size=10)
} else {
	stop("Provenance collection library is not found.", call. = FALSE)
}

# Calculate total time of execution
endTime <- Sys.time()
cat("Execution Time =", difftime(endTime, startTime, units="secs"))
