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

# command arguments
args <- commandArgs(TRUE)

tool <- args[1]
detailLevel <- args[2]

# determine library
if( identical(tool, "rdt") ) {
	library(rdt)
} else if( identical(tool, "rdtLite") ) {
	library(rdtLite)
} else {
	stop("Provenance collection library is not specified.", call. = FALSE)
}

# determine detail level
if( identical(detailLevel, "true") ) {
	detailLevel <- TRUE
} else {
	detailLevel <- FALSE
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
if( identical(tool, "rdt") ) {
	prov.run("[SCRIPT]", "[DIR_DDG]", 
		details=detailLevel,
		annotate.inside.functions=TRUE, 
		max.loops=1, snapshot.size=10)
} else if( identical(tool, "rdtLite") ) {
	prov.run("[SCRIPT]", "[DIR_DDG]", 
		details=detailLevel, snapshot.size=10, filecopy.size=2)
} else {
	stop("Provenance collection library is not found.", call. = FALSE)
}

# Calculate total time of execution
endTime <- Sys.time()
cat("Execution Time =", difftime(endTime, startTime, units="secs"))
