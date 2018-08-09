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
library(RDataTracker)

## Directories
testDir <- "C:/github/rdatatracker/scriptTests/Plot3Test/"
setwd(testDir)

# get initial time
startTime <- Sys.time()
invisible(force(startTime))

# turn off usage of fancy quotes
options(useFancyQuotes=FALSE)

# Run the script
#ddg.run("Plot3Test.R", "C:/github/rdatatracker/scriptTests/Plot3Test", ignore.ddg.calls = F)
prov.run("Plot3Test.R", "C:/github/rdatatracker/scriptTests/Plot3Test", annotate.inside.functions=TRUE, max.loops=1, max.snapshot.size=10)

# Calculate total time of execution
endTime <- Sys.time()
cat("Execution Time =", difftime(endTime, startTime, units="secs"))
