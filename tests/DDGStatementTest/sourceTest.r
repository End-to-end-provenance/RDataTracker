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
testDir <- "/home/thomas/workspace/RDataTracker/tests/DDGStatementTest/"
setwd(testDir)

# get initial time
startTime <- Sys.time()
invisible(force(startTime))

# Run the script
#ddg.run("DDGStatementTest.R", "/home/thomas/workspace/RDataTracker/tests/DDGStatementTest/ddg", ignore.ddg.calls = F)
ddg.run("DDGStatementTest.R", "/home/thomas/workspace/RDataTracker/tests/DDGStatementTest/ddg")

# Calculate total time of execution
endTime <- Sys.time()
cat("Execution Time =", difftime(endTime, startTime,units="secs"))
