# Tests of ddg functions using ddg.source command.  This is a template file 
# which is copied to examples/TEST/local directory, changed in format, and then
# executed. The expected output template for each test should be found in 
# examples/TEST/EXPECTED_SOURCE_OUPUT 

# In theory, the output of this script should be the exact same as the normal 
# output for scripts being sourced. The only difference is that when automatically 
# testing, this script should produce the manual ddg.txt as IF it had been run from the
# console.
 
# Author @Luis Perez

library(RDataTracker)

## Directories
testDir <- "[DIR_LOCAL]/"
setwd(testDir)

# get initial time
startTime <- Sys.time()
invisible(force(startTime))

# Run the script
#ddg.run("[SCRIPT]", "[DIR_DDG]", ignore.ddg.calls = F)
ddg.run("[SCRIPT]", "[DIR_DDG]")

# Calculate total time of execution
endTime <- Sys.time()
cat("Execution Time =", difftime(endTime, startTime,units="secs"))
