# Tests of RDataTracker as if the commands are entered from the console.
# The template script comes in 2 parts:  a pre part that appears before
# the script being tested and a post part that appears after the script
# being tested.  To form the test script, we concatenate the pre-part,
# the script being tested, and the post-part.
# This combined script is copied to examples/TEST/local directory, changed in format, and then
# executed. The expected output template for each test should be found in 
# examples/TEST/EXPECTED_SOURCE_OUPUT 

# In theory, the output of this script should be the exact same as the normal 
# output for the scripts being run from the console. The script being included
# should include the ddg.init and ddg.quit calls, and optionally 1 or more
# ddg.save calls betweent these.
 
# Author @Barabara Lerner - adapted from sourceTest.r

library(methods)
library(rdt)

## Directories
testDir <- "[DIR_LOCAL]/"
setwd(testDir)

# get initial time
startTime <- Sys.time()
invisible(force(startTime))

# turn off usage of fancy quotes
options(useFancyQuotes=FALSE)

