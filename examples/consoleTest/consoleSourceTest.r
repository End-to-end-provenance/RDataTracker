# Tests of ddg functions using ddg.source command.  

# The output of this script should be the exact same as the normal output for 
# script being sourced. The only difference is that when automatically testing,
# this script should produce the manual ddg.txt as IF it had been run from the
# console.
 
# Author @Luis Perez

library(RDataTracker)

## Directories
testDir <- "[DIR_DEFAULT]/"
setwd(testDir)

# Source the script
ddg.source("consoleTest.r")
