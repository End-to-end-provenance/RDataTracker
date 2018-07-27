# This is the part that follows the script being tested.
# See the comment in consoleTest-pre.R for more details
 
# Author @Barabara Lerner - adapted from sourceTest.r


# Calculate total time of execution
endTime <- Sys.time()
cat("Execution Time =", difftime(endTime, startTime,units="secs"))
