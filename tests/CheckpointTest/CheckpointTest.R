###
# Test Script for Checkpoint Functionality. 
# Author @Barbara Lerner

# Modified by Luis Perez 7-Jul-2014
# Modified by Luis Perez 17-Jul-2014

rm (list=ls())

source("/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/R/DDGCheckpoint.R")

f <- function() {
	n
}

n <<- 1
print (paste("n before checkpoint 1 =", f()))
checkpoint.file.1 <- ddg.checkpoint("checkpoint1")

n <<- 2
print (paste("n before checkpoint 2 =", f()))
checkpoint.file.2 <- ddg.checkpoint()
print("Returned from checkpoint function")

print(paste("Restoring ", checkpoint.file.1))
ddg.restore(checkpoint.file.1)
print (paste("n after restore from checkpoint 1 =", f()))
if (n != 1) {
	stop ("restore of checkpoint 1 failed!")
}
	
n <<- 3

f()
	
print(paste("Restoring ", checkpoint.file.2))
ddg.restore(checkpoint.file.2)
if (n != 2) {
	stop ("restore of checkpoint 2 failed!")
}
print (paste("n after restore from checkpoint 2 =", f()))
	

