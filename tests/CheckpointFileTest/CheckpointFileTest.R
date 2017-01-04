###
# Test Script for Checkpoint Functionality. 
# Author @Barbara Lerner

# Modified by Luis Perez 7-Jul-2014
# Modified by Luis Perez 17-Jul-2014

rm (list=ls())

source("/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/R/DDGCheckpoint.R")


write("This file will be deleted", "testfile3.txt")
if (!file.exists("testfile3.txt")) {
  print ("testfile3.txt is missing!")
  stop()
}
	
checkpoint <- ddg.checkpoint("my checkpoint")
	
write("abc",file="testfile2.txt",append=TRUE)
if (!file.exists("testfile2.txt")) {
  print ("testfile2.txt is missing!")
  stop()
}

unlink("testfile3.txt")
if (file.exists("testfile3.txt")) {
  print ("testfile3.txt should have been removed!")
  stop()
}

ddg.restore(checkpoint)
if (!file.exists("testfile3.txt")) {
  print ("testfile3.txt was not restored!")
  stop()
}

if (file.exists("testfile2.txt")) {
  print ("testfile2.txt was not removed!")
  stop()
}

print ("Yay!  Tests passed!")

