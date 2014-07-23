################################
### Interactive Timer Script ###
################################

# Interacts with the user and prompts them for a subdirectory and a script name of the vanilla
# script to be tested in compared. Prints the results. The directory should begin with 
# /subpath where subtpath is the path beginning from the path specified in timerFunctions.r

# Note that this script will create a minimal instrumentation script-min.r and overwrite any file by
# that name. It will also create the exact same script if script-annotated.r does not exists. Otherwise,
# it will time the performance of script-annotated.r
 
setwd("D:/Users/Luis/Documents/Harvard School Work/Summer 2014/RDataTracker/utilities")
source("timerFunctions.r")
source("helpers.r")

### Main script
# set initial conditions
setInitialVal(getFilePath())

# calculate results
results <- calcResults(getFileName())

# print results
print("Timing Results:")
print(results)
