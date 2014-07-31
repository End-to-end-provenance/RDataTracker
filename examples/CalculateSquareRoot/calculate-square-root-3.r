#############################
### CALCULATE SQUARE ROOT ###
#############################

# Calculate square root iteratively
# ERB rev.5-Feb-2014
# Modified by Luis Perez 7-Jul-2014
# Modified by Luis Perez 17-Jul-2014

### Directories

#source("/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/R/RDataTracker.R")
library(RDataTracker)

# ddg.init(ddg.r.script.path,ddg.path)

## Directories
if (interactive()) {
  testDir <- getwd()
} else {
  testDir <- "[DIR_DEFAULT]"
  setwd(testDir)
}

### Functions

get.initial.values <- function() {
  number <<- 10
  tolerance <<- 0.00001
  
  ddg.procedure()
  ddg.data.out(number)
  ddg.data.out(tolerance)
}

get.random <- function(number) {
  # get random seed value
  estimate <- runif(1,1,number)
  
  ddg.procedure()
  ddg.data.in(number)
  
  ddg.return(estimate)
}

calc.square.root <- function(number,estimate) {
  # calculate square root
  x <- number/estimate
  estimate <- (estimate+x)/2
  
  ddg.procedure(lookup.ins=TRUE)
  ddg.return(estimate)
}

get.difference <- function(number,estimate) {
  # test result
  difference <- abs(number-estimate^2)
  
  ddg.procedure(lookup.ins=TRUE)
  ddg.return(difference)
}

get.check.value <- function(difference,tolerance) {
  #compare difference to tolerance
  check <- difference - tolerance
  
  ddg.procedure(lookup.ins=TRUE)
  ddg.return(check)
}

store.result <- function(number,estimate) {
  sqr.root <- data.frame(number,estimate)
  
  ddg.procedure(lookup.ins=TRUE)
  ddg.return(sqr.root)
}

write.result <- function(sqr.root) {
  file.name <- "results.csv"
  file.out <- paste(getwd(),"/",file.name,sep="")
  write.csv(sqr.root,file.out,row.names=FALSE)
  
  ddg.procedure(lookup.ins=TRUE)
  ddg.file.out(file.name)
}

### Main Program

main <- function() {
  ddg.start("main")
  
  get.initial.values()
  ddg.eval("estimate <- get.random(number)")
  
  ddg.start("get.square.root")
  
  check <- number
  
  while (check > 0) {
    ddg.start("get.next.estimate")
    
    # repeat calculation until tests OK
    ddg.eval("estimate <- calc.square.root(number,estimate)")
    ddg.eval("difference <- get.difference(number,estimate)")
    ddg.eval("check <- get.check.value(difference,tolerance)")
    
    ddg.finish("get.next.estimate")
  }
  
  ddg.finish("get.square.root")
  
  ddg.start("write.result")
  
  ddg.eval("sqr.root <<- store.result(number,estimate)")
  write.result(sqr.root)
  
  ddg.finish("write.result")
  
  ddg.finish("main")
}

ddg.run( 	
        paste(testDir,"calculate-square-root-3.r",sep="/"),
        paste(testDir,"ddg",sep="/"),
        main,
        enable.console=FALSE)

# display values

sqr.root

ddg.save(quit=TRUE)
