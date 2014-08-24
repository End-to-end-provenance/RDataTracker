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
  
  ddg.function()
  ddg.data.out(number)
  ddg.data.out(tolerance)
}

get.random <- function(n) {
  # get random seed value
  e <- runif(1,1,n)
  
  ddg.function()
  ddg.return.value(e)
}

calc.square.root <- function(n,e) {
  # calculate square root
  x <- n/e
  e <- (e+x)/2
  
  ddg.function()
  ddg.return.value(e)
}

get.difference <- function(n,e) {
  # test result
  d <- abs(n-e^2)
  
  ddg.function()
  ddg.return.value(d)
}

get.check.value <- function(d,t) {
  #compare difference to tolerance
  c <- d - t
  
  ddg.function()
  ddg.return.value(c)
}

store.result <- function(n,e) {
  sr <- data.frame(n,e)
  
  ddg.function()
  ddg.return.value(sr)
}

write.result <- function(sr) {
  fn <- "results.csv"
  file.out <- paste(getwd(),"/",fn,sep="")
  write.csv(sr,file.out,row.names=FALSE)
  
  ddg.function()
  ddg.file.out(fn)
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
  
  ddg.eval("sqr.root <- store.result(number,estimate)")
  write.result(sqr.root)
  
  ddg.finish("write.result")
  
  ddg.finish("main")
}

ddg.run( 	
        paste(testDir,"calculate-square-root-3.r",sep="/"),
        paste(testDir,"ddg",sep="/"),
        main,
        enable.console=FALSE)

ddg.save(quit=TRUE)
