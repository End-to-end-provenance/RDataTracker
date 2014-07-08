#############################
### CALCULATE SQUARE ROOT ###
#############################

# Calculate square root iteratively
# Record DDG in text format
# ERB rev. 6-Feb-2014

### Directories

#ddg.library <- Sys.getenv("DDG_LIBRARY")
#if (ddg.library == "") {
#	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
#}
#source(ddg.library)
library(RDataTracker)

## Directories
testDir <- "[DIR_DEFAULT]/"
setwd(testDir)

### Functions

get.initial.values <- function() {
  number <<- 10
  tolerance <<- 0.00001

  ddg.procedure(outs.data=list("number","tolerance"))
}

get.random <- function(number) {
  # get random seed value
  estimate <- runif(1,1,number)

  ddg.procedure(ins=list("number"),outs.data=list("estimate"))

  return(estimate)
}

calc.square.root <- function(number,estimate) {
  # calculate square root
  x <- number/estimate
  estimate <- (x+estimate)/2

  ddg.procedure(ins=list("number","estimate"),outs.data=list("estimate"))

  return(estimate)
}

get.difference <- function(number,estimate) {
  # test result
  difference <- abs(number-estimate^2)

  ddg.procedure(ins=list("number","estimate"),outs.data=list("difference"))

  return(difference)
}

get.check.value <- function(difference,tolerance) {
  #compare difference to tolerance
  check <- difference - tolerance
    
  ddg.procedure(ins=list("difference","tolerance"),outs.data=list("check"))
  
  return(check)
}

store.result <- function(number,estimate) {
  sqr.root <- data.frame(number,estimate)

  ddg.procedure(ins=list("number","estimate"),outs.data=list("sqr.root"))
  
  return(sqr.root)
}

write.result <- function(sqr.root) {
  file.name <- "results.csv"
  file.out <- paste(getwd(),"/",file.name,sep="")
  write.csv(sqr.root,file.out,row.names=FALSE)

 ddg.procedure(ins=list("sqr.root"),outs.file=list("file.name"))
}

### Main Program

main <- function() {
  ddg.start("main")

  get.initial.values()

  estimate <- get.random(number)

  ddg.start("get.square.root")

  check <- number

  while (check > 0) {
    ddg.start("get.next.estimate")

    # repeat calculation until tests OK
    estimate <- calc.square.root(number,estimate)
    difference <- get.difference(number,estimate)
    check <- get.check.value(difference,tolerance)
  
    ddg.finish("get.next.estimate")
  }

  ddg.finish("get.square.root")

  ddg.start("write.result")
  sqr.root <<- store.result(number,estimate)
  write.result(sqr.root)
  ddg.finish("write.result")

  ddg.finish("main")
}

ddg.run(main,		
		paste(testDir,"calculate-square-root-2.r",sep=""),
		paste(testDir,"ddg", sep=""))

### Display values

sqr.root

