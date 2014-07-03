#############################
### CALCULATE SQUARE ROOT ###
#############################

# Calculate square root iteratively
# ERB rev.5-Feb-2014

### Directories

#ddg.library <- Sys.getenv("DDG_LIBRARY")
#if (ddg.library == "") {
#	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
#}
#source(ddg.library)
library(RDataTracker)

# ddg.init(ddg.r.script.path,ddg.path)

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
  ddg.data.out(estimate)

  return(estimate)
}

calc.square.root <- function(number,estimate) {
  # calculate square root
  x <- number/estimate
  estimate <- (estimate+x)/2

  ddg.procedure()
  ddg.data.in(number)
  ddg.data.in(estimate)
  ddg.data.out(estimate)

  return(estimate)
}

get.difference <- function(number,estimate) {
  # test result
  difference <- abs(number-estimate^2)

  ddg.procedure()
  ddg.data.in(number)
  ddg.data.in(estimate)
  ddg.data.out(difference)

  return(difference)
}

get.check.value <- function(difference,tolerance) {
  #compare difference to tolerance
  check <- difference - tolerance
    
  ddg.procedure()
  ddg.data.in(difference)
  ddg.data.in(tolerance)
  ddg.data.out(check)
  
  return(check)
}

store.result <- function(number,estimate) {
  sqr.root <- data.frame(number,estimate)

  ddg.procedure()
  ddg.data.in(number)
  ddg.data.in(estimate)
  ddg.data.out(sqr.root)
  
  return(sqr.root)
}

write.result <- function(sqr.root) {
  file.name <- "results.csv"
  file.out <- paste(getwd(),"/",file.name,sep="")
  write.csv(sqr.root,file.out,row.names=FALSE)

  ddg.procedure()
  ddg.data.in(sqr.root)
  ddg.file.out(file.name)
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
		"/Users/blerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/CalculateSquareRoot/calculate-square-root-1.r",
		"/Users/blerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/CalculateSquareRoot/ddg")

# display values

sqr.root
