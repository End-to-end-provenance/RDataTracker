source("D:/Users/Luis/Documents/Harvard School Work/Summer 2014/RDataTracker/R/RDataTracker.R")
ddg.init('D:/Users/Luis/Documents/Harvard School Work/Summer 2014/RDataTracker/examples/CalculateSquareRoot/calculate-square-root-annotated.r','D:/Users/Luis/Documents/Harvard School Work/Summer 2014/RDataTracker/examples/CalculateSquareRoot/ddg-annotated',enable.console=TRUE)

##------ Wed Jul 23 00:49:26 2014 ------##
#############################
### CALCULATE SQUARE ROOT ###
#############################

# Calculate square root iteratively
# ERB rev. 29-May-2014

# Modified by Luis Perez 22-Jul-2014

### Functions

set.initial.values <- function() {
  number <<- 10
  ddg.data("number")
  tolerance <<- 0.00001
  ddg.procedure(lookup.ins = TRUE, outs.data=list("tolerance", "number"))
}

get.random <- function(number) {
  # get random seed value
  estimate <- runif(1,1,number)
  ddg.procedure(lookup.ins = TRUE,outs.data=list("estimate"))
  return(estimate)
}

calc.square.root <- function(number,estimate) {
  # calculate square root
  x <- number/estimate
  estimate <- (estimate+x)/2
  ddg.procedure(lookup.ins = TRUE,outs.data=list("estimate"))
  return(estimate)
}

get.difference <- function(number,estimate) {
  # test result
  difference <- abs(number-estimate^2)
  ddg.procedure(lookup.ins = TRUE, outs.data=list("difference"))
  return(difference)
}

get.check.value <- function(difference,tolerance) {
  # compare difference to tolerance
  check <- difference - tolerance
  ddg.procedure(lookup.ins = TRUE, outs.data=list("check"))
  return(check)
}

store.result <- function(number,estimate) {
  sqr.root <- data.frame(number,estimate)
  ddg.procedure(lookup.ins = TRUE, outs.data=list("sqr.root"))
  return(sqr.root)
}

write.result <- function(sqr.root) {
  file.name <- "results.csv"
  file.out <- paste(getwd(),"/",file.name,sep="")
  write.csv(sqr.root,file.out,row.names=FALSE)
  ddg.procedure(lookup.ins = TRUE, outs.file=list(file.name))
}

### Main Program
ddg.start("Calculate Square Root")
ddg.start("Initial estimate")
set.initial.values()
  
estimate <- get.random(number)
check <- number

ddg.finish("Initial estimate")

ddg.start("Approximations")
i <- 0
while (check > 0) {
  # repeat calculation until tests OK
  i <- i + 1
  ddg.start(paste("Iteration",i))
  estimate <- calc.square.root(number,estimate)
  difference <- get.difference(number,estimate)  
  check <- get.check.value(difference,tolerance)
  ddg.finish(paste("Iteration", i))
}
ddg.finish("Approximations")

ddg.start("Export to file")
sqr.root <- store.result(number,estimate)
write.result(sqr.root)
ddg.finish("Export to file")

### Display values

sqr.root

ddg.finish("Calculate Square Root")


ddg.save(quit=TRUE)
