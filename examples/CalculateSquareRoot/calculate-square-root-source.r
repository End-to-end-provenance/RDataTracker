source("D:/Users/Luis/Documents/Harvard School Work/Summer 2014/RDataTracker/R/RDataTracker.R")
ddg.init('D:/Users/Luis/Documents/Harvard School Work/Summer 2014/RDataTracker/examples/CalculateSquareRoot/calculate-square-root-source.r','D:/Users/Luis/Documents/Harvard School Work/Summer 2014/RDataTracker/examples/CalculateSquareRoot/ddg-source',enable.console=TRUE)

##------ Wed Jul 23 13:19:50 2014 ------##
#############################
### CALCULATE SQUARE ROOT ###
#############################

# Calculate square root iteratively
# ERB rev. 29-May-2014

# Modified for timing scripts by Luis Perez on 22-Jul-2014

### Functions

set.initial.values <- function() {
  number <<- 10
  tolerance <<- 0.00001
}

get.random <- function(number) {
  # get random seed value
  estimate <- runif(1,1,number)
  return(estimate)
}

calc.square.root <- function(number,estimate) {
  # calculate square root
  x <- number/estimate
  estimate <- (estimate+x)/2
  return(estimate)
}

get.difference <- function(number,estimate) {
  # test result
  difference <- abs(number-estimate^2)
  return(difference)
}

get.check.value <- function(difference,tolerance) {
  # compare difference to tolerance
  check <- difference - tolerance
  return(check)
}

store.result <- function(number,estimate) {
  sqr.root <- data.frame(number,estimate)
  return(sqr.root)
}

write.result <- function(sqr.root) {
  file.name <- "results.csv"
  file.out <- paste(getwd(),"/",file.name,sep="")
  write.csv(sqr.root,file.out,row.names=FALSE)
}

### Main Program

set.initial.values()
  
estimate <- get.random(number)
check <- number

while (check > 0) {
  # repeat calculation until tests OK
  estimate <- calc.square.root(number,estimate)
  difference <- get.difference(number,estimate)
  check <- get.check.value(difference,tolerance)
}

sqr.root <- store.result(number,estimate)
write.result(sqr.root)

### Display values

sqr.root

ddg.save(quit=TRUE)
