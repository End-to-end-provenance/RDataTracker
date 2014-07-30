#############################
### CALCULATE SQUARE ROOT ###
#############################

# Calculate square root iteratively
# ERB rev. 29-May-2014

# Modified by Luis Perez 22-Jul-2014

### Functions

get.initial.values <- function() {
  number <<- 10
  tolerance <<- 0.00001
  ddg.procedure(lookup.ins = TRUE, outs.data=list("tolerance", "number"))
}

get.random <- function(number) {
  # get random seed value
  estimate <- runif(1,1,number)
  ddg.procedure(lookup.ins = TRUE)
  ddg.return(estimate)
}

calc.square.root <- function(number,estimate) {
  # calculate square root
  x <- number/estimate
  estimate <- (estimate+x)/2
  ddg.procedure(lookup.ins = TRUE)
  ddg.return(estimate)
}

get.difference <- function(number,estimate) {
  # test result
  difference <- abs(number-estimate^2)
  ddg.procedure(lookup.ins = TRUE)
  ddg.return(difference)
}

get.check.value <- function(difference,tolerance) {
  # compare difference to tolerance
  check <- difference - tolerance
  ddg.procedure(lookup.ins = TRUE)
  ddg.return(check)
}

store.result <- function(number,estimate) {
  sqr.root <- data.frame(number,estimate)
  ddg.procedure(lookup.ins = TRUE)
  ddg.return(sqr.root)
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
get.initial.values()
  
ddg.eval("estimate <- get.random(number)")
check <- number

ddg.finish("Initial estimate")

ddg.start("Approximations")
i <- 0
while (check > 0) {
  # repeat calculation until tests OK
  i <- i + 1
  ddg.start(paste("Iteration",i))
  ddg.eval("estimate <- calc.square.root(number,estimate)")
  ddg.eval("difference <- get.difference(number,estimate)")
  ddg.eval("check <- get.check.value(difference,tolerance)")
  ddg.finish(paste("Iteration", i))
}
ddg.finish("Approximations")

ddg.start("Export to file")
ddg.eval("sqr.root <- store.result(number,estimate)")
write.result(sqr.root)
ddg.finish("Export to file")

### Display values

sqr.root

ddg.finish("Calculate Square Root")

