#############################
### CALCULATE SQUARE ROOT ###
#############################

# Calculate square root iteratively
# Record DDG in text format
# ERB rev. 22-Nov-2013

### Functions

get.initial.values <- function() {
  number <<- 10
  tolerance <<- 0.00001
}

get.random <- function(x) {
  # get random seed value
  # z <- runif(1,1,x)

  # use fixed value for regression test
  z <- 3

  return(z)
}

calc.square.root <- function(n,x) {
  # calculate square root
  y <- n/x
  z <- (x+y)/2

  return(z)
}

get.difference <- function(n,x) {
  # test result
  z <- abs(n-x^2)

  return(z)
}

get.check.value <- function(d,t) {
  #compare difference to tolerance
  z <- d - t
    
  return(z)
}

store.result <- function(n,x) {
  num <- n
  sqrt <- x  
  zz <- data.frame(num,sqrt)

  return(zz)
}

write.result <- function(fn,zz) {
  file.out <- paste(getwd(),"/",fn,sep="")
  write.csv(zz,file.out,row.names=FALSE)
}

### Main Program

get.initial.values()
estimate <- get.random(number)
	
check <- number
	
while (check > 0) {
	  # repeat calculation until tests OK
      estimate <- calc.square.root(number,estimate)
      difference <- get.difference(number,estimate)
      check <- get.check.value(difference,tolerance)
}
	
sqr.root <- store.result(number,estimate)
write.result("sqr-root.csv",sqr.root)
	


