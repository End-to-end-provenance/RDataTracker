#############################
### CALCULATE SQUARE ROOT ###
#############################

# Calculate square root iteratively
# Record DDG in text format
# ERB rev. 7-July-2013

# Notes to run this.  
# 1. The DDG_LIBRARY environment variable should be set to the location
#    of the ddg-library.r file.  Can change with Sys.setenv(DDG_LIBRARY = "location of ddg-library.r")

### DDG library

#ddg.library <- Sys.getenv("DDG_LIBRARY")
#if (ddg.library == "") {
#	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
#}
#source(ddg.library)

### Functions

set.global.var <- function() {
  # set global variables
  x <- 10
  sqr.root <<- c(0)
  tolerance <<- 0.00001
  count <<- 1
  difference <<- x

  return(x)
}

get.random <- function(z) {
  # get random seed value
  x <- runif(1,1,z)

  return(x)
}

calc.square.root <- function(z,x) {
  # calculate square root
  y <- z/x
  x <- (x+y)/2

  return(x)
}

get.difference <- function(z,x) {
  # test result
  d <- abs(z-x^2)

  return(d)
}

### Main Program

number <- set.global.var()

sqr.root[1] <- get.random(number)

while (difference > tolerance) {
  # repeat calculation until tests OK
  count <- count + 1
  sqr.root[count] <- calc.square.root(number,sqr.root[count-1])
  difference <- get.difference(number,sqr.root[count])
}

### Print Values

sqr.root
sqr.root^2

