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

ddg.library <- Sys.getenv("DDG_LIBRARY")
if (ddg.library == "") {
	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
}
source(ddg.library)

### Functions

set.global.var <- function() {
	ddg.procedure("set.global.var")
	
  # set global variables
  x <- 10
  sqr.root <<- c(0)
  ddg.data.out("set.global.var", "sqr.root", sqr.root)
  tolerance <<- 0.00001
  ddg.data.out("set.global.var", "tolerance", tolerance)
  count <<- 1
  ddg.data.out("set.global.var", "count", count)
  difference <<- x
  ddg.data.out("set.global.var", "difference", difference)
  
  return(x)
}

get.random <- function(z) {
	ddg.procedure("get.random")
	
	# get random seed value
  x <- runif(1,1,z)

  return(x)
}

calc.square.root <- function(z,x) {
	ddg.procedure("calc.square.root")
	
  # calculate square root
  y <- z/x
  x <- (x+y)/2

  return(x)
}

get.difference <- function(z,x) {
	ddg.procedure("get.difference")
	
  # test result
  d <- abs(z-x^2)

  return(d)
}

### Main Program
print(c("ddg.text=", ddg.text))
ddg.start("main")
number <- set.global.var()
ddg.data.out("set.global.var", "number", number)

# The main program uses globals should there be nodes to represent different
# parts of the main program with the corresponding data flow edges????

sqr.root[1] <- get.random(number)
ddg.data.in("get.random", "number")
ddg.data.out("get.random", "sqr.root", sqr.root)

# Did not include these loop start/finish in DailySolarRadiation.
# Do we like this?
ddg.start("while")
while (difference > tolerance) {
	ddg.start(paste("iteration", count, sep=""))
	
  # repeat calculation until tests OK
  count <- count + 1
  sqr.root[count] <- calc.square.root(number,sqr.root[count-1])
  ddg.data.in("calc.square.root", "number")
  # Interesting.  Only one element of sqr.root is passed in.  
  # Should calc.square.root also show a dependence on count???
	ddg.data.in("calc.square.root", "sqr.root")
	ddg.data.out("calc.square.root", "sqr.root", sqr.root[count])
	
	difference <- get.difference(number,sqr.root[count])
  ddg.data.in("get.difference", "number")
  ddg.data.in("get.difference", "sqr.root")
  ddg.data.out("get.difference", "difference", difference)
  ddg.finish(paste("iteration", count-1, sep=""))
}
ddg.finish("while")

### Print Values

sqr.root
sqr.root^2
ddg.finish("main")
print(c("ddg.text=", ddg.text))
ddg.save("calculate-square-root-barb-instrumentation.r")
