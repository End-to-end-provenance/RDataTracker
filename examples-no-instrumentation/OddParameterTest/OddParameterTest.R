### Originally written by Barbara Lerner
# Modified by Luis Perez 7-Jul-2014
# Modified by Luis Perez 17-Jul-2014

f <- function(x) {
	return(1)
}

f3 <- function() {
	stop("f3 stopped execution")
}

a <- 3
f(a)
	
# b is infinity
b <- 1/0
f(b)
	
# d doesn't exist yet
f(d)
d <- 1
f(d)
	
d <- 6
f(d[[2]])
	
# Passing a function as a parameter
f(f3)
	
# Passing an environment as a parameter
f (.GlobalEnv)
