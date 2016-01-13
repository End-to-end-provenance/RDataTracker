# TODO: Add comment
# 
# Author: blerner
###############################################################################


#library(RDataTracker)

options(warn=1)

# We would like f and f2 to produce the same DDG but currently
# we do not analyze the code within a function to do this.
# Issue 67 on github
f <- function() {
   a <<- b * 10
}

f2 <- function() {
  a <<- b * 10
  #ddg.function(outs.data=list("a"))
  #ddg.return.value(a)
  return (a)
}

g <- function(a) {
    c <- a + 10
    d <- 1000
}

h <- function() {
   d <- 333
}

i <- function() {
   x <<- 1000
   return(j(x))
}

j <- function(xx) {
   return(3)
}

k <- function (xx = 0, yy = 1) {
  return (xx + yy)
}

a <- 1
b <- a + 1

f()
f2()

c <- 100
if (g(c) != 1000) print("g(c) returned the wrong value")

d <- g(c)

h()

i()

# Calling k with no parameters produces a ddg where the function definition
# has a dataflow edge to the function call.  If we call the function with
# parameters, this edge does not exist.
# Issue 68 on github.
k(10)
k(a, b)
k(a)
k(yy = b)
k()
k(b+1)
k(a+b+1)

