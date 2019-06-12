#library(RDataTracker)

fun <- function(a,b){
  return(a+b)
}

x <- 6
y <- 10
z <- fun(x,y)

# source a script which actually used z and sets w in the global environment
source("../source1.r")

# use w and new z value
v <- fun(w,z)

# then script without them
source("../source3.r")

# a script which we call with a local environment
source("../source4.r")

# source which tets the local aspect of this thing!

# another script, we ignore everything but do it all on a new environment

# we use some of the variables set in source3.r
m <- 10
f(m)
f(x)

