# This test was added to test that non-local variables in functions
# get linked into the data flow.  This test case is particularly useful
# when not collecting provenance inside functions.

# Since d is a parameter we should not see an output
# data edges when d is called.  There will be an input
# data edge to bind the parameter.
f1 <- function (d) {
  d <- d + 1
}
d <- 1
f1 (d)
print (paste ("d =", d, "(expecting 1)"))

# Since e is first assigned as a local we should not see any input or output
# data edges when e is called.
f2 <- function () {
  e <- 1
  e <- e + 1
}
e <- 1
f2 ()
print (paste ("e =", e, "(expecting 1)"))

# g is set as a non-local, so we should see an output data edge.
# There is no input data edge since the value used for g in the
# second statement is the one set in the first statement, not
# the value from before the function is called.
f3 <- function () {
  g <<- 1
  g <- g + 1
}
g <- 1
f3 ()
print (paste ("g =", g, "(expecting 1)"))

# We should see an input edge for h but no output edge since
# h is set as a local.
f4 <- function () {
  h <- h + 1
}
h <- 1
f4 ()
print (paste ("h =", h, "(expecting 1)"))

# We should see an output edge for i with a value of 2 but
# no input edge
f5 <- function () {
  i <<- 1
  i <<- i + 1
}
i <- 1
f5 ()
print (paste ("i =", i, "(expecting 2)"))

# We should see both an input and output edge for j
f6 <- function () {
  j <<- j + 1
}
j <- 1
f6 ()
print (paste ("j =", j, "(expecting 2)"))

# We should see an input edge for k but no output edge.
f7 <- function () {
  k = k + 1
}
k <- 1
f7 ()
print (paste ("k =", k, "(expecting 1)"))

# We will see an input edge for l in this case because
# the if statement adds uncertainty about l being local.
f8 <- function () {
  if (TRUE) l <- 2
  l <- l + 1
}
l <- 1
f8 ()
print (paste ("l =", l, "(expecting 1)"))

# There should be no input edge in this case even though
# the if-statement makes things uncertain again.  However,
# since there is no non-local named m to link to, the 
# edge will not be created.
f9 <- function () {
  if (TRUE) m <- 2
  m <- m + 1
}
f9 ()
