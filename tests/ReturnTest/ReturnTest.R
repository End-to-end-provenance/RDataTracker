f <- function (aa, bb) {
  retValue <- f2(aa) + f2(bb)
  return(retValue)
}

f2 <- function(p_a) {
   return(10)
}

f3 <- function(aa, bb) {
   return (aa + bb)
}

f4 <- function(n) {
  if (n == 0) {
    return(0)
  }
  else {
    return(f4(n-1)+1)
  }
}

f6 <- function(s1, s2, s3, s4) {
  return(3)
}

f7 <- function(n) {
  if (n == 0) {
    return(0)
  }
  else {
    retValue <- f7(n-1)
    return(retValue+1)
  }
}

f8 <- function (aa, bb) {
  retValue <- f9(aa) + f9(bb)
  return(retValue)
}

f9 <- function(p_a) {
  retValue <- f10(p_a)
  return(retValue)
}

f10 <- function(x) {
  return(x + 1)
}

a <- 1
b <- 2

# Testing a simple call and return
i <- f2(a)

# Testing a return statement that contains an expression
e <- f3(a, b)

# Testing a function calling a function
d <- f(a, b)
stopifnot(d == 20)

# Testing a function calling a function calling a function
i <- f8(a, b)


# Testing 2 calls to the same function in one statement
g <- f2(a) + f2(b)

# Tests different ways that parameters can get bound.
abc <- "abc"
x <- 0
h <- f6(abc, 5, "a b", x + 1)

# Tests recursion
x <- f4(3)
x <- f7(3)

# Tests returning NULL when no branch of a final
# if is executed
f11 <- function(x) {
  y <- 1
  if (x > 0) return (10)
}

f11(-1)

# Testing for loop as the last line of a function.
f12 <- function (x) {
  sum <- 0
  for (i in 1:x) {
    sum <- sum + i
  }
}
 
f12(3)
