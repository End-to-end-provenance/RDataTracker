f <- function(x) {
  g(x)
  h(x)
  return(1)
}

g <- function(x) {
#  ddg.function()
#  return(ddg.return.value(1))
  return(1)
}

h <- function(x) {
#  ddg.function()
#  return(ddg.return.value(1))
  return(1)
}

someVector <- function() {
  return(c(1, 3, 5))
}

### Run script

x <- 10

f(x)
f(x)

