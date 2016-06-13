# Tests that higher-order functions produce correct ddgs
#
# Author:  Barbara Lerner  June 3, 2016

d <- c(1, 2, 3, 4)

inc <- function (num) {
  num + 1
}

e <- sapply (d, inc)
print(e)

is.even <- function(num) {
  num %% 2 == 0
}

evens <- Filter (is.even, d)
print (evens)

add <- function (x, y) {
  x + y
}

sum <- Reduce(add, d, 0)
print (sum)
