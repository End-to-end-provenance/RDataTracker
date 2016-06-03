# Tests that higher-order functions produce correct ddgs
#
# Author:  Barbara Lerner  June 3, 2016

d <- c(1, 2)

inc <- function (num) {
  num + 1
}

e <- sapply (d, inc)
print(e)

