# Tests for various control constructs.

# Created by Barbara Lerner 27 June 2016.
# Modified by Emery Boose 3 Nov 2016.

if (TRUE) {
  n <- 1
}

m <- 2

{
  a <- 1
  b <- 2
}

if (b == 1) {
  cc <- 3
} else {
  d <- 4
}

repeat {
  d <- d + 1
  if (d == 7) {
    break
  }
}

while (d < 10) {
  d <- d + 1
}

for (e in 1:5) {
  f <- e + 1
}

if (a == 1) x <- 1

if (a == 1) {
  x <- 2
}

if (a == 0) {
  x <- 3
} else {
  x <- 4  
}

if (a == 0) {
  x <- 5
} else if (a == 1) {
  x <- 6
}

if (a == 2) {
  y <- 1
  z <- 1
} else if (a == 3) {
  y <- 2
  z <- 2
} else {
  y <- 3
  z <- 3
}
  
for (i in 1:3) {
  if (i == 2) next
  a <- a + i
}

for (i in 1:3) {
  if (i == 2) break
  b <- b + i
}

f <- function (a) {
  if (a < 0) {
    return (-1)
  }
  return (1)
}

f(-2)
f(2)

q <- 1

repeat {
  q <- q + 1
  if (TRUE) r <- q^2
  if (FALSE) s <- q^3
  if (q > 4) break
}

f <- function(x, y) {if (b == 1) x else y}
f(1, 2)

