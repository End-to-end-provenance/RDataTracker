# TODO: Add comment
# 
# Author: blerner
###############################################################################

a <- 1

if(TRUE) {
  a <- 1
}
 
ddddd <- 0
repeat {  
  ddddd <- ddddd + 1
  if (ddddd == 7) { 
    break
  } 
}

f <- function (a) {
  return(a + 1)
}

g <- function() 1

h <- function (a, b) {
  if (a > b) return (1)
  
  sum <- a + b
  product <- a * b
  source("foo.R")
  write.csv("foo.csv")
  pdf("foo.pdf")
  dev.off()
  return (sum)
}

.ddg.start("foo")

exprs <- parse("DDGStatementTest.R")

constructDDGStatement <- function (expr) {
  print(expr)
  print(typeof(expr))
  return (new (Class = "DDGStatement", parsed = expr))
}

.ddg.finish("foo")

