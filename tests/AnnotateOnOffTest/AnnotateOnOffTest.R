ddg.annotate.on("f")
ddg.annotate.off("g")

f <- function() {
  x <- 1
  return(x)
}

y <- f()
f()

ddg.annotate.off("f")
f()

g <- function() {
  x <- 2
  return(x)
}

g()

ddg.annotate.on(c("f","g"))
f()
g()
