# This script does not assume the existance of any variables and is supposed to 
# execute locally using ddg.source. It also uses ddg.return and ddg.procedure.
x <- 5
f <- function(w){
  return(w+1)
}

# just random testing
y <- 4
z <- f(x) + y

# parameter with the same name??
w <- f(x)

