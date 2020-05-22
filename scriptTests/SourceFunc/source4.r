# This script does not assume the existance of any variables and is supposed to 
# execute locally using ddg.source. It also uses ddg.return and ddg.procedure.
x <- 5
f1 <- function(w){
  return(w+1)
}

# just random testing
y <- 4
z <- f1(x) + y

# parameter with the same name??
w <- f1(x)

