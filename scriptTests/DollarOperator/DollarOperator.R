# Using $ with environments.  The variable in the environment should be linked to.
myEnv <- new.env()
myEnv$var <- 1
myVar <- myEnv$var

# Using $ with data frames.  The data frame should be linked to.
a <- (1:10)
b <- (11:20)
df <- data.frame (a, b)
df$c <- (21:30)  # Output node should be df

# Checking use of $ within a function.
f <- function() {
  plot (df$a, df$b)
  df$d <- (31:40)
}
f()   # This should just use f and df (and should not output df)
