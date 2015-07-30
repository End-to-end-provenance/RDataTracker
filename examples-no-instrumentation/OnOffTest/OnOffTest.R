# Test the ability to dynamically turn the console on and off
#
# Author: Luis Perez
###############################################################################


# Turn off console for definitions
ddg.console.off()

f <- function() {
   return(10)
}

g <- function(a) {
    c <- a + 10
    return(c)
}

# turn on for a single definition
ddg.console.on()
h <- function() {
   d <- 333
   return(d)
}
ddg.console.off()

i <- function() {
   return(1000)
}

# Turn on the console to capture some information
ddg.console.on()
a <- 1
b <- a + 1

f_val <- f()

c <- 100
ddg.console.off()


d <- g(c)
h_val <- h()

ddg.console.on()

x <- 5
i_val <- i()

# Turn off for the rest of the script
ddg.console.off()

foobar <- read.csv("foobar.csv")
