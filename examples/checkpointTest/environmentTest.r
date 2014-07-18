# Remove all objects
rm (list=ls())

n <- 1

# Save the environment with n bound to 1
env1 <- environment()
print(paste("n =", get("n",env1)))

f <- function() {print(n)}

print("Should print 1.")
f()

n <- 2
print(paste("n =", get("n",env1)))

print("Should print 2.")
f()

# Restore the environment in which n is bound to 1
# ACK!!! It prints 2.
environment(f) <- env1
print("Should print 1.")
f()
