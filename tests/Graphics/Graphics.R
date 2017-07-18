# Test creating and closing a graphics device
# Also testing dev.off as last line of function
f1 <- function() {
  nums <- c(1, 3, 6, 4, 9)
  pdf("nums.pdf")
  plot(nums)
  dev.off()
}

f1()

# Test closing a graphics device that was not opened.
# Also testing dev.off not as last line of function.
f2 <- function() {
  nums <- c(1, 2, 4, 6, 8, 10)
  plot(nums)
  dev.off()
  return (1)
}

f2()

# Test graphic output where the device is neither opened
# nor explicitly closed.
f3 <- function() {
  nums <- c(10, 9, 8, 7)
  plot(nums)
}

f3()



