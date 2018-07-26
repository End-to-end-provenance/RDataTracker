# This test is different from other tests as it should be run
# manually to test out the console features.  The user should 
# select the contents of this file in RStudio and click Run.

ddg.init(ddgdir=".")
a <- 1
b <- a + 2
a <- 5
ddg.save()
if (a == 1) {
  b = 10
} else if (a == 5) {
  d = 50
}
ddg.save()
f <- function () {
  return (1)
}

x <- f()
ddg.quit()
