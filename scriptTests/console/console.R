# This test is different from other tests as it should be run
# manually to test out the console features.  The user should 
# select the contents of this file in RStudio and click Run.
# 
# You can check if the resulting ddg is correct by running:
# ant -file tests.xml console
#
# You should examine console output in RStudio manually for errors.

prov.init(prov.dir=".")
a <- 1
b <- a + 2
a <- 5
prov.save()
if (a == 1) {
  b = 10
} else if (a == 5) {
  d = 50
}
prov.save()
f <- function () {
  return (1)
}

x <- f()
prov.quit()

