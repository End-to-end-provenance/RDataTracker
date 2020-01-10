# This test tests the saving of variables that exist in GlobalEnv before
# the script executes. This runs like console test.
# @author Elizabeth Fong
# @version Dec 2019

x <- matrix(1:100, 5)

# this is an unused variable
unused.var <- 50

prov.init(prov.dir = ".", snapshot.size = 10)
a <- rep(x,5)
prov.quit()
