# Loading one data set
data (mtcars)
cars.df <- mtcars

# Loading multiple data sets
data(airmiles, AirPassengers)
airmiles.df <- airmiles
airPassengers.df <- AirPassengers

# Used to list data sets
data()

# Loading a local data set
f2 <- function() {
  data(trees)
  trees.df <- trees
}
f2()
