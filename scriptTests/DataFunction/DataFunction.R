# Loading one data set
data (mtcars)
cars.df <- mtcars

# Loading multiple data sets
data(diamonds, economics)
diamonds.df <- diamonds
economics.df <- economics

# Used to list data sets
data()

# Loading a local data set
f2 <- function() {
  data(pets)
  pets.df <- pets
}
f2()
