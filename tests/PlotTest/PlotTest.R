data (mtcars)

# All the cars
allCars.df <- mtcars

# Create separate data frames for each number of cylinders
cars4Cyl.df <- allCars.df[allCars.df$cyl == 4, ]
cars6Cyl.df <- allCars.df[allCars.df$cyl == 6, ]
cars8Cyl.df <- allCars.df[allCars.df$cyl == 8, ]

# Create a table with the average mpg for each # cylinders
cylinders = c(4, 6, 8)
mpg = c(mean(cars4Cyl.df$mpg), mean(cars6Cyl.df$mpg), mean(cars8Cyl.df$mpg))

# Plot it to the default device
plot(cylinders, mpg)

# Plot to X11
X11()
plot(cylinders, mpg)
dev.off()

# Plot to a file
pdf ("plot.pdf")
plot(cylinders, mpg)
#invisible(dev.off())
dev.off()

x <- 1



