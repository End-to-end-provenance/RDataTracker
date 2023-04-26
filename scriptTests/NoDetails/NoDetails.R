# normal function call
# this section should be omitted from the ddg!
f1 <- function(n)
{
	if( n == 0 )
		return(5)
	else
		return(n+1)
}

x <- f1(0)

# a plot to a pdf file (modified from Plot1)
data (mtcars)
allCars.df <- mtcars

cars4Cyl.df <- allCars.df[allCars.df$cyl == 4, ]
cars6Cyl.df <- allCars.df[allCars.df$cyl == 6, ]
cars8Cyl.df <- allCars.df[allCars.df$cyl == 8, ]

cylinders = c(4, 6, 8)
mpg = c(mean(cars4Cyl.df$mpg), mean(cars6Cyl.df$mpg), mean(cars8Cyl.df$mpg))

pdf ("plot.pdf")
plot(cylinders, mpg)
title("RStudio display")
dev.off()

# an error
# Note that as of R 4.3.0, the error message is different when run in 
# prov.run and when run normally.  I believe this is a bug in 4.3.0
# that I have reported.  See comment at the end of FunctionOrigin.R for details.
as.roman(z)
