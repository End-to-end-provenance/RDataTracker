library(ggplot2)
# data (mtcars)
# 
# # All the cars
# allCars.df <- mtcars
# 
# # Create separate data frames for each number of cylinders
# cars4Cyl.df <- allCars.df[allCars.df$cyl == 4, ]
# cars6Cyl.df <- allCars.df[allCars.df$cyl == 6, ]
# cars8Cyl.df <- allCars.df[allCars.df$cyl == 8, ]
# 
# # Create a table with the average mpg for each # cylinders
# cylinders = c(4, 6, 8)
# mpg = c(mean(cars4Cyl.df$mpg), mean(cars6Cyl.df$mpg), mean(cars8Cyl.df$mpg))
# 
# # Plot it to the default device
# df <- data.frame (cylinders, mpg)

xVals <- 1:10
yVals <- 10:1
df <- data.frame(xVals, yVals)

# ggsave when the plot is specified explicitly
print (paste ("Before ggplot call, Rplots.pdf exists?", file.exists("Rplots.pdf"))) 
p1 <- ggplot(df, aes(xVals, yVals)) + geom_point() + labs(title="ggsave with explicit plot")
print (paste ("Before ggsave call, Rplots.pdf exists?", file.exists("Rplots.pdf"))) 
ggsave ("plot1.pdf", p1)
print (paste ("After plot1, Rplots.pdf exists?", file.exists("Rplots.pdf"))) 

# ggsave when the last plot created is saved implicitly
p2 <- ggplot(df, aes(xVals, yVals)) + geom_point() + labs(title="ggsave with implicit last plot")
ggsave ("plot2.pdf")
print (paste ("After plot2, Rplots.pdf exists?", file.exists("Rplots.pdf"))) 

# plot created by ggplot not saved to a variable
ggplot(df, aes(xVals, yVals)) + geom_point() + labs(title="ggsave with implicit last plot and not plot variable")
ggsave ("plot3.pdf")
print (paste ("After plot3, Rplots.pdf exists?", file.exists("Rplots.pdf"))) 

# plot built up over several statements
p4a <- ggplot(df, aes(xVals, yVals))
p4b <- p4a + geom_point()
p4c <- p4b + labs(title="plot built up over multiple statements")
ggsave ("plot4.pdf", p4c)
print (paste ("After plot4, Rplots.pdf exists?", file.exists("Rplots.pdf"))) 

# plot built up over several statements and implicit to ggsave
p5a <- ggplot(df, aes(xVals, yVals))
p5b <- p5a + geom_point()
p5c <- p5b + labs(title="plot built up over multiple statements")
ggsave ("plot5.pdf")
print (paste ("After plot5, Rplots.pdf exists?", file.exists("Rplots.pdf"))) 



