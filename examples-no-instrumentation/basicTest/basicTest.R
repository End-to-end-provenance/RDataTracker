# Tests of ddg functions.  No useful computation here.
# This test should not produce any errors and does not use 
# files or snapshots so that ddg text files can be easily 
# compared to be sure everything is working as expected.

# Modified by Luis Perez 7-Jul-2014
# Modified by Luis PErez 17-Jul-2014

## Directories


### Functions
f <- function (a, b, yy, d, e, f) {
  return (a+1)
}



# Test basic assignments
x <- 1+2
y <- paste("a", "b", "c")
z <- x + 2
w <- x + 3

# Test saving structured data
year <- c(1992, 1995)
name <- c("Ben", "Greg")
male <- c(TRUE, TRUE) 
kids.df <- data.frame(year, name, male)

# Test NA and NULL as values
x<- NA
y <- NULL

# Test function call
val <- f(w, x, y, z,  x + 1, vector())

# Use a function call on left side of assignment
z <- 5
a <- "character"
storage.mode(z) <- a

# Test ddg.start and ddg.finish
ddg.start("File tests")
# Test files and URLs
data.df <- read.csv ("http://harvardforest.fas.harvard.edu/data/p00/hf000/hf000-01-daily-m.csv")
if (FALSE) read.csv ("foo.csv")
shortdata.df <- data.df[1:100, ]
write.csv (shortdata.df, "shortdata.csv")
pdf("airt-vs-prec.pdf")
plot (shortdata.df$airt, shortdata.df$prec)
dev.off()
ddg.finish("File tests")


# Test error
error <- 1 / 0

