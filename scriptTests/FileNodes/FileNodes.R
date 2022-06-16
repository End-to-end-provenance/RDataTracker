# URL input -- changed to a page that won't change unexpectedly causing our tests to fail
file.in <- url ("https://www.barbara-lerner.com/test.html")
df <- readLines(file.in, warn=FALSE)
close (file.in)

# File output
file.out <- file("test.dat", "w")
writeLines(df, file.out)
close (file.out)

# Pipe input - this may fail on Windows machines without UNIX shell
file.in <- pipe ("echo foo")
df <- readLines(file.in)
close (file.in)
print(df)

# File within a zip file
file.in <- unz ("../ab.zip", "a.txt")
df <- readLines (file.in)
close(file.in)
print(df)

# Using the same file for both writing and reading
file <- "x.csv"

x <- 1
write(x, file)
y <- read.table(file)

# also tests referencing the same file but given the full path instead
z <- read.table(normalizePath(file, winslash="/", mustWork=FALSE))
stopifnot(identical(y,z))

# This one shold not create a file node since it is reading from text.
table="1,2\n3,4"
read.table(text=table)

# Test that vroom functions get traced.
data (mtcars)
vroom::vroom_write(mtcars, "./mtcars.dat")
mtcars <- vroom::vroom("./mtcars.dat")
library(vroom)
vroom_write(mtcars, "./mtcars.dat")
mtcars <- vroom("./mtcars.dat")

#raster::raster ("../land_water.tif")
