# URL input
file.in <- url ("http://www.mtholyoke.edu/index.html")
df <- readLines(file.in, warn=FALSE)
close (file.in)

# File output
file.out <- file("test.dat", "w")
writeLines(df, file.out)
close (file.out)

# Pipe input - this may fail on Windows machines without UNIX shell
#file.in <- pipe ("cat ../foo.txt")
#df <- readLines(file.in)
#close (file.in)
#print(df)

# File within a zip file
file.in <- unz ("../ab.zip", "a.txt")
df <- readLines (file.in)
close(file.in)
print(df)

# Using the same file for both writing and reading
x <- 1
write(x, "x.csv")
y <- read.table("x.csv")
