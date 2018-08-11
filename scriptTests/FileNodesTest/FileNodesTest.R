# URL input
file.in <- url ("http://www.mtholyoke.edu/index.html")
df <- readLines(file.in, warn=FALSE)
close (file.in)

# File output
file.out <- file("test.dat", "w")
writeLines(df, file.out)
close (file.out)

# Pipe input
file.in <- pipe ("cat foo.txt")
df <- readLines(file.in)
close (file.in)
print(df)

# File within a zip file
file.in <- unz ("ab.zip", "a.txt")
df <- readLines (file.in)
close(file.in)
print(df)

