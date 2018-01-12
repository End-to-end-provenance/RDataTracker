file.in <- url("http://harvardforest.fas.harvard.edu/data/p00/hf000/hf000-01-daily-m.csv")
df <- read.csv(file.in)
file.out <- file("test.csv", "w+")
write.csv(df, file.out)
close(file.out)

# Note that there is something wrong with the protocol here.  I get a Bad Request page
# but that is ok.  The point is that we can read from a socket and get the right ddg.
socket <- socketConnection("harvardforest.fas.harvard.edu", port=80, blocking=TRUE)
writeLines("GET / HTTP/1.1", socket)
writeLines("Host: harvardforest.fas.harvard.edu", socket)
writeLines("", socket)
homepage <- readLines(socket)
file.out <- file("home.html", "w+")
writeLines(homepage, file.out)
#close(file.out)

file.in <- unz("foo.zip", "foo.txt")
unzipped <- readLines(file.in)
file.out <- file("foo_copy.txt", "w+")
writeLines(unzipped, file.out)
#close(file.out)

writeLines ("foobar", "foobar.txt")

closeAllConnections()

# Note that we will create a file node at the end of the script
# for this, but since the connection has not been closed, the
# last write might not have been flushed to the file, so the copied
# file may be empty.
file.out <- file ("asdf.txt", "w+")
writeLines ("asdf", file.out)

