# Commented out for now because there is an expired certiricate (June 2, 2020)
#file.in <- url("http://harvardforest.fas.harvard.edu/data/p00/hf000/hf000-01-daily-m.csv")
#df <- read.csv(file.in)
#file.out <- file("test.csv", "w+")
#write.csv(df, file.out)
#close(file.out)

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

file.in <- unz("../foo.zip", "foo.txt")
unzipped <- readLines(file.in)
file2.out <- file("foo_copy.txt", "w+")
writeLines(unzipped, file2.out)
close(file.out)

writeLines ("foobar", "foobar.txt")

# Commenting out this test for now.  We use capture.output to 
# save standard output, but closeAllConnections closes the 
# connection that capture.output uses, leading to an error
# when capture.output later tries to close it.  This was
# reported to R as a bug on Aug. 7, 2019:
#> capture.output(closeAllConnections())
#Error in close.connection(file) : invalid connection
#In addition: Warning message:
#    In sink(type = type, split = split) : no sink to remove
#> traceback()
#3: close.connection(file)
#2: close(file)
#1: capture.output(closeAllConnections())
#
closeAllConnections()

# Note that we will create a file node at the end of the script
# for this, but since the connection has not been closed, the
# last write might not have been flushed to the file, so the copied
# file may be empty.
file3.out <- file ("asdf.txt", "w+")
writeLines ("asdf", file3.out)
