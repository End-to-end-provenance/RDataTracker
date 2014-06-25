rm (list=ls())

ddg.library <- Sys.getenv("DDG_LIBRARY")
if (ddg.library == "") {
	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
}
ddg.r.script.path = "/Users/barbaralerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/environmentTest/CheckpointTest.r"
ddg.path = "/Users/barbaralerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/environmentTest/ddg"
source(ddg.library)

source("checkpoint.r")

f <- function() {
	ddg.procedure()
	ddg.data.in("f", "n")
	n
}

before <- function() {
	ddg.start("before")
	n <<- 1
	ddg.data("n", n)

	ddg.procedure("n <- 2")
	ddg.data.in("n <- 2", "n")
	n <<- 2
	ddg.data.out("n <- 2", "n", n)
	print (paste("At end of before, n =", n))

	ddg.finish("before")
}

after <- function() {
	ddg.start("after")
	print (paste("At beginning of after, n =", f()))
	
	ddg.procedure("n <- 3")
	ddg.data.in("n <- 3", "n")
	n <<- 3
	ddg.data.out("n <- 3", "n", n)
	
	f()
	
	ddg.finish("after")
}

ddg.run(before)
#ddg.run(after)

