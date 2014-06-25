rm (list=ls())

ddg.library <- Sys.getenv("DDG_LIBRARY")
if (ddg.library == "") {
	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
}
source(ddg.library)

f <- function(x) {
	return(1)
}

f2 <- function(x) {
	ddg.procedure("f2")
	ddg.data.in(deparse(substitute(x)), "f2")
	return(1)
}

f3 <- function() {
	stop("f3 stopped execution")
}

main <- function() {
	ddg.start("main")
	
	a <- 3
	f(a)
	ddg.data(a)
	f2(a)
	
	b <- 1/0
	f(b)
	ddg.data(b)
	f2(b)
	
	f(c)
	ddg.data(c)
	f2(c)
	
	d <- 6
	f(d[[2]])
	ddg.data(d[[2]])
	f2(d[[2]])
	
	f(f3)
	ddg.data(f3)
	f2(f3)
	
	ddg.data("no.such.var", no.such.var)
	
	ddg.finish("main")
}

### Run script

ddg.run(main, 
		"/Users/barbaralerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/ddgChangesBehavior/ddgChangesBehavior.r",
		"/Users/barbaralerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/ddgChangesBehavior/ddg")

