# Tests of ddg functions.  No useful computation here.
# This test should not produce any errors and does not use 
# files or snapshots so that ddg text files can be easily 
# compared to be sure everything is working as expected.

# Modified by Luis Perez 7-Jul-2014
# Modified by Luis PErez 17-Jul-2014

## Directories

#source("/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/R/RDataTracker.R")
library(RDataTracker)
#options(warn=2)

# get initial time
startTime <- Sys.time()
invisible(force(startTime))

testDir <- "[DIR_DEFAULT]/"
setwd(testDir)

ddg.r.script.path = paste(testDir,"basicTest.r",sep="")
ddg.path = paste(testDir,"[DDG-DIR]",sep="")

### Functions
no.name.or.args.given <- function (a, b, c, d, e) {
	ddg.procedure()
}

lookup.args <- function (a, b, c, d, e) {
	ddg.procedure(lookup.ins = TRUE)
}

only.args.given <- function (a, b, yy, d, e, f) {
	ddg.procedure(ins=list("w", "x", "y", "z"))
}

only.name.given <- function (a, b, c, d, e, f) {
	ddg.procedure(only.name.given)
}

string.name.and.args.given <- function (a, b, c, d, e, f) {
	ddg.procedure("string.name.and.args.given", list("w", "x", "y", "z"))
}

data.in.test <- function(arg1) {
	ddg.procedure("data.in.test")
	ddg.data.in(dname="x")
}

out.test.1 <- function() {
	ddg.procedure()
	ddg.data.out(dname="out3", dvalue="c")
}

url.out.test <- function() {
	ddg.procedure()
	ddg.url.out(dname="MHC home", dvalue="https://www.mtholyoke.edu/")
}

exc.test. <- function() {
	ddg.procedure()
	ddg.exception.out(dname="exc3", dvalue="c")
}

start.finish.test <- function() {
	ddg.start()
	ddg.finish()
}

main <- function() {
	ddg.start("main")
	# Test ddg.data
	x <- 1+2
	ddg.data("x", x)  # String name, explicit value
	y <- paste("a", "b", "c")
	ddg.data(y)  # Name as name, no value
	z <- x + 2
	ddg.data ("z")  # String name, no value
  
  	w <- x + 3
	ddg.data(w, w)  # Name as name, explicit value
	
	# Test ddg.procedure
	no.name.or.args.given(w, x, y, z, x + 1)
	lookup.args(w, x, y, z, x + 1)
	only.args.given(w, x, y, z,  x + 1, vector())
	only.name.given(w, x, y, z,  x + 1, vector())
	string.name.and.args.given(w, x, y, z,  x + 1, vector())
	ddg.procedure("no.func")  # For the case where the name does not correspond to a function.  Must be a string in this case
	simple.value <- 10
	ddg.procedure("g0", outs.data=list("simple.value"))
	year <- c(1992, 1995)
	name <- c("Ben", "Greg")
	male <- c(TRUE, TRUE) 
	kids.df <- data.frame(year, name, male)
	ddg.procedure("g1", outs.data=list("year"))
	
	# Test ddg.url
	ddg.url("HF home page", "http://harvardforest.fas.harvard.edu/")
	
	# Test ddg.exception
	ddg.exception("error", "test error")
	error2 <- "Trivial error"
	ddg.exception(error2)
	
	# Test ddg.data.in
	ddg.procedure("f")
	ddg.data.in("HF home page", "f")
	ddg.data.in("error", "f")
	ddg.data.in(error2, "f")
	data.in.test(x)
	
	# Test ddg.data.out
	ddg.data.out("out1", "a","no.name.or.args.given")
	ddg.data.out("out2", "b", no.name.or.args.given)
	out.test.1()
	out4 <- "d"
	ddg.data.out("out4", pname="no.name.or.args.given")
	out5 <- "e"
	ddg.data.out(out5, pname="no.name.or.args.given")
	
	# Test ddg.url.out
	ddg.url.out("Harvard home", "http://www.harvard.edu", "only.args.given")
	ddg.url.out("R home", "http://www.r-project.org/", only.args.given)
	url.out.test()
	
	# Test ddg.exception.out
	ddg.exception.out("exc1", "a", "no.name.or.args.given")
	ddg.exception.out("exc2", "b", no.name.or.args.given)
	exc.test.()
	exc4 <- "d"
	ddg.exception.out("exc4", pname="no.name.or.args.given")
	exc5 <- "e"
	ddg.exception.out(exc5, pname="no.name.or.args.given")

	# Test ddg.start and ddg.finish
	start.finish.test()
	
	ddg.finish("main")
}


### Run script

ddg.run(main, 
		ddg.r.script.path,
         ddg.path)

# Calculate total time of execution
endTime <- Sys.time()
cat("Execution Time =", difftime(endTime, startTime,units="secs"))
