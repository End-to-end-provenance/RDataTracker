# Test R script
# ERB rev. 3-Apr-2014

### Directories

library(RDataTracker)
options(warn=2)

increment.value <- function(a) {
	a <- a + 1
	
	ddg.procedure(ins=list("a"),outs.data=list("a"))
	
	return(a)  
}

main <- function() {
	a <- 0
	n <- 20
	
	ddg.data(a)
	
	ddg.start("iteration i")
	for (i in 1:n) {
		ddg.start("iteration j")    
		for (j in 1:n) {
			ddg.start("iteration k")
			for (k in 1:n) {
				a <- increment.value(a)
				print(a)
			}
			ddg.finish("iteration k")         
		}
		ddg.finish("iteration j") 
	}
	ddg.finish("iteration i")
}

ddg.r.script.path <- "/Users/blerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/DDGHangBug/DDGHangBug.R"
ddg.path <- "/Users/blerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/DDGHangBug/ddg"
ddg.run(main,ddg.r.script.path,ddg.path)