# Test R script
# ERB rev. 3-Apr-2014

# Modified by Luis Perez 7-Jul-2014
# Modified by Luis Perez 17-Jul-2014

increment.value <- function(a) {
	a <- a + 1
}

a <- 0
# n <- 100 Value reduced for automated function annotations
n <- 10
	
ddg.start("iteration i")
for (i in 1:n) {
	ddg.start("iteration j")    
	for (j in 1:n) {
		ddg.start("iteration k")
		for (k in 1:n) {
			a <- increment.value(a)
			invisible(a)
		}
		ddg.finish("iteration k")         
	}
	ddg.finish("iteration j") 
}
ddg.finish("iteration i")


