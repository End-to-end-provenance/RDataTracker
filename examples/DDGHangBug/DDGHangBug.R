# Test R script
# ERB rev. 3-Apr-2014

# Modified by Luis Perez 7-Jul-2014
# Modified by Luis Perez 17-Jul-2014

## Directories
testDir <- "[DIR_DEFAULT]/"
setwd(testDir)

ddg.r.script.path = paste(testDir,"DDGHangBug.R",sep="")
ddg.path = paste(testDir,"ddg",sep="")

library(RDataTracker)

# get initial time
startTime <- Sys.time()
invisible(force(startTime))

options(warn=2)

increment.value <- function(a) {
	a <- a + 1
	
	ddg.function()
	ddg.return.value(a)  
}

main <- function() {
	a <- 0
	n <- 10
	
	ddg.data(a)
	
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
}

#ddg.r.script.path <- "D:/Users/Luis/Documents/Harvard School Work/Summer 2014/RDataTracker/examples/DDGHangBug/DDGHangBug.R"
#ddg.path <- "D:/Users/Luis/Documents/Harvard School Work/Summer 2014/RDataTracker/examples/DDGHangBug/ddg"
ddg.run(ddg.r.script.path,
        ddg.path, 
        main,
        enable.console=FALSE)

ddg.save(quit=TRUE)

# Calculate total time of execution
endTime <- Sys.time()
cat("Execution Time =", difftime(endTime, startTime,units="secs"))
