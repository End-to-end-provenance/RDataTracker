#############################
### CALCULATE SQUARE ROOT ###
#############################

# Calculate square root iteratively
# Record DDG in text format
# ERB rev. 6-Feb-2014

### Directories

ddg.library <- Sys.getenv("DDG_LIBRARY")
if (ddg.library == "") {
	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
}
source(ddg.library)

ddg.init(		
		"/Users/barbaralerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/CalculateSquareRoot/calculate-square-root-minimal.r",
		"/Users/barbaralerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/CalculateSquareRoot/ddg",
		enable.console=TRUE)

number <- 10
tolerance <- 0.00001
estimate <- runif(1,1,number)
difference <- abs(number-estimate^2)

# repeat calculation until tests OK
while (difference > tolerance) {
	x <- number/estimate
	estimate <- (x+estimate)/2
	difference <- abs(number-estimate^2)
}

sqr.root
ddg.save()
