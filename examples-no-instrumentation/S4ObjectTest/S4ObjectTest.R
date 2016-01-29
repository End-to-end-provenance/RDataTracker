### Tests the S4 Object functionalities of RDataTracker
# Originally created by Barbara Lerner

# Modified by Luis Perez 7-Jul-2014
# Modified by Luis Perez 17-Jul-2014



require(methods)

#setGeneric("toString", function(object) {
#			standardGeneric("toString")
#		})

setClass("SampleObj",
		slots = c(
				sampleCode = "integer", 
				nIndividuals = "integer", 
				areaCode = "integer"))

as.character.SampleObj <- function(x) {
#toString.SampleObj <- function(object) {
	str <- paste("sampleCode =", x@sampleCode, "\n")
	str <- paste(str, "nIndividuals =", x@nIndividuals, "\n")
	str <- paste(str, "areaCode =", x@areaCode, "\n")
	return(str)
}
#
#as.character.SampleObj <- function(x) {
#	toString.SampleObj(x)
#}

#setMethod("toString", 
##setMethod("as.vector", 
##		signature(object = "SampleObj", mode = "character"),
#		signature(object = "SampleObj"),
#		toString.SampleObj)
#		
setMethod("as.character", 
		signature(x = "SampleObj"),
		as.character.SampleObj)

generateSamples <- function (n) {
	samplesArr <- list()
	for (ix in 1:n) {
		newSample <- new ("SampleObj", 
				sampleCode = ix, 
				nIndividuals = as.integer(ix * 10), 
				areaCode = as.integer(ix * 100))
		samplesArr = c(samplesArr, newSample)
				
	}
	return (samplesArr)
}

generate1Sample <- function () {
	newSample <- new ("SampleObj", 
				sampleCode = as.integer(1), 
				nIndividuals = as.integer(10), 
				areaCode = as.integer(100))
		
	return (newSample)
}

totalNumOfSample <- 5
newSample <- generate1Sample()
samplesArr <- generateSamples(totalNumOfSample)


