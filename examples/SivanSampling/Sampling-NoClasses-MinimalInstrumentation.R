# Translation of Sampling Generator code originally written in VisualBasic into R
# VisualBasic author:  Sivan Margalit
# R translator:  Barbara Lerner
# April 2014

# Load the library to create the provenance graphs.  All the function calls below that begin "ddg."
# are calls to functions in this library.

start.time <- Sys.time()
force(start.time)

library(RDataTracker)
ddg.debug.off()

# Initialize the provenance graph
ddg.init("/Users/blerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/SivanSampling/Sampling-NoClasses-MinimalInstrumentation.r",
		"/Users/blerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/SivanSampling/ddg",
		enable.console = TRUE)

######################################################################################################

# Initializes the arrays that describe the percentage of the population comes from each species in an area.

# This is what a function declaration looks like.  It is defining a function and giving it the
# name "newSpeciesDistribution".
defineAreasDistributions <- function(totalNumOfSpecies, area1str, area2str) {
	speciesDistribution <- list()
	speciesDistribution[[1]] <- newSpeciesDistribution(1, totalNumOfSpecies, area1str)
	speciesDistribution[[2]] <- newSpeciesDistribution(2, totalNumOfSpecies, area2str)
	return (speciesDistribution)
}

# This function essentially behaves as a constructor.  It creates the SpeciesDistibutionAreas object
# and initializes based on the values in probabilityStr, replacing the VisualBasic function 
# addSpeciesDistribution.
# signature(aCode: numeric, numOfSpecies: integer, probabilityStr: character vector)
# The proabibilty string should be a comma-separated list of <species code>(<speciesP>)
# Returns a data frame with columns:  speciesCode, p and accP
# The species code is redundant with the row number.
# p is the probability extracted from the probabilityStr for that species
# accP is the accumulated probability for all species up to the current one.  By calculating
#	and storing this, we can avoid the search later.
# The data frame contains one row for each species.
newSpeciesDistribution <- function(aCode, numOfSpecies, probabilityStr) {

	if (numOfSpecies > 0) {
		# A data frame is a table where we can access each column by name.  Each column must have 
		# the same length.
		# Initialize data frame to right size.
		areaDictionary <- data.frame(speciesCode = 1:numOfSpecies, p = rep(0, numOfSpecies), accP = rep(0, numOfSpecies))
		
		# Separate the values
		speciesStrArr <- strsplit(probabilityStr, ",")
		
		# Add the data to the dictionary.  
		for (speciesStr in speciesStrArr) {
			areaDictionary <- addToDictionary(areaDictionary, speciesStr)
		}

		# Count the number of species with probability greater than 0
		countSpecies <- sum(areaDictionary$p > 0)
		
		# Divide the remaining probability equally among the missing species
		areaDictionary <- calculateAccP(areaDictionary, numOfSpecies)
		accP <- areaDictionary[numOfSpecies, "accP"]
		if (accP > 1) stop ("addToDictionary:  total probability exceeds 1")
		
		if (accP < 1) {
			defaultP <- (1 - accP) / (numOfSpecies - countSpecies)
		
			# The following line replaces the 2nd for loop in the VisualBasic addSpeciesDistribution 
			# function.
			areaDictionary$p[areaDictionary$p == 0] <- defaultP
			
			# Recalculate accP
			areaDictionary <- calculateAccP(areaDictionary, numOfSpecies)
		}
	}

	return (areaDictionary)
}

# Calculate the accumulated probability of the species and update the area dictionary
# with that information.
calculateAccP <- function(areaDictionary, numOfSpecies) {
	areaDictionary[1, "accP"] <- areaDictionary[1, "p"]
	for (i in 2:numOfSpecies) {
		areaDictionary[i, "accP"] <- areaDictionary[i, "p"] + areaDictionary[i-1, "accP"]
	}
	
	return (areaDictionary)
}

# This function essentially replaces the first for-loop in the VisualBasic definition of 
# addSpeciesDistribution. 
# Signature:  areaDictionary:  data frame, speciesStr: character vector
# Returns the updated area dictionary
# Syntax of speciesStr should be:  <species code>(<speciesP>)
# Species code should be an integer
# Species probability should be a numeric value
addToDictionary <- function (areaDictionary, speciesStr) {
	# Parse the species string  "code(probability)"
	leftParenPos <- regexpr("(", speciesStr, fixed=TRUE)
	rightParenPos <- regexpr(")", speciesStr, fixed=TRUE)
	speciesCode <- as.integer(substr (speciesStr, 1, leftParenPos - 1))
	speciesP <- as.numeric(substr (speciesStr, leftParenPos + 1, rightParenPos - 1))
	
	# Add the information to the table
	if (speciesCode < 1 || speciesCode > nrow(areaDictionary)) {
		stop ("addToDictionary:  speciesCode ", speciesCode, " is out of range")
	}
	if (speciesP < 0 || speciesP > 1) {
		stop ("addToDictionary:  speciesP", speciesP, " should be between 0 and 1")
	}
	areaDictionary[speciesCode, "speciesCode"] <- speciesCode
	areaDictionary[speciesCode, "p"] <- speciesP

	return (areaDictionary)
}


############################################################################

# Table of constraints

# Initializes the constraints about the probability of different population sizes
# Returns a data frame containing columns for the probability for the size range,
# the minimum size, the maximum size, and the accumulated probability for that
# size and smaller.
defineSamplesSizeDistribution <- function() {
	# for now we have only one size distribution pattern
	n <- 7
	sampleSizeDistributionMng <- data.frame(p=rep(NA, n), minSize=rep(NA, n), maxSize=rep(NA, n), accP=rep(NA, n))
	sampleSizeDistributionMng[1, ] <- c(0.42, 0, 0, 0.42)
	sampleSizeDistributionMng[2, ] <- c(0.26, 1, 2, NA)
	sampleSizeDistributionMng[3, ] <- c(0.165, 3, 4, NA)
	sampleSizeDistributionMng[4, ] <- c(0.075, 5, 6, NA)
	sampleSizeDistributionMng[5, ] <- c(0.045, 7, 9, NA)
	sampleSizeDistributionMng[6, ] <- c(0.025, 10, 12, NA)
	sampleSizeDistributionMng[7, ] <- c(0.007, 13, 20, NA)
	
	# Calculate the accumulated probabilities.  Need to use a for loop since the later calculations depend on the earlier ones
	for (i in 2:7) {
		sampleSizeDistributionMng$accP[i] <- sampleSizeDistributionMng$p[i] + sampleSizeDistributionMng$accP[i-1]
	}
	if (sampleSizeDistributionMng$accP[7] > 1) stop("defineSampleSizeDistribution:  accumulated probability > 1")
	
	return(sampleSizeDistributionMng)
}

# Signature:  sampleSizeDistributionMng: data frame, maxSampleSize: integer
# Returns a random integer to be used as the sample size
raffleSampleSize <- function(sampleSizeDistributionMng, maxSampleSize = 25) {
	
	randNo1 <- runif(1)
	
	# Find the first entry whose accumulated probability is greater than the 
	# random number generated above.  Return a random number between the 
	# minimum and maximum sizes for that entry.
	matches <- sampleSizeDistributionMng[randNo1 <= sampleSizeDistributionMng$accP, ]
	if (nrow(matches) >= 1){
		return (sample(matches$minSize[1]:matches$maxSize[1], 1))
	}
	
	# The random number exceeded the total probability associated with all constraints.
	# Return a number at least as high as the last maximum.
	lastMaxSampleSize <- sampleSizeDistributionMng$maxSize[nrow(sampleSizeDistributionMng)]
	if (lastMaxSampleSize > maxSampleSize) {
		maxSampleSize <- lastMaxSampleSize + 1
	}
	
	return(sample(lastMaxSampleSize:maxSampleSize, 1))
}

############################################################################

# This function randomly selects species to use in the sample.  numInd is the number of 
# species that are selected.
# Signature:  sampleCode: integer, numInd: integer, speciesDist: data frame, numOfSpecies: integer
# Return: a data frame with columns for the sample code, species code, and species number 
raffleIndividualsPerSample <- function(sampleCode, numInd, speciesDist, numOfSpecies) {
	if (numOfSpecies == 0) return

	speciesComposition <- data.frame(sampleCode = rep(NA, numOfSpecies), speciesCode = rep(NA, numOfSpecies), speciesNumber = rep(0, numOfSpecies))
	individualId <- 1

	# Each iteration randomly selects a species for an individual, based on the species distribution
	# expected for this area
	while (individualId <= numInd) {
		randNo1 <- runif(1)
		countIndividuals <- 1
		
		# Find the first species whose accumulated probability exceeds the random number
		matches <- subset(speciesDist, randNo1 <= accP, select=c("speciesCode"))
		if (nrow(matches) >= 1){
			matchedSpecies <- matches[1, "speciesCode"]
			
			# Save the new count in the dictionary
			speciesComposition$sampleCode[matchedSpecies]<-sampleCode
			speciesComposition$speciesCode[matchedSpecies]<-matchedSpecies
			speciesComposition$speciesNumber[matchedSpecies]<-speciesComposition[matchedSpecies, "speciesNumber"] + countIndividuals
		}
		
		individualId <- individualId + countIndividuals
	}
	
	# Return just the entries for species that were randomly selected.
	return(subset(speciesComposition, speciesNumber > 0))
}

############################################################################

# Initialize the samples to be empty
# n - number of samples
# Return the initalized samples.  This is a data frame with a column for the number of 
#	individuals, and a column for the area in which they are found
generateSamples <- function (n) {
	
	samplesArr <- data.frame(nIndividuals = rep(NA, n), areaCode = rep(NA, n))
	return (samplesArr)
}

# samplesArr - data frame describing the samples
# samplesMapsStr - string defining ??
# speciesDistribution - data frame describing probability of a species
# sampleSizeDistributionMng - SampleSizeDistribution defining probability of finding population of each size
assignSamplesToAreas <- function (samplesArr, samplesMapsStr, speciesDistribution, sampleSizeDistributionMng) {

	# Divide map into its areas
	splitAreasArr <- strsplit(samplesMapsStr, "[", fixed=TRUE)[[1]]
	
	# Remove any empty entries
	splitAreasArr <- splitAreasArr[splitAreasArr != ""]
	
	# Assign sample for each area.
	for (areaStr in splitAreasArr) {
		samplesArr <- assignSamplesToArea(samplesArr, areaStr, speciesDistribution, sampleSizeDistributionMng)
	}
	
	return(samplesArr)
}

# Assigns sample data for an area.
# samplesArr - data frame describing the samples
# areaStr - string for an individual area
# speciesDistribution - data frame describing probability of a species
# sampleSizeDistributionMng - data frame defining probability of finding population of each size
# Returns updated samplesArr with values as given in areaStr
assignSamplesToArea <- function(samplesArr, areaStr, speciesDistribution, sampleSizeDistributionMng) {
	
	if (length(areaStr > 0)) {
		# Parse the string:  <areaCode>:<sampleListStr>], where <sampleListStr> are comma-separated 
		# sample information
		colonPosition <- regexpr(":", areaStr, fixed=TRUE)
		areaCode <- as.integer(substr(areaStr, 1, colonPosition - 1))

		closeBracket <- regexpr("]", areaStr, fixed=TRUE)
		sampleListStr <- substr (areaStr, colonPosition + 1, closeBracket - 1)
		tmpSamplesInArea <- strsplit(sampleListStr, ",")[[1]]
		
		if (!is.na(areaCode) && areaCode <= length(speciesDistribution)) {
			# Assign sample information for each sample found in the string
			for (sampleInAreaStr in tmpSamplesInArea) {
				samplesArr <- assignSampleData(samplesArr, sampleInAreaStr, areaCode, sampleSizeDistributionMng)
			}
		}
	}

	return(samplesArr)
}

# samplesArr - data frame of samples
# sampleInAreaStr - sample code, passed as a string
# areaCode - area sample is for
# sampleSizeDistributionMng - population distribution 
# speciesDistribution - distribution of species
assignSampleData <- function(samplesArr, sampleInAreaStr, areaCode, sampleSizeDistributionMng) {
	
	sampleCode <- as.integer(sampleInAreaStr)
	num <- raffleSampleSize(sampleSizeDistributionMng)
	samplesArr[sampleCode, ] <- c(num, areaCode)
	return(samplesArr)
}

#######################################################################################

# samplesArr - complete information about the size of samples desired
# speciesDistributionAreas - probabilities of each species in an area
# totalNumOfSample - 
# Returns a data frame descirbing how many of each speices is found in a sample, randomly
#	selecting the species.  The data frame has columns for sample id, species code and
#	the number of individuals in that species in the sample
raffleSamples <- function (samplesArr, speciesDistributionAreas, totalNumOfSample ) {
	smpCompositions <- data.frame(sampleId = rep(NA, totalNumOfSample * 2), speciesCode = rep(NA, totalNumOfSample * 2), speciesNumber = rep(NA, totalNumOfSample * 2))
	
	# Do the random sampling
	smplx <- 1
	index <- 1
	for (i in 1:nrow(samplesArr)) {
		numInd <- samplesArr[i,"nIndividuals"]
		if (numInd > 0) {
			areaCode <- samplesArr[i,"areaCode"]
			sampleRaffle <- raffleIndividualsPerSample(i, numInd, speciesDistributionAreas[[areaCode]], totalNumOfSpecies)
			numEntries <- nrow(sampleRaffle)
			
			smpCompositions$sampleId[index:(index+numEntries-1)] <- smplx
			smpCompositions$speciesCode[index:(index+numEntries-1)] <- sampleRaffle$speciesCode
			smpCompositions$speciesNumber[index:(index+numEntries-1)] <- sampleRaffle$speciesNumber
			
			smplx <- smplx + 1
			index <- index + numEntries
		}
	}
	
	return(smpCompositions)
}

# Writes random sample output to a csv file
# wsName - name of the file
# title - header inside the file
# totalNumOfSpecies
# totalNumOfSample
# samplingResult - data frame describing the generated samples
writeToFile<- function (wsName, title, totalNumOfSpecies, totalNumOfSample, samplingResult) {

	# Open the file
	fileConn <- file(paste(wsName, ".csv", sep=""), open="w")
	
	# Write header information
	writeLines(title, fileConn)
	writeLines(paste(totalNumOfSpecies, totalNumOfSample, sep=","), fileConn)

	# Write data
	write.table(subset(samplingResult, !is.na(speciesCode)), fileConn, sep=",", row.names=FALSE, col.names=FALSE)
	
	# Write end of data marker
	writeLines("-1,-1,-1", fileConn)
	
	close(fileConn)
}

#############################################################################################


# Set species distribution pattern
# speciesCode(speciesProbability)
area1str <- "1(0.01) , 2(0.01), 3(0.01), 4(0.01), 5(0.01), 6(0.05), 7(0.05), 8(0.1), 9(0.15), 10(0.25), 11(0.35)"
area2str <- "12(0.3), 13(0.3), 14(0.3), 15(0.1)"
area3str <- ""

#~20% patch - sampling1 20%
# [areaCode: <sampleCode>* ]
# Identifiew which specimens are found in which area?
samplesMapsStr <- paste("[1:  1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 33, 34, 35, 36, 37, 38, 39, 40, 41, 43, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 70, 72, 73, 74, 75, 77, 78, 83, 84, 85, 86, 87, 88, 89, 91, 92, 93, 95, 96, 97, 98, 99, 100, 101, 103, 104, 105, 106, 107, 108, 110, 111, 112, 113, 114, 115, 116, 117, 118, 120, 121, 122, 123, 124, 125, 127, 128, 129, 130, 131, 133, 134, 136, 137, 138, 139, 140, 142, 143, 144, 145, 146, 147, 148, 149, 151, 153, 154, 155, 156, 158, 159, 160, 162, 163, 165, 166, 167, 169, 170, 171, 174, 176, 177, 179, 180],",
		"[2: 2, 14, 20, 32, 42, 44, 69, 71, 76, 79, 80, 81, 82, 90, 94, 102, 109, 119, 126, 132, 135, 141, 150, 152, 157, 161, 164, 168, 172, 173, 175, 178]")

totalNumOfSpecies <- 33
totalNumOfSample <- 180

speciesDistribution <- defineAreasDistributions(totalNumOfSpecies, area1str, area2str)
sampleSizeDistributionMng <- defineSamplesSizeDistribution()
samplesArr <- generateSamples(totalNumOfSample)
samplesArr <- assignSamplesToAreas(samplesArr, samplesMapsStr, speciesDistribution, sampleSizeDistributionMng)

samplingResult <- raffleSamples(samplesArr, speciesDistribution, totalNumOfSample)

writeToFile("genSmpls1", "virtual sampling1 (Uniform distribution)", totalNumOfSpecies, totalNumOfSample, samplingResult)

ddg.save()
finish.time <- Sys.time()
print(paste("Elapsed time =", (finish.time - start.time)))


