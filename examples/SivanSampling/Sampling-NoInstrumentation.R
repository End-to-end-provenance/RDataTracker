# Translation of Sampling Generator code originally written in VisualBasic into R
# VisualBasic author:  Sivan Margalit
# R translator:  Barbara Lerner
# April 2014

start.time <- Sys.time()
force(start.time)

# Declare some simple classes
setClass("SpeciesInSampleObj", 
		slots = c(sampleCode = "integer", speciesCode = "integer", speciesNumber = "integer"))

setClass("DistributionConstraintObj",
		slots = c(p = "numeric", minSize = "integer", maxSize = "integer"))

#########################################################################

# SpeciesDistributionAreas class

# areaDictionary is vector of numeric values.  They are indexed an an integer that represents 
# the species code.  The values represent a probability that the species is in that area.
setClass("SpeciesDistributionAreas",
		slots = c(#areaCode = "integer", 
				areaDictionary = "vector"))

# Define the signatures of the methods of the class

setGeneric("getNumSpecies", function(object) {
			standardGeneric("getNumSpecies")
		})

setGeneric("getDistributionOfSpecies", function(object, index) {
	standardGeneric("getDistributionOfSpecies")
})

# This function essentially behaves as a constructor.  It creates the SpeciesDistibutionAreas object
# and initializes based on the values in probabilityStr, replacing the VisualBasic function 
# addSpeciesDistribution.
# signature(aCode: numeric, numOfSpecies: integer, probabilityStr: character vector)
# The proabibilty string should be a comma-separated list of <species code>(<speciesP>)
# Returns SpeciesDistributionAreas object

# This is what a function declaration looks like.  It is defining a function and giving it the
# name "newSpeciesDistribution".
newSpeciesDistribution <- function(aCode, numOfSpecies, probabilityStr) {
	
	# <- is the syntax for an assignment statement in R.
	newDist <- new("SpeciesDistributionAreas")
	
	# I have the following assignment statement, but I don't see anything equivalent in 
	# the VisualBasic code.  Is this field needed in the SpeciesDistributionAreas class?
#	newDist@areaCode <- as.integer(aCode)
	
	if (numOfSpecies > 0) {
		# Initialize array to right size, filled with NA.  NA means Not Available.
		newDist@areaDictionary <- rep(NA_integer_, numOfSpecies)
		
		# Separate the values
		speciesStrArr <- strsplit(probabilityStr, ",")
		
		# Add the data to the dictionary.  lapply applies the given function to each value in the
		# speciesStrArr.  It returns a vector that is the same length as speciesStrArr, with each
		# value being the result of calling the function on the corresponding value in speciesStr.
		newDist <- lapply(speciesStrArr, function(speciesStr) {addToDictionary(newDist, speciesStr)})[[1]]
		
		# Total the probability and # species.  These add up the real values, skipping NAs.
		accP <- sum(newDist@areaDictionary[!is.na(newDist@areaDictionary)])
		if (accP > 1) stop ("newSpeciesDistribution:  total probability exceeds 1")
		countSpecies <- sum(!is.na(newDist@areaDictionary))
		
		# Divide the remaining probability equally among the missing species
		defaultP <- (1 - accP) / (numOfSpecies - countSpecies)
		
		# The following line replaces the 2nd for loop in the VisualBasic addSpeciesDistribution 
		# function.
		newDist@areaDictionary[is.na(newDist@areaDictionary)] <- defaultP
	}
	return(newDist)
}

# This function essentially replaces the first for-loop in the VisualBasic definition of 
# addSpeciesDistribution. 
# Signature:  speciesDistributionAreas: SpeciesDistributionAreas, speciesStr: character vector
# Returns the updated speciesDistribuionAreas object.
# Syntax of speciesStr should be:  <species code>(<speciesP>)
# Species code should be an integer
# Species probability should be a numeric value
addToDictionary <- function (speciesDistributionAreas, speciesStr) {
	# Parse the species string  "code(probability)"
	leftParenPos <- regexpr("(", speciesStr, fixed=TRUE)
	rightParenPos <- regexpr(")", speciesStr, fixed=TRUE)
	speciesCode <- as.integer(substr (speciesStr, 1, leftParenPos - 1))
	speciesP <- as.numeric(substr (speciesStr, leftParenPos + 1, rightParenPos - 1))
	
	# Add the information to the table
	if (speciesCode < 1 || speciesCode > length(speciesDistributionAreas@areaDictionary)) {
		stop ("addToDictionary:  speciesCode ", speciesCode, " is out of range")
	}
	if (speciesP < 0 || speciesP > 1) {
		stop ("addToDictionary:  speciesP", speciesP, " should be between 0 and 1")
	}
	speciesDistributionAreas@areaDictionary[speciesCode] <- speciesP
	return (speciesDistributionAreas)
}

# Signature:  object: SpeciesDistributionAreas
# Return integer representing the number of species in the distribution dictionary
getNumSpeciesFun <- function(object) {
	return (length(object@areaDictionary))
}

# Signature:  object: SpeciesDistributionAreas, index: integer
# Return the distribution probability for the species at position index
getDistributionOfSpeciesFun <- function(object, index) {
	return (object@areaDictionary[index])
}

setMethod("getNumSpecies", 
		signature(object = "SpeciesDistributionAreas"), 
		getNumSpeciesFun)

setMethod("getDistributionOfSpecies", 
		signature(object = "SpeciesDistributionAreas", index = "integer"),
		getDistributionOfSpeciesFun)


############################################################################

# SampleSizeDistribution class

# sampleSizeDistDictionary is a list of constraints of type DistribuitonConstraintObj
# numEntries is the number of entries in the list
# There seems to be an assumption that the min..max range of constraints do not overlap.
# Also, the total probabilities of all constraints should be <= 1.
setClass("SampleSizeDistribution",
		slots = c(sampleSizeDistDictionary = "list", limitSampleSize = "integer"),
		prototype = list(limitSampleSize = as.integer(25)))
		
setGeneric("addConstraint", function(object, inP, inMin, inMax) {
			standardGeneric("addConstraint")
		})

setGeneric("raffleSampleSize", function(object, maxSampleSize) {
			standardGeneric("raffleSampleSize")
		})

# Creates a new DisibutionConstraint and adds it to the collection of constraints
# Signature:  object: SampleSizeDistribtuion, inP: numeric, inMin: integer, inMax: integer
# Returns an updated DistributionConstraintObj object
addConstraintFun <- function (object, inP, inMin, inMax) {
	if (inP > 0) {
		newDistConstraint <- 
				new ("DistributionConstraintObj", p = inP, minSize = as.integer(inMin), maxSize = as.integer(inMax))
		object@sampleSizeDistDictionary <- c(object@sampleSizeDistDictionary,newDistConstraint)
	}
	return(object)
}

# Signature:  object: SampleSizeDistribution, maxSampleSize: integer
# If 0 is passed in for maxSampleSize, the default limit is used instead.
# Returns a random integer to be used as the sample size
raffleSampleSizeFun <- function(object, maxSampleSize) {
	
	# Use default maximum if none provided in the call.
	if (maxSampleSize == 0) {
		maxSampleSize <- object@limitSampleSize
	}
	
	randNo1 <- runif(1)
	lastMaxSampleSize <- 0
	accDist <- 0
	
	# Search through the constraints adding up the probabilities of each constraint until
	# finding the one that execeeds the random number.  Once the constraint is found, 
	# return a random number between that constraints minimum and maximum size.
	for (distConstrain in object@sampleSizeDistDictionary) {
		accDist <- accDist + distConstrain@p
		if (randNo1 <= accDist) {
			return(sample(distConstrain@minSize:distConstrain@maxSize, 1))
		}
		else {
			lastMaxSampleSize <- distConstrain@maxSize
		}
	}
	
	# The random number exceeded the total probability associated with all constraints.
	# Return a number at least as high as the last maximum.
	if (lastMaxSampleSize > maxSampleSize) {
		maxSampleSize <- lastMaxSampleSize + 1
	}
	
	retValue <- sample(lastMaxSampleSize:maxSampleSize, 1)
	return(retValue)
}

setMethod("addConstraint", 
		signature(object = "SampleSizeDistribution", inP = "numeric", inMin = "numeric", inMax = "numeric"), 
		addConstraintFun)

setMethod("raffleSampleSize", 
		signature(object = "SampleSizeDistribution", maxSampleSize = "numeric"), 
		raffleSampleSizeFun)

############################################################################

# SampleObj class

setClass("SampleObj",
		slots = c(
				sampleCode = "integer", 
				sampleSizeDistObj = "SampleSizeDistribution",
				nIndividuals = "integer", 
				speciesDistributionDef = "SpeciesDistributionAreas",
				areaCode = "integer"))

setGeneric("assignArea", function(object, inAreaCode, speciesDistributionAreas) {
			standardGeneric("assignArea")
		})

setGeneric("assignSizeDistProperty", function(object, inSampleSizeDist, limitSampleSize) {
			standardGeneric("assignSizeDistProperty")
		})

setGeneric("raffleIndividualsPerSample", function(object) {
			standardGeneric("raffleIndividualsPerSample")
		})

# Signature: object: SampleObj, inAreaCode: integer, speciesDistributionAreas: list of SpeciesDistributionAreas
# Returns an updated SampleObj with the speciesDistribution and areaCode set.
assignAreaFun <- function(object, inAreaCode, speciesDistributionAreas) {
	if (inAreaCode >= 1 && inAreaCode <= length(speciesDistributionAreas)) {
		object@speciesDistributionDef <- speciesDistributionAreas[[inAreaCode]]
		object@areaCode <- inAreaCode
	}
	return (object)
}

# Signature:  object: SampleObj, inSampleSizeDist: SampleSizeDistribution
# Return updated SampleObj with sampeSizeDistObj set and nIndividuals set to a random size
assignSizeDistPropertyFun <- function(object, inSampleSizeDist) {
	object@sampleSizeDistObj <- inSampleSizeDist
	object@nIndividuals <- raffleSampleSize(object@sampleSizeDistObj, 0)
	return(object)
}

# Signature:  object: SampleObj
# Return: a dictionary mapping species codes to number of individuals of that species present in the 
#	sample.  The total number of individuals is set earlier.  This function distributes those individuals
#   across species.
raffleIndividualsPerSampleFun <- function(object) {
	if (getNumSpecies(object@speciesDistributionDef) == 0) return

	speciesComposition <- new.env()
	individualId <- 1

	# Each iteration randomly selects a species for an individual, based on the species distribution
	# expected for this sample.
	while (individualId <= object@nIndividuals) {
		randNo1 <- runif(1)
		accDist <- 0
		countIndividuals <- 1
		
		# Search for the species to add
		for (k in 1:getNumSpecies(object@speciesDistributionDef)) {
			probability <- getDistributionOfSpecies(object@speciesDistributionDef, k)
			accDist <- accDist + probability
			
			if (randNo1 <= accDist) {
				
				# if sample size is big, than probably several individuals from same nest sampled
				# define a 70% of its probability chance the next individual of same nest
				# If (nIndividuals > 6) And (individualID + countIndividuals < nIndividuals) Then
				#	randNo2 = Rnd()
				#	Do While randNo2 < 0.7 * propability
				#	  countIndividuals = countIndividuals + 1
				#     randNo2 = Rnd()
				#   Loop
				# End If
				
				# Found species k
				key <- as.character(k)
				if (exists(key, speciesComposition)) {
					# There already exists an individual of this species, so increase the count
					existingSpeciesCounter <- get(key,speciesComposition)
					speciesCounter <- new ("SpeciesInSampleObj", sampleCode = object@sampleCode,
							speciesCode = k, speciesNumber = as.integer(existingSpeciesCounter@speciesNumber + countIndividuals))
				}
				else {
					# This is the first individual of this species
					speciesCounter <- new ("SpeciesInSampleObj", sampleCode = object@sampleCode,
						speciesCode = k, speciesNumber = as.integer(countIndividuals))
				}
				
				# Save the new count in the dictionary
				speciesComposition[[key]]<-speciesCounter
				
				# Go on to the next individual
				break
			}
			
		}
		
		individualId <- individualId + countIndividuals
	}
	
	return(speciesComposition)
}

setMethod("assignArea", 
		signature(object = "SampleObj", inAreaCode = "integer", speciesDistributionAreas = "list"),
		assignAreaFun)

setMethod("assignSizeDistProperty", 
		signature(object = "SampleObj", inSampleSizeDist = "SampleSizeDistribution"),		
		assignSizeDistPropertyFun)

setMethod("raffleIndividualsPerSample", 
		signature(object = "SampleObj"),
		raffleIndividualsPerSampleFun)

############################################################################


# Initializes the arrays that describe the percentage of the population comes from each species in an area.
defineAreasDistributions <- function(totalNumOfSpecies, area1str, area2str) {
	speciesDistribution <- list()
	speciesDistribution[[1]] <- newSpeciesDistribution(1, totalNumOfSpecies, area1str)
	speciesDistribution[[2]] <- newSpeciesDistribution(2, totalNumOfSpecies, area2str)
	return (speciesDistribution)
}

# Initializes the constraints about the probability of each number of a species being found
defineSamplesSizeDistribution <- function() {
	# for now we have only one size distribution pattern
	sampleSizeDistributionMng <- new ("SampleSizeDistribution")
	sampleSizeDistributionMng <- addConstraint(sampleSizeDistributionMng, 0.42, 0, 0)
	sampleSizeDistributionMng <- addConstraint(sampleSizeDistributionMng, 0.26, 1, 2)
	sampleSizeDistributionMng <- addConstraint(sampleSizeDistributionMng, 0.165, 3, 4)
	sampleSizeDistributionMng <- addConstraint(sampleSizeDistributionMng, 0.075, 5, 6)
	sampleSizeDistributionMng <- addConstraint(sampleSizeDistributionMng, 0.045, 7, 9)
	sampleSizeDistributionMng <- addConstraint(sampleSizeDistributionMng, 0.025, 10, 12)
	sampleSizeDistributionMng <- addConstraint(sampleSizeDistributionMng, 0.007, 13, 20)
	
	return(sampleSizeDistributionMng)
}

# Initialize the samples to be empty sample objects.
# n - number of samples
# Return the initalized samples.
generateSamples <- function (n) {
	samplesArr <- list()
	for (ix in 1:n) {
		samplesArr = c(samplesArr, new ("SampleObj", sampleCode = ix))
	}
	return (samplesArr)
}

# sampleInAreaStr - sample code, passed as a string
# areaCode - area sample is for
# sampleSizeDistributionMng - distribution of individuals
# speciesDistribution - distribution of species
assignSampleData <- function(sampleInAreaStr, areaCode, speciesDistribution, sampleSizeDistributionMng) {
	sampleCode <- as.integer(sampleInAreaStr)
	samplesArr <<- replace(samplesArr, sampleCode, assignSizeDistProperty(samplesArr[sampleCode][[1]], sampleSizeDistributionMng))
	samplesArr <<- replace(samplesArr, sampleCode, assignArea(samplesArr[sampleCode][[1]], areaCode, speciesDistribution))
}

# Assigns sample data for an area.
# areaStr - string for an individual area, means what?
# speciesDistribution - list of SpeciesDistributionAreas
# sampleSizeDistributionMng - SampleSizeDistribution defining probability of finding population of each size
assignSamplesToArea <- function(areaStr, speciesDistribution, sampleSizeDistributionMng) {

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
			lapply(tmpSamplesInArea, function(sampleInAreaStr) {assignSampleData(sampleInAreaStr, areaCode, speciesDistribution, sampleSizeDistributionMng)})
		}
	}

	return(samplesArr)
}

# samplesMapStr - string defining ??
# speciesDistribution - list of SpeciesDistributionAreas
# sampleSizeDistributionMng - SampleSizeDistribution defining probability of finding population of each size
assignSamplesToAreas <- function (samplesMapStr, speciesDistribution, sampleSizeDistributionMng) {
	
	# Divide map into its areas
	splitAreasArr <- strsplit(samplesMapsStr, "[", fixed=TRUE)[[1]]
	
	# Remove any empty entries
	splitAreasArr <- splitAreasArr[splitAreasArr != ""]

	# Assign sample for each area.
	lapply(splitAreasArr, function(areaStr) {assignSamplesToArea(areaStr, speciesDistribution, sampleSizeDistributionMng)})

	return(samplesArr)
}

# Writes random sample output to a csv file
# wsName - name of the file
# title - header inside the file
# totalNumOfSpecies
# totalNumOfSample
# samplesArr - random sample
raffleSamplesToWorksheet <- function (wsName, title, totalNumOfSpecies, totalNumOfSample, samplesArr) {

	# Open the file
	fileConn <- file(paste(wsName, ".csv", sep=""), open="w")
	
	# Write header information
	writeLines(title, fileConn)
	writeLines(paste(totalNumOfSpecies, totalNumOfSample, sep=","), fileConn)

	# Write data
	smplx <- 1
	for (sample in samplesArr) {
		writeToFile(sample, smplx, fileConn, totalNumOfSample)
		smplx <- smplx + 1
	}
	
	# Write end of data marker
	writeLines("-1,-1,-1", fileConn)
	
	close(fileConn)
}

# Generates the number of individuals for this sample and outputs it to the file
# sample - one sample object
# smplx - 
# fileConn - file to write to
# totalNumOfSample 
writeToFile <- function(sample, smplx, fileConn, totalNumOfSample) {
	smpCompositions <- raffleIndividualsPerSample(sample)
	sampleId <- vector("integer", totalNumOfSample)
	speciesCodes <- vector("integer", totalNumOfSample)
	speciesNumbers <- vector("integer", totalNumOfSample)

	i <- 1
	for (smpComp in as.list(smpCompositions)) {
		sampleId [i] <- smplx
		speciesCodes[i] <- smpComp@speciesCode
		speciesNumbers[i] <- smpComp@speciesNumber
		i <- i + 1
	}

	if (i > 1) {
		df <- data.frame(sampleId[1:i-1], speciesCodes[1:i-1], speciesNumbers[1:i-1])
		write.table(df, fileConn, sep=",", row.names=FALSE, col.names=FALSE)
	}
}


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
samplesArr <- assignSamplesToAreas(samplesMapStr, speciesDistribution, sampleSizeDistributionMng)
raffleSamplesToWorksheet("genSmpls1", "virtual sampling1 (Uniform distribution)", totalNumOfSpecies, totalNumOfSample, samplesArr)

finish.time <- Sys.time()
print(paste("Elapsed time =", (finish.time - start.time)))
