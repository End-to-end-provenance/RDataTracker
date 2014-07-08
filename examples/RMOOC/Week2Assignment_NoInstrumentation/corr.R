# Took 12.7 minutes to run on Mac with ddg instrumentation.
# ddg directory contains 42 MB
start.time <- Sys.time()

source("getmonitor.R")

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  corrs <- vector()

  for (nextId in 1:332) {
	  data <- getmonitor(nextId, directory)
	  goodData <- subset(data, !is.na(sulfate) & !is.na(nitrate))
	  
	  numobs <- nrow(goodData)
	  
	  if (numobs > threshold) {
		  corrs <- c(corrs, cor(goodData$sulfate, goodData$nitrate))	
	  }	  
  }
  
  return (corrs)
}

corr("../../../../../R MOOC/Week2ProgAssign/specdata")

end.time <- Sys.time()
print (c("Start time =", start.time))
print (c("End time =", end.time))
print (c("Length =", end.time - start.time))
