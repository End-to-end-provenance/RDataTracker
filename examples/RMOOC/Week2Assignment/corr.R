# Took 12.7 minutes to run on Mac with ddg instrumentation.
# ddg directory contains 42 MB
# DDG contains 2643 procedure nodes and 2642 data nodes
# Took 7 seconds to run without ddg instrumentation (100 times slower with instrumentation)
start.time <- Sys.time()

ddg.library <- Sys.getenv("DDG_LIBRARY")
if (ddg.library == "") {
	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
}
ddg.r.script.path = "/Users/barbaralerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/RMOOC/Week2Assignment/corr.R"
ddg.path = "/Users/barbaralerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/RMOOC/Week2Assignment/ddg"
source(ddg.library)

corr <- function(directory, threshold = 0) {
  ddg.start()
  ddg.data("directory", directory)
  ddg.data("threshold", threshold)
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  
  corrs <- vector()
  ddg.data("corrs")
  
  ddg.start("for (nextId in 1:332)")
  for (nextId in 1:332) {
	  ddg.start(paste("iteration", nextId))
	  ddg.data("nextId", nextId)
	  data <- getmonitor(nextId, directory)
	  ddg.snapshot.out("getmonitor", "data.csv", data)
	  goodData <- subset(data, !is.na(sulfate) & !is.na(nitrate))
	  ddg.procedure("subset")
	  ddg.data.in("subset", "data.csv")
	  ddg.snapshot.out("subset", "goodData.csv", goodData)
	  
	  numobs <- nrow(goodData)
	  ddg.procedure("nrow")
	  ddg.data.in("nrow", "goodData.csv")
	  ddg.data.out("nrow", "numobs", numobs)
	  
	  ddg.procedure(">")
	  ddg.data.in(">", "numobs")
	  ddg.data.in(">", "threshold")
	  ddg.data.out(">", "enough obs", numobs > threshold)
	  if (numobs > threshold) {
		  corrs <- c(corrs, cor(goodData$sulfate, goodData$nitrate))	
		  ddg.procedure("cor")
		  ddg.data.in("cor", "goodData.csv")
		  ddg.data.out("cor", "sulfate-nitrate cor", cor(goodData$sulfate, goodData$nitrate))
		  ddg.procedure("c")
		  ddg.data.in("c", "corrs")
		  ddg.data.in("c", "sulfate-nitrate cor")
		  ddg.snapshot.out("c", "corrs.txt", corrs)
	  }	  
	  ddg.finish(paste("iteration", nextId))
  }
  ddg.finish("for (nextId in 1:332)")
  
  ddg.procedure("corr")
  ddg.snapshot.out("corr", "corr_return.txt", corrs)
  
  ddg.finish()
  return (corrs)
}

corr("../../../../../R MOOC/Week2ProgAssign/specdata")
#corr("/Users/barbaralerner/Documents/Process/DataProvenance/R MOOC/Week2ProgAssign/specdata")
ddg.save()

end.time <- Sys.time()
print (c("Start time =", start.time))
print (c("End time =", end.time))
print (c("Length =", end.time - start.time))
