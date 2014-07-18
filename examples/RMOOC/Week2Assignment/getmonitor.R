getmonitor <- function(id, directory, summarize = FALSE) {
  ## 'id' is a vector of length 1 indicating the monitor ID
  ## number. The user can specify 'id' as either an integer, a
  ## character, or a numeric.
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'summarize' is a logical indicating whether a summary of
  ## the data should be printed to the console; the default is
  ## FALSE
  
  ddg.procedure()
  
  if (id < 10) {
    id <- paste("00", id, sep="")
  }
  else if (id < 100) {
    id <- paste("0", id, sep="")
  }
  filename <- paste(directory, "/", id, ".csv", sep="")
  data <- read.csv(filename)
  ddg.file(paste(id, ".csv", sep=""), directory)
  ddg.data.in("getmonitor", paste(id, ".csv", sep=""))
  
  if (summarize) {
    print (summary (data))
  }
  #ddg.snapshot.out("getmonitor", "data", data)
  return(data)
}