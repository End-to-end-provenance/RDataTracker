library ("RDataTracker")
#ddg.run ("EnvTest.R")

startTime <- Sys.time()
invisible(force(startTime))

if (interactive()) {
  testDir <- getwd()
  ddgDir <- "ddg"
} else {
  testDir <- "[DIR_DEFAULT]"
  ddgDir <- "[DDG-DIR]"
  setwd(testDir)
}

ddg.r.script.path = paste(testDir,"EnvTest.R",sep="/")
ddg.path = paste(testDir,ddgDir,sep="/")

ddg.init(ddg.r.script.path,
		ddg.path,
     enable.console=TRUE)

f <- function (env) {
  ddg.function()
  return (ddg.return.value (1))
}

f (.GlobalEnv)

ddg.save(quit=TRUE)

# Calculate total time of execution
endTime <- Sys.time()
cat("Execution Time =", difftime(endTime, startTime,units="secs"))
