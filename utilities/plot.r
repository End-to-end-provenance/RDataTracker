# import libraries
library(ggplot2)
library(gridExtra)
library(plyr)

# set the working directory to source script
if (!exists("base.dir")) base.dir <- "D:/Users/Luis/Documents/Harvard School Work/Summer 2014/RDataTracker"
util.dir <- paste0(base.dir, "/utilities")
test.dir <- paste0(base.dir, "/examples")
setwd(util.dir)

# source in the files
source("scriptTimer.r")
source("helpers.r")

### Define Functions ###
# create barplot of yaxis vs xaxis grouped by source with colors representing cType
# based on subset of observations passed in
plotByType <- function(data, xaxis, yaxis, title, xlabel = xaxis, ylabel = yaxis, cType = "type"){
  ggplot(data, aes_string(x = xaxis, y = yaxis, fill = cType)) +
    geom_bar(stat="identity", width = 0.5, position = "dodge") +
    xlab(xlabel) + ylab(ylabel) +
    ggtitle(title) +
    theme_bw(axis.text.x = element_text(angle = 45, hjust = 1))
}

# creates a double line plot of yaxes vs xaxis grouped where each line represents minimal and manual annotations
plotDoubleLine <- function(data, xaxis, yaxis, title, xlabel=xaxis, ylabel=yaxis) {
  ggplot(data, aes_string(y=yaxis, x = xaxis, group="type", shape="type", color="type")) +
    geom_line(aes(linetype=type), size=1) +
    geom_point(size=3, fill="white") +
    scale_colour_hue(name="type",
                     l=30) +
    scale_shape_manual(name="type",
                       values=c(22,21)) +
    scale_linetype_discrete(name="type") +
    xlab(xlabel) + ylab(ylabel) + 
    ggtitle(title) +
    theme_bw() +
    theme(legend.position=c(.7, .4))
}

# function mapping *-min.r to minimal, *-annotated.r to annotated, and everything else to original
findType <- function(name){
  return(gsub("^(.*)-", "", sub(".r$", "", name, ignore.case=TRUE)))
}

### MAIN ###
# params:
# @outPath - the relative output path for the resulting pdf files (the graphs) not starting with "/"
# @inFile (optional) - the input path to the file of a data frame already produced by scriptTimer, 
#                   or with a similar format (same columns, at the very least). If null, scriptTimer
#                   is executed and the resulting data is used.
# @fileNamePattern (optional) - the pattern to follow when saving the result graphs. Two files are 
#                  output in the outPath directory. The files are short_fileNamePattern.pdf and
#                  long_fileNamePattern.pdf. If fileNamePattern is NULL, then fileNamePattern is
#                  the pattern used as the output file for scriptTimer (or the basename of the inFile).
plot.main <- function(outPath = "plots", inFile = NULL, fileNamePattern = NULL) {
  # we want to work from within the base directory
  # browser()
  setwd(base.dir)

  # execute scriptTimer to capture results (this writes out the results in examples/_timingResults)
  scriptInfo <- if (is.null(inFile)) scriptTimer.main() else list("data"=read.csv(inFile), "file.name"= basename(inFile))

  # deduce the fileNamePattern and the main data based on inputs given
  fileNamePattern <- if (is.null(fileNamePattern)) sub(".csv$","",scriptInfo$file.name) else fileNamePattern
  results <- scriptInfo$data

  # set the working directory
  path <- paste0(base.dir,"/",outPath)
  dir.create(path, showWarnings=FALSE)
  setwd(path)

  # The column names so our script works
  old.colnames <- colnames(results)
  colnames(results) <- c("script.file", "source", "script.loc", "type", "exec.time", "file.size", "ddg.dir", "ddg.dir.size", "lib.version")
  
  # calculate the maximum for each script source so we can subset based on time and ddg dir size
  results <- ddply(results, "source", transform, max.exec.time = max(exec.time), max.ddg.dir.size = max(ddg.dir.size))

  # divide into long and short executions (over 1 min and under 1 min)
  fastScripts <- droplevels(subset(results, max.exec.time <= 20))
  mediumScripts <- subset(results, max.exec.time > 20 && max.exec.time <= 60)
  slowScripts <- subset(results, max.exec.time > 60)
  slowScripts$exec.time <- slowScripts$exec.time / 60 # convert to minutes from seconds

  # combine into one full data set
  sectioned.data <- list("Fast" = fastScripts, "Moderate" = mediumScripts, "Slow" = slowScripts)
  time.units <- list("Fast" =  "sec", "Moderate" = "sec", "Slow"="min")

  # Start PLOTTING

  # for small and large, create time vs script grouped by source colored by instrumentation type
  time.vs.script <- Map(function(data, subset.type, time.unit){
        plotByType( data, "source", "exec.time", paste0("Execution Time Results (", subset.type, " scripts)"),
                    xlabel="Source Script File", ylabel=paste0("Total Execution Time (", time.unit, ")"))
      }, sectioned.data, names(sectioned.data), time.units)

  # for small and large, create script size vs script grouped by source colored by instrumentation type
  script.vs.script <- Map(function(data, subset.type){
      plotByType( data, "source", "file.size", paste0("Library Complexity (", subset.type, " scripts)"),
                  xlabel="Source Script File", ylabel="File Size (kB)")
    }, sectioned.data, names(sectioned.data))

  # for small and large, create ddg.dir.size vs script grouped by source colored by instrumentation type
  dir.vs.script <- Map(function(data, subset.type){
      plotByType( subset(data, type != "original"), "source", "ddg.dir.size", paste0("Data Collected (", subset.type, " scripts)"),
                  xlabel="Source Script File", ylabel="Amount of Data Collected (kB)")
    }, sectioned.data, names(sectioned.data))

  # for small and large, execition time vs ddg.dir.size by source colored by instrumentation type
  time.vs.dir <- Map(function(data, subset.type, time.unit){
      plotByType( subset(data, type != "original"), "ddg.dir.size", "exec.time", paste0("Execution Time Dependency on Stored Data (", subset.type, " scripts)"),
                  xlabel="Size of DDG Directory (kB)", ylabel=paste0("Execution Time of Script (", time.unit, ")"))
    }, sectioned.data, names(sectioned.data), time.units)
  
  # arrange and into single device and save to pdf (can only fit 3)
  suffixes <- Map(function(x){paste0(x, "_")}, names(sectioned.data))
  for (i in 1:length(sectioned.data)) {
    pdf(paste0(suffixes[[i]], fileNamePattern, ".pdf"))
    grid.arrange(time.vs.script[[i]], script.vs.script[[i]], dir.vs.script[[i]], ncol=1)
    dev.off()
  }

  # arrange and into single device and save to pdf (this is the report, so make a title)
  pdf(paste0("report_", fileNamePattern, ".pdf"), onefile=TRUE)
  
  # make a title page
  plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
  text(5, 8, "RDataTracker Performance Overview")
  text(5, 7, paste("Input data from", results$file.name))
  text(5, 6, paste("version(s):", levels((results$data)$lib.version)))
  
  # Print all the graphs to the device
  Map(function(one.type){
    Map(function(x){
      print(x)
    }, one.type)
  }, list(time.vs.script, script.vs.script, dir.vs.script, time.vs.dir))
  dev.off()

  invisible()
}

# execute main function if correct parameters are passed in
options <- commandArgs(trailingOnly = TRUE)
if ("plot" %in% options && length(options) == 2) plot.main(options[[2]])
