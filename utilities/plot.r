# import libraries
library(ggplot2)
library(gridExtra)
library(plyr)

# set the working directory to source script if not already set as such
# NOTE: If running from Ant, this does not need to be changed as Ant 
#       automatically replaces it with the right location (ie, the path to the 
#       RDataTracker directory)
if (!exists("base.dir")) base.dir <- "D:/Users/Luis/Documents/Harvard School Work/Summer 2014/RDataTracker"
util.dir <- paste0(base.dir, "/utilities")
test.dir <- paste0(base.dir, "/examples")
setwd(util.dir)

# source in the files containing the functions we want
source("scriptTimer.r")
source("helpers.r")

### Define Functions ###
# create barplot of yaxis vs xaxis grouped by source with colors representing cType
# based on subset of observations passed in. Adjusts the labels to be turned by 45 degrees
plotByType <- function(data, xaxis, yaxis, title, xlabel = xaxis, ylabel = yaxis, cType = "type"){
  pl <- ggplot(data, aes_string(x = xaxis, y = yaxis, fill = cType)) +
          geom_bar(stat="identity", width = 0.5, position = "dodge") +
          xlab(xlabel) + ylab(ylabel) +
          ggtitle(title) +
          theme_bw()
    
    # this turns the x axis labels by 45 degrees if multiple scripts are being plot
  if (length(levels(data$source)) > 1) 
    return(p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1)))
  else 
    return(p1)
}

# creates a double line plot of yaxes vs xaxis grouped where each line represents a different
# type of annotation
plotDoubleLine <- function(data, xaxis, yaxis, title, xlabel=xaxis, ylabel=yaxis) {
  ggplot(data, aes_string(y=yaxis, x = xaxis, group="type", shape="type", color="type")) +
    geom_line(aes(linetype=type), size=1) +
    geom_point(size=3, fill="white") +
    scale_colour_hue(name="type",
                     l=30) +
    scale_shape_manual(name="type",
                       values=c(22,21,20)) +
    scale_linetype_discrete(name="type") +
    xlab(xlabel) + ylab(ylabel) + 
    ggtitle(title) +
    theme_bw() +
    theme(legend.position=c(.7, .4))
}

# function mapping *-min.r to minimal, *-annotated.r to annotated, and everything
# else to original
findType <- function(name){
  return(gsub("^(.*)-", "", sub(".r$", "", name, ignore.case=TRUE)))
}

# Funtion that generates the report and saves it to an output file
# params: @reportTitle - a string specifying the report title
#         @inputDataFile - a string specifying the filename of the file used to 
#                          create the report
#         @libVersions - a string vector containing unique strings for all the 
#                         versions of the library included in the report
#         @plots - a list of the types of plots to be included in the report, 
#                  one list for each type of plot. Each of those internal lists 
#                  will contain the actual plots, as many as plotted.
print.report <- function(plots, reportTitle = "report", inputDataFile = "unknown", 
                         libVersions = c("unknown")) {

  # arrange into single device and save to pdf (this is the report, so make a title)
  # This report contains all the data generated by this test
  pdf(reportTitle, onefile=TRUE)
  
  # make a title page
  plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
  text(5, 8, "RDataTracker Performance Overview")
  text(5, 7, paste("Input data from: \n", inputDataFile))
  text(5, 6, paste("version(s):", libVersions))
  
  # Print all the graphs to the device (order doesn't particularly matter, as 
  # they are all titled) but if it ever did, you would a double for loop here 
  # instead

  # Look at each list of plots for each type of graphic that we created
  Map(function(one.type){
    # In each list, look at each section of data that was plotted with that type
    # of plot
    Map(function(x){
      # print it
      print(x)
    }, one.type)
  }, plots)

  # turn off the devide
  dev.off()

}

### MAIN ###
# params:
# @outPath - the relative output path for the resulting pdf files (the graphs) 
#            not starting with "/"
# @inFile (optional) - the input path to the file of a data frame already
#                      produced by scriptTimer, 
#                      or with a similar format (same columns, at the very least). 
#                      If null, scriptTimer is executed and the resulting data is used.
# @fileNamePattern (optional) - the pattern to follow when saving the result graphs.
#                               Two files are  output in the outPath directory.
#                               The files are short_fileNamePattern.pdf and
#                               long_fileNamePattern.pdf. If fileNamePattern is NULL,
#                               then fileNamePattern is the pattern used as the output 
#                               file for scriptTimer (or the basename of the inFile).
plot.main <- function(outPath = "plots", inFile = NULL, fileNamePattern = NULL) {
  # we want to work from within the base directory
  setwd(base.dir)

  # execute scriptTimer to capture results (this writes out the results in examples/_timingResults)
  scriptInfo <- if (is.null(inFile)) scriptTimer.main() 
                else list("data"=read.csv(inFile), "file.name"= basename(inFile))

  # deduce the fileNamePattern and the main data based on inputs given
  fileNamePattern <- if (is.null(fileNamePattern)) sub(".csv$","",scriptInfo$file.name)
                     else fileNamePattern

  # results variable will store the actual data for easy access
  results <- scriptInfo$data

  # set the working directory
  path <- paste0(base.dir,"/",outPath)
  dir.create(path, showWarnings=FALSE)
  setwd(path)

  # The column names so our script works
  old.colnames <- colnames(results)
  colnames(results) <- c("script.file", "source", "script.loc", "type", "exec.time",
                         "file.size", "ddg.dir", "ddg.dir.size", "lib.version")
  
  # calculate the maximum for each script source so we can subset based on time 
  # and ddg dir size
  results <- ddply(results, "source", transform, max.exec.time = max(exec.time),
                   max.ddg.dir.size = max(ddg.dir.size))

  # divide into long and short executions (over 1 min and under 1 min)
  fastScripts <- droplevels(subset(results, max.exec.time <= 20))
  mediumScripts <- droplevels(subset(results, max.exec.time > 20 && max.exec.time <= 60))
  slowScripts <- droplevels(subset(results, max.exec.time > 60))

  # convert to minutes from seconds
  slowScripts$exec.time <- slowScripts$exec.time / 60

  # create list containinig all these different data sets, but only if they're not empty
  sectioned.data <- Filter(function(x){nrow(x) > 0}, list("Fast" = fastScripts,
                           "Moderate" = mediumScripts, "Slow" = slowScripts,
                           "All" = results))
  time.units <- list("Fast" =  "sec", "Moderate" = "sec", "Slow"="min")
  time.units <- time.units[unlist(Map(function(x,y){x == y}, names(time.units),
                                  names(sectioned.data)))]

  # Start PLOTTING
  # for sectioned data, create time vs script grouped by source colored by instrumentation type
  time.vs.script <- Map(function(data, subset.type, time.unit){
        plotByType( data, "source", "exec.time", paste0("Execution Time Results (",
                   subset.type, " scripts)"), xlabel="Source Script File",
        ylabel=paste0("Total Execution Time (", time.unit, ")"))
      }, sectioned.data, names(sectioned.data), time.units)

  # for sectioned data, create script size vs script grouped by source colored by 
  # instrumentation type
  script.vs.script <- Map(function(data, subset.type){
      plotByType( data, "source", "file.size", paste0("Library Complexity (",
                  subset.type, " scripts)"), xlabel="Source Script File", 
      ylabel="File Size (kB)")
    }, sectioned.data, names(sectioned.data))

  # for sectioned data, create ddg.dir.size vs script grouped by source colored by 
  # instrumentation type
  dir.vs.script <- Map(function(data, subset.type){
      plotByType( subset(data, type != "original"), "source", "ddg.dir.size", 
                  paste0("Data Collected (", subset.type, " scripts)"),
                  xlabel="Source Script File", ylabel="Amount of Data Collected (kB)")
    }, sectioned.data, names(sectioned.data))

  # for sectioned data, execition time vs ddg.dir.size by source colored by instrumentation type
  time.vs.dir <- Map(function(data, subset.type, time.unit){
      plotDoubleLine(subset(data, type != "original"), "ddg.dir.size", "exec.time",
                     paste0("Execution Time Dependency on Stored Data (",
                            subset.type, " scripts)"),
                  xlabel="Size of DDG Directory (kB)", 
                  ylabel=paste0("Execution Time of Script (", time.unit, ")"))
    }, sectioned.data, names(sectioned.data), time.units)
  
  # arrange each sectioned data into single device and save to pdf 
  # (can only fit 3 plots) - this is an overview of the results 
  suffixes <- Map(function(x){paste0(x, "_")}, names(sectioned.data))
  presentables.dir <- paste0(fileNamePattern, "/")

  # for each sectioned data element, make a nice 3-plot of the results, put it 
  # in a subdirectory with fileNamePattern as the name of the directory and as
  # the name of the file, with suffix derived from the section of data plotted
  dir.create(presentables.dir, showWarnings=FALSE)
  for (i in 1:length(sectioned.data)) {
    pdf(paste0(presentables.dir, suffixes[[i]], fileNamePattern, ".pdf"))
    grid.arrange(time.vs.script[[i]], script.vs.script[[i]], dir.vs.script[[i]], ncol=1)
    dev.off()
  }

  # create the report
  print.report(list(time.vs.script, script.vs.script, dir.vs.script, time.vs.dir),
               paste0("report_", fileNamePattern, ".pdf"), scriptInfo$file.name, 
               levels(results$lib.version))

  # don't return anything
  invisible()
}

# execute main function if correct parameters are passed in
# get the parametners
options <- commandArgs(trailingOnly = TRUE)

# if 'plot' is one of them and their are two parameters, call plot.main with the 
# second (which which sould be the directory under which to store the results)
if ("plot" %in% options && length(options) == 2) plot.main(options[[2]])
