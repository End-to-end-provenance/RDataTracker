# import libraries
library(ggplot2)
library(gridExtra)

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
plotByType <- function(data, xaxis, yaxis, title, xlabel = xaxis, ylabel = yaxis, cType = "annotType"){
  ggplot(data, aes_string(x = xaxis, y = yaxis, fill = cType)) +
    geom_bar(stat="identity", width = 0.5, position = "dodge") +
    xlab(xlabel) + ylab(ylabel) +
    ggtitle(title) +
    theme_bw()
}

# creates a double line plot of yaxes vs xaxis grouped where each line represents minimal and manual annotations
plotDoubleLine <- function(data, xaxis, yaxis, title, xlabel=xaxis, ylabel=yaxis) {
  ggplot(data, aes_string(y=yaxis, x = xaxis, group="annotType", shape="annotType", color="annotType")) +
    geom_line(aes(linetype=annotType), size=1) +
    geom_point(size=3, fill="white") +
    scale_colour_hue(name="AnnotType",
                     l=30) +
    scale_shape_manual(name="AnnotType",
                       values=c(22,21)) +
    scale_linetype_discrete(name="AnnotType") +
    xlab(xlabel) + ylab(ylabel) + 
    ggtitle(title) +
    theme_bw() +
    theme(legend.position=c(.7, .4))
}

# function mapping *-min.r to minimal, *-annotated.r to annotated, and everything else to original
findType <- function(name){
  return(gsub("^(.*)-", "", sub(".r$", "", name, ignore.case=TRUE)))
  }
}

### MAIN ###
main <- function(relPath, fileName) {
  # execute scriptTimer to capture results (this writes out the results in examples/_timingResults)
  rowResults <- scriptTimer.main()

  # set the working directory
  path <- setwd(paste0(basedir, "/",relPath))
  setwd(path)

  # The column names so our script works
  old.colnames <- colnames(rowResults)
  colnames(rowResults) <- c("script.file", "source" "script.loc", "type", "exec.time", "file.size", "ddg.dir", "ddg.dir.size", "lib.version")

  # remove over 1 min execution
  underOneMin <- subset(rowResults, exec.time <= 60)
  overOneMin <- subset(rowResults, exec.time > 60)
  overOneMin$exec.time = overOneMin$exec.time / 60 # convert to seconds from minutes

  # Start PLOTTING

  # for small and large, create time vs script grouped by source colored by instrumentation type
  p1 <- plotByType(underOneMin, "source", "exec.time", "Execution Time Results (modederate scripts)", 
                   xlabel="source Script File", ylabel="Total Execution Time (sec)")
  p2 <- plotByType(overOneMin, "source", "exec.time", "Execution Time Results (all scripts)", 
                   xlabel="source Script File", ylabel="Total Execution Time (min)")

  # for small and large, create script size vs script grouped by source colored by instrumentation type
  p3 <- plotByType(underOneMin, "source", "file.size", "Library Complexity (modederate scripts)",
                   xlabel="source Script File", ylabel="File Size (kB)")
  p4 <- plotByType(overOneMin, "source", "file.size", "Library Complexity (all scripts)",
                   xlabel="source Script File", ylabel="File Size (kB)")

  # for small and large, create ddg.dir.size vs script grouped by source colored by instrumentation type
  # for small and large, create script size vs script grouped by source colored by instrumentation type
  p5 <- plotByType(underOneMin[underOneMin$type != "original", ], "source",
                   "ddg.dir.size", "Data Collected (modederate scripts)", 
                   xlabel="source Script File", ylabel="Amount of Data Collected (kB)")
  p6 <- plotByType(overOneMin[overOneMin$type != "original", ], "source",
                   "ddg.dir.size", "Data Collected (all scripts)",
                    xlabel="source Script File", ylabel="Amount of Data Collected (kB)")

  p7 <- plotDoubleLine(underOneMin[underOneMin$type !="original" ,], "ddg.dir.size", "exec.time", "Execution Time Dependency on Stored Data (moderate scripts)", xlabel = "Size of DDG Directory (kB)", ylabel="Execution Time of Script (sec)")
  p8 <- plotDoubleLine(overOneMin[overOneMin$type !="original" ,], "ddg.dir.size", "exec.time", "Execution Time Dependency on Stored Data (all scripts)", xlabel = "Size of DDG Directory (kB)", ylabel="Execution Time of Script (sec)")

  # arrange and into single device and save to pdf (can only fit 3)
  pdf("elevDataSlide-LuisP.pdf")
  grid.arrange(p1,p3,p5, ncol=1)
  dev.off()

  # arrange and into single device and save to pdf
  pdf(paste0(fileName, ".pdf"))
  p1
  p2
  p3
  p4
  p5
  p6
  p7
  p8
  dev.off()
}
