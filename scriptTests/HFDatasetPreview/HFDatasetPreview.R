######################################
### HARVARD FOREST DATASET PREVIEW ###
######################################

# This script creates a PDF file of summary statistics for one or more
# data tables contained in a dataset in the Harvard Forest Data Archive.
# The resulting previews are intended to help users identify datasets 
# of possible interest and are not intended to be comprehensive.

# The user provides a dataset ID number (e.g. hf123) and optionally a 
# file number (e.g. 4).  If the file number is set to zero, summaries 
# are created for all data tables in the dataset. The PDF file is stored 
# on the user's current working directory.

# The script retrieves the EML file for the dataset, downloads the data
# file, and reads the file into an R data frame, using the following 
# mapping from EML data types to R data types:

#   EML ratio or interval -> R numeric
#   EML dateTime (format YYYY-MM-DD) -> R Date
#   EML dateTime (format YYYY) -> R numeric
#   EML nominal, ordinal, other dateTime -> R character

# The PDF file contains a table of summary statistics for numerical and 
# date variables, followed by a series of time series plots (using the R
# plot command) or a scatterplot matrix (using the R pairs command). In 
# order to keep file size and computational time within reasonable limits,
# long data tables are shortened by sampling at regular intervals to 
# about 10000 rows for time series plots and about 1000 rows for 
# scatterplot matricies. For the latter, variables are plotted in groups 
# of six variables at a time, so not not all possible combinations of
# variables are plotted for wide tables.

# Copyright (C) 2015 Emery R Boose

# revised 2-May-2015

# libraries
library(RCurl)
library(XML)
library(gplots)

# HF web server EML directory
hf.eml.dir <- "http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/data/eml/"

# HF web server data directory
hf.data.dir <- "http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/data/"

#------------------------DOWNLOAD FILES----------------------------#

# get and parse EML file
get.parsed.eml <- function(dataset.id) {
  # get URL (GLOBAL)
  eml.url <<- paste(hf.eml.dir, dataset.id, ".xml", sep="")
  if (!url.exists(eml.url)) {
    msg <- paste(dataset.id, ": EML file does not exist", sep="")
    warning(msg)
    eml <- NULL
  } else {
    eml <- xmlTreeParse(eml.url, useInternalNodes=TRUE)
  }
  return(eml)
}

# parse time series string
parse.time.series.string <- function(st) {
  # split string and convert to integer vector
  stp <- strsplit(st, ",")
  for (i in 1:length(stp[[1]])) {
    if (i == 1) {
      ids <- as.integer(stp[[1]][i])
    } else {
      ids <- append(ids, as.integer(stp[[1]][i]), length(ids))
    }
  }
  return(ids)
}

# get time series ids from EML
get.time.series.ids <- function(dataset.id) {
  xpath <- paste("//additionalMetadata/metadata/additionalClassifications/timeSeries", sep="")
  ns <- getNodeSet(eml, xpath, fun=xmlValue) 
  if (length(ns) == 0) {
    time.series.ids <- NULL
  } else {
    st <- ns[[1]]
    time.series.ids <- parse.time.series.string(st)
  }  
  return(time.series.ids)
}

# get datafile id from dataset id and datafile number
get.datafile.id <- function(dataset.id, datafile.num) {
# create string for datafile.id
  if (datafile.num > 9) {
    st <- toString(datafile.num)
  } else {
    st <- paste("0", toString(datafile.num), sep="")
  }
  datafile.id <- paste(dataset.id, "-", st, sep="")
  # check lower and upper case
  xpath1 <- paste("//dataTable[@id='", datafile.id, "']", sep="")
  xpath2 <- paste("//dataTable[@id='", toupper(datafile.id), "']", sep="")
  ns1 <- getNodeSet(eml, xpath1)
  ns2 <- getNodeSet(eml, xpath2)
  # set to NULL if no data table
  if (length(ns1) == 0 & length(ns2) == 0) {
    datafile.id <- NULL    
  }
  return(datafile.id)  
}

# get all data table ids from node set
get.all.datafile.ids <- function(ns) {
  datafile.ids <- NULL
  num <- xmlSize(ns)
  if (num > 0) {
    ns2 <- xmlSApply(ns, xmlGetAttr, "id")
    for (i in 1:num) {
      datafile.id <- tolower(ns2[[i]])
      if (i==1) {
        datafile.ids <- datafile.id
      } else {
        datafile.ids <- append(datafile.ids, datafile.id, after=length(datafile.ids))
      }
    }
  }
  return(datafile.ids)
}

# get data file ids from EML
get.datafile.ids <- function (dataset.id) {
  # get node set
  xpath <- paste("//dataTable", sep="")
  ns <- getNodeSet(eml, xpath)
  # get datafile ids
  datafile.ids <- get.all.datafile.ids(ns)
  return(datafile.ids)
}

# get datafile name
get.datafile.name <- function(eml, datafile.id) {
  # check lower and upper case
  xpath1 <- paste("//dataTable[@id='", datafile.id, "']/entityName", sep="")
  xpath2 <- paste("//dataTable[@id='", toupper(datafile.id), "']/entityName", sep="")
  ns <- getNodeSet(eml, xpath1, fun=xmlValue)
  if (length(ns) == 0) ns <- getNodeSet(eml, xpath2, fun=xmlValue)
  name <- ns[[1]]
  return(name)
}

# get URL for data file
get.data.url <- function(eml, datafile.id) {
  # check lower and upper case
  xpath1 <- paste("//dataTable[@id='", datafile.id, "']/physical/distribution/online/url", sep="")
  xpath2 <- paste("//dataTable[@id='", toupper(datafile.id), "']/physical/distribution/online/url", sep="")
  ns <- getNodeSet(eml, xpath1, fun=xmlValue)
  if (length(ns) == 0) ns <- getNodeSet(eml, xpath2, fun=xmlValue)
  data.url <- ns[[1]]
  if (!url.exists(data.url)) {
    data.url <- NULL
  }
  return(data.url)
}

# get node set for data table
get.data.table.node.set <- function(eml, datafile.id) {
  # check lower and upper case
  xpath1 <- paste("//dataTable[@id='", datafile.id, "']/attributeList/attribute", sep="")
  xpath2 <- paste("//dataTable[@id='", toupper(datafile.id), "']/attributeList/attribute", sep="")
  ns <- getNodeSet(eml, xpath1)
  if (length(ns) == 0) ns <- getNodeSet(eml, xpath2)
  return(ns)
}

# get R variable type
get.variable.type <- function(ns) {
  # EML ratio or interval -> R numeric
  x1 <- xpathApply(ns, "measurementScale/ratio", xmlValue)
  x2 <- xpathApply(ns, "measurementScale/interval", xmlValue)
  if (length(x1) > 0 | length(x2) > 0) {
    var.type <- "numeric"
  } else {
    x3 <- xpathApply(ns, "measurementScale/dateTime/formatString", xmlValue)
    if (length(x3) > 0) {
      if (x3[[1]] == "YYYY-MM-DD") {
        # EML dateTime in YYYY-MM-DD format -> R Date
        var.type <- "Date"
      } else if ( x3[[1]] == "YYYY") {
        # EML dateTime in YYYY format -> R numeric
        var.type <- "numeric"
      } else {
        # EML dateTime in other formats -> R character
        var.type <- "character"
      }
    } else {
      # EML nominal, ordinal -> R character
      var.type <- "character"
    }
  }
  return(var.type)
}

# get all variable types for data table
get.all.variable.types <- function(ns) {
  num <- xmlSize(ns)
  for (i in 1:num) {
    var.type <- get.variable.type(ns[[i]])
    if (i==1) var.types <- var.type
    else var.types <- append(var.types, var.type, after=length(var.types))
  }
  return(var.types)
}

# get variable format string
get.variable.format.string <- function(ns) {
  x <- xpathApply(ns, "measurementScale/dateTime/formatString", xmlValue)
  if (length(x) > 0) var.format.string <- x[[1]]
  else var.format.string <- ""
  return(var.format.string)
}

# get all variable format strings for data table
get.all.variable.format.strings <- function(ns) {
  num <- xmlSize(ns)
  for (i in 1:num) {
    var.format.string <- get.variable.format.string(ns[[i]])
    if (i==1) var.format.strings <- var.format.string
    else var.format.strings <- append(var.format.strings, var.format.string, after=length(var.format.strings))
  }
  return(var.format.strings)
}

# get data file
get.data.file <- function(data.url, var.types) {
  con <- file(data.url)
  xx <- read.csv(con, header=TRUE, colClasses=var.types)
  return(xx)
}

# read HF Data Archive file into data frame
get.hf.datafile <- function(datafile.id) {  
  # get data file name (GLOBAL)
  datafile.name <<- get.datafile.name(eml, datafile.id)

  # get URL for data file (GLOBAL)
  data.url <<- get.data.url(eml, datafile.id)

  # get node set for data table
  ns <- get.data.table.node.set(eml, datafile.id)
      
  # get variable types (GLOBAL)
  var.types <<- get.all.variable.types(ns)
      
  # get format strings (GLOBAL)
  var.format.strings <<- get.all.variable.format.strings(ns) 
  
  if (is.null(data.url)) {
    msg <- paste(datafile.id, ": data file does not exist", sep="")
    warning(msg)
    xx <- NULL
  } else {
    # read data file into data frame
    xx <- get.data.file(data.url, var.types)
  }
  return(xx)
}

#----------------------------SUBSET------------------------------#

# get data frame for summary stats table
get.summary.stats.data.frame <- function(xx) {  
  # summary variable types (GLOBAL)
  sum.types <<- ""
  # sumary variable names (GLOBAL)
  sum.names <<- ""
  
  # get date & numerical variables
  count <- 0
  for (i in 1:length(xx)) {
    if (var.types[i] == "Date" | var.types[i] == "numeric" | var.format.strings[i] == "YYYY-MM-DDThh:mm") {
      count <- count + 1
      if (count == 1) {
        sum.types <<- var.types[i]
        sum.names <<- names(xx)[i]
      } else {
        sum.types <<- append(sum.types, var.types[i], length(sum.types))
        sum.names <<- append(sum.names, names(xx)[i], length(sum.names))
      }
    }
  }
  
  # subset for date & numerical variables
  if (all(sum.names == "")) {
    xx.sum <- NULL
  } else {
    xx.sum <- subset(xx, select=sum.names)
  }
  
  return(xx.sum)
}

# get data frame for time-series or scatterplot matrix
get.plot.dataframe <- function(xx, max.rows) {
  # plot variable types (GLOBAL)
  plt.types <<- ""
  # plot variable names (GLOBAL)
  plt.names <<- ""
  
  # get date & numerical variables
  count <- 0
  for (i in 1:length(xx)) {
    if (var.types[i] == "numeric" | var.types[i] == "Date") {
      count <- count + 1
      if (count == 1) {
        plt.types <<- var.types[i]
        plt.names <<- names(xx)[i]
      } else {
        plt.types <<- append(plt.types, var.types[i], length(plt.types))
        plt.names <<- append(plt.names, names(xx)[i], length(plt.names))
      }
    }
  }
  
  # subset for selected variables
  if (all(plt.names == "")) {
    xx.plt <- NULL
  } else {
    xx.plt <- subset(xx, select=plt.names)
    
    # sample rows at regular intervals if necessary
    plt.rows <- nrow(xx.plt)
    if (plt.rows > max.rows) {
      skip <- round(plt.rows/max.rows)
      index <- seq(from=1, to=plt.rows, by=skip)
      xx.plt <- xx.plt[index, , drop=FALSE]
    }
    
    # remove empty variables
    keep <- 0
    count <- 0  
    if (is.vector(xx.plt)) {
      if (all(is.na(xx.plt))) {
        xx.plt <- NULL
      }
    } else {
      for (i in 1:length(xx.plt)) {
        if (!all(is.na(xx.plt[ ,i]))) {
          count <- count + 1
          if (count == 1) {
            keep <- i
          } else {
            keep <- append(keep, i, length(keep))
          }        
        }
      }
      if (length(keep) == 1) {
        if (keep == 0) {
          xx.plt <- NULL
        }
      } else if (length(keep) > 1) {
        xx.plt <- xx.plt[ , keep]
      }
    }
  }  
  return(xx.plt)
}

#-----------------------SUMMARY STATS TABLE-------------------------#

# create summary stats table
create.summary.stats.table <- function(xx) {
  # get summary data frame   
  xx.sum <- get.summary.stats.data.frame(xx)
  
  # create output table
  xstr <- paste("Harvard Forest Data Archive ", toupper(datafile.id), "\n\n", sep="")
  xstr <- paste(xstr, "Data File = ", datafile.name, "\n\nRows = ", nrow(xx), "  Columns = ", length(xx), "\n\n", sep="")
  
  # check for variables to summarize
  if (is.null(xx.sum)) {
    msg <- paste(datafile.id, ": no date or numerical variables")
    warning(msg)
    xstr <- paste(xstr, "No date or numerical variables\n", sep="")
  } else {
    xstr <- paste(xstr, "Variable         Min         Median      Mean        Max         NAs\n", sep="")
    xstr <- paste(xstr, "------------------------------------------------------------------------\n", sep="")
  
    for (i in 1:length(xx.sum)) {
      # variable name
      xstr <- paste(xstr, substr(paste(names(xx.sum[i]), "               ", sep=""), 1, 12), sep="")
  
      # all values missing
      if (all(is.na(xx.sum[ , i]))) {
        xnas <- sum(is.na(xx.sum[ , i]))
        xstr <- paste(xstr,"                                                     ", xnas, "\n", sep="")            
      
      # datetime variable
      } else if (var.format.strings[i] == "YYYY-MM-DDThh:mm") {
        # get min & max only
        xmin <- min(xx[ , i], na.rm=TRUE)
        xmax <- max(xx[ , i], na.rm=TRUE)      
        xstr <- paste(xstr, xmin, "                    ", xmax, "\n", sep="")
    
      # get all stats  
      } else {
        xmin <- min(xx.sum[ , i], na.rm=TRUE)
        xmedian <- median(xx.sum[ , i], na.rm=TRUE)
        xmean <- mean(xx.sum[ , i], na.rm=TRUE)
        xmax <- max(xx.sum[ , i], na.rm=TRUE)
        xnas <- sum(is.na(xx.sum[ , i]))
      
        # date variable
        if (sum.types[i] == "Date") {
          xstr <- paste(xstr, "  ", xmin, "  ", xmedian, "  ", xmean, "  ", xmax, formatC(xnas,0,8,"f"), "\n", sep="")
        
          # numeric variable
        } else {
          if (xmin < -1000000 | xmax > 1000000) {
            d <- 0
          } else {
            d <- 3
          }    
          xstr <- paste(xstr,
          formatC(xmin,d,12,"f"),
          formatC(xmedian,d,12,"f"),
          formatC(xmean,d,12,"f"),
          formatC(xmax,d,12,"f"),
          formatC(xnas,0,8,"f"),
          "\n", sep="")
        }
      }
    }
  }  
  # write table to file
  textplot(capture.output(writeLines(xstr)))
}

#--------------------------TIME SERIES-----------------------------#

# create time series plot
create.time.series.plot <- function(xx) {
  # get plot data frame
  xx.plt <- get.plot.dataframe(xx, max.rows=10000)

  # check for variables to plot (at least one)
  if (is.null(xx.plt)) {
    msg <- paste(datafile.id, ": no variables to plot", sep="")
    warning(msg)
   } else {
     # max number of variables per plot
     max.var <- 4
     
     # get number of plots
     var.num <- length(xx.plt)
     if (var.num <= max.var) {
       plot.num <- 1
       plot.last <- 0
     } else {
       plot.num <- var.num %/% max.var
       plot.last <- var.num %% max.var
       if (plot.last > 0) plot.num <- plot.num + 1
     }  
    
    # create each plot
     for (i in 1:plot.num) {
       first.col <- (i-1)*max.var + 1
       if (plot.num == 1) {
         last.col <- first.col + var.num - 1
       } else if (i < plot.num | plot.last == 0) {
         last.col <- first.col + max.var - 1
       } else {
         last.col <- first.col + plot.last - 1
       }
       
       # subset for variables to plot and create time series
       if (first.col == last.col ) {
         zz <- as.data.frame(xx.plt[ , c(first.col:last.col)])
         colnames(zz) <- names(xx.plt[first.col])
       } else {
         zz <- xx.plt[ , c(first.col:last.col)]
       }    
       
       # create time series
       xx.ts <- ts(zz)
       
       # create plot
       plot.title <- paste(toupper(datafile.id), " Plot ", i, sep="")     
       plot(xx.ts, main=plot.title, col="blue")
     }
     
     # display number of plots
     xstr <- paste("\nTime-series plots = ", plot.num, "\n", sep="")
     writeLines(xstr)
  }
}

#----------------------SCATTERPLOT MATRIX--------------------------#

# create scatterplot matrix
create.scatterplot.matrix <- function(xx) {
  # get plot data frame
  xx.plt <- get.plot.dataframe(xx, max.rows=1000)
  
  # check for variables to plot (at least two)
  if (is.vector(xx.plt) | length(xx.plt) < 2) {
    msg <- paste(datafile.id, ": too few variables to plot", sep="")
    warning(msg)
  } else {
    # max number of variables to plot
    max.var <- 6
  
    # get number of plots
    var.num <- length(xx.plt)
    if (var.num <= max.var) {
      plot.num <- 1
      plot.last <- 0
    } else {
      plot.num <- var.num %/% max.var
      plot.last <- var.num %% max.var
      if (plot.last > 1) plot.num <- plot.num + 1
    }  
    
    # create each plot
    for (i in 1:plot.num) {
      first.col <- (i-1)*max.var + 1
      if (plot.num == 1) {
        last.col <- first.col + var.num - 1
      } else if (i < plot.num | plot.last == 0) {
        last.col <- first.col + max.var - 1
      # plots must contain at least 2 variables
      } else if (plot.last == 1) {
        last.col <- first.col + max.var
      } else {
        last.col <- first.col + plot.last - 1
      }
      
      # subset for variables to plot
      xx.sp <- xx.plt[ , c(first.col:last.col)]
    
      # create plot
      plot.title <- paste(toupper(datafile.id), " Plot ", i, sep="")
      pairs(xx.sp, main=plot.title, col="blue")
    }
  
    # number of plots
    xstr <- paste("\nScatterplot matrices = ", plot.num, "\n", sep="")
    writeLines(xstr)
  }
}

#---------------------PROCESS DATA FILE------------------------#

# get plot type
get.plot.type <- function(datafile.id) {
  num <- as.integer(substr(datafile.id, 7, 8))  
  if (num %in% time.series.ids) {
    plot.type <- "time-series"
  } else {
    plot.type <- "scatterplot-matrix"
  }
  return(plot.type)
}

# create preview PDF for a data file
create.datafile.preview <- function(datafile.id) {
  # read HF Data Archive file into data frame (GLOBAL)
  dd <<- get.hf.datafile(datafile.id)  

  if (is.null(dd)) {
   msg <- paste(datafile.id, ": unable to download data file", sep="")
   warning(msg)
  } else {
   # create PDF file
     pdf.file <- paste(datafile.id, ".pdf", sep="")
     pdf(pdf.file)
  
    # create summary stats table
    create.summary.stats.table(dd)
  
    # get plot type (GLOBAL)
    plot.type <<- get.plot.type(datafile.id)

    # create plot
    if (plot.type == "time-series") create.time.series.plot(dd)
    if (plot.type == "scatterplot-matrix") create.scatterplot.matrix(dd)
  
     dev.off()
     
     xstr <- paste("\n*** ", toupper(datafile.id), " completed ***\n", sep="")
     writeLines(xstr)
  }
}

#------------------------MAIN PROGRAM---------------------------#

# set datafile.num = 0 to process all data tables in a dataset

# working directory
#setwd("c:/plots")

# dataset id
dataset.id <- "hf000"
datafile.num <- 1

# read eml file
eml <- get.parsed.eml(dataset.id)
time.series.ids <- get.time.series.ids(dataset.id)

# process dataset or individual data file
if (datafile.num != 0) {
  datafile.id <- get.datafile.id(dataset.id, datafile.num)
  if (is.null(datafile.id)) {
    msg <- paste(dataset.id, " : no data table with the number ", datafile.num, sep="")
    warning(msg)
  } else {
    create.datafile.preview(datafile.id)
  }
} else {
  datafile.ids <- get.datafile.ids(dataset.id)  
  if (is.null(datafile.ids)) {
    msg <- paste(dataset.id, ": no data tables in this dataset", sep="")
    warning(msg)
  } else {
    for (i in 1:length(datafile.ids)) {
      datafile.id <- datafile.ids[i]
      create.datafile.preview(datafile.id)
    }
  }
}


############################ DONE ##############################



