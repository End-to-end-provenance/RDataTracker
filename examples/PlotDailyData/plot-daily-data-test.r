#######################
### PLOT DAILY DATA ###
#######################

# Plot daily time series data
# Record DDG in text format
# Date field = date, format = YYYY-MM-DD
# Mixed format
# ERB rev. 29-Sep-2013

# Tests the automatic capturing of output plots. 

# Test by @Luis Perez

### R packages

library(chron)
require(tcltk)
require(gWidgets)
options(guiToolkit="tcltk")

### Directories

# setwd("c:/data/r/ddg/plot-daily-data")
# ddg.r.script <- paste(getwd(),"/plot-daily-data-m.r",sep="")
# ddg.path <- paste(getwd(),"/ddg",sep="")
# source("c:/data/r/lib/ddg-library.r")

library(RDataTracker)

## Directories
testDir <- "D:/Users/Luis/Documents/Harvard School Work/Summer 2014/RDataTracker/examples/PlotDailyData/"
setwd(testDir)

ddg.r.script <- paste(getwd(),"/plot-daily-data-m.r",sep="")
ddg.path <- paste(getwd(),"/ddg",sep="")

ddg.init(r.script.path, ddg.path, enable.consoel=TRUE)

### Functions


INPUT <- function(message) {
  # open dialog box for user input
  CHOICE <- NA
  w <- gbasicdialog(title=message, handler = function(h,...) 
    CHOICE <<- svalue(input))
  input <- gedit("", initial.msg="", cont=w, width=20)
  addHandlerChanged(input, handler=function (h,...) {
    CHOICE <<- svalue(input)
    dispose(w)
  })
  visible(w, set=TRUE)
  
  return(CHOICE)
}

get.file.name <- function() {
  # get file name
  message <- "Enter file name"
  
  x <- INPUT(message)
  file.name <- as.character(x)
  
  return(file.name)
}

get.variable <- function () {
  # get variable
  message <- "Enter file name"
  
  x <- INPUT(message)
  variable <- as.character(x)
  
  return(variable)
}

read.data <- function(file.name) {
  xx <- read.csv(file.name)
  xx$date <- as.Date(xx$date)
  met.data <- xx
  
  return(met.data)
}

plot.data <- function(xx,v) {
  # create plot as jpeg file
  
  jname <- "/plot.jpeg"
  dpfile <- paste(getwd(),jname,sep="")
  jpeg(file=dpfile,width=800,height=500,quality=100)
  
  rows <- nrow(xx)
  xmin <- xx$date[1]
  xmax <- xx$date[rows]
  xlim <- c(xmin,xmax)
  xrange <- xmax-xmin
  daterange <- c(as.POSIXlt(xmin),as.POSIXlt(xmax))
  
  vv <- xx[,v]
  ymin <- min(vv,na.rm=TRUE)
  ymax <- max(vv,na.rm=TRUE)
  ylim <- c(ymin,ymax)
  yrange <- ymax-ymin
  
  par(mar=c(5.1,5.1,5.1,10.1))
  
  plot(xaxt="n",xlim,ylim,cex.main=1.7,cex.axis=1.7,cex.lab=1.7,xlab="Date",
       ylab=v,main="Harvard Forest")
  
  if (xrange<=30)axis.Date(1,at=seq(daterange[1],daterange[2],by="day"),format="%d-%b-%Y")
  if (xrange>30 && xrange<100)axis.Date(1,at=seq(daterange[1],daterange[2],by="week"),format="%d-%b-%Y")
  if (xrange>=100 && xrange <=1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="month"),format="%d-%b-%Y")
  if (xrange>1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="year"),format="%b-%Y")
  
  lines(xx[c("date",v)],lwd=2,col="blue")
  dev.off()
  
  
}

### Main Program



file.name <- get.file.name()
met.data <- read.data(file.name)
variable <- get.variable()


plot.data(met.data,variable)

ddg.save()

