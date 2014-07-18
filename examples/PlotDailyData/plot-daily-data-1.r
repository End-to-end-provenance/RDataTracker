#######################
### PLOT DAILY DATA ###
#######################

# Plot daily time series data
# Date field = date, format = YYYY-MM-DD
# ERB rev. 5-Feb-2014

### R packages

library(chron)
require(tcltk)
require(gWidgets)
options(guiToolkit="tcltk")

### Directories

ddg.library <- Sys.getenv("DDG_LIBRARY")
if (ddg.library == "") {
	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
}
source(ddg.library)
#setwd("c:/data/r/ddg/plot-daily-data")
#ddg.r.script.path <- paste(getwd(),"/plot-daily-data-1.r",sep="")
#ddg.path <- paste(getwd(),"/ddg",sep="")
#source("c:/data/r/lib/ddg-library.r")

### Functions

INPUT <- function(message) {
  # open dialog box for user input
  CHOICE <- NA
  w <- gbasicdialog(title=message, handler = function(h,...) CHOICE <<- svalue(input))
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
  x <- INPUT("Enter file name")
  file.name <- as.character(x)

  ddg.procedure()
  ddg.data.out(file.name)

  return(file.name)
}

get.variable <- function () {
  # get variable
  x <- INPUT("Enter variable")
  variable <- as.character(x)
  
  ddg.procedure()
  ddg.data.out(variable)

  return(variable)
}

read.data <- function(file.name) {
  xx <- read.csv(file.name)
  xx$date <- as.Date(xx$date)  
  met.data <- xx
  
  ddg.procedure()
  ddg.file(file.name)
  ddg.data.in(file.name)
  ddg.snapshot.out(met.data)

  return(met.data)
}

plot.data <- function(xx,v) {
  # create plot as jpeg file

  jname <- "plot.jpeg"
  dpfile <- paste(getwd(),"/plot.jpeg",sep="")
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
 
  # copy jpeg file to DDG directory
  
  ddg.procedure()
  ddg.data.in(met.data)
  ddg.data.in(variable)
  ddg.file.out(jname)
}

### Main Program

main <- function() {

  ddg.start("main")

  ddg.start("get.data")
  file.name <- get.file.name()
  met.data <- read.data(file.name)
  ddg.finish("get.data")

  ddg.start("create.plot")
  variable <- get.variable()
  plot.data(met.data,variable)
  ddg.finish("create.plot")

  ddg.finish("main")
}

r.script.path <- "~/Documents/Process/DataProvenance/workspace/ddg-r/examples/PlotDailyData/plot-daily-data-1.r"
ddg.run(main, 
		r.script.path,
		ddgdir = paste(dirname(r.script.path), "ddg", sep="/"))
#ddg.run(main,ddg.r.script.path,ddg.path)

