#######################
### PLOT DAILY DATA ###
#######################

# Plot daily time series data
# Record DDG in text format
# Date field = date, format = YYYY-MM-DD
# Mixed format
# ERB rev. 29-Sep-2013

### R packages

library(chron)
require(gWidgets)
options(guiToolkit="RGtk2")

### Directories

setwd("c:/data/r/ddg/plot-daily-data")
ddg.r.script <- paste(getwd(),"/plot-daily-data-m.r",sep="")
ddg.path <- paste(getwd(),"/ddg",sep="")
source("c:/data/r/lib/ddg-library.r")

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

### Main Program

ddg.start("main")

# get file name             
x <- INPUT("Enter file name")
file.name <- as.character(x)

ddg.procedure("get.file.name")
ddg.data.out("get.file.name","file.name",file.name)

# get variable
x <- INPUT("Enter variable")
variable <- as.character(x)
  
ddg.procedure("get.variable")
ddg.data.out("get.variable","variable",variable)

# read data
data <- read.csv(file.name)
data$date <- as.Date(data$date)  

ddg.procedure("read.data")
ddg.data.in("read.data","file.name")
ddg.snapshot.out("read.data","data.csv",data)

# create plot as jpeg file
dpfile <- paste(getwd(),"/plot.jpeg",sep="")
jpeg(file=dpfile,width=800,height=500,quality=100)

rows <- nrow(data)
xmin <- data$date[1]
xmax <- data$date[rows]
xlim <- c(xmin,xmax)
xrange <- xmax-xmin
daterange <- c(as.POSIXlt(xmin),as.POSIXlt(xmax))

vv <- data[,variable]
ymin <- min(vv,na.rm=TRUE)
ymax <- max(vv,na.rm=TRUE)
ylim <- c(ymin,ymax)
yrange <- ymax-ymin

# windows(15,10)
par(mar=c(5.1,5.1,5.1,10.1))

plot(xaxt="n",xlim,ylim,cex.main=1.7,cex.axis=1.7,cex.lab=1.7,xlab="Date",
  ylab=variable,main="Harvard Forest")

if (xrange<=30)axis.Date(1,at=seq(daterange[1],daterange[2],by="day"),format="%d-%b-%Y")
if (xrange>30 && xrange<100)axis.Date(1,at=seq(daterange[1],daterange[2],by="week"),format="%d-%b-%Y")
if (xrange>=100 && xrange <=1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="month"),format="%d-%b-%Y")
if (xrange>1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="year"),format="%b-%Y")

lines(data[c("date",variable)],lwd=2,col="blue")
dev.off()
  
# recreate plot for DDG
ddg.procedure("plot.data")
ddg.data.in("plot.data","data.csv")
ddg.data.in("plot.data","variable")
ddg.file.out("plot.data","plot.jpeg","")
  
rows <- nrow(data)
xmin <- data$date[1]
xmax <- data$date[rows]
xlim <- c(xmin,xmax)
xrange <- xmax-xmin
daterange <- c(as.POSIXlt(xmin),as.POSIXlt(xmax))

vv <- data[,variable]
ymin <- min(vv,na.rm=TRUE)
ymax <- max(vv,na.rm=TRUE)
ylim <- c(ymin,ymax)
yrange <- ymax-ymin

# windows(15,10)
par(mar=c(5.1,5.1,5.1,10.1))

plot(xaxt="n",xlim,ylim,cex.main=1.7,cex.axis=1.7,cex.lab=1.7,xlab="Date",
  ylab=variable,main="Harvard Forest")

if (xrange<=30)axis.Date(1,at=seq(daterange[1],daterange[2],by="day"),format="%d-%b-%Y")
if (xrange>30 && xrange<100)axis.Date(1,at=seq(daterange[1],daterange[2],by="week"),format="%d-%b-%Y")
if (xrange>=100 && xrange <=1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="month"),format="%d-%b-%Y")
if (xrange>1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="year"),format="%b-%Y")

lines(data[c("date",variable)],lwd=2,col="blue")
dev.off()

ddg.finish("main")
ddg.save()
