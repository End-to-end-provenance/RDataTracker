#######################
### PLOT DAILY DATA ###
#######################

# Plot daily time series data
# Record DDG in text format
# Date field = date, format = YYYY-MM-DD
# ERB rev. 22-Nov-2013

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
  x <- as.character(x)

  ddg.procedure("get.file.name")
  ddg.data.out("get.file.name","file.name",x)

  return(x)
}

get.variable <- function () {
  # get variable
  x <- INPUT("Enter variable")
  x <- as.character(x)
  
  ddg.procedure("get.variable")
  ddg.data.out("get.variable","variable",x)

  return(x)
}

read.data <- function(fn) {
  xx <- read.csv(fn)
  xx$date <- as.Date(xx$date)  

  ddg.procedure("read.data")
  ddg.file(fn)
  ddg.data.in("read.data","file.name")
  ddg.data.in("read.data",fn)
  ddg.snapshot.out("read.data","data.csv",xx)

  return(xx)
}

plot.data <- function(xx,v) {
  # create plot as jpeg file

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

  # windows(15,10)
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
  
  ddg.procedure("plot.data")
  ddg.data.in("plot.data","data.csv")
  ddg.data.in("plot.data","variable")
  ddg.file.copy.out("plot.data","plot.jpeg")
}

### Main Program

ddg.start("main")

ddg.start("get.data")
file.name <- get.file.name()
data <- read.data(file.name)
ddg.finish("get.data")

ddg.start("create.plot")
variable <- get.variable()
plot.data(data,variable)
ddg.finish("create.plot")

ddg.finish("main")

ddg.save()
