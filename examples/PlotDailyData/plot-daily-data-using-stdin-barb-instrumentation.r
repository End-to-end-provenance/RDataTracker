#######################
### PLOT DAILY DATA ###
#######################

# Plot daily time series data
# Record DDG in text format
# Date field = date, format = YYYY-MM-DD

### R packages

library(chron)

### DDG library

ddg.r.script <- paste(getwd(),"/plot-daily-data-using-stdin-barb-instrumentation.r",sep="")
ddg.path <- paste(getwd(),"/ddg",sep="")
ddg.library <- Sys.getenv("DDG_LIBRARY")
if (ddg.library == "") {
	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
}
source(ddg.library)

### Functions

INPUT <- function(message) {
	ddg.procedure()
	cat(message, " ")
	CHOICE <- readLines(n=1)
	return(CHOICE)
}

get.filename <- function() {
	ddg.start("get.filename")
	
	# get file name
	ddg.data("Enter file name", "Enter file name")
	x <- INPUT("Enter file name")
	ddg.data.out("INPUT", "x", x)
	x <- as.character(x)
	ddg.finish("get.filename")
	return(x)
}

get.variable <- function () {
	ddg.start("get.variable")
	
	# get variable
	ddg.data("Enter variable", "Enter variable")
	x <- INPUT("Enter variable")
	ddg.data.out("INPUT", "x", x)
	x <- as.character(x)
	ddg.finish("get.variable")
	return(x)
}

read.data <- function(fname) {
	ddg.procedure()
	
	xx <- read.csv(fname)
	ddg.cfile(fname)
	xx$date <- as.Date(xx$date)  
	
	return(xx)
}

plot.data <- function(xx,v) {
	ddg.procedure()
	
	# create plot as jpeg file
	
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
	
	# Bug uncovered when creating DDG!!!
	#plot(xaxt="n",xlim,ylim,cex.main=1.7,cex.axis=1.7,cex.lab=1.7,xlab="Date",
	#			ylab=variable,main="Harvard Forest")
	# ddg.data.in("", "variable")
	plot(xaxt="n",xlim,ylim,cex.main=1.7,cex.axis=1.7,cex.lab=1.7,xlab="Date",
			ylab=v,main="Harvard Forest")
	
	
	
	if (xrange<=30)axis.Date(1,at=seq(daterange[1],daterange[2],by="day"),format="%d-%b-%Y")
	if (xrange>30 && xrange<100)axis.Date(1,at=seq(daterange[1],daterange[2],by="week"),format="%d-%b-%Y")
	if (xrange>=100 && xrange <=1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="month"),format="%d-%b-%Y")
	if (xrange>1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="year"),format="%b-%Y")
	
	lines(xx[c("date",v)],lwd=2,col="blue")
	ddg.wsnapshot.out("plot.data", "plot.jpeg")
	
	dev.off()
}

### Main Program

ddg.start("main")
file.name <- get.filename()
ddg.data.out("get.filename", "file.name")

ts.data <- read.data(file.name)
ddg.wsnapshot.out("read.data", "ts.data")

variable <- get.variable()
ddg.data.out("get.variable", "variable")

plot.data(ts.data,variable)
ddg.finish("main")

ddg.save()

