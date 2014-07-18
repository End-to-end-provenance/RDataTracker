#################################
### REAL-TIME 15-MIN MET DATA ###
#################################

# Plot 15-min met data from HF met station
# Record DDG in text format
# ERB rev. 26-Sep-2013

### R Packages

library(chron)
require(gWidgets)
#options(guiToolkit="RGtk2")
options(guiToolkit="tcltk")

### DDG library

ddg.r.script <- paste(getwd(),"/real-time-15min-met-barb-instrumentation.r",sep="")
ddg.path <- paste(getwd(),"/ddg",sep="")
ddg.library <- Sys.getenv("DDG_LIBRARY")
if (ddg.library == "") {
	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
}
source(ddg.library)

### Functions

get.initial.values <- function() {
	ddg.procedure()
	archive.file <<- "archive-15min.csv"
	ddg.data("archive.file")
	ddg.data.out("get.initial.values", "archive.file")
	current.url <<- "http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/weather/metsta.dat"
	ddg.data("current.url")
	ddg.data.out("get.initial.values", "current.url")
}

get.archive.data <- function(x) {
	ddg.procedure()
	
	# read archive data from file
	zz <- read.csv(x)
	ddg.file(x)
	ddg.data.in("get.archive.data", x)
	zz.col <- c("type","year","jul","hm","airt","rh","dewp","prec","slrr","parr","netr","bar","wspd","wres","wdir","wdev","gspd","s10t")
	zz <- read.csv("archive-15min.csv",col.names=zz.col,header=FALSE)
	ddg.file("archive-15min.csv")
	ddg.data.in("get.archive.data","archive-15min.csv")
	
	return(zz)
}

get.current.data <- function(x) {
	ddg.procedure()
	
	# read current data from HF web server
	file.in <- file(x)
	zz.col <- c("type","year","jul","hm","airt","rh","dewp","prec","slrr","parr","netr","bar","wspd","wres","wdir","wdev","gspd","s10t")
	zz <- read.csv(file.in,col.names=zz.col,header=FALSE)
	ddg.url(x)
	ddg.data.in("get.current.data", x)
	
	return(zz)
}

get.final.data <- function(archive.data,current.data) {
	ddg.procedure()
	
	# append current data to archive data
	xx <- rbind(archive.data,current.data)
	# select 15 minute data
	i <- which(xx[,1]=="101")
	zz <- xx[i,]
	# add date column
	zz$date <- paste(zz$year,"-",format(strptime(zz$jul, format="%j"),format="%m-%d"),sep="")
	# add time column
	hour <- zz$hm %/% 100
	min <- zz$hm %% 100
	zz$time <- paste(sprintf("%02d",hour),":",sprintf("%02d",min),":00",sep="")
	# replace 24:00:00 with 00:00:00
	i <- which(zz$time=="24:00:00")
	zz$date[i] <- as.character(as.Date(zz$date[i])+1)
	zz$time[i] <- "00:00:00"
	# create datetime using chron
	zz$dt <- chron(dates=zz$date,times=zz$time,format=c(dates="y-m-d",times="h:m:s"))
	
	return(zz)
}

save.data <- function(file.name,x) {
	ddg.procedure()
	
	file.out <- paste(getwd(),"/",file.name,sep="")
	write.csv(x,file.out,row.names=FALSE)
	ddg.file.out(dname=file.name, data=x)
}

INPUT <- function(message) {
	ddg.procedure()
	# open dialog box for user input
	CHOICE <- NA
	w <- gbasicdialog(title=message, 
			handler = function(h,...) {
				CHOICE <<- svalue(input)
				ddg.data("CHOICE")
			})
	input <- gedit("", initial.msg="", cont=w, width=20)
	addHandlerChanged(input, handler=function (h,...) {
				CHOICE <<- svalue(input)
				ddg.data("CHOICE")
				dispose(w)
			})
	visible(w, set=TRUE)
	return(CHOICE)
}

get.input.var <- function() {
	ddg.start()
	# get variable name
	ddg.data("Enter variable.  Enter q to quit.")
	x <- INPUT("Enter variable.  Enter q to quit.")
	ddg.data.out("INPUT", "x")
	ddg.procedure("as.character")
	ddg.data.in("as.character", "x")
	x <- as.character(x)
	ddg.finish()
	
	return(x)
}

get.input.days <- function () {
	ddg.start()
	# get number of days
	ddg.data("Enter no. of days")
	x <- INPUT("Enter no. of days")
	ddg.data.out("INPUT", "x")
	ddg.procedure("as.numeric")
	ddg.data.in("as.numeric", "x")
	x <- as.numeric(x)
	ddg.data.out("as.numeric", "x")
	
	# limit to one year
	if (x > 365) { 
		x <- 365
		ddg.data("x")
	}
	ddg.finish()
	
	return(x)
}

plot.data <- function(zz,v,d) {
	ddg.procedure()
	
	# create plot in gui
	rows <- nrow(zz)
	xmin <- zz$dt[rows-96*d]  
	xmax <- zz$dt[rows]
	xlim <- c(xmin,xmax)
	xrange <- xmax-xmin
	daterange <- c(xmin,xmax)
	
	vv <- zz[,v]
	ymin <- min(vv,na.rm=TRUE)
	ymax <- max(vv,na.rm=TRUE)
	ylim <- c(ymin,ymax)
	yrange <- ymax-ymin
	
	if (Sys.info()['sysname'] == "Darwin") {
		X11(15,10)
	}
	else {
		windows(15,10)
	}
	par(mar=c(5.1,5.1,5.1,10.1))
	
	plot(xaxt="n",xlim,ylim,cex.main=1.7,cex.axis=1.7,cex.lab=1.7,xlab="Date",
			ylab=v,main="Harvard Forest")
	
	if (xrange<=30)axis.Date(1,at=seq(daterange[1],daterange[2],by="day"),format="%d-%b-%Y")
	if (xrange>30 && xrange<100)axis.Date(1,at=seq(daterange[1],daterange[2],by="week"),format="%d-%b-%Y")
	if (xrange>=100 && xrange <=1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="month"),format="%d-%b-%Y")
	if (xrange>1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="year"),format="%b-%Y")
	
	lines(zz[c("dt",v)],lwd=2,col="blue")
	
	ddg.snapshot.out(dname="plot.data.pdf")
	dev.off()
}

save.plot <- function(zz,v,d) {
	ddg.procedure()
	# save final plot to jpeg file    
	dpfile <- paste(getwd(),"/plot.jpeg",sep="")
	jpeg(file=dpfile,width=800,height=500,quality=100)
	
	rows <- nrow(zz)
	xmin <- zz$dt[rows-96*d]  
	xmax <- zz$dt[rows]
	xlim <- c(xmin,xmax)
	xrange <- xmax-xmin
	daterange <- c(xmin,xmax)
	
	vv <- zz[,v]
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
	
	lines(zz[c("dt",v)],lwd=2,col="blue")
	dev.off()
	ddg.file.out(dname="plot.jpeg", data = "")
	
	rows <- nrow(zz)
	xmin <- zz$dt[rows-96*d]  
	xmax <- zz$dt[rows]
	xlim <- c(xmin,xmax)
	xrange <- xmax-xmin
	daterange <- c(xmin,xmax)
	
	vv <- zz[,v]
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
	
	lines(zz[c("dt",v)],lwd=2,col="blue")
	
	dev.off()
}

### Main Program

ddg.start("main")

get.initial.values()
archive.data <- get.archive.data(archive.file)
ddg.data.out("get.archive.data","archive.data")
current.data <- get.current.data(current.url)
ddg.data.out("get.current.data","current.data")

final.data <- get.final.data(archive.data,current.data)
ddg.data.out("get.final.data","final.data")
ddg.data("final-data.csv")
save.data("final-data.csv",final.data)

input <- ""
ddg.data("input")

ddg.start("while (input != \\\"q\\\")")
while (input != "q") {
	iteration <- paste("iteration", input)
	ddg.start(iteration)
	input <- get.input.var()
	ddg.data.out("as.character", "input")
	
	if (input != "q") {
		ddg.start("if (input != \\\"q\\\")")
		variable <- input
		ddg.data("variable")
		days <- get.input.days()
		ddg.procedure("get.input.days return")
		ddg.data.out("get.input.days return", "days")
		
		plot.data(final.data,variable,days)
		ddg.finish("if (input != \\\"q\\\")")
	}
	ddg.finish(iteration)
}
ddg.finish("while (input != \\\"q\\\")")

save.plot(final.data,variable,days)

ddg.finish("main")
ddg.save()

# Intersting things:
# - Return value from get.input.days can come from one of 2 places.  How to 
#   show what is getting set in main program?  Need a general way to represent
#   function output.  Look at DataBindingEents.  How were they used in LJil?
# - Why doesn't ddg.url have similar signature to ddg.file?
# - Where should snapshots be taken?

