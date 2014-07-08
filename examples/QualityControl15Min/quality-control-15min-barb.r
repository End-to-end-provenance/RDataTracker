#################################
### QUALITY CONTROL 15-MINUTE ###
#################################

# Quality control for 15-minute time-series data
# QC tests: maximum value, minimum value, repeated value, slope
# Create plots in gui and save as jpeg files
# Record DDG in text format
# Datetime field = datetime, format = YYYY-MM-DDTHH:MM:SS
# ERB rev. 18-Sep-2013

### R packages

library(chron)
require(tcltk)
require(gWidgets)
options(guiToolkit="tcltk")

### DDG library

ddg.r.script <- paste(getwd(),"/quality-control-15min-barb.r",sep="")
ddg.path <- paste(getwd(),"/ddg",sep="")
ddg.library <- Sys.getenv("DDG_LIBRARY")
if (ddg.library == "") {
	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
}
source(ddg.library)

### Functions

get.initial.values <- function() {
	ddg.procedure()
	data.file <<- "met-15min.csv"
	ddg.data.out("get.initial.values", "data.file")
	variable <<- "airt"
	ddg.data.out("get.initial.values", "variable")
	start.date <<- "2012-01-01"
	ddg.data.out("get.initial.values", "start.date")
	end.date <<- "2012-03-31"
	ddg.data.out("get.initial.values", "end.date")
}

read.data <- function(x) {
	ddg.procedure()
	# read data file
	zz <- read.csv(x)
	ddg.file(x)
	ddg.data.in("read.data", x)
	
	return(zz)
}

select.data <- function(zz) {  
	ddg.procedure()
	# select data for analysis
	zz$datetime <- as.character(zz$datetime)
	zz$date <- substr(zz$datetime,1,10)
	zz$time <- paste(substr(zz$datetime,12,16),":00",sep="")
	i <- which(zz$time=="24:00:00")
	zz$date[i] <- as.character(as.Date(zz$date[i])+1)
	zz$time[i] <- "00:00:00"
	zz$dt <- chron(dates=zz$date,times=zz$time,format=c(dates="y-m-d",times="h:m:s"))
	
	zz2 <- zz[,c("date","time","dt",variable)]
	ddg.data.in("select.data", "variable")
	names(zz2)[names(zz2)==variable] <- "var"  
	
	zz3 <- subset(zz2,zz2$dt>=start.date & zz2$dt<=end.date)
	ddg.data.in("select.data", "start.date")
	ddg.data.in("select.data", "end.date")
	
	return(zz3)
}

INPUT <- function(message) {
	ddg.procedure()
	
	# open dialog box for user input
	CHOICE <- NA
	w <- gbasicdialog(title=message, 
			handler = function(h,...) {
				# Can't create the edge for the input parameter.
				# There is no node corresponding to the parameter h.
				# Maybe we just shouldn't record information for 
				# closures???
				# ddg.procedure()
				CHOICE <<- svalue(input)
				# ddg.data.out("handler", "CHOICE")
			})
	input <- gedit("", initial.msg="", cont=w, width=20)
	addHandlerChanged(input, 
			handler=function (h,...) {
				# Another closure.
				#ddg.procedure()
				CHOICE <<- svalue(input)
				#ddg.data.out("handler", "CHOICE")
				dispose(w)
			})
	visible(w, set=TRUE)
	# CHOICE is just used in INPUT and the two handler functions
	# above.
	# ddg.data.in("INPUT", "CHOICE")
	return(CHOICE)
}

get.input.test <- function() {
	ddg.start()
	
	# name of test
	# When it looks up the parameter, inside ddg.procedure, it gets
	# broken into 2 parameters.  Why?  Because the tokenizing is done
	# by splitting, with () as split characters, intended to separate
	# function name from function parameters.  May need to do better
	# parsing.
	#ddg.data("Enter test ")
	#ddg.data("q=quit")
	# x <- INPUT("Enter test (q=quit)")
	
	# Need to create this data node manually since it is a literal.
	# Parameter lookup inside the ddg.procedure for INPUT can't
	# find it otherwise.  Is there some way we could tell it was
	# a literal???  Maybe the exists function???
	ddg.data("Enter test. Enter q to quit.")
	x <- INPUT("Enter test. Enter q to quit.")
	ddg.data.out("INPUT", "x")
	x <- as.character(x)
	ddg.procedure("as.character")
	ddg.data.in("as.character", "x")
	
	ddg.finish()
	return(x)
}

get.input.num <- function () {
	ddg.start()
	
	# get number
	ddg.data("Enter number")
	x <- INPUT("Enter number")
	ddg.data.out("INPUT", "x")
	x <- as.numeric(x)
	ddg.procedure("as.numeric")
	ddg.data.in("as.numeric", "x")
	
	ddg.finish()
	return(x)
}

apply.test <- function(xx,t,n) {
	ddg.procedure()
	
	xx$flag <- ""
	# maximum value test
	if (t=="max") {
		i <- which(xx$var >= n)
		xx$flag[i] <- "Q"
	}
	# minimum value test
	if (t=="min") {
		i <- which(xx$var <= n)
		xx$flag[i] <- "Q"
	}
	# slope test
	if (t=="slope") {
		for (i in 2:nrow(xx)) {
			if (!is.na(xx$var[i]) & !is.na(xx$var[i-1])) {
				difference <- abs(xx$var[i] - xx$var[i-1])
				if (difference >= n) {
					xx$flag[i] <- "Q"
					xx$flag[i-1] <- "Q"
				}
			}
		}
	}  
	# repeated value test
	
	# Barb --- Probably meant to use the parameter n, rather than the global
	# num here.
	# This one ends up being redundant, causing an error when building
	# the DDG.  ddgbuilder should probably just ignore duplicate input edges.  That
	# could also happen if the same actual argument were passed in for
	# multiple formal parameters.
	ddg.data.in("apply.test", "num")
	if (t=="repeat" & num > 1) {
		for (i in num:nrow(xx)) {
			if (!is.na(xx$var[i])) {
				questionable <- 1
				for (j in 1:(num-1)) {
					if (is.na(xx$var[i-j]) | xx$var[i]!=xx$var[i-j]) questionable <- 0          }
				if (questionable==1) {
					for (j in 1:num) xx$flag[i-j+1] <- "Q"
				} 
			}
		}  
	}
	
	return(xx)
}

plot.data <- function(xx,t,n) {
	ddg.procedure()
	
	# create plot in gui
	rows <- nrow(xx)
	xmin <- xx$dt[1]
	xmax <- xx$dt[rows]
	xlim <- c(xmin,xmax)
	xrange <- xmax-xmin
	daterange <- c(xmin,xmax)
	
	ymin <- min(xx$var,na.rm=TRUE)
	ymax <- max(xx$var,na.rm=TRUE)
	ylim <- c(ymin,ymax)
	yrange <- ymax-ymin
	
	title <- paste(variable," ",t," ",n,sep="")
	ddg.data.in("plot.data", "variable")
	
	if (Sys.info()['sysname'] == "Darwin") {
		X11(15,10)
	}
	else {
		windows(15,10)
	}
	par(mar=c(5.1,5.1,5.1,10.1))
	
	plot(xaxt="n",xlim,ylim,cex.main=1.7,cex.axis=1.7,cex.lab=1.7,xlab="Date",ylab=variable,main=title)
	
	if (xrange<=30)axis.Date(1,at=seq(daterange[1],daterange[2],by="day"),format="%d-%b-%Y")
	if (xrange>30 && xrange<100)axis.Date(1,at=seq(daterange[1],daterange[2],by="week"),format="%d-%b-%Y")
	if (xrange>=100 && xrange <=1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="month"),format="%d-%b-%Y")
	if (xrange>1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="year"),format="%b-%Y")
	
	good <- subset(xx,xx$flag=="")
	ques <- subset(xx,xx$flag=="Q")
	
	points(good$dt,good$var,lwd=2,col="blue")
	points(ques$dt,ques$var,lwd=2,col="red")
	
	labs <- c("Good","Questionable")
	cols <- c("blue","red")
	par(xpd=TRUE)
	
	xmin.date = as.Date(xx$date[1])
	xmax.date = as.Date(xx$date[rows])
	xrange.date = xmax.date-xmin.date
	legend(xmax.date+xrange.date/15,ymax,labs,cols,cex=1.0)
	
	rows <- nrow(xx)
	xmin <- xx$dt[1]
	xmax <- xx$dt[rows]
	xlim <- c(xmin,xmax)
	xrange <- xmax-xmin
	daterange <- c(xmin,xmax)
	
	ymin <- min(xx$var,na.rm=TRUE)
	ymax <- max(xx$var,na.rm=TRUE)
	ylim <- c(ymin,ymax)
	yrange <- ymax-ymin
	
	title <- paste(variable," ",t," ",n,sep="")
	
	par(mar=c(5.1,5.1,5.1,10.1))
	
	plot(xaxt="n",xlim,ylim,cex.main=1.7,cex.axis=1.7,cex.lab=1.7,xlab="Date",ylab=variable,main=title)
	
	if (xrange<=30)axis.Date(1,at=seq(daterange[1],daterange[2],by="day"),format="%d-%b-%Y")
	if (xrange>30 && xrange<100)axis.Date(1,at=seq(daterange[1],daterange[2],by="week"),format="%d-%b-%Y")
	if (xrange>=100 && xrange <=1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="month"),format="%d-%b-%Y")
	if (xrange>1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="year"),format="%b-%Y")
	
	good <- subset(xx,xx$flag=="")
	ques <- subset(xx,xx$flag=="Q")
	
	points(good$dt,good$var,lwd=2,col="blue")
	points(ques$dt,ques$var,lwd=2,col="red")
	
	labs <- c("Good","Questionable")
	cols <- c("blue","red")
	par(xpd=TRUE)
	
	xmin.date = as.Date(xx$date[1])
	xmax.date = as.Date(xx$date[rows])
	xrange.date = xmax.date-xmin.date
	legend(xmax.date+xrange.date/15,ymax,labs,cols,cex=1.0)
	
	dev.off()
}

save.data <- function(file.name,x) {
	ddg.procedure()
	
	file.out <- paste(getwd(),"/",file.name,sep="")
	write.csv(x,file.out,row.names=FALSE)
	ddg.file.out(dname=file.name,data=x)
	
}

save.plot <- function(xx,t,n) {
	ddg.procedure()
	
	# save final plot to jpeg file   
	dpfile <- paste(getwd(),"/plot.jpeg",sep="")
	jpeg(file=dpfile,width=800,height=500,quality=100)
	ddg.file.out(dname="plog.jpeg")
	
	rows <- nrow(xx)
	xmin <- xx$dt[1]
	xmax <- xx$dt[rows]
	xlim <- c(xmin,xmax)
	xrange <- xmax-xmin
	daterange <- c(xmin,xmax)
	
	ymin <- min(xx$var,na.rm=TRUE)
	ymax <- max(xx$var,na.rm=TRUE)
	ylim <- c(ymin,ymax)
	yrange <- ymax-ymin
	
	title <- paste(variable," ",t," ",n,sep="")
	ddg.data.in("save.plot", "variable")
	
	par(mar=c(5.1,5.1,5.1,10.1))
	
	plot(xaxt="n",xlim,ylim,cex.main=1.7,cex.axis=1.7,cex.lab=1.7,xlab="Date",ylab=variable,main=title)
	
	if (xrange<=30)axis.Date(1,at=seq(daterange[1],daterange[2],by="day"),format="%d-%b-%Y")
	if (xrange>30 && xrange<100)axis.Date(1,at=seq(daterange[1],daterange[2],by="week"),format="%d-%b-%Y")
	if (xrange>=100 && xrange <=1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="month"),format="%d-%b-%Y")
	if (xrange>1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="year"),format="%b-%Y")
	
	good <- subset(xx,xx$flag=="")
	ques <- subset(xx,xx$flag=="Q")
	
	points(good$dt,good$var,lwd=2,col="blue")
	points(ques$dt,ques$var,lwd=2,col="red")
	
	labs <- c("Good","Questionable")
	cols <- c("blue","red")
	par(xpd=TRUE)
	
	xmin.date = as.Date(xx$date[1])
	xmax.date = as.Date(xx$date[rows])
	xrange.date = xmax.date-xmin.date
	legend(xmax.date+xrange.date/15,ymax,labs,cols,cex=1.0)
	
	dev.off()
	
	rows <- nrow(xx)
	xmin <- xx$dt[1]
	xmax <- xx$dt[rows]
	xlim <- c(xmin,xmax)
	xrange <- xmax-xmin
	daterange <- c(xmin,xmax)
	
	ymin <- min(xx$var,na.rm=TRUE)
	ymax <- max(xx$var,na.rm=TRUE)
	ylim <- c(ymin,ymax)
	yrange <- ymax-ymin
	
	title <- paste(variable," ",t," ",n,sep="")
	
	par(mar=c(5.1,5.1,5.1,10.1))
	
	plot(xaxt="n",xlim,ylim,cex.main=1.7,cex.axis=1.7,cex.lab=1.7,xlab="Date",ylab=variable,main=title)
	
	if (xrange<=30)axis.Date(1,at=seq(daterange[1],daterange[2],by="day"),format="%d-%b-%Y")
	if (xrange>30 && xrange<100)axis.Date(1,at=seq(daterange[1],daterange[2],by="week"),format="%d-%b-%Y")
	if (xrange>=100 && xrange <=1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="month"),format="%d-%b-%Y")
	if (xrange>1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="year"),format="%b-%Y")
	
	good <- subset(xx,xx$flag=="")
	ques <- subset(xx,xx$flag=="Q")
	
	points(good$dt,good$var,lwd=2,col="blue")
	points(ques$dt,ques$var,lwd=2,col="red")
	
	labs <- c("Good","Questionable")
	cols <- c("blue","red")
	par(xpd=TRUE)
	
	xmin.date = as.Date(xx$date[1])
	xmax.date = as.Date(xx$date[rows])
	xrange.date = xmax.date-xmin.date
	legend(xmax.date+xrange.date/15,ymax,labs,cols,cex=1.0)
	
	dev.off()
}

### Main Program

ddg.start("main")
get.initial.values()
all.data <- read.data(data.file)
ddg.data.out("read.data", "all.data")

selected.data <- select.data(all.data)
ddg.data.out("select.data", "selected.data")

input <- ""
ddg.data("input")

ddg.start("while (input != 'q')")
while (input != "q") {
	loopVar <- input
	ddg.start(paste("iteration ", loopVar))
	ddg.procedure("loopVar <- input")
	ddg.data.out("loopVar <- input", "loopVar")
	
	input <- get.input.test()
	ddg.data.out("as.character", "input")
	if (input != "q") {
		ddg.start("if (input != 'q')")
		test <- input
		ddg.procedure("test <- input")
		ddg.data.in("test <- input", "input")
		ddg.data.out("test <- input", "test")
		num <- get.input.num()
		ddg.data.out("as.numeric", "num")
		flagged.data <- apply.test(selected.data,test,num)
		ddg.data.out("apply.test", "flagged.data")
		plot.data(flagged.data,test,num)
		ddg.finish("if (input != 'q')")
	} 
	ddg.finish(paste("iteration ", loopVar))
	
}
ddg.finish("while (input != 'q')")

ddg.data("flagged-data.csv")
save.data("flagged-data.csv",flagged.data)
save.plot(flagged.data,test,num)

ddg.finish("main")
ddg.save()


## NOTE: New annotations:
## Use start-finish for functions that call local functions
## Use start-finish for while loops, loop iterations, if-statements that call local functions