#################################
### REAL-TIME 15-MIN MET DATA ###
#################################

# Plot 15-min met data from HF met station
# Record DDG in text format
# ERB rev. 26-Sep-2013

# Modified by Luis Perez 22-Jul-2014

### R Packages

library(chron)
require(gWidgets)
#options(guiToolkit="RGtk2")
options(guiToolkit="tcltk")

library(RDataTracker)

# get initial time
startTime <- Sys.time()
invisible(force(startTime))

## Directories
testDir <- "[DIR_DEFAULT]/"
setwd(testDir)

r.script.path <- paste(testDir,"real-time-15min-met-test.r",sep="")
ddgdir <- paste(testDir,"[DDG-DIR]",sep="")

ddg.init(r.script.path, ddgdir, enable.console=TRUE)

### Functions

get.initial.values <- function() {
  archive.file <<- "archive-15min.csv"
  current.url <<- "http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/weather/metsta.dat"
}

get.archive.data <- function(x) {
  # read archive data from file
  zz <- read.csv(x)
  zz.col <- c("type","year","jul","hm","airt","rh","dewp","prec","slrr","parr","netr","bar","wspd","wres","wdir","wdev","gspd","s10t")
  zz <- read.csv("archive-15min.csv",col.names=zz.col,header=FALSE)

  return(zz)
}

get.current.data <- function(x) {
  # read current data from HF web server
  file.in <- file(x)
  zz.col <- c("type","year","jul","hm","airt","rh","dewp","prec","slrr","parr","netr","bar","wspd","wres","wdir","wdev","gspd","s10t")
  zz <- read.csv(file.in,col.names=zz.col,header=FALSE)

  return(zz)
}

get.final.data <- function(archive.data,current.data) {
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
  file.out <- paste(getwd(),"/",file.name,sep="")
  write.csv(x,file.out,row.names=FALSE)
}
                                        
plot.data <- function(zz,v,d) {

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

  if (Sys.info()['sysname'] == "Darwin" || Sys.info()['sysname'] == "Linux") {
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
  ddg.procedure(lookup.ins=T)

  # dev.off()
}

save.plot <- function(zz,v,d) {
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

  # dev.off()
}

### Main Program

get.initial.values()
archive.data <- get.archive.data(archive.file)
current.data <- get.current.data(current.url)

final.data <- get.final.data(archive.data,current.data)
save.data("final-data.csv",final.data)
                     
inputs <- c("airt", "dewp", "wspd")
days <- 10
ddg.data("days")

for (input in inputs) {
  ddg.start("Loop")
  ddg.data("input")
	plot.data(final.data,input,days)
  ddg.finish("Loop")
  last <- input
}

save.plot(final.data,last,days)

# close off the outputs
for (i in 1:length(inputs)) {
  dev.off()
}

ddg.save(quit=TRUE)

# Calculate total time of execution
endTime <- Sys.time()
cat("Execution Time =", difftime(endTime, startTime,units="secs"))
