#################################
### REAL-TIME 15-MIN MET DATA ###
#################################

# Plot 15-min met data from HF met station
# Record DDG in text format
# Mixed format
# ERB rev. 29-Sep-2013

### R Packages

library(chron)
require(gWidgets)
options(guiToolkit="RGtk2")

### Directories

setwd("c:/data/r/ddg/real-time-15min-met")
ddg.r.script <- paste(getwd(),"/real-time-15min-met-m.r",sep="")
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

get.input.var <- function() {
  # get variable name
  x <- INPUT("Enter variable (q=quit)")
  x <- as.character(x)

  if (x != "q") {
    ddg.procedure("get.input.var")
    ddg.data.out("get.input.var","variable",x)
  }
  
  return(x)
}

get.input.days <- function () {
  # get number of days
  x <- INPUT("Enter no. of days")
  x <- as.numeric(x)
  # limit to one year
  if (x > 365) x <- 365

  ddg.procedure("get.input.days")
  ddg.data.out("get.input.days","days",x)

  return(x)
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

  windows(15,10)
  par(mar=c(5.1,5.1,5.1,10.1))

  plot(xaxt="n",xlim,ylim,cex.main=1.7,cex.axis=1.7,cex.lab=1.7,xlab="Date",
    ylab=v,main="Harvard Forest")

  if (xrange<=30)axis.Date(1,at=seq(daterange[1],daterange[2],by="day"),format="%d-%b-%Y")
  if (xrange>30 && xrange<100)axis.Date(1,at=seq(daterange[1],daterange[2],by="week"),format="%d-%b-%Y")
  if (xrange>=100 && xrange <=1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="month"),format="%d-%b-%Y")
  if (xrange>1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="year"),format="%b-%Y")

  lines(zz[c("dt",v)],lwd=2,col="blue")

  # save to jpeg file  
  ddg.procedure("plot.data")
  ddg.data.in("plot.data","final-data.csv")
  ddg.data.in("plot.data","variable")
  ddg.data.in("plot.data","days")
  ddg.snapshot.out("plot.data","plot.jpeg","")

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

# get initial values
archive.file <- "archive-15min.csv"
current.url <- "http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/weather/metsta.dat"
  
ddg.data("archive.file",archive.file)
ddg.url("current.url",current.url)  

# get archive data
# read archive data from file
zz.col <- c("type","year","jul","hm","airt","rh","dewp","prec","slrr","parr","netr","bar","wspd","wres","wdir","wdev","gspd","s10t")
archive.data <- read.csv(archive.file,col.names=zz.col,header=FALSE)

ddg.procedure("get.archive.data")
ddg.data.in("get.archive.data","archive.file")
ddg.snapshot.out("get.archive.data","archive-data.csv",archive.data)

# get current data
# read current data from HF web server
file.in <- file(current.url)
zz.col <- c("type","year","jul","hm","airt","rh","dewp","prec","slrr","parr","netr","bar","wspd","wres","wdir","wdev","gspd","s10t")
current.data <- read.csv(file.in,col.names=zz.col,header=FALSE)

ddg.procedure("get.current.data")
ddg.data.in("get.current.data","current.url")
ddg.snapshot.out("get.current.data","current-data.csv",current.data)

# get final data
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
final.data <- zz

ddg.procedure("get.final.data")
ddg.data.in("get.final.data","archive-data.csv")
ddg.data.in("get.final.data","current-data.csv")
ddg.snapshot.out("get.final.data","final-data.csv",final.data)

# save final data
file.out <- paste(getwd(),"/final-data.csv",sep="")
write.csv(final.data,file.out,row.names=FALSE)

ddg.procedure("save.data")
ddg.data.in("save.data","final-data.csv")
ddg.file.out("save.data","final-data.csv",final.data)

# create plots as requested
input <- ""
while (input != "q") {
  input <- get.input.var()
  if (input != "q") {
    variable <- input
    days <- get.input.days()
    plot.data(final.data,variable,days)
  }
}

# save final plot to jpeg file
zz <- final.data
v <- variable
d <- days

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
  
# recreate plot for DDG  
ddg.procedure("save.plot")
ddg.data.in("save.plot","final-data.csv")
ddg.data.in("save.plot","variable")
ddg.data.in("save.plot","days")
ddg.file.out("save.plot","plot.jpeg","")

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

ddg.finish("main")
ddg.save()
