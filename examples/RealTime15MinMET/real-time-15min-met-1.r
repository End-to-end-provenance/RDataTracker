#################################
### REAL-TIME 15-MIN MET DATA ###
#################################

# Plot 15-min met data from HF met station
# ERB rev. 14-Feb-2014

### R Packages

library(chron)
require(tcltk)
require(gWidgets)
options(guiToolkit="tcltk")

### Directories

#setwd("c:/data/r/ddg/real-time-15min-met")
#ddg.r.script.path <- paste(getwd(),"/real-time-15min-met.r",sep="")
#ddg.path <- paste(getwd(),"/ddg",sep="")
#source("c:/data/r/lib/ddg-library.r")

ddg.library <- Sys.getenv("DDG_LIBRARY")
if (ddg.library == "") {
	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
}
source(ddg.library)

### Functions

set.initial.values <- function() {
  # archive URL
  archive.url <<- "http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/weather/archive.dat"
  # current URL
  current.url <<- "http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/weather/metsta.dat"
  
  ddg.procedure()
  ddg.url.out(archive.url)
  ddg.url.out(current.url)
}

get.archive.data <- function() {
  # read archive data from HF web server
  file.in <- file(archive.url)
  zz.col <- c("type","year","jul","hm","airt","rh","dewp","prec","slrr","parr","netr","bar","wspd","wres","wdir","wdev","gspd","s10t")
  zz <- read.csv(file.in,col.names=zz.col,header=FALSE)
  
  archive.data <- zz
  
  ddg.procedure()
  ddg.data.in(archive.url)
  ddg.snapshot.out(archive.data)
  
  return(archive.data)
}

get.current.data <- function() {
  # read current data from HF web server
  file.in <- file(current.url)
  zz.col <- c("type","year","jul","hm","airt","rh","dewp","prec","slrr","parr","netr","bar","wspd","wres","wdir","wdev","gspd","s10t")
  zz <- read.csv(file.in,col.names=zz.col,header=FALSE)

  current.data <- zz
  
  ddg.procedure()
  ddg.data.in(current.url)
  ddg.snapshot.out(current.data)

  return(current.data)
}

get.final.data <- function(archive.data,current.data) {
  ad <- archive.data
  cd <- current.data
  
  # append current data to archive data
  xx <- rbind(ad,cd)
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
  
  ddg.procedure()
  ddg.data.in(archive.data)
  ddg.data.in(current.data)
  ddg.snapshot.out(final.data)

  return(final.data)
}

save.data <- function(final.data) {
  file.name <- "final-data.csv"
  file.out <- paste(getwd(),"/",file.name,sep="")
  write.csv(final.data,file.out,row.names=FALSE)

  ddg.procedure()
  ddg.data.in(final.data)
  ddg.file.out(file.name)
}

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
  variable <- as.character(x)

  if (variable != "q") {
    ddg.procedure()
    ddg.data.out(variable)
  }
  
  return(variable)
}

get.input.days <- function () {
  # get number of days
  x <- INPUT("Enter no. of days")
  x <- as.numeric(x)
  # limit to one month
  if (x > 30) x <- 30
  days <- x

  ddg.procedure()
  ddg.data.out(days)

  return(days)
}
                                        
plot.data <- function(final.data,variable,days,output) {
  zz <- final.data
  v <- variable
  d <- days
  
  # if file, save plot as jpeg
  if (output=="file") {
    dpfile <- paste(getwd(),"/plot.jpeg",sep="")
    jpeg(file=dpfile,width=800,height=500,quality=100)
  }

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

  if (output=="gui") {
    if (Sys.info()['sysname']=="Darwin") X11(15,10)  # mac os
    else windows(15,10)  # windows os
  }
  
  par(mar=c(5.1,5.1,5.1,10.1))

  plot(xaxt="n",xlim,ylim,cex.main=1.7,cex.axis=1.7,cex.lab=1.7,xlab="Date",
    ylab=v,main="Harvard Forest")

  if (xrange<=30)axis.Date(1,at=seq(daterange[1],daterange[2],by="day"),format="%d-%b-%Y")
  if (xrange>30 && xrange<100)axis.Date(1,at=seq(daterange[1],daterange[2],by="week"),format="%d-%b-%Y")
  if (xrange>=100 && xrange <=1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="month"),format="%d-%b-%Y")
  if (xrange>1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="year"),format="%b-%Y")

  lines(zz[c("dt",v)],lwd=2,col="blue")

  # if gui, save to PDF file in DDG
  if (output=="gui") {
    ddg.procedure()
    ddg.data.in(final.data)
    ddg.data.in(variable)
    ddg.data.in(days)
    ddg.snapshot.out("plot",fext="pdf")
  }
  
  # if file, copy jpeg file to DDG directory
  if (output=="file") {
    dev.off()
    ddg.procedure()
    ddg.data.in(final.data)
    ddg.data.in(variable)
    ddg.data.in(days)
    ddg.file.out("plot.jpeg")
  }
}

### Main Program

main <- function() {
  
  ddg.start("main")

  ddg.start("get.data")

  set.initial.values()
  archive.data <<- get.archive.data()
  current.data <<- get.current.data()
  final.data <<- get.final.data(archive.data,current.data)
  save.data(final.data)
                     
  ddg.finish("get.data")

  ddg.start("create.plots")

  input <- ""

  while (input != "q") {
    ddg.start("create.plot")

    input <- get.input.var()
    if (input != "q") {
      variable <- input
      days <- get.input.days()
      plot.data(final.data,variable,days,"gui")
    }

    ddg.finish("create.plot")
  }

  ddg.finish("create.plots")

  ddg.start("save.plot")
  plot.data(final.data,variable,days,"file")
  ddg.finish("save.plot")

  ddg.finish("main")
}

ddg.run(main,
		"/Users/barbaralerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/RealTime15MinMET/real-time-15min-met.r",
		"/Users/barbaralerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/RealTime15MinMET/ddg")

