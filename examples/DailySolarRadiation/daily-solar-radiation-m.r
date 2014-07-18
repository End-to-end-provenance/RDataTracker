#############################
### DAILY SOLAR RADIATION ###
#############################

# Process daily solar radiation data
# Use input parameter files
# Create plot files
# Record DDG in text format
# Date field = date, format = YYYY-MM-DD
# Mixed format
# ERB rev. 29-Sep-2013

### Directories

setwd("c:/data/r/ddg/daily-solar-radiation")
ddg.r.script <- paste(getwd(),"/daily-solar-radiation-m.r",sep="")
ddg.path <- paste(getwd(),"/ddg",sep="")
source("c:/data/r/lib/ddg-library.r")

### Functions

plot.data <- function(x,v) {
  # create plot as jpeg file
  
  if (v=="R") name <- "raw"
  else if (v=="C") name <- "calibrated"
  else if (v=="Q") name <- "quality-controlled"
  else if (v=="G") name <- "gap-filled"

  dname <- paste(name,"-data.csv",sep="")
  jname <- paste(name,"-plot.jpeg",sep="")

  dpfile <- paste(getwd(),"/",jname,sep="")
  jpeg(file=dpfile,width=800,height=500,quality=100)

  xmin <- x$date[1]
  xmax <- x$date[data.rows]
  xlim <- c(xmin,xmax)
  xrange <- xmax-xmin
  daterange <- c(as.POSIXlt(xmin),as.POSIXlt(xmax))

  ymin <- min(x$raw,x$cal,x$gc,x$gf,na.rm=TRUE)
  ymax <- max(x$raw,x$cal,x$gc,x$gf,na.rm=TRUE)
  ylim <- c(ymin,ymax)

  par(mar=c(5.1,5.1,5.1,10.1))

  plot(xaxt="n",xlim,ylim,cex.main=1.7,cex.axis=1.7,cex.lab=1.7,xlab="Date",
    ylab="Daily Total Solar Radiation (MJ/m2)")

  if (xrange<=30)axis.Date(1,at=seq(daterange[1],daterange[2],by="day"),format="%d-%b-%Y")
  else if (xrange>30 && xrange<100)axis.Date(1,at=seq(daterange[1],daterange[2],by="week"),format="%d-%b-%Y")
  else if (xrange>=100 && xrange <=1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="month"),format="%d-%b-%Y")
  else if (xrange>1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="year"),format="%b-%Y")

  good <- subset(x,x$qc.f=="")
  ques <- subset(x,x$qc.f=="Q")

  mea <- subset(x,x$gf.f=="")
  mod <- subset(x,x$gf.f=="E")

  if (v=="R") {
    title(main="Raw Data")
    points(x$date,x$raw,lwd=2,col="black")
  }
  else if (v=="C") {
    title(main="Calibrated Data")
    points(x$date,x$raw,lwd=2,col="black")
    points(x$date,x$cal,lwd=2,col="blue")
  }
  else if (v=="Q") {
    title(main="Quality Controlled Data")
    points(good$date,good$qc,lwd=2,col="blue")
    points(ques$date,ques$qc,lwd=2,col="red")
  }
  else if (v=="G") {
    title(main="Gap Filled Data")
    points(mea$date,mea$gf,lwd=2,col="blue")
    points(mod$date,mod$gf,lwd=2,col="green")
  }

  labs <- c("Raw","Calibrated","QC Check","Modeled")
  cols <- c("black","blue","red","green")
  par(xpd=TRUE)
  legend(xmax+xrange/15,ymax,labs,cols,cex=1.0)
  dev.off()

  # recreate plot for DDG

  ddg.procedure("plot.data")
  ddg.data.in("plot.data",dname)
  ddg.file.out("plot.data",jname,"") 

  xmin <- x$date[1]
  xmax <- x$date[data.rows]
  xlim <- c(xmin,xmax)
  xrange <- xmax-xmin
  daterange <- c(as.POSIXlt(xmin),as.POSIXlt(xmax))

  ymin <- min(x$raw,x$cal,x$gc,x$gf,na.rm=TRUE)
  ymax <- max(x$raw,x$cal,x$gc,x$gf,na.rm=TRUE)
  ylim <- c(ymin,ymax)

  par(mar=c(5.1,5.1,5.1,10.1))

  plot(xaxt="n",xlim,ylim,cex.main=1.7,cex.axis=1.7,cex.lab=1.7,xlab="Date",
    ylab="Daily Total Solar Radiation (MJ/m2)")

  if (xrange<=30)axis.Date(1,at=seq(daterange[1],daterange[2],by="day"),format="%d-%b-%Y")
  else if (xrange>30 && xrange<100)axis.Date(1,at=seq(daterange[1],daterange[2],by="week"),format="%d-%b-%Y")
  else if (xrange>=100 && xrange <=1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="month"),format="%d-%b-%Y")
  else if (xrange>1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="year"),format="%b-%Y")

  good <- subset(x,x$qc.f=="")
  ques <- subset(x,x$qc.f=="Q")

  mea <- subset(x,x$gf.f=="")
  mod <- subset(x,x$gf.f=="E")

  if (v=="R") {
    title(main="Raw Data")
    points(x$date,x$raw,lwd=2,col="black")
  }
  else if (v=="C") {
    title(main="Calibrated Data")
    points(x$date,x$raw,lwd=2,col="black")
    points(x$date,x$cal,lwd=2,col="blue")
  }
  else if (v=="Q") {
    title(main="Quality Controlled Data")
    points(good$date,good$qc,lwd=2,col="blue")
    points(ques$date,ques$qc,lwd=2,col="red")
  }
  else if (v=="G") {
    title(main="Gap Filled Data")
    points(mea$date,mea$gf,lwd=2,col="blue")
    points(mod$date,mod$gf,lwd=2,col="green")
  }

  labs <- c("Raw","Calibrated","QC Check","Modeled")
  cols <- c("black","blue","red","green")
  par(xpd=TRUE)
  legend(xmax+xrange/15,ymax,labs,cols,cex=1.0)
  dev.off()
}

### Main Program

ddg.start("main")

# get initial values
data.file <- "met-daily.csv"
cal.file <- "par-cal.csv"
qc.file <- "par-qc.csv"
gf.file <- "par-gf.csv"
start.date <- "2012-01-01"
end.date <- "2012-03-31"
variable <- "slrt"

ddg.data("data.file",data.file)
ddg.data("cal.file",cal.file)
ddg.data("qc.file",qc.file)
ddg.data("gf.file",gf.file)
ddg.data("start.date",start.date)
ddg.data("end.date",end.date)
ddg.data("variable",variable)

# read data file
zz <- read.csv(data.file)
zz$date <- as.Date(zz$date)
all.data <- subset(zz,zz$date>=start.date & zz$date<=end.date)
raw.data <- all.data[c("date",variable)]
names(raw.data)[names(raw.data)==variable] <- "raw"
data.rows <- nrow(raw.data)

# read parameter files
calibration.parameters <- read.csv(cal.file)
quality.control.parameters <- read.csv(qc.file)
gap.fill.parameters <- read.csv(gf.file)

# RawData table
raw.data$cal <- 0
raw.data$cal.f <- ""
raw.data$qc <- 0
raw.data$qc.f <- ""
raw.data$gf <- 0
raw.data$gf.f <- ""

ddg.procedure("read.data")
ddg.data.in("read.data","data.file")
ddg.data.in("read.data","cal.file")
ddg.data.in("read.data","qc.file")
ddg.data.in("read.data","gf.file")
ddg.data.in("read.data","start.date")
ddg.data.in("read.data","end.date")
ddg.data.in("read.data","variable")
ddg.snapshot.out("read.data","raw-data.csv",raw.data)
ddg.snapshot.out("read.data","all-data.csv",all.data)  
ddg.snapshot.out("read.data","par-cal.csv",calibration.parameters)
ddg.snapshot.out("read.data","par-qc.csv",quality.control.parameters)
ddg.snapshot.out("read.data","par-gf.csv",gap.fill.parameters)

plot.data(raw.data,"R")

# calibration
# correct for sensor drift using linear interpolation
date.start <- as.Date(calibration.parameters$start)
date.finish <- as.Date(calibration.parameters$finish)
days <- as.numeric(date.finish - date.start)
daily.drift <- calibration.parameters$correction/days
calibrated.data <- raw.data
for (i in 1:data.rows) {
  if (is.na(calibrated.data$raw[i])) {calibrated.data$cal[i] <- NA}
  else {
    calibrated.data$cal[i] <- (1.0 + (i-1) * daily.drift) * calibrated.data$raw[i]
    calibrated.data$cal.f[i] <- "C"
  }
}

ddg.procedure("calibrate")
ddg.data.in("calibrate","raw-data.csv")
ddg.data.in("calibrate","par-cal.csv")
ddg.snapshot.out("calibrate","calibrated-data.csv",calibrated.data)

plot.data(calibrated.data,"C")

# quality control
# check for repeated values
repeats <- quality.control.parameters$repeats
quality.controlled.data <- calibrated.data
for (i in 1:data.rows) {
  if (is.na(quality.controlled.data$cal[i])) {
    quality.controlled.data$qc[i] <- NA
    quality.controlled.data$qc.f[i] <- "M"
  }
  else {
    quality.controlled.data$qc[i] <- quality.controlled.data$cal[i]  
    if (i >= repeats) {
      questionable <- 1
      for (j in 1:(repeats-1)) {
        if (is.na(quality.controlled.data$raw[i-j]) | quality.controlled.data$raw[i]!= quality.controlled.data$raw[i-j]) questionable <- 0           }
      if (questionable==1) {
        for (j in 1:repeats) quality.controlled.data$qc.f[i-j+1] <- "Q"
      }
    }
  }
}

ddg.procedure("quality.control")
ddg.data.in("quality.control","calibrated-data.csv")
ddg.data.in("quality.control","par-qc.csv")
ddg.snapshot.out("quality.control","quality-controlled-data.csv",quality.controlled.data)

plot.data(quality.controlled.data,"Q")

# gap filling
# estimate missing values from PAR values
slope <- gap.fill.parameters$slope
gap.filled.data <- quality.controlled.data
for (i in 1:data.rows) {
  if (gap.filled.data$qc.f[i]=="M" | gap.filled.data$qc.f[i]=="Q") {
    gap.filled.data$gf[i] <- slope * all.data$part[i]
    gap.filled.data$gf.f[i] <- "E"
  }
  else {gap.filled.data$gf[i] <- gap.filled.data$qc[i]}
}

ddg.procedure("gap.fill")
ddg.data.in("gap.fill","quality-controlled-data.csv")
ddg.data.in("gap.fill","par-gf.csv")
ddg.data.in("gap.fill","all-data.csv")
ddg.snapshot.out("gap.fill","gap-filled-data.csv",gap.filled.data)

plot.data(gap.filled.data,"G")

# write result
file.out <- paste(getwd(),"/processed-data.csv",sep="")
write.csv(gap.filled.data,file.out,row.names=FALSE)

ddg.procedure("write.result")
ddg.data.in("write.result","gap-filled-data.csv")
ddg.file.out("write.result","processed-data.csv",gap.filled.data)

ddg.finish("main")
ddg.save()
