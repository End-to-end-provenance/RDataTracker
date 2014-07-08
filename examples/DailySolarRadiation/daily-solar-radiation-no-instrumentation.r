#############################
### DAILY SOLAR RADIATION ###
#############################

# Process daily solar radiation data
# Use input parameter files
# Create plot files
# Record DDG in text format
# Date field = date, format = YYYY-MM-DD
# ERB rev. 9-July-2013

# Notes to run this.  
# 1. The working directory must be the one containing the data files. 
#    Can change with setwd("directory")

### Functions

set.global.var <- function() {
  # Data preparation (Alper)
  # set global variables
  data.file <<- "met-daily.csv"
  cal.file <<- "par-cal.csv"
  qc.file <<- "par-qc.csv"
  gf.file <<- "par-gf.csv"
  
  start.date <<- "2012-01-01"
  end.date <<- "2012-03-31"
  variable <<- "slrt"

}

read.data <- function() {
  # read data file
  # Data moving (Alper)
  zz <- read.csv(data.file)
  
  # Data preparation (Alper)
  zz$date <- as.Date(zz$date)
  
  # Data organization (Alper)
  all.data <<- subset(zz,zz$date>=start.date & zz$date<=end.date)
  raw.data <- all.data[c("date",variable)]
  
  # Data preparation (Alper)
  names(raw.data)[names(raw.data)==variable] <- "raw"
  data.rows <<- nrow(raw.data)

  # read parameter files
  calibration.parameters <<- read.csv("par-cal.csv")
  quality.control.parameters <<- read.csv("par-qc.csv")
  gap.fill.parameters <<- read.csv("par-gf.csv")
  

  # RawData table
  raw.data$cal <- 0
  raw.data$cal.f <- ""
  raw.data$qc <- 0
  raw.data$qc.f <- ""
  raw.data$gf <- 0
  raw.data$gf.f <- ""

  return(raw.data)
}

calibrate <- function(x) {
  # correct for sensor drift using linear interpolation
  # Data preparation (Alper)
  date.start <- as.Date(calibration.parameters$start)
  date.finish <- as.Date(calibration.parameters$finish)
  days <- as.numeric(date.finish - date.start)
  daily.drift <- calibration.parameters$correction/days
  
  # Data analysis (Alper)
  # Transform (Bowers):  calibrated values are derived from daily.drift and raw data
  for (i in 1:data.rows) {
    if (is.na(x$raw[i])) {x$cal[i] <- NA}
    else {
      x$cal[i] <- (1.0 + (i-1) * daily.drift) * x$raw[i]
      x$cal.f[i] <- "C"
    }
  }

  return(x)
}

quality.control <- function(x) {
  # check for repeated values
  # Data preparation (Alper)
  repeats <- quality.control.parameters$repeats
  
  # Data analysis (Alper)
  # Filter (Bowers) - qualtiy controlled data is derived from calibrated data and depends on repeats
  for (i in 1:data.rows) {
    if (is.na(x$cal[i])) {
      x$qc[i] <- NA
      x$qc.f[i] <- "M"
    }
    else {
      x$qc[i] <- x$cal[i]  
      if (i >= repeats) {
        questionable <- 1
        for (j in 1:(repeats-1)) {
          if (is.na(x$raw[i-j]) | x$raw[i]!=x$raw[i-j]) questionable <- 0           }
        if (questionable==1) {
          for (j in 1:repeats) x$qc.f[i-j+1] <- "Q"
        }
      }
    }
  }

 return(x)
}

gap.fill <- function(x) {
  # estimate missing values from PAR values
  # Data preparation (Alper)
  slope <- gap.fill.parameters$slope
  
  # Data analysis (Alper)
  # Transform (Bowers) - gap filled data is derived from quality control data, slope, and all data
  for (i in 1:data.rows) {
    if (x$qc.f[i]=="M" | x$qc.f[i]=="Q") {
      x$gf[i] <- slope * all.data$part[i]
      x$gf.f[i] <- "E"
    }
    else {x$gf[i] <- x$qc[i]}
  }

  return(x)
}

plot.data <- function(x,v) {
  # create plot as jpeg file
  # Data preparation (Alper)
  if (v=="R") name <- "raw"
  else if (v=="C") name <- "calibrated"
  else if (v=="Q") name <- "quality-controlled"
  else if (v=="G") name <- "gap-filled"

  dname <- paste(name,"-data.csv",sep="")
  jname <- paste(name,"-plot.jpeg",sep="")
  
  xmin <- x$date[1]
  xmax <- x$date[data.rows]
  xlim <- c(xmin,xmax)
  xrange <- xmax-xmin
  daterange <- c(as.POSIXlt(xmin),as.POSIXlt(xmax))

  ymin <- min(x$raw,x$cal,x$gc,x$gf,na.rm=TRUE)
  ymax <- max(x$raw,x$cal,x$gc,x$gf,na.rm=TRUE)
  ylim <- c(ymin,ymax)

  par(mar=c(5.1,5.1,5.1,10.1))

  # Data visualization (Alper)
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
  # Need to comment out the following line so CXXR does not crash.
  # dev.off()  
}

### Main Program

# Need to start X11 to keep CXXR from crashing
X11()

set.global.var()
raw.data <- read.data()
plot.data(raw.data,"R")

calibrated.data <- calibrate(raw.data)
plot.data(calibrated.data,"C")

quality.controlled.data <- quality.control(calibrated.data)
plot.data(quality.controlled.data,"Q")

gap.filled.data <- gap.fill(quality.controlled.data)
plot.data(gap.filled.data,"G")

