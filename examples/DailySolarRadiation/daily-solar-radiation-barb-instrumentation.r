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
# 2. The DDG_LIBRARY environment variable should be set to the location
#    of the ddg-library.r file.  Can change with Sys.setenv(DDG_LIBRARY = "location of ddg-library.r")

### DDG library

ddg.library <- Sys.getenv("DDG_LIBRARY")
if (ddg.library == "") {
	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
}
source(ddg.library)
ddg.text=""

### Functions

set.global.var <- function() {
	ddg.procedure("set.global.var")
	
  # set global variables
  data.file <<- "met-daily.csv"
  ddg.data.out("set.global.var", "data.file", data.file)
  cal.file <<- "par-cal.csv"
  ddg.data.out("set.global.var", "cal.file", cal.file)
  qc.file <<- "par-qc.csv"
  ddg.data.out("set.global.var", "qc.file", qc.file)
  gf.file <<- "par-gf.csv"
  ddg.data.out("set.global.var", "gf.file", gf.file)
  
  start.date <<- "2012-01-01"
  ddg.data.out("set.global.var", "start.date", start.date)
  end.date <<- "2012-03-31"
  ddg.data.out("set.global.var", "end.date", end.date)
  variable <<- "slrt"
  ddg.data.out("set.global.var", "variable", variable)

}

read.data <- function() {
	ddg.procedure("read.data")
	
  # read data file
  zz <- read.csv(data.file)
  ddg.data.in("read.data", "data.file")
  ddg.cfile(data.file)
  ddg.data.in("read.data", data.file)
  zz$date <- as.Date(zz$date)
  all.data <<- subset(zz,zz$date>=start.date & zz$date<=end.date)
  ddg.data.in("read.data", "start.date")
  ddg.data.in("read.data", "end.date")
  # Can't pass in the value
  ddg.data.out("read.data", "all.data")
	# ddg.data.out("read.data", "all.data", all.data)
  raw.data <- all.data[c("date",variable)]
  ddg.data.in("read.data", "variable")
  names(raw.data)[names(raw.data)==variable] <- "raw"
  data.rows <<- nrow(raw.data)
  ddg.data.out("read.data", "data.rows", data.rows)

  # read parameter files
  calibration.parameters <<- read.csv("par-cal.csv")
  ddg.cfile("par-cal.csv","calibration.parameters")
  ddg.data.in("read.data", "calibration.parameters")
  quality.control.parameters <<- read.csv("par-qc.csv")
  ddg.cfile("par-qc.csv", "quality.control.parameters")
  ddg.data.in("read.data", "quality.control.parameters")
  gap.fill.parameters <<- read.csv("par-gf.csv")
  ddg.cfile("par-gf.csv", "gap.fill.parameters")
  ddg.data.in("read.data", "gap.fill.parameters")
  

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
	ddg.procedure("calibrate")
	
  # correct for sensor drift using linear interpolation
  date.start <- as.Date(calibration.parameters$start)
  ddg.data.in("calibrate", "calibration.parameters")
  date.finish <- as.Date(calibration.parameters$finish)
  days <- as.numeric(date.finish - date.start)
  daily.drift <- calibration.parameters$correction/days
  for (i in 1:data.rows) {
    if (is.na(x$raw[i])) {x$cal[i] <- NA}
    else {
      x$cal[i] <- (1.0 + (i-1) * daily.drift) * x$raw[i]
      x$cal.f[i] <- "C"
    }
  }
  ddg.data.in("calibrate", "data.rows")
  return(x)
}

quality.control <- function(x) {
	ddg.procedure("quality.control")
	
  # check for repeated values
  repeats <- quality.control.parameters$repeats
  ddg.data.in("quality.control", "quality.control.parameters")
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
  ddg.data.in("quality.control", "data.rows")
  
 return(x)
}

gap.fill <- function(x) {
	ddg.procedure("gap.fill")
	
  # estimate missing values from PAR values
  slope <- gap.fill.parameters$slope
  ddg.data.in("gap.fill", "gap.fill.parameters")
  
  # Introduced this variable to know if the all.data actually got used!
  allDataUsed = FALSE
  for (i in 1:data.rows) {
    if (x$qc.f[i]=="M" | x$qc.f[i]=="Q") {
      x$gf[i] <- slope * all.data$part[i]
	  allDataUsed = TRUE
      x$gf.f[i] <- "E"
    }
    else {x$gf[i] <- x$qc[i]}
  }
  ddg.data.in("gap.fill", "data.rows")
  if (allDataUsed) {
  	ddg.data.in("gap.fill","all.data")
  }
  
  return(x)
}

plot.data <- function(x,v) {
	ddg.procedure("plot.data")
	
  # create plot as jpeg file
  if (v=="R") name <- "raw"
  else if (v=="C") name <- "calibrated"
  else if (v=="Q") name <- "quality-controlled"
  else if (v=="G") name <- "gap-filled"

  dname <- paste(name,"-data.csv",sep="")
  jname <- paste(name,"-plot.jpeg",sep="")
  
  # Copied this from Emery's version.  This seems a bit odd.
  # By doing this, the graphs no longer show up on the display
  # so this instrumentation actually changes behavior.
  ddg.wfile.out("plot.data",jname)
  
  xmin <- x$date[1]
  xmax <- x$date[data.rows]
  ddg.data.in("plot.data", "data.rows")
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

set.global.var()
print(c("ddg.text=", ddg.text))
raw.data <- read.data()
# Can't pass the value in.
ddg.data.out("read.data", "raw.data")
plot.data(raw.data,"R")
# ddg.data("raw.data", raw.data)
ddg.data.in("plot.data","raw.data")
ddg.data("R", "R")
ddg.data.in("plot.data", "R")

print(c("ddg.text=", ddg.text))

calibrated.data <- calibrate(raw.data)
ddg.data.in("calibrate", "raw.data")
# Can't pass in value
ddg.data.out("calibrate", "calibrated.data")
plot.data(calibrated.data,"C")
#ddg.data("calibrated.data", calibrated.data)
ddg.data.in("plot.data","calibrated.data")
ddg.data("C","C")
ddg.data.in("plot.data", "C")
print(c("ddg.text=", ddg.text))

quality.controlled.data <- quality.control(calibrated.data)
ddg.data.in("quality.control","calibrated.data")
# Can't pass in value
ddg.data.out("quality.control", "quality.controlled.data")
plot.data(quality.controlled.data,"Q")
# ddg.data("quality.controlled.data", quality.controlled.data)
ddg.data.in("plot.data","quality.controlled.data")
ddg.data("Q","Q")
ddg.data.in("plot.data", "Q")
print(c("ddg.text=", ddg.text))

gap.filled.data <- gap.fill(quality.controlled.data)
ddg.data.in("gap.fill","quality.controlled.data")
# Can't pass in value
ddg.data.out("gap.fill", "gap.filled.data")
plot.data(gap.filled.data,"G")
# ddg.data("gap.filled.data", gap.filled.data)
ddg.data.in("plot.data","gap.filled.data")
ddg.data("G","G")
ddg.data.in("plot.data", "G")
print(c("ddg.text=", ddg.text))

ddg.finish("main")
print(c("ddg.text=", ddg.text))
ddg.save("daily-solar-radiation-barb-instrumentation.r")