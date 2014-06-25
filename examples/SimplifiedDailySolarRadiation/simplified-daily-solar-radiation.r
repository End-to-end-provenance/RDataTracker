#############################
### DAILY SOLAR RADIATION ###
#############################

# Process daily solar radiation data
# Use input parameter files
# Create plot files
# Record DDG in text format
# Date field = date, format = YYYY-MM-DD
# ERB rev. 22-Nov-2013

### Directories

ddg.library <- Sys.getenv("DDG_LIBRARY")
if (ddg.library == "") {
	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
}
source(ddg.library)
ddg.init("simplified-daily-solar-radiation.r", "ddg", TRUE)

### Functions

read.data <- function() {
  # get initial values
  data.file <<- "met-daily.csv"
  cal.file <<- "par-cal.csv"
  qc.file <<- "par-qc.csv"
  gf.file <<- "par-gf.csv"
  start.date <<- "2012-01-01"
  end.date <<- "2012-03-31"
  variable <<- "slrt"
  
  # read data file
  zz <- read.csv(data.file)
  zz$date <- as.Date(zz$date)
  all.data <<- subset(zz,zz$date>=start.date & zz$date<=end.date)
  raw.data <<- all.data[c("date",variable)]
  names(raw.data)[names(raw.data)==variable] <<- "raw"
  data.rows <<- nrow(raw.data)

  # read parameter files
  calibration.parameters <<- read.csv(cal.file)
  quality.control.parameters <<- read.csv(qc.file)
  gap.fill.parameters <<- read.csv(gf.file)

  # RawData data frame
  raw.data$cal <<- 0
  raw.data$cal.f <<- ""
  raw.data$qc <<- 0
  raw.data$qc.f <<- ""
  raw.data$gf <<- 0
  raw.data$gf.f <<- ""

  ddg.procedure("read.data")
  ddg.file(data.file)
  #ddg.file(cal.file)
  #ddg.file(qc.file)
  #ddg.file(gf.file)
  ddg.data("start.date",start.date)
  ddg.data("end.date",end.date)
  ddg.data("variable",variable)
  ddg.data.in(data.file, "read.data")
  #ddg.data.in("read.data",cal.file)
  #ddg.data.in("read.data",qc.file)
  #ddg.data.in("read.data",gf.file)
  ddg.data.in("start.date")
  ddg.data.in("end.date")
  ddg.data.in("variable")
  ddg.snapshot.out("raw-data", "csv", raw.data)
  #ddg.snapshot.out("read.data","all-data.csv",all.data)  
  #ddg.snapshot.out("read.data","par-cal.csv",calibration.parameters)
  #ddg.snapshot.out("read.data","par-qc.csv",quality.control.parameters)
  #ddg.snapshot.out("read.data","par-gf.csv",gap.fill.parameters)
 
  return(raw.data)
}

calibrate <- function(xx) {
  # correct for sensor drift using linear interpolation
  date.start <- as.Date(calibration.parameters$start)
  date.finish <- as.Date(calibration.parameters$finish)
  days <- as.numeric(date.finish - date.start)
  daily.drift <- calibration.parameters$correction/days
  for (i in 1:data.rows) {
    if (is.na(xx$raw[i])) {xx$cal[i] <- NA}
    else {
      xx$cal[i] <- (1.0 + (i-1) * daily.drift) * xx$raw[i]
      xx$cal.f[i] <- "C"
    }
  }

  ddg.procedure("calibrate")
  ddg.data.in("raw-data")
  #ddg.data.in(par-cal.csv")
  ddg.snapshot.out("calibrated-data", "csv",xx)

  return(xx)
}

quality.control <- function(xx) {
  # check for repeated values
  repeats <- quality.control.parameters$repeats
  for (i in 1:data.rows) {
    if (is.na(xx$cal[i])) {
      xx$qc[i] <- NA
      xx$qc.f[i] <- "M"
    }
    else {
      xx$qc[i] <- xx$cal[i]  
      if (i >= repeats) {
        questionable <- 1
        for (j in 1:(repeats-1)) {
          if (is.na(xx$raw[i-j]) | xx$raw[i]!=xx$raw[i-j]) questionable <- 0           }
        if (questionable==1) {
          for (j in 1:repeats) xx$qc.f[i-j+1] <- "Q"
        }
      }
    }
  }

  ddg.procedure("quality.control")
  ddg.data.in("calibrated-data")
  #ddg.data.in("par-qc.csv")
  ddg.snapshot.out("quality-controlled-data", "csv",xx)

  return(xx)
}

gap.fill <- function(xx) {
  # estimate missing values from PAR values
  slope <- gap.fill.parameters$slope
  for (i in 1:data.rows) {
    if (xx$qc.f[i]=="M" | xx$qc.f[i]=="Q") {
      xx$gf[i] <- slope * all.data$part[i]
      xx$gf.f[i] <- "E"
    }
    else {xx$gf[i] <- xx$qc[i]}
  }

  ddg.procedure("gap.fill")
  ddg.data.in("quality-controlled-data")
  #ddg.data.in("par-gf.csv")
  #ddg.data.in("all-data.csv")
  ddg.snapshot.out("gap-filled-data", "csv",xx)

  return(xx)
}

write.result <- function(fn,xx) {
  file.out <- paste(getwd(),"/",fn,sep="")
  write.csv(xx,file.out,row.names=FALSE)

  ddg.procedure("write.result")
  ddg.data.in("gap-filled-data")
  ddg.file.out(fn)
}

plot.data <- function(xx,v) {
  # create plot as jpeg file
  
  if (v=="R") name <- "raw"
  else if (v=="C") name <- "calibrated"
  else if (v=="Q") name <- "quality-controlled"
  else if (v=="G") name <- "gap-filled"

  dname <- paste(name,"-data",sep="")
  jname <- paste(name,"-plot.jpeg",sep="")

  dpfile <- paste(getwd(),"/",jname,sep="")
  jpeg(file=dpfile,width=800,height=500,quality=100)

  xmin <- xx$date[1]
  xmax <- xx$date[data.rows]
  xlim <- c(xmin,xmax)
  xrange <- xmax-xmin
  daterange <- c(as.POSIXlt(xmin),as.POSIXlt(xmax))

  ymin <- min(xx$raw,xx$cal,xx$gc,xx$gf,na.rm=TRUE)
  ymax <- max(xx$raw,xx$cal,xx$gc,xx$gf,na.rm=TRUE)
  ylim <- c(ymin,ymax)

  par(mar=c(5.1,5.1,5.1,10.1))

  plot(xaxt="n",xlim,ylim,cex.main=1.7,cex.axis=1.7,cex.lab=1.7,xlab="Date",
    ylab="Daily Total Solar Radiation (MJ/m2)")

  if (xrange<=30)axis.Date(1,at=seq(daterange[1],daterange[2],by="day"),format="%d-%b-%Y")
  else if (xrange>30 && xrange<100)axis.Date(1,at=seq(daterange[1],daterange[2],by="week"),format="%d-%b-%Y")
  else if (xrange>=100 && xrange <=1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="month"),format="%d-%b-%Y")
  else if (xrange>1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="year"),format="%b-%Y")

  good <- subset(xx,xx$qc.f=="")
  ques <- subset(xx,xx$qc.f=="Q")

  mea <- subset(xx,xx$gf.f=="")
  mod <- subset(xx,xx$gf.f=="E")

  if (v=="R") {
    title(main="Raw Data")
    points(xx$date,xx$raw,lwd=2,col="black")
  }
  else if (v=="C") {
    title(main="Calibrated Data")
    points(xx$date,xx$raw,lwd=2,col="black")
    points(xx$date,xx$cal,lwd=2,col="blue")
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

  # copy jpeg file to DDG directory
  
  ddg.procedure("plot.data")
  ddg.data.in(dname)
  ddg.file.out(jname)
}

### Main Program
ddg.start("main")

#ddg.start("get.data")

raw.data <- read.data()
#plot.data(raw.data,"R")

#ddg.finish("get.data")

ddg.start("analyze.data")

#ddg.start("calibrate.data")
calibrated.data <- calibrate(raw.data)
plot.data(calibrated.data,"C")
#ddg.finish("calibrate.data")

#ddg.start("quality.control.data")
quality.controlled.data <- quality.control(calibrated.data)
plot.data(quality.controlled.data,"Q")
#ddg.finish("quality.control.data")

#ddg.start("gap.fill.data")
gap.filled.data <- gap.fill(quality.controlled.data)
plot.data(gap.filled.data,"G")
#ddg.finish("gap.fill.data")

ddg.finish("analyze.data")

#ddg.start("write.result")
write.result("processed-data.csv",gap.filled.data)
#ddg.finish("write.result")

ddg.finish("main")

ddg.save()
