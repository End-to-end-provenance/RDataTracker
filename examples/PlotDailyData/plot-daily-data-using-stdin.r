#######################
### PLOT DAILY DATA ###
#######################

# Plot daily time series data
# Record DDG in text format
# Date field = date, format = YYYY-MM-DD

### R packages

library(chron)

### DDG library

ddg.r.script <- paste(getwd(),"/plot-daily-data-using-stdin.r",sep="")
ddg.path <- paste(getwd(),"/ddg",sep="")
ddg.library <- Sys.getenv("DDG_LIBRARY")
if (ddg.library == "") {
	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
}
source(ddg.library)

### Functions

INPUT <- function(message) {
	cat(message, " ")
	CHOICE <- readLines(n=1)
  return(CHOICE)
}

get.filename <- function() {
  # get file name
  x <- INPUT("Enter file name")
  x <- as.character(x)

  ddg.procedure("get.filename")
  ddg.data.out("get.filename","filename",x)

  return(x)
}

get.variable <- function () {
  # get variable
  x <- INPUT("Enter variable")
  x <- as.character(x)
  
  ddg.procedure("get.variable")
  ddg.data.out("get.variable","variable",x)

  return(x)
}

read.data <- function(fname) {
  xx <- read.csv(fname)
  xx$date <- as.Date(xx$date)  

  ddg.procedure("read.data")
  ddg.data.in("read.data","filename")
  
  # Barb change.  I get the error:  
	# Error in charToDate(x) : 
	#   character string is not in a standard unambiguous format
	#ddg.wfile.out("read.data","data.csv",xx)
	ddg.wfile.out("read.data","data.csv",as.character(xx))

  return(xx)
}

plot.data <- function(xx,v) {
  # create plot as jpeg file
  ddg.procedure("plot.data")
  ddg.data.in("plot.data","data.csv")
  ddg.data.in("plot.data","variable")
  ddg.wfile.out("plot.data","plot.jpeg","")

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

  plot(xaxt="n",xlim,ylim,cex.main=1.7,cex.axis=1.7,cex.lab=1.7,xlab="Date",
    ylab=variable,main="Harvard Forest")

  if (xrange<=30)axis.Date(1,at=seq(daterange[1],daterange[2],by="day"),format="%d-%b-%Y")
  if (xrange>30 && xrange<100)axis.Date(1,at=seq(daterange[1],daterange[2],by="week"),format="%d-%b-%Y")
  if (xrange>=100 && xrange <=1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="month"),format="%d-%b-%Y")
  if (xrange>1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="year"),format="%b-%Y")

  lines(xx[c("date",v)],lwd=2,col="blue")
  dev.off()
}

### Main Program

ddg.start("main")

ddg.start("get.data")
print("Calling get.filename")
file.name <- get.filename()
print("Returned from get.filename")
ts.data <- read.data(file.name)
ddg.finish("get.data")

ddg.start("create.plot")
variable <- get.variable()
plot.data(ts.data,variable)
ddg.finish("create.plot")

ddg.finish("main")

# Barb change to be consistent with revised R library
#ddg.save("plot-daily-data.r")
ddg.save()

