#######################
### PLOT DAILY DATA ###
#######################

# Plot daily time series data
# Record DDG in text format
# Date field = date, format = YYYY-MM-DD

### R packages

library(chron)

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

  return(x)
}

get.variable <- function () {
  # get variable
  x <- INPUT("Enter variable")
  x <- as.character(x)
  
  return(x)
}

read.data <- function(fname) {
  xx <- read.csv(fname)
  xx$date <- as.Date(xx$date)  

  return(xx)
}

plot.data <- function(xx,v) {
  # create plot as jpeg file

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

file.name <- get.filename()
ts.data <- read.data(file.name)

variable <- get.variable()
plot.data(ts.data,variable)
