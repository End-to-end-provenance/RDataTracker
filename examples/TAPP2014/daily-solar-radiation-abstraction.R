read.data <- function(file, startDate, endDate, var) {
	return (read.csv(file))
}

#read.data <- function(data.file, 
#		start.date, end.date, variable) {
#	ddg.start("read.data")
#	zz <- read.csv(data.file)
#	ddg.procedure(pname="read.csv", 
#			ins=list("data.file"), 
#			outs.snapshot=list("zz"))
#	
#	zz$date <- as.Date(zz$date)
#	all.data <<- subset(zz,
#			zz$date>=start.date & zz$date<=end.date)
#	ddg.procedure(pname="subset dates", 
#			ins=list("zz", "start.date", "end.date"), 
#			outs.snapshot=list("all.data"))
#	
#	raw.data <<- all.data[c("date",variable)]
#	names(raw.data)[names(raw.data)==variable] <<- "raw"
#	ddg.procedure(pname="subset variable", 
#			ins=list("all.data", "variable"), 
#			outs.snapshot=list("raw.data"))
#	ddg.finish("read.data")
#	return(raw.data)
#}


plot.data <- function(data, mode) {
	
}

calibrate <- function(data, params) {
	return(data)
}

quality.control <- function(data, params) {
	return(data)
}

gap.fill <- function(data, params) {
	return(data)
}

write.result <- function(data) {
	
}

# Initialize the provenance graph.
library(RDataTracker)
ddg.init("daily-solar-radiation-abstraction.r", "ddg", enable.console=TRUE)



data.file <- "met-daily.csv"
start.date <- "2012-01-01"
end.date <- "2012-03-31"
variable <- "slrt"
ddg.start("Read data")
raw.data <- read.data(data.file, start.date, end.date, variable)
ddg.finish("Read data")
plot.data(raw.data,"R")

ddg.start("Calibrate")
calibration.parameters <- read.csv("par-cal.csv")
calibrated.data <- calibrate(raw.data, calibration.parameters)
plot.data(calibrated.data,"C")
ddg.finish("Calibrate")

ddg.start("Apply quality control")
quality.control.parameters <- read.csv("par-qc.csv")
quality.controlled.data <- quality.control(calibrated.data, quality.control.parameters)
plot.data(quality.controlled.data,"Q")
ddg.finish("Apply quality control")

ddg.start("Gap fill")
gap.fill.parameters <- read.csv("par-gf.csv")
gap.filled.data <- gap.fill(quality.controlled.data, gap.fill.parameters)
plot.data(gap.filled.data,"G")
ddg.finish("Gap fill")

write.result(gap.filled.data)

# Save the provenance graph
ddg.save()
