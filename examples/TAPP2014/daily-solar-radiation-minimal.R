# Initialize the provenance graph.
library(RDataTracker)
#source("/Users/blerner/Documents/Process/DataProvenance/github/RDataTracker/R/RDataTracker.R")
ddg.init("daily-solar-radiation-minimal.R", "ddg", enable.console=TRUE)

data.file <- "met-daily.csv"
start.date <- "2012-01-01"
end.date <- "2012-03-31"
variable <- "slrt"
raw.data <- read.data(data.file, start.date, end.date, variable)
plot.data(raw.data,"R")

calibration.parameters <- read.csv("par-cal.csv")
calibrated.data <- calibrate(raw.data, calibration.parameters)
plot.data(calibrated.data,"C")

quality.control.parameters <- read.csv("par-qc.csv")
quality.controlled.data <- quality.control(calibrated.data, quality.control.parameters)
plot.data(quality.controlled.data,"Q")

gap.fill.parameters <- read.csv("par-gf.csv")
gap.filled.data <- gap.fill(quality.controlled.data, gap.fill.parameters)
plot.data(gap.filled.data,"G")
write.result(gap.filled.data)

# Save the provenance graph
ddg.save()
