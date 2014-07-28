# Annotated Template - Modified by Luis Perez on 28-Jul-2014


1:10
ddg.start("Script")
ddg.start("Read and Clean")
url <- "http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/weather/metsta.dat"
ddg.url("MetSta Data", url)

alldata <- read.csv (url, header = FALSE)
ddg.procedure("Read Data", ins=list("MetSta Data"),outs.data=list("alldata"))

headers <- c("type","year","jul","hm","airt","rh","dewp","prec",
              "slrr","parr","netr","bar","wspd","wres","wdir",
              "wdev","gspd","s10t")
colnames(alldata) <- headers
ddg.procedure("Add Headers", ins=list("alldata"), outs.data=list("alldata"))

metdata <- alldata[alldata$type == 101, ]
ddg.procedure("Remove Daily Data", ins=list("alldata"), outs.data=list("metdata"))
print("Number of rows")
nrow(alldata)
nrow(metdata)
print("Maximum type")
max(alldata$type)
max(metdata$type)
ddg.finish("Read and Clean")

ddg.start("Gathering Data")
print("Number of observations with precipitation")
nrow(metdata[metdata$prec>0, ])

print("Number of observations with air temperature lower than soil temperature")
cold.obs <- metdata[metdata$airt < metdata$s10t, ]
ddg.procedure("Retain Cold Observations", ins=list("metdata"), outs.data=list("cold.obs"))
nrow(cold.obs)

print("Number of observations where air temp was lower than soil temp and it rained")
nrow(cold.obs[cold.obs$prec > 0, ])
nrow(metdata[metdata$airt < metdata$s10t & metdata$prec>0, ])

print("Print only interesting columns showing all rows")
metdata[, c("jul", "hm", "airt", "s10t", "prec")]

print("Print only interesting rows and interesting columns")
cold.rain.obs <- cold.obs[cold.obs$prec > 0, ]
ddg.procedure("Cold/Rainy", ins=list("cold.obs"), outs.data=list("cold.rain.obs"))
cold.rain.obs[, c("jul", "hm", "airt", "s10t", "prec")]

print("Print 5 least windy observations")
least.windy <- metdata[order(metdata$wspd), ]
ddg.procedure("Least Windy", ins=list("metdata"), outs.data=list("least.windy"))
least.windy[1:5, "wspd"]

print("Print 5 windiest observations")
windy <- metdata[order(-metdata$wspd), ]
windy[1:5, "wspd"]
windy <- metdata[order(metdata$wspd, decreasing=TRUE), ]
windy[1:5, "wspd"]
ddg.procedure("Windy", ins=list("metdata"), outs.data=list("windy"))

print("Interesing columns for windiest observations")
windy[1:5, c("jul", "hm", "airt", "prec", "wspd")]

print("All air temperatures")
metdata$airt

print("Max air temperature")
max(metdata$airt)

print("Max temperature for day 160")
max(metdata[metdata$jul==160, "airt"])

print("Function to find highest temperature for a date")
airHigh <- function(jul) {
  day.metdata <- metdata[metdata$jul == jul, ]
  maxVal <- max(day.metdata$airt)

  return(maxVal)
}
print("High temperature for day 160")
airHigh(160)

print("Adding Rainy column")
metdata$rainy <- (metdata$prec > 0)
ddg.procedure("Add rain", ins=list("metdata"), outs.data=list("metdata"))

print("Min Julian date")
min(metdata$jul)

print("Max Julian date")
max(metdata$jul)

print("Highest temp for each date")
daterange <- min(metdata$jul):max(metdata$jul)
ddg.procedure("Create Data Range", ins=list("metdata"), outs.data=list("daterange"))
highs <- sapply(daterange, airHigh)
ddg.procedure("Calculate Higs", ins=list("daterange","metdata"),outs.data=list("highs"))
print(paste(daterange, highs))

print("Total precip for each date")
totalprecip <- function(jul) {
  day.metdata <- metdata[metdata$jul == jul, ]
  return(sum(day.metdata$prec))
}
precip <- sapply(daterange, totalprecip)
ddg.procedure("Precipitation Data", ins=list("daterange","metdata"), outs.data=list("precip"))
print(paste(daterange, precip))

print("Number of rainy days")
rain <- precip > 0
rainy <- sum(rain)
ddg.procedure("Rainy Days", ins=list("precip"), outs.data=list("rainy"))
ddg.finish("Gathering Data")

ddg.start("Plotting")
print("Plot high air temp against date")
plot(daterange, highs)
ddg.procedure("Produce Plot", ins=list("daterange","highs"), outs.graphic="High Air vs Data")
ddg.finish("Plotting")
ddg.finish("Script")
