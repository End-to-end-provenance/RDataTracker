# Clean version of the script. Writting by Barbara Lerner for the 4th R Seminar for
# REU 2014. 

# Modified by Luis Perez on 28-Jul-2014

1:10

alldata <- read.csv ("http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/weather/metsta.dat", 
                     header = FALSE)
headers <- c("type","year","jul","hm","airt","rh","dewp","prec",
              "slrr","parr","netr","bar","wspd","wres","wdir",
              "wdev","gspd","s10t")
colnames(alldata) <- headers

metdata <- alldata[alldata$type == 101, ]
print("Number of rows")
nrow(alldata)
nrow(metdata)
print("Maximum type")
max(alldata$type)
max(metdata$type)

print("Number of observations with precipitation")
nrow(metdata[metdata$prec>0, ])

print("Number of observations with air temperature lower than soil temperature")
cold.obs <- metdata[metdata$airt < metdata$s10t, ]
nrow(cold.obs)

print("Number of observations where air temp was lower than soil temp and it rained")
nrow(cold.obs[cold.obs$prec > 0, ])
nrow(metdata[metdata$airt < metdata$s10t & metdata$prec>0, ])

print("Print only interesting columns showing all rows")
metdata[, c("jul", "hm", "airt", "s10t", "prec")]

print("Print only interesting rows and interesting columns")
cold.rain.obs <- cold.obs[cold.obs$prec > 0, ]
cold.rain.obs[, c("jul", "hm", "airt", "s10t", "prec")]

print("Print 5 least windy observations")
least.windy <- metdata[order(metdata$wspd), ]
least.windy[1:5, "wspd"]

print("Print 5 windiest observations")
windy <- metdata[order(-metdata$wspd), ]
windy[1:5, "wspd"]
windy <- metdata[order(metdata$wspd, decreasing=TRUE), ]
windy[1:5, "wspd"]

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
  return(max(day.metdata$airt))
}
print("High temperature for day 160")
airHigh(160)

print("Adding Rainy column")
metdata$rainy <- (metdata$prec > 0)

print("Min Julian date")
min(metdata$jul)

print("Max Julian date")
max(metdata$jul)

print("Highest temp for each date")
daterange <- min(metdata$jul):max(metdata$jul)
highs <- sapply(daterange, airHigh)
print(paste(daterange, highs))

print("Total precip for each date")
totalprecip <- function(jul) {
  day.metdata <- metdata[metdata$jul == jul, ]
  return(sum(day.metdata$prec))
}
precip <- sapply(daterange, totalprecip)
print(paste(daterange, precip))

print("Number of rainy days")
rain <- precip > 0
sum(rain)

print("Plot high air temp against date")
plot(daterange, highs)

