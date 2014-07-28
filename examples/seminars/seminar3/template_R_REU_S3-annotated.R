# Harvard Forest R for REUs

## Session 3, 19 June 2014**

##Introduction**

## This session will focus on temporal data, date-time stamps, line plots, scatterplots, 
## summarizing using ddply and indexing.

## Examples Using Meteorological Data**

## The Fisher Meteorological Station at Harvard Forest records air temperature, 
## relative humidity, dew point, precipitation (water equivalent of snow), global
## solar radiation, PAR radiation, net radiation, barometric pressure, scalar wind
## speed, vector wind speed, peak gust speed (1-second), vector wind direction,
## standard deviation of wind direction (wind measurements at 10 m height), and
## soil temperature (10 cm depth). Instruments are scanned once per second, and 
## 15-minute (hourly before 2005) and daily values are calculated and stored by 
## a datalogger. 

## HF Met archived data here:
## http://harvardforest.fas.harvard.edu:8080/exist/xquery/data.xq?id=hf001

## HF Met current data here:
## http://harvardforest.fas.harvard.edu/harvard-forest-weather-station

## I have downloaded the 15-min and daily files to my working directory. Both data 
## sets have two header rows, the first contains variable names and the second contains units.
## This format complicates things a little when reading the data into R; R sees the 
## second row as data and defines the structure of the data frame based on it. 
## To avoid this, we can read the data without header rows by instructing read.csv() 
## to skip the first two lines. We can then read it again with headers and assign the 
## first one to our appropriately structured data. I have not yet found a more efficient 
## way to do this - bonus points for you if you can!

## Note: The met data includes flags for missing, questionable and estimated data points. For the sake of simplicity, we will ignore these for the following examples and use all available data.

## ```{r}
#read in Fisher Met 15 min data without header rows by skipping the first two lines
ddg.start("Read Data")
minute.url <- "hf001-10-15min-m.csv"
ddg.file("minute.url", minute.url)
met.15m<-read.csv(minute.url, skip=2, header=F)

#read in same data with header rows
met.15m.h<-read.csv(minute.url, header=T)

#assign names to properly structured data
names(met.15m)<-names(met.15m.h)

#confirm that it worked!
names(met.15m)
ddg.procedure("Read Minute", ins=list("minute.url"), outs.data=list("met.15m"))

#repeat this for the daily met data, which we will use later in this session
daily.url <- "hf001-06-daily-m.csv"
ddg.file("daily.url", daily.url)
met.d<-read.csv(daily.url, skip=2, header=F)
met.d.h<-read.csv(daily.url, header=T)
names(met.d)<-names(met.d.h)
names(met.d)
ddg.procedure("Read Minute", ins=list("daily.url"), outs.data=list("met.d"))
ddg.finish("Read Data")
## ```

# **Date-Time Conversion**
# 
# Each 15-minute Fisher Met data observation is identified by a timestamp in the form YYYY-MM-DDThh:mm. The *strptime* function can be used to manipulate dates and timestamps and extract information from them. This is useful when you want to summarize your data by periods of time or plot your data as a function of time.
# 
# Lots more information on dates and time in R:
# https://www.stat.berkeley.edu/classes/s133/dates.html
# 
# ```{r}
#look at the format of our timestamps
ddg.start("Calculate Extra Data")
head(met.15m$DateTime)

#first we need to convert timestamps to character
dt<-as.character(met.15m$DateTime)
class(dt)

#using strptime(), convert timestamps to class POSIXlt, which is extractable. note that we need to specify the format of our timestamp. the default is to local time zone. if gmt, use tz="gmt". 
time<-strptime(dt, "%Y-%m-%dT%H:%M", tz="EST5EDT") 
class(time)

#now we can extract pieces of our timestamp and add it to our data frame.  we have to add 1900 to the year values in this case.
met.15m$year<-time$year + 1900
summary(met.15m$year)
ddg.procedure("Add year", ins=list("met15m"), outs.data=list("met15m"))

#we can extract other information about the date in the same manner, such as month
met.15m$month<-time$mon + 1
summary(met.15m$month)
ddg.procedure("Add month", ins=list("met15m"), outs.data=list("met15m"))

#day of the month
met.15m$dom<-time$mday
summary(met.15m$dom)
ddg.procedure("Add day of month", ins=list("met15m"), outs.data=list("met15m"))

#day of the year
met.15m$doy<-time$yday+1
summary(met.15m$doy)
ddg.procedure("Add day of year", ins=list("met15m"), outs.data=list("met15m"))

#hour of the day
met.15m$hour<-time$hour
summary(met.15m$hour)
ddg.procedure("Add hour of day", ins=list("met15m"), outs.data=list("met15m"))

#minute of the day, in 15 minute intervals
met.15m$min<-time$min
head(met.15m$min)
ddg.procedure("Add minute of day", ins=list("met15m"), outs.data=list("met15m"))

#calculate the decimal date
met.15m$decdate <- round(met.15m$doy + met.15m$hour/24 + met.15m$min/1440, 4)
head(met.15m$decdate)
ddg.procedure("Add decimal data", ins=list("met15m"), outs.data=list("met15m"))
ddg.finish("Calculate Extra Data")
# ```

# **Line Graphs**
# 
# Line Graphs are typically used for visualizing how one continuous variable (y) changes in relation to another (x). The variable represented on the x-axis is often time, but can be any ordered or continuous variable.
# 
# The met station variables can be plotted over time using basic line graphs. For these examples, we will subset the data using our new time variables to take a closer look at three days of 15-min data.
# 
# ```{r fig.width=12, fig.height=4}
#we can subset our data from within the ggplot() function. we'll look at average air temperature from a three day period, day of year 203 to 205 (July 22 to 24) in 2013. we will use this ggplot object (our subset) several times, so we can assign it a name and call it for each plot and assign the aesthetics from within the geom.

library(ggplot2)
ddg.start("Plot Minute Data")

days3<-ggplot(subset(met.15m, year==2013 & doy %in% c(203,204,205)))

days3 +
  
  geom_line(aes(decdate, AirT)) +
  
  ggtitle("15-min Average Air Temperature (degC), 2013") +
  
  xlab("day of year") +
  
  theme_bw()

ddg.procedure("Lines", ins=list("met15m"), outs.graphic="AirT vs decdata")
# ```
# 
# ```{r fig.width=12, fig.height=4}
#from the same period, plot total precipitation

days3 +
  
  geom_line(aes(decdate, Prec)) +
  
  ggtitle("15-min Total Rainfall (mm), 2013") +
  
  xlab("day of year") +
  
  theme_bw()

ddg.procedure("Lines", ins=list("met15m"), outs.graphic="Prec vs decdata")
# ```
# 
# ```{r fig.width=12, fig.height=4}
#we can also plot two lines on the same graph without converting the data frame to long format. here we want to look at air temperature and soil temperature together so we use one geom-line command for each. we can assign the variables different colors using colour= and adjust the legend labels and name using scale_colour_discrete().

days3 +
  
  geom_line(aes(decdate, AirT, colour="blue")) +
  
  geom_line(aes(decdate, S10T, colour="red")) +
  
  ggtitle("15-min Average Air and Soil Temp (degC), 2013") +
  
  xlab("day of year") +
  
  scale_colour_discrete(labels=c("Air", "Soil"), name="Temp") +
  
  theme_bw()
ddg.procedure("Lines", ins=list("met15m"), outs.graphic="(AirT and S10T) vs decdata")
# ```
# 
# ```{r fig.width=12, fig.height=4}
#we can recreate the above graph and assign different line types to the two variables using linetype= and adjust the legend labels and name using scale_linetype()

days3 +
  
  geom_line(aes(decdate, AirT, linetype="dashed")) +
  
  geom_line(aes(decdate, S10T, linetype="solid")) +
  
  ggtitle("15-min Average Air and Soil Temp (degC), 2013") +
  
  xlab("day of year") +
  
  scale_linetype(labels=c("Air", "Soil"), name="Temp") +
  
  theme_bw()
ddg.procedure("Lines", ins=list("met15m"), outs.graphic="(AirT and S10T) vs decdata")
ddg.finish("Plot Minute Data")
# ```
# * * *
# **More fun with dates**
# 
# Each daily met data observation is identified by a date variable (Date) in the format YYYY-MM-DD and a day of year variable (Jul). If we wish to extract pieces of our timestamp for subsetting or summarizing, such as year, we can treat the Date variable in the same way that we did the 15-min data timestamps; Alternatively, we can use substr() to extract parts of the date string. 
# 
# ```{r}
#print the names of the daily met data and the first few lines of the date variable to see the format

names(met.d)
head(met.d$Date)

#create a new variable for year in the daily data frame by using substr() to extract the first four characters (YYYY) from the date variable
ddg.start("Collect Daily Data")
met.d$year<-substr(met.d$Date, 1, 4)

#with summary and encoding year as a factor, we can see how many observations were made per year. notice that both 2001 and 2014 are incomplete years.

summary(as.factor(met.d$year))
# ```
# 
# ```{r fig.width=12, fig.height=4}
#plot air temp as a function of time for all years. be sure that the Date variable is read as class "Date", so that the x-axis is mapped appropriately.

met.d$Date<-as.Date(met.d$Date)
ddg.procedure("Add year", ins=list("met.d"), outs.data=list('met.d'))
ddg.finish("Collect Daily Data")

ddg.start("Plot Daily Data")

ggplot(met.d, aes(x=Date, y=AirT)) +
  
  geom_path() +
  
  xlab("year") +
  
  ylab("air temp (C)") +
  
  theme_bw()
ddg.procedure("Lines", ins=list("met.d"), outs.graphic="AirT vs Data")
# ```

# For the following examples, we will subset the data using our new year variable to take a closer look at 2010, agian subsetting from within the ggplot function and assigning the aesthetics from within geom.
# 
# It is possible to create a line graph of daily air temperature data with a shaded region representing the daily maximum and minimum temperatures using geom_ribbon(). Notice below that geom_ribbon() comes before geom_line(). This is so that the line will be drawn on top of the shaded region; the order of the commands in ggplot matter.

# ```{r fig.width=12, fig.height=4}
#plot daily average air temp from 2010. include a shaded region representing daily maximum and minimum temperatures using geom_ribbon() and set ymin and ymax equal to AirTMIn and and AirTMax. Set alpha=0.3 to make the shaded region 70% transparent.

ggplot(subset(met.d, year==2010), aes(x=Date)) +
  
  geom_ribbon(aes(ymin=AirTMin, ymax=AirTMax), alpha=0.3) +
  
  geom_line(aes(y=AirT)) +
  
  ggtitle("Daily Average Air Temperature") +
  
  xlab("month of year") +
  
  ylab("air temp (C)") +
  
  theme_bw()
ddg.procedure("Lines", ins=list("met.d"), outs.graphic="AirTMin vs AirTMax")
# ```
# * * *
# **Scatter Plots**
# 
# Scatter Plots are used for visualizing the relationship between two continuous variables, with each observation represented by a single point. many of the daily met station variables can be plotted using scatterplots. We will continue to look at data from 2010.
# 
# ```{r fig.width=12, fig.height=4}
#create a new ggplot object for daily data from 2010

days2010<-ggplot(subset(met.d, year==2010))

#create a scatterplot of average daily air temperature data from 2010

days2010 +
  
  geom_point(aes(Date, AirT)) +
  
  xlab("month of year") +
  
  ylab("air temp (C)") +
  
  ggtitle("Average Air Temperature") +
  
  theme_bw()

ddg.procedure("Scatter Plots", ins=list("met.d"), outs.graphic="AirT vs Date")
# ```
# 
# We can also use scatterplots to look at non-temporal relationships between variables, such as air and soil temperature.
# 
# ```{r fig.width=12, fig.height=4}
#create a scatterplot of average daily air temperature and average daily soil temperature from 2010. we can assign color to the points based on day of year using colour= to map the variable and scale_colour_gradientn() to assign the gradient type and colors.

days2010 +
  
  geom_point(aes(AirT, S10T, colour=Jul)) +
  
  xlab("air temp (C)") +
  
  ylab("soil temp (C)") +
  
  scale_colour_gradientn(colours=c("blue","red","blue")) +

  theme_bw()
ddg.procedure("Scatter Plots", ins=list("met.d"), outs.graphic="S10T vs AirT")
# ```
# 
# **Summarizing data**
# 
# Remember that aggregate() can summarize data and apply the same function on multiple columns. Here we can use ddply() from the *plyr* package to summarize our daily data by year and apply different functions to each variable we wish to summarize. This function subsets a data frame, applies a function and returns the results in a new data frame.
# 
# Note: An annual summary table for Fisher Met data is available, but it is useful to know how summarize data in this way, so we will do it ourselves.
# 
# ```{r}
#load the plyr package
library(plyr)

#using the ddply() function, calculate mean yearly air temp and annual sums of precip in meters.
ddg.start("Data Analysis")

mety <- ddply(met.d, "year", 
              
       summarise,
              
       AirT = mean(AirT),
              
       Prec.m = sum(Prec)/1000
              
       )
ddg.procedure("Get Meants", ins=list("met.d"), outs.data=list("mety"))
ddg.finish("Data Analysis")

mety
# ```
# * * *
# **Quick notes on indexing**
# 
# There are many ways in R to extract elements from a vector, matrix, or data frame; Indexing is a useful one. For data frames, you can use the row and column numbers to specify parts of the data you want to deal with. The notation for data frame indexing is data[rows, columns]. In the following graphing example we will ask to plot only rows 2 through 13 of our annual summary table with indexing instead of subset(). The notation will be mety[2:13, ]; we specify the rows but not which columns because we want to retain them all.
# 
# We can refer to any value, or subset of values, in a data frame using the same type of notation. 
# ```{r}
#row 12, column 2
mety[12, 2]

#row 8, all columns
mety[8, ]                            

# rows 1 to 5, all columns
mety[1:5, ]                          

#all rows, column 2
mety[ , 2]   

#all rows, minus column 2
mety[ ,-2]

#rows 1, 3, 7, and 13, column 3
mety[c(1,3,7,13), 3] 

#all rows except 1, 3, 7, and 13, column 3
mety[-c(1,3,7,13), 3]
# ```
# 
# More on the multitude of ways to select or exclude data: http://www.statmethods.net/management/subset.html
# 
# ```{r fig.width=8, fig.height=4}
#plot yearly average air temp. remember that the first and last years are not complete, so we will not include them.

ggplot(mety[2:13, ]) +
  
  geom_point(aes(year, AirT)) +
  
  theme_bw()
ddg.procedure("Scatter Plots", ins=list("mety"), outs.graphic="AirT vs year")
# ```
# 
# ```{r fig.width=8, fig.height=4}
#plot annual rainfall sums in meters using the same range of years.

ggplot(mety[2:13, ]) +
  
  geom_bar(aes(year, Prec.m), width=0.5, stat="identity") +
  
  theme_bw()
ddg.procedure("Bar Plots", ins=list("mety"), outs.graphic="AirT vs year")
ddg.finish("Plot Daily Data")
# ```
# 
# **Packages and functions presented**
# 
# Packages: ggplot2, plyr
# 
# Functions: read.csv, names, head, as.character, class,
# 
# strptime, summary, c, ggplot, subset, geom_line, aes, 
# 
# ggtile, xlab, ylab, theme_bw, scale_colour_discrete, 
# 
# scale_linetype, geom_ribbon, geom_point, 
# 
# scale_colour_gradientn, ddply, mean, sum, geom_bar
# 
# **On your own before next session**
# 
# Using the two met datasets we worked with above, create several plots that you think would be interesting to visualize. Try out the diferent ypes of plots we have covered in these sessions. For variable-level metadata, be sure to utilize the Harvard Forest Data Archive page for the Fisher Met Data, dataset HF001.

