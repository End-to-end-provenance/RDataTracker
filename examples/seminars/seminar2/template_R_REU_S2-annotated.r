#R REU INTRO
#SESSION 2
#LIZA NICOLL
#12 JUNE 2014

#install RCurl in the Packages window and load it
library(RCurl)

#download the URL of the data and save it as a new object. This downloads the
#data as a csv file.
ddg.start("Script")
ddg.start("Manipulate Data")
url <- "http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/data/p12/hf126/hf126-02-tree.csv"
hem_url<-getURL(url)
ddg.url("HF Data", url)
#use read.csv() to transform your URL data into a data frame.

hem<-read.csv(text=hem_url)
ddg.procedure("Read Data", ins=list("HF Data"), outs.data=list("hem"))

#display the names of the variables in our data frame. you can find
#variable-level metadata for each of these variables on the main page for hf126
#in the data archive.

names(hem)

##SUBSETTING DATA

#subset the data to isolate the variables and observations of interest. here we
#ask to retain data for 4 specific species in 2 specific plots that were alive
#in 2009. c() is used to create lists of things to retain. we also specify the
#columns that we would like to retain from the original dataset.

hk<-subset(hem,  
           species %in% c("ACRU","BELE","PIST","TSCA") & 
             plot %in% c(3,6) & 
             cond09 %in% "L", 
           select = c(plot, tree, species, dbht0, dbh09)) 
ddg.procedure("Subset", ins=list("hem"), outs.data=list("hk"))

summary(hk) #print a summary to verify subset

str(hk) #check the structure of the subset

#we know that the variables "plot" and "tree" should be factors since they
#identify specific things, but str() showed us that R reads them as integers. we
#can use as.factor() to convert them. this will be important when we want to use
#the plot number to plot the data.

hk$plot<-as.factor(hk$plot)

hk$tree<-as.factor(hk$tree)
ddg.procedure("Convert to Factors", ins=list("hk"), outs.data=list("hk"))
ddg.finish("Manipulate Data")
##PLOTTING WITH GGPLOT2
ddg.start("Plotting with GGPLOT2")
library(ggplot2)

#PLOT 1. Create box plots of the distribution of dbh per species in both plots.

ggplot(hk, aes(x=species, y=dbht0)) + 
  
  geom_boxplot() + 
  
  facet_grid(~plot) +
  
  ggtitle("DBH at T0 for Plots 3 and 6") +
  
  theme_bw()

ddg.procedure("Plot 1", ins=list("hk"), outs.graphic="DBH Box Plot")
##PLOT BREAKDOWN

#PLOT 2. Create a histogram of the distribution of dbh for all trees in both
#plots at time zero (t0). Specify binwidth to represent 5cm size classes and
#color the bars black. Notice that we are now using geom_histogram() and that
#the aesthetic (aes) only requires one variable.

ggplot(hk, aes(dbht0)) + 
  
  geom_histogram(binwidth = 5, colour="black") + 
  
  facet_grid(~ plot) +
  
  ggtitle("Histogram of DBH at T0 for Plots 3 and 6") +
  
  theme_bw()

ddg.procedure("Plot 2", ins=list("hk"), outs.graphic="DBH Histogram Plot")

#PLOT 3. Create histograms of the distribution of dbh for each species in both
#plots at time zero. Notice the change in facet_grid, requesting to facet by
#both species and plot in the form facet_grid(vertical ~ horizontal).

ggplot(hk, aes(dbht0)) + 
  
  geom_histogram(binwidth = 5, colour="black") + 
  
  facet_grid(plot ~ species) +
  
  theme_bw()

ddg.procedure("Plot 3", ins=list("hk"), outs.graphic="DBH Histogram Species Plot")
ddg.finish("Plotting with GGPLOT2")

##RESHAPING DATA
ddg.start("Reshaping Data")
#if we want to plot our data faceted by year of measurement, we will need to
#reshape our data using melt() from the reshape2 package.

library(reshape2)

#recall the variable names of our data frame

names(hk)

hk_long<-melt(hk, 
              
              id.vars=c("plot", "tree", "species"), 
              
              variable.name = "year", 
              
              value.name = "dbh",
              
              measure.vars=c("dbht0", "dbh09"))
ddg.procedure("Melt", ins=list("hk"), outs.data=list("hk_long"))

head(hk_long)

#here we might want to have the values under "year" represent the actual year.
#We can use an ifelse() statement to find and replace the values based on
#conditions we give in the form ifelse(condition, value.if.true,
#value.if.false). we also want to make sure that R sees year as a factor and not
#an integer, so we wrap our ifelse() statement in as.factor()

hk_long$year<-as.factor(ifelse(hk_long$year=="dbht0", 2003, 2009))
ddg.procedure("Change to factor", ins=list("hk_long"), outs.data=list("hk_long"))

str(hk_long)
ddg.finish("Reshaping Data")
#PLOT 4. plot a histogram of 5cm size classes for live trees in plots 3 and 6,
#including bars for both years. we can use position=position_dodge(3.5) to have
#the bars overlap slighty. this number can be adjusted to have the bars side by
#side or overlapping completely.
ddg.start("More Plots")
ggplot(hk_long, aes(dbh, fill=year)) + 
  
  geom_histogram(binwidth = 5, position=position_dodge(3.5)) + 
  
  facet_grid( ~ plot) +
  
  theme_bw()

ddg.procedure("Plot 4", ins=list("hk_long"), outs.graphic="Tree Histogram Class Plot")
##BASAL AREA

#convert the size of each plot from 90 meters squared to hectares and save it as
#a new object

size<-(90^2)*0.0001
ddg.data("size")

#calculate basal area and add the variable to our long data frame. we can do
#this by naming it using the data frame name and $ preceeding the new variable
#name, hk_long$BA.h

hk_long$BA.h<-(pi*((hk_long$dbh/200)^2))*size
ddg.procedure("Add Basal Area", ins=list("hk_long"), outs.data=list("hk_long"))

head(hk_long)

##AGGREGATE
ddg.start("Aggregate")
#create a new, summary data frame showing the sum of basal area by plot, species
#and year. The aggreagte function can be read as "aggreagte basal area by plot,
#species and year. sum the basal area for each subset. use the data frame
#hk_long."

BAsp<-aggregate(BA.h~plot+species+year, sum, data=hk_long)
ddg.procedure("Basal Area Aggregation", ins=list("hk_long"), outs.data=list("BAsp"))

#because we have 2 plots, 2 years and 4 species, the combination of these should
#return 16 rows with sums of basal area for each.

BAsp

#PLOT 5. produce a barplot of basal area per plot faceted by year, with the
#individual bars representing annual sums of basal area per species per plot.
#Note that we are now using geom_bar to produce a bar plot. If you want the
#heights of the bars to represent values in the data, use stat="identity". We
#can control the width of the bars and set the position="dodge" (note that no
#dodge value is given here) so that the fill groups appear side by side.

ggplot(BAsp, aes(x=species, y=BA.h, fill=plot)) +
  
  geom_bar(stat="identity", width=0.5, position="dodge") + 
  
  facet_grid( ~ year) +
  
  theme_bw()

ddg.procedure("Plot 5", ins=list("BAsp"), outs.graphic="Basal Area Bar Plot")
#using aggreagte(), create a new table with sums of basal area by plot and year

BAplot<-aggregate(BA.h~plot+year, sum, data=hk_long)
ddg.procedure("Basal Area Aggregation", ins=list("hk_long"), outs.data=list("BAplot"))

#since we are now aggregating by 2 years and 2 plots, we should now see four
#sums of basal area

BAplot

#PLOT 6. produce a bar plot of basal area per plot with bars representing annual
#sums per plot. there is no need for faceting in this plot, so we do not include
#a term for it.

ggplot(BAplot, aes(x=plot, y=BA.h, fill=year)) +
  
  geom_bar(width=0.5, position="dodge") +
  
  theme_bw()

ddg.procedure("Plot 6", ins=list("BAplot"), outs.graphic="Anoter Basel Area Plot")
ddg.finish("Aggregate")
##SPATIAL DATA
ddg.start("Spatial Data")
#PLOT 7. using the full dataset, create a subset of live trees in 2009 in plot
#6. produce a map of these trees. assign a different color to each species and
#base the size of the point on dbh in 2009.

plot6<-subset(hem,plot==6 & cond09=="L")
ddg.procedure("Subset to plot 6", ins=list("hem"),outs.data=list("plot6"))
names(plot6)

ggplot(plot6, aes(x=xcoord, y=ycoord, colour=species, size=dbh09)) + 
  geom_point() +
  
  theme_bw()
ddg.procedure("Plot 7", ins=list("plot6"), outs.graphic="Tree Map Plot 3")
#PLOT 8. agian using the full dataset, create a subset of all QURU trees in 2009
#in plot 3. produce a map of these trees, assigning a different color to live
#and dead trees. base the size of the point on dbh at t0 so that a point appears
#for trees that dies before 2009. add tree number labels to each tree.

plot3<-subset(hem,plot==3 & species=="QURU")
ddg.procedure("Subset to plot 3", ins=list("hem"),outs.data=list("plot3"))

ggplot(plot3, aes(x=xcoord, y=ycoord, colour=cond09, size=dbht0)) + 
   geom_point() +
  
  geom_text(aes(y=ycoord+2, label=tree), size=3) +
  
  theme_bw()
ddg.procedure("Plot 8", ins=list("plot3"), outs.graphic="Tree Map Plot 3")
ddg.finish("Spatial Data")
ddg.finish("More Plots")
ddg.finish("Script")
