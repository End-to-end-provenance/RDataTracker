#R REU INTRO
#SESSION 1
#LIZA NICOLL
#05 JUNE 2014

#R ignores any text that comes after #. Get in the habit of writing lots of
#notes and comments for yourself and and for future users of your scripts.

#each of the ten trees has a unique ID. You can use seq() to create a sequence
#of number from 1 to 10.

#we can access the help page for any function by typing ? before the name. This
#is where you can find details about what the function requires to work and what
#options are available.
?seq

#here treeID is the object name. the arrow <- points to the object name and
#tells R what it should represent, in this case numbers 1 to 10

#R is case sensitive. tree_id ≠ tree_ID

treeID<-seq(1,10)

#write the object name alone to print it

treeID

#All trees are of the same species, Red Oak - Quercus rubra or QURU for short.
#Using rep(), repeat "QURU" ten times in a list to create the "species"
#variable. Note the quotes around QURU indicating that it is a string of
#letters, or a character variable.

species<-rep("QURU", 10)
species

#there are five trees in each of two plots, A and B. Use rep() to repeat A and B
#five times each. Use c() to concatenate (join) the two sequences together in a
#vector. Note how parantheses are used to nest functions.

plotID<-c(rep("A",5), rep("B",5))
plotID

#enter the dbh for each tree and use c() again to join them. Order matters here
#because these measurements will be linked to tree ID and plot in the next step.

dbh<-c(24.4, 18.4, 31.7, 34, 38, 30.5, 46.4, 43.8, 46.2, 39.5)
dbh

#use data.frame() to join the four variables into a data frame. A data frame is
#a way to store lists of vectors of equal length. This is the step where we
#actually create the data table that we have on our paper data sheet.

data<-data.frame(treeID, species, plotID, dbh)

#display the data frame. Note that the first column is the row number.

data

#print a summary of the data frame. This is a good way to check your data for
#errors.
summary(data) 

#display the structure of the data frame using str(). This is another good
#diagnostic tool, useful for identifying variable types and data dimensions.

str(data)

#NOTES ABOUT VARIABLE TYPES

#display the dimenstions (length and width) of the data frame. Note how R
#displays dimension information as [rows, columns] or [observations, variables].
#This will be important later.
dim(data) #dimensions of data frame
nrow(data) #number of rows
ncol(data) #number of columns
names(data) #display the variable (header) names of the data frame
head(data,4) #display the first four lines of the data frame
tail(data,2) #display the last two lines of the data frame

#display only data from plot B using subset(). Notice the double equal signs and
#the quotes around B.

subset(data, plotID=="B")

#display only data with a DBH greater than 25

subset(data, dbh>25)

#OPERATORS

#MORE DATA

#RSTUDIO INTERLUDE

#once your directory is set, read in the file using read.csv()
#you can print a list of files in the current directory using dir()

dir()

#this is a good way to select, copy and paste the file name that you want to
#read into R.

data2<-read.csv("reu_r_data1.csv")

#print the data

data2

#now we want to add this new data to the data we already have. Notice that the
#format and header row is exactly the same as the file we created previously.
#because of this, we can used use rbind() to join the two data frames by rows
#(rbind=row bind)

all<-rbind(data, data2)
all
summary(all)

#SINGLE VARIABLE

#print the dbh column from our new data frame (all)

all$dbh

#print the maximum, minimum, median and mean of all$dbh

max(all$dbh)
min(all$dbh)
median(all$dbh)
mean(all$dbh)

#remember that summary() presents all of this information at once

summary(all$dbh)

#we can also easily calculate the standard deviation (a measure of how spread out 
#values are) of our variable with built-in functions.

sd(all$dbh)

#PLOTTING


#par() is used to set the graphing parameters, such as how many plots appear on
#the same page. Here we ask that there be 1 row and 2 columns of graphs, or two
#graphs side-by-side.

par(mfrow=c(1,2))

#create a histogram of all dbh measurements to show the distribution of dbh
#measurements in the four plots

hist(all$dbh)
hist(all$dbh, xlab="DBH", main="Histogram of DBH", col="red", breaks=10)

#create a boxplot of all dbh measurements, another way of looking at the
#distribution of dbh

par(mfrow=c(1,2))
boxplot(all$dbh)
boxplot(all$dbh, horizontal=T, xlab="DBH", main="Boxplot of DBH", col = "lightgreen")

#using plot(), create box and whisker plots for dbh in each plot. Because our
#plot data is categorical, R is smart enough to know to print a boxplot, if the
#data were only numeric it would print a scatterplot.

par(mfrow=c(1,2))
plot(all$plotID, all$dbh)
plot(all$plotID, all$dbh, xlab="Plot", ylab="DBH", main="DBH by Plot", col=c(2,3,4,5))

#SAVING DATA

write.table(all,"reu.r.data2.csv")
