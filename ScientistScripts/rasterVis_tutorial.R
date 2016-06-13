# June 15, 2015

# Mayra I. Rodriguez Gonzalez
# mayra.rodriguez11@upr.edu

# Module's objective: To learn the rasterVis package because WE can.

# Tutorial from : coding is hard, but check out this link: http://rpubs.com/alobo/rasterVis_Intro1
# And this one is from the dude that created the package: http://oscarperpinan.github.io/rastervis/

# Also, plots can't be seen in the RStudio version on the server.
# So let's just run outside the server for now...

#
#
#
# Sec. 1 Learning rasterVis and its importance!
#
# Learning the rasterVis package is important because...
# The rasterVis package complements the raster package by enhancing graphs/figures visualization (i.e. we all want pretty graphs).

# And basically, that's it.
# So let's start!


# Round one: installing the package and required stuff that we'll hopefully use

install.packages('rasterVis') # This is the stable version.

require(utils)
require(rgdal)
require(raster)
require(rasterVis)


# Round two: download that data!


# Sadly, downloading the files through R didn't work for me.
# However, you can download it directly from: https://dl.dropboxusercontent.com/u/3180464/rprob520.tif
# And create a working directory with your sample data in it.

#wd <- ("Y:\\Mayra\\r_training\\rasterVis_tutorial")
#setwd(wd)

# GDALinfo reports the size and other parameters of the dataset (from mostly any format... whatever that means).
GDALinfo("data_first_section_rasterVis.tif")

# Create the raster layer object
r <- raster("data_first_section_rasterVis.tif")

# Plot your data to see a "standard" raster plot in R.
plot(r)

# You can change color tables however you want. Here are some examples provided in the online tutorial:

par(mfrow = c(2,2))
plot(r, col = rev(heat.colors(32)), zlim = c(0, 1))
plot(r, col = rev(terrain.colors(32)), zlim = c(0, 1))
plot(r, col = grey.colors(32, start = 0, end = 1, gamma = 1))
plot(r, col = rev(grey.colors(32, start = 0, end = 1, gamma = 1)))

# Round 3: the levelplot function

levelplot(r)
levelplot(r, zscaleLog = TRUE, contour = TRUE)
levelplot(r, zscaleLog = NULL, contour = TRUE, FUN.margin = median)
levelplot(r, contour = TRUE, margin = FALSE, main = "p(dep > 1kg/m2 per eruption event)")
levelplot(r, margin = FALSE, scales = list(draw = FALSE), colorkey = FALSE)

miat = c(0, 0.25, 0.5, 0.75, 1)
levelplot(r, contour = TRUE, margin = FALSE, at = miat)

# Round 4: control of the color key

myColorkey <- list(at = c(0, 0.5, 1))

# the zscale argument allow us to log-transform the object before plotting it. If NULL (default), then it won't be transformed.
# According to Oscar's blog (the dude that created the ratserVis package):
# Other possible values include base-number for taking logarithm. (Yay Math...)

levelplot(r, zscaleLog = NULL, contour = TRUE, margin = FALSE, at = miat, colorkey = myColorkey)

myColorkey <- list(at=miat, ## where the colors change
                   labels=list(labels=miat, ##what to print
                               at=miat))    ##where to print       

levelplot(r,zscaleLog=NULL,contour=TRUE, margin=FALSE,at=miat,colorkey=myColorkey)

myColorkey <- list(at = miat, labels = list(labels = c("Low", "Medium-low", "Medium-high", "High"), at = miat + 0.125))
levelplot(r, zscaleLog = NULL, contour = TRUE, margin = FALSE, at = miat, colorkey = myColorkey)

# The following are predefines themes

levelplot(r, contour = TRUE, margin = FALSE, at = (0:10)/10, par.settings = GrTheme)

levelplot(r, contour = TRUE, margin = FALSE, at = (0:10)/10, par.settings = BTCTheme)

levelplot(r, contour = TRUE, margin = FALSE, at = (0.:10)/10, par.settings = RdBuTheme)



#############################################################################################################################################
#############################################################################################################################################
##################################################     WORKING WITH RASTER STACKS      ######################################################
#############################################################################################################################################
#############################################################################################################################################




# Sec.2 Working with raster stacks

# Round let's not lose all hope: playing around with data?

library(raster)
library(rasterVis)


# Same again, downloading the files through R didn't seem to work.
# However, you can download it directly from: https://raw.github.com/oscarperpinan/spacetime-vis/master/data/SISmm2008_CMSAF.zip
# And create a working directory with your sample data in it.

# Don't forget to unzip it!


wd2 <- ("Y:\\Mayra\\r_training\\rasterVis_tutorial\\data_second_section_rasterVis")
setwd(wd2)
list.wd2 <- dir(wd2)

# To open ncdf files, you need to first install package "ncdf" or "ncdf4".

install.packages('ncdf')

# then proceed to stack your layers

stack.data <- stack(list.wd2)
stack.data <- stack.data*24 ### from irradiance (W/m2) to irradiation (Wh/m2)

index <- seq(as.Date("2008-01-15"), as.Date("2008-12-15"), "month")

data.mm <- setZ (stack.data, index)

# the month.abb is one of the few built-in constants in R. it would be the same to just create an object that included all months.
names(data.mm) <- month.abb

# Once the raster stack is defined we can go ahead and display it with the levelplot function.
# Each panel show one layer of the created raster stack.

levelplot(data.mm)

# to display only one of the layers of the stack, change the value of the argument layers.

levelplot(data.mm, layers = 1, FUN.margin = median, contour = TRUE)

# This ends up as a trellis object (after extensive googling, I still can't explain what this is. (Therefore, I'm skiping this part.)

# Round I think I stopped counting a long time ago: Themes, themes and more themes

# When working with only one layer (from the previous tutorial), we learned that R provide us with several themes for our figures.

# The previous figure had the 'default' one. Let's review the other themes available for us:

# GrTheme (gray palette), BTCTheme (BTC palette - hexbin package-) and the RdBuTheme (red and blue palette, you genius!).

Aug <- raster (data.mm, 8)
meanAug <- cellStats(Aug, mean)
levelplot (Aug-meanAug, par.settings = RdBuTheme)

# It is super easy to define a new theme for each different palette!

# As Oscar's example shows, we can use a sequantial palette from the colorspace package:

library (colorspace)
myTheme=rasterTheme(region=sequential_hcl(10, power=2.2))
levelplot(Aug, par.settings=myTheme, contour=TRUE)

# or we can even use the colour-blindness corrections from the dichromat package for a pretty awesome output!!
# (Excuse my fake enthusiasm, I'm trying not to fall asleep here.)

library(dichromat)
myTheme <- rasterTheme(region=dichromat(terrain.colors(15)))
levelplot(Aug, par.settings=myTheme)

# But now now, let us never forget about hsitograms and scatter plots!

xyplot(Jan+Feb~Jul|cut(x,4), data=data.mm, auto.key=list(space="right"))  ### Relation between the January & February versus
                                                                          ### July radiation for four
                                                                          ### differents longitude regions.

# That took quite some time, didn't it?
# Guess what? It is even faster with hexbinplot! (apparently)

# hexbinplot(Jan~Jul|cut(x, 6), data=data.mm)
# ok, let's forget about this one. I can't make it work.

# but let's do some matrices! and histograms! and box-and-whisker and violin plots! or even density estimates!

splom(data.mm)
histogram(data.mm)
density(data.mm)
bwplot(data.mm)

# Once again, according to (wait for it) Oscar's blog: since these methods accept a FUN argument, it can be applied
# to the z slot (whatever that is) of the raster, resulting in a grouping variable for the plot (whatever that means).

#histogram(data.mm, FUN=as.yearqtr)
# yeah, this didn't work out for me so let's ommit it until I figure it out.

# whatever, here's a cute rainbow themed plot.

pal(rev(rainbow(n = 10))) ### to visualize your palett
rainbTheme10 <- rasterTheme(region = rev(rainbow(n = 10))) ### to make your theme
levelplot(data.mm, margin = FALSE, contour = TRUE, par.settings = rainbTheme10) ### to display using the levelplot function

# for more info about this (http://rpubs.com/alobo/rasterVis_Intro1)



#############################################################################################################################################
#############################################################################################################################################
##################################################    SPACE-TIME & VECTOR FIELD PLOTS     ###################################################
#############################################################################################################################################
#############################################################################################################################################



# Sec.3 What's left of the tutorial... I guess.

library(zoo)

setwd(wd) # form section 1
new.stack <- stack("data_third_section_rasterVis.nc")
index <- seq(as.Date("1970-01-01"), as.Date("2003-03-01"), by = "month")
tt <- as.yearmon(index)
new.stack <- setZ(new.stack, tt)
names(new.stack) <- as.character(tt)

# Extract month value from a Date or yearmon object

month <- function(x)format(x, "%m")

# Compute anomaly using monthly grouping with ave

anomaly <- function(x){     # This is literally a copy form the tutorial, I have no idea what's happening here.
  ## Monthly means
  mm <- ave(x, month(tt), FUN = mean)
  ## Monthly standard deviation
  msd <- ave(x, month(tt), FUN = sd)
  ## anomaly
  (x - mm)/msd
}

## Use anomaly with calc
stack.anom <- calc(new.stack, anomaly)
stack.anom <- setZ(stack.anom, tt)
#setwd(wd)                 # Yay calculations!!

hovmoller(stack.anom,
          at = seq (-3, 3, .25),
          panel = panel.levelplot.raster,
          interpolate = TRUE,
          yscale.components = yscale.raster.subticks,
          par.settings = BuRdTheme)

# So, Oscar also thinks that the horizont and xyplot methods are useful for the space-time Raster objects:

horizonplot(stack.anom, col.regions = rev(brewer.pal(n = 10, "RdBu")))

# Now, with raster field plots:

proj <- CRS("+proj=longlat +datum=WGS84")
df <- expand.grid(x = seq(-2, 2, .01), y = seq (-2, 2, .01))

df$z <- with(df, (3*x^2 + y)*exp(-x^2-y^2))
r <- rasterFromXYZ(df, crs=proj)

vectorplot(r, par.settings=RdBuTheme())

# Streamlines, a family of curves that are tangent to the vector field, show the direction an element (droplet)
# will follow under the effect of the field. streamplot displays streamlines:

streamplot(r)

# streamplot has two arguments, droplets and streamlets. These control # of droplets, length of streamlets and streamlet calculation.

df$z <- with (df, sqrt(x^2 + y^2))
df$phi <- with(df, atan2(-y, x))
r2 <- rasterFromXYZ(df, crs=proj)

streamplot(r2, isField=TRUE, streamlet=list(L=30), droplet=list(pc=.3), par.settings=streamTheme(symbol = brewer.pal(n=5, name = "Reds")))

# Lastly, it appears that this package is able to include two functions to interact with the trellis (seriously, what is this?) objects.

# Quoting Oscar, "The identifyRaster method labels and returns points of a trellis graphic according to mouse clicks.
# It is commonly used after levelplot, although it can be also used after xyplot, hexbinplot --which did not work, mind you-- or even splom."

# ----------------- Let's not even try this, it crashed the program. -------------------

#levelplot(data.mm)
#chosen <- identifyRaster(data.mm, layer = 3, values = TRUE)

# "The choseRegion function provides a set of points (in the form of a SpatialPoints object) inside a region defined by several mouse clicks.
# Use the left button of the mouse to build a border with points, and the right button to finish.
# The points enclosed by the border will be highlighted and returned as a SpatialPoints object."

#reg <- chooseRegion()
