# JUne 16, 2015
# Mayra Rodriguez Gonzalez
# mayra.rodriguez11@upr.edu

# Histograms for all services using the rasterVis package.


library(raster)
library(rasterVis)

scenario <- c("CT1", "FM1", "GF1", "RL1")


###################################################################################
scener <- scenario[1] ################ change scenario here (1-4) #################
###################################################################################


wd <- paste ("Y:\\Mayra\\Project")
setwd(wd)

ending <- c(5, 25, 50)


###################################################################################
this.year <- ending [2] #################### change year here #####################
###################################################################################

zonal.layers <- list.files(paste(wd, "\\", "Zonal_Layer", sep=""), pattern = "img$")

percentile <- .8

###################################################################################
############# change zonal.layers' value for different scale (1-3) ################
###################################################################################

zonal.layer <- raster(paste(wd, "\\Zonal_Layer\\", zonal.layers[1], sep = ""))
zonal.vals <- getValues(zonal.layer)
zonal.vals[is.na(zonal.vals)] <- 2000

wd2 <- paste (wd, "\\Stakeholder_Services\\", scener, sep = "")

excluded.layers <- c("Agriculture", "Conservation", "Development", "Impervious", "N_export", "P_export", "Water_Yield")

list.wd <- dir(wd2, pattern = paste("_", this.year, ".img$", sep =""))

for(i in 1:length(excluded.layers)){
  
  list.wd <- list.wd[ !grepl(excluded.layers[i], list.wd) ]
  
}

print(list.wd)

###################################################################################
################################## QUANTILES ######################################
###################################################################################

all.quantiles <- rep(NA, 7)
c <- 1

for (j in 1:length(list.wd)){
  service.file <- paste(wd2, "\\", list.wd[j], sep = "")
  print(service.file)
  dat.file <- raster(service.file)
  x1 <- getValues(dat.file)
  x2 <- data.table(x1)
  dt1 <- data.table(as.integer(zonal.vals))
  order <- data.table(c(1:length(zonal.vals)))
  dt2 <- cbind(order, dt1)
  dt <- cbind(dt2, x2)
  dt <- setNames(dt, c("order", "zone", "value"))
  setkey(dt, "zone")
  
  that.value <- dt[,value]
  quantile.value <- quantile(that.value, probs = percentile, na.rm = T)
  print(quantile.value)
  
  all.quantiles[c] <- quantile.value
  print(all.quantiles)
  c <- c + 1
}

###################################################################################
################################### HISTOGRAMS ####################################
###################################################################################

setwd(wd2)

stack.data <- stack(list.wd)

x.labels <- c("Carbon Storage total Tg", "Habitat Quality", "Timber Harvest", "Nitrogen Retention kg/ha", "Perviousness ha(total)/% of state", "Phosphorus retention kg/ha", "Water Yield mm/year")
index <- c("Carbon Storage", "Habitat Quality", "Timber Harvest", "Nitrogen Retention", "Pervious", "Phosphorus Retention", "Water Yield")
my.data <- setZ (stack.data, index)
names(my.data) <- index
histogram(my.data, xlab = x.labels, ylab = "Frequency", main = index)








#### TEST RUN ####

#library(raster)
#library(rasterVis)

#wd <- ("Y:\\Mayra\\Project\\other\\test\\2035")
#setwd(wd)
#list.wd <- dir(wd, pattern = "_35.img$")

#services <- c("pervious", "water")
#all.services <- paste(services, "_test_35.img", sep = "")

#stack.data <- stack(all.services)
#stack.data <- stack(list.wd)
#my.data <- stack(list.wd)

#x.labels <- c("Perviousness ha(total)/% of state", "Water Yield mm/year")
#index <- c("Pervious surface", "Water Yield")
#my.data <- setZ (stack.data, index)
#names(my.data) <- index
#histogram(my.data, xlab = x.labels, ylab = "Frequency" , main = index)
