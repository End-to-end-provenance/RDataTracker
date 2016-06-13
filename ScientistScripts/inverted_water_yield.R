# June 4, 2015
# Mayra I. Rodriguez-Gonzalez
# mayra.rodriguez11@upr.edu

# NOTES: THIS CODE INVERTS WATER YIELD FOR ECOSYSTEM SERVICES ANALYSIS.


########################################################################
########################################################################
############# NESTED FOR LOOPS AT THE END OF SCRIPT ####################
########################################################################
########################################################################


wd <- "Y:\\Mayra\\Project\\Stakeholder_Services\\CT1"
setwd(wd)
library(raster)

wateryield.img <- list.files(wd, pattern = glob2rx("Water_Yield*img$"))
years <- c(0,25,50)

for (i in 1:length(wateryield.img)){
  #print(impervious.img[i])
  r <- raster(wateryield.img[i])
  r.values <- getValues (r)
  new.values <- 1297.61 - r.values
  #print(new.values)
  newr <- r
  values(newr) <- new.values
  writeRaster(newr, paste("WaterYield_inverted_",years[i],".img", sep = ""), package="raster")
}


#############################################################################
#############################################################################
################################# HERE ######################################
#############################################################################
#############################################################################


#############################################################################
####################### all files for water yield ###########################
############################ NESTED FOR LOOP ################################
#############################################################################




wd <- "Y:\\Mayra\\Project\\Stakeholder_Services"
setwd(wd)
library(raster)

files <- list.files(wd)
folders.only <- files[ !grepl(".txt",files) ]

years <- c(0,25,50)

for (i in 1:length(folders.only)){
  wateryield.img<- list.files(paste(wd,"\\",folders.only[i],sep=""), pattern = glob2rx("Water_Yield*img$"))
  for (j in 1:length(wateryield.img)){
    r <- raster(paste(wd,"\\",folders.only[i],"\\",wateryield.img[j], sep=""))
    r.values <- getValues (r)
    new.values <- 1297.61 - r.values
    newr <- r
    values(newr) <- new.values
    writeRaster(newr, paste("Y:\\Mayra\\Project\\Stakeholder_Services\\",folders.only[i],"\\WaterYield_inverted_",years[j],".img", sep = ""), package="raster")
  }
}
#paste(wd,"\\",folders.only[1],"\\",impervious.img[1], sep = "")




#############################################################################
######################## histograms for water yield #########################
############################ NESTED FOR LOOP ################################
#############################################################################




wd <- "Y:\\Mayra\\Project\\Stakeholder_Services"
setwd(wd)
library(raster)
options(scipen = 999)

files <- list.files(wd)
folders.only <- files[!grepl(".txt", files)]

names <- c("Recent Trends", "Forest as Infrastructure", "Opportunistic Growth", "Regional Self-Reliance")
years <- c(2010, 2035, 2060)

windows (10,10)
par(mfrow=c(4,3))

for (i in 1:length(folders.only)){
  wateryield.img<- list.files(paste(wd,"\\",folders.only[i],sep=""), pattern = glob2rx("Water_Yield*img$"))
  for (j in 1:length (wateryield.img)){
    r <- raster(paste(wd,"\\",folders.only[i],"\\",wateryield.img[j],sep=""))
    r.values <- getValues(r)
    r.new <- na.omit(r.values)
    mr.new <- mean(r.new)
    rounded.mean <- round (mr.new, digits = 2)
    #h <- hist(r.new, main = paste(names[i], years[j]), xlab= "Water Yield")
    h <- hist(r.new, main = paste(names[i], years[j]), xlab= "Water Yield", ylim = c(0,2000000))
    #abline (v = mr.new, col = "blue", lwd = 2)
    text(1000, 1800000, paste("mean = ", rounded.mean, sep = ""), cex = 0.9)
    #test <- text(x = h$mids, y = (h$count+500000), labels = paste(h$counts))
    for(k in 1:length(h$counts)){
      if(h$counts[k] > 0) {
        text(x = h$mids[k], y = h$count[k]+350000, labels = paste(h$counts[k]), cex = 0.7, srt = 90)
        #}else{
        # print(" ")
      }
    }
    #save(r.new, file = paste("Y:\\Mayra\\Project\\Stakeholder_Services\\",folders.only[i],"\\Pervious_Hist_",years[i],".jpg", sep = ""))
  }
}

