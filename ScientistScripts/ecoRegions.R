### Plots of ecoregion patterns for dana 
# Multivarible dataset, reading in ordinated values each time
#DDG.ANNOTE Init
source('global.R')
#DDG.ANNOTE Reading data
### Fig 1. Ordination of ecoregions moving through climate space
stay <- read.csv('../data/AllLocations_EnvStay.csv')
#DDG.ANNOTE Cleaning the data
stay <- stay[,-8]
#DDG.ANNOTE Subsetting?
stay$Time <- as.character(stay$Time)
#DDG.ANNOTE labeling
stay$Time[as.character(stay$Time) == "Current"] <- '2010'

                                        #relativize climate variables
#DDG.ANNOTE Computation
stay[,5:7] <- apply(stay[,5:7],2,function(x) x/max(x))
                                        #ordinate
### ord.stay <- nmds(dist(stay[,5:7]),2,2)
### min.stay <- nmds.min(ord.stay) / code for conducting ordination
#DDG.ANNOTE Reading Data This is here due to the legacy where ordination occurs
min.stay <- read.csv('../data/ord_stay.csv')
#DDG.ANNOTE  Cleaning the data
vec.stay <- envfit(min.stay,stay[,-1:-4])
#DDG.ANNOTE Organizing / cleaning the data
ord.col <- c('blue','green','red')[as.numeric(stay$Ecoregion)]
ord.time <- as.numeric(factor(stay$Time))
# Time here is for displaying figures over time
ord.time <- c(1,0.45,0.5)[ord.time]
#DDG.ANNOTE Joining observations and computation / generating alpha values for plot
ord.alpha <- apply(cbind(ord.col,ord.time),1,function(x) alpha(x[1],x[2]))
#DDG.ANNOTE Cleaning the data
ord.pch <- as.numeric(factor(stay$Time))
ord.pch <- c(19,19,1)[ord.pch]
f.stay <- paste(stay[,1],stay[,4])
leg.names <- unique(paste(stay[,1],stay[,4]))
#DDG.ANNOTE Labeling / static data set
leg.names <- leg.names[c(3,1,2,6,4,5,9,7,8)]
#DDG.ANNOTE Labelign ^^^
leg.col <- unique(ord.alpha)[c(3,1,2,6,4,5,9,7,8)]

#DDG.ANNOTE PDF WRAPPER -> For documentation / final presentation
pdf('../results/EcoReg_FigA.pdf')
#DDG.ANNOTE Plotting
chPlot(min.stay,f=f.stay,col=ord.alpha,pch=ord.pch,xlim=c(-1,1.25),ylim=c(-1,0.5))
plot(vec.stay,col=grey(0.75))
legend('bottomright',legend=leg.names,pch=rep(c(19,19,1),3),col=leg.col)
#DDG.ANNOTE Finish Plotting
dev.off()
#DDG.ANNOTE Push to github for publication
gitPush('../results')
#DDG.ANNOTE Reading / inputting data
### Fig 2. Plot of points moving through geographic space based on climate
move.all <- read.csv('../data/AllLocations_EnvMove.csv')
#DDG.ANNOTE Computation
move.all[,2:3] <- apply(move.all[,2:3],2,function(x) (x-mean(x))/sd(x))
#DDG.ANNOTE Cleaning
vec.move <- envfit(move.all[,2:3],move.all[,5:7])
move.col <- c('blue','green','red')[as.numeric(move.all$Ecoregion)]
move.time <- as.numeric(factor(move.all$Time))
#DDG.ANNOTE Labeling / static setting
move.time <- c(1,0.45,0.5)[move.time]
#DDG.ANNOTE Joining observations / unionizing
move.alpha <- apply(cbind(move.col,move.time),1,function(x) alpha(x[1],x[2]))
#DDG.ANNOTE Cleaning
move.pch <- as.numeric(factor(move.all$Time))
#DDG.ANNOTE Labeling / static setting
move.pch <- c(19,19,1)[move.pch]
#DDG.ANNOTE Cleaning
f <- paste(move.all[,1],move.all[,4])
#DDG.ANNOTE PDF WRAPPER -> For documentation / final presentation
pdf('../results/EcoReg_FigB.pdf')
#DDG.ANNOTE Plotting woo
chPlot(move.all[,2:3],f=f,col=move.alpha,pch=move.pch,xlim=c(-1.5,2),ylim=c(-2,1))
plot(vec.move,col=grey(0.75))
legend('topright',legend=leg.names,pch=rep(c(19,19,1),3),col=leg.col)
#DDG.ANNOTE Finish plotting / close pdf
dev.off()
#DDG.ANNOTE Push to github for publication
gitPush('../results')
