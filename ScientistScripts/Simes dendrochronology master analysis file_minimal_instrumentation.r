# Time the execution
start.time <- Sys.time()
#ddg.library <- Sys.getenv("DDG_LIBRARY")
#ddg.library <- "~/Documents/Process/DataProvenance/workspace/ddg-r/ddg-library_0.2.r"
#if (ddg.library == "") {
  #ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
#}
#source(ddg.library)

ddg.init("~/Documents/Process/DataProvenance/workspace/ddg-r/examples/Aaron/")
r.script.path <- paste(getwd(),"/Simes dendrochronology master analysis file_minimal_instrumentation.R",sep="")
ddgdir <- paste(getwd(),"/ddg",sep="")
library(RDataTracker)  
                  
#Visualization and analysis for Simes dendrochronology ms.
#========================================================

#Load required libraries


#```r
library(dplR)
#```

#```
## Warning: package 'dplR' was built under R version 3.0.2
#```

#```r
library(zoo)
#```

#```
## Warning: package 'zoo' was built under R version 3.0.2
#```

#```
## 
## Attaching package: 'zoo'
## 
## The following object is masked from 'package:base':
## 
##     as.Date, as.Date.numeric
#```

#```r
library(ggplot2)
library(gdata)
#```

#```
## Warning: running command '"C:\PROGRA~1\MIKTEX~1.9\miktex\bin\perl.exe"
## "C:/Users/aellison/Documents/R/win-library/3.0/gdata/perl/supportedFormats.pl"'
## had status 2
#```

#```
## gdata: Unable to load perl libaries needed by read.xls()
## gdata: to support 'XLX' (Excel 97-2004) files.
## 
## gdata: Unable to load perl libaries needed by read.xls()
## gdata: to support 'XLSX' (Excel 2007+) files.
## 
## gdata: Run the function 'installXLSXsupport()'
## gdata: to automatically download and install the perl
## gdata: libaries needed to support Excel XLS and XLSX formats.
## 
## Attaching package: 'gdata'
## 
## The following object is masked from 'package:stats':
## 
##     nobs
## 
## The following object is masked from 'package:utils':
## 
##     object.size
#```

#```r
library(grid)
library(mgcv)
#```

#```
## This is mgcv 1.7-24. For overview type 'help("mgcv-package")'.
#```

#```r
library(akima)
library(spatstat)
#```

#```
## Loading required package: deldir
## deldir 0.0-22
## spatstat 1.31-3 
## Type 'help(spatstat)' for an overview of spatstat 
##      'latest.news()' for news on latest version 
##      'licence.polygons()' for licence information on polygon calculations
#```


#Generate required custom functions filled.contour3 and multiplot

#filled.contour3 from: http://wiki.cbr.washington.edu/qerm/sites/qerm/images/1/16/Filled.contour3.R

#Multiplot is from R cookbook: 
#http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/

#```r
filled.contour3 <- function(x = seq(0, 1, length.out = nrow(z)), y = seq(0, 
    1, length.out = ncol(z)), z, xlim = range(x, finite = TRUE), ylim = range(y, 
    finite = TRUE), zlim = range(z, finite = TRUE), levels = pretty(zlim, nlevels), 
    nlevels = 20, color.palette = cm.colors, col = color.palette(length(levels) - 
        1), plot.title, plot.axes, key.title, key.axes, asp = NA, xaxs = "i", 
    yaxs = "i", las = 1, axes = TRUE, frame.plot = axes, mar, ...) {
    # modification by Ian Taylor of the filled.contour function to remove the
    # key and facilitate overplotting with contour() further modified by Carey
    # McGilliard and Bridget Ferris to allow multiple plots on one page
    
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            } else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        } else stop("no 'z' matrix specified")
    } else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")
    # mar.orig <- (par.orig <- par(c('mar', 'las', 'mfrow')))$mar
    # on.exit(par(par.orig)) w <- (3 + mar.orig[2]) * par('csi') * 2.54 par(las
    # = las) mar <- mar.orig
    plot.new()
    # par(mar=mar)
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
        stop("no proper 'z' matrix specified")
    if (!is.double(z)) 
        storage.mode(z) <- "double"
    .filled.contour(as.double(x), as.double(y), z, as.double(levels), col = col)
    if (missing(plot.axes)) {
        if (axes) {
            title(main = "", xlab = "", ylab = "")
            Axis(x, side = 1)
            Axis(y, side = 2)
        }
    } else plot.axes
    if (frame.plot) 
        box()
    if (missing(plot.title)) 
        title(...) else plot.title
    invisible()
}


# Multiple plot function ggplot objects can be passed in ..., or to plotlist
# (as a list of ggplot objects) - cols: Number of columns in layout -
# layout: A matrix specifying the layout. If present, 'cols' is ignored.  If
# the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE), then
# plot 1 will go in the upper left, 2 will go in the upper right, and 3 will
# go all the way across the bottom.
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
    require(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel ncol: Number of columns of plots nrow: Number of rows
        # needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), ncol = cols, 
            nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots == 1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
        }
    }
}
#```


#Read in raw data files

#```r
trees <- read.csv("trees03.csv")
#trees <- read.csv("C:/Users/aellison/Dropbox/Aaron's Briefcase/manuscript - Simes dendro and land use/analysis/trees03.csv")
dendro <- read.csv("hf086-01-dendro.csv")
#dendro <- read.csv("C:/Users/aellison/Dropbox/Aaron's Briefcase/manuscript - Simes dendro and land use/analysis/hf086-01-dendro.csv")

head(trees)
#```

#```
##   plot sec tree genus species  dbh   cc live xcoord ycoord zelev elev0
## 1    1   C  501 Tsuga    TSCA 13.4 <NA>    D   30.1   37.6 -2.57 50;40
## 2    1   C  512 Tsuga    TSCA 29.0    C    L   30.1   51.7 -3.28 50;40
## 3    1   C  507 Tsuga    TSCA 12.3    S    L   30.8   44.5 -3.17 50;40
## 4    1   C  513 Tsuga    TSCA 43.8    D    L   31.3   55.1 -3.50 50;40
## 5    1   C  502  Acer    ACRU  9.0    S    L   31.5   40.3 -3.03 50;40
## 6    1   C  506 Tsuga    TSCA  9.9    S    L   31.8   43.5 -3.22 50;40
#```

#```r
head(dendro)
#```

#```
##   Plot Genus Species Code TreeNum Year mmperyear
## 1    1  Acer  rubrum ACRU     532 1935     1.275
## 2    1  Acer  rubrum ACRU     532 1936     1.519
## 3    1  Acer  rubrum ACRU     532 1937     1.375
## 4    1  Acer  rubrum ACRU     532 1938     0.891
## 5    1  Acer  rubrum ACRU     532 1939     0.737
## 6    1  Acer  rubrum ACRU     532 1940     1.139
#```


#Create contour plots of trees in each plot (ms. Figures 5, 6)
ddg.procedure("Create contour plots of trees in each plot")

#```r

# extract tree location data for each plot
attach(trees)
plot.1 <- trees[plot == 1, ]
plot.2 <- trees[plot == 2, ]
plot.3 <- trees[plot == 3, ]
plot.4 <- trees[plot == 4, ]
plot.5 <- trees[plot == 5, ]
plot.6 <- trees[plot == 6, ]
plot.7 <- trees[plot == 7, ]
plot.8 <- trees[plot == 8, ]

genera <- levels(genus)

detach(trees)

# remove NAs
p1 <- na.omit(plot.1)
p2 <- na.omit(plot.2)
p3 <- na.omit(plot.3)
p4 <- na.omit(plot.4)
p5 <- na.omit(plot.5)
p6 <- na.omit(plot.6)
p7 <- na.omit(plot.7)
p8 <- na.omit(plot.8)

# set up contour plot grids
p.1.i <- interp(p1$xcoord, p1$ycoord, scale(p1$zelev), xo = seq(0, 90, length = 40), 
    yo = seq(0, 90, length = 40), duplicate = "strip", extrap = FALSE, linear = FALSE)

p.2.i <- interp(p2$xcoord, p2$ycoord, scale(p2$zelev), xo = seq(0, 90, length = 40), 
    yo = seq(0, 90, length = 40), duplicate = "strip", extrap = FALSE, linear = FALSE)
p.3.i <- interp(p3$xcoord, p3$ycoord, scale(p3$zelev), xo = seq(0, 90, length = 40), 
    yo = seq(0, 90, length = 40), duplicate = "strip", extrap = FALSE, linear = FALSE)
p.4.i <- interp(p4$xcoord, p4$ycoord, scale(p4$zelev), xo = seq(0, 90, length = 40), 
    yo = seq(0, 90, length = 40), duplicate = "strip", extrap = FALSE, linear = FALSE)
p.5.i <- interp(p5$xcoord, p5$ycoord, scale(p5$zelev), xo = seq(0, 90, length = 40), 
    yo = seq(0, 90, length = 40), duplicate = "strip", extrap = FALSE, linear = FALSE)
p.6.i <- interp(p6$xcoord, p6$ycoord, scale(p6$zelev), xo = seq(0, 90, length = 40), 
    yo = seq(0, 90, length = 40), duplicate = "strip", extrap = FALSE, linear = FALSE)
p.7.i <- interp(p7$xcoord, p7$ycoord, scale(p7$zelev), xo = seq(0, 90, length = 40), 
    yo = seq(0, 90, length = 40), duplicate = "strip", extrap = FALSE, linear = FALSE)
p.8.i <- interp(p8$xcoord, p8$ycoord, scale(p8$zelev), xo = seq(0, 90, length = 40), 
    yo = seq(0, 90, length = 40), duplicate = "strip", extrap = FALSE, linear = FALSE)


# set color palette for genera

genera.colors.1 <- c("firebrick4", "coral", "black", "black", "black", "black", 
    "black", "black", "black", "olivedrab3", "black", "tan4", "black", "darkgreen")

genera.rank <- c(14, 2, 1, 12, 10, 7, 9, 4, 11, 8, 3, 6, 5, 13)

cex.I <- c(0.6, 0.6, rep(0.3, 7), 0.6, 0.3, 0.6, 0.3, 0.6)

# begin hemlock plot uncomment pdf and dev.off() lines to generate pdfs

pdf(file='hemlock_countors.pdf', width=4.75, height=6, colormodel='cmyk',
  pointsize=9)

layout(matrix(c(1, 6, 2, 5, 3, 4, 0, 0, 7, 7), 5, 2, byrow = TRUE), widths = rep(lcm(4.08), 
    2), heights = c(rep(lcm(4.08), 3), lcm(0.75), lcm(1.5)))
# layout.show(7)
par(pin = c(1.75, 1.75), plt = c(0.0625, 0.9375, 0.0625, 0.9375))


filled.contour3(p.1.i, xlim = c(0, 90), ylim = c(0, 90), axes = FALSE, nlevels = 5)

for (i in 1:length(genera)) {
    points(plot.1$xcoord[plot.1$genus == genera[i]], plot.1$ycoord[plot.1$genus == 
        genera[i]], col = genera.colors.1[i], pch = 19, cex = cex.I[i])
}
axis(1, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
axis(2, at = seq(0, 80, 20), labels = TRUE, tcl = -0.25, lwd = 2, font.axis = 2)
axis(3, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
axis(4, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
box(lwd = 2)
rect(30, 30, 60, 60, border = "black", lty = "solid", lwd = 1)
mtext("Valley", side = 3, font = 2, padj = -1, cex = 1)

filled.contour3(p.2.i, xlim = c(0, 90), ylim = c(0, 90), axes = FALSE, nlevels = 5)

for (i in 1:length(genera)) {
    points(plot.2$xcoord[plot.2$genus == genera[i]], plot.2$ycoord[plot.2$genus == 
        genera[i]], col = genera.colors.1[i], pch = 19, cex = cex.I[i])
}
axis(1, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
axis(2, at = seq(0, 80, 20), labels = TRUE, tcl = -0.25, lwd = 2, font.axis = 2)
axis(3, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
axis(4, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
box(lwd = 2)
rect(30, 30, 60, 60, border = "black", lty = "solid", lwd = 1)

mtext("Meters", side = 2, font = 2, padj = -3, cex = 0.85)

filled.contour3(p.3.i, xlim = c(0, 90), ylim = c(0, 90), axes = FALSE, nlevels = 5)

for (i in 1:length(genera)) {
    points(plot.3$xcoord[plot.3$genus == genera[i]], plot.3$ycoord[plot.3$genus == 
        genera[i]], col = genera.colors.1[i], pch = 19, cex = cex.I[i])
}
axis(1, at = seq(0, 80, 20), labels = TRUE, tcl = -0.25, lwd = 2, font.axis = 2)
axis(2, at = seq(0, 80, 20), labels = TRUE, tcl = -0.25, lwd = 2, font.axis = 2)
axis(3, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
axis(4, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
box(lwd = 2)
rect(30, 30, 60, 60, border = "black", lty = "solid", lwd = 1)

filled.contour3(p.4.i, xlim = c(0, 90), ylim = c(0, 90), axes = FALSE, nlevels = 5)

for (i in 1:length(genera)) {
    points(plot.4$xcoord[plot.4$genus == genera[i]], plot.4$ycoord[plot.4$genus == 
        genera[i]], col = genera.colors.1[i], pch = 19, cex = cex.I[i])
}
axis(1, at = seq(0, 80, 20), labels = TRUE, tcl = -0.25, lwd = 2, font.axis = 2)
axis(2, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
axis(3, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
axis(4, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
box(lwd = 2)
rect(30, 30, 60, 60, border = "black", lty = "solid", lwd = 1)

mtext("Meters", side = 1, font = 2, padj = 3, adj = -0.35, cex = 0.85)

filled.contour3(p.5.i, xlim = c(0, 90), ylim = c(0, 90), axes = FALSE, nlevels = 5)

for (i in 1:length(genera)) {
    points(plot.5$xcoord[plot.5$genus == genera[i]], plot.5$ycoord[plot.5$genus == 
        genera[i]], col = genera.colors.1[i], pch = 19, cex = cex.I[i])
}
axis(1, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
axis(2, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
axis(3, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
axis(4, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
box(lwd = 2)
rect(30, 30, 60, 60, border = "black", lty = "solid", lwd = 1)


filled.contour3(p.6.i, xlim = c(0, 90), ylim = c(0, 90), axes = FALSE, nlevels = 5)

for (i in 1:length(genera)) {
    points(plot.6$xcoord[plot.6$genus == genera[i]], plot.6$ycoord[plot.6$genus == 
        genera[i]], col = genera.colors.1[i], pch = 19, cex = cex.I[i])
}
axis(1, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
axis(2, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
axis(3, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
axis(4, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
box(lwd = 2)
rect(30, 30, 60, 60, border = "black", lty = "solid", lwd = 1)

mtext("Ridge", side = 3, font = 2, padj = -1, cex = 1)

genera.colors.1 <- c("firebrick4", "coral", "black", "black", "black", "black", 
    "black", "black", "black", "olivedrab3", "black", "tan4", "black", "darkgreen")

genera.rank <- c(14, 2, 1, 12, 10, 7, 9, 4, 11, 8, 3, 6, 5, 13)

image(1:6, 1, as.matrix(1:6), col = genera.colors.1[genera.rank[1:6]], xlab = "", 
    ylab = "", xaxt = "n", yaxt = "n", bty = "n")
blacks <- c(2, 5)
whites <- c(1, 3, 4)
for (i in whites) text(i, 1, genera[genera.rank][i], font = 4, col = "white", 
    srt = 90)
for (i in blacks) text(i, 1, genera[genera.rank][i], font = 4, col = "black", 
    srt = 90)
text(6, 1, "Other", font = 2, col = "white", srt = 90)
#```

#![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-41.png) 

#```r

dev.off() 
#end hemlock



# begin hardwoods

pdf(file='hardwood_countors.pdf', width=4.75, height=6, colormodel='cmyk',
  pointsize=9)

layout(matrix(c(2, 1, 0, 0, 3, 3), 3, 2, byrow = TRUE), widths = rep(lcm(4.08), 
    2), heights = c(rep(lcm(4.08), 1), lcm(0.75), lcm(1.5)))
# layout.show(3)

par(pin = c(1.75, 1.75), plt = c(0.0625, 0.9375, 0.0625, 0.9375))

filled.contour3(p.7.i, xlim = c(0, 90), ylim = c(0, 90), axes = FALSE, nlevels = 5)

for (i in 1:length(genera)) {
    points(plot.7$xcoord[plot.7$genus == genera[i]], plot.7$ycoord[plot.7$genus == 
        genera[i]], col = genera.colors.1[i], pch = 19, cex = cex.I[i])
}
axis(1, at = seq(0, 80, 20), labels = TRUE, tcl = -0.25, lwd = 2, font.axis = 2)
axis(2, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2, font.axis = 2)
axis(3, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
axis(4, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
box(lwd = 2)
rect(30, 30, 60, 60, border = "black", lty = "solid", lwd = 1)
mtext("Meters", side = 1, font = 2, padj = 3, adj = -0.35, cex = 0.85)
mtext("Ridge", side = 3, font = 2, padj = -1, cex = 1)
filled.contour3(p.8.i, xlim = c(0, 90), ylim = c(0, 90), axes = FALSE, nlevels = 5)

for (i in 1:length(genera)) {
    points(plot.8$xcoord[plot.8$genus == genera[i]], plot.8$ycoord[plot.8$genus == 
        genera[i]], col = genera.colors.1[i], pch = 19, cex = cex.I[i])
}
axis(1, at = seq(0, 80, 20), labels = TRUE, tcl = -0.25, lwd = 2, font.axis = 2)
axis(2, at = seq(0, 80, 20), labels = TRUE, tcl = -0.25, lwd = 2, font.axis = 2)
axis(3, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
axis(4, at = seq(0, 80, 20), labels = FALSE, tcl = -0.25, lwd = 2)
box(lwd = 2)
rect(30, 30, 60, 60, border = "black", lty = "solid", lwd = 1)
mtext("Meters", side = 2, font = 2, padj = -3, cex = 0.85)
mtext("Valley", side = 3, font = 2, padj = -1, cex = 1)

genera.colors.1 <- c("firebrick4", "coral", "black", "black", "black", "black", 
    "black", "black", "black", "olivedrab3", "black", "tan4", "black", "darkgreen")

genera.rank <- c(14, 2, 1, 12, 10, 7, 9, 4, 11, 8, 3, 6, 5, 13)

image(1:6, 1, as.matrix(1:6), col = genera.colors.1[genera.rank[1:6]], xlab = "", 
    ylab = "", xaxt = "n", yaxt = "n", bty = "n")
blacks <- c(2, 5)
whites <- c(1, 3, 4)
for (i in whites) text(i, 1, genera[genera.rank][i], font = 4, col = "white", 
    srt = 90)
for (i in blacks) text(i, 1, genera[genera.rank][i], font = 4, col = "black", 
    srt = 90)
text(6, 1, "Other", font = 2, col = "white", srt = 90)

dev.off()
#```

#![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-42.png) 


#Compute Ripley's K for each species, each plot
ddg.procedure("Compute Ripley's K for each species, each plot")
#First, create sptaial point patterns; remove points with x or y outside of c(0,90)


#```r

plot.1 <- plot.1[plot.1[, 9] >= 0 & plot.1[, 9] <= 90, ]
plot.1 <- plot.1[plot.1[, 10] >= 0 & plot.1[, 10] <= 90, ]
plot.1.pp <- ppp(plot.1[, 9], plot.1[, 10], c(0, 90), c(0, 90))
#```

#```
## Warning: data contain duplicated points
#```

#```r

plot.2 <- plot.2[plot.2[, 9] >= 0 & plot.2[, 9] <= 90, ]
plot.2 <- plot.2[plot.2[, 10] >= 0 & plot.2[, 10] <= 90, ]
plot.2.pp <- ppp(plot.2[, 9], plot.2[, 10], c(0, 90), c(0, 90))
#```

#```
## Warning: data contain duplicated points
#```

#```r

plot.3 <- plot.3[plot.3[, 9] >= 0 & plot.3[, 9] <= 90, ]
plot.3 <- plot.3[plot.3[, 10] >= 0 & plot.3[, 10] <= 90, ]
plot.3.pp <- ppp(plot.3[, 9], plot.3[, 10], c(0, 90), c(0, 90))
#```

#```
## Warning: data contain duplicated points
#```

#```r

plot.4 <- plot.4[plot.4[, 9] >= 0 & plot.4[, 9] <= 90, ]
plot.4 <- plot.4[plot.4[, 10] >= 0 & plot.4[, 10] <= 90, ]
plot.4.pp <- ppp(plot.4[, 9], plot.4[, 10], c(0, 90), c(0, 90))
#```

#```
## Warning: data contain duplicated points
#```

#```r

plot.5 <- plot.5[plot.5[, 9] >= 0 & plot.5[, 9] <= 90, ]
plot.5 <- plot.5[plot.5[, 10] >= 0 & plot.5[, 10] <= 90, ]
plot.5.pp <- ppp(plot.5[, 9], plot.5[, 10], c(0, 90), c(0, 90))
#```

#```
## Warning: data contain duplicated points
#```

#```r

plot.6 <- plot.6[plot.6[, 9] >= 0 & plot.6[, 9] <= 90, ]
plot.6 <- plot.6[plot.6[, 10] >= 0 & plot.6[, 10] <= 90, ]
plot.6.pp <- ppp(plot.6[, 9], plot.6[, 10], c(0, 90), c(0, 90))
#```

#```
## Warning: data contain duplicated points
#```

#```r

plot.7 <- plot.7[plot.7[, 9] >= 0 & plot.7[, 9] <= 90, ]
plot.7 <- plot.7[plot.7[, 10] >= 0 & plot.7[, 10] <= 90, ]
plot.7.pp <- ppp(plot.7[, 9], plot.7[, 10], c(0, 90), c(0, 90))
#```

#```
## Warning: data contain duplicated points
#```

#```r

plot.8 <- plot.8[plot.8[, 9] >= 0 & plot.8[, 9] <= 90, ]
plot.8 <- plot.8[plot.8[, 10] >= 0 & plot.8[, 10] <= 90, ]
plot.8.pp <- ppp(plot.8[, 9], plot.8[, 10], c(0, 90), c(0, 90))
#```

#```
## Warning: data contain duplicated points
#```

#Now, plot, generate contours, and Kobs vs Ktheor


#```r
par(mfrow = c(5, 3), pin = c(1.75, 1.75), plt = c(0.0625, 0.9375, 0.0625, 0.9375))


plot(plot.1.pp[plot.1$genus == "Tsuga"])
plot(density(plot.1.pp[plot.1$genus == "Tsuga"]))
plot(envelope(plot.1.pp[plot.1$genus == "Tsuga"], Lest, global = FALSE))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  L[obs](r)
## theo   2   2 theo L[theo](r)
## hi     1   8   hi   L[hi](r)
## lo     1   8   lo   L[lo](r)
##                                                meaning
## obs            observed value of L(r) for data pattern
## theo                 theoretical value of L(r) for CSR
## hi   upper pointwise envelope of L(r) from simulations
## lo   lower pointwise envelope of L(r) from simulations
#```

#```r

plot(plot.1.pp[plot.1$genus == "Betula"])
plot(density(plot.1.pp[plot.1$genus == "Betula"]))
plot(envelope(plot.1.pp[plot.1$genus == "Betula"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r


plot(plot.1.pp[plot.1$genus == "Quercus"])
plot(density(plot.1.pp[plot.1$genus == "Quercus"]))
plot(envelope(plot.1.pp[plot.1$genus == "Quercus"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.1.pp[plot.1$genus == "Acer"])
plot(density(plot.1.pp[plot.1$genus == "Acer"]))
plot(envelope(plot.1.pp[plot.1$genus == "Acer"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r


plot(plot.1.pp[plot.1$genus == "Pinus"])
plot(density(plot.1.pp[plot.1$genus == "Pinus"]))
plot(envelope(plot.1.pp[plot.1$genus == "Pinus"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-61.png) 

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

par(mfrow = c(5, 3), pin = c(1.75, 1.75), plt = c(0.0625, 0.9375, 0.0625, 0.9375))
plot(plot.2.pp[plot.2$genus == "Tsuga"])
plot(density(plot.2.pp[plot.2$genus == "Tsuga"]))
plot(envelope(plot.2.pp[plot.2$genus == "Tsuga"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.2.pp[plot.2$genus == "Betula"])
plot(density(plot.2.pp[plot.2$genus == "Betula"]))
plot(envelope(plot.2.pp[plot.2$genus == "Betula"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.2.pp[plot.2$genus == "Quercus"])
plot(density(plot.2.pp[plot.2$genus == "Quercus"]))
plot(envelope(plot.2.pp[plot.2$genus == "Quercus"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.2.pp[plot.2$genus == "Acer"])
plot(density(plot.2.pp[plot.2$genus == "Acer"]))
plot(envelope(plot.2.pp[plot.2$genus == "Acer"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r
ddg.grabhistory()
plot(plot.2.pp[plot.2$genus == "Pinus"])
plot(density(plot.2.pp[plot.2$genus == "Pinus"]))
plot(envelope(plot.2.pp[plot.2$genus == "Pinus"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-62.png) 

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

par(mfrow = c(5, 3), pin = c(1.75, 1.75), plt = c(0.0625, 0.9375, 0.0625, 0.9375))
plot(plot.3.pp[plot.3$genus == "Tsuga"])
plot(density(plot.3.pp[plot.3$genus == "Tsuga"]))
plot(envelope(plot.3.pp[plot.3$genus == "Tsuga"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.3.pp[plot.3$genus == "Betula"])
plot(density(plot.3.pp[plot.3$genus == "Betula"]))
plot(envelope(plot.3.pp[plot.3$genus == "Betula"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.3.pp[plot.3$genus == "Quercus"])
plot(density(plot.3.pp[plot.3$genus == "Quercus"]))
plot(envelope(plot.3.pp[plot.3$genus == "Quercus"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.3.pp[plot.3$genus == "Acer"])
plot(density(plot.3.pp[plot.3$genus == "Acer"]))
plot(envelope(plot.3.pp[plot.3$genus == "Acer"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.3.pp[plot.3$genus == "Pinus"])
plot(density(plot.3.pp[plot.3$genus == "Pinus"]))
plot(envelope(plot.3.pp[plot.3$genus == "Pinus"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-63.png) 

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

par(mfrow = c(5, 3), pin = c(1.75, 1.75), plt = c(0.0625, 0.9375, 0.0625, 0.9375))
plot(plot.4.pp[plot.4$genus == "Tsuga"])
plot(density(plot.4.pp[plot.4$genus == "Tsuga"]))
plot(envelope(plot.4.pp[plot.4$genus == "Tsuga"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.4.pp[plot.4$genus == "Betula"])
plot(density(plot.4.pp[plot.4$genus == "Betula"]))
plot(envelope(plot.4.pp[plot.4$genus == "Betula"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.4.pp[plot.4$genus == "Quercus"])
plot(density(plot.4.pp[plot.4$genus == "Quercus"]))
plot(envelope(plot.4.pp[plot.4$genus == "Quercus"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.4.pp[plot.4$genus == "Acer"])
plot(density(plot.4.pp[plot.4$genus == "Acer"]))
plot(envelope(plot.4.pp[plot.4$genus == "Acer"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.4.pp[plot.4$genus == "Pinus"])
plot(density(plot.4.pp[plot.4$genus == "Pinus"]))
plot(envelope(plot.4.pp[plot.4$genus == "Pinus"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-64.png) 

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.5.pp[plot.5$genus == "Tsuga"])
plot(density(plot.5.pp[plot.5$genus == "Tsuga"]))
plot(envelope(plot.5.pp[plot.5$genus == "Tsuga"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```
ddg.grabhistory()
#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.5.pp[plot.5$genus == "Betula"])
plot(density(plot.5.pp[plot.5$genus == "Betula"]))
plot(envelope(plot.5.pp[plot.5$genus == "Betula"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.5.pp[plot.5$genus == "Quercus"])
plot(density(plot.5.pp[plot.5$genus == "Quercus"]))
plot(envelope(plot.5.pp[plot.5$genus == "Quercus"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.5.pp[plot.5$genus == "Acer"])
plot(density(plot.5.pp[plot.5$genus == "Acer"]))
plot(envelope(plot.5.pp[plot.5$genus == "Acer"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.5.pp[plot.5$genus == "Pinus"])
plot(density(plot.5.pp[plot.5$genus == "Pinus"]))
plot(envelope(plot.5.pp[plot.5$genus == "Pinus"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-65.png) 

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r


plot(plot.6.pp[plot.6$genus == "Tsuga"])
plot(density(plot.6.pp[plot.6$genus == "Tsuga"]))
plot(envelope(plot.6.pp[plot.6$genus == "Tsuga"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.6.pp[plot.6$genus == "Betula"])
plot(density(plot.6.pp[plot.6$genus == "Betula"]))
plot(envelope(plot.6.pp[plot.6$genus == "Betula"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.6.pp[plot.6$genus == "Quercus"])
plot(density(plot.6.pp[plot.6$genus == "Quercus"]))
plot(envelope(plot.6.pp[plot.6$genus == "Quercus"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.6.pp[plot.6$genus == "Acer"])
plot(density(plot.6.pp[plot.6$genus == "Acer"]))
plot(envelope(plot.6.pp[plot.6$genus == "Acer"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.6.pp[plot.6$genus == "Pinus"])
plot(density(plot.6.pp[plot.6$genus == "Pinus"]))
plot(envelope(plot.6.pp[plot.6$genus == "Pinus"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-66.png) 

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.7.pp[plot.7$genus == "Tsuga"])
plot(density(plot.7.pp[plot.7$genus == "Tsuga"]))
plot(envelope(plot.7.pp[plot.7$genus == "Tsuga"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.7.pp[plot.7$genus == "Betula"])
plot(density(plot.7.pp[plot.7$genus == "Betula"]))
plot(envelope(plot.7.pp[plot.7$genus == "Betula"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.7.pp[plot.7$genus == "Quercus"])
plot(density(plot.7.pp[plot.7$genus == "Quercus"]))
plot(envelope(plot.7.pp[plot.7$genus == "Quercus"], Kest))
#```
ddg.grabhistory()
#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.7.pp[plot.7$genus == "Acer"])
plot(density(plot.7.pp[plot.7$genus == "Acer"]))
plot(envelope(plot.7.pp[plot.7$genus == "Acer"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.7.pp[plot.7$genus == "Pinus"])
plot(density(plot.7.pp[plot.7$genus == "Pinus"]))
plot(envelope(plot.7.pp[plot.7$genus == "Pinus"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-67.png) 

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.8.pp[plot.8$genus == "Tsuga"])
plot(density(plot.8.pp[plot.8$genus == "Tsuga"]))
plot(envelope(plot.8.pp[plot.8$genus == "Tsuga"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.8.pp[plot.8$genus == "Betula"])
plot(density(plot.8.pp[plot.8$genus == "Betula"]))
plot(envelope(plot.8.pp[plot.8$genus == "Betula"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.8.pp[plot.8$genus == "Quercus"])
plot(density(plot.8.pp[plot.8$genus == "Quercus"]))
plot(envelope(plot.8.pp[plot.8$genus == "Quercus"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.8.pp[plot.8$genus == "Acer"])
plot(density(plot.8.pp[plot.8$genus == "Acer"]))
plot(envelope(plot.8.pp[plot.8$genus == "Acer"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```

#```r

plot(plot.8.pp[plot.8$genus == "Pinus"])
plot(density(plot.8.pp[plot.8$genus == "Pinus"]))
plot(envelope(plot.8.pp[plot.8$genus == "Pinus"], Kest))
#```

#```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
## 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
## 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
## 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
## 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
#```

#![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-68.png) 

#```
##      lty col  key      label
## obs    1   1  obs  K[obs](r)
## theo   2   2 theo K[theo](r)
## hi     1   8   hi   K[hi](r)
## lo     1   8   lo   K[lo](r)
##                                                meaning
## obs            observed value of K(r) for data pattern
## theo                 theoretical value of K(r) for CSR
## hi   upper pointwise envelope of K(r) from simulations
## lo   lower pointwise envelope of K(r) from simulations
#```


#Next, run maximum absolute deviation tests of CSR on ppp data
ddg.procedure("run maximum absolute deviation tests of CSR on ppp data")

#```r

mad.test(plot.1.pp[plot.1$genus == "Tsuga"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.1.pp[plot.1$genus == "Tsuga"]
## mad = 1.016, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.1.pp[plot.1$genus == "Betula"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.1.pp[plot.1$genus == "Betula"]
## mad = 17, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.1.pp[plot.1$genus == "Quercus"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.1.pp[plot.1$genus == "Quercus"]
## mad = 5.256, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.1.pp[plot.1$genus == "Acer"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.1.pp[plot.1$genus == "Acer"]
## mad = 6.744, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.1.pp[plot.1$genus == "Pinus"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.1.pp[plot.1$genus == "Pinus"]
## mad = 1.467, rank = 24, p-value = 0.24
#```

#```r

mad.test(plot.2.pp[plot.2$genus == "Tsuga"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.2.pp[plot.2$genus == "Tsuga"]
## mad = 1.196, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.2.pp[plot.2$genus == "Betula"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.2.pp[plot.2$genus == "Betula"]
## mad = 3.826, rank = 10, p-value = 0.1
#```

#```r
mad.test(plot.2.pp[plot.2$genus == "Quercus"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.2.pp[plot.2$genus == "Quercus"]
## mad = 2.451, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.2.pp[plot.2$genus == "Acer"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.2.pp[plot.2$genus == "Acer"]
## mad = 1.653, rank = 5, p-value = 0.05
#```

#```r
mad.test(plot.2.pp[plot.2$genus == "Pinus"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.2.pp[plot.2$genus == "Pinus"]
## mad = 3.442, rank = 1, p-value = 0.01
#```

#```r

mad.test(plot.3.pp[plot.3$genus == "Tsuga"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.3.pp[plot.3$genus == "Tsuga"]
## mad = 1.032, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.3.pp[plot.3$genus == "Betula"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.3.pp[plot.3$genus == "Betula"]
## mad = 3.301, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.3.pp[plot.3$genus == "Quercus"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.3.pp[plot.3$genus == "Quercus"]
## mad = 3.272, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.3.pp[plot.3$genus == "Acer"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.3.pp[plot.3$genus == "Acer"]
## mad = 3.35, rank = 2, p-value = 0.02
#```

#```r
mad.test(plot.3.pp[plot.3$genus == "Pinus"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.3.pp[plot.3$genus == "Pinus"]
## mad = 7.287, rank = 3, p-value = 0.03
#```

#```r

mad.test(plot.4.pp[plot.4$genus == "Tsuga"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.4.pp[plot.4$genus == "Tsuga"]
## mad = 0.6014, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.4.pp[plot.4$genus == "Betula"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.4.pp[plot.4$genus == "Betula"]
## mad = 2.95, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.4.pp[plot.4$genus == "Quercus"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.4.pp[plot.4$genus == "Quercus"]
## mad = 2.418, rank = 4, p-value = 0.04
#```

#```r
mad.test(plot.4.pp[plot.4$genus == "Acer"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.4.pp[plot.4$genus == "Acer"]
## mad = 3.07, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.4.pp[plot.4$genus == "Pinus"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.4.pp[plot.4$genus == "Pinus"]
## mad = 10.7, rank = 17, p-value = 0.17
#```

#```r

mad.test(plot.5.pp[plot.5$genus == "Tsuga"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.5.pp[plot.5$genus == "Tsuga"]
## mad = 0.7566, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.5.pp[plot.5$genus == "Betula"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.5.pp[plot.5$genus == "Betula"]
## mad = 2.039, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.5.pp[plot.5$genus == "Quercus"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.5.pp[plot.5$genus == "Quercus"]
## mad = 2.853, rank = 12, p-value = 0.12
#```

#```r
mad.test(plot.5.pp[plot.5$genus == "Acer"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.5.pp[plot.5$genus == "Acer"]
## mad = 1.97, rank = 3, p-value = 0.03
#```

#```r
mad.test(plot.5.pp[plot.5$genus == "Pinus"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## Error: Some function values were infinite or NaN at distances r up to
## 22.5. Please specify a shorter 'rinterval'
#```
ddg.grabhistory()
#```r

mad.test(plot.6.pp[plot.6$genus == "Tsuga"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.6.pp[plot.6$genus == "Tsuga"]
## mad = 1.098, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.6.pp[plot.6$genus == "Betula"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.6.pp[plot.6$genus == "Betula"]
## mad = 3.468, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.6.pp[plot.6$genus == "Quercus"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.6.pp[plot.6$genus == "Quercus"]
## mad = 4.589, rank = 59, p-value = 0.59
#```

#```r
mad.test(plot.6.pp[plot.6$genus == "Acer"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.6.pp[plot.6$genus == "Acer"]
## mad = 3.882, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.6.pp[plot.6$genus == "Pinus"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.6.pp[plot.6$genus == "Pinus"]
## mad = 11.43, rank = 1, p-value = 0.01
#```

#```r

mad.test(plot.7.pp[plot.7$genus == "Tsuga"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.7.pp[plot.7$genus == "Tsuga"]
## mad = 10.23, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.7.pp[plot.7$genus == "Betula"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.7.pp[plot.7$genus == "Betula"]
## mad = 4.043, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.7.pp[plot.7$genus == "Quercus"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.7.pp[plot.7$genus == "Quercus"]
## mad = 3.823, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.7.pp[plot.7$genus == "Acer"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.7.pp[plot.7$genus == "Acer"]
## mad = 4.794, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.7.pp[plot.7$genus == "Pinus"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.7.pp[plot.7$genus == "Pinus"]
## mad = 2.411, rank = 1, p-value = 0.01
#```

#```r

mad.test(plot.8.pp[plot.8$genus == "Tsuga"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.8.pp[plot.8$genus == "Tsuga"]
## mad = 8.653, rank = 5, p-value = 0.05
#```

#```r
mad.test(plot.8.pp[plot.8$genus == "Betula"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.8.pp[plot.8$genus == "Betula"]
## mad = 1.542, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.8.pp[plot.8$genus == "Quercus"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.8.pp[plot.8$genus == "Quercus"]
## mad = 1.787, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.8.pp[plot.8$genus == "Acer"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.8.pp[plot.8$genus == "Acer"]
## mad = 1.194, rank = 1, p-value = 0.01
#```

#```r
mad.test(plot.8.pp[plot.8$genus == "Pinus"], Lest, verbose = FALSE, nsim = 99)
#```

#```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: L(r)
## 	Reference function: sample mean
## 	Interval of distance values: [0, 22.5]
## 
## data:  plot.8.pp[plot.8$genus == "Pinus"]
## mad = 4.07, rank = 1, p-value = 0.01
#```



#Plot age x dbh, with smoother and marginal histograms - ms. Figure 8
ddg.procedure("Plot age x dbh, with smoother and marginal histograms")

#```r

#### Extract tree ages, combine with species and dbh from trees

attach(dendro)

tree.sprout <- by(dendro[, 6], INDICES = as.factor(dendro$TreeNum), min, simplify = FALSE)

tree.ids <- by(dendro[, 1], INDICES = as.factor(dendro$TreeNum), mean, simplify = FALSE)

sprout <- NULL
for (i in 1:length(tree.sprout)) sprout[i] <- tree.sprout[[i]]

plotnum <- NULL
for (i in 1:length(tree.ids)) plotnum[i] <- tree.ids[[i]]

tree.ids <- as.integer(names(tree.sprout))

tree.ages <- cbind(tree.ids, sprout)

tree.age <- 2003 - tree.ages[, 2]

tree.ages <- cbind(tree.ages, tree.age)

tree.ages <- as.data.frame(tree.ages)

tree.ages[, 4] <- plotnum

names(tree.ages)[4] <- "plot"


## deal with crashes from duplicate records at 27, 119, 230

for (i in 1:26) {
    tree.ages[i, 5] <- trees$species[trees$tree == tree.ages$tree.ids[i]]
}
for (i in 28:117) {
    tree.ages[i, 5] <- trees$species[trees$tree == tree.ages$tree.ids[i]]
}
for (i in 119:229) {
    tree.ages[i, 5] <- trees$species[trees$tree == tree.ages$tree.ids[i]]
}

tree.ages[27, 5] <- "TSCA"
tree.ages[118, 5] <- "PIST"
tree.ages[230, 5] <- "TSCA"

for (i in 1:26) {
    tree.ages[i, 6] <- trees$dbh[trees$tree == tree.ages$tree.ids[i]]
}
for (i in 28:117) {
    tree.ages[i, 6] <- trees$dbh[trees$tree == tree.ages$tree.ids[i]]
}
for (i in 119:229) {
    tree.ages[i, 6] <- trees$dbh[trees$tree == tree.ages$tree.ids[i]]
}

tree.ages[27, 6] <- 15
tree.ages[118, 6] <- 62.5
tree.ages[230, 6] <- 39.5

for (i in 1:26) {
    tree.ages[i, 7] <- trees$genus[trees$tree == tree.ages$tree.ids[i]]
}
for (i in 28:117) {
    tree.ages[i, 7] <- trees$genus[trees$tree == tree.ages$tree.ids[i]]
}
for (i in 119:229) {
    tree.ages[i, 7] <- trees$genus[trees$tree == tree.ages$tree.ids[i]]
}

tree.ages[27, 7] <- "Tsuga"
tree.ages[118, 7] <- "Pinus"
tree.ages[230, 7] <- "Tsuga"

tree.ages[, 8] <- NA

tree.ages[, 8] <- ifelse(tree.ages$plot < 7, tree.ages[, 8] <- "hemlock", tree.ages[, 
    8] <- "hardwood")

tree.ages[, 9] <- NA
tree.ages[, 9] <- ifelse(tree.ages$plot > 3 & tree.ages$plot < 8, tree.ages[, 
    9] <- "Ridge", tree.ages[, 9] <- "Valley")

detach(dendro)

names(tree.ages)[5:9] <- c("species", "dbh", "genus", "init.treat", "block")


head(tree.ages)
#```

#```
##   tree.ids sprout tree.age plot species  dbh  genus init.treat block
## 1        3   1961       42    4    TSCA  5.9  Tsuga    hemlock Ridge
## 2        5   1917       86    4    BELE 11.9 Betula    hemlock Ridge
## 3        9   1953       50    4    TSCA  8.3  Tsuga    hemlock Ridge
## 4       11   1927       76    4    ACRU 16.2   Acer    hemlock Ridge
## 5       21   1946       57    4    TSCA  6.4  Tsuga    hemlock Ridge
## 6       28   1937       66    4    TSCA 18.0  Tsuga    hemlock Ridge
#```


#Create the plot


#```r

#original colors for all genera

genera.colors <- c("firebrick4", "coral", "orange4", "black", "orange", 
  		"darkmagenta", "darkolivegreen1", "yellow", "orange3", "olivedrab3", 
			"red", "tan4", "maroon4", "darkgreen")

age.cols <- genera.colors[c(1,2, 7,9, 10, 12, 14)]

for (i in 1:length(genera)) tree.ages[tree.ages$genus == genera[i],10] <- genera.colors[i]
names(tree.ages)[10] <- "tree.color"

tree.ages <- drop.levels(tree.ages)
tree.ages$init.treat <- as.factor(tree.ages$init.treat)
tree.ages$tree.color <- as.factor(tree.ages$tree.color)
tree.ages$block <- as.factor(tree.ages$block)


vh <- tree.ages[tree.ages$block=="Valley" & tree.ages$init.treat=="hemlock",]
vd <- tree.ages[tree.ages$block=="Valley" & tree.ages$init.treat=="hardwood",]
rh <- tree.ages[tree.ages$block=="Ridge" & tree.ages$init.treat=="hemlock",]
rd <- tree.ages[tree.ages$block=="Ridge" & tree.ages$init.treat=="hardwood",]


#histograms
#age distributions
hist.vh.age <- hist(vh$tree.age[vh$genus=="Tsuga"], plot=FALSE, breaks=seq(10,150,10))
hist.vh.bet.age<- hist(vh$tree.age[vh$genus=="Betula"], plot=FALSE, breaks=seq(10,150,10))
hist.vh.acer.age<- hist(vh$tree.age[vh$genus=="Acer"], plot=FALSE, breaks=seq(10,150,10))
hist.vh.quercus.age<- hist(vh$tree.age[vh$genus=="Quercus"], plot=FALSE, breaks=seq(10,150,10))
hist.vh.pinus.age<- hist(vh$tree.age[vh$genus=="Pinus"], plot=FALSE, breaks=seq(10,150,10))

hist.rh.age <- hist(rh$tree.age[rh$genus=="Tsuga"], plot=FALSE, breaks=seq(10,150,10))
hist.rh.bet.age<- hist(rh$tree.age[rh$genus=="Betula"], plot=FALSE, breaks=seq(10,150,10))
hist.rh.acer.age<- hist(rh$tree.age[rh$genus=="Acer"], plot=FALSE, breaks=seq(10,150,10))
hist.rh.quercus.age<- hist(rh$tree.age[rh$genus=="Quercus"], plot=FALSE, breaks=seq(10,150,10))
hist.rh.pinus.age<- hist(rh$tree.age[rh$genus=="Pinus"], plot=FALSE, breaks=seq(10,150,10))

hist.vd.age <- hist(vd$tree.age[vd$genus=="Tsuga"], plot=FALSE, breaks=seq(10,150,10))
hist.vd.bet.age<- hist(vd$tree.age[vd$genus=="Betula"], plot=FALSE, breaks=seq(10,150,10))
hist.vd.acer.age<- hist(vd$tree.age[vd$genus=="Acer"], plot=FALSE, breaks=seq(10,150,10))
hist.vd.quercus.age<- hist(vd$tree.age[vd$genus=="Quercus"], plot=FALSE, breaks=seq(10,150,10))
hist.vd.pinus.age<- hist(vd$tree.age[vd$genus=="Pinus"], plot=FALSE, breaks=seq(10,150,10))

hist.rd.age <- hist(rd$tree.age[rd$genus=="Tsuga"], plot=FALSE, breaks=seq(10,150,10))
hist.rd.bet.age <- hist(rd$tree.age[rd$genus=="Betula"], plot=FALSE, breaks=seq(10,150,10))
hist.rd.acer.age <- hist(rd$tree.age[rd$genus=="Acer"], plot=FALSE, breaks=seq(10,150,10))
hist.rd.quercus.age<- hist(rd$tree.age[rd$genus=="Quercus"], plot=FALSE, breaks=seq(10,150,10))
hist.rd.pinus.age<- hist(rd$tree.age[rd$genus=="Pinus"], plot=FALSE, breaks=seq(10,150,10))

#dbh distributions

hist.vh.dbh <- hist(vh$dbh[vh$genus=="Tsuga"], plot=FALSE, breaks=seq(0,80,10))
hist.vh.bet.dbh <- hist(vh$dbh[vh$genus=="Betula"], plot=FALSE, breaks=seq(0,80,10))
hist.vh.acer.dbh <- hist(vh$dbh[vh$genus=="Acer"], plot=FALSE, breaks=seq(0,80,10))
hist.vh.quercus.dbh <- hist(vh$dbh[vh$genus=="Quercus"], plot=FALSE, breaks=seq(0,80,10))
hist.vh.pinus.dbh <- hist(vh$dbh[vh$genus=="Pinus"], plot=FALSE, breaks=seq(0,80,10))

hist.rh.dbh <- hist(rh$dbh[rh$genus=="Tsuga"], plot=FALSE, breaks=seq(0,80,10))
hist.rh.bet.dbh <- hist(rh$dbh[rh$genus=="Betula"], plot=FALSE, breaks=seq(0,80,10))
hist.rh.acer.dbh <- hist(rh$dbh[rh$genus=="Acer"], plot=FALSE, breaks=seq(0,80,10))
hist.rh.quercus.dbh <- hist(rh$dbh[rh$genus=="Quercus"], plot=FALSE, breaks=seq(0,80,10))
hist.rh.pinus.dbh <- hist(rh$dbh[rh$genus=="Pinus"], plot=FALSE, breaks=seq(0,80,10))


hist.vd.dbh <- hist(vd$dbh[vd$genus=="Tsuga"], plot=FALSE, breaks=seq(0,80,10))
hist.vd.bet.dbh <- hist(vd$dbh[vd$genus=="Betula"], plot=FALSE, breaks=seq(0,80,10))
hist.vd.acer.dbh <- hist(vd$dbh[vd$genus=="Acer"], plot=FALSE, breaks=seq(0,80,10))
hist.vd.quercus.dbh <- hist(vd$dbh[vd$genus=="Quercus"], plot=FALSE, breaks=seq(0,80,10))
hist.vd.pinus.dbh <- hist(vd$dbh[vd$genus=="Pinus"], plot=FALSE, breaks=seq(0,80,10))

hist.rd.dbh <- hist(rd$dbh[rd$genus=="Tsuga"], plot=FALSE, breaks=seq(0,80,10))
hist.rd.bet.dbh <- hist(rd$dbh[rd$genus=="Betula"], plot=FALSE, breaks=seq(0,80,10))
hist.rd.acer.dbh <- hist(rd$dbh[rd$genus=="Acer"], plot=FALSE, breaks=seq(0,80,10))
hist.rd.quercus.dbh <- hist(rd$dbh[rd$genus=="Quercus"], plot=FALSE, breaks=seq(0,80,10))
hist.rd.pinus.dbh <- hist(rd$dbh[rd$genus=="Pinus"], plot=FALSE, breaks=seq(0,80,10))


vh.ages <- rbind(hist.vh.age$counts, hist.vh.bet.age$counts, hist.vh.quercus.age$counts, 
  		hist.vh.acer.age$counts, hist.vh.pinus.age$counts)
rh.ages <- rbind(hist.rh.age$counts, hist.rh.bet.age$counts, hist.rh.quercus.age$counts, 
			hist.rh.acer.age$counts, hist.rh.pinus.age$counts)
vd.ages <- rbind(hist.vd.age$counts, hist.vd.bet.age$counts, hist.vd.quercus.age$counts, 
			hist.vd.acer.age$counts, hist.vd.pinus.age$counts)
rd.ages <- rbind(hist.rd.age$counts, hist.rd.bet.age$counts, hist.rd.quercus.age$counts, 
			hist.rd.acer.age$counts, hist.rd.pinus.age$counts)


vh.dbh <- rbind(hist.vh.dbh$counts, hist.vh.bet.dbh$counts, hist.vh.quercus.dbh$counts, 
			hist.vh.acer.dbh$counts, hist.vh.pinus.dbh$counts)
rh.dbh <- rbind(hist.rh.dbh$counts, hist.rh.bet.dbh$counts, hist.rh.quercus.dbh$counts, 
			hist.rh.acer.dbh$counts, hist.rh.pinus.dbh$counts)
vd.dbh <- rbind(hist.vd.dbh$counts, hist.vd.bet.dbh$counts, hist.vd.quercus.dbh$counts, 
			hist.vd.acer.dbh$counts, hist.vd.pinus.dbh$counts)
rd.dbh <- rbind(hist.rd.dbh$counts, hist.rd.bet.dbh$counts, hist.rd.quercus.dbh$counts, 
			hist.rd.acer.dbh$counts, hist.rd.pinus.dbh$counts)


#some plotting setup

five.colors <-c("darkgreen","coral", "tan4", "firebrick4", "olivedrab3") 
limits <- list(xlims=c(10,150), ylims=c(0,80))
xpreds <- seq(10,150,1)
xpreds.rev <- sort(xpreds, decreasing=TRUE)

maxcount.ages <- max(c(colSums(vh.ages), colSums(rh.ages), colSums(vd.ages), colSums(rd.ages) ) )
maxcount.dbh <- max(c(colSums(vh.dbh), colSums(rh.dbh), colSums(vd.dbh), colSums(rd.dbh) ) )


pdf(file="Figure 7 - age_dbh.pdf", width=6, height=6.5, colormodel="cmyk")

layout(matrix(c(0, 1, 2, 0, 3, 9, 10, 5, 4, 11, 12, 6, 0, 7, 8, 0), nrow=4, byrow=TRUE), heights=c(lcm(1.5), lcm(5.08), lcm(5.08), lcm(1.5)), widths=c(lcm(1.5), lcm(5.08), lcm(5.08), lcm(1.5)) ) 

#layout.show(12)


#barcharts

par(mar=c(0,1,0,1))
barplot(vh.ages, axes=FALSE, ylim=c(0,maxcount.ages), space=0, col=five.colors)
barplot(rh.ages, axes=FALSE, ylim=c(0,maxcount.ages), space=0, col=five.colors)
par(mar=c(1,0,.5,0))
barplot(vh.dbh, axes=FALSE, xlim=c(maxcount.dbh,0),  space=0, horiz=TRUE, col=five.colors)

mtext("DBH (cm)", side=2, line=.5, adj=-.5, font=2)

par(mar=c(.5,0,1,0))
barplot(vd.dbh, axes=FALSE, xlim=c(maxcount.dbh,0), space=0, horiz=TRUE, col=five.colors)
par(mar=c(1,0,.5,0))
barplot(rh.dbh, axes=FALSE, xlim=c(0,maxcount.dbh), space=0, horiz=TRUE, col=five.colors)
par(mar=c(.5,0,1,0))
barplot(rd.dbh, axes=FALSE, xlim=c(0,maxcount.dbh), space=0, horiz=TRUE, col=five.colors)
par(mar=c(0,1,0,1))
barplot(vd.ages, axes=FALSE, ylim=c(maxcount.ages,0), space=0, col=five.colors)
barplot(rd.ages, axes=FALSE, ylim=c(maxcount.ages,0), space=0, col=five.colors)

mtext("Age (years)", side=1, line=0, adj=-.5, font=2)


#scatterplots



par(mar=c(1,1,.5,1))
plot.vh <- plot(vh$dbh ~ vh$tree.age, data=vh,
			xlim=limits$xlims, ylim=limits$ylims,									#axis ranges
			xlab= "", ylab="", axes=FALSE, pch=19, 									#misc
			col=as.character(vh$tree.color)										#colors		
	)
	axis(1, at=seq(15,145,10), labels=TRUE, tcl=-.25, lwd=2, font.axis=2, cex.axis=0.8, padj=-1.1, hadj=.5)
	axis(4, at=seq(5,75,10), labels=TRUE, tcl=-.25, lwd=2, font.axis=2, las=1, cex.axis=0.8, hadj= .5, padj=.25)
	axis(3, at=seq(15,145,10), labels=FALSE, tcl=-.25, lwd=2, font.axis=2)
	axis(2, at=seq(5,75,10), labels=FALSE, tcl=-.25, lwd=2, font.axis=2)
	box(lwd=2)

	vh.loess <- loess(vh$dbh ~ vh$tree.age, span=1)											#add loess smooth + cis
	pred.vh <- predict(vh.loess, xpreds, se=TRUE)
	lines(xpreds, pred.vh$fit, lty="solid", lwd=2, col="blue")
	y.poly.vh <- c((pred.vh$fit+1.96*pred.vh$se.fit), (pred.vh$fit-1.96*pred.vh$se.fit)[order(xpreds, decreasing=TRUE)])
	x.poly.vh <- c(xpreds, xpreds.rev)
	polygon(x.poly.vh[!is.na(y.poly.vh)], y.poly.vh[!is.na(y.poly.vh)], col="#00009933", border=NA)
	text(20,75,"A", cex=1.5, font=2)
	
	text(15,2,"1990", cex=0.8, font=2)
	text(145, 2, "1860", cex=0.8, font=2)
	text(67, 2, "1938", cex=0.8, font=2)

plot.rh <- plot(rh$dbh ~ rh$tree.age, data=rh,
			xlim=limits$xlims, ylim=limits$ylims,									#axis ranges
			xlab= "", ylab="", axes=FALSE, pch=19, 									#misc
			col=as.character(rh$tree.color)										#colors		
	)
	axis(1, at=seq(15,145,10), labels=TRUE, tcl=-.25, lwd=2, font.axis=2, cex.axis=0.8, padj=-1.1, hadj=.5)
	axis(2, at=seq(5,75,10), labels=FALSE, tcl=-.25, lwd=2, font.axis=2)
	axis(3, at=seq(15,145,10), labels=FALSE, tcl=-.25, lwd=2, font.axis=2)
	axis(4, at=seq(5,75,10), labels=FALSE, tcl=-.25, lwd=2, font.axis=2)
	box(lwd=2)

	rh.loess <- loess(rh$dbh ~ rh$tree.age, span=1)										#add loess smooth + cis
	pred.rh <- predict(rh.loess, xpreds, se=TRUE)
	lines(xpreds, pred.rh$fit, lty="solid", lwd=2, col="blue")
	y.poly.rh <- c((pred.rh$fit+1.96*pred.rh$se.fit), (pred.rh$fit-1.96*pred.rh$se.fit)[order(xpreds, decreasing=TRUE)])
	x.poly.rh <- c(xpreds, xpreds.rev)
	polygon(x.poly.rh[!is.na(y.poly.rh)], y.poly.rh[!is.na(y.poly.rh)], col="#00009933", border=NA)
	text(20,75,"B", cex=1.5, font=2)

	text(15,2,"1990", cex=0.8, font=2)
	text(145, 2, "1860", cex=0.8, font=2)
	text(67, 2, "1938", cex=0.8, font=2)

par(mar=c(.5,1,1,1))
plot.vd <- plot(vd$dbh ~ vd$tree.age, data=vd,
			xlim=limits$xlims, ylim=limits$ylims,									#axis ranges
			xlab= "", ylab="", axes=FALSE, pch=19, 									#misc
			col=as.character(vd$tree.color)										#colors		
	)
	axis(1, at=seq(15,145,10), labels=FALSE, tcl=-.25, lwd=2, font.axis=2)
	axis(4, at=seq(5,75,10), labels=TRUE, tcl=-.25, lwd=2, font.axis=2, cex.axis=0.8, las=1, hadj= .5, padj=.25)
	axis(3, at=seq(15,145,10), labels=FALSE, tcl=-.25, lwd=2, font.axis=2)
	axis(2, at=seq(5,75,10), labels=FALSE, tcl=-.25, lwd=2, font.axis=2)
	box(lwd=2)

	vd.loess <- loess(vd$dbh ~ vd$tree.age, span=1)											#add loess smooth + cis
	pred.vd <- predict(vd.loess, xpreds, se=TRUE)
	lines(xpreds, pred.vd$fit, lty="solid", lwd=2, col="blue")
	y.poly.vd <- c((pred.vd$fit+1.96*pred.vd$se.fit), (pred.vd$fit-1.96*pred.vd$se.fit)[order(xpreds, decreasing=TRUE)])
	x.poly.vd <- c(xpreds, xpreds.rev)
	polygon(x.poly.vd[!is.na(y.poly.vd)], y.poly.vd[!is.na(y.poly.vd)], col="#00009933", border=NA)
	text(20,75,"C", cex=1.5, font=2)

	text(15,2,"1990", cex=0.8, font=2)
	text(145, 2, "1860", cex=0.8, font=2)
	text(67, 2, "1938", cex=0.8, font=2)
	
plot.rd <- plot(rd$dbh ~ rd$tree.age, data=rd,
			xlim=limits$xlims, ylim=limits$ylims,									#axis ranges
			xlab= "", ylab="", axes=FALSE, pch=19, 									#misc
			col=as.character(rd$tree.color)										#colors		
	)
	axis(1, at=seq(15,145,10), labels=FALSE, tcl=-.25, lwd=2, font.axis=2)
	axis(2, at=seq(5,75,10), labels=FALSE, tcl=-.25, lwd=2, font.axis=2)
	axis(3, at=seq(15,145,10), labels=FALSE, tcl=-.25, lwd=2, font.axis=2)
	axis(4, at=seq(5,75,10), labels=FALSE, tcl=-.25, lwd=2, font.axis=2)
	box(lwd=2)

	rd.loess <- loess(rd$dbh ~ rd$tree.age, span=1)										#add loess smooth + cis
	pred.rd <- predict(rd.loess, xpreds, se=TRUE)
	lines(xpreds, pred.rd$fit, lty="solid", lwd=2, col="blue")
	y.poly.rd <- c((pred.rd$fit+1.96*pred.rd$se.fit), (pred.rd$fit-1.96*pred.rd$se.fit)[order(xpreds, decreasing=TRUE)])
	x.poly.rd <- c(xpreds, xpreds.rev)
	polygon(x.poly.rd[!is.na(y.poly.rd)], y.poly.rd[!is.na(y.poly.rd)], col="#00009933", border=NA)
	text(20,75,"D", cex=1.5, font=2)

	text(15,2,"1990", cex=0.8, font=2)
	text(145, 2, "1860", cex=0.8, font=2)
	text(67, 2, "1938", cex=0.8, font=2)
#```

#![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

#```r

dev.off()
#```


#Enviromental data - ms. Figure 7 now Figure 4
ddg.procedure("Environmental data")
ddg.save()

#Read in data from a variety of sources:
#Climate data: 
#  Amherst, MA 1893-2012 (http://www.ncdc.noaa.gov/cdo.web); 1896 is missing!!!
  
#  Harvard Forest 1964-2012 (2 files; Shaler 1964-2002 daily and Fisher 2001 to present monthly) (http://harvardforest.fas.harvard.edu/data-archive)
  
#  PDSI 481 - 2003 (http://www.ncdc.noaa.gov/paleo/pdsidata.html)
  
#  population data from HF1980-13 and US Census bureau

# 
# 
# #Harvard Forest climate data
# 
# #```r
# hf.Shaler <- read.csv("C:/Users/aellison/Dropbox/Aaron's Briefcase/manuscript - Simes dendro and land use/env data/hf000-01-daily-m.csv", 
#     header = TRUE)
# 
# head(hf.Shaler)
# #```
# 
# #```
# ##       Date AirT AirTmax AirTmin Prec
# ## 1 1/1/1964  -14      -6     -21  0.0
# ## 2 1/2/1964   -7       0     -14  6.4
# ## 3 1/3/1964   -2       1      -4  0.0
# ## 4 1/4/1964    2       6      -2  0.0
# ## 5 1/5/1964    0       4      -4  0.0
# ## 6 1/6/1964   -6       1     -12  0.0
# #```
# 
# #```r
# summary(hf.Shaler)
# #```
# 
# #```
# ##        Date            AirT          AirTmax         AirTmin     
# ##  1/1/1964:    1   Min.   :-24.0   Min.   :-20.0   Min.   :-33.0  
# ##  1/1/1965:    1   1st Qu.: -1.0   1st Qu.:  4.0   1st Qu.: -6.0  
# ##  1/1/1966:    1   Median :  8.0   Median : 14.0   Median :  2.0  
# ##  1/1/1967:    1   Mean   :  7.3   Mean   : 13.1   Mean   :  1.5  
# ##  1/1/1968:    1   3rd Qu.: 16.0   3rd Qu.: 23.0   3rd Qu.: 10.0  
# ##  1/1/1969:    1   Max.   : 28.0   Max.   : 36.0   Max.   : 24.0  
# ##  (Other) :14055   NA's   :489     NA's   :288     NA's   :384    
# ##       Prec       
# ##  Min.   :  0.00  
# ##  1st Qu.:  0.00  
# ##  Median :  0.00  
# ##  Mean   :  3.01  
# ##  3rd Qu.:  1.50  
# ##  Max.   :119.40  
# ##  NA's   :148
# #```
# 
# #```r
# str(hf.Shaler)
# #```
# 
# #```
# ## 'data.frame':	14061 obs. of  5 variables:
# ##  $ Date   : Factor w/ 14061 levels "1/1/1964","1/1/1965",..: 1 430 859 976 1015 1054 1093 1132 1171 40 ...
# ##  $ AirT   : int  -14 -7 -2 2 0 -6 -1 -5 -6 -4 ...
# ##  $ AirTmax: int  -6 0 1 6 4 1 4 3 -1 1 ...
# ##  $ AirTmin: int  -21 -14 -4 -2 -4 -12 -6 -12 -11 -9 ...
# ##  $ Prec   : num  0 6.4 0 0 0 0 0 0 10.2 0 ...
# #```
# 
# #```r
# hf.Shaler$Date <- as.character(hf.Shaler$Date)
# 
# a <- as.Date(hf.Shaler$Date, "%m/%d/%Y")
# 
# hf.Shaler <- cbind(a, hf.Shaler)
# names(hf.Shaler)[1] <- "date"
# head(hf.Shaler)
# #```
# 
# #```
# ##         date     Date AirT AirTmax AirTmin Prec
# ## 1 1964-01-01 1/1/1964  -14      -6     -21  0.0
# ## 2 1964-01-02 1/2/1964   -7       0     -14  6.4
# ## 3 1964-01-03 1/3/1964   -2       1      -4  0.0
# ## 4 1964-01-04 1/4/1964    2       6      -2  0.0
# ## 5 1964-01-05 1/5/1964    0       4      -4  0.0
# ## 6 1964-01-06 1/6/1964   -6       1     -12  0.0
# #```
# 
# #```r
# str(hf.Shaler)
# #```
# 
# #```
# ## 'data.frame':	14061 obs. of  6 variables:
# ##  $ date   : Date, format: "1964-01-01" "1964-01-02" ...
# ##  $ Date   : chr  "1/1/1964" "1/2/1964" "1/3/1964" "1/4/1964" ...
# ##  $ AirT   : int  -14 -7 -2 2 0 -6 -1 -5 -6 -4 ...
# ##  $ AirTmax: int  -6 0 1 6 4 1 4 3 -1 1 ...
# ##  $ AirTmin: int  -21 -14 -4 -2 -4 -12 -6 -12 -11 -9 ...
# ##  $ Prec   : num  0 6.4 0 0 0 0 0 0 10.2 0 ...
# #```
# 
# #```r
# 
# hf.Fisher <- read.csv("C:/Users/aellison/Dropbox/Aaron's Briefcase/manuscript - Simes dendro and land use/env data/hf001-02-annual-m.csv", 
#     header = TRUE)
# 
# Fisher.2002 <- hf.Fisher[1:2, c(2, 3, 4, 7)]
# #```
# 
# 
# #Take Fisher met data (1964 to mid-2002) and get mean temp values and total annual precip. Then, replace 2002 Fisher met data (incomplete) with Shaler met data for 2002; append 2003 data from Fisher
# 
# 
# #```r
# Shaler.temps <- aggregate(hf.Shaler[, 3:5], list(year = cut(hf.Shaler$date, 
#     "years", right = TRUE)), mean, na.rm = TRUE)
# 
# Shaler.precip <- aggregate(hf.Shaler[, 6], list(year = cut(hf.Shaler$date, "years", 
#     right = TRUE)), sum, na.rm = TRUE)
# 
# Shaler.annual <- merge(Shaler.temps, Shaler.precip)
# names(Shaler.annual)[5] <- "Precip"
# 
# HF.annual <- Shaler.annual
# HF.annual$year <- as.character(HF.annual$year)
# HF.annual[39:40, 2:5] <- Fisher.2002[, 1:4]
# HF.annual[40, 1] <- "2003-01-01"
# HF.annual
# #```
# 
# #```
# ##          year  AirT AirTmax  AirTmin Precip
# ## 1  1964-01-01 7.050   13.32  0.78779  689.2
# ## 2  1965-01-01 7.269   13.17  0.95251  622.1
# ## 3  1966-01-01 6.857   13.11  0.61157  806.1
# ## 4  1967-01-01 6.319   12.11  0.57534 1119.7
# ## 5  1968-01-01 6.638   12.55  0.72981  883.7
# ## 6  1969-01-01 7.039   12.59  1.54270 1014.0
# ## 7  1970-01-01 6.371   12.13  0.68000  866.3
# ## 8  1971-01-01 6.888   12.66  1.15890  993.5
# ## 9  1972-01-01 6.449   11.56  1.25344 1266.1
# ## 10 1973-01-01 8.208   13.72  2.70083 1198.4
# ## 11 1974-01-01 6.964   12.58  1.39452 1082.6
# ## 12 1975-01-01 7.540   13.05  2.00000 1349.8
# ## 13 1976-01-01 6.598   12.31  0.96175 1113.6
# ## 14 1977-01-01 7.168   12.51  1.80495 1289.8
# ## 15 1978-01-01 6.562   12.29  0.81370  966.5
# ## 16 1979-01-01 7.808   13.03  2.50411 1424.5
# ## 17 1980-01-01 7.126   12.84  1.43169  859.0
# ## 18 1981-01-01 7.462   12.96  1.90659 1224.1
# ## 19 1982-01-01 6.995   12.82  1.15659 1111.6
# ## 20 1983-01-01 8.200   13.67  2.64932 1379.7
# ## 21 1984-01-01 7.885   13.27  2.41644 1282.5
# ## 22 1985-01-01 7.478   13.23  1.65746  944.2
# ## 23 1986-01-01 7.088   12.79  1.37260 1163.6
# ## 24 1987-01-01 7.556   13.42  1.67218 1003.1
# ## 25 1988-01-01 7.298   13.20  1.29670 1044.7
# ## 26 1989-01-01 7.482   13.08  1.81818 1309.0
# ## 27 1990-01-01 9.181   14.99  3.33973 1184.1
# ## 28 1991-01-01 8.796   14.75  2.86777 1380.6
# ## 29 1992-01-01 6.307   11.92  0.93036 1110.3
# ## 30 1993-01-01 7.387   13.65  0.79006 1277.4
# ## 31 1994-01-01 6.452   12.80 -0.07541 1227.3
# ## 32 1995-01-01 8.858   13.27  1.80080 1029.2
# ## 33 1996-01-01 7.558   12.85  1.65507 1379.8
# ## 34 1997-01-01 7.201   13.07  0.79630  907.1
# ## 35 1998-01-01 8.576   14.95  1.91395  954.4
# ## 36 1999-01-01 8.429   14.74  1.84150 1033.5
# ## 37 2000-01-01 6.909   12.69  1.16022 1080.1
# ## 38 2001-01-01 8.068   14.17  1.94366  808.2
# ## 39 2002-01-01 8.400   14.00  3.50000 1111.0
# ## 40 2003-01-01 7.400   12.80  2.40000 1310.0
# #```
# 
# #Amherst climate data
# 
# 
# #```r
# amherst <- read.csv("C:/Users/aellison/Dropbox/Aaron's Briefcase/manuscript - Simes dendro and land use/env data/Amherst_met_1893-2012-reduced.csv", 
#     header = TRUE)
# 
# amherst$AirT <- apply(amherst[, 10:11], 1, mean)
# amherst.temps <- aggregate(amherst[, c(10, 11, 25)], by = list(year = amherst$YEAR), 
#     mean, na.rm = TRUE)
# 
# amherst.precip <- aggregate(amherst[, 7], by = list(year = amherst$YEAR), sum, 
#     na.rm = TRUE)
# 
# amherst.annual <- merge(amherst.temps, amherst.precip)
# names(amherst.annual)[2:5] <- c("AirTmax", "AirTmin", "AirT", "Precip")
# 
# # rescale to deg C and mm as ncdc data are in tenths
# amherst.annual[, 2:5] <- amherst.annual[, 2:5]/10
# 
# head(amherst.annual)
# #```
# 
# #```
# ##   year AirTmax AirTmin  AirT Precip
# ## 1 1893   13.38   1.610 7.507 1038.4
# ## 2 1894   14.73   2.934 8.818  791.1
# ## 3 1895   14.14   2.088 8.092 1031.1
# ## 4 1897   13.66   2.137 7.910 1259.7
# ## 5 1898   14.71   3.612 9.215 1285.9
# ## 6 1899   14.11   2.720 8.389 1056.9
# #```
# 
# #```r
# 
# # need to add in missing year for 1896)
# amherst.annual.fixed <- amherst.annual
# amherst.annual.fixed[5:121, ] <- amherst.annual[4:120, ]
# amherst.annual.fixed[4, ] <- c(1896, NA, NA, NA, NA)
# head(amherst.annual.fixed)
# #```
# 
# #```
# ##   year AirTmax AirTmin  AirT Precip
# ## 1 1893   13.38   1.610 7.507 1038.4
# ## 2 1894   14.73   2.934 8.818  791.1
# ## 3 1895   14.14   2.088 8.092 1031.1
# ## 4 1896      NA      NA    NA     NA
# ## 5 1897   13.66   2.137 7.910 1259.7
# ## 6 1898   14.71   3.612 9.215 1285.9
# #```
# 
# #```r
# dim(amherst.annual.fixed)
# #```
# 
# #```
# ## [1] 121   5
# #```
# 
# #```r
# 
# #```
# 
# 
# 
# 
# #Palmer Drought Severity Index, station 267 (72.5 W, 42.5 N = near Leverett, MA. Data) from 
# 
# #```r
# leverett.pdsi <- read.csv("C:/Users/aellison/Dropbox/Aaron's Briefcase/manuscript - Simes dendro and land use/env data/PDSO - 267.csv", 
#     header = TRUE)
# 
# head(leverett.pdsi)
# #```
# 
# #```
# ##   YEAR NREC NCRN  RECON  X20LP  CRSQ  VRSQ    RE    CE  ACT X20LP.1
# ## 1  481    1    1 -1.737 -0.585 0.106 0.284 0.234 0.134 -100    -100
# ## 2  482    1    1  2.334 -0.631 0.106 0.284 0.234 0.134 -100    -100
# ## 3  483    1    1 -1.308 -0.684 0.106 0.284 0.234 0.134 -100    -100
# ## 4  484    1    1 -0.723 -0.732 0.106 0.284 0.234 0.134 -100    -100
# ## 5  485    1    1 -1.770 -0.761 0.106 0.284 0.234 0.134 -100    -100
# ## 6  486    1    1 -0.708 -0.763 0.106 0.284 0.234 0.134 -100    -100
# #```
# 
# #```r
# summary(leverett.pdsi)
# #```
# 
# #```
# ##       YEAR           NREC             NCRN            RECON       
# ##  Min.   : 481   Min.   :-99.00   Min.   :-99.00   Min.   :-4.450  
# ##  1st Qu.: 862   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:-1.258  
# ##  Median :1242   Median :  2.00   Median :  3.00   Median :-0.261  
# ##  Mean   :1242   Mean   :  2.53   Mean   :  7.16   Mean   :-0.192  
# ##  3rd Qu.:1622   3rd Qu.:  7.00   3rd Qu.:  9.00   3rd Qu.: 0.810  
# ##  Max.   :2003   Max.   :  7.00   Max.   : 29.00   Max.   : 4.070  
# ##      X20LP             CRSQ              VRSQ               RE         
# ##  Min.   :-1.893   Min.   :-100.00   Min.   :-100.00   Min.   :-100.00  
# ##  1st Qu.:-0.606   1st Qu.:   0.11   1st Qu.:   0.28   1st Qu.:   0.23  
# ##  Median :-0.178   Median :   0.20   Median :   0.31   Median :   0.26  
# ##  Mean   :-0.192   Mean   :  -1.41   Mean   :  -1.30   Mean   :  -1.33  
# ##  3rd Qu.: 0.182   3rd Qu.:   0.36   3rd Qu.:   0.42   3rd Qu.:   0.42  
# ##  Max.   : 1.427   Max.   :   0.43   Max.   :   0.44   Max.   :   0.43  
# ##        CE               ACT             X20LP.1       
# ##  Min.   :-100.00   Min.   :-100.00   Min.   :-100.00  
# ##  1st Qu.:   0.13   1st Qu.:-100.00   1st Qu.:-100.00  
# ##  Median :   0.16   Median :-100.00   Median :-100.00  
# ##  Mean   :  -1.39   Mean   : -93.17   Mean   : -93.17  
# ##  3rd Qu.:   0.40   3rd Qu.:-100.00   3rd Qu.:-100.00  
# ##  Max.   :   0.42   Max.   :   3.25   Max.   :   0.97
# #```
# 
# 
# #population data
# ddg.procedure("Population data")
# 
# #```r
# pop.data <- read.csv("C:/Users/aellison/Dropbox/Aaron's Briefcase/manuscript - Simes dendro and land use/env data/phampop.csv")
# #```
# 
# 
# #rework these into time-series objects and plot to check
# ddg.procedure("Rework these into time-series objects and plot to check")
# 
# #```r
# HF.ts <- ts(data = HF.annual[, 2:5], start = 1964, frequency = 1)
# plot(HF.ts)
# #```
# 
# #![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-151.png) 
# 
# #```r
# 
# Amherst.ts <- ts(data = amherst.annual.fixed[-(1:4), 2:5], start = 1897, end = 2003, 
#     frequency = 1)
# plot(Amherst.ts)
# #```
# 
# #![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-152.png) 
# 
# #```r
# 
# 
# pdsi.ts <- ts(data = leverett.pdsi$RECON, start = 481, frequency = 1)
# plot(pdsi.ts, type = "b", xlim = c(1875, 2003))
# #```
# 
# #![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-153.png) 
# 
# #```r
# 
# pop.ts <- ts(data = pop.data$popn, start = 1900, frequency = 0.1)
# plot(pop.ts)
# #```
# 
# #![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-154.png) 
# 
# 
# #build a nice plot
# ddg.procedure("Plot")
# 
# #```r
# pdf('Simes_environment_v3.pdf', width=4, height=6, pointsize=8)
# par(las = 1, mar = c(0, 0, 0, 1) + 0.1, fin = c(3.5, 4.6), yaxs = "r")
# ylimits <- c(min(pdsi.ts) - 4.75, max(Amherst.ts[, 1]) + 22)
# out.limits <- ylimits * 1.04
# 
# plot(Amherst.ts[, 3], ylim = out.limits, xlim = c(1895, 2003), type = "n", axes = FALSE, 
#     xlab = "", ylab = "")
# 
# abline(h = -2, lty = 1, lwd = 1, col = "gray")
# # abline(v=1911, lty=2, lwd=1, col='black') Removed blight for rectangle
# abline(v = 1938, lty = 2, lwd = 1, col = "black")
# abline(v = 1981, lty = 2, lwd = 1, col = "black")
# rect(1910, -11, 1915, 43, col = rgb(255, 192, 203, alpha = 75, max = 255), lty = 0, 
#     lwd = 0)  #Chestnut blight
# rect(1925, -11, 1932, 43, col = rgb(255, 192, 203, alpha = 75, max = 255), lty = 0, 
#     lwd = 0)  #1925-32 logging
# rect(1962, -11, 1966, 43, col = rgb(255, 192, 203, alpha = 75, max = 255), lty = 0, 
#     lwd = 0)  #1960s drought
# # abline(v=1918, lty=2, lwd=1, col='black') Removed WPBR
# 
# 
# lines(pop.ts/100 + 27, lwd = 2, col = "magenta4")
# 
# polygon(c(seq(1964, 2003, 1), seq(2003, 1964, -1)), c(HF.ts[, 3] + 15, rev(HF.ts[, 
#     2] + 15)), col = rgb(0, 0, 255, alpha = 150, max = 255), lty = 0)
# 
# polygon(c(seq(1897, 2003, 1), seq(2003, 1897, -1)), c(Amherst.ts[, 1] + 15, 
#     rev(Amherst.ts[, 2] + 15)), col = rgb(210, 210, 210, alpha = 200, max = 255), 
#     lty = 0)
# 
# lines(rollapply(Amherst.ts[, 3], 10, mean) + 15, lty = 1, lwd = 2)
# lines(Amherst.ts[, 3] + 15, lty = 1, lwd = 1)
# 
# lines(rollapply(HF.ts[, 1], 10, mean) + 15, lty = 1, lwd = 2, col = "blue")
# lines(HF.ts[, 1] + 15, col = "blue", lty = 1, lwd = 1)
# 
# lines(rollapply(Amherst.ts[, 4]/100, 10, mean) - 1, lty = 1, lwd = 2, col = "darkgoldenrod")
# lines(Amherst.ts[, 4]/100 - 1, lwd = 1, col = "darkgoldenrod")
# 
# lines(rollapply(HF.ts[, 4]/100, 10, mean) - 1, lty = 1, lwd = 2, col = "darkgreen")
# lines(HF.ts[, 4]/100 - 1, lwd = 1, col = "darkgreen")
# 
# lines(rollapply(window(pdsi.ts, 1897, 2003), 5, mean) - 2, lty = 1, lwd = 2, 
#     col = "red")
# lines(window(pdsi.ts, 1897, 2003) - 2, lty = 1, lwd = 1, col = "red")
# 
# 
# 
# 
# axis(1, tcl = -0.25, font = 1, padj = -1.5, cex.axis = 0.75)
# axis(1, tcl = -0.125, at = seq(1895, 2005, 5), labels = FALSE)
# axis(2, tcl = -0.25, at = seq(15, 30, 5), labels = c("  0", "  5", "10", "15"), 
#     font = 1, hadj = 0.5, cex.axis = 0.75)
# axis(2, tcl = -0.125, at = seq(15, 30, 1), labels = FALSE)
# 
# axis(4, tcl = -0.25, at = seq(33, 39, 3), labels = c(" 600", " 900", "1200"), 
#     font = 1, cex.axis = 0.75, hadj = 0.25)
# axis(4, tcl = -0.125, at = seq(33, 39, 1), labels = FALSE)
# 
# axis(4, tcl = -0.25, at = seq(5, 15, 5), labels = c("  600", "1100", "1600"), 
#     font = 1, cex.axis = 0.75, hadj = 0.25)
# axis(4, tcl = -0.125, at = seq(5, 15, 1), labels = FALSE)
# 
# axis(2, tcl = -0.25, at = seq(-6, 2, 4), font = 1, labels = c("-4", " 0", " 4"), 
#     hadj = 0.5, cex.axis = 0.75)
# axis(2, tcl = -0.125, at = seq(-6, 2, 1), labels = FALSE)
# 
# text(1892, 31, expression(bold(paste(degree, "C"))), pos = 4, cex = 0.75, font = 2)
# text(1892, 3, "PDSI", pos = 4, cex = 0.75, font = 2, col = "red")
# text(2008, 15, "mm", pos = 2, cex = 0.75, font = 2, col = "darkgreen")
# text(2008, 40, "Population", pos = 2, cex = 0.75, font = 2, col = "magenta4")
# 
# mtext(side = 1, "Year", padj = 2.5, cex = 0.85, font = 2)
# 
# 
# arrows(x0 = 1938, y0 = -8, y1 = -11, lwd = 1, angle = 20, length = 0.1)
# text(1937.5, -8.25, "The Great", pos = 4, cex = 0.75)
# text(1937.5, -9.5, "Hurricane", pos = 4, cex = 0.75)
# arrows(x0 = 1981, y0 = -8, y1 = -11, lwd = 1, angle = 20, length = 0.1)
# text(1980.5, -8.25, "Gypsy", pos = 4, cex = 0.75)
# text(1980.5, -9.5, "moth", pos = 4, cex = 0.75)
# 
# # segments(1925, -10, 1932, -10, lty=1, lwd=2)
# text(1928.5, -8.875, "Logging", cex = 0.75)
# 
# # arrows(x0=1911, y0=-8, y1=-11, lwd=1, angle=20, length=.1)
# text(1912.5, -6.25, "Drought", cex = 0.75)
# text(1912.5, -7.25, "and", cex = 0.65)
# text(1912.5, -8.25, "Chestnut", cex = 0.75)
# text(1912.5, -9.5, "blight", cex = 0.75)
# 
# 
# text(1964, -8.875, "Drought", cex = 0.75)
# #```
# 
# #![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 
# 
# #```r
# dev.off()
# #```
# 
# 
# #Some analysis
# ddg.procedure("Analysis")
# 
# #```r
# # mean temps
# 
# apply(Amherst.ts, 2, mean)
# #```
# 
# #```
# ##  AirTmax  AirTmin     AirT   Precip 
# ##   14.991    2.815    8.901 1084.090
# #```
# 
# #```r
# apply(HF.ts, 2, mean)
# #```
# 
# #```
# ##     AirT  AirTmax  AirTmin   Precip 
# ##    7.395   13.116    1.568 1095.010
# #```
# 
# #```r
# 
# # average annual difference in temperatures
# mean(window(Amherst.ts[, 3], start = 1964) - HF.ts[, 1])
# #```
# 
# #```
# ## [1] 1.687
# #```
# 
# #```r
# mean(window(pdsi.ts, 1897, 2003))
# #```
# 
# #```
# ## [1] -0.09738
# #```
# 
# #```r
# 
# # running means
# plot(rollapply(HF.ts, 10, mean))
# #```
# 
# #![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-171.png) 
# 
# #```r
# plot(rollapply(window(pdsi.ts, start = 1897, end = 2003), 10, mean))
# #```
# 
# #![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-172.png) 
# 
# 
# 
# #```r
# # compare HF and Amherst data during period of overlap
# 
# cor.test(as.vector(window(Amherst.ts[, 3], start = 1964)), as.vector(HF.ts[, 
#     1]))
# #```
# 
# #```
# ## 
# ## 	Pearson's product-moment correlation
# ## 
# ## data:  as.vector(window(Amherst.ts[, 3], start = 1964)) and as.vector(HF.ts[, 1])
# ## t = 5.502, df = 38, p-value = 2.746e-06
# ## alternative hypothesis: true correlation is not equal to 0
# ## 95 percent confidence interval:
# ##  0.4471 0.8095
# ## sample estimates:
# ##    cor 
# ## 0.6659
# #```
# 
# #```r
# 
# lm(as.vector(HF.ts[, 1]) ~ as.vector(window(Amherst.ts[, 3], start = 1964)))
# #```
# 
# #```
# ## 
# ## Call:
# ## lm(formula = as.vector(HF.ts[, 1]) ~ as.vector(window(Amherst.ts[, 
# ##     3], start = 1964)))
# ## 
# ## Coefficients:
# ##                                      (Intercept)  
# ##                                            1.187  
# ## as.vector(window(Amherst.ts[, 3], start = 1964))  
# ##                                            0.684
# #```
# 
# #```r
# 
# cor.test(as.vector(window(Amherst.ts[, 4], start = 1964)), as.vector(HF.ts[, 
#     4]))
# #```
# 
# #```
# ## 
# ## 	Pearson's product-moment correlation
# ## 
# ## data:  as.vector(window(Amherst.ts[, 4], start = 1964)) and as.vector(HF.ts[, 4])
# ## t = 8.8, df = 38, p-value = 1.052e-10
# ## alternative hypothesis: true correlation is not equal to 0
# ## 95 percent confidence interval:
# ##  0.6814 0.9007
# ## sample estimates:
# ##   cor 
# ## 0.819
# #```
# 
# #```r
# 
# lm(as.vector(HF.ts[, 4]) ~ as.vector(window(Amherst.ts[, 4], start = 1964)))
# #```
# 
# #```
# ## 
# ## Call:
# ## lm(formula = as.vector(HF.ts[, 4]) ~ as.vector(window(Amherst.ts[, 
# ##     4], start = 1964)))
# ## 
# ## Coefficients:
# ##                                      (Intercept)  
# ##                                          182.295  
# ## as.vector(window(Amherst.ts[, 4], start = 1964))  
# ##                                            0.824
# #```
# 
# 
# 
# 
# 
# #Add age, radius, and BAI to dendro
# ddg.procedure("Add age, radius, and BAI to dendro")
# 
# #```r
# den <- dendro
# Age <- NULL
# Radius <- NULL
# for (n in unique(den$TreeNum)) {
#     hold <- den[den$TreeNum == n, ]
#     Age <- c(Age, hold$Year - min(hold$Year))
#     Radius <- c(Radius, cumsum(hold$mmperyear))
# }
# 
# BAI <- (pi * Radius^2) - (pi * (Radius - den$mmperyear)^2)
# 
# gpyr <- 1865 + (den$Year - 1861)%/%5 * 5
# 
# GenSpec <- paste(den$Genus, den$Species)
# 
# den <- data.frame(den, GenSpec, gpyr, Age, Radius, BAI)
# 
# birthyr.table <- tapply(den$Year, list(den$TreeNum), min)
# birthyr.TreeNum <- as.numeric(names(birthyr.table))
# birthyr.Year <- as.integer(birthyr.table)
# birthyr <- data.frame(birthyr.TreeNum, birthyr.Year)
# names(birthyr) <- c("TreeNum", "Byear")
# 
# for (n in unique(den$TreeNum)) {
#     len <- length(den[den$Treenum == n, 1])
#     den$Byear[den$TreeNum == n] <- rep(birthyr$Byear[birthyr$TreeNum == n], 
#         len)
# }
# 
# dendro.raw <- den
# 
# # add blocks and canopy as factors, and level in correct order;
# 
# dendro.raw$block <- ifelse(dendro.raw$Plot > 3 & dendro.raw$Plot < 8, "Ridge", 
#     "Valley")
# 
# dendro.raw$block <- as.factor(dendro.raw$block)
# 
# dendro.raw$canopy <- ifelse(dendro.raw$Plot < 7, "Hemlock", "Hardwood")
# dendro.raw$canopy <- as.factor(dendro.raw$canopy)
# 
# dendro.raw$block <- relevel(dendro.raw$block, ref = "Valley")
# dendro.raw$canopy <- relevel(dendro.raw$canopy, ref = "Hemlock")
# 
# dendro.raw$TreeNum <- as.factor(dendro.raw$TreeNum)
# 
# head(dendro.raw)
# #```
# 
# #```
# ##   Plot Genus Species Code TreeNum Year mmperyear     GenSpec gpyr Age
# ## 1    1  Acer  rubrum ACRU     532 1935     1.275 Acer rubrum 1935   0
# ## 2    1  Acer  rubrum ACRU     532 1936     1.519 Acer rubrum 1940   1
# ## 3    1  Acer  rubrum ACRU     532 1937     1.375 Acer rubrum 1940   2
# ## 4    1  Acer  rubrum ACRU     532 1938     0.891 Acer rubrum 1940   3
# ## 5    1  Acer  rubrum ACRU     532 1939     0.737 Acer rubrum 1940   4
# ## 6    1  Acer  rubrum ACRU     532 1940     1.139 Acer rubrum 1940   5
# ##   Radius    BAI  block  canopy
# ## 1  1.275  5.107 Valley Hemlock
# ## 2  2.794 19.418 Valley Hemlock
# ## 3  4.169 30.078 Valley Hemlock
# ## 4  5.060 25.833 Valley Hemlock
# ## 5  5.797 25.138 Valley Hemlock
# ## 6  6.936 45.562 Valley Hemlock
# #```
# 
# #```r
# dim(dendro.raw)
# #```
# 
# #```
# ## [1] 17199    14
# #```
# 
# #```r
# str(dendro.raw)
# #```
# 
# #```
# ## 'data.frame':	17199 obs. of  14 variables:
# ##  $ Plot     : int  1 1 1 1 1 1 1 1 1 1 ...
# ##  $ Genus    : Factor w/ 7 levels "Acer","Betula",..: 1 1 1 1 1 1 1 1 1 1 ...
# ##  $ Species  : Factor w/ 13 levels "alba","allegheniensis",..: 9 9 9 9 9 9 9 9 9 9 ...
# ##  $ Code     : Factor w/ 12 levels "ACRU","ACSA",..: 1 1 1 1 1 1 1 1 1 1 ...
# ##  $ TreeNum  : Factor w/ 230 levels "3","5","9","11",..: 158 158 158 158 158 158 158 158 158 158 ...
# ##  $ Year     : int  1935 1936 1937 1938 1939 1940 1941 1942 1943 1944 ...
# ##  $ mmperyear: num  1.275 1.519 1.375 0.891 0.737 ...
# ##  $ GenSpec  : Factor w/ 13 levels "Acer rubrum",..: 1 1 1 1 1 1 1 1 1 1 ...
# ##  $ gpyr     : num  1935 1940 1940 1940 1940 ...
# ##  $ Age      : int  0 1 2 3 4 5 6 7 8 9 ...
# ##  $ Radius   : num  1.27 2.79 4.17 5.06 5.8 ...
# ##  $ BAI      : num  5.11 19.42 30.08 25.83 25.14 ...
# ##  $ block    : Factor w/ 2 levels "Valley","Ridge": 1 1 1 1 1 1 1 1 1 1 ...
# ##  $ canopy   : Factor w/ 2 levels "Hemlock","Hardwood": 1 1 1 1 1 1 1 1 1 1 ...
# #```
# 
# 
# #Reduce to 5 key species; reorder factors:
# ddg.procedure("Reduce to 5 key species; reorder factors")
# 
# #```r
# dendro.5sp <- dendro.raw[dendro.raw$Code == "TSCA" | dendro.raw$Code == "ACRU" | 
#     dendro.raw$Code == "BELE" | dendro.raw$Code == "QURU" | dendro.raw$Code == 
#     "PIST", ]
# 
# dendro.5sp <- droplevels(dendro.5sp)
# dendro.5sp$Code <- reorder(dendro.5sp$Code, new.order = c(5, 2, 4, 1, 3))
# dendro.5sp$Code <- factor(dendro.5sp$Code, labels = c("Tsuga", "Betula", "Quercus", 
#     "Acer", "Pinus"))
# 
# # relative BAI
# 
# dendro.5sp$rBAI <- ((pi * dendro.5sp$Radius^2) - pi * (dendro.5sp$Radius - dendro.5sp$mmperyear)^2)/(dendro.5sp$Radius - 
#     dendro.5sp$mmperyear)
# 
# dendro.5sp$rBAI[dendro.5sp$rBAI == Inf] <- NA
# 
# # pad the few 0s for log plotting
# 
# dendro.5sp$rBAI[dendro.5sp$rBAI == 0] <- 1e-04
# 
# head(dendro.5sp)
# #```
# 
# #```
# ##   Plot Genus Species Code TreeNum Year mmperyear     GenSpec gpyr Age
# ## 1    1  Acer  rubrum Acer     532 1935     1.275 Acer rubrum 1935   0
# ## 2    1  Acer  rubrum Acer     532 1936     1.519 Acer rubrum 1940   1
# ## 3    1  Acer  rubrum Acer     532 1937     1.375 Acer rubrum 1940   2
# ## 4    1  Acer  rubrum Acer     532 1938     0.891 Acer rubrum 1940   3
# ## 5    1  Acer  rubrum Acer     532 1939     0.737 Acer rubrum 1940   4
# ## 6    1  Acer  rubrum Acer     532 1940     1.139 Acer rubrum 1940   5
# ##   Radius    BAI  block  canopy   rBAI
# ## 1  1.275  5.107 Valley Hemlock     NA
# ## 2  2.794 19.418 Valley Hemlock 15.229
# ## 3  4.169 30.078 Valley Hemlock 10.765
# ## 4  5.060 25.833 Valley Hemlock  6.197
# ## 5  5.797 25.138 Valley Hemlock  4.968
# ## 6  6.936 45.562 Valley Hemlock  7.860
# #```
# 
# #```r
# str(dendro.5sp)
# #```
# 
# #```
# ## 'data.frame':	15919 obs. of  15 variables:
# ##  $ Plot     : int  1 1 1 1 1 1 1 1 1 1 ...
# ##  $ Genus    : Factor w/ 5 levels "Acer","Betula",..: 1 1 1 1 1 1 1 1 1 1 ...
# ##  $ Species  : Factor w/ 6 levels "canadensis","candensis",..: 5 5 5 5 5 5 5 5 5 5 ...
# ##  $ Code     : Factor w/ 5 levels "Tsuga","Betula",..: 4 4 4 4 4 4 4 4 4 4 ...
# ##  $ TreeNum  : Factor w/ 212 levels "3","5","9","11",..: 145 145 145 145 145 145 145 145 145 145 ...
# ##  $ Year     : int  1935 1936 1937 1938 1939 1940 1941 1942 1943 1944 ...
# ##  $ mmperyear: num  1.275 1.519 1.375 0.891 0.737 ...
# ##  $ GenSpec  : Factor w/ 6 levels "Acer rubrum",..: 1 1 1 1 1 1 1 1 1 1 ...
# ##  $ gpyr     : num  1935 1940 1940 1940 1940 ...
# ##  $ Age      : int  0 1 2 3 4 5 6 7 8 9 ...
# ##  $ Radius   : num  1.27 2.79 4.17 5.06 5.8 ...
# ##  $ BAI      : num  5.11 19.42 30.08 25.83 25.14 ...
# ##  $ block    : Factor w/ 2 levels "Valley","Ridge": 1 1 1 1 1 1 1 1 1 1 ...
# ##  $ canopy   : Factor w/ 2 levels "Hemlock","Hardwood": 1 1 1 1 1 1 1 1 1 1 ...
# ##  $ rBAI     : num  NA 15.23 10.77 6.2 4.97 ...
# #```
# 
# #Plots
# ddg.procedure("Plots")
# #Figure 9: Individual traces of rBAI as a function of year, with smoothers. Overlay hemlock and hardwood plots.
# 
# 
# #```r
# # To create pdf, uncomment pdf and dev.off() lines.
# pdf('Figure-9-BAI-both-v2.pdf', width=4, height=6)
# 
# ggplot(data = dendro.5sp[dendro.5sp$canopy == "Hemlock", ], aes(x = Year, y = rBAI, 
#     group = TreeNum)) + facet_grid(Code ~ block) + scale_y_log10(limits = c(0.1, 
#     100), breaks = c(1, 10, 100)) + scale_x_continuous(limits = c(1860, 2005), 
#     breaks = c(seq(1880, 2000, 40))) + 
# # add environmental markers
# 
# geom_rect(xmin = 1910, xmax = 1915, ymin = -Inf, ymax = Inf, fill = "pink", 
#     alpha = 0.3, inherit.aes = FALSE) + geom_rect(xmin = 1925, xmax = 1932, 
#     ymin = -Inf, ymax = Inf, fill = "pink", alpha = 0.3, inherit.aes = FALSE) + 
#     geom_rect(xmin = 1962, xmax = 1966, ymin = -Inf, ymax = Inf, fill = "pink", 
#         alpha = 0.3, inherit.aes = FALSE) + 
# # geom_vline(xintercept=1911, color='grey50', linetype=2, size=.25) +
# # geom_vline(xintercept=1918, color='grey50', linetype=2, size=.25) +
# geom_vline(xintercept = 1938, color = "grey50", linetype = 2, size = 0.25) + 
#     geom_vline(xintercept = 1981, color = "grey50", linetype = 2, size = 0.25) + 
#     
# geom_line(colour = "grey", size = 0.25) + 
# # add the hardwood plots in light blue
# 
# geom_line(data = dendro.5sp[dendro.5sp$canopy == "Hardwood", ], aes(x = Year, 
#     y = rBAI, group = TreeNum), colour = "lightblue", size = 0.25) + 
# # smoothers go next
# 
# # geom_smooth(se=FALSE, data=dendro.5sp[dendro.5sp$canopy=='Hemlock',],
# # aes(group=1), method='loess', na.action=na.omit, span=.1, linetype=1,
# # colour='black', weight=2) +
# 
# # geom_smooth(se=FALSE, data=dendro.5sp[dendro.5sp$canopy=='Hardwood',],
# # aes(group=1), method='loess', na.action=na.omit, span=.1, linetype=1,
# # colour='blue', weight=2) +
# 
# stat_smooth(se = FALSE, data = dendro.5sp[dendro.5sp$canopy == "Hemlock", ], 
#     aes(group = 1), method = "loess", degree = 0, na.action = na.omit, span = 0.1, 
#     linetype = 1, colour = "black", weight = 2) + 
# 
# stat_smooth(se = FALSE, data = dendro.5sp[dendro.5sp$canopy == "Hardwood", ], 
#     aes(group = 1), method = "loess", degree = 0, na.action = na.omit, span = 0.1, 
#     linetype = 1, colour = "blue", weight = 2) + 
# theme_bw() + theme(strip.text.y = element_text(face = "italic", size = 8), strip.text.x = element_text(size = 8), 
#     axis.text = element_text(size = 7), axis.title = element_text(size = 10), 
#     axis.ticks.length = unit(1, "mm"), axis.ticks = element_line(size = 0.25)) + 
#     # ylab(expression(paste('Relative change in basal area (' mm^2 cdot mm^)
# ylab(expression(paste("Relative change in basal area (", mm^2 %.% mm^-2 %.% 
#     yr^-1, ")")))
# #```
# 
# #```
# ## Warning: Removed 59 rows containing missing values (stat_smooth).
# ## Warning: Removed 61 rows containing missing values (stat_smooth).
# ## Warning: Removed 1 rows containing missing values (stat_smooth).
# ## Warning: Removed 10 rows containing missing values (stat_smooth).
# ## Warning: Removed 8 rows containing missing values (stat_smooth).
# ## Warning: Removed 3 rows containing missing values (stat_smooth).
# ## Warning: Removed 7 rows containing missing values (stat_smooth).
# ## Warning: Removed 8 rows containing missing values (stat_smooth).
# ## Warning: Removed 7 rows containing missing values (stat_smooth).
# ## Warning: Removed 2 rows containing missing values (stat_smooth).
# ## Warning: Removed 3 rows containing missing values (stat_smooth).
# ## Warning: Removed 11 rows containing missing values (stat_smooth).
# ## Warning: Removed 9 rows containing missing values (stat_smooth).
# ## Warning: Removed 4 rows containing missing values (stat_smooth).
# ## Warning: Removed 2 rows containing missing values (stat_smooth).
# ## Warning: Removed 11 rows containing missing values (stat_smooth).
# ## Warning: Removed 6 rows containing missing values (stat_smooth).
# ## Warning: Removed 7 rows containing missing values (stat_smooth).
# ## Warning: Removed 57 rows containing missing values (geom_path).
# ## Warning: Removed 61 rows containing missing values (geom_path).
# ## Warning: Removed 1 rows containing missing values (geom_path).
# ## Warning: Removed 10 rows containing missing values (geom_path).
# ## Warning: Removed 8 rows containing missing values (geom_path).
# ## Warning: Removed 3 rows containing missing values (geom_path).
# ## Warning: Removed 7 rows containing missing values (geom_path).
# ## Warning: Removed 8 rows containing missing values (geom_path).
# ## Warning: Removed 5 rows containing missing values (geom_path).
# ## Warning: Removed 2 rows containing missing values (geom_path).
# ## Warning: Removed 3 rows containing missing values (geom_path).
# ## Warning: Removed 11 rows containing missing values (geom_path).
# ## Warning: Removed 9 rows containing missing values (geom_path).
# ## Warning: Removed 4 rows containing missing values (geom_path).
# ## Warning: Removed 2 rows containing missing values (geom_path).
# ## Warning: Removed 11 rows containing missing values (geom_path).
# ## Warning: Removed 6 rows containing missing values (geom_path).
# ## Warning: Removed 7 rows containing missing values (geom_path).
# #```
# 
# #![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21.png) 
# 
# #```r
# 
# dev.off()
# #```
# 
# #Fit 1-d GAMs
# 
# 
# #```r
# 
# gam.acer1 <- gam(rBAI ~ as.factor(Year) + s(Age, fx = TRUE, k = 4, bs = "cs"), 
#     data = subset(dendro.5sp, Code == "Acer"))
# gam.betula1 <- gam(rBAI ~ as.factor(Year) + s(Age, fx = TRUE, k = 4, bs = "cs"), 
#     data = subset(dendro.5sp, Code == "Betula"))
# gam.quercus1 <- gam(rBAI ~ as.factor(Year) + s(Age, fx = TRUE, k = 4, bs = "cs"), 
#     data = subset(dendro.5sp, Code == "Quercus"))
# gam.pinus1 <- gam(rBAI ~ as.factor(Year) + s(Age, fx = TRUE, k = 4, bs = "cs"), 
#     data = subset(dendro.5sp, Code == "Pinus"))
# gam.tsuga1 <- gam(rBAI ~ as.factor(Year) + s(Age, fx = TRUE, k = 4, bs = "cs"), 
#     data = subset(dendro.5sp, Code == "Tsuga"))
# 
# 
# # get wiggly year effects
# 
# 
# years.a <- c(min(dendro.5sp$Year[dendro.5sp$Code == "Acer"]):2004)
# lastyr.a <- 2005 - min(dendro.5sp$Year[dendro.5sp$Code == "Acer" & dendro.5sp$Age == 
#     0])
# fixed.year.a <- c(gam.acer1$coef[1], gam.acer1$coef[1] + gam.acer1$coef[2:lastyr.a])
# 
# 
# years.b <- c(min(dendro.5sp$Year[dendro.5sp$Code == "Betula"]):2004)
# lastyr.b <- 2005 - min(dendro.5sp$Year[dendro.5sp$Code == "Betula" & dendro.5sp$Age == 
#     0])
# fixed.year.b <- c(gam.betula1$coef[1], gam.betula1$coef[1] + gam.betula1$coef[2:lastyr.b])
# 
# years.q <- c(min(dendro.5sp$Year[dendro.5sp$Code == "Quercus"]):2004)
# lastyr.q <- 2005 - min(dendro.5sp$Year[dendro.5sp$Code == "Quercus" & dendro.5sp$Age == 
#     0])
# fixed.year.q <- c(gam.quercus1$coef[1], gam.quercus1$coef[1] + gam.quercus1$coef[2:lastyr.q])
# 
# years.p <- c(min(dendro.5sp$Year[dendro.5sp$Code == "Pinus"]):2004)
# lastyr.p <- 2005 - min(dendro.5sp$Year[dendro.5sp$Code == "Pinus" & dendro.5sp$Age == 
#     0])
# fixed.year.p <- c(gam.pinus1$coef[1], gam.pinus1$coef[1] + gam.pinus1$coef[2:lastyr.p])
# 
# years.t <- c(min(dendro.5sp$Year[dendro.5sp$Code == "Tsuga"]):2004)
# lastyr.t <- 2005 - min(dendro.5sp$Year[dendro.5sp$Code == "Tsuga" & dendro.5sp$Age == 
#     0])
# fixed.year.t <- c(gam.tsuga1$coef[1], gam.tsuga1$coef[1] + gam.tsuga1$coef[2:lastyr.t])
# 
# #```
# 
# #Nice 10-panel plot per Rhodora specs
# 
# 
# #```r
# 
# # gam plots in first column remove comments on pdf() and dev.off() to
# # generate pdf
# 
# pdf('Figure_10-single-gams-v2.pdf', height=7, width=5, pointsize=9)
# 
# layout(matrix(c(1:10), 5, 2, byrow = FALSE), widths = rep(lcm(5), 2), heights = rep(lcm(3), 
#     5))
# # layout.show(10)
# par(mar = c(1, 2, 0, 0.5))
# plot(gam.tsuga1, se = TRUE, xlab = "", ylab = "", xlim = c(0, 140), rug = FALSE, 
#     tcl = -0.25, col.axis = "white")
# axis(2, las = 1, tcl = -0.25, at = c(-2, 0, 2))
# text(0, -2, "Tsuga", font = 3, pos = 4)
# mtext(side = 3, "s(Age)", adj = 0, line = 0.5, cex = 1)
# 
# 
# plot(gam.betula1, se = TRUE, xlab = "", ylab = "", xlim = c(0, 140), rug = FALSE, 
#     tcl = -0.25, col.axis = "white")
# axis(2, las = 1, tcl = -0.25, at = seq(-10, 5, 5))
# text(0, -10, "Betula", font = 3, pos = 4)
# 
# plot(gam.quercus1, se = TRUE, xlab = "", ylab = "", xlim = c(0, 140), rug = FALSE, 
#     tcl = -0.25, col.axis = "white")
# axis(2, las = 1, tcl = -0.25, at = seq(-20, 10, 10))
# text(0, -20, "Quercus", font = 3, pos = 4)
# 
# plot(gam.acer1, se = TRUE, xlab = "", ylab = "", xlim = c(0, 140), rug = FALSE, 
#     tcl = -0.25, col.axis = "white")
# axis(2, las = 1, tcl = -0.25, at = seq(-10, 5, 5))
# text(0, -10, "Acer", font = 3, pos = 4)
# 
# plot(gam.pinus1, se = TRUE, xlab = "Age", ylab = "", xlim = c(0, 140), rug = FALSE, 
#     tcl = -0.25, col.axis = "white")
# axis(2, las = 1, tcl = -0.25, at = seq(-25, 5, 15))
# axis(1, tcl = -0.25, padj = -0.5)
# text(0, -23, "Pinus", font = 3, pos = 4)
# mtext(side = 1, "Age", , line = 2.25, cex = 1)
# 
# 
# # age wiggles in 2nd column
# 
# par(mar = c(1, 0.5, 0, 2))
# plot(years.t, fixed.year.t, type = "n", xlab = "", ylab = "", xlim = c(1860, 
#     2005), col.axis = "white", tcl = 0)
# abline(v = c(1938, 1981), lty = 2, col = "grey")
# rect(1910, -11, 1915, 43, col = rgb(190, 190, 190, alpha = 75, max = 255), lty = 0, 
#     lwd = 0)  #Chestnut blight
# rect(1925, -11, 1932, 43, col = rgb(190, 190, 190, alpha = 75, max = 255), lty = 0, 
#     lwd = 0)  #1925-32 logging
# rect(1962, -11, 1966, 43, col = rgb(190, 190, 190, alpha = 75, max = 255), lty = 0, 
#     lwd = 0)  #1960s drought
# lines(years.t, fixed.year.t)
# axis(1, tcl = -0.25, at = seq(1860, 2000, 20), labels = FALSE)
# axis(4, tcl = -0.25, at = seq(0, 20, 10), las = 1)
# mtext(side = 3, "f(Year)", adj = 1, line = 0.5, cex = 1)
# box()
# 
# 
# plot(years.b, fixed.year.b, type = "n", xlab = "", ylab = "", xlim = c(1860, 
#     2005), col.axis = "white", tcl = 0)
# abline(v = c(1938, 1981), lty = 2, col = "grey")
# rect(1910, -11, 1915, 43, col = rgb(190, 190, 190, alpha = 75, max = 255), lty = 0, 
#     lwd = 0)  #Chestnut blight
# rect(1925, -11, 1932, 43, col = rgb(190, 190, 190, alpha = 75, max = 255), lty = 0, 
#     lwd = 0)  #1925-32 logging
# rect(1962, -11, 1966, 43, col = rgb(190, 190, 190, alpha = 75, max = 255), lty = 0, 
#     lwd = 0)  #1960s drought
# lines(years.b, fixed.year.b)
# axis(1, tcl = -0.25, at = seq(1860, 2000, 20), labels = FALSE)
# axis(4, tcl = -0.25, at = seq(0, 20, 10), las = 1)
# box()
# 
# 
# plot(years.q, fixed.year.q, type = "n", xlab = "", ylab = "", xlim = c(1860, 
#     2005), col.axis = "white", tcl = 0)
# abline(v = c(1938, 1981), lty = 2, col = "grey")
# rect(1910, -11, 1915, 43, col = rgb(190, 190, 190, alpha = 75, max = 255), lty = 0, 
#     lwd = 0)  #Chestnut blight
# rect(1925, -11, 1932, 43, col = rgb(190, 190, 190, alpha = 75, max = 255), lty = 0, 
#     lwd = 0)  #1925-32 logging
# rect(1962, -11, 1966, 43, col = rgb(190, 190, 190, alpha = 75, max = 255), lty = 0, 
#     lwd = 0)  #1960s drought
# lines(years.q, fixed.year.q)
# axis(1, tcl = -0.25, at = seq(1860, 2000, 20), labels = FALSE)
# axis(4, tcl = -0.25, at = seq(5, 15, 5), las = 1)
# box()
# 
# plot(years.a, fixed.year.a, type = "n", xlab = "", ylab = "", xlim = c(1860, 
#     2005), col.axis = "white", tcl = 0)
# abline(v = c(1938, 1981), lty = 2, col = "grey")
# rect(1910, -11, 1915, 43, col = rgb(190, 190, 190, alpha = 75, max = 255), lty = 0, 
#     lwd = 0)  #Chestnut blight
# rect(1925, -11, 1932, 43, col = rgb(190, 190, 190, alpha = 75, max = 255), lty = 0, 
#     lwd = 0)  #1925-32 logging
# rect(1962, -11, 1966, 43, col = rgb(190, 190, 190, alpha = 75, max = 255), lty = 0, 
#     lwd = 0)  #1960s drought
# lines(years.a, fixed.year.a)
# axis(1, tcl = -0.25, at = seq(1860, 2000, 20), labels = FALSE)
# axis(4, tcl = -0.25, at = seq(0, 20, 10), las = 1)
# box()
# 
# 
# plot(years.p, fixed.year.p, type = "n", xlab = "", ylab = "", xlim = c(1860, 
#     2005), col.axis = "white", tcl = 0)
# abline(v = c(1938, 1981), lty = 2, col = "grey")
# rect(1910, -11, 1915, 43, col = rgb(190, 190, 190, alpha = 75, max = 255), lty = 0, 
#     lwd = 0)  #Chestnut blight
# rect(1925, -11, 1932, 43, col = rgb(190, 190, 190, alpha = 75, max = 255), lty = 0, 
#     lwd = 0)  #1925-32 logging
# rect(1962, -11, 1966, 43, col = rgb(190, 190, 190, alpha = 75, max = 255), lty = 0, 
#     lwd = 0)  #1960s drought
# lines(years.p, fixed.year.p)
# axis(1, tcl = -0.25, at = seq(1860, 2000, 20), labels = FALSE)
# axis(1, tcl = 0, at = seq(1880, 2000, 40), padj = -0.5)
# axis(4, tcl = -0.25, at = seq(10, 30, 10), las = 1)
# mtext(side = 1, "Year", line = 2.25, cex = 1)
# box()
# #```
# 
# #![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23.png) 
# 
# #```r
# 
# dev.off()
# #```
# 
# 
# #Fit 2D-GAMS
# 
# 
# #```r
# 
# # pad the few 0s for log plotting
# 
# dendro.5sp$rBAI[dendro.5sp$rBAI == 0] <- 1e-04
# # add birth year (Byear) to dendro.5sp
# 
# birthyr.table <- tapply(dendro.5sp$Year, list(dendro.5sp$TreeNum), min)
# birthyr.TreeNum <- as.numeric(names(birthyr.table))
# birthyr.Year <- as.integer(birthyr.table)
# birthyr <- data.frame(birthyr.TreeNum, birthyr.Year)
# names(birthyr) <- c("TreeNum", "Byear")
# 
# for (n in unique(dendro.5sp$TreeNum)) {
#     len <- length(dendro.5sp[dendro.5sp$TreeNum == n, 1])
#     dendro.5sp$Byear[dendro.5sp$TreeNum == n] <- rep(birthyr$Byear[birthyr$TreeNum == 
#         n], len)
# }
# 
# species <- levels(dendro.5sp$Code)
# 
# 
# for (tr in species) {
#     dt <- subset(dendro.5sp, Code == tr)
#     o <- table(dt$Byear)
#     o <- o[o > 1]
#     
#     ## For trees with same birth year, add 0.1 (to jitter)##
#     for (k in as.numeric(names(o))) {
#         x <- unique(dt[dt$Byear == k, ]$TreeNum)
#         z <- 0
#         for (j in x) {
#             dt[dt$TreeNum == j, ]$Year <- dt[dt$TreeNum == j, ]$Year + z
#             z <- z + 0.1
#         }
#     }
# }
# 
# dendro.5sp$rBAI2 <- dendro.5sp$rBAI
# dendro.5sp$rBAI2[is.na(dendro.5sp$rBAI2)] <- 1
# 
# head(dendro.5sp)
# #```
# 
# #```
# ##   Plot Genus Species Code TreeNum Year mmperyear     GenSpec gpyr Age
# ## 1    1  Acer  rubrum Acer     532 1935     1.275 Acer rubrum 1935   0
# ## 2    1  Acer  rubrum Acer     532 1936     1.519 Acer rubrum 1940   1
# ## 3    1  Acer  rubrum Acer     532 1937     1.375 Acer rubrum 1940   2
# ## 4    1  Acer  rubrum Acer     532 1938     0.891 Acer rubrum 1940   3
# ## 5    1  Acer  rubrum Acer     532 1939     0.737 Acer rubrum 1940   4
# ## 6    1  Acer  rubrum Acer     532 1940     1.139 Acer rubrum 1940   5
# ##   Radius    BAI  block  canopy   rBAI Byear  rBAI2
# ## 1  1.275  5.107 Valley Hemlock     NA  1935  1.000
# ## 2  2.794 19.418 Valley Hemlock 15.229  1935 15.229
# ## 3  4.169 30.078 Valley Hemlock 10.765  1935 10.765
# ## 4  5.060 25.833 Valley Hemlock  6.197  1935  6.197
# ## 5  5.797 25.138 Valley Hemlock  4.968  1935  4.968
# ## 6  6.936 45.562 Valley Hemlock  7.860  1935  7.860
# #```
# 
# 
# 
# #```r
# 
# gam.acer2 <- gam(rBAI ~ s(Year, Age), data = subset(dendro.5sp, Code == "Acer"))
# gam.betula2 <- gam(rBAI ~ s(Year, Age), data = subset(dendro.5sp, Code == "Betula"))
# gam.quercus2 <- gam(rBAI ~ s(Year, Age), data = subset(dendro.5sp, Code == "Quercus"))
# gam.pinus2 <- gam(rBAI ~ s(Year, Age), data = subset(dendro.5sp, Code == "Pinus"))
# gam.tsuga2 <- gam(rBAI ~ s(Year, Age), data = subset(dendro.5sp, Code == "Tsuga"))
# 
# 
# ## build contour plots of the predictions of the 2d gams
# 
# ## Acer rubrum
# 
# minYr <- min(dendro.5sp$Year[dendro.5sp$Code == "Acer"])
# maxAge <- max(dendro.5sp$Age[dendro.5sp$Code == "Acer"])
# maxBirthYr <- max(dendro.5sp$Byear[dendro.5sp$Code == "Acer"])
# Year <- c(minYr:2004)
# Age <- c(0:maxAge)
# XY <- expand.grid(Year, Age)
# names(XY) <- c("Year", "Age")
# 
# pred <- predict(gam.acer2, newdata = XY)
# XY$rBAI <- pred
# newXY.acer <- XY[XY$Year - minYr >= XY$Age & XY$Year - maxBirthYr <= XY$Age, 
#     ]
# 
# ## Betula lenta
# 
# minYr <- min(dendro.5sp$Year[dendro.5sp$Code == "Betula"])
# maxAge <- max(dendro.5sp$Age[dendro.5sp$Code == "Betula"])
# maxBirthYr <- max(dendro.5sp$Byear[dendro.5sp$Code == "Betula"])
# Year <- c(minYr:2004)
# Age <- c(0:maxAge)
# XY <- expand.grid(Year, Age)
# names(XY) <- c("Year", "Age")
# 
# pred <- predict(gam.betula2, newdata = XY)
# XY$rBAI <- pred
# newXY.betula <- XY[XY$Year - minYr >= XY$Age & XY$Year - maxBirthYr <= XY$Age, 
#     ]
# 
# ## Quercus rubra
# 
# minYr <- min(dendro.5sp$Year[dendro.5sp$Code == "Quercus"])
# maxAge <- max(dendro.5sp$Age[dendro.5sp$Code == "Quercus"])
# maxBirthYr <- max(dendro.5sp$Byear[dendro.5sp$Code == "Quercus"])
# Year <- c(minYr:2004)
# Age <- c(0:maxAge)
# XY <- expand.grid(Year, Age)
# names(XY) <- c("Year", "Age")
# 
# pred <- predict(gam.quercus2, newdata = XY)
# XY$rBAI <- pred
# newXY.quercus <- XY[XY$Year - minYr >= XY$Age & XY$Year - maxBirthYr <= XY$Age, 
#     ]
# 
# ## Pinus strobus
# 
# minYr <- min(dendro.5sp$Year[dendro.5sp$Code == "Pinus"])
# maxAge <- max(dendro.5sp$Age[dendro.5sp$Code == "Pinus"])
# maxBirthYr <- max(dendro.5sp$Byear[dendro.5sp$Code == "Pinus"])
# Year <- c(minYr:2004)
# Age <- c(0:maxAge)
# XY <- expand.grid(Year, Age)
# names(XY) <- c("Year", "Age")
# 
# pred <- predict(gam.pinus2, newdata = XY)
# XY$rBAI <- pred
# newXY.pinus <- XY[XY$Year - minYr >= XY$Age & XY$Year - maxBirthYr <= XY$Age, 
#     ]
# 
# ## Tsuga canadensis
# 
# minYr <- min(dendro.5sp$Year[dendro.5sp$Code == "Tsuga"])
# maxAge <- max(dendro.5sp$Age[dendro.5sp$Code == "Tsuga"])
# maxBirthYr <- max(dendro.5sp$Byear[dendro.5sp$Code == "Tsuga"])
# Year <- c(minYr:2004)
# Age <- c(0:maxAge)
# XY <- expand.grid(Year, Age)
# names(XY) <- c("Year", "Age")
# 
# pred <- predict(gam.tsuga2, newdata = XY)
# XY$rBAI <- pred
# newXY.tsuga <- XY[XY$Year - minYr >= XY$Age & XY$Year - maxBirthYr <= XY$Age, 
#     ]
# 
# # Contour plots
# 
# # Acer
# 
# a.cont <- ggplot(data = newXY.acer, aes(Year, Age, z = rBAI, xmin = 1860, ymax = 120)) + 
#     
# # add environmental markers
# 
# geom_rect(xmin = 1910, xmax = 1915, ymin = -Inf, ymax = Inf, fill = "pink", 
#     alpha = 0.3, inherit.aes = FALSE) + geom_rect(xmin = 1925, xmax = 1932, 
#     ymin = -Inf, ymax = Inf, fill = "pink", alpha = 0.3, inherit.aes = FALSE) + 
#     geom_rect(xmin = 1962, xmax = 1966, ymin = -Inf, ymax = Inf, fill = "pink", 
#         alpha = 0.3, inherit.aes = FALSE) + geom_vline(xintercept = 1938, color = "grey50", 
#     linetype = 2, size = 0.25) + geom_vline(xintercept = 1981, color = "grey50", 
#     linetype = 2, size = 0.25) + 
# 
# geom_tile(aes(fill = rBAI)) + geom_contour(colour = "black", linetype = 0) + 
#     scale_fill_gradient(low = "yellow", high = "red", name = "rBAI") + labs(x = "Year", 
#     y = "Age") + theme_bw() + theme(legend.position = "none", axis.title.x = element_blank(), 
#     axis.text.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_text(size = 6), 
#     axis.ticks.length = unit(1, "mm"), axis.ticks = element_line(size = 0.25)) + 
#     scale_x_continuous(breaks = seq(1880, 2000, 40)) + scale_y_continuous(breaks = seq(0, 
#     120, 20))
# 
# # Betula
# 
# b.cont <- ggplot(data = newXY.betula, aes(Year, Age, z = rBAI, xmin = 1860, 
#     ymax = 120)) + 
# # add environmental markers
# 
# geom_rect(xmin = 1910, xmax = 1915, ymin = -Inf, ymax = Inf, fill = "pink", 
#     alpha = 0.3, inherit.aes = FALSE) + geom_rect(xmin = 1925, xmax = 1932, 
#     ymin = -Inf, ymax = Inf, fill = "pink", alpha = 0.3, inherit.aes = FALSE) + 
#     geom_rect(xmin = 1962, xmax = 1966, ymin = -Inf, ymax = Inf, fill = "pink", 
#         alpha = 0.3, inherit.aes = FALSE) + geom_vline(xintercept = 1938, color = "grey50", 
#     linetype = 2, size = 0.25) + geom_vline(xintercept = 1981, color = "grey50", 
#     linetype = 2, size = 0.25) + 
# geom_tile(aes(fill = rBAI)) + geom_contour(colour = "black", linetype = 0) + 
#     scale_fill_gradient(low = "yellow", high = "red", name = "rBAI") + labs(x = "Year", 
#     y = "Age") + theme_bw() + theme(legend.position = "none", axis.title.x = element_blank(), 
#     axis.text.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_text(size = 6), 
#     axis.ticks.length = unit(1, "mm"), axis.ticks = element_line(size = 0.25)) + 
#     scale_x_continuous(breaks = seq(1880, 2000, 40)) + scale_y_continuous(breaks = seq(0, 
#     120, 20))
# 
# # Quercus
# 
# q.cont <- ggplot(data = newXY.quercus, aes(Year, Age, z = rBAI, xmin = 1860, 
#     ymax = 100)) + 
# # add environmental markers
# 
# geom_rect(xmin = 1910, xmax = 1915, ymin = -Inf, ymax = Inf, fill = "pink", 
#     alpha = 0.3, inherit.aes = FALSE) + geom_rect(xmin = 1925, xmax = 1932, 
#     ymin = -Inf, ymax = Inf, fill = "pink", alpha = 0.3, inherit.aes = FALSE) + 
#     geom_rect(xmin = 1962, xmax = 1966, ymin = -Inf, ymax = Inf, fill = "pink", 
#         alpha = 0.3, inherit.aes = FALSE) + geom_vline(xintercept = 1938, color = "grey50", 
#     linetype = 2, size = 0.25) + geom_vline(xintercept = 1981, color = "grey50", 
#     linetype = 2, size = 0.25) + 
# geom_tile(aes(fill = rBAI)) + geom_contour(colour = "black", linetype = 0) + 
#     scale_fill_gradient(low = "yellow", high = "red", name = "rBAI") + labs(x = "Year", 
#     y = "Age") + theme_bw() + theme(legend.position = "none", axis.title.x = element_blank(), 
#     axis.text.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_text(size = 6), 
#     axis.ticks.length = unit(1, "mm"), axis.ticks = element_line(size = 0.25)) + 
#     scale_x_continuous(breaks = seq(1880, 2000, 40)) + scale_y_continuous(breaks = seq(0, 
#     100, 20))
# 
# # Pinus
# 
# p.cont <- ggplot(data = newXY.pinus, aes(Year, Age, z = rBAI, xmin = 1860, ymax = 100)) + 
#     
# # add environmental markers
# 
# geom_rect(xmin = 1910, xmax = 1915, ymin = -Inf, ymax = Inf, fill = "pink", 
#     alpha = 0.3, inherit.aes = FALSE) + geom_rect(xmin = 1925, xmax = 1932, 
#     ymin = -Inf, ymax = Inf, fill = "pink", alpha = 0.3, inherit.aes = FALSE) + 
#     geom_rect(xmin = 1962, xmax = 1966, ymin = -Inf, ymax = Inf, fill = "pink", 
#         alpha = 0.3, inherit.aes = FALSE) + geom_vline(xintercept = 1938, color = "grey50", 
#     linetype = 2, size = 0.25) + geom_vline(xintercept = 1981, color = "grey50", 
#     linetype = 2, size = 0.25) + 
# geom_tile(aes(fill = rBAI)) + geom_contour(colour = "black", linetype = 0) + 
#     scale_fill_gradient(low = "yellow", high = "red", name = "rBAI") + labs(x = "Year", 
#     y = "Age") + theme_bw() + theme(legend.position = "none", axis.title.x = element_text(size = 8), 
#     axis.text.x = element_text(size = 6), axis.title.y = element_text(colour = "white"), 
#     axis.text.y = element_text(size = 6), axis.ticks.length = unit(1, "mm"), 
#     axis.ticks = element_line(size = 0.25)) + scale_x_continuous(breaks = seq(1880, 
#     2000, 40)) + scale_y_continuous(breaks = seq(0, 120, 20))
# 
# # Tsuga
# 
# t.cont <- ggplot(data = newXY.tsuga, aes(Year, Age, z = rBAI, xmin = 1860, ymax = 140)) + 
#     
# # add environmental markers
# 
# geom_rect(xmin = 1910, xmax = 1915, ymin = -Inf, ymax = Inf, fill = "pink", 
#     alpha = 0.3, inherit.aes = FALSE) + geom_rect(xmin = 1925, xmax = 1932, 
#     ymin = -Inf, ymax = Inf, fill = "pink", alpha = 0.3, inherit.aes = FALSE) + 
#     geom_rect(xmin = 1962, xmax = 1966, ymin = -Inf, ymax = Inf, fill = "pink", 
#         alpha = 0.3, inherit.aes = FALSE) + geom_vline(xintercept = 1938, color = "grey50", 
#     linetype = 2, size = 0.25) + geom_vline(xintercept = 1981, color = "grey50", 
#     linetype = 2, size = 0.25) + 
# geom_tile(aes(fill = rBAI)) + geom_contour(colour = "black", linetype = 0) + 
#     scale_fill_gradient(low = "yellow", high = "red", name = "rBAI") + labs(x = "Year", 
#     y = "Age") + theme_bw() + theme(legend.position = "none", axis.title.x = element_blank(), 
#     axis.text.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_text(size = 6), 
#     axis.ticks.length = unit(1, "mm"), axis.ticks = element_line(size = 0.25)) + 
#     scale_x_continuous(breaks = seq(1880, 2000, 40)) + scale_y_continuous(breaks = seq(0, 
#     140, 20))
# 
# 
# # line plots
# 
# t.lines <- ggplot(data = dendro.5sp[dendro.5sp$Code == "Tsuga", ], aes(x = Year, 
#     y = Age, group = TreeNum, geom = "line", xmin = 1860, ymax = 140, color = rBAI2, 
#     xlab = "", ylab = "Age")) + 
# # add environmental markers
# 
# geom_rect(xmin = 1910, xmax = 1915, ymin = -Inf, ymax = Inf, fill = "pink", 
#     alpha = 0.3, inherit.aes = FALSE) + geom_rect(xmin = 1925, xmax = 1932, 
#     ymin = -Inf, ymax = Inf, fill = "pink", alpha = 0.3, inherit.aes = FALSE) + 
#     geom_rect(xmin = 1962, xmax = 1966, ymin = -Inf, ymax = Inf, fill = "pink", 
#         alpha = 0.3, inherit.aes = FALSE) + geom_vline(xintercept = 1938, color = "grey50", 
#     linetype = 2, size = 0.25) + geom_vline(xintercept = 1981, color = "grey50", 
#     linetype = 2, size = 0.25) + 
# geom_line(size = 1) + scale_colour_gradientn(colours = c("yellow", "orange", 
#     "orangered", "red", "darkred")) + theme_bw() + theme(legend.position = "none", 
#     axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_text(colour = "white"), 
#     axis.text.y = element_text(size = 6), axis.ticks.length = unit(1, "mm"), 
#     axis.ticks = element_line(size = 0.25)) + scale_x_continuous(breaks = seq(1880, 
#     2000, 40)) + scale_y_continuous(breaks = seq(0, 140, 20)) + annotate("text", 
#     label = "Tsuga", x = 1870, y = 122, size = 2, colour = "black", fontface = 3)
# 
# b.lines <- ggplot(data = dendro.5sp[dendro.5sp$Code == "Betula", ], aes(x = Year, 
#     y = Age, group = TreeNum, geom = "line", xmin = 1860, ymax = 120, color = rBAI2, 
#     xlab = "", ylab = "Age")) + 
# # add environmental markers
# 
# geom_rect(xmin = 1910, xmax = 1915, ymin = -Inf, ymax = Inf, fill = "pink", 
#     alpha = 0.3, inherit.aes = FALSE) + geom_rect(xmin = 1925, xmax = 1932, 
#     ymin = -Inf, ymax = Inf, fill = "pink", alpha = 0.3, inherit.aes = FALSE) + 
#     geom_rect(xmin = 1962, xmax = 1966, ymin = -Inf, ymax = Inf, fill = "pink", 
#         alpha = 0.3, inherit.aes = FALSE) + geom_vline(xintercept = 1938, color = "grey50", 
#     linetype = 2, size = 0.25) + geom_vline(xintercept = 1981, color = "grey50", 
#     linetype = 2, size = 0.25) + 
# geom_line(size = 1) + scale_colour_gradientn(colours = c("yellow", "orange", 
#     "orangered", "red", "darkred")) + theme_bw() + theme(legend.position = "none", 
#     axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_text(colour = "white"), 
#     axis.text.y = element_text(size = 6), axis.ticks.length = unit(1, "mm"), 
#     axis.ticks = element_line(size = 0.25)) + scale_x_continuous(breaks = seq(1880, 
#     2000, 40)) + scale_y_continuous(breaks = seq(0, 120, 20)) + annotate("text", 
#     label = "Betula", x = 1870, y = 102, size = 2, colour = "black", fontface = 3)
# 
# q.lines <- ggplot(data = dendro.5sp[dendro.5sp$Code == "Quercus", ], aes(x = Year, 
#     y = Age, group = TreeNum, geom = "line", xmin = 1860, ymax = 100, color = rBAI2, 
#     xlab = "", ylab = "Age")) + 
# # add environmental markers
# 
# geom_rect(xmin = 1910, xmax = 1915, ymin = -Inf, ymax = Inf, fill = "pink", 
#     alpha = 0.3, inherit.aes = FALSE) + geom_rect(xmin = 1925, xmax = 1932, 
#     ymin = -Inf, ymax = Inf, fill = "pink", alpha = 0.3, inherit.aes = FALSE) + 
#     geom_rect(xmin = 1962, xmax = 1966, ymin = -Inf, ymax = Inf, fill = "pink", 
#         alpha = 0.3, inherit.aes = FALSE) + geom_vline(xintercept = 1938, color = "grey50", 
#     linetype = 2, size = 0.25) + geom_vline(xintercept = 1981, color = "grey50", 
#     linetype = 2, size = 0.25) + 
# geom_line(size = 1) + scale_colour_gradientn(colours = c("yellow", "orange", 
#     "orangered", "red", "darkred")) + theme_bw() + theme(legend.position = "none", 
#     axis.title.x = element_blank(), axis.title.y = element_text(size = 8), axis.text.x = element_blank(), 
#     axis.text.y = element_text(size = 6), axis.ticks.length = unit(1, "mm"), 
#     axis.ticks = element_line(size = 0.25)) + scale_x_continuous(breaks = seq(1880, 
#     2000, 40)) + scale_y_continuous(breaks = seq(0, 100, 20)) + annotate("text", 
#     label = "Quercus", x = 1875, y = 82, size = 2, colour = "black", fontface = 3)
# 
# a.lines <- ggplot(data = dendro.5sp[dendro.5sp$Code == "Acer", ], aes(x = Year, 
#     y = Age, group = TreeNum, geom = "line", xmin = 1860, ymax = 120, color = rBAI2, 
#     xlab = "", ylab = "Age")) + 
# # add environmental markers
# 
# geom_rect(xmin = 1910, xmax = 1915, ymin = -Inf, ymax = Inf, fill = "pink", 
#     alpha = 0.3, inherit.aes = FALSE) + geom_rect(xmin = 1925, xmax = 1932, 
#     ymin = -Inf, ymax = Inf, fill = "pink", alpha = 0.3, inherit.aes = FALSE) + 
#     geom_rect(xmin = 1962, xmax = 1966, ymin = -Inf, ymax = Inf, fill = "pink", 
#         alpha = 0.3, inherit.aes = FALSE) + geom_vline(xintercept = 1938, color = "grey50", 
#     linetype = 2, size = 0.25) + geom_vline(xintercept = 1981, color = "grey50", 
#     linetype = 2, size = 0.25) + 
# geom_line(size = 1) + scale_colour_gradientn(colours = c("yellow", "orange", 
#     "orangered", "red", "darkred")) + theme_bw() + theme(legend.position = "none", 
#     axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_text(colour = "white"), 
#     axis.text.y = element_text(size = 6), axis.ticks.length = unit(1, "mm"), 
#     axis.ticks = element_line(size = 0.25)) + scale_x_continuous(breaks = seq(1880, 
#     2000, 40)) + scale_y_continuous(breaks = seq(0, 120, 20)) + annotate("text", 
#     label = "Acer", x = 1870, y = 102, size = 2, colour = "black", fontface = 3)
# 
# p.lines <- ggplot(data = dendro.5sp[dendro.5sp$Code == "Pinus", ], aes(x = Year, 
#     y = Age, group = TreeNum, geom = "line", xmin = 1860, ymax = 100, color = rBAI2, 
#     xlab = "Year", ylab = "Age")) + 
# # add environmental markers
# 
# geom_rect(xmin = 1910, xmax = 1915, ymin = -Inf, ymax = Inf, fill = "pink", 
#     alpha = 0.3, inherit.aes = FALSE) + geom_rect(xmin = 1925, xmax = 1932, 
#     ymin = -Inf, ymax = Inf, fill = "pink", alpha = 0.3, inherit.aes = FALSE) + 
#     geom_rect(xmin = 1962, xmax = 1966, ymin = -Inf, ymax = Inf, fill = "pink", 
#         alpha = 0.3, inherit.aes = FALSE) + geom_vline(xintercept = 1938, color = "grey50", 
#     linetype = 2, size = 0.25) + geom_vline(xintercept = 1981, color = "grey50", 
#     linetype = 2, size = 0.25) + 
# geom_line(size = 1) + scale_colour_gradientn(colours = c("yellow", "orange", 
#     "orangered", "red", "darkred")) + theme_bw() + theme(legend.position = "none", 
#     axis.title.x = element_text(size = 8), axis.text.x = element_text(size = 6), 
#     axis.title.y = element_text(colour = "white"), axis.text.y = element_text(size = 6), 
#     axis.ticks.length = unit(1, "mm"), axis.ticks = element_line(size = 0.25)) + 
#     scale_x_continuous(breaks = seq(1880, 2000, 40)) + scale_y_continuous(breaks = seq(0, 
#     100, 20)) + annotate("text", label = "Pinus", x = 1870, y = 82, size = 2, 
#     colour = "black", fontface = 3)
# 
# 
# 
# pdf('Figure-11-countours-v2.pdf', height=6, width=4)
# multiplot(t.lines, b.lines, q.lines, a.lines, p.lines, t.cont, b.cont, q.cont, 
#     a.cont, p.cont, cols = 2)
# #```
# 
# #![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-25.png) 
# 
# #```r
# dev.off()
# #```
# 
# #Extract deviance from and compare the 1-d and 2-d GAMs
# ddg.procedure("Extract deviance from and compare the 1-d and 2-d GAMs")
# 
# #```r
# 1 - gam.acer1$deviance/gam.acer1$null.deviance
# #```
# 
# #```
# ## [1] 0.2583
# #```
# 
# #```r
# 1 - gam.acer2$deviance/gam.acer1$null.deviance
# #```
# 
# #```
# ## [1] 0.2601
# #```
# 
# #```r
# AIC(gam.acer1, gam.acer2)
# #```
# 
# #```
# ##               df   AIC
# ## gam.acer1 118.00 13156
# ## gam.acer2  26.63 12968
# #```
# 
# #```r
# 
# 1 - gam.betula1$deviance/gam.betula1$null.deviance
# #```
# 
# #```
# ## [1] 0.3788
# #```
# 
# #```r
# 1 - gam.betula2$deviance/gam.betula1$null.deviance
# #```
# 
# #```
# ## [1] 0.4528
# #```
# 
# #```r
# AIC(gam.betula1, gam.betula2)
# #```
# 
# #```
# ##                 df   AIC
# ## gam.betula1 125.00 13176
# ## gam.betula2  29.28 12728
# #```
# 
# #```r
# 
# 1 - gam.quercus1$deviance/gam.quercus1$null.deviance
# #```
# 
# #```
# ## [1] 0.333
# #```
# 
# #```r
# 1 - gam.quercus2$deviance/gam.quercus1$null.deviance
# #```
# 
# #```
# ## [1] 0.4169
# #```
# 
# #```r
# AIC(gam.quercus1, gam.quercus2)
# #```
# 
# #```
# ##                 df  AIC
# ## gam.quercus1 106.0 7989
# ## gam.quercus2  27.3 7671
# #```
# 
# #```r
# 
# 1 - gam.pinus1$deviance/gam.pinus1$null.deviance
# #```
# 
# #```
# ## [1] 0.297
# #```
# 
# #```r
# 1 - gam.pinus2$deviance/gam.pinus1$null.deviance
# #```
# 
# #```
# ## [1] 0.3942
# #```
# 
# #```r
# AIC(gam.pinus1, gam.pinus2)
# #```
# 
# #```
# ##                df  AIC
# ## gam.pinus1 108.00 7227
# ## gam.pinus2  27.62 6930
# #```
# 
# #```r
# 
# 1 - gam.tsuga1$deviance/gam.tsuga1$null.deviance
# #```
# 
# #```
# ## [1] 0.2453
# #```
# 
# #```r
# 1 - gam.tsuga2$deviance/gam.tsuga1$null.deviance
# #```
# 
# #```
# ## [1] 0.2492
# #```
# 
# #```r
# AIC(gam.tsuga1, gam.tsuga2)
# #```
# 
# #```
# ##                df   AIC
# ## gam.tsuga1 146.00 65388
# ## gam.tsuga2  30.22 65107
#```
end.time <- Sys.time()
print(paste("Execution time =", (end.time - start.time)))
#ddg.save()
