### DEPENDS.R ###
#
#	This file checks the current R installation for packages on which
# 	tests in examples/ depend on. It attempts to install them if they 
#	are not installed yet.
###

usePackage <- function(p) {
    if (!is.element(p, installed.packages()[,1]))
        install.packages(p, dep = TRUE)

    return(TRUE)
}

pkgs <- c("RDataTracker", "chron", "gWidgets", "dplR", "zoo", "ggplot2",
		"gdata", "grid", "mgcv", "akima", "spatstat")

if (all(unlist(Map(usePackage, pkgs))))  print("All required packages installed.")