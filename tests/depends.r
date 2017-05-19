### DEPENDS.R ###
#
#	This file checks the current R installation for packages on which
# 	tests in examples/ depend on. It attempts to install them if they
#	are not installed yet.
###

usePackage <- function(p) {
	print(p)
	if (!is.element(p, installed.packages()[,1])){
		print("Installing...")
  	install.packages(p, dependencies = TRUE, repos = "http://cloud.r-project.org",
                     clean=TRUE, quiet=FALSE, verbose=TRUE)
	}
	print("Installed!")
	return(TRUE)
}

installFonts <- function() {

  # create folder to store fonts and add to search path
  path <- path.expand("~/.fonts")
  dir.create(path, showWarnings=FALSE)
  font.paths(path)

  download.file("http://simonsoftware.se/other/xkcd.ttf", destfile="~/.fonts/xkcd.ttf", mode="wb")
  font.add("xkcd", regular = path.expand("~/.fonts/xkcd.ttf"))

  font_import(paths= path.expand("~/.fonts"), prompt=FALSE)
}

# List of packages that are needed
pkgs <- c("RDataTracker", "chron", "gWidgets", "dplR", "zoo", "ggplot2",
		"gdata", "grid", "gridExtra", "mgcv", "akima", "spatstat", "reshape2", "RCurl", "plyr",
    "xkcd", "sysfonts", "extrafont")

# Install the packages that might be missing
if (all(unlist(Map(usePackage, pkgs))))  print("All required packages installed.")

# Once installed, we want to require the sysfonts package to check for the xckd font
# require(sysfonts)
# require(extrafont)
# if (!("xkcd.ttf" %in% font.files())) installFonts()
