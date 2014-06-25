######################
### Helpers Script ###
######################

# Contains useful helper functions to be used throughout the summer

### R Packages

library(chron)
require(tcltk)
require(gWidgets)
options(guiToolkit="tcltk")

### Function which presents a pop-up window and returns user intput
# @params message - string to be presented to user as message
# $return CHOICE - the user input (not necessarily a string)
INPUT <- function(message) {
  # open dialog box for user input
  CHOICE <- NA
  w <- gbasicdialog(title=message, handler = function(h,...) CHOICE <<- svalue(input))
  input <- gedit("", initial.msg="", cont=w, width=20)
  addHandlerChanged(input, handler=function (h,...) {
    CHOICE <<- svalue(input)
    dispose(w)
  })
  visible(w, set=TRUE)
  
  return(CHOICE)
}


### Function: Returns the size of the an arbitrary directory
# @params dir - the name of the directory. The directory must be contained inside
#               the working directory.
# $return - the size, in mB of the directory (all files and folder insider, recursively)
#           If the directory does not exists, it return NA
dirSize <- function(dir){
  fdir <- paste(getwd(),dir,sep="")
  tryCatch({
    dirFiles <- list.files(path=fdir, full.names = TRUE, recursive = TRUE)
  }, warning = function(w) {
    return(NA)
  })
  dirInfos <- file.info(dirFiles)
  dirSizes <- dirInfos$size
  dirSize <- sum(dirSizes)/2^10 # we want result in kB
  return(dirSize)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
