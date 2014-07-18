#############################################
### CALCULATE SQUARE ROOT WITH USER INPUT ###
#############################################

# Calculate square root iteratively
# Get numbers from user
# Quit on "q"
# ERB rev. 5-Feb-2014

### R Packages
require(tcltk)
require(gWidgets)
options(guiToolkit="tcltk")

### Directories

ddg.library <- Sys.getenv("DDG_LIBRARY")
if (ddg.library == "") {
	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
}
source(ddg.library)

#setwd("c:/data/r/ddg/calculate-square-root-input")
#ddg.r.script.path <- paste(getwd(),"/calculate-square-root-input-1.r",sep="")
#ddg.path <- paste(getwd(),"/ddg",sep="")
#source("c:/data/r/lib/ddg-library.r")

### Functions

get.initial.values <- function() {
  tolerance <<- 0.00001
  num <- 0
  sqrt <- 0
  sqr.roots <- data.frame(num,sqrt)
  
  ddg.procedure()
  ddg.data.out(tolerance)
  ddg.snapshot.out(sqr.roots)

  return(sqr.roots)
}

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

get.input <- function() {
  # process user input
  x.char <- "-1"
  x <- as.numeric(x.char)
  while ((is.na(as.numeric(x.char)) | x <= 0) & exit==0) {
    x.char <- INPUT("Enter number (q=quit)")
    if (x.char=="q") exit <<- 1
    x <- as.numeric(x.char)
  }
  number <- x
  
  ddg.procedure()
  ddg.data.out(number)
    
  return(number)
}

get.random <- function(number) {
  # get random seed value
  estimate <- runif(1,1,number)
  
  ddg.procedure()
  ddg.data.in(number)
  ddg.data.out(estimate)
  
  return(estimate)
}

calc.square.root <- function(number,estimate) {
  # calculate square root
  x <- number/estimate
  estimate <- (estimate+x)/2

  ddg.procedure()
  ddg.data.in(number)
  ddg.data.in(estimate)
  ddg.data.out(estimate)
  
  return(estimate)
}

get.difference <- function(number,estimate) {
  # test result
  difference <- abs(number-estimate^2)

  ddg.procedure()
  ddg.data.in(number)
  ddg.data.in(estimate)
  ddg.data.out(difference)

  return(difference)
}

get.check.value <- function(difference,tolerance) {
  #compare difference to tolerance
  check <- difference - tolerance
    
  ddg.procedure()
  ddg.data.in(difference)
  ddg.data.in(tolerance)
  ddg.data.out(check)
  
  return(check)
}

store.result <- function(sqr.roots,number,estimate) {
  sqr.roots[rnum,1] <- number
  sqr.roots[rnum,2] <- estimate
  rnum <<- rnum + 1

  ddg.procedure()
  ddg.data.in(number)
  ddg.data.in(estimate)
  ddg.data.in(sqr.roots)
  ddg.snapshot.out(sqr.roots)  

  return(sqr.roots)
}

write.result <- function(sqr.roots) {
  file.name <- "results.csv"
  file.out <- paste(getwd(),"/",file.name,sep="")
  write.csv(sqr.roots,file.out,row.names=FALSE)

  ddg.procedure()
  ddg.data.in(sqr.roots)
  ddg.file.out(file.name)
}

### Main Program

main <- function() {
  ddg.start("main")
  
  exit <<- 0
  rnum <<- 1
  
  sqr.roots <<- get.initial.values()
  
  ddg.start("calc.square.roots")
  
  while (exit==0) {
    ddg.start("calc.square.root")
  
    number <- get.input()
    
    if (exit==0) {
      check <- number
      estimate <- get.random(number)
      
      while (check > 0) {
        ddg.start("get.next.estimate")
    
        # repeat calculation until tests OK
        estimate <- calc.square.root(number,estimate)
        difference <- get.difference(number,estimate)        
        check <- get.check.value(difference,tolerance)
        
        ddg.finish("get.next.estimate")
      }
      # store results  
      sqr.roots <<- store.result(sqr.roots,number,estimate)
    }
    
    ddg.finish("calc.square.root")
  }
  
  ddg.finish("calc.square.roots")

  write.result(sqr.roots)

  ddg.finish("main")
}

r.script.path <- "~/Documents/Process/DataProvenance/workspace/ddg-r/examples/CalculateSquareRoot/calculate-square-root-input-1.r"
ddg.run(main, 
		r.script.path,
		ddgdir = paste(dirname(r.script.path), "ddg", sep="/"))
#ddg.run(main,ddg.r.script.path,ddg.path)

# display values

sqr.roots

