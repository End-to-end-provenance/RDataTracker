#############################################
### CALCULATE SQUARE ROOT WITH USER INPUT ###
#############################################

# Calculate square root iteratively
# Get numbers from user
# Quit on "q"
# Record DDG in text format
# Mixed format
# ERB rev. 29-Sep-2013

### R Packages

require(gWidgets)
options(guiToolkit="RGtk2")

### Directories

setwd("c:/data/r/ddg/calculate-square-root-input")
ddg.r.script <- paste(getwd(),"/calculate-square-root-input-m.r",sep="")
ddg.path <- paste(getwd(),"/ddg",sep="")
source("c:/data/r/lib/ddg-library.r")

### Functions

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
 
    ddg.procedure("get.input")
    ddg.data.out("get.input","number",x)
  }
  return(x)
}

get.random <- function(x) {
  # get random seed value
  z <- runif(1,1,x)

  ddg.procedure("get.random")
  ddg.data.in("get.random","number")
  ddg.data.out("get.random","estimate",z)

  return(z)
}

calc.square.root <- function(n,x) {
  # calculate square root
  y <- n/x
  z <- (x+y)/2

  ddg.procedure("calc.square.root")
  ddg.data.in("calc.square.root","number")
  ddg.data.in("calc.square.root","estimate")
  ddg.data.out("calc.square.root","estimate",z)

  return(z)
}

get.difference <- function(n,x) {
  # test result
  z <- abs(n-x^2)

  ddg.procedure("get.difference")
  ddg.data.in("get.difference","estimate")
  ddg.data.in("get.difference","number")
  ddg.data.out("get.difference","difference",z)

  return(z)
}

get.check.value <- function(d,t) {
  #compare difference to tolerance
  z <- d - t
    
  ddg.procedure("get.check.value")
  ddg.data.in("get.check.value","difference")
  ddg.data.in("get.check.value","tolerance")
  ddg.data.out("get.check.value","check",z)
  
  return(z)
}

store.result <- function(n,x) {
  sqr.roots[rnum,1] <<- n
  sqr.roots[rnum,2] <<- x
  rnum <<- rnum + 1

  ddg.procedure("store.results")
  ddg.data.in("store.results","sqr-roots.csv")
  ddg.data.in("store.results","number")
  ddg.data.in("store.results","estimate")
  ddg.snapshot.out("store.results","sqr-roots.csv",sqr.roots)
}

### Main Program

ddg.start("main")

# get initial values
tolerance <- 0.00001
num <- 0
sqrt <- 0
sqr.roots <- data.frame(num,sqrt)
  
ddg.data("tolerance",tolerance)
ddg.snapshot("sqr-roots.csv",sqr.roots)

# calculate square roots iteratively
exit <- 0
rnum <- 1
while (exit==0) {
  number <- get.input()
  if (exit==0) {
    check <- number
    estimate <- get.random(number)
    while (check > 0) {
      # repeat calculation until tests OK
      estimate <- calc.square.root(number,estimate)
      difference <- get.difference(number,estimate)
      check <- get.check.value(difference,tolerance)
    }
    # store results  
    store.result(number,estimate)
  }
}

# write result
file.out <- paste(getwd(),"/sqr.roots.csv",sep="")
write.csv(sqr.roots,file.out,row.names=FALSE)

ddg.procedure("write.result")
ddg.data.in("write.result","sqr-roots.csv")
ddg.file.out("write.result","sqr-roots.csv",sqr.roots)

ddg.finish("main")
ddg.save()

### Display Values

sqr.roots
