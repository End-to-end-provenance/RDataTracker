#############################################
### CALCULATE SQUARE ROOT WITH USER INPUT ###
#############################################

# Calculate square root iteratively
# Get numbers from user
# Quit on "q"
# Record DDG in text format
# ERB rev. 22-Nov-2013

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


### Functions

get.initial.values <- function() {
  tolerance <<- 0.00001
  num <- 0
  sqrt <- 0
  sqr.roots <<- data.frame(num,sqrt)
  
  ddg.procedure("get.initial.values")
  ddg.data.out("get.initial.values","tolerance",tolerance)
  ddg.snapshot.out("get.initial.values","sqr-roots.csv",sqr.roots)
}

get.random <- function(x) {
  # get random seed value
  z <- runif(1,1,x)

  ddg.procedure("get.random")
  ddg.data.in("get.random","number")
  ddg.data.out("get.random","estimate",z)

  return(z)
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
  if (is.na(CHOICE)) return("q")
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
  ddg.data.in("get.difference","number")
  ddg.data.in("get.difference","estimate")
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

write.result <- function(fn,xx) {
  file.out <- paste(getwd(),"/",fn,sep="")
  write.csv(xx,file.out,row.names=FALSE)

  ddg.procedure("write.result")
  ddg.data.in("write.result","sqr-roots.csv")
  ddg.file.out("write.result",fn,xx)
}

### Main Program

ddg.start("main")

ddg.start("get.initial.values")
get.initial.values()
ddg.finish("get.initial.values")

exit <- 0
rnum <- 1

ddg.start("calc.square.roots")

while (exit==0) {
  ddg.start("calc.square.root")

  ddg.start("get.input")
  number <- get.input()
  ddg.finish("get.input")

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
    store.result(number,estimate)
  }
  ddg.finish("calc.square.root")
}

ddg.finish("calc.square.roots")

ddg.start("write.result")
write.result("sqr-roots.csv",sqr.roots)
ddg.finish("write.result")

ddg.finish("main")

ddg.save()

### Display Values

sqr.roots
