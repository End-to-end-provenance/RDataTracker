#############################
### CALCULATE SQUARE ROOT ###
#############################

# Calculate square root iteratively
# Record DDG in text format
# Mixed format
# ERB rev. 29-Sep-2013

### Directories

setwd("c:/data/r/ddg/calculate-square-root")
ddg.r.script <- paste(getwd(),"/calculate-square-root-m.r",sep="")
ddg.path <- paste(getwd(),"/ddg",sep="")
source("c:/data/r/lib/ddg-library.r")

### Functions

calc.square.root <- function(n,x) {
  # calculate square root
  y <- n/x
  z <- (x+y)/2

  ddg.procedure("calc.square.root")
  ddg.data.in("calc.square.root","estimate")
  ddg.data.in("calc.square.root","number")
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

### Main Program

ddg.start("main")

# get initial values
number <- 10
tolerance <- 0.00001

ddg.data("number",number)
ddg.data("tolerance",tolerance)
  
# get random seed
estimate <- runif(1,1,number)

ddg.procedure("get.random")
ddg.data.in("get.random","number")
ddg.data.out("get.random","estimate",estimate)

# estimate square root iteratively
check <- number
while (check > 0) {
  # repeat calculation until tests OK
  estimate <- calc.square.root(number,estimate)
  difference <- get.difference(number,estimate)
  check <- get.check.value(difference,tolerance)
}
        
# store result
sqr.root <- data.frame(number,estimate)

ddg.procedure("store.results")
ddg.data.in("store.results","number")
ddg.data.in("store.results","estimate")
ddg.snapshot.out("store.results","sqr-root.csv",sqr.root)

# write result
file.out <- paste(getwd(),"/sqr-root.csv",sep="")
write.csv(sqr.root,file.out,row.names=FALSE)

ddg.procedure("write.result")
ddg.data.in("write.result","sqr-root.csv")
ddg.file.out("write.result","sqr-root.csv",sqr.root)

ddg.finish("main")
ddg.save()

### Display values

sqr.root
