#############################
### CALCULATE SQUARE ROOT ###
#############################

# Calculate square root iteratively
# Record DDG in text format
# ERB rev. 9-Dec-2013

### Directories

#setwd("c:/data/r/test")
ddg.r.script.path <- paste(getwd(),"/calculate-square-root-2.r",sep="")
ddg.path <- paste(getwd(),"/ddg",sep="")
#source("c:/data/r/lib/ddg-library.r")
ddg.library <- "~/Documents/Process/DataProvenance/workspace/ddg-r/ddg-library-emery.r"
source(ddg.library)
ddg.init(ddg.r.script.path,ddg.path)

### Functions

get.initial.values <- function() {
  number <<- 10
  tolerance <<- 0.00001

  ddg.data(number)
  ddg.data(tolerance)
  ddg.procedure(outs=list("number","tolerance"))
}

get.random <- function(number) {
  # get random seed value
  estimate <- runif(1,1,number)

  ddg.data(estimate)
  ddg.procedure(ins=list("number"),outs=list("estimate"))

  return(estimate)
}

calc.square.root <- function(number,estimate) {
  # calculate square root
  x <- number/estimate
  estimate <- (x+estimate)/2

  ddg.data(estimate)
  ddg.procedure(ins=list("number","estimate"),outs=list("estimate"))

  return(estimate)
}

get.difference <- function(number,estimate) {
  # test result
  difference <- abs(number-estimate^2)

  ddg.data(difference)
  ddg.procedure(ins=list("number","estimate"),outs=list("difference"))

  return(difference)
}

get.check.value <- function(difference,tolerance) {
  #compare difference to tolerance
  check <- difference - tolerance
    
  ddg.data(check)
  ddg.procedure(ins=list("difference","tolerance"),outs=list("check"))
  
  return(check)
}

store.result <- function(number,estimate) {
  sqr.root <- data.frame(number,estimate)

  ddg.snapshot(sqr.root)
  ddg.procedure(ins=list("number","estimate"),outs=list("sqr.root"))
  
  return(sqr.root)
}

write.result <- function(sqr.root) {
  file.name = "results.csv"
  file.out <- paste(getwd(),"/",file.name,sep="")
  write.csv(sqr.root,file.out,row.names=FALSE)

  ddg.file(file.name)
  ddg.procedure(ins=list("sqr.root"),outs=list(file.name))
}

### Main Program

ddg.start("main")

ddg.start("get.initial.values")
get.initial.values()
ddg.finish("get.initial.values")

ddg.start("get.random")
estimate <- get.random(number)
ddg.finish("get.random")

ddg.start("get.square.root")

check <- number

#while (check > 0) {
  ddg.start("get.next.estimate")

  # repeat calculation until tests OK
  estimate <- calc.square.root(number,estimate)
  difference <- get.difference(number,estimate)
  check <- get.check.value(difference,tolerance)
  
  ddg.finish("get.next.estimate")
#}

ddg.finish("get.square.root")

ddg.start("write.result")
sqr.root <- store.result(number,estimate)
write.result(sqr.root)
ddg.finish("write.result")

ddg.finish("main")

ddg.save()

### Display values

sqr.root
