#############################
### CALCULATE SQUARE ROOT ###
#############################

# Calculate square root iteratively
# Record DDG in text format
# ERB rev. 22-Nov-2013

### Directories

ddg.library <- Sys.getenv("DDG_LIBRARY")
if (ddg.library == "") {
	ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
}
ddg.r.script.path = "/Users/barbaralerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/trycatchTest/calculate-square-root.r"
ddg.path = "/Users/barbaralerner/Documents/Process/DataProvenance/workspace/ddg-r/examples/trycatchTest/ddg"
source(ddg.library)

### Functions

get.initial.values <- function() {
  number <<- 10
  tolerance <<- 0.00001

  ddg.procedure("get.initial.values")
  ddg.data.out("get.initial.values","number",number)
  ddg.data.out("get.initial.values","tolerance",tolerance)
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
	foobar()
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
  num <- n
  sqrt <- x  
  zz <- data.frame(num,sqrt)

  ddg.procedure("store.results")
  ddg.data.in("store.results","number")
  ddg.data.in("store.results","estimate")
  ddg.snapshot.out("store.results","sqr-root.csv",zz)
  
  return(zz)
}

write.result <- function(fn,zz) {
  file.out <- paste(getwd(),"/",fn,sep="")
  write.csv(zz,file.out,row.names=FALSE)

  ddg.procedure("write.result")
  ddg.data.in("write.result","sqr-root.csv")
  ddg.file.out("write.result",fn,zz)
}

### Main Program

main <- function() {
	ddg.start("main")
	
	ddg.start("get.initial.values")
	get.initial.values()
	ddg.finish("get.initial.values")
	
	ddg.start("get.random")
	estimate <- get.random(number)
	ddg.finish("get.random")
	
	ddg.start("get.square.root")
	
	check <- number
	
	while (check > 0) {
	  ddg.start("get.next.estimate")
	  
	  # Intentional error
	  #foobar()
	
	  # repeat calculation until tests OK
	  estimate <- calc.square.root(number,estimate)
	  difference <- get.difference(number,estimate)
	  check <- get.check.value(difference,tolerance)
	  
	  ddg.finish("get.next.estimate")
	}
	
	ddg.finish("get.square.root")
	
	ddg.start("write.result")
	sqr.root <<- store.result(number,estimate)
	write.result("sqr-root.csv",sqr.root)
	ddg.finish("write.result")
	
	ddg.finish("main")
}

tryCatch(
	main(),
	error=function(e){
		print(structure(e)) 
		print(traceback())
		ddg.text<<-paste(ddg.text, e, "\n")},
	finally={ddg.save()}
)


### Display values

sqr.root
