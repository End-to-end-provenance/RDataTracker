rm (list=ls())

setwd("c:/data/r/chkpt")
ddg.r.script.path <- paste(getwd(),"/CheckpointTest.r",sep="")
ddg.path <- paste(getwd(),"/ddg",sep="")
source("c:/data/r/lib/ddg-library.r")
source("checkpoint-erb.r")

f <- function() {
	ddg.procedure()
	ddg.data.in("f", "n")
	n
}

main <- function() {
	ddg.start("main")
	n <<- 1
	ddg.data("n", n)
	print (paste("n before checkpoint 1 =", f()))
	checkpoint.file.1 <- ddg.checkpoint("checkpoint.file.1")

	ddg.procedure("n <- 2")
	ddg.data.in("n <- 2", "n")
	n <<- 2
	ddg.data.out("n <- 2", "n", n)
	print (paste("n before checkpoint 2 =", f()))
	checkpoint.file.2 <- ddg.checkpoint("checkpoint.file.2")

	ddg.restore(checkpoint.file.1)
	print (paste("n after restore from checkpoint 1 =", f()))
	
	ddg.procedure("n <- 3")
	ddg.data.in("n <- 3", "n")
	n <<- 3
	ddg.data.out("n <- 3", "n", n)
	
	f()
	
	ddg.restore(checkpoint.file.2)
	print (paste("n after restore from checkpoint 2 =", f()))
	
	ddg.finish("main")
}

ddg.run(main)

