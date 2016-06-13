###Cushion Co-occurrence Null Modeling
###14 Jan 2014
###


###https://github.com/CommunityGeneticsAnalyses/ComGenR
library(ComGenR)
###Import Data
owd <- getwd()
setwd('../data/')
data <- sapply(dir(),read.csv)
setwd(owd)

###Organize
com <- lapply(data,function(x) x[,5:ncol(x)])
com <- lapply(com,as.matrix)
for (i in 1:length(com)){com[[i]][is.na(com[[i]])] <- 0}
loc <- lapply(data,function(x) as.character(x[,4]))
obs <- lapply(data,function(x) as.character(x[,3]))
##Fix typos
for (i in 1:length(loc)){
  loc[[i]] <- as.character(loc[[i]])
  loc[[i]][loc[[i]]=='cusion'] <- 'cushion'
  loc[[i]][loc[[i]]=='cusion '] <- 'cushion'
  loc[[i]][loc[[i]]=='cushion '] <- 'cushion'
  loc[[i]][loc[[i]]=='open '] <- 'open'
}


###Analyses
##Test for co-occurrence and get ses
cnm.results <- pblapply(com,cnm.test,nits=100)

##Test for the effect of cushion on community similarity (paired PerMANOVA)

##Also do this the way Callaway et al. did it using rs
##Use the response statistic = (I-O)/(I+O)
##rs=function(x){if ((x[1]-x[2])==0){0}else{(x[1]-x[2])/(sum(x))}}

