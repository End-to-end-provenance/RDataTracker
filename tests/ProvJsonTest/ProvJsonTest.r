
library(jsonvalidate)

args <- commandArgs(TRUE)

schema <- args[1]
json <- args[2]

print( json_validate(schema, json, verbose=TRUE) )