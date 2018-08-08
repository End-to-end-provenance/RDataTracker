library(jsonvalidate)

args <- commandArgs(TRUE)

json <- args[1]
schema <- args[2]

print( json_validate(json, schema, verbose=TRUE) )