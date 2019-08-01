library(jsonvalidate)

args <- commandArgs(TRUE)

schema <- args[1]
json <- args[2]

json_validate(json, schema, verbose=TRUE)
