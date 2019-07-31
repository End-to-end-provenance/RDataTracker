library(jsonvalidate)

args <- commandArgs(TRUE)

schema <- args[1]
json <- args[2]
print(schema)
print(json)

tryCatch (json_validate(json, schema, verbose=TRUE),
    error = function (e) {print (sys.calls())})
