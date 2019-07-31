library(jsonvalidate)

args <- commandArgs(TRUE)

schema <- args[1]
json <- args[2]
print(schema)
print(json)

options (error=function() traceback(2))
tryCatch (json_validate(json, schema, verbose=TRUE),
    error = function (e) {
      lines <- readLines(json)
      writeLines(lines)
    })
