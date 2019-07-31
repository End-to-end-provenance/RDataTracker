library(jsonvalidate)

args <- commandArgs(TRUE)

schema <- args[1]
json <- args[2]
print(schema)
print(json)
print (dir(dirname(json)))

options (error=function() traceback(2))
tryCatch (json_validate(json, schema, verbose=TRUE),
    error = function (e) {
      print (dir(dirname(json)))
      lines <- readLines(json)
      writeLines(lines)
    })
