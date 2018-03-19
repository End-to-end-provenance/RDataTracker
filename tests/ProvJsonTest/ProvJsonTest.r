library(jsonvalidate)

schema <- "schema.json"

# pass cases
stopifnot( json_validate("cases/pass1.json", schema) )
stopifnot( json_validate("cases/empty.json", schema) )

# fail cases
stopifnot( ! json_validate("cases/fail1.json", schema) )