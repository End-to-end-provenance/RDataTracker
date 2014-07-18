library(RDataTracker)

#ddg.library <- Sys.getenv("DDG_LIBRARY")
#if (ddg.library == "") {
# ddg.library <- "c:/data/r/ddg/lib/ddg-library.r"
#}
#source(ddg.library)

f <- function(x) {
  g(x)
  h(x)
  return(1)
}

g <- function(x) {
  ddg.procedure()
  ddg.data.in(substitute(x))
  return(1)
}

h <- function(x) {
  ddg.procedure()
  ddg.data.in(substitute(x))
  return(1)
}

someVector <- function() {
  return(c(1, 3, 5))
}

### Run script

x <- 10

f(x)
f(x)

# Then user should do things at console and end by calling ddg.save() from the console.

ddg.save()
