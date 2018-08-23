.ddg.init.function.def.table <- function () {
  .ddg.set("ddg.function.defs", 
      data.frame(
          func.name = character(),
          nonlocals.set = vector(),
          stringsAsFactors=FALSE))
}

.ddg.save.func.decl.info <- function (funcname, funcdecl) {
  nonlocals.set <- .ddg.find.nonlocals.set (funcdecl)
  
  # Add to the table
  ddg.func.defs <- .ddg.get ("ddg.function.defs")
  new.row <- data.frame (func.name=character(1), stringsAsFactors=FALSE )
  new.row$func.name[1] <- as.character(funcname)
  new.row$nonlocals.set <- vector ("list", 1)
  
  if (length(nonlocals.set > 0)) {
    new.row$nonlocals.set[[1]] <- nonlocals.set
  }
  ddg.func.defs <- rbind( ddg.func.defs, new.row)
  .ddg.set( "ddg.function.defs", ddg.func.defs )
  
}

.ddg.find.nonlocals.set <- function (funcdecl) {
  return (.ddg.find.assign (funcdecl[[3]], globals.only = TRUE))
}

.ddg.get.nonlocals.set <- function (pfunctions) {
  if( is.null(pfunctions) || is.na(pfunctions) || nrow(pfunctions) == 0) {
    return()
  } 
  
  localfunctions <- pfunctions [!grepl ("package:", pfunctions$ddg.lib), ]
  localfunctions <- localfunctions [localfunctions$ddg.lib != "base", ]
  localfunctions <- localfunctions$ddg.fun
  return (sapply (localfunctions, .ddg.lookup.nonlocals.set))
}


.ddg.lookup.nonlocals.set <- function (funcname) {
  ddg.func.defs <- .ddg.get ("ddg.function.defs")
  nonlocals <- ddg.func.defs [ddg.func.defs$func.name == funcname, "nonlocals.set"]
  if (length(nonlocals) == 0) return (character())
  return (nonlocals[[1]])
}

