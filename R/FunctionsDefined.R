.ddg.init.function.def.table <- function () {
  .ddg.set("ddg.function.defs", 
      data.frame(
          func.name = character(),
          nonlocals.set = vector(),
          nonlocals.used = vector(),
          stringsAsFactors=FALSE))
}

.ddg.save.func.decl.info <- function (funcname, funcdecl) {
  nonlocals.set <- .ddg.find.nonlocals.set (funcdecl)
  nonlocals.used <- .ddg.find.nonlocals.used (funcdecl)
  
  # Add to the table
  ddg.func.defs <- .ddg.get ("ddg.function.defs")
  new.row <- data.frame (func.name=character(1), stringsAsFactors=FALSE )
  new.row$func.name[1] <- as.character(funcname)

  new.row$nonlocals.set <- vector ("list", 1)
  if (length(nonlocals.set > 0)) {
    new.row$nonlocals.set[[1]] <- nonlocals.set
  }
  
  new.row$nonlocals.used <- vector ("list", 1)
  if (length(nonlocals.used > 0)) {
    new.row$nonlocals.used[[1]] <- nonlocals.used
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

.ddg.find.nonlocals.used <- function (funcdecl) {
  print ("In .ddg.find.nonlocals.used")
  print ("funcdecl:")
  print (funcdecl)
  
  funcparams <- funcdecl[[2]]
  print ("funcfuncparams:")
  print (funcparams)
  if (is.null (funcparams)) {
    locals <- character()
  }
  else {
    print ("names(funcparams):")
    print (names(funcparams))
    locals <- names (funcparams)
  }
  
  funcbody <- funcdecl[[3]]
  if (funcbody [[1]] == "{" && length(funcbody) > 1) {
    print (paste ("Found block containing", length(funcbody) - 1, "statements"))
    nonlocal.uses <- character()
    for (i in 2:length(funcbody)) {
      var.uses <- .ddg.find.var.uses (funcbody[[i]])
      nonlocal.uses <- unique (c (nonlocal.uses, setdiff (var.uses, locals)))
      var.assigned <- .ddg.find.simple.assign (funcbody[[i]], locals.only = TRUE)
      if (var.assigned != "" && !var.assigned %in% locals) {
        locals <- c(locals, var.assigned)
      }
    }
    print ("locals set")
    print (locals)
    print ("nonlocal.uses")
    print (nonlocal.uses)
  }
  else {
    print ("Found single statement")
    var.uses <- .ddg.find.var.uses (funcbody)
    print ("var.uses:")
    print (var.uses)
    nonlocal.uses <- setdiff (var.uses, locals)
    print ("nonlocal.uses")
    print (nonlocal.uses)
  }
  
  return (nonlocal.uses)
}

