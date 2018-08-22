.ddg.init.function.def.table <- function () {
  .ddg.set("ddg.function.defs", 
      data.frame(
          func.name = character(),
          globals.set = vector(),
          stringsAsFactors=FALSE))
}

.ddg.save.func.decl.info <- function (funcname, funcdecl) {
  globals.set <- .ddg.find.globals.set (funcdecl)
  print ("globals.set =")
  print (globals.set)
  print ("str(globals.set)")
  print (str(globals.set))
  
  # Add to the table
  ddg.func.defs <- .ddg.get ("ddg.function.defs")
  new.row <- data.frame (func.name=character(1), stringsAsFactors=FALSE )
  new.row$func.name[1] <- as.character(funcname)
  new.row$globals.set <- vector ("list", 1)
  
  if (length(globals.set > 0)) {
    new.row$globals.set[[1]] <- globals.set
  }
  ddg.func.defs <- rbind( ddg.func.defs, new.row)
  .ddg.set( "ddg.function.defs", ddg.func.defs )
  
  #print (paste ("number of rows =", nrow(ddg.func.defs)))
  #print (paste ("number of cols =", ncol(ddg.func.defs)))
  print ("ddg.function.defs =")
  print (.ddg.get("ddg.function.defs"))
}

.ddg.find.globals.set <- function (funcdecl) {
  return (.ddg.find.assign (funcdecl[[3]], globals.only = TRUE))
}

.ddg.get.globals.set <- function (funcname) {
  ddg.func.defs <- .ddg.get ("ddg.function.defs")
  globals <- ddg.func.defs [ddg.func.defs$func.name == funcname, "globals.set"][[1]]
  print ("globals =")
  print (globals)
  print ("str(globals) =")
  print (str(globals))
  return (globals)
}

