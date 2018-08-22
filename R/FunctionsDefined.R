.ddg.init.function.def.table <- function () {
  .ddg.set("ddg.function.defs", 
      data.frame(
          func.name = character(),
          globals.set = character(),
          stringsAsFactors=FALSE))
}

.ddg.save.func.decl.info <- function (funcname, funcdecl) {
  globals.set <- .ddg.find.globals.set (funcdecl)
  
  # Add to the table
  ddg.func.defs <- .ddg.get ("ddg.function.defs")
  ddg.func.defs <- rbind( ddg.func.defs, 
      (data.frame (func.name=character(1), globals.set=character(1), stringsAsFactors=FALSE )))
  ddg.func.defs$globals.set[nrow(ddg.func.defs)] <- as.character(funcname)
  
  if (length(globals.set > 0)) {
    ddg.func.defs$globals.set[nrow(ddg.func.defs)] <- globals.set
  }
  .ddg.set( "ddg.function.defs", ddg.func.defs )
  
  print (paste ("number of rows =", nrow(ddg.func.defs)))
  print (paste ("number of cols =", ncol(ddg.func.defs)))
  print (paste ("ddg.function.defs =", .ddg.get("ddg.function.defs")))
}

.ddg.find.globals.set <- function (funcdecl) {
  return (.ddg.find.assign (funcdecl[[3]], globals.only = TRUE))
}

