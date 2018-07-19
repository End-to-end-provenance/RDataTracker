###############

# These functions are currently exported, but should no longer be available.
# Removing them also involves cleaning up the NAMESPACE and the help files
# so deferring this temporarily.

# ddg.data creates a data node for a single or complex data value.
# If the value is omitted, the argument passed in for dname is
# evaluated in the calling environment to determine the value.
# If the value is determined to be complex, the output data is
# written out to a csv if possible. Otherwise, the data are
# written out as a .txt file if the variable is determined to
# be an object.

# dname - the label for the node. This can be passed as a string, name,
#   or expression.
# dvalue (optional) - the value of the node.
# graphic.fext (optional) - the file extention to be used for
#   saving the variable if it is a graphical output. Otherwise
#   ignored. Default is jpeg.
#
# TODO:  This should be deleted when we remove calls that used
# to be made manually by the user.

ddg.data <- function(dname, dvalue=NULL, graphic.fext = "jpeg") {
  if (!.ddg.is.init()) return(invisible())
  
  # Look up the value if one was not provided.
  env <- parent.frame()
  .ddg.lookup.value(dname, dvalue, env, "ddg.data", warn=TRUE)
  
  # Save the value appropriately.  If the name is not a string,
  # use the argument instead of the value.
  if (!is.character(dname)) dname <- deparse(substitute(dname))
  .ddg.save.data(dname, dvalue, graphic.fext, env=env)
}

# ddg.file creates a data node of type File by copying an existing
# file to the DDG directory.

# filename - the name of the file to copy, including path to the file
#   if it is not in the working directory.
# dname (optional) - the label for the node. If omitted, the filename,
#   minus the directory path, is used as the label.

# TODO:  This should be deleted when we remove calls that used
# to be made manually by the user.

ddg.file <- function(filename, dname=NULL) {
  if (!.ddg.is.init()) return(invisible())
  
  scope <- if (!is.null(dname)) .ddg.get.scope(dname)
      else NULL
  invisible(.ddg.file.copy(filename, dname, scope))
}

# ddg.data.in creates a data flow edge from data node dname to
# procedure node pname.

# dname - the name of the data node.  This can be passed as
#   a string, name, or expression.
# pname (optional) - the name of the procedure that created
#   this data value.  This can be passed as a string or as
#   a name. It may be omitted if ddg.data.in is called by a function,
#   in which case the name of the function will be used.

ddg.data.in <- function(dname, pname=NULL) {
  if (!.ddg.is.init()) return(invisible())
  
  .ddg.lookup.function.name(pname)
  
  arg <- substitute(dname)
  if (!is.character(arg)) {
    print("ddg.data.in: inside if")
    argname <- deparse(arg)
    dscope <- .ddg.get.scope(argname)
    if (.ddg.data.node.exists(argname, dscope)) {
      print("ddg.data.in: inside inner if")
      dname <- argname
    }
    else {
      print("ddg.data.in: inside inner else")
      dscope <- .ddg.get.scope(argname, for.caller=TRUE)
      if (.ddg.data.node.exists(argname, dscope)) {
        print("ddg.data.in: inside inner inner if")
        dname <- argname
      }
      else {
        print("ddg.data.in: inside inner inner else")
        #### Path taken in FileNodesTest
        # This case is for file names.  The table records the file
        # name, using the scope "undefined".
        dscope <- "undefined"
        if (!is.character(dname) || !.ddg.data.node.exists(dname, dscope)) {
          error.msg <- paste("No data node found for", arg)
          .ddg.insert.error.message(error.msg)
          return()
        }
      }
    }
  }
  else if (exists (arg, envir=parent.frame(), inherits=TRUE)) {
    print("ddg.data.in: inside first else if")
    dscope <- .ddg.get.scope(dname)
  }
  else if (exists (arg, envir=parent.frame(2), inherits=TRUE)) {
    print("ddg.data.in: inside second else if")
    dscope <- .ddg.get.scope(dname, for.caller=TRUE)
  }
  else {
    print("ddg.data.in: inside else")
    dscope <- environmentName(.GlobalEnv)
  }
  
  # Create data flow edge from data node to operation node.
  .ddg.data2proc(dname, dscope, pname)
}

# ddg.data.out creates a data or snapshot node of type Data.
# It also creates a data flow edge from procedure node pname
# to the output node. Used for simple or complex data values.

# dname - the label for the data node being created. This can
#   be passed as a string, name, or expression. Complex data
#   are written to the file dname.
# dvalue (optional) - the value to associate with the node.
#   If no value is given, the argument passed in for dname is
#   evaluated in the calling environment.
# pname (optional) - the name of the procedure that created the
#   data. This can be passed as a string or name. It may be
#   omitted if ddg.data.out is called by a function, in which
#   case the name of the function will be used.
# graphic.fext (optional) - the file extension that should be
#   used when saving a graphics file. Ignored unless the value
#   to be saved is determined to be a graphic.

ddg.data.out <- function(dname, dvalue=NULL, pname=NULL, graphic.fext="jpeg") {
  if (!.ddg.is.init()) return(invisible())
  
  # If no value is provided, get value in calling environment.
  env <- parent.frame()
  .ddg.lookup.value(dname, dvalue, env, "ddg.data.out", warn=TRUE)
  
  # Convert name to a string if necessary.
  if (!is.character(dname)) dname <- deparse(substitute(dname))
  
  # Save the complex data in appropriate format.
  .ddg.save.data(dname, dvalue, graphic.fext, env=env)
  
  .ddg.lookup.function.name(pname)
  
  # Create data flow edge from operation node to data node.
  .ddg.proc2data(pname, dname)
}

# ddg.exception.out creates a data node of type Exception. It
# also creates a data flow edge from the procedure node pname
# to this node.

# dname - the label for the exception node being created.
#   This can be passed as a string or name.
# dvalue (optional) - the value to associate  with the node.
#   If no value is given, the argument passed in for dname is
#   evaluated in the calling environment.
# pname (optional) - the name of the procedure that created this
#   exception. This can be passed as a string or as name. It
#   may be ommited if ddg.exception.out is called by a function,
#   in which case the name of the function will be used.

ddg.exception.out <- function(dname, dvalue=NULL, pname=NULL) {
  if (!.ddg.is.init()) return(invisible())
  
  # If no value is provided, get value in calling environment.
  env <- parent.frame()
  .ddg.lookup.value(dname, dvalue, env, "ddg.exception.out", warn=TRUE)
  
  # Create output exception node.
  .ddg.data.node("Exception", dname, dvalue, "ddg.library")
  
  .ddg.lookup.function.name(pname)
  
  # Create data flow edge from procedure node to exception node.
  .ddg.proc2data(pname, dname)
}

# ddg.url.out creates a data node of type URL called dname with
# address dvalue. It also creates a data flow edge from procedure
# node pname to the URL node dname. Use for URL addresses.

# dname - the label for the data node being created.
# dvalue (optional) - the full URL. If a value is not provided,
#   the argument passed in for dname is evaluated in the calling
#   environment to determine the value.
# pname (optional) - the name of the procedure that created this
#   URL node. This can be passed as a string or as a name. It may
#   be omitted if ddg.url.out is called by a function, in which
#   case the name of the function will be used.

ddg.url.out <- function(dname, dvalue=NULL, pname=NULL) {
  if (!.ddg.is.init()) return(invisible())
  
  # If no value is provided, get value in calling environment.
  env <- parent.frame()
  .ddg.lookup.value(dname, dvalue, env, "ddg.url.out", warn=TRUE)
  
  # URL labels are not necessarily variables, so make sure
  # it is a variable before trying to determine its scope.
  if (exists (dname, inherits = TRUE)) {
    dscope <- .ddg.get.scope(dname)
  }
  else {
    dscope <- environmentName(.GlobalEnv)
  }
  
  # Create output URL node where dvalue = address.
  .ddg.data.node("URL", dname, dvalue, dscope)
  
  .ddg.lookup.function.name(pname)
  
  # Create data flow edge from operation node to URL node.
  .ddg.proc2data(pname, dname, dscope)
}

# ddg.file.out creates a data node of type File called dname by
# copying an existing file to the DDG directory. A data flow edge
# is also created from procedure node pname to data node dname.
# Use for output files already created by the main script. Returns
# the full path to the file that is saved.

# filename - name of the file.  The name should include the path
#   to the file if it is not in the working directory.
# dname (optional) - the label for the node being created. If
#   omitted, the filename, minus the directory path, is used as
#   the label.
# pname (optional) - the name of the procedure that created this
#   node. This can be passed as a string or as a name. It may be
#   omitted if ddg.file.out is called by a function, in which
#   case the name of the function is used.

ddg.file.out <- function(filename, dname=NULL, pname=NULL) {
  if (!.ddg.is.init()) return(invisible())
  
  # Adds the files written to ddg.outfilenodes for use in determining reads
  # and writes in the hashtable.
  .ddg.add.outfiles (filename)
  
  if (is.null(dname)) {
    dname <- basename(filename)
    scope <- NULL
  }
  else {
    scope <- .ddg.get.scope (dname)
  }
  
  # Create output file node called filename and copy file.
  #print(paste("ddg.file.out copying ", filename))
  saved.file <- .ddg.file.copy(filename, dname, scope)
  #print(paste("ddg.file.out done copying ", filename))
  
  .ddg.lookup.function.name(pname)
  
  # Create data flow edge from operation node to file node.
  .ddg.proc2data(pname, dname, scope)
  
  return (saved.file)
}

# ddg.graphic.out creates a data node of type Snapshot called
# dname by capturing the current image in the active graphics
# device and saving it in the DDG directory. The name of the
# file is dname plus the extension specified by the fext
# parameter. Available extensions are bmp, jpeg, png, and tiff.
# A data flow edge is also created from procedure pname to the
# data node dname.

# dname - the label for the node being created.
# pname (optional) - the name of the procedure that created this
#   node. This can be passed as a string or as a name. It may be
#   omitted if ddg.graphic.out is called by a function, in which
#   case the name of the function is used.
# fext (optional) - the file extention to be used for the captured
#   image file. If omitted, this value defaults to jpeg.

ddg.graphic.out <- function(dname, pname=NULL, graphic.fext="jpeg") {
  if(!.ddg.is.init()) return
  # Write out the graphic.
  .ddg.write.graphic(dname, 'Graphical Plot. Not saved in script.', graphic.fext)
  
  .ddg.lookup.function.name(pname)
  
  # Create the data flow edge from oepration node to the file node.
  .ddg.proc2data(pname,dname)
}

