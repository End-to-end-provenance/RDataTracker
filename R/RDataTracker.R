#################### DDG LIBRARY FOR R ####################

# The functions in this library may be used to annotate an R script
# in order to collect provenance in the form of a data derivation
# graph (DDG) as the script executes. The DDG is saved as a text file
# (ddg.txt) that may be viewed and queried using DDG Explorer.

# Copyright (C) 2014 Emery R. Boose & Barbara S. Lerner & Luis Perez
# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public
#   License along with this program.  If not, see
#   <http://www.gnu.org/licenses/>.

# Create DDG environment variable.

.ddg.env <- new.env(parent=emptyenv())

# Set maximum number of checkpoints in DDG.

ddg.MAX_CHECKPOINTS <- 10

# Set the number of lines the history file keeps (and therefore
# can be analyzed). Note: this setting has no effect on some
# systems.

ddg.MAX_HIST_LINES <- 2^14

library(tools)
library(jsonlite)

#-------- FUNCTIONS TO MANAGE THE GLOBAL VARIABLES--------#

# Global variables cannot be used directly in a library.  Instead,
# we need to place the variables in our own environment.  These
# functions make that environment easier to use.

.onLoad <- function(libname, pkgname) {
  .ddg.init.tables()
}

.ddg.set <- function(var, value) {
  .ddg.env[[var]] <- value
  return(invisible(.ddg.env[[var]]))
}

.ddg.is.set <- function(var) {
  return(exists(var, envir=.ddg.env))
}

.ddg.get <- function(var) {
  if (!.ddg.is.set(var)) {
    error.msg <- paste("No binding for", var, ". DDG may be incorrect!")
    .ddg.insert.error.message(error.msg)
    return(NULL)
  }
  else {
    return(.ddg.env[[var]])
  }
}

# .ddg.clear removes all objects from the .ddg.env environment.

.ddg.clear <- function() {
  # reinitialize tables
  .ddg.init.tables()
}

##### Getters for specific variables

.ddg.debug.lib <- function() {
  return (.ddg.get("ddg.debug.lib"))
}

.ddg.break <- function() {
  return (.ddg.get("ddg.break"))
}

.ddg.break.ignore <- function() {
  return (.ddg.get("ddg.break.ignore"))
}

.ddg.path <- function() {
  return (.ddg.get("ddg.path"))
}

.ddg.data.dir <- function() {
  return ("data")
}

.ddg.path.data <- function() {
  return(paste(.ddg.path(), .ddg.data.dir() , sep="/"))
}

.ddg.path.debug <- function() {
  return(paste(.ddg.path(), "/debug", sep=""))
}

.ddg.path.scripts <- function() {
  return(paste(.ddg.path(), "/scripts", sep=""))
}

.ddg.save.debug <- function() {
  return(.ddg.get("ddg.save.debug"))
}

.ddg.dnum <- function() {
  return (.ddg.get("ddg.dnum"))
}

.ddg.pnum <- function() {
  return (.ddg.get("ddg.pnum"))
}

.ddg.enum <- function() {
  return (.ddg.get("ddg.enum"))
}

.ddg.data.nodes <- function() {
  return (.ddg.get("ddg.data.nodes"))
}

.ddg.proc.nodes <- function() {
  return (.ddg.get("ddg.proc.nodes"))
}

.ddg.edges <- function() {
  return (.ddg.get("ddg.edges"))
}

.ddg.initial.env <- function() {
  return(.ddg.get("ddg.initial.env"))
}

.ddg.enable.console <- function() {
  return (.ddg.get(".ddg.enable.console"))
}

.ddg.annotate.on <- function() {
  return (.ddg.get("ddg.annotate.on"))
}

.ddg.annotate.off <- function() {
  return (.ddg.get("ddg.annotate.off"))
}

.ddg.is.sourced <- function() {
  return (.ddg.get(".ddg.is.sourced"))
}

.ddg.source.parsed <- function() {
  return(.ddg.get(".ddg.source.parsed"))
}

.ddg.parsed.num <- function() {
  return(.ddg.get(".ddg.parsed.num"))
}

.ddg.sourced.scripts <- function() {
  return(.ddg.get(".ddg.sourced.scripts"))
}

.ddg.next.script.num <- function() {
  return(.ddg.get(".ddg.next.script.num"))
}

.ddg.script.num.stack <- function() {
  return(.ddg.get(".ddg.script.num.stack"))
}

.ddg.enable.source <- function() {
  return(.ddg.is.set("from.source") && .ddg.get("from.source"))
}

.ddg.start.proc.time <- function() {
  if (.ddg.is.set(".ddg.proc.start.time")) return (.ddg.get(".ddg.proc.start.time"))
  else return (0)
}

.ddg.statement.num <- function() {
  return(.ddg.get("ddg.statement.num"))
}

.ddg.statements <- function() {
  return(.ddg.get("ddg.statements"))
}

.ddg.statement <- function(i) {
  ddg.statements <- .ddg.statements()
  return(ddg.statements[[i]])
}

.ddg.loop.num <- function() {
  return(.ddg.get("ddg.loop.num"))
}

.ddg.loops <- function() {
  return(.ddg.get("ddg.loops"))
}

# value should be TRUE or FALSE
# Keeps track of whether the last loop has all iterations
# recorded or not.
.ddg.set.details.omitted <- function (value) {
  .ddg.set ("details.omitted", value)
}

.ddg.were.details.omitted <- function () {
  .ddg.get ("details.omitted")
}


# Functions that allow us to save warnings when they occur
# so that we can create the warning node after the node
# that caused the warning is created.

# .ddg.set.warning is attached as a handler when we evaluate
# expressions.  It saves the warning so that a warning
# node can be created after the procedural node that
# corresponds to the expression that caused the warning
#
# w - the simplewarning object created by R

.ddg.set.warning <- function(w) {
  .ddg.set(".ddg.warning", w)
}

.ddg.clear.warning <- function() {
  .ddg.set(".ddg.warning", NA)
}

.ddg.get.warning <- function () {
  return (.ddg.get(".ddg.warning"))
}

.ddg.warning.occurred <- function() {
  return (.ddg.is.set(".ddg.warning") && !is.na(.ddg.get(".ddg.warning")))
}

##### Mutators for specific common actions

.ddg.inc <- function(var) {
  value <- .ddg.get(var)
  .ddg.set(var, value + 1)
}

.ddg.dec <- function(var) {
  value <- .ddg.get(var)
  .ddg.set(var, value - 1)
}

.ddg.append <- function(...) {
  text <- .ddg.get("ddg.txt")
  if (.ddg.debug.lib() && length(text) > 1) stop()
  .ddg.set("ddg.txt", paste(text, ..., sep=""))
}

.ddg.append.inc <- function(...) {
  increment <- .ddg.get("ddg.increment")
  .ddg.set("ddg.increment", paste(increment, ..., sep=""))
}

.ddg.append.activity <- function(...){
  text <- .ddg.get('ddg.activity')
  if(text!=""){
    text <- paste(text, ",\n")
  }
  .ddg.set("ddg.activity", paste(text, ..., sep=""))
}

.ddg.append.entity <- function(...){
  text <- .ddg.get('ddg.entity')
  if(text!=""){
    text <- paste(text, ",\n")
  }
  .ddg.set("ddg.entity", paste(text, ..., sep=""))
}

.ddg.append.wasInformedBy <- function(...){
  text <- .ddg.get('ddg.wasInformedBy')
  if(text!=""){
    text <- paste(text, ",\n")
  }
  .ddg.set("ddg.wasInformedBy", paste(text, ..., sep=""))
}

.ddg.append.wasGeneratedBy <- function(...){
  text <- .ddg.get('ddg.wasGeneratedBy')
  if(text!=""){
    text <- paste(text, ",\n")
  }
  .ddg.set("ddg.wasGeneratedBy", paste(text, ..., sep=""))
}

.ddg.append.used <- function(...){
  text <- .ddg.get('ddg.used')
  if(text!=""){
    text <- paste(text, ",\n")
  }
  .ddg.set("ddg.used", paste(text, ..., sep=""))
}

.ddg.add.rows <- function(df, new.rows) {
  table <- .ddg.get(df)
  .ddg.set(df, rbind(table, new.rows))
}

.ddg.push <- function(x, value) {
  return(assign(as.character(substitute(x)), c(x, value), parent.frame()))
}

.ddg.pop <- function(x) {
  return(assign(as.character(substitute(x)), x[-length(x)], parent.frame()))
}

.ddg.add.ddgstatement <- function(parsed.stmt) {
  ddg.statements <- c(.ddg.statements(), parsed.stmt)
  .ddg.set("ddg.statements", ddg.statements)
}

.ddg.add.loop <- function() {
  ddg.loops <- c(.ddg.loops(), 0)
  .ddg.set("ddg.loops", ddg.loops)
}

#-------------------BASIC FUNCTIONS-----------------------#

# .ddg.get.initial.env creates a table of non-ddg objects present in the
# R environment before the script is executed.

.ddg.get.initial.env <- function() {
  e <- globalenv()
  e.ls <- ls(e, all.names=TRUE)

  not.ddg.func <- function (name) {
    return (!grepl("ddg", name) && name != ".onLoad")
  }

  x <- Filter (not.ddg.func, e.ls)

  ddg.initial.env <- data.frame(x)
  colnames(ddg.initial.env) <- "ddg.name"

  .ddg.set("ddg.initial.env", ddg.initial.env)
}


# .ddg.init.tables creates data frames to store the initial environment,
# procedure nodes, data nodes, edges, function return values, and
# checkpoints. It also initializes selected constants and variables.
# Tables are saved as tab-delimited files in ddg.save.

.ddg.init.tables <- function() {
  size <- 100

  .ddg.get.initial.env()

  .ddg.set("ddg.proc.nodes", data.frame(ddg.type = character(size),
          ddg.num = numeric(size),
          ddg.name = character(size),
          ddg.value = character(size),
          ddg.return.linked = logical(size),
          ddg.auto.created = logical(size),
          ddg.time = numeric(size),
          ddg.snum = numeric(size),
          ddg.startLine = numeric(size),
          ddg.startCol= numeric(size),
          ddg.endLine= numeric(size),
          ddg.endCol= numeric(size),
          stringsAsFactors=FALSE))

  .ddg.set("ddg.data.nodes", data.frame(ddg.type = character(size),
          ddg.num = numeric(size),
          ddg.name = character(size),
          ddg.value = character(size),
          ddg.val.type = character(size),
          ddg.scope = character(size),
          ddg.from.env = logical(size),
          ddg.time = character(size),
          ddg.loc = character(size),
          ddg.current = logical(size), stringsAsFactors=FALSE))

  .ddg.set("ddg.edges", data.frame(ddg.num = numeric(size),
          ddg.type = character(size),
          ddg.from = character(size),
          ddg.to = character(size), stringsAsFactors=FALSE))

  # Create procedure and data node counters.
  .ddg.set("ddg.pnum", 0)
  .ddg.set("ddg.dnum", 0)
  .ddg.set("ddg.enum", 0)

  # Create DDG string. This string is written to file when ddg.save
  # is called.
  .ddg.set("ddg.txt", "")

  # Create DDG increment string. This string contains the current
  # incremental change to the DDG string.
  .ddg.set("ddg.increment", "")

  # Create strings used to generate the JSON file.
  .ddg.set("ddg.activity", "")
  .ddg.set("ddg.entity", "")
  .ddg.set('ddg.wasInformedBy', "")
  .ddg.set('ddg.wasGeneratedBy', "")
  .ddg.set('ddg.used', "")

  # Used to control debugging output.  If already defined, don't
  # change its value.
  if (!.ddg.is.set("ddg.debug.lib")) .ddg.set("ddg.debug.lib", FALSE)

  # Used to control script debugging.
  .ddg.set("ddg.break", FALSE)
  .ddg.set("ddg.break.ignore", FALSE)

  # Used to control sourcing. If already defined, don't change
  # its value.
  if (!.ddg.is.set("from.source")) .ddg.set("from.source", FALSE)

  # Set current number of checkpoints.
  .ddg.set("ddg.checkpoint.num", 0)

  # Create table for checkpoints.
  .ddg.set("ddg.checkpoints",
          data.frame(filename=character(ddg.MAX_CHECKPOINTS),
          checkpoint.name=character(ddg.MAX_CHECKPOINTS),
          stringsAsFactors=FALSE))

  # Record last command from the preceding console block.
  .ddg.set(".ddg.last.cmd", NULL)

  # Record value returned by calls to ddg.return.
  # ddg.call - the string representing the call, like "f(a)".
  # line - the line where the function is called that is now returning
  # return.used - remembers if this function return value has been
  #   linked to the caller.
  # return.node.id - the id of the data node that holds the return
  #   value.
  .ddg.set(".ddg.return.values",
          data.frame(ddg.call=character(size),
          line = integer(size),
          return.used = logical(size),
          return.node.id = integer(size),
          stringsAsFactors=FALSE))

  .ddg.set(".ddg.num.returns", 0)

  # Record the current command to be opened during console execution
  # (used when executing a script using ddg.source).
  .ddg.set(".ddg.possible.last.cmd", NULL)

  # Keep track of history.
  .ddg.set(".ddg.history.timestamp", NULL)

  # Keep track of the last device seen (0 implies NULL).
  .ddg.set("prev.device", 0)

  # Store path of current script.
  .ddg.set("ddg.r.script.path", NULL)

  # Store path of current ddg.
  .ddg.set("ddg.path", NULL)

  # No ddg initialized.
  .ddg.set(".ddg.initialized", FALSE)

  # No history file.
  .ddg.set(".ddg.history.file", NULL)

  # Console is disabled.
  .ddg.set(".ddg.enable.console", FALSE)

  # Functions to be annotated.
  .ddg.set("ddg.annotate.on", NULL)

  # Functions not to be annotated.
  .ddg.set("ddg.annotate.off", NULL)

  # Script sourced with ddg.source
  .ddg.set(".ddg.is.sourced", FALSE)

  # Number of first sourced script (main script).
  .ddg.set(".ddg.next.script.num", 0)

  # Number of first parsed command.
  .ddg.set(".ddg.parsed.num", 1)

  # Stack for sourced files
  .ddg.set(".ddg.script.num.stack", 0)

  # Table of sourced scripts
  .ddg.set(".ddg.sourced.scripts", NULL)

  # Table of script, line & parsed command numbers
  .ddg.set(".ddg.source.parsed", NULL)

  # Save debug files on debug directory
  .ddg.set("ddg.save.debug", FALSE)

  # DDGStatement number
  .ddg.set("ddg.statement.num", 0)

  # DDGStatements list
  .ddg.set("ddg.statements", list())

  # Control loop number
  .ddg.set("ddg.loop.num", 0)

  # Control loop list
  .ddg.set("ddg.loops", list())

  # Loop annotation
  .ddg.set("ddg.loop.annotate", TRUE)

  # Set max.snapshot.size for console mode.
  if (!.ddg.is.set("ddg.max.snapshot.size")) {
    .ddg.set("ddg.max.snapshot.size", 100)
  }

  # List of files read and written
  .ddg.set("ddg.infilenodes", list())
  .ddg.set("ddg.outfilenodes", list())

  # Data frame containing file reads and writes
  .ddg.set("ddg.hashtable", data.frame())
}

# .ddg.set.history provides a wrapper to change the number of
# history lines during execution of an R script.

# lines - number of lines in history file.

.ddg.set.history <- function(lines=16384){
  Sys.setenv("R_HISTSIZE" = lines)
}

# .ddg.init.environ() sets up the filesystem and R environments
# for use.

.ddg.init.environ <- function() {
  dir.create(.ddg.path(), showWarnings=FALSE)
  dir.create(.ddg.path.data(), showWarnings=FALSE)
  dir.create(.ddg.path.debug(), showWarnings=FALSE)
  dir.create(.ddg.path.scripts(), showWarnings=FALSE)

  if (interactive() && .ddg.enable.console()) {
    .ddg.set('ddg.original.hist.size', Sys.getenv('R_HISTSIZE'))
    .ddg.set.history()
  }
}

# .ddg.sourced.script.names returns a string containing the names
# of sourced scripts, if any. If no scripts were sourced it returns
# an empty string.

.ddg.sourced.script.names <- function() {
  ss <- .ddg.sourced.scripts()
  # First row is main script.
  if (nrow(ss) == 1) snames <- ""
  else {
    snames <- ss[ss$snum >= 1, "sname"]
    snames <- paste0(snames, collapse=",")
  }
  return(snames)
}

# .ddg.sourced.script.timestamps returns a string containing the timestamps
# of sourced scripts, if any. If no scripts were sourced it returns
# an empty string.

.ddg.sourced.script.timestamps<- function() {
  ss <- .ddg.sourced.scripts()
  # First row is main script.
  if (nrow(ss) == 1) stimes <- ""
  else {
    snames <- ss[ss$snum >= 1, "sname"]
    #print(paste(".ddg.sourced.scripts: snames = ", snames))
    stimes <- file.info(snames)$mtime
    #print(paste(".ddg.sourced.script.timestamps: after file.info, stimes = ", stimes))
    stimes <- .ddg.format.time(stimes)
    #print(paste(".ddg.sourced.script.timestamps: after format, stimes = ", stimes))
    stimes <- paste0(stimes, collapse=",")
    #print(paste(".ddg.sourced.script.timestamps: after collapsing, stimes = ", stimes))
  }
  return(stimes)
}

# .ddg.sourced.script.names.json returns sourced script names,
# numbers and timestamps for the JSON file.

.ddg.sourced.script.names.json <- function() {
  ss <- .ddg.sourced.scripts()
  # First row is main script.
  if (nrow(ss) == 1) {
    output <- "\"\"\n"
  } else {
    ss <- ss[ss$snum > 0, ]
    stimes <- file.info(ss$sname)$mtime
    stimes <- .ddg.format.time(stimes)

    scriptarray <- paste("\t{\"number\" : \"", ss[ , 1], "\",
                             \"name\" : \"",ss[ , 2], "\",
                             \"timestamp\" : \"",stimes, "\"}",
                         sep = "", collapse =",\n")
    output <- paste("[\n", scriptarray, " ]", sep = "")
  }
  return(output)
}

# ddg.installedpackages() returns information on packages installed at the time of execution
# and their versions.
.ddg.installedpackages <- function(){
  packages <- devtools::session_info()
  packages <- packages [[2]]
  installed <- packages[packages[,2] == "*",]
  installed <- installed[ ,c(1,3)]
  return(installed)
}

.ddg.installedpackages.json <- function(){
  installed <- .ddg.installedpackages()
  output <- "\"rdt:installedPackages\" : [\n\t"
  packagearray <- paste("{\"package\" : \"", installed[,1], "\", \"version\" : \"",installed[,2], "\"}", sep = "", collapse =",\n\t")
  output <- paste(output, packagearray, "]", sep = "")
  return(output)
}
# ddg.txt.environ returns environment information for the ddg.txt
# string.

.ddg.txt.environ <- function() {
  architecture <- R.Version()$arch
  operating.system <- .Platform$OS.type
  r.version <- R.Version()$version
  lib.version <- packageVersion("RDataTracker")
  time <- .ddg.get("ddg.start.time")
  environ <- paste("Architecture=\"", architecture, "\"\n", sep="")
  environ <- paste(environ, "OperatingSystem=\"", operating.system, "\"\n", sep="")
  environ <- paste(environ, "Language=\"R\"\n", sep="")
  environ <- paste(environ, "LanguageVersion=\"", r.version, "\"\n", sep="")
  environ <- paste(environ, "RDataTrackerVersion=\"", lib.version, "\"\n", sep="")
  ddg.r.script.path <- .ddg.get("ddg.r.script.path")
  if (!is.null(ddg.r.script.path)) {
    environ <- paste(environ, "Script=\"", ddg.r.script.path, "\"\n", sep="")
    environ <- paste(environ, "SourcedScripts=\"", .ddg.sourced.script.names(), "\"\n", sep="")
    environ <- paste(environ, "ProcessFileTimestamp=\"", .ddg.format.time(file.info(ddg.r.script.path)$mtime), "\"\n", sep="")
    #print(paste(".ddg.txt.environ: .ddg.sourced.script.names() = ", .ddg.sourced.script.names()))
    stimes <- .ddg.sourced.script.timestamps()
    if (stimes != "") {
      environ <- paste(environ, "SourcedScriptTimestamps=\"", stimes, "\"\n", sep="")
    }
  }
  else {
    environ <- paste(environ, "Script=\"Console\"\n")
    environ <- paste(environ, "ProcessFileTimestamp=\"", time, "\"\n", sep="")
  }
  environ <- paste(environ, "WorkingDirectory=\"", getwd(), "\"\n", sep="")
  environ <- paste(environ, "DDGDirectory=\"", .ddg.path(), "\"\n", sep="")
  environ <- paste(environ, "DateTime=\"", time, "\"\n", sep="")
  installed <- .ddg.installedpackages()
  environ <- paste(environ, "InstalledPackages=\"",
                   paste(installed[,1], installed[,2], sep = " ", collapse = ", "), "\"\n", sep = "")
  return (environ)
}

# .ddg.txt.current returns the current ddg.txt string.

.ddg.txt.current <- function() {
  environ <- .ddg.txt.environ()
  pnum <- .ddg.get("ddg.pnum")
  dtxt <- .ddg.get("ddg.txt")
  dstr <- paste(environ, pnum, "\n", dtxt, sep="")
  return(dstr)
}

# .ddg.txt.write writes the current ddg.txt string to the file
# ddg.txt on the ddg directory.

.ddg.txt.write <- function() {
  fileout <- paste(.ddg.path(), "/ddg.txt", sep="")
  # if (interactive()) print(paste("Saving DDG in ", fileout))
  ddg.txt <- .ddg.txt.current()
  write(ddg.txt, fileout)
}

# .ddg.txt.increment returns the current incremental change to the
# ddg.txt string.

.ddg.txt.increment <- function() {
  # Get current increment.
  dtxt <- .ddg.get("ddg.increment")
  # Reset for next increment.
  .ddg.set("ddg.increment", "")
  return(dtxt)
}

# .ddg.json.nv returns a name-value pair for the ddg.json string.

.ddg.json.nv <- function(name, value) {
  jstr <- paste("\"", name, "\" : \"", value, "\",\n", sep="")
  return(jstr)
}

.ddg.json.prefix <- function(){
  # add json prefix
  prefix <- "\"prefix\" : {\n\"prov\" : \"http://www.w3.org/ns/prov#\",\n\"rdt\" : \"http://rdatatracker.org/\"\n},\n"
  return (prefix)
}


# .ddg.json.environ returns prefix and environment information
# for the ddg.json string.

.ddg.json.environ <- function() {
  # add environment entities
  environ <- ""
  environ <- paste(environ, "\n\"environment\" : {\n", sep="")

  environ <- paste(environ, .ddg.json.nv("rdt:name", "environment"), sep="")

  architecture <- R.Version()$arch
  environ <- paste(environ, .ddg.json.nv("rdt:architecture", architecture), sep="")

  operating.system <- .Platform$OS.type
  environ <- paste(environ, .ddg.json.nv("rdt:operatingSystem", operating.system), sep="")

  language="R"
  environ <- paste(environ, .ddg.json.nv("rdt:language", language), sep="")

  r.version <- R.Version()$version
  environ <- paste(environ, .ddg.json.nv("rdt:rVersion", r.version), sep="")

  ddg.r.script.path <- .ddg.get("ddg.r.script.path")
  if (!is.null(ddg.r.script.path)) {
    script <- ddg.r.script.path
    sourced.scripts <- .ddg.sourced.script.names.json()
    script.timestamp <- .ddg.format.time(file.info(ddg.r.script.path)$mtime)
  } else {
    script <- ""
    sourced.scripts <- ""
    script.timestamp <- ""
    sourced.scripts.timestamps <- ""
  }

  environ <- paste(environ, .ddg.json.nv("rdt:script", ddg.r.script.path), sep="")

  environ <- paste(environ, "\"rdt:sourcedScripts\" : ", sourced.scripts, ",\n", sep="")

  environ <- paste(environ, .ddg.json.nv("rdt:scriptTimeStamp", script.timestamp), sep="")

  working.directory=getwd()
  environ <- paste(environ, .ddg.json.nv("rdt:workingDirectory", working.directory), sep="")

  ddg.directory=.ddg.path()
  environ <- paste(environ, .ddg.json.nv("rdt:ddgDirectory", ddg.directory), sep="")

  ddg.timestamp <- .ddg.get("ddg.start.time")
  environ <- paste(environ, .ddg.json.nv("rdt:ddgTimeStamp", ddg.timestamp), sep="")

  lib.version <- packageVersion("RDataTracker")
  environ <- paste(environ, .ddg.json.nv("rdt:rdatatrackerVersion", lib.version), sep="")

  environ <- paste(environ, .ddg.installedpackages.json(), sep = "")
  environ <- paste(environ, "\n}", sep = "")

  return(environ)
}


# .ddg.json.procedure.node adds a procedure node to the ddg.json
# string.

.ddg.json.procedure.node <- function(id, pname, ptype, ptime, snum, pos) {

  if (is.object(pos)) {
    jstr <- paste("\n\"p", id, "\" : {\n\"rdt:name\" : \"", pname, "\",\n\"rdt:type\" : \"", ptype,
        "\",\n\"rdt:elapsedTime\" : \"", ptime, "\",\n\"rdt:scriptNum\" : \"", snum,
        "\",\n\"rdt:startLine\" : \"", pos@startLine, "\"",
        ",\n\"rdt:startCol\" : \"", pos@startCol, "\"",
        ",\n\"rdt:endLine\" : \"", pos@endLine, "\"",
        ",\n\"rdt:endCol\" : \"", pos@endCol, "\"",
        "\n}", sep="")
  }
  else {
    jstr <- paste("\n\"p", id, "\" : {\n\"rdt:name\" : \"", pname, "\",\n\"rdt:type\" : \"", ptype,
        "\",\n\"rdt:elapsedTime\" : \"", ptime, "\",\n\"rdt:scriptNum\" : \"", snum,
        "\",\n\"rdt:startLine\" : \"NA\"",
        ",\n\"rdt:startCol\" : \"NA\"",
        ",\n\"rdt:endLine\" : \"NA\"",
        ",\n\"rdt:endCol\" : \"NA\"",
        "\n}", sep="")
  }

  .ddg.append.activity(jstr)
}


# .ddg.json.data.node adds a data node to the ddg.json string.

.ddg.json.data.node <- function(id, dname, dvalue, val.type, dtype, dscope, from.env, dhash, dtime, dloc) {

  jstr <- paste("\n\"d", id, "\" : {\n\"rdt:name\" : \"", dname, "\",\n\"rdt:value\" : \"", dvalue, "\",\n\"rdt:valType\" : ", val.type, ",\n\"rdt:type\" : \"", dtype, "\",\n\"rdt:scope\" : \"", dscope, "\",\n\"rdt:fromEnv\" : \"", from.env, "\",\n\"rdt:MD5hash\" : \"", dhash, "\",\n\"rdt:timestamp\" : \"", dtime, "\",\n\"rdt:location\" : \"", dloc, "\"\n}", sep="")

  .ddg.append.entity(jstr)
}


# .ddg.json.control.edge adds a control flow edge to the ddg.json
# string.

.ddg.json.control.edge <- function(id, node1, node2) {

  jstr <- paste("\n\"e", id , "\" : {\n\"prov:informant\" : \"", node1, "\",\n\"prov:informed\" : \"", node2, "\"\n}", sep="")

  .ddg.append.wasInformedBy(jstr)
}


# .ddg.json.data.out.edge adds an output data flow edge to the
# ddg.json string.

.ddg.json.data.out.edge <- function(id, node1, node2) {

  jstr <- paste("\n\"e", id , "\" : {\n\"prov:entity\" : \"", node2, "\",\n\"prov:activity\" : \"", node1, "\"\n}", sep="")

  .ddg.append.wasGeneratedBy(jstr)
}


# .ddg.json.data.in.edge adds an input data flow edge to the
# ddg.json string.

.ddg.json.data.in.edge <- function(id, node1, node2) {

  jstr <- paste("\n\"e", id , "\" : {\n\"prov:activity\" : \"", node2, "\",\n\"prov:entity\" : \"", node1, "\"\n}", sep="")

  .ddg.append.used(jstr)
}


# .ddg.json.current returns the current ddg.json string.

.ddg.json.current <- function() {
  prefix <- .ddg.json.prefix()
  environ <- .ddg.json.environ()
  .ddg.append.activity(environ)
  activity <- .ddg.get("ddg.activity")
  entity <- .ddg.get("ddg.entity")
  wasInformedBy <- .ddg.get('ddg.wasInformedBy')
  wasGeneratedBy <- .ddg.get('ddg.wasGeneratedBy')
  used <- .ddg.get('ddg.used')
  ddg.json <- paste("{\n\n", prefix, '"activity":{\n', activity, '},\n', '"entity":{\n', entity, '},\n', '"wasInformedBy":{\n', wasInformedBy, '},\n', '"wasGeneratedBy":{\n', wasGeneratedBy, '},\n', '"used":{\n', used, '}\n', "}", sep="")
  return(ddg.json)
}


# .ddg.json.write writes the current ddg.json string to the file
# ddg.json on the ddg directory.

.ddg.json.write <- function() {
  fileout <- paste(.ddg.path(), "/ddg.json", sep="")
  #if (interactive()) print(paste("Saving DDG in ", fileout))
  ddg.json <- .ddg.json.current()
  write(ddg.json, fileout)
}

# .ddg.output.procedure.node outputs a procedure node.

.ddg.output.procedure.node <- function(ptype, pname, pvalue, auto.created, ptime, snum, pos) {
  # Get counter
  ddg.pnum <- .ddg.get("ddg.pnum")

  # Prepare values
  pname <- gsub("\\\"", "\\\\\"", pname)

  if (pvalue !="") {
    pvalue <- gsub("\\\"", "\\\\\"", pvalue)
    value.str <- paste(" Value=\"", pvalue, "\"", sep="")
  } else value.str <- ""

  if (is.object(pos)) {
    dtxt <- paste(ptype, " p", ddg.pnum, " \"", ddg.pnum, "-", pname, "\"", value.str, " Time=\"", ptime ,
        "\" Script=\"", snum, "\"", " Pos=\"", pos@startLine, ",", pos@startCol, ",", pos@endLine, ",", pos@endCol, "\";\n", sep="")
    #print(".ddg.output.procedure.node: dtxt =")
    #print(dtxt)
  }
  else {
    dtxt <- paste(ptype, " p", ddg.pnum, " \"", ddg.pnum, "-", pname, "\"", value.str, " Time=\"", ptime ,
        "\" Script=\"", snum, "\"", " Pos=\"NA\";\n", sep="")
  }

  # Record in ddg.txt
  .ddg.append(dtxt)

  # Record in ddg.increment
  .ddg.append.inc(dtxt)

  # Record in ddg.json
  .ddg.json.procedure.node(ddg.pnum, pname, ptype, ptime, snum, pos)
}


# .ddg.output.data.node outputs a data node.

.ddg.output.data.node <- function(dscriptpath, dtype, dname, dvalue, val.type, dscope, from.env, dhash, drw, dtime, dloc) {
  # Get counter
  ddg.dnum <- .ddg.get("ddg.dnum")

  # Prepare values
  if (from.env) dname <- paste(dname, " [ENV]", sep="")

  if (dvalue != "") value.str <- paste(" Value=\"", dvalue, "\"", sep="")
  else value.str <- ""

  if(dscriptpath != "" ) dscriptpath.str <- paste(" Script Path=\"", dscriptpath, "\"", sep="")
  else dscriptpath.str <- ""
  
  if(val.type != "" ) val.type.str <- paste(" ValType=\"", .ddg.replace.quotes(val.type), "\"", sep="")
  else val.type.str <- ""

  if (dtime != "") time.str <- paste(" Time=\"", dtime, "\"", sep="")
  else time.str <- ""

  if (dloc != "") loc.str <- paste(" Location=\"", dloc, "\"", sep="")
  else loc.str <- ""
  
  if (dhash != "" && !is.na(dhash)) dhash.str <- paste(" MD5 Hash=\"", dhash, "\"", sep="")
  else dhash.str <- ""

  if (drw != "") drw.str <- paste(" RW=\"", drw, "\"", sep="")
  else drw.str <- ""

  dtxt <- paste(dtype, " d", ddg.dnum, " \"", ddg.dnum, "-", dname, "\"", value.str, val.type.str, time.str, loc.str, dhash.str, ";\n", sep="")
  
  # Record in ddg.txt
  .ddg.append(dtxt)

  # Record in ddg.increment
  .ddg.append.inc(dtxt)

  # Record in ddg.json
  .ddg.json.data.node(ddg.dnum, dname, dvalue, val.type, dtype, dscope, from.env, dhash, dtime, dloc)
}


# .ddg.output.edge outputs a control flow or data flow edge.

.ddg.output.edge <- function(etype, node1, node2) {
  # Get counter
  ddg.enum <- .ddg.get("ddg.enum")

  # Prepare values

  if (etype == "cf") dtxt <- paste("CF ", node1, " ", node2, "\n", sep="")
  else dtxt <- paste("DF ", node1, " ", node2, "\n", sep="")

  # Record in ddg.txt
  .ddg.append(dtxt)

  # Record in ddg.increment
  .ddg.append.inc(dtxt)

  # Record in ddg.json
  if (etype == "cf") .ddg.json.control.edge(ddg.enum, node1, node2)
  else if (etype == "df.in") .ddg.json.data.in.edge(ddg.enum, node1, node2)
  else .ddg.json.data.out.edge(ddg.enum, node1, node2)
}


# .ddg.data.objects returns a list of data objects used or created by
# the script. The list includes node number, name, value, type, scope,
# line number (if any) where the object was created, and line numbers(s)
# (if any) where the object was used. The scope is set to ENV if the
# object was not created by the script and was taken from the pre
# -existing environment.

.ddg.data.objects <- function() {
  # Get data node, procedure node, and edge tables.
  dnodes <- .ddg.data.nodes()
  pnodes <- .ddg.proc.nodes()
  edges <- .ddg.edges()

  # Subset data node table
  dnum <- .ddg.dnum()
  dinv <- dnodes[1:dnum , c("ddg.num", "ddg.name", "ddg.value", "ddg.type", "ddg.scope")]

  # Replace scope with ENV if from initial environment
  index <- which(dnodes$ddg.from.env==TRUE)
  if (length(index) > 0) {
    dinv$ddg.scope[index] <- "ENV"
  }

  # Rename columns
  colnames(dinv) <- c("node", "name", "value", "type", "scope")

  return(dinv)
}

# .ddg.is.init is called at the beginning of all user accessible
# functions. It verifies that a DDG has been initialized. If it
# hasn't, it returns FALSE.

.ddg.is.init <- function() {
    # Short circuits evaluation.
    return(.ddg.is.set(".ddg.initialized") && .ddg.get(".ddg.initialized"))
}

# .ddg.format.time reformats time string. Input format is
# yyyy-mm-dd hh:mm:ss. Output format is (yyyy-mm-ddThh.mm.ss).

# time - input time string.

.ddg.format.time <- function(time) {
  formatted.time <- strftime(time, format="%Y-%m-%dT%H.%M.%S",usetz=TRUE)

  # The strftime call leaves a space between time and time
  # zone. We remove that here.
  return (sub(" ", "", formatted.time))
}

# .ddg.timestamp gets the current date and time from the system.

.ddg.timestamp <- function() {
  ts <- Sys.time()
  return (.ddg.format.time(ts))
}

.ddg.elapsed.time <- function(){
  time <- proc.time()
  elapsed <- time[1] +time[2] - .ddg.start.proc.time()
  # time[4] and time[5] are NA under Windows
  # elapsed <- time[1] +time[2] +time[4] +time[5]
  return(elapsed)
}

# .ddg.write.timestamp.to.history writes the current timestamp to
# the R history. The timestamp function does not work properly in
# Windows from within RStudio (the arguments are ignored).  In this
# case we create our own timestamp value and hope that the time
# does not change between when we set .ddg.history.timestamp and
# when the timestamp function inserts the timestamp in the history.

# var - the variable name under which the timestamp is saved.

.ddg.write.timestamp.to.history <- function(var=".ddg.history.timestamp") {
  if (Sys.getenv("RSTUDIO") != "" && Sys.info()['sysname'] == "Windows") {
    .ddg.set(var, paste("##------", date(), "------##"))
    timestamp(quiet=TRUE)
  }
  else {
    .ddg.set(var, timestamp(prefix = "##-ddg-- ", quiet=TRUE))
  }
}

# .ddg.is.graphic tries to decipher if the value snapshot should be
# written to file directly from the data or if it is a graphic which
# can be captured from the image device. This function, as written,
# is basically a hack. There must be a better way to implement it.

# value - input value.

.ddg.is.graphic <- function(value){
  # Matching any of these classes automatically classifies the
  # object as a graphic.
  graph.classes <- list("gg", "ggplot")
  return(is.object(value) && any(class(value) %in% graph.classes))
}

# .ddg.is.simple returns TRUE if the value passed in is a simple
# data value which should be saved locally as opposed to stored
# in a separate file. The assumption is that the value passed in
# has already been declared not to be a graphic.

# value - input value.

.ddg.is.simple <- function(value) {
  # Note that is.vector returns TRUE for lists, so we need to check
  # lists separately.  Since every value in a list can have a
  # different type, if it is a list, we will assume the value is
  # complex. We consider NULL values to be simple.
  return((!.ddg.is.graphic(value) &&
         !is.list(value) &&
         is.vector(value) &&
         length(value) == 1) ||
         is.null(value))
}

# .ddg.is.csv returns TRUE if the value passed in should be saved
# as a csv file, i.e. if it is a vector, matrix, or data frame.
# Note that is.vector returns TRUE for lists.

# value - input value.

.ddg.is.csv <- function(value) {
  return(!.ddg.is.simple(value) && ((is.vector(value) && !is.list(value)) || is.matrix(value) || is.data.frame(value)))
}


# .ddg.is.object returns TRUE if the value is determined to be an
# object by our standards.

# value - input value.

.ddg.is.object <- function(value){
  return(is.object(value) || is.environment(value))
}

# .ddg.is.function returns TRUE if the value is determined to be a
# function or we want to save it as a function.

# value - input value.

.ddg.is.function <- function(value){
  return(is.function(value))
}

# .ddg.dev.change determines whether or not a new graphic device
# has become active and whether or not we should capture the
# previous graphic device. It returns the device number we should
# capture (0 means we shouldn't capture any device).

.ddg.dev.change <- function(){
  prev.device <- .ddg.get("prev.device")
  curr.device <- dev.cur()
  device.list <- dev.list()

  # We've switched devices .
  if (prev.device != curr.device) {
    # Update device.
    .ddg.set("prev.device", curr.device)

    # Previous device still accessible.
    if (prev.device %in% device.list) return(prev.device)
  }

  # No switching, or previous is not accessible (NULL or removed).
  return(0)

}

# .ddg.save.simple takes in a simple name-value pair and saves
# it to the DDG. It does not however create any edges. Extra long
# strings are saved as snapshots.

# name - data node name.
# value - data node value.
# scope - data node scope.

.ddg.save.simple <- function(name, value, scope=NULL, from.env=FALSE) {
  #print(paste("In .ddg.save.simple: name =", name))
  #print(paste("In .ddg.save.simple: scope =", scope))
  # Save extra long strings as snapshot.
  if (is.character(value) && nchar(value) > 200) {
    #print(".ddg.save.simple: saving snapshot")
    #print(head(value))
    .ddg.snapshot.node(name, "txt", value, dscope=scope, from.env=from.env)
  } else {
    # Save the true value.
    #print(".ddg.save.simple: saving data")
    #print(paste(".ddg.save.simple: saving value", value))
    .ddg.data.node("Data", name, value, scope, from.env=from.env)
  }
}

# .ddg.write.graphic takes as input the name of a variable as well
# as its value and attempts to write it out as a graphics file. If
# all else fails, it writes out the information as a text file and
# also writes out an RData Object which can later be read back into
# the system.

# name - data node name.
# value - data node value.
# fext - file extension.
# scope - data node scope.

.ddg.write.graphic <- function(name, value=NULL, fext="jpeg", scope=NULL, from.env=FALSE){
  # Try to output graphic value.
  tryCatch({
    .ddg.snapshot.node(name, fext, NULL, dscope=scope, from.env=from.env)
  }, error = function(e) {
    # warning(paste("Attempted to write", name, "as", fext, "snapshot. Trying jpeg", ".", e))
    tryCatch({
      .ddg.snapshot.node(name, "jpeg", NULL, dscope=scope, from.env=from.env)
    }, error = function(e) {
       # warning(paste("Attempted to write", name, "as jpeg snapshot. Failed.", e, "Defaulting to saving RObject and .txt file."))
      .ddg.snapshot.node(name, "txt", value, save.object = TRUE, dscope=scope, from.env=from.env)
    })
  })
}

# .ddg.write.csv takes as input a name-value pair for a
# variable and attempts to save the data as a csv file. It does
# not create any edges but does add the node to the DDG. Edge
# creation should occur from wherever this function is called.

# name - data node name.
# value - data node value.
# scope - data node scope.

.ddg.write.csv <- function(name, value, scope=NULL, from.env=FALSE) {
  tryCatch({
    .ddg.snapshot.node(name, "csv", value, dscope=scope, from.env=from.env)
  }, error = function(e) {
    # warning(paste("Attempted to write", name, "as .csv snapshot but failed. Out as RDataObject.", e))
    .ddg.snapshot.node(name, "txt", value, save.object = TRUE, dscope=scope, from.env=from.env)
  })
}

# .ddg.save.data takes as input the name and value of a data node
# that needs to be created. It determines how the data should be
# output (or saved) and saves it in that format.

# name - name of created node.
# value - value of created node.
# from.env - if node is from initial environment
# fname (optional) - name of calling function. Used to generate
#   helpful error messages if something goes wrong.
# graphic.fext (optional) - file extension for graphic file.
# error (optional) - if TRUE, raise an R error rather than a
#   DDG error.
# scope (optional) - scope of node.
# stack (optional) - stack to use in determing scope.

.ddg.save.data <- function(name, value, fname=".ddg.save.data", graphic.fext='jpeg', error=FALSE, scope=NULL, from.env=FALSE, stack=NULL, env=NULL){
  #print (paste (".ddg.save.data: looking for name =", name, "with scope", scope))
  #print(paste(".ddg.save.data saving ", name, "with value structured as", str(value)))
  #if (is.null(value)) print(".ddg.save.data: value is null")
  if (is.null(scope)) {
    scope <- .ddg.get.scope(name, calls=stack, env=env)
  }
  
  #print (paste (".ddg.save.data: saving", name, "in scope", scope))
  # Determine type for value, and save accordingly.
  if (.ddg.is.graphic(value)) .ddg.write.graphic(name, value, graphic.fext, scope=scope, from.env=from.env)
  else if (.ddg.is.simple(value)) .ddg.save.simple(name, value, scope=scope, from.env=from.env)
  else if (.ddg.is.csv(value)) .ddg.write.csv(name, value, scope=scope, from.env=from.env)
  else if (is.list(value) || is.array(value)) .ddg.snapshot.node(name, "txt", value, save.object=TRUE, dscope=scope, from.env=from.env)
  else if (.ddg.is.object(value)) .ddg.snapshot.node(name, "txt", value, dscope=scope, from.env=from.env)
  else if (.ddg.is.function(value)) .ddg.save.simple(name, "#ddg.function", scope=scope, from.env=from.env)
  else if (error) stop("Unable to create data (snapshot) node. Non-Object value to", fname, ".")
  else {
    error.msg <- paste("Unable to create data (snapshot) node. Non-Object value to", fname, ".")
    .ddg.insert.error.message(error.msg)
  }
  #print(".ddg.save.data: Done saving data")
  invisible()
}

# .ddg.record.proc records a procedure node in the procedure node
# table.

# ptype - procedure node type.
# pname - procedure node name.
# pvalue - procedure node value.
# auto.created - TRUE means the node is being created automatically
#   when a return is found
# ptime - elapsed time
# snum - number of sourced script (main script = 0)
# pos - starting and ending lines and columns in source code (if available)

.ddg.record.proc <- function(ptype, pname, pvalue, auto.created=FALSE, ptime, snum=NA, pos=NA) {
  # Increment procedure node counter.
  .ddg.inc("ddg.pnum")
  ddg.pnum <- .ddg.pnum()

  # If the table is full, make it bigger.
  ddg.proc.nodes <- .ddg.proc.nodes()
  if (nrow(ddg.proc.nodes) < ddg.pnum) {
    size = 100
    new.rows <- data.frame(ddg.type = character(size),
        ddg.num = numeric(size),
        ddg.name = character(size),
        ddg.value = character(size),
        ddg.return.linked = logical(size),
        ddg.auto.created = logical(size),
        ddg.time = numeric(size),
        ddg.snum = numeric(size),
        ddg.startLine = numeric(size),
        ddg.startCol= numeric(size),
        ddg.endLine= numeric(size),
        ddg.endCol= numeric(size),
        stringsAsFactors=FALSE)
    .ddg.add.rows("ddg.proc.nodes", new.rows)
    ddg.proc.nodes <- .ddg.proc.nodes()
  }

  ddg.proc.nodes$ddg.type[ddg.pnum] <- ptype
  ddg.proc.nodes$ddg.num[ddg.pnum] <- ddg.pnum
  ddg.proc.nodes$ddg.name[ddg.pnum] <- pname
  ddg.proc.nodes$ddg.value[ddg.pnum] <- pvalue
  ddg.proc.nodes$ddg.auto.created[ddg.pnum] <- auto.created
  ddg.proc.nodes$ddg.time[ddg.pnum] <- ptime

  ddg.proc.nodes$ddg.snum[ddg.pnum] <- snum
  if (is.object(pos) && length(pos@startLine == 1)) {
    ddg.proc.nodes$ddg.startLine[ddg.pnum] <- pos@startLine
    ddg.proc.nodes$ddg.startCol[ddg.pnum] <- pos@startCol
    ddg.proc.nodes$ddg.endLine[ddg.pnum] <- pos@endLine
    ddg.proc.nodes$ddg.endCol[ddg.pnum] <- pos@endCol
  }
  else {
    ddg.proc.nodes$ddg.startLine[ddg.pnum] <- NA
    ddg.proc.nodes$ddg.startCol[ddg.pnum] <- NA
    ddg.proc.nodes$ddg.endLine[ddg.pnum] <- NA
    ddg.proc.nodes$ddg.endCol[ddg.pnum] <- NA
  }

  .ddg.set("ddg.proc.nodes", ddg.proc.nodes)

  # Output procedure node.
  .ddg.output.procedure.node(ptype, pname, pvalue, auto.created, ptime, snum, pos)

  if (.ddg.debug.lib()) {
    print (paste("Adding procedure node", ddg.pnum, "named", pname))
  }
}

# .ddg.record.data records a data node in the data node table.

# dtype - data node type.
# dname - data node name.
# dvalue - data node value.
# value - the value of the data
# dscope - data node scope.
# from.env - if object is from initial environment.
# dhash - the MD5 hash of original file.
# drw - whether the file was read or written.
# dtime (optional) - timestamp of original file.
# dloc (optional) -  path and name of original file.

.ddg.record.data <- function(dtype, dname, dvalue, value, dscope, from.env=FALSE, dtime="", dloc="", dhash="", drw="", dscriptpath="") {
  #print("In .ddg.record.data")
  # Increment data node counter.
  .ddg.inc("ddg.dnum")
  ddg.dnum <- .ddg.dnum()

  #Initialize dscriptpath
  if (!is.null(.ddg.get("ddg.r.script.path"))) {
    dscriptpath <- .ddg.get("ddg.r.script.path")
  }

  # If the table is full, make it bigger.
  ddg.data.nodes <- .ddg.data.nodes()
  if (nrow(ddg.data.nodes) < ddg.dnum) {
    size = 100
    new.rows <- data.frame(ddg.type = character(size),
        ddg.num = numeric(size),
        ddg.name = character(size),
        ddg.path = character(size),
        ddg.value = character(size),
        ddg.val.type = character(size),
        ddg.scope = character(size),
        ddg.from.env = logical(size),
        ddg.time = character(size),
        ddg.hash = character(size),
        ddg.rw = character(size),
        ddg.loc = character(size),
        ddg.current = logical(size), stringsAsFactors=FALSE)
    .ddg.add.rows("ddg.data.nodes", new.rows)
    ddg.data.nodes <- .ddg.data.nodes()
  }

  if (length(dvalue) > 1 || !is.atomic(dvalue)) dvalue2 <- "complex"
  else if (!is.null(dvalue)) dvalue2 <- dvalue
  else dvalue2 <- ""

  # get value type
  val.type <- .ddg.get.val.type.string(value)

  #print(".ddg.record.data: adding info")
  ddg.data.nodes$ddg.type[ddg.dnum] <- dtype
  ddg.data.nodes$ddg.num[ddg.dnum] <- ddg.dnum
  ddg.data.nodes$ddg.path[ddg.dnum] <- dscriptpath
  ddg.data.nodes$ddg.name[ddg.dnum] <- dname
  ddg.data.nodes$ddg.value[ddg.dnum] <- dvalue2
  ddg.data.nodes$ddg.val.type[ddg.dnum] <- val.type
  ddg.data.nodes$ddg.scope[ddg.dnum] <- dscope
  ddg.data.nodes$ddg.from.env[ddg.dnum] <- from.env
  ddg.data.nodes$ddg.hash[ddg.dnum] <- dhash
  ddg.data.nodes$ddg.rw[ddg.dnum] <- drw
  ddg.data.nodes$ddg.time[ddg.dnum] <- dtime
  ddg.data.nodes$ddg.loc[ddg.dnum] <- dloc

  #print(".ddg.record.data: adding file MD5 hash info")
  if (dtype == "File") {
    infiles <- .ddg.get("ddg.infilenodes")
    dhash <- md5sum(dname)
    ddg.data.nodes$ddg.hash[ddg.dnum] <- dhash
    if (length(infiles) != 0 && dname == infiles[length(infiles)]) {
      drw <- "read"
      ddg.data.nodes$ddg.rw[ddg.dnum] <- drw
      .ddg.set("ddg.infilenodes", list())
    } else {
      drw <- "write"
      ddg.data.nodes$ddg.rw[ddg.dnum] <- drw
      .ddg.set("ddg.outfilenodes", list())
    }
    longpath <- paste0(getwd(), substring(.ddg.path(),2))
    .ddg.set("ddg.hashtable", rbind(.ddg.get("ddg.hashtable"), c(dscriptpath, dloc, longpath, paste(.ddg.path(), dvalue, sep="/"), ddg.dnum, dhash, drw, dtime, dvalue2), stringsAsFactors = FALSE))
  }

  ddg.data.nodes$ddg.current[ddg.dnum] <- TRUE
  .ddg.set("ddg.data.nodes", ddg.data.nodes)

  # Output data node.
  #print(".ddg.record.data outputting data node")
  .ddg.output.data.node(dscriptpath, dtype, dname, dvalue2, val.type, dscope, from.env, dhash, drw, dtime, dloc)

  if (.ddg.debug.lib()) {
    if (dtype != "File") {
      print(paste("Adding data node", ddg.dnum, "named", dname, "with scope", dscope, " and value ", ddg.data.nodes$ddg.value[ddg.dnum]))
    } else {
      print(paste("Adding data node", ddg.dnum, "named", dname, "with scope", dscope, " and value ", ddg.data.nodes$ddg.value[ddg.dnum], " that hashes to ", dhash, " and performs a file ", drw))
    }
  }
}

# .ddg.hashtable.write writes relevant information about the ddg
# to the .ddg directory in the user's home directory. If the
# function is unable to access or create this directory, then 
# it will write to the working directory.

.ddg.hashtable.write <- function() {
  # if (interactive()) print(paste("Saving DDG in ", fileout))
  writedir <- paste0(path.expand("~"),"/.ddg/")
  if (!dir.exists(writedir)) {
    tryCatch({
      dir.create(writedir)
    },
    error = function(c){
      writedir <- paste0(getwd(),"/.ddg/")
      if (!dir.exists(writedir)) {
        dir.create(writedir)
      }
    })
  }
  writefile <- paste0(writedir,"/hashtable.csv")
  new_hashtable.csv <- .ddg.get("ddg.hashtable")
  if (nrow(new_hashtable.csv) > 0) {
    colnames(new_hashtable.csv) <- c("ScriptPath", "FilePath","DDGPath","NodePath","NodeNumber","MD5Hash","ReadWrite","Timestamp","Value")
  
    if (file.exists(writefile)) {
      old_hashtable.csv <- .ddg.hashtable.cleanup(writefile)
      new_hashtable.csv <- rbind(old_hashtable.csv,new_hashtable.csv)
    }
    write.table(new_hashtable.csv, writefile, append = FALSE, col.names = TRUE, row.names = FALSE, sep = ",")
  
  
    # JSON TABLE WORK GOES HERE
  
    writejsonfile <- paste0(writedir,"/hashtable.json")
    new_hashtable.json <- .ddg.get("ddg.hashtable")
    colnames(new_hashtable.json) <- c("ScriptPath", "FilePath","DDGPath","NodePath","NodeNumber","MD5Hash","ReadWrite","Timestamp","Value")
  
    if (file.exists(writejsonfile)) {
      old_hashtable.json <- .ddg.hashtable.json.cleanup(writejsonfile)
      new_hashtable.json <- rbind(old_hashtable.json, new_hashtable.json)
    }
    write_json(new_hashtable.json, writejsonfile)
  }
  }
  
# .ddg.hashtable.cleanup cleans the previous hashtable.csv of entries containing
# ddg data that has been overwritten. Ddg data is considered to be overwritten if
# it has an identical ddg path to the new elements being written to the file.

.ddg.hashtable.cleanup <- function(writefile) {
  # if (interactive()) print(paste("Cleaning ddg of entries with DDGPath ", .ddg.path()))
  old_hashtable.csv <- read.csv(file = writefile)
  longpath <- paste0(getwd(), substring(.ddg.path(),2))
  old_hashtable.csv <- subset(old_hashtable.csv, DDGPath != longpath)
  return(old_hashtable.csv)
}

.ddg.hashtable.json.cleanup <- function(writejsonfile) {
  old_hashtable.json <- read_json(writejsonfile, simplifyVector = TRUE)
  longpath <- paste0(getwd(), substring(.ddg.path(),2))
  old_hashtable.json <- subset(old_hashtable.json, DDGPath != longpath)
  return(old_hashtable.json)
}

# Returns a string representation of the type information of the given value.
.ddg.get.val.type.string <- function(value)
{
  val.type <- .ddg.get.val.type(value)

  if( is.null(val.type) )
  	return( 'null' )

  # list, object, environment, function, language
  if( length(val.type) == 1 )
  	return( paste('"',val.type,'"',sep="") )

  # vector, matrix, array, data frame
  # type information recorded in a list of 3 vectors (container,dimension,type)
  container <- val.type[[1]]
  dimension <- val.type[[2]]
  type <- val.type[[3]]

  # vector: a 1-dimensional array (uniform typing)
  if( identical(container,"vector") )
    return( paste('{"container":"vector", "dimension":[', dimension, '], "type":["' , type, '"]}', sep = "") )

  # matrix: a 2-dimensional array (uniform typing)
  if( identical(container,"matrix") )
  {
	dimension <- paste( dimension , collapse = "," )
	return( paste('{"container":"matrix", "dimension":[', dimension, '], "type":["' , type, '"]}', sep = "") )
  }

  # array: n-dimensional (uniform typing)
  if( identical(container,"array") )
  {
	dimension <- paste( dimension , collapse = "," )
	return( paste('{"container":"array", "dimension":[', dimension, '], "type":["' , type, '"]}', sep = "") )
  }

  # data frame: is a type of list
  dimension <- paste( dimension , collapse = "," )
  type <- paste( type , collapse = '","' )

  return( paste('{"container":"data_frame", "dimension":[', dimension, '], "type":["' , type, '"]}', sep = "") )
}


# Returns the type information of the value of the given variable.
# Does not contain information on dimensions.

.ddg.get.val.type.from.var <- function(var)
{
  val.type <- .ddg.get.val.type( get(var) )

  # remove dimension information, if any
  if( length(val.type) > 1 )
  	val.type[2] <- NULL

  return( val.type )
}


# Returns the type information of the given value,
# broken into its parts and returned in a vecctor or a list.

.ddg.get.val.type <- function(value)
{
  # vector: a 1-dimensional array (uniform typing)
  if(is.vector(value))
    return( list("vector", length(value), class(value)) )

  # matrix: a 2-dimensional array (uniform typing)
  if(is.matrix(value))
    return( list("matrix", dim(value), class(value[1])) )

  # array: n-dimensional (uniform typing)
  if(is.array(value))
  	return( list("array", dim(value), class(value[1])) )

  # data frame: is a type of list
  if(is.data.frame(value))
  {
    types <- unname(sapply(value,class))
    return( unname(list("data_frame", dim(value), types)) )
 }

  # a list
  if(is.list(value))
    return("list")

  # an object
  if(is.object(value))
    return("object")

  # envrionment, function, language
  if(is.environment(value))
    return("environment")
  if(is.function(value))
    return("function")
  if(is.language(value))
    return("language")

  # none of the above - null is a character, not NULL or NA
  return(NULL)
}


# .ddg.record.edge records a control flow edge or a data flow edge
# in the edges table.

# etype - type of edge
# node1 - name of first node
# node1 - name of second node

.ddg.record.edge <- function(etype, node1, node2) {
  # Increment edge counter.
  .ddg.inc("ddg.enum")
  ddg.enum <- .ddg.enum()

  # If the table is full, make it bigger.
  ddg.edges <- .ddg.edges()
  if (nrow(ddg.edges) < ddg.enum) {
    size = 100
    new.rows <- data.frame(ddg.num = numeric(size),
        ddg.type = character(size),
        ddg.from = character(size),
        ddg.to = character(size),
        stringsAsFactors=FALSE)
    .ddg.add.rows("ddg.edges", new.rows)
    ddg.edges <- .ddg.edges()
  }

  ddg.edges$ddg.num[ddg.enum] <- ddg.enum
  ddg.edges$ddg.type[ddg.enum] <- etype
  ddg.edges$ddg.from[ddg.enum] <- node1
  ddg.edges$ddg.to[ddg.enum] <- node2
  .ddg.set("ddg.edges", ddg.edges)

  # Output control flow or data flow edge.
  .ddg.output.edge(etype, node1, node2)

  if (.ddg.debug.lib()) {
    if (etype == "cf") etype.long <- "control flow"
    else if (etype == "df.in") etype.long <-"data flow in"
    else etype.long <- "data flow out"
    print (paste("Adding", etype.long, "edge", ddg.enum, "for", node1, "to", node2))
  }
}

# .ddg.is.proc.node returns TRUE if the specified type supports
# input and output edges in an expanded DDG. Currently this
# includes all procedure node types except Start.

# type - procedure node type.

.ddg.is.proc.node <- function(type) {
  return(type == "Operation" |
          type == "Checkpoint" |
          type == "Restore" |
          type == "Start" |
          type == "Finish" |
          type == "Binding")
}

# .ddg.proc.node.exists returns true if there is a
# procedure node with the given name

.ddg.proc.node.exists <- function(pname) {
  ddg.proc.nodes <- .ddg.proc.nodes()
  rows <- nrow(ddg.proc.nodes)
  for (i in rows:1) {
    type <- ddg.proc.nodes$ddg.type[i]
    if (.ddg.is.proc.node(type) & ddg.proc.nodes$ddg.name[i] == pname & !ddg.proc.nodes$ddg.return.linked[i] & !ddg.proc.nodes$ddg.auto.created[i]) {
      return(TRUE)
    }
  }

  return(FALSE)
}

# .ddg.proc.number gets the number of the nearest preceding
# matching Operation, Checkpoint, or Restore node. It returns
# zero if no match is found.

# pname - name of procedure node.
# find.unreturned.function - if true, only return the number if the
#    procedure has not previously been linked to a return value

.ddg.proc.number <- function(pname, find.unreturned.function=FALSE) {
  #print (paste0("Looking for function ", pname))
  ddg.proc.nodes <- .ddg.proc.nodes()
  rows <- nrow(ddg.proc.nodes)
  for (i in rows:1) {
    type <- ddg.proc.nodes$ddg.type[i]
    if (.ddg.is.proc.node(type) & ddg.proc.nodes$ddg.name[i] == pname) {
      #print (paste0("Found a matching function for ", pname))
      if (!find.unreturned.function) {
        #print (paste0("Returning ", ddg.proc.nodes$ddg.num[i]))
        return(ddg.proc.nodes$ddg.num[i])
      }

      if (find.unreturned.function & !ddg.proc.nodes$ddg.return.linked[i]) {
        #print (paste0("Returning ", ddg.proc.nodes$ddg.num[i]))
        return(ddg.proc.nodes$ddg.num[i])
      }
    }
  }

  # Error message if no match is found.
  #print ("Returning error!")
  error.msg <- paste("No procedure node found for", pname)
  if (.ddg.debug.lib()) print (sys.calls())
  .ddg.insert.error.message(error.msg)
  return(0)
}

# .ddg.last.proc.number returns the node number of the last
# procedure node in the ddg procedure node table. Procedure nodes
# are determined as defined in .ddg.is.proc.node above.

.ddg.last.proc.number <- function() {
  ddg.proc.nodes <- .ddg.proc.nodes()
  rows <- nrow(ddg.proc.nodes)
  for (i in rows:1) {
    type <- ddg.proc.nodes$ddg.type[i]
    if (.ddg.is.proc.node(type)) return(i)
  }

  error.msg <- paste("No final procedure nodes")
  .ddg.insert.error.message(error.msg)
  return(0)
}

# .ddg.data.node.exists searches the data node table for a matching
# data node and returns TRUE if a match is found. Otherwise it searches
# the initial environment table and, if a match is found, creates a
# data node and returns TRUE. Otherwise it returns FALSE.

# dname - data node name.
# dscope - data node scope.

.ddg.data.node.exists <- function(dname, dscope=NULL) {
  if (is.null(dscope)) dscope <- .ddg.get.scope(dname)

  # Search data nodes table.
  #print (paste (".ddg.data.node.exists: Looking for", dname, "in scope", dscope))
  ddg.data.nodes <- .ddg.data.nodes()
  rows <- nrow(ddg.data.nodes)
  for (i in rows:1) {
    if (ddg.data.nodes$ddg.current[i]) {
      if (ddg.data.nodes$ddg.name[i] == dname) {
        #print(paste(".ddg.data.node.exist:  found node with name", dname, "in scope", ddg.data.nodes$ddg.scope[i]))
        if (ddg.data.nodes$ddg.scope[i] == "ddg.library" || ddg.data.nodes$ddg.scope[i] == dscope) {
          #print(".ddg.data.node.exists found")
          return (TRUE)
        }
      }
    }
  }

  # Search initial environment table.
  if (dscope == "R_GlobalEnv") {
    #print("Searching global environment")
    if (exists(dname, globalenv())) {
      dvalue <- get(dname, envir = globalenv())
      if (!is.function(dvalue)) {
        .ddg.save.data(dname, dvalue, scope=dscope, from.env=TRUE)
        return (TRUE)
      }
    }

  }

  #print(".ddg.data.node.exists NOT found")
  return(FALSE)
}

# .ddg.data.number retrieves the number of the nearest preceding
# current matching data node. It returns zero if no match is found.

# dname - data node name.
# dscope (optional) - data node scope.

.ddg.data.number <- function(dname, dscope=NULL) {
  if (is.null(dscope)) dscope <- .ddg.get.scope(dname)
  ddg.data.nodes <- .ddg.data.nodes()
  rows <- nrow(ddg.data.nodes)
  for (i in rows:1) {
    if (ddg.data.nodes$ddg.current[i]) {
      if (ddg.data.nodes$ddg.name[i] == dname) {
        if (ddg.data.nodes$ddg.scope[i] == "ddg.library" || ddg.data.nodes$ddg.scope[i] == dscope) return (ddg.data.nodes$ddg.num[i])
      }
    }
  }

  # Error message if no match found.
  error.msg <- paste("No data node found for", dname)
  .ddg.insert.error.message(error.msg)
  return(0)
}

# .ddg.proc.name returns the name of a procedure node. It returns a
# empty string if no match is found.

# pnum - node number in procedure node table.

.ddg.proc.name <- function(pnum) {
  if (pnum < 1 || pnum > .ddg.pnum()) {
    error.msg <- paste("No name found for procedure number", pnum)
    .ddg.insert.error.message(error.msg)
    return ("")
  }

  return(.ddg.proc.nodes()$ddg.name[pnum])
}

# .ddg.proc2proc creates a control flow edge from the preceding
# procedure node to the current procedure node.

.ddg.proc2proc <- function() {
  ddg.pnum <- .ddg.pnum()

  if (ddg.pnum > 1) {
    # Record in edges table
    etype <- "cf"
    node1 <- paste("p", ddg.pnum-1, sep="")
    node2 <- paste("p", ddg.pnum, sep="")
    .ddg.record.edge(etype, node1, node2)

    if (.ddg.debug.lib()) {
      pname1 <- .ddg.proc.name(ddg.pnum-1)
      pname2 <- .ddg.proc.name(ddg.pnum)
      print(paste("proc2proc: ", pname1, " ", pname2))
      print(paste("CF ", node1, " ", node2, sep=""))
    }
  }

  invisible()
}

# .ddg.data2proc creates a data flow edge from a data node to a
# procedure node.

# dname - data node name.
# dscope - data node scope.
# pname - procedure node name.

.ddg.data2proc <- function(dname, dscope, pname) {
  # Get data & procedure numbers.
  dn <- .ddg.data.number(dname, dscope)
  pn <- .ddg.proc.number(pname)

  # Record in edges table
  etype <- "df.in"
  node1 <- paste("d", dn, sep="")
  node2 <- paste("p", pn, sep="")
  .ddg.record.edge(etype, node1, node2)
  
  if (.ddg.debug.lib()) {
    print(paste("data2proc: ", dname, " ", pname, sep=""))
    print(paste("DF ", node1, " ", node2, sep=""))
  }
  invisible()
}

# .ddg.proc2data creates a data flow edge from a procedure node to
# a data node.

# pname - procedure node name.
# dname - data node name.
# dscope (optional) - data node scope.
# return.value (optional) - if true it means we are linking to a return
# value. In this case, we need to be sure that there is not already a
# return value linked.  This is necessary to manage recursive functions
# correctly.

.ddg.proc2data <- function(pname, dname, dscope=NULL, return.value=FALSE) {
  # Get data & procedure numbers.
  #print (paste(".ddg.proc2data: Looking for", dname, "in scope", dscope))
  dn <- .ddg.data.number(dname, dscope)
  
  #print (paste(".ddg.proc2data: Found node", dn))
  
  # attach data node to the last procedure node if pname is NULL.
  if(is.null(pname) || startsWith(pname,".ddg.") || startsWith(pname,"ddg"))
    pn <- .ddg.last.proc.number()
  else
    pn <- .ddg.proc.number(pname, return.value)

  # Create data flow edge from procedure node to data node.
  if (dn != 0 && pn != 0) {

    # Record in edges table
    etype <- "df.out"
    node1 <- paste("p", pn, sep="")
    node2 <- paste("d", dn, sep="")
    .ddg.record.edge(etype, node1, node2)

    # Record that the function is linked to a return value.  This
    # is necessary for recursive functions to get linked to their
    # return values correctly.
    if (return.value) {
      ddg.proc.nodes <- .ddg.proc.nodes()
      #print ("Marking return value as being used")
      ddg.proc.nodes$ddg.return.linked[pn] <- TRUE
      .ddg.set("ddg.proc.nodes", ddg.proc.nodes)
    }

    if (.ddg.debug.lib()) {
      print(paste("proc2data: ", pname, " ", dname, sep=""))
      print(paste("DF ", node1, " ", node2, sep=""))
    }
  }

  invisible()
}

# .ddg.lastproc2data creates a data flow edge from the last
# procedure node to a data node.

# dname - data node name.
# all (optional) - whether all nodes should be considered (TRUE)
#   or only procedure nodes (FALSE).
# dscope - the scope in which dname should be looked up

.ddg.lastproc2data <- function(dname, all=TRUE, dscope=NULL) {
  # Get data & procedure numbers.
  dn <- .ddg.data.number(dname, dscope)
  pn <- if(all) .ddg.pnum() else .ddg.last.proc.number()

  # Record in edges table
  etype <- "df.out"
  node1 <- paste("p", pn, sep="")
  node2 <- paste("d", dn, sep="")
  .ddg.record.edge(etype, node1, node2)

  if (.ddg.debug.lib()) {
    print(paste("lastproc2data:", dname))
    print(paste("DF ", node1, " ", node2, sep=""))
  }
}

# .ddg.is.nonlocal.assign returns TRUE if the object passed is an
# expression object containing a non-local assignment.

# expr - input expression.

.ddg.is.nonlocal.assign <- function (expr)
{
  # <<- or ->> means that the assignment is non-local
  if (is.call(expr))
  {
    # This also finds uses of ->>.
    if (identical(expr[[1]], as.name("<<-")))
      return (TRUE)
  }
  return (FALSE)
}


# .ddg.create.empty.vars.set creates an empty data frame
# initialized to contain information about variable assignments.
# The difference between first.writer and possible.first.writer is
# that first.writer is for simple assignments (like a <- 1), while
# possible.first.writer is for situations where the assignment might
# not have occurred, like "if (foo) a <- 1".

# The data frame is structured as follows
# - the variable name.
# - the position of the statement that wrote the variable first.
# - the position of the statement that wrote the variable last.
# - the position of the first statement that may have assigned to a
#   variable .
# - the position of the last statement that may have assigned to a
#   variable.

# var.table.size - desired size of the data frame. Negative values
#   and 0 are coerced to 1.

.ddg.create.empty.vars.set <- function(var.table.size=1) {

  if (var.table.size <= 0) var.table.size <- 1

  vars.set <- data.frame(variable=character(var.table.size),
      first.writer=numeric(var.table.size),
      last.writer=numeric(var.table.size),
      possible.first.writer=numeric(var.table.size),
      possible.last.writer=numeric(var.table.size),
      stringsAsFactors=FALSE)

  # Initialize first writer.
  vars.set$first.writer <- var.table.size + 1
  vars.set$possible.first.writer <- var.table.size + 1

  #print(".ddg.create.empty.vars.set returning")
  #print(vars.set)
  return(vars.set)
}

#.ddg.increase.vars.set simply doubles the size of a variable
# assignment data frame and returns the new one.

# vars.set - data frame containing variable assignments.
# size (optional) - number of rows in data frame.

.ddg.double.vars.set <- function(vars.set, size=nrow(vars.set)) {
  # Create the right size data frame from input frame.
  new.vars.set <- rbind(vars.set,.ddg.create.empty.vars.set(size))

  # Update first/last writer.
  new.vars.set$first.writer <- ifelse(new.vars.set$first.writer == size + 1, size*2 + 1, new.vars.set$first.writer)
  new.vars.set$possible.first.writer <- ifelse(new.vars.set$possible.first.writer == size + 1, size*2 + 1, new.vars.set$possible.first.writer)

  return(new.vars.set)
}

# .ddg.add.to.vars.set parses a command and adds the new variable
# information to the variable assignment data frame. Note that
# var.num is a global variable! It should be intialized when
# vars.set is first created.

# vars.set - variable assignment data frame.
# cmd.expr - command expression.
# i - position of variable in data frame.

.ddg.add.to.vars.set <- function(vars.set, cmd, i) {
  #print("In .ddg.add.to.vars.set")

  # Find out the variable being assigned to by a simple assignment
  # statement.
  main.var.assigned <- cmd@vars.set

  # Find all the variables that may be assigned in the statement.
  vars.assigned <- cmd@vars.possibly.set

  for (var in vars.assigned) {
    nRow <- which(vars.set$variable == var)

    # If the variable is already in the table, update its entry.
    if (length(nRow) > 0) {
      if (!is.null(main.var.assigned) && var == main.var.assigned) {
        vars.set$last.writer[nRow] <- i
      }
      else {
        vars.set$possible.last.writer[nRow] <- i
      }
    }

    # The variable was not in the table. Add a new line for this
    # variable.
    else {
      # Find the first empty row
      empty.rows <- which(vars.set$variable == "")
      if (length(empty.rows) == 0) {
        vars.set <- .ddg.double.vars.set(vars.set,nrow(vars.set))
        empty.rows <- which(vars.set$variable == "")
      }
      var.num <- empty.rows[1]

      # Set the variable.
      vars.set$variable[var.num] <- var
      if (!is.null(main.var.assigned) && var == main.var.assigned) {
        vars.set$first.writer[var.num] <- i
        vars.set$last.writer[var.num] <- i
      }
      else {
        vars.set$possible.first.writer[var.num] <- i
        vars.set$possible.last.writer[var.num] <- i
      }
    }
  }

  #print (".ddg.add.to.vars.set: returning")
  #print(vars.set)
  return(vars.set)
}


## .ddg.find.var.assigments finds the possible variable assignments
## for a fixed set of parsed commands. See .ddg.create.empty.vars.set
## for more information on the structure of the returned data frame.
#
## parsed.commands - a list of parsed commands.
#
.ddg.find.var.assignments <- function(cmds) {
  #print("In .ddg.find.var.assignments")
  if (length(cmds) == 0) return (data.frame())

  # Make it big so we don't run out of space.
  var.table.size <- length(cmds)
  vars.set <- .ddg.create.empty.vars.set(var.table.size)

  # Build the table recording where variables are assigned to or may
  # be assigned to.
  for ( i in 1:length(cmds)) {
    cmd.expr <- cmds[[i]]
    #print(paste("Looking for var assignments in", cmd.expr@abbrev))
    vars.set <- .ddg.add.to.vars.set(vars.set,cmd.expr, i)
  }
  return (vars.set)
}


# .ddg.auto.graphic.node attempts to figure out if a new graphics
# device has been created and take a snapshot of a previously active
# device, setting the snapshot node to be the output of the
# specified command.

# cmd.abbrev (optional) - name of procedure node.
# dev.to.capture (optional) - function specifying which device
#   should be captured, where zero indicates no device and
#   negative values are ignored.

.ddg.auto.graphic.node <- function(cmd.abbrev=NULL, dev.to.capture=.ddg.dev.change) {

  num.dev.to.capture <- dev.to.capture()
  if (num.dev.to.capture > 1) {
    # Make the capture device active (store info on previous
    # device).
    prev.device <- dev.cur()
    dev.set(num.dev.to.capture)

    # Capture it as a jpeg.
    name <- if (!is.null(cmd.abbrev) && cmd.abbrev != "") paste0("graphic", substr(cmd.abbrev,1,10)) else "graphic"
    .ddg.snapshot.node(name, fext="jpeg", data=NULL)

    # Make the previous device active again.
    dev.set(prev.device)

    # We're done, so create the edge.
    if(is.null(cmd.abbrev)) .ddg.lastproc2data(name, all=FALSE)
    else .ddg.proc2data(cmd.abbrev, name)
  }
}


# .ddg.create.data.use.edges.for.console.cmd creates a data flow
# edge from the node for each variable used in cmd.expr to the
# procedural node labeled cmd, as long as the value would either
# be one that exists prior to starting the console block, or
# corresponds to the last setting of this variable in the console
# block.

# vars.set - variable assignment data frame.
# cmd - name of procedure node.
# cmd.expr - command expression.
# cmd.pos - position of command.

.ddg.create.data.use.edges.for.console.cmd <- function (vars.set, cmd, cmd.pos, for.caller) {
  # Find all the variables used in this command.
  #print (paste(".ddg.create.data.use.edges.for.console.cmd: cmd = ", cmd@text))
  vars.used <- cmd@vars.used

  for (var in vars.used) {
    #print(paste(".ddg.create.data.use.edges.for.console.cmd: var =", var))
    # Make sure there is a node we could connect to.
    scope <- .ddg.get.scope(var, for.caller)

    #print(paste(".ddg.create.data.use.edges.for.console.cmd: scope =", scope))

    if (.ddg.data.node.exists(var, scope)) {
      # print(".ddg.create.data.use.edges.for.console.cmd found data node")
      nRow <- which(vars.set$variable == var)

      # Check if the node is written in the console block.
      if (length(nRow) > 0) {
        first.writer <- min(vars.set$first.writer[nRow], vars.set$possible.first.writer[nRow])
        last.writer <- max(vars.set$last.writer[nRow], vars.set$possible.last.writer[nRow])
        
        # Draw the edge if we will connect to a node that exists
        # before the console block or to the last writer of this
        # variable within the console block.

        if (cmd.pos <= first.writer || cmd.pos >= last.writer) {
        # Note: the following line leads to the self-referencing
        # node problem.
        # if (cmd.pos <= first.writer || cmd.pos > last.writer) {
          .ddg.data2proc(var, scope, cmd@abbrev)
        }

        # TODO - add some sort of warning to the user that the node
        # is not being created
      }

      # The variable is not set at all in this console block.
      # Connect to a pre-existing data node.
      else {
        .ddg.data2proc(var, scope, cmd@abbrev)
      }
    }
    else {
      # TODO - add some sort of warning that the data node was NOT
      # found.

      # error.msg <- paste("Unable to find data node for",var, ". Command", parse(text=cmd.expr), "appears to use it for procedure node", cmd, ".")
      # .ddg.insert.error.message(error.msg)
    }
  }
  #print (".ddg.create.data.use.edges.for.console.cmd Done")
}


# .ddg.create.data.set.edges.for.cmd creates edges that correspond
# to a console command assigning to a variable.

# vars.set - variable assignment data frame.
# cmd.abbrev - name of procedure node.
# cmd.expr - command expression.
# cmd.pos - position of command.
# env - environment to use for evaluating variable.
# for.finish.node (optional) - if TRUE, data edge is for finish
#   node.
# scope (optional) - scope of variable.
# stack (optional) - stack to use for evaluating variable.

.ddg.create.data.set.edges.for.cmd <- function(vars.set, cmd, cmd.pos, env, for.finish.node = FALSE, scope=NULL, stack=NULL) {
  # print(paste("In .ddg.create.data.set.edges.for.cmd: cmd = ", cmd@abbrev))
  #print(paste(".ddg.create.data.set.edges.for.cmd: env =", environmentName(env)))
  vars.assigned <- cmd@vars.set

  # print(paste("In .ddg.create.data.set.edges.for.cmd: vars.assigned = ", vars.assigned))
  # print("In .ddg.create.data.set.edges.for.cmd: vars.set = ")
  # print(vars.set)

  for (var in vars.assigned) {

    # print(paste(".ddg.create.data.set.edges.for.cmd: var = ", var))
    whichRows <- which(vars.set$variable == var)

    # Only create a node edge for the last place that a variable is
    # set within a console block.
    if ((length(whichRows) > 0 && vars.set$last.writer[whichRows] == cmd.pos && vars.set$possible.last.writer[whichRows] <= vars.set$last.writer[whichRows]) || for.finish.node) {
        if (is.null(env)) {
          env <- .ddg.get.env(var, calls=stack)
        }
        scope <- .ddg.get.scope(var, calls=stack, env=env)

        # Special operators are defined by enclosing the name in `.  However,
        # the R parser drops those characters when we deparse, so when we parse
        # here they are missing and we get an error about unexpected SPECIAL
        # characters.  The first tryCatch, puts the ` back in and parses again.
        # The second tryCatch handles errors associated with evaluated the variable.
        parsed <- tryCatch(parse(text=var),
            error = function(e) parse(text=paste("`",var,"`",sep="")))
        val <- tryCatch(eval(parsed, env),
          error = function(e) {
            eval (parse(text=var), parent.env(env))
          }
        )

        tryCatch(.ddg.save.data(var, val, fname=".ddg.create.data.set.edges.for.cmd", error=TRUE, scope=scope, stack=stack, env=env),
               error = function(e){.ddg.data.node("Data", var, "complex", scope)})

        .ddg.proc2data(cmd@abbrev, var, scope)
    }
  }

}


# .ddg.create.data.node.for.possible.writes creates a data node for
# each variable that might have been set in something other than a
# simple assignment.  An edge is created from the last node in the
# console block.

# vars.set - variable assignment data frame.
# last.command - last command in console block.

.ddg.create.data.node.for.possible.writes <- function (vars.set, last.command, env= NULL) {
  #print("In .ddg.create.data.node.for.possible.writes")
  environment <- if (is.environment(env)) env else .GlobalEnv

  for (i in 1:nrow(vars.set)) {
    # print(paste("Checking ", vars.set$variable[i]))
    if (vars.set$possible.last.writer[i] > vars.set$last.writer[i]) {
      value <- tryCatch(eval(parse(text=vars.set$variable[i]), environment),
          error = function(e) {
            #print(paste("Could not find value for", vars.set$variable[i], "in environment", environment))
            NULL
          }
      )

      # Only create the node and edge if we were successful in
      # looking up the value.
      if (!is.null(value)) {
        envName <- environmentName(environment)
        if (envName == "") envName <- .ddg.get.scope(vars.set$variable[i])
        .ddg.data.node("Data", vars.set$variable[i], value, envName)
        .ddg.proc2data(last.command@abbrev, vars.set$variable[i], envName)
      }
    }
  }
  #print("Done with .ddg.create.data.node.for.possible.writes")

}


# Given a parse tree, this function returns a list containing
# the expressions that correspond to the filename argument
# of the calls to functions that read or write the files.  If there are
# none, it returns NULL.
#
# main.object - the parsed expression to search through
# func.df - the data frame describing the functions with file arguments
.ddg.find.files <- function(main.object, func.df, env=NULL) {
  environment <- if (is.environment(env)) env else .GlobalEnv

  # Recursive helper function.
  find.files.rec <- function(obj) {
    #print (obj)
    # Base cases.
    if (!is.recursive(obj)) {
      return(NULL)
    }

    if (length(obj) == 0) {
      return(NULL)
    }
    ## It might be useful to record somehow that this function
    # reads a file, but we wouldn't actually do the reading
    # until the function is called, not here where it is
    # being declared.
    if (.ddg.is.functiondecl(obj)) return(NULL)

    if (is.call(obj)) {

      # Call has no arguments, so it can't be reading a function.  Recurse
      # on the first part, in case it is more than just a symbol.
      if (length (obj) == 1) return (find.files.rec (obj[[1]]))

      # Call with arguments
      else if (is.symbol (obj[[1]])) {
        # Is this is file reading function?
        read.func.pos <- match (as.character(obj[[1]]), func.df$function.names)
        if (!is.na (read.func.pos)) {
          # Find the file argument.
          arg.name <- func.df$param.names[read.func.pos]
          # print (paste(".ddg.find.files: arg.name = ", arg.name))

          # Find a matching parameter passed by name
          file.name.arg.matches <- unlist(lapply (names(obj), function (arg) {return (pmatch (arg, arg.name))}))
          match.pos <- match (1, file.name.arg.matches)
          #print (paste(".ddg.find.files: match.pos = ", match.pos))


          # If no argument qualified by the file parameter name, use the argument in the
          # expected position
          if (is.na (match.pos)) {
            file.name <- tryCatch (eval(obj[[func.df$param.pos[read.func.pos]+1]], environment),
                error = function (e) NULL)
          }
          else {
            #print (paste(".ddg.find.files: obj[[match.pos]] = ", obj[[match.pos]]))
            file.name <- tryCatch (eval(obj[[match.pos]], environment),
                error = function (e) NULL)
          }

          # Recurse over the arguments to the function.  We can't just skip over the 2nd
          # element since the filename parameter is not necessarily there if it was passed
          # by name.
          funcs <- find.files.rec (obj[2:length(obj)])

          # Add this file name to the list of files being read.
          # Make sure the file name could be evaluated and that it results in
          # a name, not a connection.
          if (!is.null(file.name) && is.character(file.name)) {
            unique (c (file.name, funcs))
          }
        }

        # Not a file reading function.  Recurse over the arguments.
        else {
          find.files.rec (obj[2:length(obj)])
        }
      }

      # Function call, but the first list element is not simply a function name.  Recurse
      # over all the list elements.
      else {
        unique (append (find.files.rec (obj[[1]]), find.files.rec (obj[2:length(obj)])))
      }
    }

    # A recursive structure that is not a call.  Not sure if there are any, but just in case...
    else if (length(obj) == 1) {
      unique (find.files.rec (obj[[1]]))
    }
    else {
      unique (append (find.files.rec (obj[[1]]), find.files.rec (obj[2:length(obj)])))
    }
  }

  return(find.files.rec(main.object@parsed))
}


# Initialize the information about functions that read from files
.ddg.create.file.read.functions.df <- function () {
  # Functions that read files
  function.names <-
    c ("source", "read.csv", "read.csv2", "read.delim", "read.delim2", "read.table", "read.xls", "file", "readLines")

  # The argument that represents the file name
  param.names <-
    c ("file", "file", "file", "file", "file", "file", "xls", "description", "con")

  # Position of the file parameter if it is passed by position
  param.pos <-
    c (1, 1, 1, 1, 1, 1, 1, 1, 1)

  return (data.frame (function.names, param.names, param.pos, stringsAsFactors=FALSE))
}

.ddg.set (".ddg.file.read.functions.df", .ddg.create.file.read.functions.df ())

# Given a parse tree, this function returns a list containing
# the expressions that correspond to the filename argument
# of the calls to functions that read from files.  If there are
# none, it returns NULL.
.ddg.find.files.read <- function(main.object, env) {
  return (.ddg.find.files (main.object, .ddg.get(".ddg.file.read.functions.df"), env))
}

# Creates file nodes and data in edges for any files that are read in this cmd
# cmd - text command
# cmd.expr - parsed command
.ddg.create.file.read.nodes.and.edges <- function (cmd, env) {
  #print("In .ddg.create.file.read.nodes.and.edges")
  # Find all the files potentially read in this command.
  # This may include files that are not actually read if the
  # read are within an if-statement, for example.
  files.read <- .ddg.find.files.read(cmd, env)
  # Adds the files read to ddg.infilenodes for use in determining reads
  # and writes in the hashtable.
  .ddg.set("ddg.infilenodes", c(.ddg.get("ddg.infilenodes"), files.read))
  #print (".ddg.create.file.read.nodes.and.edges: Files read:")
  # print (files.read)

  for (file in files.read) {

    # Only create the node and edge if there actually is a file
    # Even if the file exists, it is possible that it was not read here
    if (file.exists(file))
    {
      # Create the file node and edge
      ddg.file(file)
      ddg.data.in(basename(file), pname=cmd@abbrev)
    }
    else if (grepl ("^http", file) || grepl ("^ftp", file))
    {
      scope <- environmentName(.GlobalEnv)
      .ddg.data.node("URL", file, file, scope)
      .ddg.data2proc(file, scope, cmd@abbrev)
    }
  }
}

# Initialize the information about functions that read from files
.ddg.create.file.write.functions.df <- function () {
  # Functions that read files
  function.names <-
    c ("write.csv", "write.csv2", "write.table", "ggsave")

  # The argument that represents the file name
  param.names <-
    c ("file", "file", "file", "filename")

  # Position of the file parameter if it is passed by position
  param.pos <-
    c (2, 2, 2, 1)

  return (data.frame (function.names, param.names, param.pos, stringsAsFactors=FALSE))
}

.ddg.set (".ddg.file.write.functions.df", .ddg.create.file.write.functions.df ())

# Given a parse tree, this function returns a list containing
# the expressions that correspond to the filename argument
# of the calls to functions that write files.  If there are
# none, it returns NULL.
.ddg.find.files.written <- function(main.object, env) {
  return (.ddg.find.files (main.object, .ddg.get(".ddg.file.write.functions.df"), env))
}

# Creates file nodes and data in edges for any files that are written in this cmd
# cmd - text command
# cmd.expr - parsed command
.ddg.create.file.write.nodes.and.edges <- function (cmd, env) {
  # Find all the files potentially written in this command.
  # This may include files that are not actually written if the
  # write calls are within an if-statement, for example.
  files.written <- .ddg.find.files.written(cmd, env)
  # Adds the files written to ddg.outfilenodes for use in determining reads
  # and writes in the hashtable.
  .ddg.set("ddg.outfilenodes", c(.ddg.get("ddg.outfilenodes"), files.written))
  #print (".ddg.create.file.read.nodes.and.edges: Files written:")
  #print (files.written)

  for (file in files.written) {
    # Check that the file exists.  If it does, we will assume that
    # it was created by the write call that we just found.
    if (file.exists (file)) {
      # Create the file node and edge
      ddg.file.out (file, pname=cmd@abbrev)
    }
  }
}

# Initialize the information about functions that initialize graphics devices
.ddg.create.graphics.functions.df <- function () {
  # Functions that read files
  function.names <-
    c ("pdf", "postscript", "bmp", "jpeg", "png", "tiff", "X11")

  # The argument that represents the file name
  param.names <-
    c ("file", "file", "filename", "filename", "filename", "filename", NA)

  # Position of the file parameter if it is passed by position
  param.pos <-
    c (1, 1, 1, 1, 1, 1, NA)

  return (data.frame (function.names, param.names, param.pos, stringsAsFactors=FALSE))
}

.ddg.set (".ddg.graphics.functions.df", .ddg.create.graphics.functions.df ())

# Initialize the information about functions that update graphics
.ddg.set(".ddg.graphics.update.functions", ls(which(search()=="package:graphics")))
#print(paste("graphics update functions:", .ddg.get(".ddg.graphics.update.functions")))


# Given a parse tree, this function returns a list containing
# the expressions that correspond to the filename argument
# of the calls to functions that create graphics devices.  If there are
# none, it returns NULL.
.ddg.set.graphics.files <- function(main.object, env) {
  tryCatch(
    # Allows dev.print to work when we want to save the plot.
    dev.control("enable"),
    error = function (e) return()
  )
  #print(paste(".ddg.set.graphics.files: main.object =", main.object@text))
  
  # Add the newly-opened graphics device to the list of open devices
  .ddg.set("ddg.open.devices", union(.ddg.get("ddg.open.devices"), dev.cur()))
  

  # Find all the graphics files that have potentially been opened.
  # Remember these file names until we find the dev.off call and then
  # determine which was written.
  new.possible.graphics.files.open <- .ddg.find.files (main.object, .ddg.get(".ddg.graphics.functions.df"), env)
  if (!is.null(new.possible.graphics.files.open)) {
    #print(paste(".ddg.set.grpahics.files: opened", new.possible.graphics.files.open))
    if (!is.null(.ddg.get ("possible.graphics.files.open"))) {
      possible.graphics.files.open <- .ddg.get ("possible.graphics.files.open")
      .ddg.set ("possible.graphics.files.open",
                c (new.possible.graphics.files.open, possible.graphics.files.open))
    }

    else {
      .ddg.set ("possible.graphics.files.open", new.possible.graphics.files.open)
    }
  
    #print (paste (".ddg.set.graphics.files: Found ", new.possible.graphics.files.open))

  }
  
  #print(paste(".ddg.set.graphics.files: dev.cur =", dev.cur()))
  dev.node.name <- paste0("dev.", dev.cur())
  .ddg.data.node("Data", dev.node.name, "graph", NULL)
  .ddg.proc2data(main.object@abbrev, dev.node.name)
  
}

# Add data in and data out nodes that represent the current device.
#
# cmd - Assumed to be a function that modifies the graphics device,
# such as a function in the base graphics package.

.ddg.add.graphics.io <- function (cmd) {
  # Try adding the input edge.  It is not a problem if the node 
  # can't be found.  It means that the output is going to the
  # RStudio window, not a file, so there has been no call like pdf
  # or jpg that would have created the data node.
  dev.node.name <- paste0("dev.", dev.cur())
  if (dev.cur() %in% .ddg.get("ddg.open.devices")) {
    .ddg.data2proc(dev.node.name, NULL, cmd@abbrev)
  }
  else {
    # Add the newly-opened graphics device to the list of open devices
    .ddg.set("ddg.open.devices", union(.ddg.get("ddg.open.devices"), dev.cur()))    
  }
  
  # Add an output node with the same name
  .ddg.data.node("Data", dev.node.name, "graph", NULL)
  .ddg.proc2data(cmd@abbrev, dev.node.name)
  
}

.ddg.capture.graphics <- function(cmd, called.from.save = FALSE) {
  
  proc.node.name <- 
      if (is.null(cmd)) NULL
      else if (is.character(cmd)) cmd
      else cmd@abbrev
  
  dev.number <- .ddg.get(".ddg.dev.number")
  .ddg.set("ddg.open.devices", setdiff(.ddg.get("ddg.open.devices"), dev.number))
  
  #print(paste(".ddg.capture.graphics: ", proc.node.name))
  if (!is.null(.ddg.get ("possible.graphics.files.open")) && !is.null(proc.node.name)) {
    possible.graphics.files.open <- .ddg.get ("possible.graphics.files.open")

    # Find the most recent file
    #print(paste(".ddg.capture.graphics: possible.graphics.files.open =", possible.graphics.files.open))
    #print(".ddg.capture.graphics: getting file info")
    graphics.file.info <- file.info(possible.graphics.files.open)
    #print(".ddg.capture.graphics: getting modification time")
    latest.file.date.row <- which.max (graphics.file.info$mtime)
      
    # Check if the device is still open and close it if it is
    # We need to do this so that the file.out call can
    # copy the file.
    if (dev.number %in% dev.list()) dev.off(dev.number)
    
    #print(".ddg.capture.graphics: creating file node")
    
    if (!is.null(proc.node.name)) {
      ddg.file.out (possible.graphics.files.open[latest.file.date.row], pname=proc.node.name)
  
      # Add an input edge from the current device
      dev.node.name <- paste0("dev.", dev.number)
      #print(paste(".ddg.capture.current.graphics: dev.node.name =", dev.node.name))
      #print(".ddg.capture.graphics: creating in edge")
      
      # If the device was opened but never written to there will be no node.
      if (.ddg.data.node.exists (dev.node.name)) {
       .ddg.data2proc(dev.node.name, NULL, proc.node.name)
      }
      #print(".ddg.capture.graphics: done creating in edge")
        
      #.ddg.capture.current.graphics(cmd, possible.graphics.files.open[latest.file.date.row])
      #print(paste(".ddg.capture.graphics: writing to ", possible.graphics.files.open[latest.file.date.row]))
      .ddg.set ("possible.graphics.files.open", NULL)
    }
    return(possible.graphics.files.open[latest.file.date.row])
  }

  # Output is going to the display, so we need to make up a name
  dev.file <- .ddg.capture.current.graphics(proc.node.name)
  
  if (called.from.save) {
    #print(paste(".ddg.capture.graphics: dev.file =", dev.file))
    #print(paste(".ddg.capture.graphics: proc.node.name =", proc.node.name))
    ddg.file.out (dev.file, pname=proc.node.name)
    #print(paste(".ddg.capture.graphics: returned from ddg.file.out"))
  
    # Remove the temporary file
    file.remove(dev.file)
    
    # Add an input edge from the current device
    dev.node.name <- paste0("dev.", dev.cur())
    #print(paste(".ddg.capture.current.graphics: dev.node.name =", dev.node.name))
    # If the device was opened but never written to there will be no node.
    if (.ddg.data.node.exists (dev.node.name)) {
      .ddg.data2proc(dev.node.name, NULL, proc.node.name)
    }
  }
  
  return (dev.file)

}


# Captures what is on the current display to a file, creates a file node
# and connects to the ddg.
.ddg.capture.current.graphics <- function(proc.node.name, file=NULL) {
  if (is.null(file)) {
    file <- paste0("dev.off.", .ddg.dnum()+1, ".pdf")
  }
  #print(paste(".ddg.capture.graphics: writing to ", file))
  
  # Save the graphic to a file temporarily
  #print(sys.calls())
  dev.print(device=pdf, file=file)
  .ddg.set ("possible.graphics.files.open", file)
  return(file)
  

}

# .ddg.loadhistory takes in the name of a history file, opens it,
# scans it for the last occurrence of the string specified by
# timestamp, and returns the lines from that point forward.

# hist.file - name of history file.
# timestamp - timestamp string.

.ddg.loadhistory <- function(hist.file, timestamp) {
  # Read from specified file.
  history <- readLines(hist.file)
  history.lines <- length(history)

  # Find the timestamp specified in the history.  There may be
  # more than one with the same timestamp, so pick the last of
  # these.
  history.timestamp.line <- tail(which(history == timestamp), 1)

  if (length(history.timestamp.line) == 0) {
    error.msg <- paste("Part of history is missing. DDG may be incomplete! Tried reading from",
                       hist.file, "but could not find timestamp:", timestamp)

    .ddg.insert.error.message(error.msg)
    history.timestamp.line <- 0
  }

  # Need to check if the timestamp line is the last line in the file
  # explicitly.  If we don't do that and take the vector, we will
  # get the last line in the file since R will create a descending
  # sequence for the vector.
  if (history.timestamp.line == history.lines) return (vector())

  # NEED the paren around sum.
  return(history[(history.timestamp.line+1):history.lines])
}

# .ddg.savehistory saves the current and unsaved R command history
# to the specified file if that file matches the DDG history file.
# Note: the commented section of code appends the information to
# this file.

# savehistory is not supported on all R platforms.  If it is not supported,
# this will fail silently.

# hist.file - name of history file.

.ddg.savehistory <- function(hist.file) {

  # USED TO STORE ENTIRE HISTORY IN SEP. FILE.
  # Write history out to temporary file

  # ddg.grab.timestamp <- .ddg.get(".ddg.grab.timestamp.history")
  # ddg.tmp.history.file <- paste(hist.file,".tmp", sep="")

  if (.ddg.is.set(".ddg.history.file") &&
      is.character(.ddg.get(".ddg.history.file")) &&
      .ddg.get(".ddg.history.file") == hist.file) {
      savehistory(hist.file)
  }

  # USED TO STORE ENTIRE HISTORY IN SEP. FILE.
  # Read in changes and writ eout to extended file.

  # newlines <- .ddg.loadhistory(ddg.tmp.history.file,ddg.grab.timestamp)
  # write(newlines, file=hist.file, append=TRUE)
  # insert timestamp to history
  # .ddg.write.timestamp.to.history(var=".ddg.grab.timestamp.history")
}


# .ddg.link.function.returns determines if the command calls a
# function for which ddg.return has created a node for the return
# value.  If so, a data flow edge is created from the return value
# data node to the finish node for the command.  Note that if the
# assignment is an expression, like "d <- f(a) + f(b)", there may
# be multiple return value nodes to link to.

# command - input command.

.ddg.link.function.returns <- function(command) {
  # Find the functions that have completed but whose returns have
  # not been used yet.
  returns <- .ddg.get(".ddg.return.values")
  if (!is.na(command@pos@startLine)) {
    unused.returns <- returns[!returns$return.used & returns$return.node.id > 0 & !is.na(returns$line) & returns$line == command@pos@startLine, ]
  }
  else {
    unused.returns <- returns[!returns$return.used & returns$return.node.id > 0, ]
  }
  if (nrow(unused.returns) == 0) return()
  #print (paste(".ddg.link.function.returns: unused.returns:", unused.returns))

  # See which of these are called from the command we are
  # processing now.
  unused.calls <- unused.returns$ddg.call
  command.text <- gsub(" ", "", command@text)
  uses <- sapply(unused.calls, function(call) {grepl(call, command.text, fixed=TRUE)})
  #print (paste (".ddg.link.function.returns: uses:", uses))

  # The following line is here to get around R CMD check, which
  # otherwise reports:  no visible binding for global variable.
  # Note that return.node.id is not a variable in the subset call,
  # but the name of a column in the data frame being subsetted.
  return.node.id <- NULL

  # Extracts for the return value nodes.
  new.uses <- subset(unused.returns, uses, return.node.id)
  #print (paste (".ddg.link.function.returns: new.uses:", new.uses))

  # Create an edge from each of these to the last procedure node.
  lapply (new.uses$return.node.id,
      function(data.num) {
        proc.num <- .ddg.pnum()

        # Record in edges table
        etype <- "df.in"
        node1 <- paste("d", data.num, sep="")
        node2 <- paste("p", proc.num, sep="")
        .ddg.record.edge(etype, node1, node2)

        if (.ddg.debug.lib()) {
          print(paste(".ddg.link.function.returns:", command@abbrev))
          print(paste("DF ", node1, " ", node2, sep=""))
        }

        # Set the return value as being used.
        returns$return.used[returns$return.node.id == data.num] <- TRUE
        .ddg.set(".ddg.return.values", returns)
      })
  #print ("Returning from .ddg.link.function.returns")
}

# ddg.add.abstract.node is exclusively used in .ddg.parse.commands
# (so far) and simply serves to avoid repetition of code.

# type - type of procedure node.
# cmd - command string.
# called (optional) - name of calling function.

.ddg.add.abstract.node <- function(type, cmd = NULL, env, called=".ddg.parse.commands", node.name = "") {
  #print("In .ddg.add.abstract.node")
  #print("cmd =")
  #print(cmd)
  #print("node.name =")
  #print(node.name)
  if (node.name == "") {
    if (is.null(cmd)) {
      node.name <- .ddg.abbrev.cmd(cmd)
    }
    else {
      node.name <- cmd@abbrev
    }
  }
  if (.ddg.debug.lib()) print(paste(called, ":  Adding", node.name,  type, "node"))
  .ddg.proc.node(type, node.name, node.name, TRUE, env=env, cmd = cmd)
  .ddg.proc2proc()

  return(node.name)
}

# .ddg.close.last.command.node closes the last created collapsible
# node stored in .ddg.last.cmd properly.

# env - the environment in which the close is occurring
# called (optional) - used in debugging to identify the function
#   which called .ddg.close.previous.command.
# initial (optional) - if TRUE, try to close previous command node.

.ddg.close.last.command.node <- function(env, called=".ddg.parse.commands", initial=FALSE){

  # Get both the last command and new commands.
  .ddg.last.cmd <-
    if (.ddg.is.set(".ddg.last.cmd")) {
      .ddg.get(".ddg.last.cmd")
    }
    else {
      NULL
    }
  .ddg.possible.last.cmd <-
    if (.ddg.is.set(".ddg.possible.last.cmd")) {
      .ddg.get(".ddg.possible.last.cmd")
    }
    else {
      NULL
    }

  #print (paste (".ddg.close.last.command.node: .ddg.last.cmd =", .ddg.last.cmd))
  #print (paste (".ddg.close.last.command.node: .ddg.possible.last.cmd =", .ddg.possible.last.cmd))

  # Only create a finish node if a new command exists (i.e., we've
  # parsed some lines of code).
  if (!is.null(.ddg.last.cmd) && (!is.null(.ddg.possible.last.cmd) || initial)) {
    cmd.abbrev <- .ddg.add.abstract.node("Finish", .ddg.last.cmd, env=env, called=paste(called, "-> .ddg.close.last.command.node"))

    # Add link from a function return node if there is one.
    .ddg.link.function.returns(.ddg.last.cmd)
    # .ddg.link.function.returns(.ddg.last.cmd$text)

    # Create outflowing edges.
    # Has the assignment happened yet???
    #vars.set <- .ddg.find.var.assignments(.ddg.last.cmd)
    #print (".ddg.close.last.command.node calling .ddg.create.data.set.edges.for.cmd")
    #.ddg.create.data.set.edges.for.cmd(vars.set, .ddg.last.cmd$abbrev, .ddg.last.cmd$expr, 1, env, for.finish.node = TRUE)
    #print (".ddg.close.last.command.node call to .ddg.create.data.set.edges.for.cmd returned")

    # No previous command.
    #print (".ddg.close.last.command.node: created finish node; saving .ddg.last.cmd as null")
    .ddg.set(".ddg.last.cmd", NULL)
  }
}

# .ddg.open.new.command.node opens a new collapsible command
# node depending on the information stored in .ddg.last.cmd.

# env - the environment in which the command occurs
# called (optional) - name of calling function.

.ddg.open.new.command.node <- function(env, called=".ddg.parse.commands") {
  new.command <- .ddg.get(".ddg.possible.last.cmd")
  if (!is.null(new.command)) {
    .ddg.add.abstract.node("Start", new.command, env, called=paste(called, "-> .ddg.open.new.command.node"))

    # Now the new command becomes the last command, and new command
    # is null.
    #print (paste (".ddg.open.new.command.node: saving .ddg.last.cmd as", new.command))
    .ddg.set(".ddg.last.cmd", new.command)
    .ddg.set(".ddg.possible.last.cmd", NULL)
  }
}

# .ddg.is.procedure.cmd returns TRUE if the command passed in
# (as a string) will create a procedure node and therefore
# initiate the creation of a collapsible console node.

# cmd.str - command string.

.ddg.is.procedure.cmd <- function(cmd) {
  return(grepl("^ddg.(procedure|start|finish|restore|checkpoint)", cmd@text))
}

# .ddg.parse.lines takes as input a set of lines corresponding to
# the history of an R script or to an R script itself. It parses
# and converts them to executable commands. Each command might span
# multiple lines. The function returns a named list of commands.

# The contents of the list are:
#   text - each entry is the full text string of a single command.
#   commands - each entry is the parsed command.

# lines - set of lines from command history or R script.

.ddg.parse.lines <- function(lines) {
  # No new lines passed in, so return NULL.
  if (length(lines) == 0) return(NULL)

  # Parse the new lines.
  parsed.commands <- parse(text=lines)
  parsed.commands <- Filter(function(cmd) {return (!is.call(cmd) || !grepl("^ddg.eval", cmd[[1]]))}, parsed.commands)
  return(parsed.commands)
}

# Create the warning node for the saved warning and attach it to the node
# that created the warning

.ddg.record.warning <- function () {
  # Get the saved warning
  w <- .ddg.get.warning()

  # Create a message that looks like the one R creates
  callStr <-
      if (is.null (w$call)) ""
      else paste ("In ", head (deparse(w$call)), ": ")
  warningMessage <- paste (callStr, w$message)

  # Create the warning node
  .ddg.insert.error.message(warningMessage, "warning.msg", doWarn = FALSE)

  # Clear the saved warning
  .ddg.clear.warning()
}

# .ddg.process.breakpoint pauses execution of a script when a break
# point is reached.  Breakpoints may be set by using the debug
# parameter in ddg.run, adding ddg.breakpoint to the script, or using
# ddg.set.breakpoint at the R command line. If a breakpoint has been
# set, execution is paused and the script number (if > 0) and line
# number of the next command to be executed (or the function name if
# internal to a function) are displayed, followed by the text of the
# command. Execution resumes when the user enters text at the keyboard.
# Options include: Enter = execute next command, C = continue execution
# until another breakpoint is reached, and Q = quit debugging and
# continue until execution is finished.

# command - text of current parsed command.
# inside.function - whether called from within a function.

.ddg.process.breakpoint <- function(command, inside.function) {
  # Display prompt if we are reaching a breakpoint (i.e., not single-stepping)
  if (!.ddg.get("ddg.break")) {
    writeLines("\nEnter = next command, C = next breakpoint, D = display DDG, Q = quit debugging\n")
  }

  # Abbreviate command.
  abbrev <- command@abbrev

  # Display script and line numbers if top-level command.
  if (!inside.function) {
    snum <- command@script.num
    lnum <- command@pos@startLine

    if (snum == 0) slnum <- lnum
    else slnum <- paste(snum, ":", lnum, sep="")
    print(paste(slnum,  "  |  ", abbrev, sep=""))

  # Display name of function if inside function.
  } else {
    frame.num <- .ddg.get.frame.number(sys.calls())
    func.call <- sys.call(frame.num)

    # Need to check for closure in case of mapply function
    if (typeof(func.call[[1]]) == "closure") {
      func.name <- "FUN"
    } else {
      func.name <- as.character(func.call[[1]])
    }

    print(paste("[", func.name, "]  |  ", abbrev, sep=""))
  }

  # Save ddg.
  .ddg.txt.write()
  .ddg.json.write()

  # Get user input from the keyboard.
  line <- "D"
  while (line == "D") {
    line <- toupper(readline())
    if (line == "D") ddg.display()
    else if (line == "") .ddg.set("ddg.break", TRUE)
    else if (line == "C") .ddg.set("ddg.break", FALSE)
    else if (line == "Q") .ddg.set("ddg.break.ignore", TRUE)
  }

}

# Create the DDGStatement list for a list of parsed expressions.

# exprs - a list of parsed expressions
# script.name - the name of the script the expressions come from
# parseData - information provided by the parser that we use to find line numbers
# enclosing.pos - if exprs are statements within a function definition, enclosing.pos
#   is the source position information of the entire function declaration

# Returns a list of DDGStatement objects

.ddg.create.DDGStatements <- function (exprs, script.name, script.num, parseData = NULL, enclosing.pos = NULL) {
  # The parse data gives us line number information
  if (is.null(parseData)) {
    parseData <- getParseData(exprs, includeText=TRUE)

    if (is.null(parseData)) {
      # In this case there is no line number information available
      cmds <- vector("list", (length(exprs)))
      for (i in 1:length(exprs)) {
        expr <- as.expression(exprs[i])
        cmds[[i]] <- .ddg.construct.DDGStatement(expr, NA, script.name, script.num, NA, parseData)
      }
      return(cmds)
    }

    non.comment.parse.data <- parseData[parseData$token != "COMMENT", ]
    if (nrow(non.comment.parse.data) == 0) {
      return(list())
    }

    # Start at the first non-comment expression in parseData
    next.parseData <- 1
  }

  else {
    non.comment.parse.data <- parseData[parseData$token != "COMMENT", ]

    # Start at the first entry in parse data that begins after the enclosing function begins,
    # ends before the enclosing function ends, and matches the text of the first expression.
    next.parseData <- which(non.comment.parse.data$line1 >= enclosing.pos@startLine & non.comment.parse.data$line2 <= enclosing.pos@endLine & non.comment.parse.data$text == paste(deparse(exprs[[1]]), collapse="\n") )[1]
  }

  # Get the breakpoint information
  breakpoints <- ddg.list.breakpoints()
  breakpoints <- breakpoints[breakpoints$sname == script.name, ]

  # Create the DDGStatements
  cmds <- vector("list", (length(exprs)))
  next.cmd <- 1
    for (i in 1:length(exprs)) {
      expr <- as.expression(exprs[i][[1]])
      next.expr.pos <- new (Class = "DDGStatementPos", non.comment.parse.data[next.parseData, ])
      cmds[[next.cmd]] <- .ddg.construct.DDGStatement(expr, next.expr.pos, script.name, script.num, breakpoints, parseData)
      next.cmd <- next.cmd + 1
  
      # If there are more expressions, determine where to look next in the parseData
      if (i < length(exprs)) {
         last.ending.line <- non.comment.parse.data[next.parseData,]$line2
         last.parent <- non.comment.parse.data[next.parseData,"parent"]
         last.id <- non.comment.parse.data[next.parseData,"id"]
  
         # Find the first entry in parseData that has the same parent as the
         # previous expression and starts after the previous expression.
         next.parseData <- which(non.comment.parse.data$parent == last.parent & non.comment.parse.data$line1 >= last.ending.line & non.comment.parse.data$id > last.id) [1]
      }
    }

  return (cmds)
}

# .ddg.save.annotated.script saves a copy of the annotated script to
# the debug directory.

.ddg.save.annotated.script <- function(cmds, script.name) {
  for (i in 1:length(cmds)) {
    expr <- cmds[[i]]@annotated
    for (j in 1:length(expr)) {
      line <- deparse(expr[[j]])
      if (i == 1 && j == 1) script <- line else script <- append(script, line)
    }
  }

  fileout <- file(paste(.ddg.path.debug(), "/annotated-", script.name, sep=""))
  write(script, fileout)
  close(fileout)
}

# .ddg.parse.commands takes as input a list of parsed expressions from
# an R script and creates DDG nodes for each command. If environ is an
# environment, it executes the commands in that environment
# immediately before creating the respective nodes for that
# command, and then creates the data nodes based on the information
# available in the environment. If environ is not NULL, calls to
# ddg.* are not executed so only the clean script is processed.
# If annotate.inside is TRUE, ddg.function, ddg.eval and ddg.return.value
# are added to each function definition and ddg.eval is added to control
# statements before commands are processed. If save.debug is TRUE,
# changes to the script are saved in the ddg/debug directory.
# ddg.annotate.on and ddg.annotate.off may be used to limit the
# functions that are annotated or not annotated, respectively.
#
# If run.commands is false, the commands are not executed.  This allows
# us to build ddgs for commands run from the console as those commands
# have already been executed.

# exprs - list of parsed R statements
# script.name - name of the script the statements came from
# script.num - the number of the script in the sourced scripts table
# environ - environment in which commands should be
#   executed.
# ignore.patterns (optional) - a vector of regular expressions.
#   Any commands matching these expressions will not be parsed
#   (i.e. no nodes will be created for them).
# node.name (optional) - name for the collapsible node under which
#   this DDG should be stored.
# run.commands (optional) - commands are executed only when environ
#   is an environment and run.commands is TRUE.
# echo (optional) - print each expression after parsing
# print.eval (optional) - print result of each evaluation.
# max.deparse.length (optional) - maximum number of characters
#   output for deparse of a single expression.
# called.from.ddg.eval(optional) - whether called from ddg.eval
# cmds - list of DDG Statements that correspond to the exprs passed in.  This is
#   currently only used when called from ddg.eval.  Normally, ddg.parse.commands
#   creates the DDG Statement objects.

.ddg.parse.commands <- function (exprs, script.name="", script.num=NA, environ, ignore.patterns=c('^ddg.'), node.name="Console", run.commands = FALSE, echo=FALSE, print.eval=echo, max.deparse.length=150, called.from.ddg.eval=FALSE, cmds=NULL) {

  return.value <- NULL
  
  # Gather all the information that we need about the statements
  if (is.null(cmds)) {
    cmds <- .ddg.create.DDGStatements (exprs, script.name, script.num)

    if (.ddg.save.debug()) {
      .ddg.save.annotated.script(cmds, script.name)
    }
  }
  num.cmds <- length(cmds)

  # Figure out if we will execute commands or not.
  execute <- run.commands & !is.null(environ) & is.environment(environ)

  # print (paste("ddg.parse.commands: .ddg.func.depth =", .ddg.get(".ddg.func.depth")))
  inside.func <- (.ddg.get(".ddg.func.depth") > 0)

  # Attempt to close the previous collapsible command node if a ddg
  # exists
  if (.ddg.is.init() && !inside.func) .ddg.close.last.command.node(environ, initial=TRUE)

  # Get the last command in the new commands and check to see if
  # we need to create a new .ddg.last.cmd node for future reference.
  if (!inside.func) {
      .ddg.last.cmd <- cmds[[num.cmds]]
    # print(paste(".ddg.parse.commands: setting .ddg.last.cmd to", .ddg.last.cmd$text))
    if (.ddg.last.cmd@isDdgFunc) {
      .ddg.last.cmd <- NULL
      #print(".ddg.parse.commands: setting .ddg.last.cmd to null")
    }

    else if (!execute) {
      cmds <- cmds[1:num.cmds-1]
    }
  }

  # Create start and end nodes to allow collapsing of consecutive
  # console nodes. Don't bother doing this if there is only 1 new
  # command in the history or execution.
  named.node.set <- FALSE
  start.node.created <- ""

  if (num.cmds > 0 && .ddg.is.init() && !inside.func && !called.from.ddg.eval) {
    # print(paste("ddg.new.parse.commands: Creating Start for", node.name))
    .ddg.add.abstract.node("Start", node.name = node.name, env = environ)
    named.node.set <- TRUE
    start.node.created <- node.name
  }

  # Don't set .ddg.last.cmd.  We want it to have the value from
  # the last call. We set it at the end of this function:
  # .ddg.set(".ddg.last.cmd", .ddg.last.cmd)

  # Create an operation node for each command.  We can't use lapply
  # here because we need to process the commands in order and
  # lapply does not guarantee an order. Also decide which data nodes
  # and edges to create. Only create a data node for the last
  # write of a variable and only if that occurs after the last
  # possible writer. Create an edge for a data use as long as the
  # use happens before the first writer/possible writer or after
  # the last writer/possible writer. Lastly, if execute is set to
  # true, then execute each command immediately before attempting
  # to create the DDG nodes.

  # Only go through this if  we have at least one command to parse.
  if (num.cmds > 0) {

    # Find where all the variables are assigned for non-environ
    # files.
    if (!execute) {
      vars.set <- .ddg.find.var.assignments(cmds)
    }
    else {
      vars.set <- .ddg.create.empty.vars.set()
    }

    # Loop over the commands as well as their string representations.
    for (i in 1:length(cmds)) {
      cmd <- cmds[[i]]

      if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Processing", cmd@abbrev))

      # Process breakpoint. We stop if there is a breakpoint set on this line or we are single-stepping.
      # print("Checking for breakpoints")
      if (.ddg.is.sourced() & (cmd@is.breakpoint | .ddg.get("ddg.break")) & !.ddg.break.ignore()) {
        .ddg.process.breakpoint(cmd, inside.function=called.from.ddg.eval)
      }

      # print("Checking whether to set last.cmd")
      if (.ddg.enable.source() && grepl("^ddg.eval", cmd@text) && .ddg.enable.console()) {
        if (is.null(.ddg.last.cmd)) {
          .ddg.last.cmd <- cmd
        }
      }

      # Get environment for output data node.
      d.environ <- environ

      
      if ( .ddg.is.nonlocal.assign(cmd@parsed[[1]]) )
      {
        d.environ <- .ddg.get.env(cmd@vars.set, for.caller=TRUE)
      
        if( identical(d.environ,"undefined") )
          d.environ <- globalenv()
      }
      
      
      #if ( .ddg.is.nonlocal.assign(cmd@parsed[[1]]) )
      #{
      #  # NOT WORKING!! - CAN NOT FIND ENVIRONMENT EVEN IF VARIABLE EXISTS
      #  d.environ <- .ddg.where( cmd@vars.set , env = parent.env(parent.frame()) , warning = FALSE )
      #
      #  if( identical(d.environ,"undefined") )
      #    d.environ <- globalenv()
      #}


      # Check for control & loop statements.
      st.type <- .ddg.get.statement.type(cmd@parsed[[1]])

      control.statement <- (st.type == "if" || st.type == "for" || st.type == "while" || st.type == "repeat" || st.type == "{")

      loop.statement <- (st.type == "for" || st.type == "while" || st.type == "repeat")

      # Specifies whether or not a procedure node should be created
      # for this command. Basically, if a ddg exists and the
      # command is not a DDG command or a control statement, it should
      # be created. Note that if control statements are annotated,
      # a procedure node is created for each statement inside a control
      # block, so there is no need to create additional nodes for the
      # control statement itself.

      create <- !cmd@isDdgFunc && .ddg.is.init() && .ddg.enable.console() && !(control.statement && .ddg.loop.annotate() && ddg.max.loops() > 0)
      # create <- !cmd@isDdgFunc && .ddg.is.init() && .ddg.enable.console()
      start.finish.created <- FALSE
      cur.cmd.closed <- FALSE

      # If the command does not match one of the ignored patterns.
      if (!any(sapply(ignore.patterns, function(pattern){grepl(pattern, cmd@text)}))) {

        # If sourcing, we want to execute the command.
        if (execute) {
          # Print command.
          if (echo) {
            nd <- nchar(cmd@text)
            do.trunc <- nd > max.deparse.length
            cmd.show <- paste0(substr(cmd@text, 1L, if (do.trunc)
                          max.deparse.length
                        else nd), "\n")
            cat(cmd.show)
          }

          # If we will create a node, then before execution, set
          # this command as a possible abstraction node but only
          # if it's not a call that itself creates abstract nodes.
          if (!cmd@isDdgFunc && cmd@text != "next") {
            .ddg.set(".ddg.possible.last.cmd", cmd)
            .ddg.set (".ddg.cur.cmd", cmd)

            #print(paste("Pushing onto the stack:", cmd@text))
            
            # Remember the current statement on the stack so that we
            # will be able to create a corresponding Finish node later
            # if needed.
            .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")

            if (length(.ddg.cur.cmd.stack) == 0) {
              .ddg.cur.cmd.stack <- c(cmd, FALSE)
            }
            else {
              .ddg.cur.cmd.stack <- c(.ddg.get(".ddg.cur.cmd.stack"), cmd, FALSE)
            }
            .ddg.set(".ddg.cur.cmd.stack", .ddg.cur.cmd.stack)
          }

          else if (.ddg.is.procedure.cmd(cmd)) {
            .ddg.set(".ddg.possible.last.cmd", NULL)
          }

          # Need to get this number before evaluating the command so that 
          # when we evaluate a dev.off call we know which device was closed
          .ddg.set(".ddg.dev.number", dev.cur())
          
          if (cmd@has.dev.off && !cmd@createsGraphics && is.null(.ddg.get ("possible.graphics.files.open"))) {
            dev.file.created <- .ddg.capture.current.graphics()
          }
          else {
            dev.file.created <- NULL
          }
          
          
          # Before evaluating, 
          # keep track of variable types for common variables between vars.set and vars.used.
          #common.vars <- intersect( cmd@vars.set , cmd@vars.used )
          #num.vars <- length(common.vars)

          #if( num.vars > 0 )
          #  used.types <- sapply( common.vars , .ddg.get.val.type.from.var )

          # Capture any warnings that occur when an expression is evaluated.
          # Note that we cannot just use a tryCatch here because it behaves
          # slightly differently and we would lose the value that eval
          # returns.  withCallingHandlers returns the value.
          # withCallingHandlers also re-throws the error after it is caught.

          # EVALUATE.

          if (.ddg.debug.lib()) print (paste (".ddg.parse.commands: Evaluating ", cmd@annotated))
          #print (paste (".ddg.parse.commands: Evaluating ", cmd@annotated))
          #print (paste ("length(cmd@annotated) =", length(cmd@annotated)))

          result <- withCallingHandlers(
          
              {
                for (annot in cmd@annotated) {
                  #print (paste (".ddg.parse.commands: Evaluating ", paste(annot, collapse = " ")))
                  # Don't set return.value if we are calling a ddg function or we are executing an if-statement
                  if (grepl("^ddg", annot) || grepl("^.ddg", annot) || as.character(.ddg.get.statement.type(annot)) == "if") {
                    eval(annot, environ, NULL)
                  }
                  else {
                    return.value <- eval(annot, environ, NULL)
                    #if (typeof(return.value) != "closure") {
                    #  print (paste (".ddg.parse.commands: Done evaluating ", annot))
                    #  print(paste(".ddg.parse.commands: setting .ddg.last.R.value to", return.value))
                    #}
                    .ddg.set (".ddg.last.R.value", return.value)
                  }
                }
              },
            warning = .ddg.set.warning ,
            error = function(e)
            {
              # create procedure node for the error-causing operation
              .ddg.proc.node("Operation", cmd@abbrev, cmd@abbrev, env=environ, console=TRUE, cmd=cmd)
              .ddg.proc2proc()

              # create input edges by adding variables to set
              vars.set <- .ddg.add.to.vars.set(vars.set,cmd,i)
              if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding", cmd@abbrev, "information to vars.set, for an error"))
              .ddg.create.data.use.edges.for.console.cmd(vars.set, cmd, i, for.caller=FALSE)

              # check for factors
              #msg <- e[[1]]
              #
              #if( msg == "invalid 'type' (character) of argument" | msg == "only defined on a data frame with all numeric variables" )
              #{ 
              #  containsFactor <- sapply( cmd@vars.used , .ddg.var.contains.factor )
              #  
              #  if( is.element(TRUE , containsFactor) )
              #  {
              #    factors <- names(containsFactor)[which(containsFactor==TRUE)]
              #
              #    # form suggestion
              #    cat( "The following input data contain(s) a factor:\n" )
              #    cat( paste(shQuote(factors , type="cmd") , collapse = ", ") )
              #    cat("\nThis website may be helpful:\n")
              #
              #    #if( msg == "invalid 'type' (character) of argument" )
              #    #  cat("https://stat.ethz.ch/pipermail/r-help/2010-May/239461.html")
              #    #else
              #    #  cat("http://stackoverflow.com/questions/38032814/trying-to-understand-r-error-error-in-funxi-only-defined-on-a-data")
              #    cat( "https://www.r-bloggers.com/using-r-common-errors-in-table-import/" )
              #  }
              #}
              
              # create and link to an error node
              ddg.exception.out("error.msg", toString(e) , cmd@abbrev)
            }
          )

          if (.ddg.debug.lib()) print (paste (".ddg.parse.commands: Done evaluating ", cmd@annotated))

          
          
          # EDITS
          
          packages.used <- sapply( cmd@functions.called ,.ddg.where )
          packages.used <- sapply( packages.used , environmentName )
          
          packages.used <- packages.used[ grepl("package:", packages.used) ]
          packages.used <- mapply( substring , packages.used , 9 )
          
          packages <<- packages.used
          
          cmd@functions.called <- names(packages.used)
          cmd@packages.used <- unique(packages.used)
          
          new.packages.used <<- cmd@packages.used
          new.functions <<- cmd@functions.called
          

          # After evaluating
          # Check changes to variable type for common variables between vars.set and vars.used
          #if( num.vars > 0 )
          #{
          #  set.types <- sapply( common.vars , .ddg.get.val.type.from.var )
          #
          #  # comparing type changes for variables set and variables used
          #  # only 1 common variable between vars.set and vars.used
          #  if( num.vars == 1 )
          #  {
          #    is.same <- identical( set.types , used.types )
          #
          #    # remember var names for vars whose types have changed, NULL otherwise
          #    if(is.same)
          #      changed.vars <- NULL
          #    else
          #      changed.vars <- common.vars
          #  }
          #
          #  # multiple common variables between vars.set and vars.used
          #  # currently untested since multiple variable assignment is not working!
          #  else
          #  {
          #    is.same <- mapply( identical , set.types , used.types )
          #    changed.vars <- names(is.same)[which(is.same==FALSE)]
          #
          #    # convert to string
          #    if( ! is.null(changed.vars) )
          #      changed.vars <- paste( changed.vars , collapse = "," )
          #  }
          #
          #  # show warning message
          #  if( ! is.null(changed.vars) )
          #  {
          #    msg <- paste( "For the statement \"" , cmd@text , "\", " , "in line " , cmd@pos@startLine , ",\n" , sep = "" )
          #    msg <- paste( msg , "The type changed for the following variables: " , changed.vars , sep = "" )
          #
          #    warning( msg , call. = FALSE )
          #  }
          #}

          if (!cmd@isDdgFunc && cmd@text != "next") {
            # Need to get the stack again because it could have been
            # modified during the eval call.
            .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
            stack.length <- length(.ddg.cur.cmd.stack)
            start.created <- .ddg.cur.cmd.stack[stack.length][[1]]

            # Create a finish node if a start node was created
            # start.created can have one of 3 values: "TRUE", "FALSE",
            # "MATCHES_CALL". Only create the finish node if TRUE.
            if (start.created == "TRUE") {
              .ddg.add.abstract.node("Finish", cmd, environ)
              start.finish.created <- TRUE
              .ddg.link.function.returns(cmd)

              # If the number of loop iterations exceeds max.loops, add
              # output data nodes containing final values to the finish node.
              if (loop.statement && .ddg.were.details.omitted()) {
                vars.set2 <- .ddg.add.to.vars.set(vars.set, cmd, i)
                .ddg.create.data.node.for.possible.writes(vars.set2, cmd, environ)
                .ddg.set.details.omitted(FALSE)
              }
            }

            # Remove the last command & start.created values pushed
            # onto the stack
            cur.cmd.closed <- (.ddg.cur.cmd.stack[stack.length] == "MATCHES_CALL")
            if (stack.length == 2) {
              .ddg.set(".ddg.cur.cmd.stack", vector())
            }
            else {
              .ddg.set(".ddg.cur.cmd.stack", .ddg.cur.cmd.stack[1:(stack.length-2)])
            }
          }

          # Print evaluation.
          if (print.eval) print(result)
        }

        # Figure out if we should create a procedure node for this
        # command. We don't create it if it matches a last command
        # (because that last command has now become a collapsible
        # node). Matching a last command means that the last command
        # is set, is not NULL, and is equal to the current command.

        last.proc.node.created <-
            if (.ddg.is.set (".ddg.last.proc.node.created")).ddg.get(".ddg.last.proc.node.created")
            else ""
        
        create.procedure <- create && (!cur.cmd.closed || !named.node.set) && !start.finish.created  && !grepl("^ddg.source", cmd@text)
        
        # We want to create a procedure node for this command.
        if (create.procedure) {

          # Create the procedure node.

          if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding operation node for", cmd@abbrev))

          .ddg.proc.node("Operation", cmd@abbrev, cmd@abbrev, env=environ, console=TRUE, cmd=cmd)
          .ddg.proc2proc()

          # If a warning occurred when cmd was evaluated,
          # attach a warning node
          if (.ddg.warning.occurred()) {
            .ddg.record.warning()
          }
          # Store information on the last procedure node in this
          # block.
          last.proc.node <- cmd

          # We want to create the incoming data nodes (by updating
          # the vars.set).
          if (execute) {
            # Add variables to set.
            vars.set <- .ddg.add.to.vars.set(vars.set,cmd,i)

            if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding", cmd@abbrev, "information to vars.set"))
          }

          .ddg.create.data.use.edges.for.console.cmd(vars.set, cmd, i, for.caller=FALSE)

          if (cmd@readsFile) .ddg.create.file.read.nodes.and.edges(cmd, environ)
          .ddg.link.function.returns(cmd)

          if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding input data nodes for", cmd@abbrev))

          .ddg.create.data.set.edges.for.cmd(vars.set, cmd, i, d.environ)

          if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding output data nodes for", cmd@abbrev))

          if (cmd@writesFile) .ddg.create.file.write.nodes.and.edges (cmd, environ)
          if (cmd@createsGraphics) .ddg.set.graphics.files (cmd, environ)
          if (cmd@updatesGraphics) .ddg.add.graphics.io (cmd)

          if (cmd@has.dev.off) {
            .ddg.capture.graphics(cmd)
  
            if (!is.null(dev.file.created)) {
              file.remove(dev.file.created)
            }
          }
        }
        # We wanted to create it but it matched a last command node.
        else if (create && execute) {
          .ddg.close.last.command.node(environ, initial=TRUE)
          if (execute) {
            # Add variables to set.
            vars.set <- .ddg.add.to.vars.set(vars.set,cmd, i)
            if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding", cmd@abbrev, "information to vars.set"))
            .ddg.create.data.set.edges.for.cmd(vars.set, cmd, i, environ)
          }
        }

        if (create.procedure && execute) {
          .ddg.create.data.node.for.possible.writes(vars.set, last.proc.node, env=environ)

          # Update so we don't set these again.
          vars.set$possible.last.writer <- vars.set$last.writer
        }
      }
     }

     # Create a data node for each variable that might have been set in
     # something other than a simple assignment, with an edge from the
     # last node in the console block or source .
     if (!execute) {
       .ddg.create.data.node.for.possible.writes(vars.set, last.proc.node, env=environ)
     }
  }

  #print("Done with ddg.parse.commands loop")

  # Close any node left open during execution.
  if (execute && !inside.func) .ddg.close.last.command.node(environ, initial=TRUE)

  # Close the console block if we processed anything and the DDG
  # is initialized (also, save).
  #
  if (.ddg.is.init() && named.node.set && !inside.func) {
      .ddg.add.abstract.node("Finish", node.name = node.name, env=environ)
  }

  # Open up a new collapsible node in case we need to parse
  # further later.
  if (!execute) {

    .ddg.set(".ddg.possible.last.cmd", .ddg.last.cmd)
    .ddg.set(".ddg.last.cmd", .ddg.last.cmd)
    .ddg.open.new.command.node(environ)
  }

  # Write time stamp to history.
  if (.ddg.is.init() && !.ddg.is.sourced()) .ddg.write.timestamp.to.history()

  return.value <- .ddg.get (".ddg.last.R.value")
  #if (typeof(return.value) != "closure") {
  #  print(paste(".ddg.parse.commands: returning ", return.value))
  #}
  return(return.value)
}


# Returns TRUE if the value of the given variable name is a data frame
# containing at least one factor. Returns FALSE otherwise.
# var - the variable name

.ddg.var.contains.factor <- function( var )
{
  value <- get(var)

  if( is.data.frame(value) )
    return( is.element("factor",sapply(value,class)) )

  return(FALSE)
}


# .ddg.console.node creates a console node.

.ddg.console.node <- function() {
  # Don't do anything if sourcing, because history isn't necessary
  # in this case.
  if(.ddg.enable.source()) return(NULL)

  ddg.history.file=.ddg.get(".ddg.history.file")
  ddg.history.timestamp=.ddg.get(".ddg.history.timestamp")

  # Only continue if these values exists.
  if (!(is.null(ddg.history.file) || is.null(ddg.history.timestamp))) {
    # Grab any new commands that might still be in history.
    tryCatch (
        {
          # Saving history is not supported on all platforms.
          .ddg.savehistory(ddg.history.file)

          # Load from extended history since last time we wrote out
          # a console node.
          new.lines <- .ddg.loadhistory(ddg.history.file,ddg.history.timestamp)

          # Parse the lines into individual commands.
          parsed.commands <- .ddg.parse.lines(new.lines)

          # New commands since last timestamp.
          if (!is.null(parsed.commands) && length(parsed.commands) > 0) {
           .ddg.parse.commands(parsed.commands,
                environ = .GlobalEnv,
                run.commands=FALSE)
          }
        },
        error =
            function(e) {
            })

  }
}

# .ddg.proc.node creates a procedure node.

# ptype - type of procedure node.
# pname - name of procedure node.
# pvalue (optional) - value of procedure node.
# console (optional) - if TRUE, console mode is enabled.
# auto.created - TRUE means that the node is being automatically
#   created when a return call is found
# ptime - elapsed time
# env - the environment in which the procedure occurs

# CHECK!  Looks like env parameter is not needed!
.ddg.proc.node <- function(ptype, pname, pvalue="", console=FALSE,
    auto.created=FALSE, env = sys.frame(.ddg.get.frame.number(sys.calls())),
    cmd = NULL) {
  if (.ddg.debug.lib()) {
    if (length(pname) > 1) {
      print(sys.calls())
    }
  }

  # We're not in a console node but we're capturing data
  # automatically.
  if (.ddg.enable.console()) {

    # Capture graphic output of previous procedure node.
    # Comment out this function???
    #.ddg.auto.graphic.node()

    if (!console && !.ddg.enable.source() && interactive()) {
      .ddg.console.node()
    }
  }

  if (is.null(cmd)) {
    snum <- NA
    pos <- NA
  }
  else {
    snum <- cmd@script.num
    pos <- cmd@pos
  }

  # Record start & finish information
  if (ptype == "Start") {
    .ddg.starts.open <- .ddg.get (".ddg.starts.open")
    .ddg.starts.open <- c(.ddg.starts.open, pname)
    .ddg.set (".ddg.starts.open", .ddg.starts.open)
  }
  else if (ptype == "Finish") {
    .ddg.starts.open <- .ddg.get (".ddg.starts.open")
    num.starts.open <- length(.ddg.starts.open)
    if (num.starts.open > 0) {
      last.start.open <- .ddg.starts.open[num.starts.open]
      if (num.starts.open > 1) {
        .ddg.starts.open <- .ddg.starts.open[1:num.starts.open-1]
      }
      else {
        .ddg.starts.open <- vector()
      }
      .ddg.set (".ddg.starts.open", .ddg.starts.open)
      if (last.start.open != pname) {
        .ddg.insert.error.message("Start and finish nodes do not match")
      }
    }
    else {
      .ddg.insert.error.message("Attempting to create a finish node when there are no open blocks")
    }
  }
  .ddg.set(".ddg.last.proc.node.created", paste(ptype, pname))

  ptime <- .ddg.elapsed.time()

  # Record in procedure node table
  .ddg.record.proc(ptype, pname, pvalue, auto.created, ptime, snum, pos)

  #if (ptype == "Finish") print(sys.calls())
  if (.ddg.debug.lib()) print(paste("proc.node:", ptype, pname))
}

# .ddg.replace.quotes quotes quotation characters. It also replaces
# return, newline and tab characters with spaces.

# str - input string.

.ddg.replace.quotes <- function(str) {
  if (!is.character(str)) return (str)

  str <- paste("\"", str, "\"", sep="")
  str <- gsub("\"", "\\\\\"", str)

  # Replace returns, new lines, and tabs with spaces.
  str <- gsub("\r", " ", str)
  str <- gsub("\n", " ", str)
  str <- gsub("\t", " ", str)
}

# .ddg.convert.list.to.string converts a list of values to a string
# by calling as.character on each element in the list.

# dvalue - a list of values.

.ddg.convert.list.to.string <- function (dvalue) {
  values <- .ddg.replace.quotes(lapply(dvalue, .ddg.as.character))
  positions <- 1:length(values)
  paste("[[", positions, "]]", values, collapse="\n")
}

# .ddg.as.character wraps an exception handler around as.character
# The exception handler captures the print output for the value and
# returns that instead.
.ddg.as.character <- function (value) {
  tryCatch (as.character(value),
            error=function(e) {capture.output(print(value))})
}

# .ddg.data.node creates a data node of type Data. Data nodes are
# used for single data values. The value (dvalue) is stored in the
# DDG.

# dtype - type of data node.
# dname - name of data node.
# dvalue - value of data node.
# dscope - scope of data node.
# from.env - if object is from initial environment

.ddg.data.node <- function(dtype, dname, dvalue, dscope, from.env=FALSE) {
  #print(paste(".ddg.data.node: dname =", dname))
  #print(paste(".ddg.data.node: typeof(dvalue) =", typeof(dvalue)))
  #print(paste(".ddg.data.node: dvalue =", dvalue))
  #print(paste(".ddg.data.node: dscope =", dscope))
  # If object or a long list, try to create snapshot node.

  if (is.object(dvalue)) {
    #print(".ddg.data.node: is object")
    tryCatch(
        {
          .ddg.snapshot.node (dname, "txt", dvalue, dscope=dscope, from.env=from.env)
          return(NULL)
        },
        error = function(e) {
          error.msg <- paste("Unable to create snapshot node for", dname, "Details:", e)
          .ddg.insert.error.message(error.msg)
          return (.ddg.data.node (dtype, dname, "complex", dscope, from.env=from.env))
        }
    )

  }

  else if (is.matrix(dvalue) || (is.vector(dvalue) && length(dvalue) > 20)) {
    #print(".ddg.data.node: saving as csv")
    .ddg.snapshot.node (dname, "csv", dvalue, dscope=dscope, from.env=from.env)
    return (NULL)
  }

  #print("Converting value to a string")
  # Convert value to a string.
  val <-
      if (is.list(dvalue)) {
        tryCatch(
            {
              .ddg.convert.list.to.string(dvalue)
            },
            error = function(e) {
              error.msg <- paste("Unable to convert value of", dname, "to a string.")
              .ddg.insert.error.message(error.msg)
              "complex"
            }
        )
      }
      else if (typeof(dvalue) == "closure") "#ddg.function"
      else if (length(dvalue) > 1 || !is.atomic(dvalue)) {
        tryCatch(paste(.ddg.replace.quotes(dvalue), collapse=","),
            error = function(e) {"complex"})
      }
      else if (is.null(dvalue)) "NULL"
      else if (length(dvalue) == 0) "Empty"
      else if (is.na(dvalue)) "NA"
      else if (dvalue == "complex" || dvalue == "#ddg.function") dvalue
      else if (is.character(dvalue) && dvalue == "") "NotRecorded"
      else {
         # Replace double quotes with single quotes.
         .ddg.replace.quotes(dvalue)
      }

  #print(paste(".ddg.data.node: converted value to string", val))


  if (grepl("\n", val)) {
    #print(".ddg.data.node: saving as snapshot")

    # Create snapshot node.
    .ddg.snapshot.node (dname, "txt", val, from.env=from.env)
    return
  }

  else {
    #print(".ddg.data.node: recording data")

    # Get scope if necessary.
    if (is.null(dscope)) dscope <- .ddg.get.scope(dname)

    # Record in data node table
    .ddg.record.data(dtype, dname, val, val, dscope, from.env=from.env)

    if (.ddg.debug.lib()) print(paste("data.node:", dtype, dname))
  }

  invisible()
}

# .ddg.supported.graphic - the sole purpose of this function is
# to verify that the input file extension is a supported graphic
# type. Currently supported graphics types inlude: jpg, jpeg,
# bmp, png, tiff.

# ext - file extension.

.ddg.supported.graphic <- function(ext){
  return(ext %in% c("jpeg", "jpg", "tiff", "png", "bmp", "pdf"))
}

# .ddg.graphic.snapshot provides factoring of snapshot code.

# fext - file extension.
# dpfile - path and name of file.

.ddg.graphic.snapshot <-function(fext, dpfile) {
  # pdfs require a separate procedure.
  if (fext == "pdf") dev.copy2pdf(file=dpfile)

  # At the moment, all other graphic types can be done by
  # constructing a similar function.
  else {
    # If jpg, we need to change it to jpeg for the function call.
    fext = ifelse(fext == "jpg", "jpeg", fext)

    # First, we create a string, then convert it to an actual R
    # expression and use that as the function.
    strFun <- paste(fext, "(filename=dpfile, width=800, height=500)", sep="")
    parseFun <- function(){eval(parse(text=strFun))}
    dev.copy(parseFun)

    # Turn it off (this switches back to prev device).
    dev.off()
  }
}

# .ddg.snapshot.node creates a data node of type Snapshot. Snapshots
# are used for complex data values not written to file by the main
# script. The contents of data are written to the file dname.fext
# in the DDG directory. Snapshots are also used to capture output
# plots and other graphics generated by the R script.

# The user can control the size of the snapshot files by setting the
# max.snapshot.size parameter when calling ddg.init or ddg.run.  If
# the user passes in 0, no snapshots are saved.
# Instead a data node will be created.  If the user passes in -1,
# there is no limit on the snapshot size.  If the user passes a value > 0,
# if the R value is larger than this size, only the head of the data will
# be saved.

# dname - name of data node.
# fext - file extension.
# data - value of data node.
# save.object (optional) - if TRUE, also save as an R object.
# dscope (optional) - scope of data node.

.ddg.snapshot.node <- function(dname, fext, data, save.object = FALSE, dscope=NULL, from.env=FALSE) {

  orig.data <- data

  # Determine if we should save the entire data
  max.snapshot.size <- ddg.max.snapshot.size()

  if (max.snapshot.size == 0) {
    return(.ddg.data.node ("Data", dname, "", dscope, from.env=from.env))
  }

  # Snapshot name
  snapname <- dname

  # object.size returns bytes, but max.snapshot.size is in kilobytes
  if (max.snapshot.size == -1 || object.size(data) < max.snapshot.size * 1024) {
    full.snapshot <- TRUE

  }

  else if (is.vector(data) || is.list(data) || is.data.frame(data) || is.matrix(data) || is.array(data)) {
    # Decide how much data to save

    element.size <- object.size(head(data, 1))
    num.elements.to.save <- ceiling(max.snapshot.size * 1024 / element.size)
    if (num.elements.to.save < length(data)) {
      #print (paste ("object.size(data)" = object.size(data)))
      data <- head(data, num.elements.to.save)
      snapname <- paste(dname, "-PARTIAL", sep="")
      full.snapshot <- FALSE
      #print(paste ("element.size =", element.size))
      #print (paste (".ddg.snapshot.node: Saving", num.elements.to.save, "elements for", dname))
      #print(paste("Size of saved data =", object.size(data)))

    }
    else {
      full.snapshot <- TRUE
    }
  }

  else {
    full.snapshot <- FALSE
    snapname <- paste(dname, "-PARTIAL", sep="")
  }

  # Snapshot type
  dtype <- "Snapshot"

  # If the object is an environment, update the data to be the environment's
  # name followed by a list of the variables bound in the environment.
  if (is.environment (data)) {
    envHeader <- paste0 ("<environemnt: ", environmentName (data), ">")
    data <- c (envHeader, ls(data), recursive=TRUE)
  }
  else if ("XMLInternalDocument" %in% class(data)) {
    fext <- "xml"
  }
  else if (is.vector(data) || is.data.frame(data) || is.matrix(data) || is.array(data) || is.list(data)) {
  }
  else if (!is.character(data)) {
    tryCatch(data <- as.character(data),
          error = function(e){
            # Not sure if str or summary will give us the most useful
            # information.
            #data <- capture.output(str(data));
            #print(paste(".ddg.snapshot.node: generating summary for", dname))
            data <- summary(data)
          })
  }

  # Default file extensions.
  dfile <-
      if (fext == "" || is.null(fext)) paste(.ddg.dnum()+1, "-", snapname, sep="")
      else paste(.ddg.dnum()+1, "-", snapname, ".", fext, sep="")

  # Get path plus file name.
  dpfile <- paste(.ddg.path.data(), "/", dfile, sep="")
  if (.ddg.debug.lib()) print(paste("Saving snapshot in ", dpfile))

  # Write to file .
  if (fext == "csv") write.csv(data, dpfile, row.names=FALSE)

  else if (fext == "xml") saveXML (data, dpfile)

  # Capture graphic.
  else if (.ddg.supported.graphic(fext)) .ddg.graphic.snapshot(fext, dpfile)

  # Write out RData (this is old code, not sure if we need it).
  else if (fext == "RData") file.rename(paste(.ddg.path.data(), "/", dname, sep=""), dpfile)

  # Write out text file for txt or empty fext.
  else if (fext == "txt" || fext == "") {
    file.create(dpfile, showWarnings=FALSE)
    if (is.list(data) && length(data) > 0) {
      list.as.string <- .ddg.convert.list.to.string(data)
      write(list.as.string, dpfile)
    }
    else {
      tryCatch(write(as.character(data), dpfile),
          error = function(e){
            capture.output(data, file=dpfile)
          })
    }
  }

  # Write out data node object if the file format is unsupported.
  else {
    error.msg <- paste("File extension", fext, "not recognized")
    .ddg.insert.error.message(error.msg)
    return(NULL)
  }

  # Check to see if we want to save the object.
  if (save.object && full.snapshot) save(data, file = paste(.ddg.path.data(), "/", .ddg.dnum()+1, "-", snapname, ".RObject", sep=""), ascii = TRUE)

  dtime <- .ddg.timestamp()

  # Get scope if necessary.
  if (is.null(dscope)) dscope <- .ddg.get.scope(dname)

  # Record in data node table
  .ddg.record.data(dtype, dname, paste(.ddg.data.dir(), dfile, sep="/"), orig.data, dscope, from.env=from.env, dtime)

  if (.ddg.debug.lib()) print(paste("snapshot.node: ", dname))
  return(dpfile)
}

# .ddg.file.node creates a node of type File. File nodes are used
# for files written to the DDG directory by capturing output from
# the script or by copying a file that is written by the script.
# Returns the path where the file referenced by the node is stored.

# dtype - type of data node.
# fname - path and name of original file.
# dname - name of data node.
# dscope (optional) - scope of data node.

.ddg.file.node <- function(dtype, fname, dname, dscope=NULL) {

  # Get original file location.
  file.name <- basename(fname)
  file.loc <- normalizePath(fname, winslash="/", mustWork = FALSE)

  loc.value <-
    if (dtype == "File") paste(" Location=\"", file.loc, "\"", sep="")
    else ""

  # Add number to file name.
  dfile <- paste(.ddg.dnum()+1, "-", file.name, sep="")

  # Calculate the path to the file relative to the ddg directory.
  # This is the value stored in the node.
  dpfile <- paste(.ddg.data.dir(), dfile, sep="/")

  dtime <- .ddg.timestamp()

  # Set the node label.
  if (is.null(dname)) dname <- file.name

  # Get scope if necessary.
  if (is.null(dscope)) dscope <- .ddg.get.scope(dname)

  # Not from environment.
  from.env <- FALSE

  # Record in data node table
  .ddg.record.data(dtype, dname, dpfile, dpfile, dscope, from.env=from.env, dtime, file.loc)

  # Get path plus file name to where the file will be copied
  dpath <- paste(.ddg.path.data(), "/", dfile, sep="")
  return(dpath)
}

# .ddg.file.copy creates a data node of type File. File nodes are
# used for files written by the main script. A copy of the file is
# written to the DDG directory.

# dtype - type of data node.
# fname - path and name of original file.
# dname - name of data node.
# dscope - scope of data node.

.ddg.file.copy <- function(dtype, fname, dname, dscope) {
  # Calculate location of original file.
  file.loc <- normalizePath(fname, winslash="/", mustWork = FALSE)

  # Copy file.
  if (file.exists(file.loc)) {
    # Create file node in DDG.
    dpfile.out <- .ddg.file.node(dtype,fname,dname, dscope)
    file.copy(file.loc, dpfile.out, overwrite=TRUE)
  }
  else {
    error.msg <- paste("File to copy does not exist:", fname)
    .ddg.insert.error.message(error.msg)
    return(NULL)
  }

  if (.ddg.debug.lib()) print(paste("file.copy: ", dtype, " ", file.loc))
  return (dpfile.out)
}

# .ddg.insert.error.message issues a warning and inserts an
# exception node after the last procedure step. The name of the node
# is "error.msg" and the value is the error message passed to this
# function.

# msg - error message.
# msg.type - error or warning
# scope - scope for evaluating any data
# doWarn - if true, this function displays a warning

.ddg.insert.error.message <- function(msg, msg.type="error.msg", scope="ddg.library", doWarn = TRUE) {
  if (doWarn) {
    warning(msg)
  }
  .ddg.data.node("Exception", msg.type, msg, scope)
  .ddg.lastproc2data(msg.type, dscope=scope)
}

# .ddg.lookup.function.name gets the name of the calling function
# and returns it as a string. pname may be passed as a string or
# a name. If NULL, pname is obtained from the calling environment.
# Note that it is important that these be macros, not functions,
# due to the use of the substitute function in the body.

# pname - name of procedure node.

.ddg.lookup.function.name <- gtools::defmacro (pname,
    expr =
        # If pname is not provided, get from function call.
        if (is.null(pname)) {
          
          #print(".ddg.lookup.function.name: sys.calls() =")
          #print(sys.calls())

          # Look up function call.
          call <- sys.call(-4)

          # Discard everything after left parenthesis to get
          # function name.

          # pname <- strsplit (as.character(call), "\\(")[[1]][1]
          #print(paste(".ddg.lookup.function.name: typeof(call[[1]] =", typeof(call[[1]])))
          #print(paste(".ddg.lookup.function.name: str(call[[1]] =", str(call[[1]])))
          # If the call uses a closure rather than a function name, we will
          # call the name FUN.
          if (typeof(call[[1]]) == "closure") {
            #print(".ddg.lookup.function.name:  Found a closure!")
            pname <- "FUN"
          }
          else {
            pname <- as.character(call[[1]])
          }
        }

        # Convert pname to a string if necessary.
        else if (!is.character(pname)) {
          pname <- deparse(substitute(pname))
        }
)

# .ddg.lookup.value is used to determine what value to use when
# creating data nodes.  Note that it is important that these be
# macros, not functions, due to the use of the substitute function
# in the body.

# expr - the expression to be evaluted. This can be a string or
#   a name.
# value - the value that was passed in to the calling function.
#   If value already exists, nothing happens. If value is NULL,
#   the expression is evaluated to determine the value.
# env - the environment in which the evaluation is done.
# procname - the name of the calling procedure, used to produce
#   an error message if necessary.
# warn (optional) - if TRUE, warns user that the expression could
#   not be evaluated.

.ddg.lookup.value <- gtools::defmacro(expr, value, env, procname, warn=TRUE,
    expr =
        if (is.null(value)) {
          arg <- substitute(expr)
          if (is.character(arg)) {
            tryCatch (arg <- parse(text=expr),
            error = function(e) {})
          }
          else expr <- deparse(arg)
          value <- tryCatch (
              eval(arg, env),
              error = function(e) {
                # if (is.character(expr)) return (expr)
                if (warn) {
                  error.msg <- paste("Unable to evaluate", expr, "in call to", procname)
                  .ddg.insert.error.message(error.msg)
                }
                return ("")
              }
          )
        }
)

# .ddg.delete.temp deletes any temporary files created during
# the processing of a script. These include the temporary
# history file.

.ddg.delete.temp <- function() {
  # Delete the temporary history file if we made it.
  if (.ddg.is.set('ddg.history.file')) unlink(.ddg.get('ddg.history.file'))

  # Clear the environment.
  .ddg.env <- new.env(parent=emptyenv())
}

# .ddg.create.output.nodes creates output nodes for ddg.function
# and ddg.procedure. Outs values must be passed as strings, not
# names, unless the value is a file name.

# fname - the name of the function calling .ddg.create.output.nodes.
# pname - the name of the procedure node.
# outs.graphic - the name of a snapshot node to be used as a
#    file name.  A graphical snapshot is simply a captured image
#    of the graphic device active at the time of the call to
#    ddg.function or ddg.procedure.
# outs.data - a list of names of data nodes.
# outs.exception - a list of names of exception nodes.
# outs.url - a list of names of url nodes.
# outs.file - a list of names of file nodes. Supported file
#   extensions include: .csv, .jpg, .jpeg, .pdf, and .txt.
# graphic.fext - the file extension to be used when saving the
#   captured graphic. Supported extensions are .jpg, .jpeg, .pdf.

.ddg.create.output.nodes<- function(fname, pname, outs.graphic, outs.data, outs.exception, outs.url, outs.file, graphic.fext, env) {

  # Capture graphics device.
  if (is.character(outs.graphic)) {
    name <- outs.graphic
    gfext <- as.character(graphic.fext)
    .ddg.write.graphic(name,"Graphical Plot. Not saved in script.",fext=gfext) # value is ignored
    .ddg.proc2data(pname,name)
  }

  # Create output nodes and edges if outs list provided.

  # Exception node.
  if (!is.null(outs.exception)) {
    stack <- sys.calls()
    # Get scope.
    # scope <- .ddg.get.scope(outs.exception[[1]])

    lapply(outs.exception,
        function(param) {
          # Get value in calling environment.
          name <- param
          value <- NULL
          .ddg.lookup.value(name, value, env, fname, warn=FALSE)

          # Exception node.
          scope <- .ddg.get.scope(param, calls = stack)
          .ddg.data.node("Exception", name, value, scope)
          .ddg.proc2data(pname, name, scope)
        }
    )
  }

  # URL node.
  if (!is.null(outs.url)) {
    stack <- sys.calls()
    # Get scope.
    # scope <- .ddg.get.scope(outs.url[[1]])

    lapply(outs.url,
        function(param) {
          # Get value in calling environment.
          name <- param
          value <- NULL
          .ddg.lookup.value(name, value, env, fname, warn=FALSE)

          # URL node.
          scope <- .ddg.get.scope(param, calls=stack)
          .ddg.data.node("URL", name, value, scope)
          .ddg.proc2data(pname, name, scope)
        }
    )
  }

  # Generalized data node (includes simple data values as well as
  # snapshots)
  if (!is.null(outs.data)) {
    # Get scope.
    # scope <- .ddg.get.scope(outs.data[[1]])
    stack <- sys.calls()
    lapply(outs.data,
        function(param) {
          # Get value in calling environment.
          name <- param
          value <- NULL
          .ddg.lookup.value(name, value, env, fname, warn=FALSE)

          tryCatch({
                if (!is.character(name)) name <- deparse(substitute(name))
                envName <- environmentName(env)
                scope <- .ddg.get.scope(param, calls=stack)
                .ddg.save.data(name, value, fname, error=TRUE, scope=scope)
                .ddg.proc2data(pname, name, scope)
              }, error = function(e) {
                .ddg.insert.error.message(e)
              }
          )
        }
    )
  }

  # File node.
  if (!is.null(outs.file)) {
    # Get scope.
    # scope <- .ddg.get.scope(outs.file[[1]])
    stack <- sys.calls()

    lapply(outs.file,
        function(param) {
          # Get value in calling environment.
          name <- param
          value <- NULL
          .ddg.lookup.value(name, value, env, fname, warn=FALSE)
          scope <- .ddg.get.scope(param, calls=stack)

          if (value == "") {
            # Filename passed as value.
            .ddg.file.copy("File", name, name, scope)
            .ddg.proc2data(pname, name, scope)
          }
          else {
            # Filename passed as name.
            .ddg.file.copy("File", value, name, scope)
            .ddg.proc2data(pname, name, scope)
          }
        }
    )
  }
}

# .ddg.create.function.nodes creates the procedure node, input
# binding nodes, and output nodes for the function.

# pname - name of procedure node.
# full.call - full function call.
# outs.data, etc (optional) - output nodes.
# auto.created - TRUE if the function node is created automatically
# when a return is found
# env (optional) - the environment local to the function

.ddg.create.function.nodes <- function(pname, call, full.call, outs.graphic=NULL, outs.data=NULL, outs.exception=NULL, outs.url=NULL, outs.file=NULL, graphic.fext="jpeg", auto.created=FALSE, env=NULL) {
  # Create the start node
  if (typeof(call[[1]]) == "closure") {
    #print(paste(".ddg.create.function.nodes: pname =", pname))
    .ddg.add.abstract.node ("Start", node.name=pname, env=env)
  }
  else {
    #print(paste(".ddg.create.function.nodes: deparse(call) =", deparse(call)))
    .ddg.add.abstract.node ("Start", node.name=paste(deparse(call), collapse=""), env=env)
  }

  # Tokens will contain the function name and the argument
  # expressions.

  # Get parameters and create edges.
  if (length(full.call) > 1) {
    # args contains the names of the variable that was passed into
    # the function.
    args <- full.call[2:length(full.call)]

    # param,names contains the names of the parameters (this is
    # what the variable is known as inside the function).
    #print(paste(".ddg.create.function.nodes: full.call =", full.call))
    param.names <- names(full.call)
    param.names <- param.names[2:length(param.names)]
    stack <- sys.calls()
    # scope <- .ddg.get.scope(args[[1]], for.caller = TRUE)
    bindings <- list()
    for (i in 1:length(args)) bindings[[i]] <-list(args[[i]], param.names[[i]])
    missing.params <- character()

    lapply(bindings,
        function(binding) {
          # Here, arg is the arguments passed IN.
          #print(paste(".ddg.create.function.nodes: binding =", binding))
          arg <- binding[[1]]
          #print(paste(".ddg.create.function.nodes: arg =", arg))

          # formal is the paramenter name of the function (what
          # is the variable known as inside?).
          formal <- binding[[2]][[1]]
          if (is.null(formal) || formal == "") formal <- "..."

          # Find all the variables used in this parameter.
          # If the argument is a string constant, don't bother
          # looking for variables.  Also add quotes around it
          # in the node name.
          if (is.character(arg)) {
            vars.used <- character()
            binding.node.name <- paste(formal, " <- \"", arg, "\"", sep="")
          }
          else {
            vars.used <- .ddg.find.var.uses(arg)
            binding.node.name <- paste(formal, " <- ", paste(deparse(arg), collapse=" "))
            #print(paste(".ddg.create.function.nodes: binding.node.name =", binding.node.name))
          }

          .ddg.proc.node("Binding", binding.node.name, env=env)
          .ddg.proc2proc()
          for (var in vars.used) {
            param.scope <- .ddg.get.scope(var, for.caller = TRUE, calls=stack)
            if (.ddg.data.node.exists(var, param.scope)) {
              .ddg.data2proc(as.character(var), param.scope, binding.node.name)
              if (.ddg.debug.lib()) print(paste("param:", var))
            }
          }
          if (formal != "...") {
            formal.scope <- .ddg.get.scope(formal, calls=stack)
            formal.env <- .ddg.get.env(formal, calls=stack)

            # If we can evaluate the argument without an error, we
            # record the value. If an error occurs, we do not record
            # the value as it's possible that the function never
            # actually uses it.
            tryCatch ({
                  .ddg.save.data(formal, eval(parse(text=formal), formal.env), fname=".ddg.save.data", scope=formal.scope, stack=stack)
                  .ddg.proc2data(binding.node.name, formal, formal.scope)},
                error = function(e) {})

          }
        })
  }
  #print (".ddg.create.function.nodes creating Operation node")
  .ddg.proc.node("Operation", pname, pname, auto.created = auto.created, env=env)

  # Link to the definition of the function if the function is defined in this script.
  if (.ddg.data.node.exists(pname, environmentName(.GlobalEnv))) {
    .ddg.data2proc(pname, environmentName(.GlobalEnv), pname)
  }

  if (length(full.call) > 1) {
    lapply(bindings, function(binding) {
          formal <- binding[[2]][[1]]

          # Formal will be NULL if declared as ...  Don't create the data node in that case.
          if (!is.null(formal) && formal != "") {
            formal.scope <- .ddg.get.scope(formal, calls=stack)
            if (.ddg.data.node.exists (formal, formal.scope)) {
              .ddg.data2proc(formal, formal.scope, pname)
            }
          }
        })
  }

  # Create control flow edge from preceding procedure node.
  .ddg.proc2proc()

  # create output nodes

  .ddg.create.output.nodes(fname="ddg.function", pname, outs.graphic, outs.data, outs.exception, outs.url, outs.file, graphic.fext, parent.frame(2))

}

# .ddg.get.frame.number gets the frame number of the closest
# non-library calling function.

# calls - system calls.
# for.caller (optional) - if TRUE, go up one level before searching.

.ddg.get.frame.number <- function(calls, for.caller=FALSE) {
  #print (".ddg.get.frame.number: for.caller =", for.caller)
  if (is.null(calls)) calls <- sys.calls()
  script.func.found <- FALSE
  nframe <- length(calls)
  for (i in nframe:1) {
    call <- sys.call(i)[[1]]
    # Guess that if we have a closure it is a user-defined function and not a ddg function
    # Is this a good assumption ????
    if (typeof(call) == "closure") {
      if (for.caller && !script.func.found) {
        script.func.found <- TRUE
      }
      else {
        return(i)
      }
    }
    else {
      call.func <- as.character(call)
      #print(paste(".ddg.get.frame.number: call.func =", call.func))
      # Ignore calls to ddg functions or to the functions that get called from the outermost tryCatch
      # to ddg code.
      if (substr(call.func, 1, 4) != ".ddg" && substr(call.func, 1, 3) != "ddg"
          && substr(call.func, 1, 10) != "doTryCatch" && substr(call.func, 1, 11) != "tryCatchOne"
          && substr(call.func, 1, 12) != "tryCatchList" && substr(call.func, 1, 8) != "tryCatch") {
        if (for.caller && !script.func.found) {
          script.func.found <- TRUE
        }
        else {
          return(i)
        }
      }
    }
  }
  return(0)
}


# .ddg.where looks up the environment for the variable specified
# by name.  Adapted from Hadley Wickham, Advanced R programming.

# name - name of variable.
# env (optional) - environment in which to look for variable.
# warning (optional) - set to TRUE if a warning should be thrown when a variable is not found.

.ddg.where <- function( name , env = parent.frame() , warning = TRUE )
{
  stopifnot(is.character(name), length(name) == 1)

  if (identical(env, emptyenv()))
  {
    if(warning)
      warning("Can't find ", name)

    return("undefined")
  }
  if (exists(name, env, inherits=FALSE))
  {
    env
  }
  else
  {
    .ddg.where(name, parent.env(env), warning)
  }
}


#.ddg.get.env gets the environment in which name is declared.

# name - variable name.
# for.caller (optional) - if TRUE, go up one level before searching.
# calls (optional) - system calls.

.ddg.get.env <- function(name, for.caller=FALSE, calls=NULL) {
  #print (paste(".ddg.get.env: for.caller =", for.caller))
  if (is.null(calls)) calls <- sys.calls()
  #print(".ddg.get.env getting the frame number")
  fnum <- .ddg.get.frame.number(calls, for.caller)
  #print(paste(".ddg.get.env: fnum =", fnum))
  stopifnot(!is.null(fnum))

  # This statement was broken into two statements so that we
  # can add print statements to .ddg.where or step through it
  # with a debugger without breaking it.  If we don't do that
  # the print output gets captured by capture.output and
  # does not display to the user and also causes the subsequent
  # grepl call in this function to fail.

  # scope <- sub('<environment: (.*)>', '\\1', capture.output(.ddg.where(name, sys.frame(fnum))))
  tryCatch (
    if(!exists(name, sys.frame(fnum), inherits=TRUE)) return(NULL),
    error = function(e) {}
  )
  #print(".ddg.get.env calling .ddg.where")
  env <- .ddg.where(name, sys.frame(fnum))
  #print(".ddg.get.env Done")
  return(env)
}

# .ddg.get.scope gets the id of the closest non-library
# environment.

# name - name of variable.
# for.caller (optional) - if TRUE, go up one level before searching.
# calls (optional) - system calls.
# env (optional) - the environment to get the scope for

.ddg.get.scope <- function(name, for.caller=FALSE, calls=NULL, env=NULL) {
  # Get the environment for the variable call.
  if (is.null(env)) {
    #print (".ddg.get.scope getting the environment")
    env <- .ddg.get.env(name, for.caller, calls)
    #print (".ddg.get.scope getting the environment got env")
  }

  # If no environment found, name does not exist, so scope is
  # undefined.
  if (is.null(env)) return ("undefined")

  #
  scope <- sub('^<environment: (.*)>$', '\\1', capture.output(env)[1])
  if (grepl("undefined", scope)) scope <- "undefined"
  return(scope)
}

# .ddg.is.local returns TRUE if the specified name is local in the
# specified scope.

# name of variable.
# scope of variable.

.ddg.is.local <- function(name, scope) {
  return(exists(name, scope, inherits=FALSE))
}

# Creates a start node for the current command if one has not
# been created already.
.ddg.create.start.for.cur.cmd <- function (call, caller.env) {
  if (.ddg.is.set(".ddg.cur.cmd")) {
    # print("In .ddg.create.start.for.cur.cmd")
    .ddg.cur.cmd <- .ddg.get(".ddg.cur.cmd")
    .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
    stack.length <- length(.ddg.cur.cmd.stack)
    if (stack.length >= 1) {
      last.created <- .ddg.cur.cmd.stack[stack.length]
      # Only create a start node for the current command if we have not already
      # created one and the command is more than just the call to this function
      if (last.created[[1]] == "FALSE") {
        if (.ddg.cur.cmd@text != paste(deparse(call), collapse="")) {
          cmd.abbrev <- .ddg.add.abstract.node ("Start", .ddg.cur.cmd, caller.env)
          .ddg.cur.expr.stack <- .ddg.get(".ddg.cur.expr.stack")
          st.type <- .ddg.get.statement.type(.ddg.cur.cmd@parsed[[1]])
          loop.statement <- (st.type == "for" || st.type == "while" || st.type == "repeat")
          control.statement <- loop.statement || st.type == "if"
          .ddg.create.data.use.edges.for.console.cmd(vars.set = data.frame(), .ddg.cur.cmd, 0, for.caller=!control.statement)

          # Add Details Omitted node before annotated loops if needed.
          if (loop.statement && ddg.first.loop() > 1) {
            ddg.details.omitted()
          }

          # Mark the start node as created on the stack.  Mark it even if we did not
          # create the abstract node above, because we will create it below.
          .ddg.set (".ddg.cur.cmd.stack", c(.ddg.cur.cmd.stack[1:stack.length-1], TRUE))
        }
        else {
          .ddg.set (".ddg.cur.cmd.stack", c(.ddg.cur.cmd.stack[1:stack.length-1], "MATCHES_CALL"))
        }
      }
    }
    #print("Done .ddg.create.start.for.cur.cmd")
  }
}

# .ddg.get.last.cmd returns the last command on the stack.

.ddg.get.last.cmd <- function() {
  .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
  stack.length <- length(.ddg.cur.cmd.stack)
  cmd <- .ddg.cur.cmd.stack[stack.length-1][[1]]
}

# .ddg.remove.last.cmd.start.created removes the last command and
# start.created from the stack.

.ddg.remove.last.cmd.start.created <- function () {
  .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
  stack.length <- length(.ddg.cur.cmd.stack)
  #print(paste(".ddg.remove.last.cmd.start.created: Popping from stack:", .ddg.cur.cmd.stack[stack.length-1]))

  if (stack.length == 2) {
    .ddg.set(".ddg.cur.cmd.stack", vector())
  }
  else {
    .ddg.set(".ddg.cur.cmd.stack", .ddg.cur.cmd.stack[1:(stack.length-2)])
  }
}

# .ddg.break.statement creates a procedure node for a break statement in
# a for, repeat, or while statement. It also adds a finish node for the
# if statement (if any) where the break occurs, adds a finish node
# for the for, repeat, or while loop where the break occurs, and adds a
# finish node for the for, repeat, or while statement.

.ddg.break.statement <- function() {
  # Create procedure node for break statement.
  .ddg.proc.node("Operation", "break", "break")
  .ddg.proc2proc()

  # Get last command from stack.
  cmd <- .ddg.get.last.cmd()
  # Get loop type.
  loop.type <- as.character(cmd@parsed[[1]][[1]])

  # Create finish nodes if break occurs in if statement.
  if (loop.type == "if") {
    # Create finish node for if loop.
    ddg.finish("if")
    # Create finish node for if statement.
    .ddg.add.abstract.node("Finish", cmd, parent.frame())

    # Remove last command & start.created from stack.
    .ddg.remove.last.cmd.start.created()
    # Get last command from stack.
    cmd <- .ddg.get.last.cmd()
    # Get loop type.
    loop.type <- as.character(cmd@parsed[[1]][[1]])
  }

  # Create finish node for for, repeat, or while loop.
  loop.name <- paste(loop.type, "loop")
  ddg.finish(loop.name)

  # Create finish node for for, while, or repeat statement.
  .ddg.add.abstract.node("Finish", cmd, parent.frame())

  # Remove last command & start.created from stack.
  .ddg.remove.last.cmd.start.created()
}

# .ddg.next.statement creates a procedure node for a next statement in
# a for, repeat, or while statement. It also adds a finish node for the
# if statement (if any) where the next occurs and adds a finish node for
# the for, while, or repeat loop where the next occurs.

.ddg.next.statement <- function() {
  # Create procedure node for next statement.
  .ddg.proc.node("Operation", "next", "next")
  .ddg.proc2proc()

  # Get last command from stack.
  cmd <- .ddg.get.last.cmd()
  # Get loop type.
  loop.type <- as.character(cmd@parsed[[1]][[1]])

  # Create finish nodes if break occurs in if statement.
  if (loop.type == "if") {
    # Create finish node for if loop.
    ddg.finish("if")
    # Create finish node for if statement.
    .ddg.add.abstract.node("Finish", cmd, parent.frame())

    # Remove last command & start.created from stack.
    .ddg.remove.last.cmd.start.created()
    # Get last command from stack.
    cmd <- .ddg.get.last.cmd()
    # Get loop type.
    loop.type <- as.character(cmd@parsed[[1]][[1]])
  }

  # Create finish node for for, repeat, or while loop.
  loop.name <- paste(loop.type, "loop")
  ddg.finish(loop.name)
}

# .ddg.markdown takes a Rmd file and extracts the R code and text through
# the purl function in the knitr library. It then annotates the R script
# to insert start and finish nodes based on the chunks the user already
# created. If eval = false, then the chunk will not be added to the DDG. If
# the user has a name for the chunk, then that name will be used, else a chunk
# name "ddg.chunk_1" and higher numbers will be generated.
#
# Important: If in a code chunk, there is an empty line followed by "# ----"
# or "#'", then an extra finish node will be inserted, causing an error.
#
# r.script.path is the path of the original Rmd file
# output.path is the path of the generated R script

.ddg.markdown <- function(r.script.path = NULL, output.path = NULL){

  #generates R script file from markdown file
  knitr::purl(r.script.path, documentation = 2L, quiet = TRUE)

  #moves file to ddg directory
  file.rename(from = paste(getwd(), "/", basename(tools::file_path_sans_ext(r.script.path)), ".R", sep = ""), to = output.path)
  script <- readLines(output.path)

  skip <- FALSE
  name <- "ddg.chunk"
  annotated <- character(0)
  index <- 1


  # This for loop goes through the script line by line and searches for patterns
  # to insert the start and finish nodes
  for(i in 1:length(script)){

    #eval = false means we skip this code chunk, therefore skip = TRUE
    if(regexpr("eval+(\\s*)+=+(\\s*)+FALSE", script[i]) != -1){
      skip <- TRUE
      annotated <- append(annotated, script[i])
    }

    else if(regexpr("## ----", script[i]) != -1){

      #if no options in the line, then generate default name.
      if(regexpr("## -----", script[i]) == -1){
        if(regexpr("=", script[i]) == -1){
          end <- regexpr("-----", script[i])
          name <- substring(script[i], 8, last = end -1)
        }
        else if(regexpr(",", script[i]) != -1){
          comma <- regexpr(",", script[i])
          name <- substring(script[i], 8, last = comma -1)
        }
        else{
          name <- paste("ddg.chunk_", index, sep = "")
          index <- index + 1
        }
      }
      else{
        name <- paste("ddg.chunk_", index, sep = "")
        index <- index + 1
      }
      name <- stringr::str_trim(name, side = "both")
      annotated <- append(annotated, paste("ddg.start(\"", name, "\")", sep = ""))
    }
    else if(nchar(script[i]) == 0 && (regexpr("#'", script[i + 1]) != -1 ||
                                      i == length(script) || regexpr("## ----", script[i + 1]) != -1 )){
      if(skip){
        annotated <- append(annotated, script[i])
        skip <- FALSE
      }
      else{
        annotated <- append(annotated, paste("ddg.finish(\"", name, "\")", sep = ""))
      }
    }
    else{
      annotated <- append(annotated, script[i])
    }
  }
  writeLines(annotated, output.path)
  r.script.path
}

# .ddg.save.debug.files saves debug files to the debug directory.

.ddg.save.debug.files <- function() {
  # Save initial environment table to file.
  fileout <- paste(.ddg.path.debug(), "/initial-environment.csv", sep="")
  ddg.initial.env <- .ddg.initial.env()
  write.csv(ddg.initial.env, fileout, row.names=FALSE)

  # Save procedure nodes table to file.
  fileout <- paste(.ddg.path.debug(), "/procedure-nodes.csv", sep="")
  ddg.proc.nodes <- .ddg.proc.nodes()
  ddg.proc.nodes <- ddg.proc.nodes[ddg.proc.nodes$ddg.num > 0, ]
  write.csv(ddg.proc.nodes, fileout, row.names=FALSE)

  # Save data nodes table to file.
  fileout <- paste(.ddg.path.debug(), "/data-nodes.csv", sep="")
  ddg.data.nodes <- .ddg.data.nodes()
  ddg.data.nodes2 <- ddg.data.nodes[ddg.data.nodes$ddg.num > 0, ]
  write.csv(ddg.data.nodes2, fileout, row.names=FALSE)

  # Save edges table to file.
  fileout <- paste(.ddg.path.debug(), "/edges.csv", sep="")
  ddg.edges <- .ddg.edges()
  ddg.edges2 <- ddg.edges[ddg.edges$ddg.num > 0, ]
  write.csv(ddg.edges2, fileout, row.names=FALSE)

  # Save function return table to file.
  fileout <- paste(.ddg.path.debug(), "/function-returns.csv", sep="")
  ddg.returns <- .ddg.get(".ddg.return.values")
  ddg.returns2 <- ddg.returns[ddg.returns$return.node.id > 0, ]
  write.csv(ddg.returns2, fileout, row.names=FALSE)

  # Save if script is sourced.
  if (.ddg.is.sourced()) {
    # Save sourced script table to file.
    fileout <- paste(.ddg.path.debug(), "/sourced-scripts.csv", sep="")
    ddg.sourced.scripts <- .ddg.get(".ddg.sourced.scripts")
    ddg.sourced.scripts2 <- ddg.sourced.scripts[ddg.sourced.scripts$snum >= 0, ]
    write.csv(ddg.sourced.scripts2, fileout, row.names=FALSE)

    # Save data object table to file.
    fileout <- paste(.ddg.path.debug(), "/data-objects.csv", sep="")
    ddg.data.objects <- .ddg.data.objects()
    write.csv(ddg.data.objects, fileout, row.names=FALSE)
  }
}

#--------------------USER FUNCTIONS-----------------------#

# ddg.function creates a procedure node of type Operation for
# procedures implemented as functions in the original R script.
# The function name and input parameters are obtained automatically
# from the calling environment. The outs parameters may be used
# optionally to create output data nodes. These MUST be passed as
# a list of strings, not names, unless the value is a file name.
# Users can right-click on the procedure node in DDG Explorer to
# see the code for the function in the original script. For more
# details on outs parameters, see .ddg.create.output.nodes.

# outs (optional) - a list of names of data nodes to be created as
#    outputs to this procedure node. These MUST be passed as
#    a list of strings, not names, unless the value is a file name.
# graphic.fext (optional) - the file extension for a graphics file.

ddg.function <- function(outs.graphic=NULL, outs.data=NULL, outs.exception=NULL, outs.url=NULL, outs.file=NULL, graphic.fext="jpeg") {
  #print("In ddg.function")
  if (!.ddg.is.init()) return(invisible())

  .ddg.inc(".ddg.func.depth")
  pname <- NULL
  #print("ddg.function: looking up function name")
  .ddg.lookup.function.name(pname)
  #print(paste("ddg.function: pname =", pname))

  if (interactive() && .ddg.enable.console()) .ddg.console.node()

  # Look up input parameters from calling environment.
  call <- sys.call(-1)

  #tokens <- unlist(strsplit (as.character(call), "[(,)]"))
  # match.call expands any argument names to be the
  # full parameter name
  #print(paste("ddg.function: pname =", pname))
  #print(paste("ddg.function: caller =", sys.call(-2)[[1]]))

  # Try to find the full call so that we can bind the parameters
  # by name in the DDG.  In the case that the function being executed
  # has been passed as a parameter to another function and is being
  # called from the context (for example, with lapply and other higher-order
  # functions), the match.call will fail.  In that case, we will use the
  # call as it appears in side the higher-order function.
  full.call <- tryCatch (match.call(sys.function(-1), call=call),
         error = function(e) call)

  # Create start node for the calling statement if one is not already created.
  #print("ddg.function creating start node")
  .ddg.create.start.for.cur.cmd (call, sys.frame(-1))
  #print ("ddg.function creating other nodes")
  .ddg.create.function.nodes(pname, call, full.call, outs.graphic, outs.data, outs.exception, outs.url, outs.file, graphic.fext,
      env = sys.frame(.ddg.get.frame.number(sys.calls())))
  #print("ddg.function ending")
  invisible()
}

# ddg.procedure creates a procedure node of type Operation for
# procedures not implemented as functions in the original R script.
# For more details on outs parameters, see .ddg.create.output.nodes.

# pname - the label for the node. Can be passed in as a string
#    or as a name.
# ins (optional) - a list of names of data nodes to be linked
#    as inputs to this procedure node. These MUST be passed as
#    as a list of strings, not names, unless the value is a
#    file name.
# outs (optional) - a list of names of data nodes to be created as
#    outputs to this procedure node. These MUST be passed as
#    a list of strings, not names, unless the value is a file name.
# graphic.fext (optional) - file extension for graphics file.
# env (optional) - the environment in which the procedure occurs.
#    It defaults to the global environment.

ddg.procedure <- function(pname, ins=NULL, outs.graphic=NULL, outs.data=NULL, outs.exception=NULL, outs.url=NULL, outs.file=NULL, graphic.fext="jpeg") {

  if (!.ddg.is.init()) return(invisible())

  .ddg.lookup.function.name(pname)

  #print(paste("ddg.procedure: length(pname) =", length(pname)))
  #print(paste("ddg.procedure: pname =", pname))

  .ddg.proc.node("Operation", pname, pname)

  # Create control flow edge from preceding procedure node.
  .ddg.proc2proc()

  # Create the input edges if ins list provided.
  if (!is.null(ins)) {
    # Get scope.  Cannot use lapply because that results in the
    # creation of a new stack, while .ddg.get.scope assumes that
    # it is called.
    # scope <- .ddg.get.scope(ins[[1]], for.caller = TRUE)
    stack <- sys.calls()
    function.scope <- parent.frame()

    lapply(ins,
        function(param) {
          # First see if param is defined in the function where
          # ddg.procedure is called. If that does not find it,
          # look in the scope of the function that calls the
          # function that calls ddg.procedure.  The difference
          # probably depends on whether ddg.procedure is used to
          # annotate a chunk of code where param is really in the
          # local scope but used in the annotated chunk (the first
          # case) or to annotate an entire function and param is
          # an actual funciton parameter (the second case).
          scope <- .ddg.get.scope(param, calls=stack)
          if (.ddg.is.local(param, function.scope)) {
            if (.ddg.data.node.exists(param, scope)) {
              .ddg.data2proc(param, scope, pname)
              if (.ddg.debug.lib()) print(paste("param:", param))
            }
            else {
              error.msg <- paste("No data node found for local", param)
              .ddg.insert.error.message(error.msg)
            }
          }
          else if (scope != "undefined" && .ddg.data.node.exists(param, scope)) {
            .ddg.data2proc(param, scope, pname)
            if (.ddg.debug.lib()) print(paste("param:", param))
          }
          else {
            scope <- .ddg.get.scope(param, for.caller = TRUE, calls=stack)
            if (scope != "undefined" && .ddg.data.node.exists(param, scope)) {
              .ddg.data2proc(param, scope, pname)
              if (.ddg.debug.lib()) print(paste("param:", param))
            }

            else if (.ddg.data.node.exists(param, "undefined")) {
              # This could be the case if the parameter is the name
              # of a file rather than a variable in the program.
              .ddg.data2proc(param, "undefined", pname)
              if (.ddg.debug.lib()) print(paste("param:", param))
            }

            else {
              # Attempt to allow names, not strings. This does not
              # work as written because what we get when we call
              # substitute is the parameter provided by lapply, not
              # the one provided to ddg.procedure.  We will have
              # the same problem dealing with outs.

              # arg <- substitute(param)
              # if (!is.character(arg) && .ddg.data.node.exists(arg)) {
              # .ddg.data2proc(deparse(arg), pname)
              # if (.ddg.debug.lib()) print(paste("param:", deparse(arg)))
              #   else {warning}
              # }
              error.msg <- paste("No data node found for", param)
              .ddg.insert.error.message(error.msg)
            }
          }
        })
  }

  # create output nodes

  .ddg.create.output.nodes(fname="ddg.procedure", pname, outs.graphic, outs.data, outs.exception, outs.url, outs.file, graphic.fext, parent.frame())

  invisible()
}

# .ddg.find.ddg.return.value.caller.frame.number returns the frame
# number of the first caller to ddg.return.value.  If ddg.return.value
# is called recursively, this will give us the position of the
# earliest one called.

.ddg.find.ddg.return.value.caller.frame.number <- function() {
  # Get the stack
  calls <- sys.calls()

  # Find the calls to ddg.return.value
  ddg.funcs <- unlist(lapply (calls, function (call) return (grepl("^ddg|.ddg", deparse(call)[[1]]))))
  calls.to.ddg.return.value <- unlist(lapply(calls, function (call) return(.ddg.is.call.to(call, as.name("ddg.return.value")))))
  non.ddg.calls.to.ddg.return.value <- !(ddg.funcs[1:length(ddg.funcs)-1]) & calls.to.ddg.return.value[2:length(calls.to.ddg.return.value)]
  which.frame <- Position (function (call) return (call), non.ddg.calls.to.ddg.return.value, right=TRUE)

  # Return the frame number of the caller to ddg.return.value
  return (which.frame)
}

# ddg.return.value creates a data node for a function's return value. If
# the function is called from a console command and console mode is
# enabled, a data flow edge will be created linking this node to
# the console command that uses the value. ddg.return.value returns the
# same value as the function (expr) and can be used in place of the
# function's normal return statement(s) if it is the last statement
# in the function.  Otherwise, it should be a parameter to return,
# as in return(ddg.return.value(expr)). If expr is an assignment, nodes
# and edges are created for the assignment.

# expr - the value returned by the function.

ddg.return.value <- function (expr=NULL, cmd.func=NULL) {
  if (!.ddg.is.init()) return(expr)
  
  #print("In ddg.return.value")
  
  dev.file <- NULL
  parsed.stmt <- NULL
  
  # Capture graphics if dev.off is about to be called.
  if (!is.null(cmd.func)) {
    parsed.stmt <- cmd.func()
    if (parsed.stmt@has.dev.off) {
      if (.ddg.is.call.to(parsed.stmt@parsed[[1]], "dev.off") || !.ddg.loop.annotate()) {
        dev.file <- .ddg.capture.graphics(NULL)
        dev.node.name <- paste0("dev.", dev.cur())
      }
    }
  }

  
  # If expr is an assignment, create nodes and edges for the assignment.
  orig.expr <- substitute(expr)
  #print(paste("ddg.return.value: expr =", paste(deparse(orig.expr), collapse="\n")))
  
  frame.num <- .ddg.get.frame.number(sys.calls())
  env <- sys.frame(frame.num)

  orig.return <- paste("return(", deparse(orig.expr), ")", sep="")

  pname <- NULL
  .ddg.lookup.function.name(pname)
  #print(paste("ddg.return.value: pname =", pname))
  
  # If this is a recursive call to ddg.return.value, find
  # the caller of the first ddg.return.value
  if (grepl("(^ddg|.ddg)", pname)) {
    #print("ddg.return.value: Found a recursive call")
    caller.frame <- .ddg.find.ddg.return.value.caller.frame.number ()
    pname <- as.character(sys.call(caller.frame)[[1]])
    #print(paste("ddg.return.value: updated pname =", pname))
  }
  else {
    #print("ddg.return.value: NOT a recursive call")
    caller.frame <- -1
  }

  # Prints the call & arguments.
  # expr forces evaluation of the function early.  I think that
  # causes some examples to work with debugging on but not off.
  # Checking.  (6/26/2015 - Barb).
  # Yes, ReturnTest.R fails on the recursive f5 function
  #print(paste("ddg.return.value:", sys.call(caller.frame))) #, "returns", expr))

  ddg.return.values <- .ddg.get(".ddg.return.values")
  ddg.num.returns <- .ddg.get(".ddg.num.returns")
  if (nrow(ddg.return.values) == ddg.num.returns) {
    size = 100
    new.rows <- data.frame(ddg.call = character(size),
                           line = integer(size),
                           return.used = logical(size),
                           return.node.id = integer(size),
                           stringsAsFactors=FALSE)
    .ddg.add.rows(".ddg.return.values", new.rows)
    ddg.return.values <- .ddg.get(".ddg.return.values")
  }

  # If this is not a recursive call to ddg.return.value and
  # ddg.function was not called, create the function nodes that
  # it would have created.
  call <- sys.call(caller.frame)
  if (!.ddg.proc.node.exists(pname)) {
    #print("ddg.return.value creating function nodes")
    full.call <- match.call(sys.function(caller.frame), call=call)
    .ddg.create.function.nodes(pname, call, full.call, auto.created = TRUE, env = sys.frame(.ddg.get.frame.number(sys.calls()))
    )
  }
  else {
    #print("ddg.return.value decrementing func.depth")
    .ddg.dec (".ddg.func.depth")
  }

  if (is.null(cmd.func)) {
    #print("ddg.return.value constructing DDG statement for the return call")
    return.stmt <- .ddg.construct.DDGStatement (parse(text=orig.return), pos=NA, script.num=NA, breakpoints=NA)
  }
  else {
    #print("ddg.return.value using existing DDG statement for the return call")
    return.stmt <- cmd.func()
    parsed.statement <- return.stmt@parsed
    #print(paste("ddg.return.value: parsed.statement =", deparse(parsed.statement)))
  }
  
  # Create a data node for the return value. We want the scope of
  # the function that called the function that called ddg.return.
  call.text <- gsub(" ", "", deparse(call, nlines=1))
  return.node.name <- paste(call.text, "return")
  return.node.name <- gsub("\"", "\\\\\"", return.node.name)

  #print(paste("ddg.return.value: sys.nframe =", sys.nframe()))
  #print(paste("ddg.return.value: caller.frame =", caller.frame))
  return.node.scope <-
    environmentName (if (sys.nframe() == 2) .GlobalEnv
                     else parent.env(sys.frame(caller.frame)))
  #print(paste("ddg.return.value: return.node.scope =", return.node.scope))
  .ddg.save.data(return.node.name, expr, fname="ddg.return", scope=return.node.scope)
  
  # Create a return proc node

  # Process breakpoint. We stop if there is a breakpoint set on this line or we are single-stepping.
  if (.ddg.is.sourced() & (return.stmt@is.breakpoint | .ddg.get("ddg.break")) & !.ddg.break.ignore()) {
    .ddg.process.breakpoint(return.stmt, inside.function=TRUE)
  }

  caller.env = sys.frame(caller.frame)
  
  # Check if there is a return call within this call to ddg.return.
  if (.ddg.has.call.to(parsed.stmt, "return")) {
    .ddg.proc.node("Operation", return.stmt@abbrev, return.stmt@abbrev, console = TRUE, env=caller.env, cmd=return.stmt)

    # Create control flow edge from preceding procedure node.
    .ddg.proc2proc()
    
    # Create an edge from the return statement to its return value.
    .ddg.proc2data(return.stmt@abbrev, return.node.name, return.node.scope, return.value=TRUE)
    
    if (!is.null(dev.file)) {
      ddg.file.out (dev.file, pname=return.stmt@abbrev)
      
      # Remove the temporary file
      file.remove(dev.file)
      
      # Add an input edge from the current device
      .ddg.data2proc(dev.node.name, NULL, return.stmt@abbrev)
    }
  }
  else {
    .ddg.lastproc2data(return.node.name, dscope=return.node.scope)
  }

  # Update the table.
  ddg.num.returns <- ddg.num.returns + 1
  ddg.return.values$ddg.call[ddg.num.returns] <- call.text
  ddg.return.values$return.used[ddg.num.returns] <- FALSE
  ddg.return.values$return.node.id[ddg.num.returns] <- .ddg.dnum()
  ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
  ddg.return.values$line[ddg.num.returns] <- 
      if (length(ddg.cur.cmd.stack) == 0) NA
      else ddg.cur.cmd.stack[length(ddg.cur.cmd.stack) - 1][[1]]@pos@startLine
  .ddg.set(".ddg.return.values", ddg.return.values)
  .ddg.set(".ddg.num.returns", ddg.num.returns)

  # If it does not have return, then its parameter was a call to ddg.eval
  # and this stuff has been done already.
  if (.ddg.has.call.to(parsed.stmt, "return")) {
    # Create edges from variables used in the return statement
    vars.used <- return.stmt@vars.used
    for (var in vars.used) {
      # Make sure there is a node we could connect to.
      scope <- .ddg.get.scope(var)
      if (.ddg.data.node.exists(var, scope)) {
        .ddg.data2proc(var, scope, return.stmt@abbrev)
      }
    }
  
    for (var in return.stmt@vars.set)
    {
      if (var != "")
      {
        # Create output data node.
        dvalue <- eval(as.symbol(var), envir=env)
  
        # Check for non-local assignment
        if ( .ddg.is.nonlocal.assign(return.stmt@parsed[[1]]) )
        {
          env <- .ddg.where( var, env = parent.env(parent.frame()) , warning = FALSE )
  
          if( identical(env,"undefined") )
            env <- globalenv()
        }
  
        dscope <- .ddg.get.scope(var, env=env)
        .ddg.save.data(var, dvalue, scope=dscope)
  
        # Create an edge from procedure node to data node.
        .ddg.proc2data(return.stmt@abbrev, var, dscope=dscope, return.value=FALSE)
      }
    }
  
  
    # Create nodes and edges dealing with reading and writing files
    .ddg.create.file.read.nodes.and.edges(return.stmt, env)
    .ddg.create.file.write.nodes.and.edges (return.stmt, env)
    
    if (return.stmt@createsGraphics) {
      .ddg.set.graphics.files (return.stmt, env)
    }
  }

  # Create the finish node for the function
  #print("ddg.return.value: creating finish node")
  if (typeof(call[[1]]) == "closure") {
    .ddg.add.abstract.node ("Finish", node.name=pname, env=caller.env)
  }
  else {
    .ddg.add.abstract.node ("Finish", node.name=paste(deparse(call),collapse=""), env=caller.env)
  }

  #print(paste ("ddg.return.value: returning", expr))
  return(expr)
}

# ddg.annotate.inside returns the value of the parameter
# annotate.inside.

.ddg.annotate.inside <- function() {
  return(.ddg.get("ddg.annotate.inside"))
}

# ddg.first.loop returns the value of the parameter first.loop.

ddg.first.loop <- function() {
  return(.ddg.get("ddg.first.loop"))
}

# ddg.max.loops returns the value of the parameter max.loops.

ddg.max.loops <- function() {
  return(.ddg.get("ddg.max.loops"))
}

# ddg.max.snapshot.size returns the value of the parameter
# max.snapshot.size.

ddg.max.snapshot.size <- function() {
  return(.ddg.get("ddg.max.snapshot.size"))
}

# ddg.loop.annotate returns the value of the parameter ddg.loop.annotate.
.ddg.loop.annotate <- function() {
  return(.ddg.get("ddg.loop.annotate"))
}

# ddg.loop.annotate.on turns on loop annotation.
ddg.loop.annotate.on <- function() {
  .ddg.set("ddg.loop.annotate", TRUE)
}

# ddg.loop.annotate.off turns off loop annotation.
ddg.loop.annotate.off <- function() {
  .ddg.set("ddg.loop.annotate", FALSE)
}

.ddg.inside.loop <- function() {
  return (.ddg.get("ddg.inside.loop"))
}

ddg.set.inside.loop <- function() {
  if (!.ddg.is.set("ddg.inside.loop")) {
    .ddg.set("ddg.inside.loop", 0)    
  }
  else {
    .ddg.set("ddg.inside.loop", .ddg.get("ddg.inside.loop") + 1)    
  }
}

ddg.not.inside.loop <- function() {
  .ddg.set("ddg.inside.loop", .ddg.get("ddg.inside.loop") - 1)    
}

# ddg.loop.count returns the current count for the specified loop.

ddg.loop.count <- function(loop.num) {
  ddg.loops <- .ddg.loops()
  return(ddg.loops[[loop.num]])
}

# ddg.loop.count.inc increments the current count for the specified loop
# and returns the incremented value.

ddg.loop.count.inc <- function(loop.num) {
  ddg.loops <- .ddg.loops()
  ddg.loops[[loop.num]] <- ddg.loops[[loop.num]] + 1
  .ddg.set("ddg.loops", ddg.loops)
  return(ddg.loops[[loop.num]])
}

# ddg.reset.loop.count sets the current count for the specified loop
# to zero.

ddg.reset.loop.count <- function(loop.num) {
  ddg.loops <- .ddg.loops()
  ddg.loops[loop.num] <- 0
  .ddg.set("ddg.loops", ddg.loops)
}

# ddg.for.loop inserts a procedure node and a data node in a for loop,
# indicating the value currently assigned to the index variable.

ddg.forloop <- function(index.var) {
  index.name <- as.character(deparse(substitute(index.var)))
  pnode.name <- paste(index.name, "<-", index.var)
  dscope <- .ddg.get.scope(index.name)

  .ddg.proc.node("Operation", pnode.name, pnode.name)
  .ddg.proc2proc()

  .ddg.data.node("Data", index.name, index.var, dscope, from.env=FALSE)
  .ddg.proc2data(pnode.name, index.name)
}

# ddg.details.omitted inserts an operational node called "Details Omitted"
# in cases where not all iterations of a loop are annotated.  This may
# happen if the number of the first loop to be annotaed (first.loop) is
# greater than 1 and/or if the total number of loops to be annotated is
# less than the actual number of iterations.
#
# It also sets a variable to remember that the last construct is incomplete
# so that the right data nodes get created.

ddg.details.omitted <- function() {
  pnode.name <- "Details Omitted"
  .ddg.proc.node("Incomplete", pnode.name, pnode.name)
  .ddg.proc2proc()
  .ddg.set.details.omitted(TRUE)

  if (.ddg.debug.lib()) {
    print("Adding Details Omitted node")
  }
}

# Returns true if we should run the annotated version of a function and
# false if we should run the unannotated version.

ddg.should.run.annotated <- function (func.name) {
  #print("In ddg.should.run.annotated")
  
  # Check if we are in a loop and loop annotations are off
  #print(paste("loop annotate?", .ddg.loop.annotate()))
  #print(paste("inside loop?", .ddg.inside.loop()))
  if (!.ddg.loop.annotate() && .ddg.inside.loop() > 0) return (FALSE)
  
  # Make sure this specific function has not been disabled
  if (!is.null(.ddg.annotate.off()) & func.name %in% .ddg.annotate.off()) return(FALSE)
  
  #print(paste(func.name, "is not in off list"))
  
  # Not annotating functions in general
  # Check if this specific function should be annotated
  if (!is.null(.ddg.annotate.on()) & func.name %in% .ddg.annotate.on()) return(TRUE)
  
  #print(paste(func.name, "is not in on list"))
  
  # If we do not know anything specific about this function, follow the 
  # general rule
  return (.ddg.annotate.inside()) 
}

# ddg.eval evaluates a statement and creates data flow edges from
# variable and function return nodes that are used in the
# statement. If the statement is an assignment statement, it also
# creates a data node for the variable assigned and a corresponding
# data flow edge. If ddg.eval is called from inside a function, cmd.func
# is a function that returns the corresponding DDGStatement object.
# If ddg.eval is called from inside a control block, cmd.func is an
# integer that points to the corresponding DDGStatement object stored
# in the list .ddg.statements.

# statement - the statement to evaluate.

ddg.eval <- function(statement, cmd.func=NULL) {
  #print(paste("ddg.eval: statement =", statement))
  #print(paste("ddg.eval: cmd.func =", cmd.func))

  # Statement at top level.
  if (is.null(cmd.func)) {
    parsed.statement <- parse(text=statement)
    cmd <- NULL
  }

  # Statement inside control block.
  else if (is.numeric(cmd.func)) {
    num <- cmd.func
    cmd <- .ddg.statement(num)
    parsed.statement <- cmd@parsed
    #print("ddg.eval evaluating cmd inside control block")
    #print(paste("ddg.eval:", cmd@text))

  # Statement inside function.
  } else {
    cmd <- cmd.func()
    parsed.statement <- cmd@parsed
    #print("ddg.eval evaluating cmd inside function")
    #print(paste("ddg.eval:", cmd@text))
    # print(paste("pos ="))
    # print(cmd@pos)
  }

  if (.ddg.debug.lib()) print (paste("ddg.eval: statement =", statement))

  frame.num <- .ddg.get.frame.number(sys.calls())
  env <- sys.frame(frame.num)

  if (!.ddg.is.init()) {
    # print ("ddg.eval:  no ddg!")
    return(eval(parsed.statement, env))
  }

  #print (paste("ddg.eval: statement =", statement))
  
  if (interactive() && .ddg.enable.console() && !.ddg.enable.source()) {
    # print("ddg.eval:  Creating console node")
    .ddg.console.node()
  }

  # If break statement, create procedure node and close open start nodes.

  if (!is.null(cmd) && cmd@text == "break") {
    .ddg.break.statement()
  }

  # If next statement, create procedure node and close open start nodes.

  if (!is.null(cmd) && cmd@text == "next") {
    .ddg.next.statement()
  }

  #print(paste("ddg.eval: Passing to .ddg.parse.commands as node.name:", statement))
  #print(paste("ddg.eval: cmd@abbrev =", cmd@abbrev))
  #print(paste("ddg.eval: Calling .ddg.parse.commands with ", deparse(parsed.statement)))
  return.value <- .ddg.parse.commands(parsed.statement, environ=env, run.commands = TRUE, node.name=statement, called.from.ddg.eval=TRUE, cmds=list(cmd))
  # cmd <- .ddg.parse.commands(parsed.statement, environ=env, run.commands = TRUE, node.name=statement, called.from.ddg.eval=TRUE, cmds=list(cmd))

  if (.ddg.get(".ddg.func.depth")) {
    if (!is.null(cmd)) {
      .ddg.link.function.returns(cmd)
    }
    # .ddg.link.function.returns(statement)
  }

  # Create outflowing edges .
  # .ddg.create.data.set.edges.for.cmd(cmd@vars.set, cmd, 1, env)

  #print(paste("ddg.eval: returning from", deparse(parsed.statement), "with", return.value))
  return (return.value)
}

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


ddg.data <- function(dname, dvalue=NULL, graphic.fext = "jpeg") {
  if (!.ddg.is.init()) return(invisible())

  # Look up the value if one was not provided.
  env <- parent.frame()
  .ddg.lookup.value(dname, dvalue, env, "ddg.data")

  # Save the value appropriately.  If the name is not a string,
  # use the argument instead of the value.
  if (!is.character(dname)) dname <- deparse(substitute(dname))
  .ddg.save.data(dname, dvalue, "ddg.data", graphic.fext, env=env)
}

# ddg.exception creates a data node for an exception.

# dname - the label for the node. This can be passed as a string,
#   name, or expression.
# dvalue (optional) - the value of the node.  If the value is
#   omitted, the argument passed in for dname is evaluated in
#   the calling environment to determine the value.

ddg.exception <- function(dname, dvalue=NULL) {
  if (!.ddg.is.init()) return(invisible())

  # Look up the value if one was not provided.
  env <- parent.frame()
  .ddg.lookup.value(dname, dvalue, env, "ddg.exception")

  if (is.character(dname)) {
    if (exists(dname, env, inherits=TRUE)) {
      dscope = .ddg.get.scope(dname)
    }
    else {
      dscope = environmentName(.GlobalEnv)
    }
  }
  else {
    # If dname is not a string, use its name rather than its value.
    dname <- deparse(substitute(dname))
    dscope <- .ddg.get.scope(dname)
  }

  # Create input exception node.
  .ddg.data.node("Exception", dname, dvalue, dscope)
}

# ddg.url creates a data node for a URL.

# dname - the label for the node.
# dvalue (optional) - the value of the node.  If the value is
#   omitted, the argument passed in for dname is evaluated in
#   the calling environment to determine the value.

ddg.url <- function(dname, dvalue=NULL) {
  if (!.ddg.is.init()) return(invisible())

  # Look up the value if one was not provided.
  env <- parent.frame()
  .ddg.lookup.value(dname, dvalue, env, "ddg.url")

  if (is.character(dname)) {
    dscope = environmentName(.GlobalEnv)
  }
  else {
    # If dname is not a string, use its name rather than its value.
    dname <- deparse(substitute(dname))
    dscope <- .ddg.get.scope(dname)
  }

  # Create input URL node.
  .ddg.data.node("URL", dname, dvalue, dscope)
}

# ddg.file creates a data node of type File by copying an existing
# file to the DDG directory.

# filename - the name of the file to copy, including path to the file
#   if it is not in the working directory.
# dname (optional) - the label for the node. If omitted, the filename,
#   minus the directory path, is used as the label.

ddg.file <- function(filename, dname=NULL) {
  if (!.ddg.is.init()) return(invisible())

  scope <- if (!is.null(dname)) .ddg.get.scope(dname)
       else NULL
  invisible(.ddg.file.copy("File", filename, dname, scope))
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
    argname <- deparse(arg)
    dscope <- .ddg.get.scope(argname)
    if (.ddg.data.node.exists(argname, dscope)) {
      dname <- argname
    }
    else {
      dscope <- .ddg.get.scope(argname, for.caller=TRUE)
      if (.ddg.data.node.exists(argname, dscope)) {
        dname <- argname
      }
      else {
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
    dscope <- .ddg.get.scope(dname)
  }
  else if (exists (arg, envir=parent.frame(2), inherits=TRUE)) {
    dscope <- .ddg.get.scope(dname, for.caller=TRUE)
  }
  else {
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
  .ddg.lookup.value(dname, dvalue, env, "ddg.data.out")

  # Convert name to a string if necessary.
  if (!is.character(dname)) dname <- deparse(substitute(dname))

  # Save the complex data in appropriate format.
  .ddg.save.data(dname, dvalue, "ddg.data.out", graphic.fext, env=env)

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
  .ddg.lookup.value(dname, dvalue, env, "ddg.exception.out")

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
  .ddg.lookup.value(dname, dvalue, env, "ddg.url.out")

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
  
  if (is.null(dname)) {
    dname <- basename(filename)
    scope <- NULL
  }
  else {
    scope <- .ddg.get.scope (dname)
  }
  
  # Create output file node called filename and copy file.
  #print(paste("ddg.file.out copying ", filename))
  saved.file <- .ddg.file.copy("File", filename, dname, scope)
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

# ddg.start creates a procedure node of type Start called pname.
# Users can right-click on a start node in DDG Explorer and see
# the code between start and finish nodes in the original script.

# pname (optional) - the label for the node.  This can be passed as
#   a string or as a name. It can be omitted if ddg.start is called
#   by a function, in which case the name of the function will be
#   used.

ddg.start <- function(pname=NULL) {
  if (!.ddg.is.init()) return(invisible())

  .ddg.lookup.function.name(pname)

  # Check for NULL.
  if(is.null(pname)) {
    msg <- "Cannot call ddg.start with NULL value from top-level."
    .ddg.insert.error.message(msg)
    return
  }

  # Create start node for the calling statement if one is not already created.
  frame.number <- .ddg.get.frame.number(sys.calls())
  env <- sys.frame(frame.number)
  call <- sys.call(frame.number)
  .ddg.create.start.for.cur.cmd (env)

  # Create start non-operational step.
  .ddg.proc.node("Start", pname, pname)

  # Create control flow edge from preceding procedure node.
  .ddg.proc2proc()

}

# ddg.finish creates a procedure node of type Finish called pname.
# Users can right-click on a finish node in DDG Explorer and see
# the code between start and finish nodes in the original script.

# pname (optional) - the label for the node. This can be passed as
#   a string or as a name. It can be omitted if ddg.finish is called
#   by a function, in which case the name of the function will be
#   used.

ddg.finish <- function(pname=NULL) {
  if (!.ddg.is.init()) return(invisible())

  .ddg.lookup.function.name(pname)

  # Check for NULL.
  if(is.null(pname)) {
    msg <- "Cannot call ddg.finish with NULL value from top-level."
    .ddg.insert.error.message(msg)
  }

  # Create finish non-operational step.
  .ddg.proc.node("Finish", pname, pname)

  # Create control flow edge from preceding procedure node.
  .ddg.proc2proc()
  
  # ddg.finish is added to the end of blocks.  We want the block to
  # return the value of the last R statement.
  return(.ddg.get (".ddg.last.R.value"))
}

# ddg.init intializes a new DDG.

# r.script.path (optional) - the full path to the R script file
#  that is being executed. If provided, a copy of the script will
#  be saved with the DDG.
# ddgdir (optional) - the directory where the DDG should be saved.
#   If not provided, the DDG will be saved in a subdirectory called
#   "ddg" in the current working directory.
# enable.console (optional) - if TRUE, console mode is turned on.
# annotate.inside.functions (optional) - if TRUE, functions are annotated.
# first.loop (optional) - the first loop to annotate in a for, while, or
#   repeat statement.
# max.loops (optional) - the maximum number of loops to annotate in a for,
#   while, or repeat statement. If max.loops = -1 there is no limit.
#   If max.loops = 0, no loops are annotated.  If non-zero, if-statements
#   are also annotated.
# max.snapshot.size (optional) - the maximum size for objects that
#   should be output to snapshot files. If 0, no snapshot files are saved.
#   If -1, all snapshot files are saved. Size in kilobytes.  Note that
#   this tests the size of the object that will be turned into a
#   snapshot, not the size of the resulting snapshot.
# Addition : overwrite (optional) - default TRUE, if FALSE, generates
#   timestamp for ddg directory

ddg.init <- function(r.script.path = NULL, ddgdir = NULL, overwrite = TRUE, enable.console = TRUE, annotate.inside.functions = TRUE, first.loop = 1, max.loops = 1, max.snapshot.size = 10) {
  #.ddg.DDGStatement.init()
  .ddg.init.tables()

  # Setting the path for the ddg
  if (is.null(ddgdir)) {

    # Default is the file where the script is located
    if (!is.null(r.script.path)){
      ddg.path <- paste(dirname(r.script.path), "/", basename(tools::file_path_sans_ext(r.script.path)), "_ddg", sep="")
    }
    else {
      ddg.path <- paste(getwd(), "/","ddg",sep = "")
    }
  } else ddg.path <- normalizePath(ddgdir, winslash="/", mustWork=FALSE)

  # Overwrite default is
  if(!overwrite){
    no.overwrite.folder <- paste(ddg.path, "_timestamps", sep = "")
    if(!dir.exists(no.overwrite.folder)){
      dir.create(no.overwrite.folder)
    }
    ddg.path <- paste(no.overwrite.folder, "/",  basename(tools::file_path_sans_ext(r.script.path)), "_ddg_", .ddg.timestamp(), sep = "")
  }

  .ddg.set("ddg.path", ddg.path)

  # Remove files from DDG directory
  ddg.flush.ddg()

  # Create DDG directories
  .ddg.init.environ()

  # Save copy of original script.
  file.copy(r.script.path, paste(.ddg.path.scripts(), "/", basename(r.script.path), sep = ""))

  # Reset r.script.path if RMarkdown file

  if (!is.null(r.script.path) && tools::file_ext(r.script.path) == "Rmd") {
    output.path <- paste(.ddg.path.scripts(), "/", basename(tools::file_path_sans_ext(r.script.path)), ".R", sep = "")
    .ddg.markdown(r.script.path, output.path)
    .ddg.set("ddg.r.script.path", output.path)
  } else {
    .ddg.set("ddg.r.script.path",
             if (is.null(r.script.path)) NULL
             else normalizePath(r.script.path, winslash="/"))
  }

  # Set environment constants.
  .ddg.set(".ddg.enable.console", enable.console)
  .ddg.set(".ddg.func.depth", 0)
  .ddg.set(".ddg.explorer.port", 6096)
  .ddg.set.details.omitted(FALSE)
  # .ddg.init.environ()

  # Initialize the information about the open start-finish blocks
  .ddg.set (".ddg.starts.open", vector())

  # Initialize the stack of commands and environments being executed in active functions
  .ddg.set(".ddg.cur.cmd.stack", vector())
  .ddg.set(".ddg.cur.expr.stack", vector())

  # Mark graph as initilized.
  .ddg.set(".ddg.initialized", TRUE)

  # Store the starting graphics device.
  .ddg.set("prev.device", dev.cur())
  .ddg.set("possible.graphics.files.open", NULL)
  .ddg.set("ddg.open.devices", vector())

  if (interactive() && .ddg.enable.console()) {
    ddg.history.file <- paste(.ddg.path.data(), "/.ddghistory", sep="")
    .ddg.set(".ddg.history.file", ddg.history.file)

    # Empty file if it already exists, do the same with tmp file.
    file.create(ddg.history.file, showWarnings=FALSE)

    # One timestamp keeps track of last ddg.save (the default).
    .ddg.write.timestamp.to.history()

    # Save the history if the platform supports it.
    tryCatch (savehistory(ddg.history.file),
              error = function(e) {})
  }

  # If ddg.detail is not set, use values of annotate.inside, max.loops
  # and max.snapshot.size.
  if (is.null(ddg.get.detail())) {
    # Store value of annotate.inside.
    .ddg.set("ddg.annotate.inside", annotate.inside.functions)

    # Store maximum number of loops to annotate.
    if (max.loops < 0) max.loops <- 10^10
    .ddg.set("ddg.max.loops", max.loops)

    # Store maximum snapshot size.
    .ddg.set("ddg.max.snapshot.size", max.snapshot.size)
  }
  
  # If loops are not annotated, do not annotate functions called from inside a loop.
  if (max.loops == 0) ddg.loop.annotate.off()
  
  # Initialize the counter that keeps track of nested levels
  # of ifs and loops
  ddg.set.inside.loop()

  # Set number of first loop.
  .ddg.set("ddg.first.loop", first.loop)

  .ddg.set(".ddg.proc.start.time", .ddg.elapsed.time())

  # Store time when script begins execution.
  .ddg.set("ddg.start.time", .ddg.timestamp())

  invisible()
}

# ddg.run executes a script (r.script.path) or a function (f).
# If an R error is generated, the error message is captured and
# saved in the DDG. This function includes calls to ddg.init and
# ddg.save, so it is not necessary to call those functions from
# an instrumented script if ddg.run is used. Note that one of f
# and r.script.path must be given; otherwise an error is generated.

# r.script.path (optional) - the full path to the R script.
#   If provided, a copy of the script will be saved with the DDG.
#   If only r.script.path is provided, the script is sourced using
#   ddg.source and a DDG is created for the script.
# ddgdir (optional) - the directory where the DDG will be saved.
#   If not provided, the DDG will be saved in a directory called
#   "ddg" in the current working directory.
# overwrite (optional) - if TRUE, the ddg is overwritten each time
#   the script is executed.
# f (optional) - a function to run. If supplied, the function f
#   is executed with calls to ddg.init and ddg.save so that
#   provenance for the function is captured.
# enable.console (optional) - if TRUE, console mode is turned on.
# annotate.inside.functions (optional) - if TRUE, functions are annotated.
# first.loop (optional) - the first loop to annotate in a for, while, or
#   repeat statement.
# max.loops (optional) - the maximum number of loops to annotate in a for,
#   while, or repeat statement. If max.loops = -1 there is no limit.
#   If max.loops = 0, no loops are annotated.  If non-zero, if-statements
#   are also annotated.
# max.snapshot.size (optional) - the maximum size for objects that
#   should be output to snapshot files. If 0, no snapshot files are
#   saved. If -1, all snapshot files are saved.  Size in kilobytes.
#   Note that this tests the size of the object that will be turned
#   into a snapshot, not the size of the resulting snapshot.
# debug (optional) - If TRUE, enable script debugging. This has the
#   same effect as inserting ddg.breakpoint() at the top of the script.
# save.debug (optional) - If TRUE, save debug files to debug directory.
# save.hashtable (optional) - If TRUE, save ddg information to hashtable.csv.

ddg.run <- function(r.script.path = NULL, ddgdir = NULL, overwrite = TRUE, f = NULL, enable.console = TRUE, annotate.inside.functions = TRUE, first.loop = 1, max.loops = 1, max.snapshot.size = 10, debug = FALSE, save.debug = FALSE, display = FALSE, save.hashtable = TRUE) {

  # Initiate ddg.
  ddg.init(r.script.path, ddgdir, overwrite, enable.console, annotate.inside.functions, first.loop, max.loops, max.snapshot.size)

  # Create ddg directory.
  # dir.create(.ddg.path(), showWarnings = FALSE)

  # Remove existing files if ddg directory different from working
  # directory.
  # ddg.flush.ddg()

  # Set .ddg.is.sourced to TRUE if script provided.
  if (!is.null(r.script.path)) .ddg.set(".ddg.is.sourced", TRUE)

  # Set breakpoint if debug is TRUE.
  if (debug) ddg.breakpoint()

  # Save debug files to debug directory.
  .ddg.set("ddg.save.debug", save.debug)

  # If an R error is generated, get the error message and close
  # the DDG.
  tryCatch(
      if (!is.null(f)) f()
          else if (!is.null(r.script.path)) ddg.source(
               .ddg.get("ddg.r.script.path"),
                ddgdir = ddgdir,
                ignore.ddg.calls = FALSE,
                ignore.init = TRUE,
                force.console = FALSE)
          else stop("r.script.path and f cannot both be NULL"),
      finally={
        ddg.save(r.script.path, save.hashtable = save.hashtable)
        if(display==TRUE){
          ddg.display()
        }
      }
  )

  invisible()
}

# ddg.save inserts attribute information and the number of
# procedure steps at the top of the DDG. It writes the DDG and
# the procedure nodes, data nodes, and function return tables
# to the DDG directory.

# r.script.path (optional) - Path to the R script.
# save.debug (optional) - If TRUE, save debug files to debug directory.
#   Used in console mode.
# quit (optional) - If TRUE, remove all DDG files from memory.
# save.hashtable (optional) - If TRUE, save DDG information to hashtable.csv.
#   Unlike ddg.run, this is set to false as default since it will generally
#   be called internally and by tests, as opposed to by the user.

ddg.save <- function(r.script.path = NULL, save.debug = FALSE, quit = FALSE, save.hashtable = FALSE) {
  if (!.ddg.is.init()) return(invisible())

  if (interactive() && .ddg.enable.console()) {
    # Get the final commands
    .ddg.console.node()
  }

   # If there is a display device open, grab what is on the display
   if (length(dev.list()) >= 1) {
     #print("ddg.save: Saving graphics open at end of script")
    tryCatch (.ddg.capture.graphics(basename(.ddg.get("ddg.r.script.path")), called.from.save = TRUE),
        error = function (e) print(e))
  }

  # Delete temporary files.
  # .ddg.delete.temp()

  # Save ddg.txt to file.
  .ddg.txt.write()
  if (interactive()) print(paste("Saving ddg.txt in ", .ddg.path(), sep=""))

  # Save ddg.json to file.
  .ddg.json.write()
  if (interactive()) print(paste("Saving ddg.json in ", .ddg.path(), sep=""))

  # Save hashtable.csv to file.
  if (save.hashtable) {
    if (interactive()) {
      if (dir.exists(paste0(path.expand("~"),"/.ddg/"))) {
        print("Saving hashtable.csv in .ddg directory.")
      } else {
        print("No .ddg directory found in home directory, saving hashtable.csv in local directory.")
      }
    }
    .ddg.hashtable.write()
  }
  
  # Save sourced scripts (if any). First row is main script.
  ddg.sourced.scripts <- .ddg.get(".ddg.sourced.scripts")
  if (!is.null(ddg.sourced.scripts)) {
    if (nrow(ddg.sourced.scripts) > 1 ) {
      for (i in 1:nrow(ddg.sourced.scripts)) {
        sname <- ddg.sourced.scripts[i, "sname"]
        file.copy(sname, paste(.ddg.path.scripts(), basename(sname), sep="/"))
      }
    }
  }

  # Save debug files to debug directory.
  if (save.debug | .ddg.save.debug()) {
    .ddg.save.debug.files()
  }

  # Clear DDGStatements from ddg environment.
  .ddg.set("ddg.statement.num", 0)
  .ddg.set("ddg.statements", list())

  # Clear loop information from ddg environment.
  .ddg.set("ddg.loop.num", 0)
  .ddg.set("ddg.loops", list())

  # By convention, this is the final call to ddg.save.
  if (quit) {
    # Restore history settings.
    if (.ddg.is.set('ddg.original.hist.size')) Sys.setenv("R_HISTSIZE"=.ddg.get('ddg.original.hist.size'))

    # Delete temporary files.
    .ddg.delete.temp()

    # Capture current graphics device.
    .ddg.auto.graphic.node(dev.to.capture=dev.cur)

    # Shut down the DDG.
    .ddg.clear()
  }

  invisible()
}

# ddg.source reads in an R script and executes it in the provided
# enviroment. ddg.source essentially mimics the behaviour of the
# R source command, having similar input parameters and results,
# but with additional parameters ignore.ddg.calls and ignore.init.

# file - the name of the R script file to source.
# ddgdir (optional) - the directory where the DDG will be saved.
#   If not provided, the DDG will be saved in a directory called "ddg"
#   in the current working directory.
# local (optional) - the environment in which to evaluate parsed
#   expressions. If TRUE, the environment from which ddg.source is
#   called. If FALSE, the user's workspace (global environment).
# echo (optional) - print each expression after parsing.
# print.eval (optional) - print result of each evaluation.
# verbose (optional) - print extra diagnostics.
# max.deparse.length (optional) - maximum number of characters
#   output for deparse of a single expression.
# chdir (optional) - change R working directory temporarily to
#   the directory containing the file to be sourced.
# encoding (optional) - encoding to be assumed when file is a
#   character string.
# ignore.ddg.calls (optional) - if TRUE, ignore DDG function calls.
# ignore.init (optional) - if TRUE, ignore ddg.init and ddg.run.
# force.console (optional) - if TRUE, turn console mode on.

ddg.source <- function (file,  ddgdir = NULL, local = FALSE, echo = verbose, print.eval = echo,
    verbose = getOption("verbose"), max.deparse.length = 150, chdir = FALSE, encoding = getOption("encoding"),
    ignore.ddg.calls = TRUE, ignore.init = ignore.ddg.calls, force.console=ignore.init){

  # Store script number & name.
  snum <- .ddg.next.script.num()
  sname <- basename(file)

  if (snum == 0) {
    df <- data.frame(snum, sname, stringsAsFactors=FALSE)
  } else {
    df<- rbind(.ddg.sourced.scripts(), c(snum, sname))
  }
  .ddg.set(".ddg.sourced.scripts", df)

  # Increment script number.
  .ddg.inc(".ddg.next.script.num")

  ### CODE IN THIS SECTION IS BASICALLY REPLICATION OF source FUNCTION ###

  # Get the environment under which the script should be executed.
  envir <- if (isTRUE(local)) {
        parent.frame()
      }
      else if (identical(local, FALSE)) {
        .GlobalEnv
      }
      else if (is.environment(local)) {
        local
      }
      else stop("'local' must be TRUE, FALSE or an environment")

  # Parse encoding information.
  have_encoding <- !missing(encoding) && encoding != "unknown"
  if (!missing(echo)) {
    if (!is.logical(echo))
      stop("'echo' must be logical")
    if (!echo && verbose) {
      warning("'verbose' is TRUE, 'echo' not; ... coercing 'echo <- TRUE'\n")
      echo <- TRUE
    }
  }

  # Print extra information about environment.
  if (verbose) {
    cat("'envir' chosen:")
    print(envir)
  }

  # Parse input file and figure out encoding.
  ofile <- file
  from_file <- FALSE
  srcfile <- NULL
  if (is.character(file)) {
    if (identical(encoding, "unknown")) {
      enc <- utils::localeToCharset()
      encoding <- enc[length(enc)]
    }
    else enc <- encoding
    if (length(enc) > 1L) {
      encoding <- NA
      owarn <- options("warn")
      options(warn = 2)
      for (e in enc) {
        if (is.na(e))
          next
        zz <- file(file, encoding = e)
        res <- tryCatch(readLines(zz, warn = FALSE),
            error = identity)
        close(zz)
        if (!inherits(res, "error")) {
          encoding <- e
          break
        }
      }
      options(owarn)
    }
    if (is.na(encoding))
      stop("unable to find a plausible encoding")
    if (verbose)
      cat(gettextf("encoding = \"%s\" chosen", encoding),
          "\n", sep = "")
    if (file == "") {
      filename <- "stdin"
      file <- stdin()
      srcfile <- "<stdin>"
    }
    else {
      filename <- file
      file <- file(filename, "r", encoding = encoding)
      on.exit(close(file))
      lines <- readLines(file, warn = FALSE)

      on.exit()
      close(file)
      srcfile <- srcfilecopy(filename, lines, file.info(filename)[1,
              "mtime"], isFile = TRUE)
    }
    loc <- utils::localeToCharset()[1L]
    encoding <- if (have_encoding)
          switch(loc, `UTF-8` = "UTF-8", `ISO8859-1` = "latin1",
              "unknown")
        else "unknown"
  }

  else {
    filename <- "Connection"
    lines <- readLines(file, warn = FALSE)

    srcfile <- srcfilecopy(deparse(substitute(file)), lines)
  }

  # Parse the expressions from the file.
  exprs <- if (!from_file) {
        if (length(lines)) {
          parse(stdin(), n = -1, lines, "?", srcfile,
              encoding, keep.source=TRUE)
        }
        else expression()
      }
      else {
        parse(file, n = -1, NULL, "?", srcfile, encoding, keep.source=TRUE)
      }

  on.exit()

  # Set the working directory for the current script and
  # expressions.
  if (from_file)
    close(file)

  if (verbose)
    cat("--> parsed", "expressions; now eval(.)ing them:\n")
  if (chdir) {
    if (is.character(ofile)) {
      isURL <- length(grep("^(ftp|http|file)://", ofile)) >
          0L
      if (isURL)
        warning("'chdir = TRUE' makes no sense for a URL")
      if (!isURL && (path <- dirname(ofile)) != ".") {
        owd <- getwd()
        if (is.null(owd)) {
          stop("cannot 'chdir' as current directory is unknown")
          on.exit(setwd(owd), add = TRUE)
          setwd(path)
        }
      }
    }
    else {
      warning("'chdir = TRUE' makes no sense for a connection")
    }
  }

  ### END OF MODIFIED source CODE SECTION ###

  # Calculate the regular expressions for what should be ignored
  # and what shouldn't.
  if (ignore.ddg.calls && !ignore.init) {
    if(verbose) warning("'ignore.ddg.calls' is TRUE, 'ignore.int' not; ... coercion 'ignore.init <- TRUE'\n")
    ignore.init <- TRUE
  }

  # Ignore calculation of certain execution steps.
  ignores <- c("^library[(]RDataTracker[)]$",
      if(ignore.ddg.calls) "^ddg."
          else if (ignore.init) c("^ddg.init", "^ddg.run")
          else "a^")

  # Now we can parse the commands as we normally would for a DDG.
  if(length(exprs) > 0) {

    # Turn on the console if forced to, keep track of previous
    # setting, parse previous commands if necessary.
    prev.on <- .ddg.is.init() && .ddg.enable.console()
    if (prev.on && interactive()) .ddg.console.node()
    if (force.console) ddg.console.on()

    # Let library know that we are sourcing a file.
    prev.source <- .ddg.is.init() && .ddg.enable.source()

    # Initialize the tables for ddg.capture.
    .ddg.set("from.source", TRUE)

    # Parse the commands into a console node.
    .ddg.parse.commands(exprs, sname, snum, environ=envir, ignore.patterns=ignores, node.name=sname,
        echo = echo, print.eval = print.eval, max.deparse.length = max.deparse.length,
        run.commands = TRUE)


    # Save the DDG among other things, but don't return any
    # values, TODO - should we do this?
    # ddg.save()
    .ddg.set("from.source", prev.source)

    # Turn return console to previous state.
    if (!prev.on) ddg.console.off() else ddg.console.on()
  }

  invisible()
}

.ddg.start.ddg.explorer <- function () {
  jar.path<- "/RDataTracker/java/DDGExplorer.jar"
  check.library.paths<- file.exists(paste(.libPaths(),jar.path,sep = ""))
  index<- min(which(check.library.paths == TRUE))
  ddgexplorer_path<- paste(.libPaths()[index],jar.path,sep = "")
  ddgtxt.path<- paste(.ddg.path() ,"/ddg.txt",sep = "")

  # -s flag starts DDG Explorer as a server.  This allows each new ddg to show
  # up in a new tab of an existing running DDG Explorer.
  # print("Starting DDG Explorer server")
  systemResult <- system2("java", c("-jar", ddgexplorer_path, ddgtxt.path, "-port", .ddg.get(".ddg.explorer.port")), wait = FALSE)
  # print(paste("Starting java server return code:", systemResult))
}

# ddg.display loads & displays the current DDG.

ddg.display <- function () {

  # See if the server is already running
  # print("Opening socket connection")
  tryCatch ({
        con <- socketConnection(host= "localhost", port = .ddg.get(".ddg.explorer.port"), blocking = FALSE,
            server=FALSE, open="w", timeout=1)
        ddgjson.path<- paste(getwd(), .ddg.path() ,"ddg.json",sep = "/")
        # print ("Socket open; writing to socket")
        writeLines(ddgjson.path, con)
        # print ("Wrote to socket")
        close(con)
      },
      warning = function(e) {
        .ddg.start.ddg.explorer()
      }
  )

  tryCatch(
    if(is.element('CamFlow', installed.packages()[,1])){ # did we install the CamFlow visualiser?
      json <- .ddg.json.current()
      CamFlowVisualiser(json)
    },
    error = function(e) {}
  )

  invisible()
}

# ddg.debug.lib.on turns on debugging of DDG construction.

ddg.debug.lib.on <- function () {
  .ddg.set("ddg.debug.lib", TRUE)
}

# ddg.debug.lib.off turns off debugging of DDG construction.

ddg.debug.lib.off <- function () {
  .ddg.set("ddg.debug.lib", FALSE)
}

# ddg.breakpoint turns on script debugging unless ddg.break.ignore
# is TRUE.

ddg.breakpoint <- function() {
  if (!.ddg.break.ignore()) {
    writeLines("\nEnter = next command, C = next breakpoint, D = display DDG, Q = quit debugging\n")
    .ddg.set("ddg.break", TRUE)
  }
}

# ddg.set.breakpoint sets a breakpoint for the specified script at
# the specified line number.

ddg.set.breakpoint <- function(script.name, line.num) {
  df2 <- data.frame(script.name, line.num)
  colnames(df2) <- c("sname", "lnum")

  if (.ddg.is.set("ddg.breakpoints")) {
    df1 <- ddg.list.breakpoints()
    .ddg.set("ddg.breakpoints", rbind(df1, df2))
  } else {
    .ddg.set("ddg.breakpoints", df2)
  }
}

# ddg.list.breakpoints returns a list of breakpoints set by script
# name and line number.

ddg.list.breakpoints <- function() {
  if (.ddg.is.set("ddg.breakpoints")) return (.ddg.get("ddg.breakpoints"))
  else return(NULL)
}

# ddg.clear.breakpoints removes all breakpoints at specified scripts
# and line numbers.

ddg.clear.breakpoints <- function() {
  .ddg.set("ddg.breakpoints", NULL)
}

# ddg.set.detail sets the level of provenance detail to be collected.
# If ddg.detail is not set, the values of annotate.inside, max.loops,
# and max.snapshot.size passed to ddg.run are used instead.

#   0 = no internal annotation, no snapshots.
#   1 = 1 loop, snapshots < 10k.
#   2 = 10 loops, snapshots < 100k.
#   3 = all loops, all snapshots.

ddg.set.detail <- function(detail.level) {
  if (detail.level == 0) {
    .ddg.set("ddg.annotate.inside", FALSE)
    .ddg.set("ddg.max.loops", 0)
    .ddg.set("ddg.max.snapshot.size", 0)
    .ddg.set("ddg.detail", 0)
  } else if (detail.level == 1) {
    .ddg.set("ddg.annotate.inside", TRUE)
    .ddg.set("ddg.max.loops", 1)
    .ddg.set("ddg.max.snapshot.size", 10)
    .ddg.set("ddg.detail", 1)
  } else if (detail.level == 2) {
    .ddg.set("ddg.annotate.inside", TRUE)
    .ddg.set("ddg.max.loops", 10)
    .ddg.set("ddg.max.snapshot.size", 100)
    .ddg.set("ddg.detail", 2)
  } else if (detail.level == 3) {
    .ddg.set("ddg.annotate.inside", TRUE)
    .ddg.set("ddg.max.loops", 10^10)
    .ddg.set("ddg.max.snapshot.size", -1)
    .ddg.set("ddg.detail", 3)
  } else {
    print("0 = no internal annotation, no snapshots")
    print("1 = 1 loop, snapshots < 10k")
    print("2 = 10 loops, snapshots < 100k")
    print("3 = all loops, all snapshots")
  }
}

# ddg.detail returns the current level of provenance detail.

ddg.get.detail <- function() {
  if (!.ddg.is.set("ddg.detail")) .ddg.set("ddg.detail", NULL)
  return(.ddg.get("ddg.detail"))
}

# ddg.clear.detail clears the current value of provenance detail.

ddg.clear.detail <- function() {
  .ddg.set("ddg.detail", NULL)
}

# ddg.console.off turns off the console mode of DDG construction.

ddg.console.off <- function() {
  if (!.ddg.is.init()) return(invisible())

  # Capture history if console was on up to this point.
  if (interactive() && .ddg.enable.console()) {
    .ddg.console.node()
  }

  # Set the console to off.
  .ddg.set(".ddg.enable.console", FALSE)
}

# ddg.console.on turns on the console mode of DDG construction.

ddg.console.on <- function() {
  if (!.ddg.is.init()) return(invisible())

  # Write a new timestamp if we're turning on the console so
  # we only capture history from this point forward.
  if (!.ddg.enable.console()) .ddg.write.timestamp.to.history()
  .ddg.set(".ddg.enable.console", TRUE)
}

# ddg.annotate.on enables annotation for the specified functions. Functions
# not on this list are not annotated.
# 
# If fnames is NULL, all functions will be annotated

# fnames - a list of one or more function names passed in as strings.

ddg.annotate.on <- function (fnames=NULL){
  if (is.null(fnames)) {
    .ddg.set("ddg.annotate.off", vector())
    .ddg.set("ddg.annotate.inside", TRUE)
    return()
  }

  # Add to the on list
  on.list <- .ddg.get("ddg.annotate.on")
  on.list <- union (on.list, fnames)
  .ddg.set("ddg.annotate.on", on.list)
  
  # Remove from the off list
  off.list <- .ddg.get("ddg.annotate.off")
  off.list <- Filter (function(off) !(off %in% fnames), off.list)
  .ddg.set("ddg.annotate.off", off.list) 

}

# ddg.annotate.off disables annotation for the specified functions.
# Functions not on this list are annotated.
# 
# If fnames is NULL, no functions will be annotated
#
# fnames - a list of one or more function names passed in as strings.

ddg.annotate.off <- function (fnames=NULL) {
  if (is.null(fnames)) {
    .ddg.set("ddg.annotate.on", vector())
    .ddg.set("ddg.annotate.inside", FALSE)
    return()
  }
  
  # Add to the off list
  off.list <- .ddg.get("ddg.annotate.off")
  off.list <- union (off.list, fnames)
  .ddg.set("ddg.annotate.off", off.list)
  
  # Remove from the on list
  on.list <- .ddg.get("ddg.annotate.on")
  on.list <- Filter (function(on) !(on %in% fnames), on.list)
  .ddg.set("ddg.annotate.on", on.list) 
  
}

# ddg.flush.ddg removes all files from the DDG directories unless the
#   the DDG directory is the working directory. If no DDG directory is
#   specified, the current DDG directory is assumed.

# ddg.path (optional) - path to DDG directory.

ddg.flush.ddg <- function(ddg.path=NULL) {
  # Use current DDG directories if no directory is specified.
  if (is.null(ddg.path)) {
    ddg.path <- .ddg.path()
    ddg.path.data <- .ddg.path.data()
    ddg.path.debug <- .ddg.path.debug()
    ddg.path.scripts <- .ddg.path.scripts()
  }

  # Remove files unless the DDG directory is the working directory.
  if (ddg.path != getwd()) {
    unlink(paste(ddg.path, "*.*", sep="/"))
    unlink(paste(ddg.path.data, "*.*", sep="/"))
    unlink(paste(ddg.path.data, ".ddghistory", sep="/"))
    unlink(paste(ddg.path.debug, "*.*", sep="/"))
    unlink(paste(ddg.path.scripts, "*.*", sep="/"))
  }

  invisible()
}

# ddg.checkpoint prompts the user to source DDGCheckpoint.R.

# checkpoint.name (optional) - the value associated with the checkpoint
#   procedure node.

ddg.checkpoint <- function(checkpoint.name=NULL) {
  stop("Call source(DDGCheckpoint.R to load ddg.checkpoint and ddg.restore")
}

# ddg.restore prompts the user to source DDGCheckpoint.R.

# file.path - the name of the checkpoint file to restore.

ddg.restore <- function(file.path) {
  stop("Call source(DDGCheckpoint.R to load ddg.checkpoint and ddg.restore")
}
