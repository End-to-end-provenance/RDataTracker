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

.ddg.append.line <- function(line) {
  script <- .ddg.get("ddg.annotated.script")
  .ddg.set("ddg.annotated.script", append(script, line))
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
          ddg.lnum = character(size),
          stringsAsFactors=FALSE))

  .ddg.set("ddg.data.nodes", data.frame(ddg.type = character(size),
          ddg.num = numeric(size),
          ddg.name = character(size),
          ddg.value = character(size),
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

  # Create annotated script string. This string is written to file
  # when ddg.save is called.
  .ddg.set("ddg.annotated.script", NULL)

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
  # return.used - remembers if this function return value has been
  #   linked to the caller.
  # return.node.id - the id of the data node that holds the return
  #   value.
	.ddg.set(".ddg.return.values",
          data.frame(ddg.call=character(size),
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
  if (dir.exists(.ddg.path())) {
    ddg.flush.ddg()
  } else {
    dir.create(.ddg.path(), showWarnings = FALSE)
  }
  if (interactive() && .ddg.enable.console()) {
    .ddg.set('ddg.original.hist.size', Sys.getenv('R_HISTSIZE'))
    .ddg.set.history()
  }
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
  output <- "\"rdt:InstalledPackages\": [\n\t"
  packagearray <- paste("{\"package\":\"", installed[,1], "\", \"version\":\"",installed[,2], "\"}", sep = "", collapse =",\n\t")
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
    environ <- paste(environ, "ProcessFileTimestamp=\"", .ddg.format.time(file.info(ddg.r.script.path)$mtime), "\"\n", sep="")
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
  ddg.path <- .ddg.path()
  fileout <- paste(ddg.path, "/ddg.txt", sep="")
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
    script.timestamp <- .ddg.format.time(file.info(ddg.r.script.path)$mtime)
  } else {
    script <- ""
    script.timestamp <- ""
  }

  environ <- paste(environ, .ddg.json.nv("rdt:script", ddg.r.script.path), sep="")

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

.ddg.json.procedure.node <- function(id, pname, ptype, ptime, snum, lnum) {

  jstr <- paste("\n\"p", id, "\" : {\n\"rdt:name\" : \"", pname, "\",\n\"rdt:type\" : \"", ptype, "\",\n\"rdt:elapsedTime\" : \"", ptime, "\",\n\"rdt:scriptNum\" : \"", snum, "\",\n\"rdt:scriptLine\" : \"", lnum, "\"\n}", sep="")

  .ddg.append.activity(jstr)
}

# .ddg.json.data.node adds a data node to the ddg.json string.

.ddg.json.data.node <- function(id, dname, dvalue, dtype, dscope, from.env, dtime, dloc) {

  jstr <- paste("\n\"d", id, "\" : {\n\"rdt:name\" : \"", dname, "\",\n\"rdt:value\" : \"", dvalue, "\",\n\"rdt:type\" : \"", dtype, "\",\n\"rdt:scope\" : \"", dscope, "\",\n\"rdt:fromEnv\" : \"", from.env, "\",\n\"rdt:timestamp\" : \"", dtime, "\",\n\"rdt:location\" : \"", dloc, "\"\n}", sep="")

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
  ddg.path <- .ddg.path()
  fileout <- paste(ddg.path, "/ddg.json", sep="")
  #if (interactive()) print(paste("Saving DDG in ", fileout))
  ddg.json <- .ddg.json.current()
  write(ddg.json, fileout)
}

# .ddg.output.procedure.node outputs a procedure node.

.ddg.output.procedure.node <- function(ptype, pname, pvalue, auto.created, ptime, snum, lnum) {
  # Get counter
  ddg.pnum <- .ddg.get("ddg.pnum")

  # Prepare values
  pname <- gsub("\\\"", "\\\\\"", pname)

  if (pvalue !="") {
    pvalue <- gsub("\\\"", "\\\\\"", pvalue)
    value.str <- paste(" Value=\"", pvalue, "\"", sep="")
  } else value.str <- ""

  dtxt <- paste(ptype, " p", ddg.pnum, " \"", ddg.pnum, "-", pname, "\"", value.str, " Time=\"", ptime , "\" Script=\"", snum, "\"", " Line=\"", lnum, "\";\n", sep="")

  # Record in ddg.txt
  .ddg.append(dtxt)

  # Record in ddg.increment
  .ddg.append.inc(dtxt)

  # Record in ddg.json
  .ddg.json.procedure.node(ddg.pnum, pname, ptype, ptime, snum, lnum)
}

# .ddg.output.data.node outputs a data node.

.ddg.output.data.node <- function(dtype, dname, dvalue, dscope, from.env, dtime, dloc) {
  # Get counter
  ddg.dnum <- .ddg.get("ddg.dnum")

  # Prepare values
  if (from.env) dname <- paste(dname, " [ENV]", sep="")

  if (dvalue != "") value.str <- paste(" Value=\"", dvalue, "\"", sep="")
  else value.str <- ""

  if (dtime != "") time.str <- paste(" Time=\"", dtime, "\"", sep="")
  else time.str <- ""

  if (dloc != "") loc.str <- paste(" Location=\"", dloc, "\"", sep="")
  else loc.str <- ""

  dtxt <- paste(dtype, " d", ddg.dnum, " \"", ddg.dnum, "-", dname, "\"", value.str, time.str, loc.str, ";\n", sep="")

  # Record in ddg.txt
  .ddg.append(dtxt)

  # Record in ddg.increment
  .ddg.append.inc(dtxt)

  # Record in ddg.json
  .ddg.json.data.node(ddg.dnum, dname, dvalue, dtype, dscope, from.env, dtime="", dloc="")
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

  # Get input line numbers
  dinv$line.in <- 0
  for (i in 1:nrow(dinv)) {
    dnode <- paste("d", i, sep="")
    index <- which(edges$ddg.to == dnode)
    lines <- NULL
    if (length(index) > 0 ) {
      for (j in 1:length(index)) {
        pnode <- edges$ddg.from[index[j]]
        pnum <- substr(pnode, 2, nchar(pnode))
        snum <- pnodes$ddg.snum[pnodes$ddg.num == pnum]
        lnum <- pnodes$ddg.lnum[pnodes$ddg.num == pnum]
        if (as.numeric(snum) > 0) line <- paste(snum, ":", lnum, sep="")
        else line <- lnum
        lines <- append(lines, line)
      }
      lines <- unique(lines)
      lines <- paste(lines, collapse=" ")
    } else {
      lines <- "NA"
    }
    dinv$line.in[i] <- lines
  }

  # Get output line numbers
  dinv$line.out <- 0
  for (i in 1:nrow(dinv)) {
    dnode <- paste("d", i, sep="")
    index <- which(edges$ddg.from == dnode)
    lines <- NULL
    if (length(index) > 0 ) {
      for (j in 1:length(index)) {
        pnode <- edges$ddg.to[index[j]]
        pnum <- substr(pnode, 2, nchar(pnode))
        snum <- pnodes$ddg.snum[pnodes$ddg.num == pnum]
        lnum <- pnodes$ddg.lnum[pnodes$ddg.num == pnum]
        if (as.numeric(snum) > 0) line <- paste(snum, ":", lnum, sep="")
        else line <- lnum
        lines <- append(lines, line)
      }
      lines <- unique(lines)
      lines <- paste(lines, collapse=" ")
    } else {
      lines <- "NA"
    }
    dinv$line.out[i] <- lines
  }

  # Rename columns
  colnames(dinv) <- c("node", "name", "value", "type", "scope", "line.in", "line.out")

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
  # Save extra long strings as snapshot.
  if (is.character(value) && nchar(value) > 100) {
    .ddg.snapshot.node(name, "txt", value, dscope=scope, from.env=from.env)
  } else {
    # Save the true value.
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
			# .ddg.dec("ddg.dnum")
			.ddg.snapshot.node(name, "jpeg", NULL, dscope=scope, from.env=from.env)
		}, error = function(e) {
			 # warning(paste("Attempted to write", name, "as jpeg snapshot. Failed.", e, "Defaulting to saving RObject and .txt file."))
			# .ddg.dec("ddg.dnum")
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
		# .ddg.dec("ddg.dnum")
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
# lnum - number of line in source code (if available)

.ddg.record.proc <- function(ptype, pname, pvalue, auto.created=FALSE, ptime, snum=NA, lnum=NA) {
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
        ddg.lnum = numeric(size),
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
  ddg.proc.nodes$ddg.lnum[ddg.pnum] <- lnum
  .ddg.set("ddg.proc.nodes", ddg.proc.nodes)

  # Output procedure node.
  .ddg.output.procedure.node(ptype, pname, pvalue, auto.created, ptime, snum, lnum)

  if (.ddg.debug.lib()) {
    print (paste("Adding procedure node", ddg.pnum, "named", pname))
  }
}

# .ddg.record.data records a data node in the data node table.

# dtype - data node type.
# dname - data node name.
# dvalue - data node value.
# dscope - data node scope.
# from.env - if object is from initial environment.
# dtime (optional) - timestamp of original file.
# dloc (optional) -  path and name of original file.

.ddg.record.data <- function(dtype, dname, dvalue, dscope, from.env=FALSE, dtime="", dloc="") {
  # Increment data node counter.
  .ddg.inc("ddg.dnum")
  ddg.dnum <- .ddg.dnum()

  # If the table is full, make it bigger.
  ddg.data.nodes <- .ddg.data.nodes()
  if (nrow(ddg.data.nodes) < ddg.dnum) {
    size = 100
    new.rows <- data.frame(ddg.type = character(size),
        ddg.num = numeric(size),
        ddg.name = character(size),
        ddg.value = character(size),
        ddg.scope = character(size),
        ddg.from.env = logical(size),
        ddg.time = character(size),
        ddg.loc = character(size),
        ddg.current = logical(size), stringsAsFactors=FALSE)
    .ddg.add.rows("ddg.data.nodes", new.rows)
    ddg.data.nodes <- .ddg.data.nodes()
  }

  if (length(dvalue) > 1 || !is.atomic(dvalue)) dvalue2 <- "complex"
  else if (!is.null(dvalue)) dvalue2 <- dvalue
  else dvalue2 <- ""

  ddg.data.nodes$ddg.type[ddg.dnum] <- dtype
  ddg.data.nodes$ddg.num[ddg.dnum] <- ddg.dnum
  ddg.data.nodes$ddg.name[ddg.dnum] <- dname
  ddg.data.nodes$ddg.value[ddg.dnum] <- dvalue2
  ddg.data.nodes$ddg.scope[ddg.dnum] <- dscope
  ddg.data.nodes$ddg.from.env[ddg.dnum] <- from.env
  ddg.data.nodes$ddg.time[ddg.dnum] <- dtime
  ddg.data.nodes$ddg.loc[ddg.dnum] <- dloc
  ddg.data.nodes$ddg.current[ddg.dnum] <- TRUE
  .ddg.set("ddg.data.nodes", ddg.data.nodes)

  # Output data node.
  .ddg.output.data.node(dtype, dname, dvalue2, dscope, from.env, dtime, dloc)

  if (.ddg.debug.lib()) {
    print(paste("Adding data node", ddg.dnum, "named", dname, "with scope", dscope, " and value ", ddg.data.nodes$ddg.value[ddg.dnum]))
  }
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
  #print(sys.calls())
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
  # print (paste (".ddg.data.node.exists: Looking for", dname, "in scope", dscope))
  ddg.data.nodes <- .ddg.data.nodes()
  rows <- nrow(ddg.data.nodes)
  for (i in rows:1) {
    if (ddg.data.nodes$ddg.current[i]) {
      # if (ddg.data.nodes$ddg.name[i] == dname) return (TRUE)
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
      # if (ddg.data.nodes$ddg.name[i] == dname) {
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
  #pn <- .ddg.proc.number(pname, find.unreturned.function=TRUE)
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
      #sys.calls()
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

# .ddg.is.assign returns TRUE if the object passed is an expression
# object containing an assignment statement.

# expr - input expression.

.ddg.is.assign <- function (expr) {
  if (is.call(expr)) {
    # This also finds uses of ->.
    if (identical(expr[[1]], as.name("<-")))
      return (TRUE)

    # This also finds uses of ->>.
    else if (identical(expr[[1]], as.name("<<-")))
      return (TRUE)
    else if (identical(expr[[1]], as.name("=")))
      return (TRUE)
    else if (identical(expr[[1]], as.name("assign")))
      return (TRUE)
  }
  return (FALSE)
}

# .ddg.is.global.assign returns TRUE if the object passed is an
# expression object containing a global assignment.

# expr - input expression.

.ddg.is.global.assign <- function (expr) {
  if (is.call(expr)) {
    # This also finds uses of ->>.
    if (identical(expr[[1]], as.name("<<-")))
      return (TRUE)
  }
  return (FALSE)
}

# .ddg.get.var returns the variable being referenced in an
# expression. It should be passed an expression object that is
# either a variable, a vector access (like a[1]), a list member
# (like a[[i]]) or a data frame access (like a$foo[i]).  For all of
# these examples, it would return "a".

# lvalue - input expression.

.ddg.get.var <- function(lvalue) {
  if (is.symbol(lvalue)) deparse(lvalue)
  else .ddg.get.var(lvalue[[2]])
}

# ddg.is.functiondecl tests to see if an expression is a function
# declaration.

# expr - input expression.

.ddg.is.functiondecl <- function(expr) {
  if (is.symbol(expr) || !is.language(expr)) return (FALSE)
  if (is.null(expr[[1]]) || !is.language(expr[[1]])) return (FALSE)
  return (expr[[1]] == "function")
}

# .ddg.find.assign returns a vector containing the names of all
# the variables assigned in an expression.  The parameter should
# be an expression object. For example, if obj represents the
# expression "a <- (b <- 2) * 3", the vector returned will contain
# both a and b.

# obj - input expression.

.ddg.find.assign <- function(obj) {
  # Base case.
  if (!is.recursive(obj)) return(character())

  # Assignment statement.  Add the variable being assigned to the
  # vector and recurse on the expression being assigned.
  if (.ddg.is.assign(obj)) {
    var <- .ddg.get.var(obj[[2]])

    # Don't look for assignments in the body of a function as those
    # won't happen until the function is called.
    # Don't recurse on NULL.
		if (!(is.null(obj[[3]]))) {
			if (.ddg.is.functiondecl(obj[[3]])) var
			else c(var, unlist(lapply(obj[[3]], .ddg.find.assign)))
		}
		else var
	}

	# Not an assignment statement.  Recurse on the parts of the
  # expression.
  else {
    unique(unlist(lapply(obj, .ddg.find.assign)))
  }
}

# .ddg.find.simple.assign returns the name of the variable assigned
# to if the object passed in is an expression representing an
# assignment statement.  Otherwise, it returns NULL.

# obj - input expression.

.ddg.find.simple.assign <- function(obj) {
  if (.ddg.is.assign(obj)) {
    .ddg.get.var(obj[[2]])
  }
  else {
    NULL
  }
}

# .ddg.find.var.uses returns a vector containing all the variables
# used in an expression.

# main.object - input expression.
# all (optional) - whether to return a vector of unique values
#   (FALSE) or all values (TRUE).

.ddg.find.var.uses <- function(main.object, all=FALSE) {
	# Find function to filter results.
	filter <- if (all) identity else unique

	# Recursive helper function.
	.ddg.find.var.uses.rec <- function(obj) {
		# Base cases.
    if (is.atomic(obj)) {
      return(character())  # A name is not atomic!
    }
		if (is.name(obj)) {
      if (nchar(obj) == 0) return (character())

      # Operators also pass the is.name test.  Make sure that if it is a
      # single character, then it is alpha-numeric.
      if (nchar(obj) == 1 && !grepl("[[:alpha:]]", obj)) return (character())
      #print(paste(".ddg.find.var.uses found", deparse(obj)))
      return (deparse(obj))
    }
		if (!is.recursive(obj)) return(character())
		##
		if (.ddg.is.functiondecl(obj)) return(character())

		tryCatch(
			if (.ddg.is.assign(obj)) {
				# If assigning to a simple variable, recurse on the right
        # hand side of the assignment.
				if (is.symbol(obj[[2]])) {
          filter(unlist(.ddg.find.var.uses.rec(obj[[3]])))
        }
				else if (is.call(obj[[2]])) {
          filter(c (.ddg.find.var.uses.rec(obj[[2]][[2]]), unlist(.ddg.find.var.uses.rec(obj[[3]]))))
        }
				# If assigning to an expression (like a[b]), recurse on the
	      # indexing part of the lvalue as well as on the expression.
				else {
          filter(c (.ddg.find.var.uses.rec(obj[[2]][[3]]), unlist(.ddg.find.var.uses.rec(obj[[3]]))))
        }
			}

			# Not an assignment.  Recurse on all parts of the expression
	    # except the operator.
			else {
				filter(unlist(lapply(obj[1:length(obj)], .ddg.find.var.uses.rec)))
			},
			error = function(e) {
				print (paste(".ddg.find.var.uses.rec:  Error analyzing", deparse(obj)))
				character()
			}
		)
	}

	return(.ddg.find.var.uses.rec(main.object))
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

.ddg.add.to.vars.set <- function(vars.set, cmd.expr, i) {
  # Find out the variable being assigned to by a simple assignment
  # statement.
  main.var.assigned <- .ddg.find.simple.assign(cmd.expr)

  # Find all the variables that may be assigned in the statement.
  vars.assigned <- .ddg.find.assign(cmd.expr)

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

	return(vars.set)
}


# .ddg.find.var.assigments finds the possible variable assignments
# for a fixed set of parsed commands. See .ddg.create.empty.vars.set
# for more information on the structure of the returned data frame.

# parsed.commands - a list of parsed commands.

.ddg.find.var.assignments <- function(parsed.commands) {
  if (length(parsed.commands) == 0) return (data.frame())

  # Make it big so we don't run out of space.
  var.table.size <- length(parsed.commands)
  vars.set <- .ddg.create.empty.vars.set(var.table.size)

  # Build the table recording where variables are assigned to or may
  # be assigned to.
  for ( i in 1:length(parsed.commands)) {
    cmd.expr <- parsed.commands[[i]]
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

.ddg.create.data.use.edges.for.console.cmd <- function (vars.set, cmd, cmd.expr, cmd.pos, for.caller) {
  # Find all the variables used in this command.
  #print (paste(".ddg.create.data.use.edges.for.console.cmd: cmd.expr = ", cmd.expr))
  vars.used <- .ddg.find.var.uses(cmd.expr)

  for (var in vars.used) {
    #print(paste(".ddg.create.data.use.edges.for.console.cmd: var =", var))
    # Make sure there is a node we could connect to.
    scope <- .ddg.get.scope(var, for.caller)
    #print(paste(".ddg.create.data.use.edges.for.console.cmd: scope =", scope))
    if (.ddg.data.node.exists(var, scope)) {
      #print(".ddg.create.data.use.edges.for.console.cmd found data node")
      nRow <- which(vars.set$variable == var)

      # Check if the node is written in the console block.
      if (length(nRow) > 0) {
        first.writer <- min(vars.set$first.writer[nRow], vars.set$possible.first.writer[nRow])
        last.writer <- max(vars.set$last.writer[nRow], vars.set$possible.last.writer[nRow])

        # Draw the edge if we will connect to a node that exists
        # before the console block or to the last writer of this
        # variable within the console block.
				if (cmd.pos <= first.writer || cmd.pos > last.writer) {
          .ddg.data2proc(var, scope, cmd)
          }

				# TODO - add some sort of warning to the user that the node
        # is not being created
      }

			# The variable is not set at all in this console block.
      # Connect to a pre-existing data node.
			else {
        .ddg.data2proc(var, scope, cmd)
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

.ddg.create.data.set.edges.for.cmd <- function(vars.set, cmd.abbrev, cmd.expr, cmd.pos, env, for.finish.node = FALSE, scope=NULL, stack=NULL) {
  vars.assigned <- .ddg.find.assign (cmd.expr)
  for (var in vars.assigned) {

    nRow <- which(vars.set$variable == var)

    # Only create a node edge for the last place that a variable is
    # set within a console block.
		if ((length(nRow) > 0 && vars.set$last.writer[nRow] == cmd.pos && vars.set$possible.last.writer[nRow] <= vars.set$last.writer[nRow]) || for.finish.node) {
		    if (is.null(env)) {
		      env <- .ddg.get.env(var, calls=stack)
        }
		    scope <- .ddg.get.scope(var, calls=stack, env=env)
        #print (paste (".ddg.create.data.set.edges.for.cmd: looking for ", var, "in", environmentName(env)))
		    val <- tryCatch(eval(parse(text=var), env),
					error = function(e) {
            #print (paste (".ddg.create.data.set.edges.for.cmd: looking for ", var, "in", environmentName(parent.env(env))))
            eval (parse(text=var), parent.env(env))
          }
			)
			tryCatch(.ddg.save.data(var, val, fname=".ddg.create.data.set.edges.for.cmd", error=TRUE, scope=scope, stack=stack, env=env),
			         error = function(e){.ddg.data.node("Data", var, "complex", scope)})

#			if (!is.null(val)) {
#				if (is.data.frame(val)) .ddg.snapshot.node(var, "csv", val, dscope=environmentName(.GlobalEnv))
#				else .ddg.data.node("Data", var, val, environmentName(.GlobalEnv))
#			}
#			else {
#				.ddg.data.node("Data", var, "complex", environmentName(.GlobalEnv))
#			}
      .ddg.proc2data(cmd.abbrev, var, scope)
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
  environment <- if (is.environment(env)) env else .GlobalEnv
  for (i in 1:nrow(vars.set)) {
    if (vars.set$possible.last.writer[i] > vars.set$last.writer[i]) {
      value <- tryCatch(eval(parse(text=vars.set$variable[i]), environment),
          error = function(e) {NULL}
      )
# Only create the node and edge if we were successful in
# looking up the value.
      if (!is.null(value)) {
        envName <- environmentName(environment)
        if (envName == "") envName <- .ddg.get.scope(vars.set$variable[i])
        .ddg.data.node("Data", vars.set$variable[i], value, envName)
        .ddg.proc2data(last.command, vars.set$variable[i], envName)
      }
    }
  }
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

  return(find.files.rec(main.object))
}

# Initialize the information about functions that read from files
.ddg.create.file.read.functions.df <- function () {
  # Functions that read files
  function.names <-
    c ("source", "read.csv", "read.csv2", "read.delim", "read.delim2", "read.table", "read.xls", "file")

  # The argument that represents the file name
  param.names <-
    c ("file", "file", "file", "file", "file", "file", "xls", "description")

  # Position of the file parameter if it is passed by position
  param.pos <-
    c (1, 1, 1, 1, 1, 1, 1, 1)

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
.ddg.create.file.read.nodes.and.edges <- function (cmd, cmd.expr, env) {
  # Find all the files potentially read in this command.
  # This may include files that are not actually read if the
  # read are within an if-statement, for example.
  files.read <- .ddg.find.files.read(cmd.expr, env)
  #print ("Files read:")
  #print (files.read)

  for (file in files.read) {
    # Only create the node and edge if there actually is a file
    # Even if the file exists, it is possible that it was not read here
    if (file.exists(file)) {
      # Create the file node and edge
      ddg.file (file)
      ddg.data.in (basename(file), pname=cmd)
    }
    else if (grepl ("^http", file) || grepl ("^ftp", file)) {
      scope <- environmentName(.GlobalEnv)
      .ddg.data.node("URL", file, file, scope)
      .ddg.data2proc(file, scope, cmd)
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
.ddg.create.file.write.nodes.and.edges <- function (cmd, cmd.expr, env) {
  # Find all the files potentially written in this command.
  # This may include files that are not actually written if the
  # write calls are within an if-statement, for example.
  files.written <- .ddg.find.files.written(cmd.expr, env)
  #print ("Files written:")
  #print (files.written)

  for (file in files.written) {
    # Check that the file exists.  If it does, we will assume that
    # it was created by the write call that we just found.
    if (file.exists (file)) {
      # Create the file node and edge
      ddg.file.out (file, pname=cmd)
    }
  }
}

# Initialize the information about functions that initialize graphics devices
.ddg.create.graphics.functions.df <- function () {
  # Functions that read files
  function.names <-
    c ("pdf", "postscript", "bmp", "jpeg", "png", "tiff")

  # The argument that represents the file name
  param.names <-
    c ("file", "file", "filename", "filename", "filename", "filename")

  # Position of the file parameter if it is passed by position
  param.pos <-
    c (1, 1, 1, 1, 1, 1)

  return (data.frame (function.names, param.names, param.pos, stringsAsFactors=FALSE))
}

.ddg.set (".ddg.graphics.functions.df", .ddg.create.graphics.functions.df ())

# Given a parse tree, this function returns a list containing
# the expressions that correspond to the filename argument
# of the calls to functions that create graphics devices.  If there are
# none, it returns NULL.
.ddg.set.graphics.files <- function(main.object, env) {

  # Find all the graphics files that have potentially been opened.
  # Remember these file names until we find the dev.off call and then
  # determine which was written.
  new.possible.graphics.files.open <- .ddg.find.files (main.object, .ddg.get(".ddg.graphics.functions.df"), env)
  if (!is.null(new.possible.graphics.files.open)) {
    if (.ddg.is.set ("possible.graphics.files.open")) {
      possible.graphics.files.open <- .ddg.get ("possible.graphics.files.open")
      .ddg.set ("possible.graphics.files.open",
                c (new.possible.graphics.files.open, possible.graphics.files.open))
    }

    else {
      .ddg.set ("possible.graphics.files.open", new.possible.graphics.files.open)
    }
    #print (paste (".ddg.set.graphics.files: Found ", new.possible.graphics.files.open))

  }
}

.ddg.has.dev.off.call <- function(main.object) {
  #print ("In .ddg.has.dev.off.call")
  #print (main.object)
  # Recursive helper function.
  has.dev.off.call.rec <- function(obj) {
    print (obj)
    # Base cases.
    if (!is.recursive(obj)) {
      #print(".ddg.has.dev.off.call: not recursive")
      return(FALSE)
    }

    if (length(obj) == 0) {
      #print(".ddg.has.dev.off.call: length is 0")
      return(FALSE)
    }
    ## It might be useful to record somehow that this function
    # reads a file, but we wouldn't actually do the reading
    # until the function is called, not here where it is
    # being declared.
    if (.ddg.is.functiondecl(obj)){
      #print(".ddg.has.dev.off.call: function decl")
      return(FALSE)
    }

    if (is.call(obj)) {
      #print("Found call")

      if (is.symbol (obj[[1]])) {
        # Is this a call to dev.off?
        if (as.character(obj[[1]]) == "dev.off") {
          #print (".ddg.has.dev.off.call: Found dev.off")
          return (TRUE)
        }

        else if (length (obj) == 1) {
          #print(".ddg.has.dev.off.call: length of call is 1")
          return (FALSE)
        }

        else {
          #print(paste(".ddg.has.dev.off.call: length of call =", length(obj)))
          return (has.dev.off.call.rec (obj[2:length(obj)]))
        }
      }

      else return (has.dev.off.call.rec (obj[[1]]) || has.dev.off.call.rec (obj[2:length(obj)]))
    }

    #else return (has.dev.off.call.rec (obj[[1]]) || has.dev.off.call.rec (obj[2:length(obj)]))
    return (FALSE)
  }

  # This is a vastly simpler implementation but only
  # approximate.  It is very expensive to search the
  # parse tree with every expression and sometimes
  # even went into infinite recursion.
  #
  # If this ends up causing problems, we could try
  # searching the text for dev.off and if the text is
  # found, then do the recursive search of the parse tree
  # to check that it is a call to dev.off and not some
  # other things accidentally getting caught.
  #
  # Leaving the unused recursive function in in case
  # we need to go back to it later.
  text <- deparse(main.object)

  # Make sure this is not a function declaration
  if (any(grepl("function", text))) return(FALSE)

  # Return true if dev.off appears in the text.
  return (any(grepl("dev.off", text)))
  #return(has.dev.off.call.rec(main.object))
}

.ddg.capture.graphics <- function(cmd) {
  #print(paste(".ddg.capture.graphics: ", cmd))
  if (.ddg.is.set ("possible.graphics.files.open")) {
    possible.graphics.files.open <- .ddg.get ("possible.graphics.files.open")

    # Find the most recent file
    if (!is.null(possible.graphics.files.open)) {
      #print(paste(".ddg.capture.graphics: possible.graphics.files.open =", possible.graphics.files.open))
      #print(".ddg.capture.graphics: getting file info")
      graphics.file.info <- file.info(possible.graphics.files.open)
      #print(".ddg.capture.graphics: getting modification time")
      latest.file.date.row <- which.max (graphics.file.info$mtime)

      #print(".ddg.capture.graphics: creating file node")
      ddg.file.out (possible.graphics.files.open[latest.file.date.row], pname=cmd)
      #.ddg.capture.current.graphics(cmd, possible.graphics.files.open[latest.file.date.row])
      #print(paste(".ddg.capture.graphics: writing to ", possible.graphics.files.open[latest.file.date.row]))
      .ddg.set ("possible.graphics.files.open", NULL)
      return()
    }
  }

  # Output is going to the display, so we need to make up a name
  .ddg.capture.current.graphics(cmd)

}

# Captures what is on the current display to a file, creates a file node
# and connects to the ddg.
.ddg.capture.current.graphics <- function(cmd, file=NULL) {
  if (is.null(file)) {
    file <- paste0("dev.off.", .ddg.dnum()+1, ".pdf")
  }
  #print(paste(".ddg.capture.graphics: writing to ", file))
  dev.print(device=pdf, file=file)
  ddg.file.out (file, pname=cmd)
}

# .ddg.abbrev.cmd abbreviates a command to the specified length.
# Default is 60 characters.

# cmd - command string.
# len (optional) - number of characters.

.ddg.abbrev.cmd <- function(cmd, len=60) {
  if (length(cmd) > 1) {
    cmd <- paste (cmd, collapse = " ")
  }
  if(file.exists(cmd)){
    basename(cmd);
  } else {
    if (nchar(cmd) <= len) cmd
    else if (substr(cmd, len, len) != "\\") substr(cmd, 1, len)
    else if (substr(cmd, len-1, len) == "\\\\") substr(cmd, 1, len)
    else substr(cmd, 1, len-1)
  }
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
	unused.returns <- returns[!returns$return.used & returns$return.node.id > 0, ]
  if (nrow(unused.returns) == 0) return()
  #print (paste(".ddg.link.function.returns: unused.returns:", unused.returns))

	# See which of these are called from the command we are
  # processing now.
	unused.calls <- unused.returns$ddg.call
  command <- gsub(" ", "", command)
	uses <- sapply(unused.calls, function(call) {grepl(call, command, fixed=TRUE)})
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
					print(paste(".ddg.link.function.returns:", command))
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

.ddg.add.abstract.node <- function(type, cmd, env, called=".ddg.parse.commands") {
  cmd.abbrev <- .ddg.abbrev.cmd(cmd)
  if (.ddg.debug.lib()) print(paste(called, ":  Adding", cmd.abbrev,  type, "node"))
  .ddg.proc.node(type, cmd.abbrev, cmd.abbrev, TRUE, env=env)
  .ddg.proc2proc()

  return(cmd.abbrev)
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
		cmd.abbrev <- .ddg.add.abstract.node("Finish", .ddg.last.cmd$abbrev, env, called=paste(called, "-> .ddg.close.last.command.node"))

		# Add link from a function return node if there is one.
		.ddg.link.function.returns(.ddg.last.cmd$text)

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
		.ddg.add.abstract.node("Start", new.command$abbrev, env, called=paste(called, "-> .ddg.open.new.command.node"))

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

.ddg.is.procedure.cmd <- function(cmd.str) {
  return(grepl("^ddg.(procedure|start|finish|restore|checkpoint)", cmd.str))
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

# .ddg.extract.param.from.ddg.eval extracts the parameter from a
# ddg.eval statement.

.ddg.extract.param.from.ddg.eval <- function(cmd.expr) {
  # Get parsed version.
  deparsed.cmd <- deparse(cmd.expr)
  parsed.cmd <- parse(text=deparsed.cmd)
  # Extract parameter.
  param <- parsed.cmd[[1]][[2]]
  param.txt <- deparse(param)
  updated.cmd <- list("expr" = param,
                      "abbrev" = .ddg.abbrev.cmd(gsub("\\\"", "\\\\\"", param.txt)),
                      "text" = param.txt)
  return(updated.cmd)
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

# .ddg.get.source.code.line.numbers reads a script, splits lines
# separated by semicolons, inserts ddg.breakpoint() for breakpoints
# set at the command line, and returns a data frame containing script
# number, source code line number, and parsed command number. If
# breakpoints were set, a modified version of the script is written
# to the ddg directory and sourced by ddg.source.

.ddg.get.source.code.line.numbers <- function(file, snum) {
  # Read source code.
  script.file <- file(file)
  source.code <- readLines(script.file)
  close(script.file)

  # Get file name
  fname <- basename(file)

  # Split lines separated by a semicolon (if any) but retain original
  # line numbers.
  index <- 0
  for (i in 1:length(source.code)) {
    line <- source.code[i]
    # Remove trailing comment, if any.
    if (grepl("#", line)) line <- strsplit(line, "#")[[1]][1]

    if (line == "") {
      index <- index + 1
      if (index == 1) {
        lnum <- i
        scode <- ""
      } else {
        lnum <- append(lnum, i)
        scode <- append(scode, "")
      }
    } else {
      split.line <- strsplit(source.code[i], ";")
      for (j in 1:length(split.line[[1]])) {
        index <- index + 1
        if (index == 1) {
          lnum <- i
          scode <- as.character((split.line[[1]][[j]]))
        } else {
          lnum <- append(lnum, i)
          scode <- append(scode, as.character(split.line[[1]][[j]]))
        }
      }
    }
  }

  # Add breakpoints set at command line (if any) but retain original
  # line numbers.
  breakpoints <- ddg.list.breakpoints()

  if (!is.null(breakpoints)) {
    index <- 0

    for (i in 1:length(scode)) {
      index <- index + 1
      line <- scode[i]

      # Check for set breakpoint.
      set.break <- FALSE
      for (j in 1:nrow(breakpoints)) {
        if (breakpoints$sname[j] == fname & breakpoints$lnum[j] == lnum[i]) {
          set.break <- TRUE
        }
      }

      if (set.break) {
      # Breakpoint set.
        if (index == 1) {
          lnum2 <- lnum[i]
          lnum2 <- append(lnum2, lnum[i])
          scode2 <- "ddg.breakpoint()"
          scode2 <- append(scode2, line)
        } else {
          lnum2 <- append(lnum2, lnum[i])
          lnum2 <- append(lnum2, lnum[i])
          scode2 <- append(scode2, "ddg.breakpoint()")
          scode2 <- append(scode2, line)
        }
      } else {
      # No breakpoint set.
        if (index ==1) {
          lnum2 <- lnum[i]
          scode2 <- line
        } else {
          lnum2 <- append(lnum2, lnum[i])
          scode2 <- append(scode2, line)
        }
      }
    }
    lnum <- lnum2
    scode <- scode2

    # Save modified script in ddg directory.
    out.name <- paste(.ddg.path(), "/script-", snum, ".r", sep="")
    out.file <- file(out.name, "w")
    for (i in 1:length(scode)) {
      writeLines(scode[i], out.file)
    }
    close(out.file)
  }

  # Add parsed command numbers.
  source.line <- ""
  parsed.num <- 0
  pnum <- rep(0, length(lnum))

  for (i in 1:length(lnum)) {
    tryCatch(
      {
        if (source.line == "") source.line <- scode[i]
        else source.line <- paste(source.line, "\n", scode[i])

        # Try to parse line.
        command.line <- parse(text=source.line)
        # Comment or white space.
        if (length(command.line) == 0) {
          pnum[i] <- NA
          source.line <- ""
          # Parsable R command.
        } else {
          parsed.num <- parsed.num + 1
          pnum[i] <- parsed.num
          source.line <- ""
        }
      },
      # Unable to parse.
      error=function(e) {
      }
    )
  }

  # Create data frame.
  df <- data.frame(snum, lnum, pnum)

  # Adjust for blocks.
  for (i in nrow(df):2) {
    if (!is.na(df$pnum[i-1]) & !is.na(df$pnum[i]) & df$pnum[i-1] == 0 & df$pnum[i] > 0) {
      df$pnum[i-1] <- df$pnum[i]
      df$pnum[i] <- NA
    }
  }

  # Remove unnecessary rows.
  index <- which(!is.na(df$pnum))
  df2 <- df[index, ]

  return(df2)
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
  # Abbreviate command.
  command <- substr(command, 1, 60)

  # Display script and line numbers if top-level command.
  if (!inside.function) {
    # Get number of parsed command
    pnum <- .ddg.parsed.num()

    # Get script number and line number for this command.
    source.parsed <- .ddg.source.parsed()
    snum <- source.parsed$snum[pnum]
    lnum <- source.parsed$lnum[pnum]

    if (snum == 0) slnum <- lnum
    else slnum <- paste(snum, ":", lnum, sep="")
    print(paste(slnum,  "  |  ", command, sep=""))

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

    print(paste("[", func.name, "]  |  ", command, sep=""))
  }
  # Save ddg.
  .ddg.txt.write()
  .ddg.json.write()

  # Get user input from the keyboard.
  line <- "D"
  while (line == "D") {
    line <- toupper(readline())
    if (line == "D") .ddg.loadDDG(.ddg.path())
    else if (line == "") {}
    else if (line == "C") .ddg.set("ddg.break", FALSE)
    else if (line == "Q") .ddg.set("ddg.break.ignore", TRUE)
  }
}

# .ddg.parse.commands takes as input a list of R script commands
# and creates DDG nodes for each command. If environ is an
# environment, it executes the commands in that environment
# immediately before creating the respective nodes for that
# command, and then creates the data nodes based on the information
# available in the environment. If environ is not NULL, calls to
# ddg.* are not executed so only the clean script is processed.
# If annotate.functions is TRUE, ddg.function, ddg.eval and ddg.return.value
# are added to each function definition before commands are processed and
# the changes are saved to the file annotated-script.r in the ddg directory.
# ddg.annotate.on and ddg.annotate.off may be used to limit the
# functions that are annotated or not annotated, respectively.

# parsed.commands - set of R script commands.
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
# annotate.functions (optional) - if TRUE, functions are annotated
# called.from.ddg.eval(optional) - whether called from ddg.eval

.ddg.parse.commands <- function(parsed.commands, environ, ignore.patterns=c('^ddg.'), node.name="Console", run.commands = FALSE, echo=FALSE, print.eval=echo, max.deparse.length=150, annotate.functions = FALSE, called.from.ddg.eval=FALSE) {

  # Save copy of original commands for procedural node labels
  original.parsed.commands <- parsed.commands

  # Figure out if we will execute commands or not.

  execute <- run.commands & !is.null(environ) & is.environment(environ)

	# It is possible that a command may extend over multiple lines.
  # new.commands will have one string entry for each parsed command.
	new.commands <- lapply(parsed.commands, function(cmd) {paste(deparse(cmd), collapse="")})

  filtered.commands <- Filter(function(x){return(!grepl("^ddg.", x))}, new.commands)

  # Create versions from original commands for procedural node labels.
  original.new.commands <- lapply(original.parsed.commands, function(cmd) {paste(deparse(cmd), collapse="")})

  original.filtered.commands <- Filter(function(x){return(!grepl("^ddg.", x))}, original.new.commands)

  # Create start and end nodes to allow collapsing of consecutive
  # console nodes. Don't bother doing this if there is only 1 new
  # command in the history or execution.
  num.new.commands <- length(new.commands)

  #print (paste("ddg.parse.commands: .ddg.func.depth =", .ddg.get(".ddg.func.depth")))
  inside.func <- (.ddg.get(".ddg.func.depth") > 0)

  # Attempt to close the previous collapsible command node if a ddg
  # exists
  #if (.ddg.is.init() && num.new.commands > 1) .ddg.close.last.command.node(initial=TRUE)
  if (.ddg.is.init() && !inside.func) .ddg.close.last.command.node(environ, initial=TRUE)

  # Quote the quotation (") characters so that they will appear in
  # ddg.txt.
  quoted.commands <- gsub("\\\"", "\\\\\"", new.commands)

  # Create version from original commands for procedural node labels.
  original.quoted.commands <- gsub("\\\"", "\\\\\"", original.new.commands)

  # Get the last command in the new commands and check to see if
  # we need to create a new .ddg.last.cmd node for future reference.
  if (!inside.func) {
    .ddg.last.cmd <- list("abbrev" = .ddg.abbrev.cmd(quoted.commands[[num.new.commands]]),
      "expr" = parsed.commands[[num.new.commands]],
      "text" = new.commands[[num.new.commands]])
    #print(paste(".ddg.parse.commands: setting .ddg.last.cmd to", .ddg.last.cmd$text))
    if (substr(.ddg.last.cmd$abbrev, 1, 4) == "ddg.") {
      .ddg.last.cmd <- NULL
      #print(".ddg.parse.commands: setting .ddg.last.cmd to null")
    }

    else if (!execute) {
      quoted.commands <- quoted.commands[1:num.new.commands-1]
      parsed.commands <- parsed.commands[1:num.new.commands-1]
      new.commands <- new.commands[1:num.new.commands-1]
    }
  }

  filtered.commands <- Filter(function(x){
        return(!grepl("^ddg.", x))}, new.commands)

  # Create start and end nodes to allow collapsing of consecutive
  # console nodes. Don't bother doing this if there is only 1 new
  # command in the history or execution.
  named.node.set <- FALSE
  start.node.created <- ""
  num.actual.commands <- length(filtered.commands)
  #
  #if (.ddg.is.init() && (!execute && length(new.commands) > 0) || (execute && length(new.commands) > 1)) {
	if (num.actual.commands > 0 && .ddg.is.init() && !inside.func) {
    .ddg.add.abstract.node("Start", node.name, environ)
    named.node.set <- TRUE
    start.node.created <- node.name
  }

  # Don't set .ddg.last.cmd.  We want it to have the value from
  # the last call. We set it at the end of this function:
  # .ddg.set(".ddg.last.cmd", .ddg.last.cmd)

  # We tried to use a data frame to contain new.commands,
  # quoted.commands and parsed.commands, but it does not seem
  # possible to put the parsed expressions in a data frame.

  # Create an operation node for each command.  We can't use lapply
  # here because we need to process the commands in order and
  # lapply does not guarantee an order. Also decide which data nodes
  # and edges to create. Only create a data node for the last
  # write of a variable and only if that occurs after the last
  # possible writer. Create an edge for a data use as long as the
  # use happens before the first writer/possible writer or after
  # the last writer/possible writer. Lastly, if environ is set to
  # true, then execute each command immediately before attempting
  # to create the DDG nodes.

  # Only go through this if  we have at least one command to parse.
  if (length(parsed.commands) > 0) {

    # Find where all the variables are assigned for non-environ
    # files.
    if (!execute) {
      vars.set <- .ddg.find.var.assignments(parsed.commands)
    }
    else {
      vars.set <- .ddg.create.empty.vars.set()
    }

    # Loop over the commands as well as their string representations.
    for (i in 1:length(parsed.commands)) {

      if (.ddg.is.sourced() && !called.from.ddg.eval) {
        # Updated number of parsed command.
        .ddg.set(".ddg.parsed.num", i)

        # Add annotations.
        parsed.commands[i] <- .ddg.add.annotations(parsed.commands[i], annotate.functions)

        # Save annotations.
        line <- paste(i, ": ", deparse(parsed.commands[i][[1]]), sep="")
        .ddg.append.line(line)
      }

      # Process breakpoint.
      if (.ddg.is.sourced() & .ddg.break() & !.ddg.break.ignore()) {
        .ddg.process.breakpoint(new.commands[[i]], inside.function=called.from.ddg.eval)
      }

      cmd.expr <- parsed.commands[[i]]
      cmd.text <- new.commands[[i]]
      cmd <- quoted.commands[[i]]
      #cmd <- new.commands[[i]]
      original.cmd.text <- original.new.commands[[i]]
      original.cmd <- original.quoted.commands[[i]]
      cmd.abbrev <- .ddg.abbrev.cmd(original.cmd.text)
      #cmd.abbrev <- .ddg.abbrev.cmd (cmd.text)

      #print (paste (".ddg.parse.commands: cmd.expr =", cmd.expr))

      if (.ddg.enable.source() && grepl("^ddg.eval", cmd.expr) && .ddg.enable.console()) {
        update.last.cmd <- is.null(.ddg.last.cmd)
        updated.cmd <- .ddg.extract.param.from.ddg.eval(cmd.expr)

        cmd <- updated.cmd$abbrev
        cmd.expr <- updated.cmd$expr
        cmd.text <- updated.cmd$text
        cmd.abbrev <- .ddg.abbrev.cmd(cmd.text)
        #print (paste (".ddg.parse.commands: cmd.expr updated to", cmd.expr))

        if (update.last.cmd) {
          .ddg.last.cmd <- list("abbrev"=cmd.abbrev, "expr"=cmd.expr, "text"=cmd)
          #print(paste(".ddg.parse.commands: setting ddg.last.cmd to", cmd))
        }
      }

      # Get environment for output data node.
      d.environ <- environ
      if (.ddg.is.global.assign(cmd.expr)) d.environ <- globalenv()

      # Specifies whether or not a procedure node should be created
      # for this command. Basically, if a ddg exists and the
      # command is not a DDG command, it should be created.

      create <- !grepl("^ddg.", cmd) && .ddg.is.init() && .ddg.enable.console()
      start.finish.created <- FALSE

      # If the command does not match one of the ignored patterns.
      if (!any(sapply(ignore.patterns, function(pattern){grepl(pattern, cmd)}))) {
        # cmd.abbrev <- .ddg.abbrev.cmd(original.cmd.text)

        # If sourcing, we want to execute the command.
        if (execute) {
          # Print command.
          if (echo) {
            nd <- nchar(cmd)
            do.trunc <- nd > max.deparse.length
            cmd.show <- paste0(substr(cmd, 1L, if (do.trunc)
                        max.deparse.length
                        else nd), "\n")
            cat(cmd.show)
          }

          # If we will create a node, then before execution, set
          # this command as a possible abstraction node but only
          # if it's not a call that itself creates abstract nodes.
          if (!grepl("^ddg.", cmd)) {
            .ddg.set(".ddg.possible.last.cmd", list("abbrev"=cmd.abbrev, "expr"=cmd.expr, "text"=cmd.text))
            .ddg.set (".ddg.cur.cmd", cmd.text)

            # Remember the current statement on the stack so that we
            # will be able to create a corresponding Finish node later
            # if needed.
            .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")

            # Remember the current expression on a stack
            .ddg.cur.expr.stack <- .ddg.get(".ddg.cur.expr.stack")

            #print (paste(".ddg.parse.commands: pushing expr to stack", cmd.expr))
            if (length(.ddg.cur.cmd.stack) == 0) {
              .ddg.cur.cmd.stack <- c(cmd.text, FALSE)
              .ddg.cur.expr.stack <- c(cmd.expr, FALSE)
            }
            else {
              .ddg.cur.cmd.stack <- c(.ddg.get(".ddg.cur.cmd.stack"), cmd.text, FALSE)
              .ddg.cur.expr.stack <- c(.ddg.get(".ddg.cur.expr.stack"), cmd.expr, FALSE)
            }
            .ddg.set(".ddg.cur.cmd.stack", .ddg.cur.cmd.stack)
            .ddg.set(".ddg.cur.expr.stack", .ddg.cur.expr.stack)
          }
          else if (.ddg.is.procedure.cmd(cmd)) .ddg.set(".ddg.possible.last.cmd", NULL)

          # Evaluate.
          if (.ddg.debug.lib()) print (paste (".ddg.parse.commands evaluating ", deparse(cmd.expr)))

          # Capture any warnings that occur when an expression is evaluated.
          # Note that we cannot just use a tryCatch here because it behaves slightly differently
          # and we would lose the value that eval returns.  withCallingHandlers returns the value.
          result <- withCallingHandlers (eval(cmd.expr, environ, NULL), warning = .ddg.set.warning)

          if (.ddg.debug.lib()) print (paste (".ddg.parse.commands done evaluating ", deparse(cmd.expr)))

          if (!grepl("^ddg.", cmd)) {
            # Need to get the stack again because it could have been
            # modified during the eval call.
            .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
            stack.length <- length(.ddg.cur.cmd.stack)
            start.created <- .ddg.cur.cmd.stack[stack.length][[1]]

            # Create a finish node if a start node was created
            # start.created can have one of 3 values: "TRUE", "FALSE",
            # "MATCHES_CALL". Only create the finish node if TRUE.
            if (start.created == "TRUE") {
              .ddg.add.abstract.node("Finish", cmd.text, environ)
              start.finish.created <- TRUE
              .ddg.link.function.returns(cmd.text)
            }

            # Remove the last command & start.created values pushed
            # onto the stack
            #print(".ddg.parse.commands: popping the expr stack")
            if (stack.length == 2) {
              .ddg.set(".ddg.cur.cmd.stack", vector())
              .ddg.set(".ddg.cur.expr.stack", vector())
            }
            else {
              .ddg.set(".ddg.cur.cmd.stack", .ddg.cur.cmd.stack[1:(stack.length-2)])
              .ddg.set(".ddg.cur.expr.stack", .ddg.get(".ddg.cur.expr.stack")[1:(stack.length-2)])
            }
          }

          # Print evaluation.
          if (print.eval) print(result)

          # Check if initialization call. If so, then create a
          # new console node, but only if the next command is NOT
          # a DDG command.

          # if(grepl("^ddg.init", cmd) && .ddg.enable.console()) {
          #	.ddg.add.abstract.node("Start", "Console")
          #	.ddg.set(".ddg.last.cmd", list(text="Console",expr="Console"))
          # }
        }

        # Figure out if we should create a procedure node for this
        # command. We don't create it if it matches a last command
        # (because that last command has now become a collapsible
        # node). Matching a last command means that the last command
        # is set, is not NULL, and is equal to the current command.

        last.proc.node.created <-
            if (.ddg.is.set (".ddg.last.proc.node.created")).ddg.get(".ddg.last.proc.node.created")
            else ""
        cur.cmd.closed <- (last.proc.node.created == paste ("Finish", deparse(cmd.expr)))
        create.procedure <- create && (!cur.cmd.closed || !named.node.set) && !start.finish.created  && !grepl("^ddg.source", cmd.expr)


        # We want to create a procedure node for this command.
        if (create.procedure) {

          # Create the procedure node.

          if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding operation node for", cmd.abbrev))

          .ddg.proc.node("Operation", cmd.abbrev, cmd.abbrev, env=environ, console=TRUE)
          .ddg.proc2proc()

          # If a warning occurred when cmd was evaluated,
          # attach a warning node
          if (.ddg.warning.occurred()) {
            .ddg.record.warning()
          }
          # Store information on the last procedure node in this
          # block.
          #print (paste (".ddg.parse.commands: last.proc.node being set to ", cmd.abbrev))
          last.proc.node <- cmd.abbrev

          # We want to create the incoming data nodes (by updating
          # the vars.set).
          if (execute) {
            # Add variables to set.
            vars.set <- .ddg.add.to.vars.set(vars.set,cmd.expr,i)
            if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding", cmd.abbrev, "information to vars.set"))
          }

          .ddg.create.data.use.edges.for.console.cmd(vars.set, cmd.abbrev, cmd.expr, i, for.caller=FALSE)
          .ddg.create.file.read.nodes.and.edges(cmd.abbrev, cmd.expr, environ)
          .ddg.link.function.returns(cmd.text)

          if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding input data nodes for", cmd.abbrev))
          .ddg.create.data.set.edges.for.cmd(vars.set, cmd.abbrev, cmd.expr, i, d.environ)
          if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding output data nodes for", cmd.abbrev))

          .ddg.create.file.write.nodes.and.edges (cmd.abbrev, cmd.expr, environ)
          .ddg.set.graphics.files (cmd.expr, environ)
          if (.ddg.has.dev.off.call(cmd.expr)) {
            .ddg.capture.graphics(cmd.abbrev)
          }
        }
        # We wanted to create it but it matched a last command node.
        else if (create && execute) {
          .ddg.close.last.command.node(environ, initial=TRUE)
          if (execute) {
            # Add variables to set.
            vars.set <- .ddg.add.to.vars.set(vars.set,cmd.expr,i)
            if (.ddg.debug.lib()) print(paste(".ddg.parse.commands: Adding", cmd.abbrev, "information to vars.set"))
            .ddg.create.data.set.edges.for.cmd(vars.set, cmd.abbrev, cmd.expr, i, environ)
            #.ddg.create.data.set.edges.for.cmd(vars.set, cmd.text, cmd.expr, i, environ)
          }
        }

        ###### TODO #######
        #if (execute) {
        if (create.procedure && execute) {
          #print (paste (".ddg.parse.commands, 1st if: last.proc.node =", last.proc.node))
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
      #print (paste (".ddg.parse.commands, 2nd if: last.proc.node =", last.proc.node))
      .ddg.create.data.node.for.possible.writes(vars.set, last.proc.node, env=environ)
    }
  }

  # Close any node left open during execution.
  if (execute && !inside.func) .ddg.close.last.command.node(environ, initial=TRUE)

  # Close the console block if we processed anything and the DDG
  # is initialized (also, save).
  #
  if (.ddg.is.init() && named.node.set && !inside.func) {
    .ddg.add.abstract.node("Finish", node.name, environ)
  }

  # Open up a new collapsible node in case we need to parse
  # further later.
  if (!execute) {

    .ddg.set(".ddg.possible.last.cmd", .ddg.last.cmd)
    .ddg.set(".ddg.last.cmd", .ddg.last.cmd)
    #print(".ddg.parse.commands saving .ddg.last.cmd")
    .ddg.open.new.command.node(environ)
  }

  # Write time stamp to history.
  if (.ddg.is.init() && !.ddg.is.sourced()) .ddg.write.timestamp.to.history()

  # print(paste("last.commad:",.ddg.get(".ddg.last.cmd")))
  # print(paste("command:", .ddg.get(".ddg.possible.last.cmd")))
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
.ddg.proc.node <- function(ptype, pname, pvalue="", console=FALSE, auto.created=FALSE, env = sys.frame(.ddg.get.frame.number(sys.calls()))
) {
  if (.ddg.debug.lib()) {
  	#print(paste(".ddg.proc.node: length(pname) =", length(pname)))
  	#print(paste(".ddg.proc.node: pname =", pname))
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

#    if(!console) {
      # We're sourcing, so regardless of interactivity, capture
      # commands.
 #     if (.ddg.enable.source()) {
  #      .ddg.close.last.command.node(env, called=".ddg.proc.node")
  #      .ddg.open.new.command.node(env, called=".ddg.proc.node")
  #    }
      # Running interactively, so parse command history by making
      # a console node.
  #    else if (interactive()) .ddg.console.node()

#    }
    if (!console && !.ddg.enable.source() && interactive()) {
      .ddg.console.node()
    }
  }

  # Get script & line numbers if possible
  if (.ddg.is.sourced()) {
    source.parsed <- .ddg.source.parsed()
    snum <- source.parsed$snum[1]
    pnum <- .ddg.parsed.num()
    sourced.scripts <- .ddg.sourced.scripts()
    script.name <- sourced.scripts$sname[sourced.scripts$snum==snum]
    if (pname == script.name) {
      lnum <- NA
    } else if (pnum > nrow(source.parsed)) {
      lnum <- NA
      .ddg.insert.error.message("Source code line numbers may be incorrect 1")
    } else {
      index <- which(source.parsed[ , "pnum"] == pnum)
      if (length(index) == 0) {
        lnum <- NA
        .ddg.insert.error.message("Source code line numbers may be incorrect 2")
      } else {
        snum <- source.parsed$snum[index]
        lnum <- source.parsed$lnum[index]
      }
    }
    if (snum == 0) line.num <- lnum
    else line.num <- paste(snum, ":", lnum, sep="")
  } else {
    snum <- NA
    lnum <- NA
    line.num <- ""
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
  .ddg.record.proc(ptype, pname, pvalue, auto.created, ptime, snum, lnum)

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
  # If object or a long list, try to create snapshot node.
  if (is.object(dvalue)) {
    tryCatch(
        {
          .ddg.snapshot.node (dname, "txt", dvalue, dscope=dscope, from.env=from.env)
          return(NULL)
        },
        error = function(e) {
          error.msg <- paste("Unable to create snapshot node for", dname, "Details:", e)
          .ddg.insert.error.message(error.msg)
          # .ddg.dec("ddg.dnum")
          return (.ddg.data.node (dtype, dname, "complex", dscope, from.env=from.env))
        }
    )

  }

  else if (is.matrix(dvalue) || (is.vector(dvalue) && length(dvalue) > 20)) {
    .ddg.snapshot.node (dname, "csv", dvalue, dscope=dscope, from.env=from.env)
    return (NULL)
  }

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
      else if (length(dvalue) > 1 || !is.atomic(dvalue)) {
        tryCatch(paste(.ddg.replace.quotes(dvalue), collapse=","),
            error = function(e) {"complex"})
      }
      else if (is.null(dvalue)) "NULL"
      else if (is.na(dvalue)) "NA"
      else if (dvalue == "complex" || dvalue == "#ddg.function") dvalue
      else if (is.character(dvalue) && dvalue == "") "NotRecorded"
      else {
        # Replace double quotes with single quotes.
        .ddg.replace.quotes(dvalue)
      }

  if (grepl("\n", val)) {
    # Create snapshot node.
    .ddg.snapshot.node (dname, "txt", val, from.env=from.env)
    return
  }

  else {
    # Get scope if necessary.
    if (is.null(dscope)) dscope <- .ddg.get.scope(dname)

    # Record in data node table
    .ddg.record.data(dtype, dname, val, dscope, from.env=from.env)

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
  # Determine if we should save the entire data
  max.snapshot.size <- .ddg.get(".ddg.max.snapshot.size")

  if (max.snapshot.size == 0) {
    return(.ddg.data.node ("Data", dname, "", dscope, from.env=from.env))
  }

    # object.size returns bytes, but max.snapshot.size is in kilobytes
  if (max.snapshot.size == -1 || object.size(data) < max.snapshot.size * 1024) {
    full.snapshot <- TRUE
  } else {
    full.snapshot <- FALSE
  }

  # Snapshot name
  snapname <- dname

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
  else if (is.vector(data)) {
  }
  else if (is.data.frame(data) || is.matrix(data) || is.array(data) || is.list(data)) {
    if (!full.snapshot) {
      data <- head(data, n=1000)
      snapname <- paste(dname, "-PARTIAL", sep="")
    }
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
  ddg.path <- .ddg.path()
  dpfile <- paste(ddg.path, "/", dfile, sep="")
  if (.ddg.debug.lib()) print(paste("Saving snapshot in ", dpfile))

  # Write to file .
  if (fext == "csv") write.csv(data, dpfile, row.names=FALSE)

  else if (fext == "xml") saveXML (data, dpfile)

  # Capture graphic.
  else if (.ddg.supported.graphic(fext)) .ddg.graphic.snapshot(fext, dpfile)

  # Write out RData (this is old code, not sure if we need it).
  else if (fext == "RData") file.rename(paste(ddg.path, "/", dname, sep=""), dpfile)

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
  if (save.object && full.snapshot) save(data, file = paste(.ddg.path(), "/", .ddg.dnum()+1, "-", snapname, ".RObject", sep=""), ascii = TRUE)

  dtime <- .ddg.timestamp()

  # Get scope if necessary.
  if (is.null(dscope)) dscope <- .ddg.get.scope(dname)

  # Record in data node table
  .ddg.record.data(dtype, dname, dpfile, dscope, from.env=from.env, dtime)

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

	# Get path plus file name.
	dpfile <- paste(.ddg.path(), "/", dfile, sep="")

	dtime <- .ddg.timestamp()

	# Set the node label.
	if (is.null(dname)) dname <- file.name

	# Get scope if necessary.
	if (is.null(dscope)) dscope <- .ddg.get.scope(dname)

	# Not from environment.
	from.env <- FALSE

  # Record in data node table
  .ddg.record.data(dtype, dname, dpfile, dscope, from.env=from.env, dtime, file.loc)

  return(dpfile)
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
          #print(".ddg.lookup.function.name: pname is null")
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
          #print(paste(".ddg.lookup.function.name: pname is string ", pname))
          pname <- deparse(substitute(pname))
        }
        #else {
        #  print(paste(".ddg.lookup.function.name: pname is NOT a string ", pname))
        #}
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
    .ddg.add.abstract.node ("Start", pname, env)
  }
  else {
    #print(paste(".ddg.create.function.nodes: deparse(call) =", deparse(call)))
    .ddg.add.abstract.node ("Start", deparse(call), env)
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

.ddg.where <- function(name, env=parent.frame()) {
  stopifnot(is.character(name), length(name) == 1)
  if (identical(env, emptyenv())) {
    # stop("Can't find ", name, call.=FALSE)
    warning("Can't find ", name)
    return("undefined")
  }
  if (exists(name, env, inherits=FALSE)) {
    env
  }
  else {
    .ddg.where(name, parent.env(env))
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

  #	scope <- sub('<environment: (.*)>', '\\1', capture.output(.ddg.where(name, sys.frame(fnum))))
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

# .ddg.is.call.to returns TRUE if the parsed expression passed
# in is a call to the specified function.

.ddg.is.call.to <- function(parsed.expr, func.name) {
  # Check if a function call.
  if (is.call(parsed.expr)) {
    # Check if the function called is the specified function.
    if (parsed.expr[[1]] == func.name) {
      return (TRUE)
    }
  }
  return (FALSE)
}

# .ddg.has.call.to returns TRUE if the parsed expression passed
# in contains a call to the specified function.

.ddg.has.call.to <- function(parsed.expr, func.name) {
  # Base case.
  if (!is.recursive(parsed.expr)) return(FALSE)
  # A call to the specified function.
  if (.ddg.is.call.to(parsed.expr, func.name)) {
    return (TRUE)
  }
  # Not a call to the specified function.  Recurse on the parts of
  # the expression.
  else {
    return (any(sapply(parsed.expr, function(parsed.expr) {return(.ddg.has.call.to(parsed.expr, func.name))})))
  }
}

# .ddg.is.call.to.ddg.function returns TRUE if the parsed expression
# passed in is a call to a ddg function.

.ddg.is.call.to.ddg.function <- function(parsed.expr) {
  # Check if a function call.
  if (is.call(parsed.expr)) {
    # Check if the function called is a ddg function.
    if (grepl("^ddg.", parsed.expr[1])) {
      return (TRUE)
    }
  }
  return (FALSE)
}

# .ddg.find.last.statement finds the last statement of a function.

.ddg.find.last.statement <- function (func.definition) {
  # Get function body.
  func.body <- func.definition[[3]]
  # Check to see if the function body is a block.
  if (func.body[[1]] == "{") {
    # Return the last statement in the block.
    pos <- length(func.body)
    return(func.body[[pos]])
  }
  # Not a block. Return the single statement that is the body.
  else {
    return(func.body)
  }
}

# .ddg.create.function.block creates a function block.

.ddg.create.function.block <- function(func.definition) {
  # Get the function parameters.
  func.params <- func.definition[[2]]

  # Get the body of the function.
  func.body <- func.definition[[3]]

  # Add block and reconstruct the call.
  new.func.body <- call("{", func.body)
  return(call("function", func.params, as.call(new.func.body)))
}

# .ddg.insert.ddg.function inserts ddg.function before the first line
# in a function body.

.ddg.insert.ddg.function <- function(func.definition) {
  # Get the function parameters.
  func.params <- func.definition[[2]]

  # Get the body of the function.
  func.body <- func.definition[[3]]

  pos <- length (func.body)

  # If the function body contains a single statement, insert
  # ddg.function and reconstruct the call.
  if (pos == 2) {
    inserted.statement <- call("ddg.function")
    new.statements <- c(as.list(func.body[1]), inserted.statement, as.list(func.body[2]))
    return(call("function", func.params, as.call(new.statements)))
  }

  # If the function body contains more than one statement, insert
  # ddg.function and reconstruct the call.
  else {
    inserted.statement <- call("ddg.function")
    new.statements <- c(as.list(func.body[1]), inserted.statement, as.list(func.body[2:pos]))
    return(call("function", func.params, as.call(new.statements)))
  }
}

# .ddg.wrap.return.parameters wraps parameters of return functions
# with ddg.return.value in a function body.

.ddg.wrap.return.parameters <- function(func.body) {
  pos <- length(func.body)
  # Check each statement in the function body to see if it
  # contains a return.
  for (i in 1:pos) {
    statement <- func.body[[i]]
    if (.ddg.has.call.to(statement, "return")) {

      # If statement is a return, wrap parameters with ddg.return.value.
      if (.ddg.is.call.to(statement, "return")) {
        # Need to handle empty parameter separately.
        if (length(statement) == 1) {
          ret.params <- ""
        } else {
          ret.params <- statement[[2]]
        }

        # If parameters contain a return, recurse on parameters.
        if (.ddg.has.call.to(ret.params, "return")) {
          ret.params <- .ddg.wrap.return.parameters(ret.params)
        }

        new.ret.params <- call("ddg.return.value", ret.params)
        new.statement <- call("return", new.ret.params)
        func.body[[i]] <- new.statement

      # If statement contains a return, recurse on statement.
      } else {
        func.body[[i]] <- .ddg.wrap.return.parameters(statement)
      }
    }
  }
  return(func.body)
}

# .ddg.wrap.all.return.parameters wraps parameters of all return
# functions with ddg.return.value in a function definition.

.ddg.wrap.all.return.parameters <- function(func.definition) {
  # Get function parameters.
  func.params <- func.definition[[2]]

  # Get the body of the function.
  func.body <- func.definition[[3]]

  # Wrap individual return functions.
  new.func.body <- .ddg.wrap.return.parameters(func.body)

  # Reconstruct function.
  return(call("function", func.params, as.call(new.func.body)))
}

# .ddg.wrap.last.line wraps the last line of a function with
# ddg.return.value.

.ddg.wrap.last.line <- function(func.definition) {
  # Get function parameters.
  func.params <- func.definition[[2]]

  # Get the body of the function.
  func.body <- func.definition[[3]]

  # Check to see if the function body is a block.
  pos <- length (func.body)

  # If the function body contains a single statement, wrap that
  # statement and reconstruct the call.
  if (pos == 2) {
    last.statement <- func.body[[pos]]
    wrapped.statement <- call ("ddg.return.value", last.statement)
    new.func.body <- call("{", wrapped.statement)
    return(call("function", func.params, new.func.body))
  }

  # If the function body contains more than one statement, find the
  # last statement, wrap it, and reconstruct the call.
  else {
    last.statement <- func.body[[pos]]
    wrapped.statement <- call ("ddg.return.value", last.statement)
    new.statements <- c(as.list(func.body[2:pos-1]), wrapped.statement)
    return(call("function", func.params, as.call(new.statements)))
  }
}

# .ddg.wrap.with.ddg.eval wraps each statement in a function body
# with ddg.eval if the statement is not a call to a ddg function and
# does not contain a call to ddg.return.value. The statement is enclosed
# in quotation marks.

.ddg.wrap.with.ddg.eval <- function(func.definition) {
  # Get the function parameters.
  func.params <- func.definition[[2]]

  # Get the body of the function.
  func.body <- func.definition[[3]]

  pos <- length(func.body)

  # Process each statement in the function body
  for (i in 2:pos) {
    # Wrap with ddg.eval if statement is not a call to a ddg function and
    # does not contain a call to ddg.return.value. Enclose statement in
    # quotation marks.
    statement <- func.body[[i]]
    if (!grepl("^ddg.", statement[1]) & !.ddg.has.call.to(statement, "ddg.return.value")) {
      new.statement <- call("ddg.eval", deparse(statement))
      func.body[[i]] <- new.statement
    }
  }
  # Reassemble function definition.
  func.definition <- call("function", func.params, as.call(func.body))

  return(func.definition)
}

# .ddg.add.function.annotations accepts and returns a parsed command.
# If the command is a function declaration, calls to ddg.function, ddg.eval
# and ddg.return.value are added, if not already present. Otherwise the
# command is returned unchanged. The functions ddg.annotate.on and
# ddg.annotate.off may be used to provide a list of functions to annotate
# or not to annotate, respectively.

.ddg.add.function.annotations <- function(parsed.command) {
  # Return if a list of functions to annotate is provided and this
  # function is not on the list.
  if (!is.null(.ddg.annotate.on()) & !(toString(parsed.command[[1]][[2]]) %in% .ddg.annotate.on())) return(parsed.command)

  # Return if a list of functions not to annotate is provided and this
  # function is on the list.
  else if (!is.null(.ddg.annotate.off()) & toString(parsed.command[[1]][[2]]) %in% .ddg.annotate.off()) return(parsed.command)

  # Add function annotations.
  else {
    # Get function name.
    func.name <- parsed.command[[1]][[2]]

    # Get function definition.
    func.definition <- parsed.command[[1]][[3]]

    # Create function block if necessary.
    if (func.definition[[3]][[1]] != "{") {
      func.definition <- .ddg.create.function.block(func.definition)
    }

    # Insert call to ddg.function if not already added.
    if (!.ddg.has.call.to(func.definition, "ddg.function")) {
      func.definition <- .ddg.insert.ddg.function(func.definition)
    }

    # Insert calls to ddg.return.value if not already added.
    if (!.ddg.has.call.to(func.definition, "ddg.return.value")) {
      func.definition <- .ddg.wrap.all.return.parameters(func.definition)
    }

    # Wrap last statement with ddg.return.value if not already added
    # and if last statement is not a simple return or a ddg function.
    last.statement <- .ddg.find.last.statement(func.definition)
    if (!.ddg.is.call.to(last.statement, "ddg.return.value") & !.ddg.is.call.to(last.statement, "return") & !.ddg.is.call.to.ddg.function(last.statement)) {
      func.definition <- .ddg.wrap.last.line(func.definition)
    }

    # Wrap statements with ddg.eval if not already added and if
    # statements are not calls to a ddg function and do not contain
    # ddg.return.value.
    if (!.ddg.has.call.to(func.definition, "ddg.eval")) {
      func.definition <- .ddg.wrap.with.ddg.eval(func.definition)
    }

    # Reassemble parsed.command.
    func.name.txt <- toString(func.name)
    func.definition.txt <- deparse(func.definition)
    parsed.command.txt <- paste(c(paste(func.name.txt, "<-", sep=" "), func.definition.txt, collapse="\n"))
    parsed.command <- parse(text=parsed.command.txt)
    # Return modified parsed command
    return(parsed.command)
  }
}


# .ddg.add.ddg.source replaces source with ddg.source

.ddg.add.ddg.source <- function(parsed.command) {
  script.name <- deparse(parsed.command[[1]][[2]])
  new.command.txt <- paste("ddg.source(", script.name, ")", sep="")
  parsed.command <- parse(text=new.command.txt)
  return(parsed.command)
}

# .ddg.add.annotations accepts and returns a parsed command.
# The returned command is annotated as needed.

.ddg.add.annotations <- function(parsed.command, annotate.functions=FALSE) {
  # Return if statement is empty.
  if (length(parsed.command) == 0) return(parsed.command)

  # Replace source with ddg.source.
  if (length(parsed.command[[1]]) > 1 && parsed.command[[1]][[1]] == "source") {
      return(.ddg.add.ddg.source(parsed.command))
  }

  # Annotate user-defined functions.
  if (annotate.functions && .ddg.is.assign(parsed.command[[1]]) && .ddg.is.functiondecl(parsed.command[[1]][[3]])) {
    return(.ddg.add.function.annotations(parsed.command))
  }

  # Add other annotations here.

  # No annotation required.
  return(parsed.command)
}

#.ddg.rm removes all data except ddg information
.ddg.rm <-function()

# .ddg.get.annotation.list checks to see if the script contains calls to
# ddg.annotate.on or ddg.annotate.off. If it does, these calls are executed.

.ddg.get.annotation.list <- function(parsed.command) {
  if (length(parsed.command[[1]]) > 1) {
    if (toString(parsed.command[[1]][[1]]) == "ddg.annotate.on" | toString(parsed.command[[1]][[1]]) == "ddg.annotate.off") {
      eval(parsed.command)
    }
  }
}

# Creates a start node for the current command if one has not
# been created already.
.ddg.create.start.for.cur.cmd <- function (call, caller.env) {
  if (.ddg.is.set(".ddg.cur.cmd")) {
    #print("In .ddg.create.start.for.cur.cmd")
    .ddg.cur.cmd <- .ddg.get(".ddg.cur.cmd")
    .ddg.cur.cmd.stack <- .ddg.get(".ddg.cur.cmd.stack")
    stack.length <- length(.ddg.cur.cmd.stack)
    if (stack.length >= 1) {
      last.created <- .ddg.cur.cmd.stack[stack.length]
      # Only create a start node for the current command if we have not already
      # created one and the command is more than just the call to this function
      if (last.created[[1]] == "FALSE") {
        if (.ddg.cur.cmd != paste(deparse(call), collapse="")) {
          #print(".ddg.create.start.for.cur.cmd: Creating start node")
          cmd.abbrev <- .ddg.add.abstract.node ("Start", .ddg.cur.cmd, caller.env)
          .ddg.cur.expr.stack <- .ddg.get(".ddg.cur.expr.stack")
          #print(paste(".ddg.create.start.for.cur.cmd: .ddg.cur.expr.stack =", .ddg.cur.expr.stack))
          #print(".ddg.create.start.for.cur.cmd: Creating data use edges")
          .ddg.create.data.use.edges.for.console.cmd(vars.set = data.frame(), cmd.abbrev,
              .ddg.cur.expr.stack[[length(.ddg.cur.expr.stack)-1]], 0, for.caller=TRUE)

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
              #	.ddg.data2proc(deparse(arg), pname)
              #	if (.ddg.debug.lib()) print(paste("param:", deparse(arg)))
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

ddg.return.value <- function (expr=NULL) {

  if (!.ddg.is.init()) return(expr)

  # If expr is an assignment, create nodes and edges for the assignment.
  orig.expr <- substitute(expr)

  frame.num <- .ddg.get.frame.number(sys.calls())
  env <- sys.frame(frame.num)

  orig.pname <- paste(deparse(orig.expr), collapse="")

  # Process breakpoint.
  orig.return <- paste("return(", orig.pname, ")", sep="")

  if (.ddg.break() & !.ddg.break.ignore()) {
    .ddg.process.breakpoint(orig.return, inside.function=TRUE)
  }

  # Create procedure node.
  #print ("ddg.return.value: orig.expr =")
  #print (orig.expr)
  #print ("ddg.return.value: orig.pname =")
  #print (orig.pname)
  .ddg.proc.node("Operation", orig.pname, orig.pname, console=TRUE)

  # Create control flow edge from preceding procedure node.
  .ddg.proc2proc()

  # Create data flow edges from input data nodes, if any.
  vars.used <- .ddg.find.var.uses(orig.expr)
  for (var in vars.used) {
    scope <- .ddg.get.scope(var)
    if (.ddg.data.node.exists(var, scope)) {
      .ddg.data2proc(var, scope, orig.pname)
    }
  }

  if (.ddg.is.assign(orig.expr)) {
    # Create output data node.
    var <- orig.expr[[2]]
    dname <- deparse(var)
    dvalue <- eval(orig.expr[[3]], envir=env)

    # Check for global assignment
    if (.ddg.is.global.assign(orig.expr)) env <- globalenv()
    dscope <- .ddg.get.scope(var, env=env)
    .ddg.save.data(dname, dvalue, scope=dscope)
    # Create an edge from procedure node to data node.
    .ddg.proc2data(orig.pname, dname, dscope=dscope, return.value=FALSE)
  }

  pname <- NULL
  .ddg.lookup.function.name(pname)

  # If this is a recursive call to ddg.return.value, find
  # the caller of the first ddg.return.value
  if (grepl("(^ddg|.ddg)", pname)) {
    caller.frame <- .ddg.find.ddg.return.value.caller.frame.number ()
    pname <- as.character(sys.call(caller.frame)[[1]])
  }
  else {
    caller.frame <- -1
  }

  # Prints the call & arguments.
  if (.ddg.debug.lib()) {
    # expr forces evaluation of the function early.  I think that
    # causes some examples to work with debugging on but not off.
    # Checking.  (6/26/2015 - Barb).
    # Yes, ReturnTest.R fails on the recursive f5 function
    print(paste("ddg.return.value:", sys.call(caller.frame))) #, "returns", expr))
  }

  ddg.return.values <- .ddg.get(".ddg.return.values")
  ddg.num.returns <- .ddg.get(".ddg.num.returns")
  if (nrow(ddg.return.values) == ddg.num.returns) {
    size = 100
    new.rows <- data.frame(ddg.call = character(size),
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
    #print(paste("ddg.return.value: pname = ", pname))
    full.call <- match.call(sys.function(caller.frame), call=call)
    #print (paste0("ddg.return.value creating function node for ", pname))
    .ddg.create.function.nodes(pname, call, full.call, auto.created = TRUE, env = sys.frame(.ddg.get.frame.number(sys.calls()))
    )
    #print (paste0("ddg.return.value done creating function node for ", pname))
  }
  else {
    .ddg.dec (".ddg.func.depth")
  }

  # Create a data node for the return value. We want the scope of
  # the function that called the function that called ddg.return.
  call.text <- gsub(" ", "", deparse(call, nlines=1))
  return.node.name <- paste(call.text, "return")
  return.node.name <- gsub("\"", "\\\\\"", return.node.name)

  return.node.scope <-
    environmentName (if (sys.nframe() == 2) .GlobalEnv
                     else parent.env(sys.frame(caller.frame)))
  .ddg.save.data(return.node.name, expr, fname="ddg.return", scope=return.node.scope)

  # Create a return proc node

  # Substitute can return a vector of language objects, so we will deparse,
  # concatenate the strings and then reparse to get a single language object.
  # To print the string, we use space as a separator, but for parsing purposes
  # we insert a newline to avoid having 2 statements on the same line, which
  # results in parse errors.
  return.expr <- substitute(expr)
  return.expr.text.vector <- deparse(return.expr)
  return.expr.text.toprint <- paste (return.expr.text.vector, collapse="")
  if (nchar(return.expr.text.toprint > 50)) return.expr.text.toprint <- substr(return.expr.text.toprint, 1, 50)
  return.stmt <- paste0("return (", return.expr.text.toprint, ")")
  return.expr.text.toparse <- paste (return.expr.text.vector, collapse="\n")
  return.expr <- parse (text=return.expr.text.toparse)

  caller.env = sys.frame(caller.frame)
  .ddg.proc.node("Operation", return.stmt, return.stmt, console = TRUE, env=caller.env)

  # Create control flow edge from preceding procedure node.
  .ddg.proc2proc()

  # Create edges from variables used in the return statement
  vars.used <- .ddg.find.var.uses(return.expr)
  #print (paste( "ddg.return.value: vars.used =", vars.used))
  #print (paste ("Contents of environment", ls(caller.env)))
  for (var in vars.used) {
    # Make sure there is a node we could connect to.
    #print (paste( "ddg.return.value: getting scope for", var))
    scope <- .ddg.get.scope(var)
    if (.ddg.data.node.exists(var, scope)) {
      .ddg.data2proc(var, scope, return.stmt)
    }
  }

  # Create nodes and edges dealing with reading and writing files
  return.abbrev <- orig.pname
  .ddg.create.file.read.nodes.and.edges(return.abbrev, return.expr, env)
  .ddg.create.file.write.nodes.and.edges (return.abbrev, return.expr, env)
  .ddg.set.graphics.files (return.expr, env)
  if (.ddg.has.dev.off.call(return.expr)) {
    .ddg.capture.graphics(return.abbrev)
  }

  # Create an edge from the return statement to its return value.
  .ddg.proc2data(return.stmt, return.node.name, return.node.scope, return.value=TRUE)

  # Update the table.
  ddg.num.returns <- ddg.num.returns + 1
  ddg.return.values$ddg.call[ddg.num.returns] <- call.text
  ddg.return.values$return.used[ddg.num.returns] <- FALSE
  ddg.return.values$return.node.id[ddg.num.returns] <- .ddg.dnum()
  .ddg.set(".ddg.return.values", ddg.return.values)
  .ddg.set(".ddg.num.returns", ddg.num.returns)

  # Create the finish node for the function
  #.ddg.close.last.command.node(sys.frame(caller.frame-1), called="ddg.return.value", initial=TRUE)
  if (typeof(call[[1]]) == "closure") {
    .ddg.add.abstract.node ("Finish", pname, caller.env)
  }
  else {
    .ddg.add.abstract.node ("Finish", deparse(call), caller.env)
  }

  #print ("Returning from ddg.return.value")
  return(expr)
}

# ddg.eval evaluates a statement and creates data flow edges from
# variable and function return nodes that are used in the
# statement. If the statement is an assignment statement, it also
# creates a data node for the variable assigned and a corresponding
# data flow edge.

# statement - the statement to evaluate.

ddg.eval <- function(statement) {
  if (.ddg.debug.lib()) print (paste("ddg.eval: statement =", statement))
  parsed.statement <- parse(text=statement)
  frame.num <- .ddg.get.frame.number(sys.calls())
  # print(paste("statement = ", statement, sep=""))
  # print (paste("ddg.eval:  frame.num =", frame.num))
  env <- sys.frame(frame.num)

  if (!.ddg.is.init()) {
    # print ("ddg.eval:  no ddg!")
    eval(parsed.statement, env)
    return(invisible())
  }

  if (interactive() && .ddg.enable.console() && !.ddg.enable.source()) {
    # print("ddg.eval:  Creating console node")
    .ddg.console.node()
  }

  # print ("ddg.eval:  calling .ddg.parse.commands")
  .ddg.parse.commands(parsed.statement, environ=env, run.commands = TRUE, node.name=statement, called.from.ddg.eval=TRUE)
  if (.ddg.get(".ddg.func.depth") == 0) {
    # print ("ddg.eval:  calling .ddg.link.function.returns")
    .ddg.link.function.returns(statement)

    # Create outflowing edges .
    .ddg.statement <- list("abbrev" = .ddg.abbrev.cmd(gsub("\\\"", "\\\\\"", statement)),
        "expr" = parsed.statement,
        "text" = statement)

    vars.set <- .ddg.find.var.assignments(.ddg.statement$expr)
    # print ("ddg.eval: creating data set edges")
    .ddg.create.data.set.edges.for.cmd(vars.set, .ddg.statement$abbrev, .ddg.statement$expr, 1, env)
    # print ("ddg.eval done")
  }
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
}


# ddg.init intializes a new DDG.

# r.script.path (optional) - the full path to the R script file
# that is being executed. If provided, a copy of the script will
# be saved with the DDG.
# ddgdir (optional) - the directory where the DDG should be saved.
#   If not provided, the DDG will be saved in a subdirectory called
#   "ddg" in the current working directory.
# enable.console (optional) - if TRUE, console mode is turned on.
# max.snapshot.size (optional) - the maximum size for objects that
#   should be output to snapshot files. If 0, no snapshot files are saved.
#   If -1, all snapshot files are saved. Size in kilobytes.  Note that
#   this tests the size of the object that will be turned into a
#   snapshot, not the size of the resulting snapshot.
# Addition : overwrite (optional) - default TRUE, if FALSE, generates
#   timestamp for ddg directory

ddg.init <- function(r.script.path = NULL, ddgdir = NULL, overwrite = TRUE, enable.console = TRUE, max.snapshot.size = 100) {
  .ddg.init.tables()

  #Setting the path for the ddg
  if (is.null(ddgdir)) {

    #default is the file where the script is located
    if (!is.null(r.script.path)){
      ddg.path <- paste(dirname(r.script.path), "/", basename(tools::file_path_sans_ext(r.script.path)), "_ddg", sep="")
    }
    else {
      ddg.path <- paste(getwd(), "/","ddg",sep = "")
    }
  } else ddg.path <- normalizePath(ddgdir, winslash="/", mustWork=FALSE)

  #overwrite default is
  if(!overwrite){
    no.overwrite.folder <- paste(ddg.path, "_timestamps", sep = "")
    if(!dir.exists(no.overwrite.folder)){
      dir.create(no.overwrite.folder)
    }
    ddg.path <- paste(no.overwrite.folder, "/",  basename(tools::file_path_sans_ext(r.script.path)), "_ddg_", .ddg.timestamp(), sep = "")
  }

  .ddg.set("ddg.path", ddg.path)

  .ddg.init.environ()

  # Reset r.script.path if RMarkdown file

  if (!is.null(r.script.path) && tools::file_ext(r.script.path) == "Rmd") {
    output.path <- paste(ddg.path, "/", basename(tools::file_path_sans_ext(r.script.path)), ".R", sep = "")
    .ddg.markdown(r.script.path, output.path)
    .ddg.set("ddg.r.script.path", output.path)
  } else {
  # Set r.script.path and copy it
  .ddg.set("ddg.r.script.path",
      if (is.null(r.script.path)) NULL
          else normalizePath(r.script.path, winslash="/"))
    file.copy(r.script.path, paste(ddg.path, "/", basename(tools::file_path_sans_ext(r.script.path)), ".R", sep = ""))
  }


  # Set environment constants.
  .ddg.set(".ddg.enable.console", enable.console)
  .ddg.set(".ddg.max.snapshot.size", max.snapshot.size)
  .ddg.set(".ddg.func.depth", 0)
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

  if (interactive() && .ddg.enable.console()) {
    ddg.history.file <- paste(.ddg.path(), ".ddghistory", sep="/")
    .ddg.set(".ddg.history.file", ddg.history.file)

    # Empty file if it already exists, do the same with tmp file.
    file.create(ddg.history.file)

    # One timestamp keeps track of last ddg.save (the default).
    .ddg.write.timestamp.to.history()

    # Save the history if the platform supports it.
    tryCatch (savehistory(ddg.history.file),
        error = function(e) {})
  }

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
# annotate.functions (optional) - if TRUE, functions are annotated.
# max.snapshot.size (optional) - the maximum size for objects that
#   should be output to snapshot files. If 0, no snapshot files are
#   saved. If -1, all snapshot files are saved.  Size in kilobytes.
#   Note that this tests the size of the object that will be turned
#   into a snapshot, not the size of the resulting snapshot.
# debug (optional) - If TRUE, enable script debugging. This has the
#   same effect as inserting ddg.breakpoint() at the top of the script.

ddg.run <- function(r.script.path = NULL, ddgdir = NULL, overwrite = TRUE, f = NULL, enable.console = TRUE, annotate.functions = TRUE, max.snapshot.size = 100, debug = FALSE, display = FALSE) {

  # Initiate ddg.
  ddg.init(r.script.path, ddgdir, overwrite, enable.console, max.snapshot.size)

  # Create ddg directory.
  # dir.create(.ddg.path(), showWarnings = FALSE)

  # Remove existing files if ddg directory different from working
  # directory.
  # ddg.flush.ddg()

  # Set .ddg.is.sourced to TRUE.
  .ddg.set(".ddg.is.sourced", TRUE)

  # Set breakpoint if debug is TRUE
  if (debug) ddg.breakpoint()

  # If an R error is generated, get the error message and close
  # the DDG.
  tryCatch(
      if (!is.null(f)) f()
          else if (!is.null(r.script.path)) ddg.source(
               .ddg.get("ddg.r.script.path"),
                ddgdir = ddgdir,
                ignore.ddg.calls = FALSE,
                ignore.init = TRUE,
                force.console = FALSE,
                annotate.functions = annotate.functions)
          else stop("r.script.path and f cannot both be NULL"),
      error=function(e) {
        e.str <- toString(e)
        print(e.str)
        ddg.procedure(pname="tryCatch")
        ddg.exception.out("error.msg", e.str, "tryCatch")
      },
      finally={
        ddg.save(r.script.path)
		    if(display == TRUE)
			    .ddg.loadDDG(.ddg.path())
      }
  )
  invisible()
}

#Function to display DDG automatically
.ddg.loadDDG<- function(ddg.folder){
	jar.path<- "/RDataTracker/java/DDGExplorer.jar"
	check.library.paths<- file.exists(paste(.libPaths(),jar.path,sep = ""))
	index<- min(which(check.library.paths == TRUE))
	ddgexplorer_path<- paste(.libPaths()[index],jar.path,sep = "")
	ddgtxt.path<- paste(ddg.folder,"/ddg.txt",sep = "")
	if(Sys.info()['sysname']!="Windows"){
		system(paste("java -jar ",ddgexplorer_path, ddgtxt.path,'&',sep = " "))
	}else{
		shell(paste("START java -jar ",ddgexplorer_path, ddgtxt.path,sep = " "))
	}
}

# This function takes a Rmd file and extracts the R code and text through
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

# ddg.save inserts attribute information and the number of
# procedure steps at the top of the DDG. It writes the DDG and
# the procedure nodes, data nodes, and function return tables
# to the DDG directory.

# r.script.path (optional) - Path to the R script
# quit (optional) - If TRUE, remove all DDG files from memory.

ddg.save <- function(r.script.path=NULL, quit=FALSE) {

  if (!.ddg.is.init()) return(invisible())

  if (interactive() && .ddg.enable.console()) {
    # Get the final commands
    .ddg.console.node()
  }

  # If there is a display device open, grab what is on the display
  if (length(dev.list()) > 1) {
    #print("ddg.save: Saving graphics open at end of script")
    # tryCatch (.ddg.capture.current.graphics(basename(r.script.path)),
    tryCatch (.ddg.capture.current.graphics(basename(.ddg.get("ddg.r.script.path"))),
        error = function (e) e)
  }

  # Delete temporary files.
  .ddg.delete.temp()

  # Get ddg path.
  ddg.path <- .ddg.path()

  # Save ddg.txt to file.
  .ddg.txt.write()
  if (interactive()) print(paste("Saving ddg.txt in ", ddg.path, sep=""))

  # Save json.txt to file.
  .ddg.json.write()
  if (interactive()) print(paste("Saving ddg.json in ", ddg.path, sep=""))

  # Save initial environment table to file.
  fileout <- paste(ddg.path, "/inenv.txt", sep="")
  ddg.initial.env <- .ddg.initial.env()
  write.table(ddg.initial.env, fileout, quote=FALSE, sep="\t", na="NA", row.names=FALSE, col.names=TRUE)

  # Save procedure nodes table to file.
  fileout <- paste(ddg.path, "/pnodes.txt", sep="")
  ddg.proc.nodes <- .ddg.proc.nodes()
  write.table(ddg.proc.nodes[ddg.proc.nodes$ddg.num > 0, ], fileout, quote=FALSE, sep="\t", na="NA", row.names=FALSE, col.names=TRUE)

  # Save data nodes table to file.
  fileout <- paste(ddg.path, "/dnodes.txt", sep="")
  ddg.data.nodes <- .ddg.data.nodes()
  write.table(ddg.data.nodes[ddg.data.nodes$ddg.num > 0, ], fileout, quote=FALSE, sep="\t", na="NA", row.names=FALSE, col.names=TRUE)

  # Save data flow table to file.
  fileout <- paste(ddg.path, "/edges.txt", sep="")
  ddg.edges <- .ddg.edges()
  write.table(ddg.edges[ddg.edges$ddg.num > 0, ], fileout, quote=FALSE, sep="\t", na="NA", row.names=FALSE, col.names=TRUE)

  # Save the function return table to file.
  fileout <- paste(ddg.path, "/returns.txt", sep="")
  ddg.returns <- .ddg.get(".ddg.return.values")
  write.table(ddg.returns[ddg.returns$return.node.id > 0, ], fileout, quote=FALSE, sep="\t", na="NA", row.names=FALSE, col.names=TRUE)

  # Save if script is sourced.
  ddg.is.sourced <- .ddg.is.sourced()

  if (ddg.is.sourced) {
    # Save sourced script table to file.
    fileout <- paste(ddg.path, "/sourced-scripts.txt", sep="")
    ddg.sourced.scripts <- .ddg.get(".ddg.sourced.scripts")
    write.table(ddg.sourced.scripts[ddg.sourced.scripts$snum >= 0, ], fileout, quote=FALSE, sep="\t", na="NA", row.names=FALSE, col.names=TRUE)

    # Save annotated script to file.
    fileout <- file(paste(.ddg.path(), "annotated-script.r", sep="/"))
    ddg.annotated.script <- .ddg.get("ddg.annotated.script")
    write(ddg.annotated.script, fileout)
    close(fileout)

    # Save data object table to file.
    fileout <- paste(ddg.path, "dobjects.csv", sep="/")
    ddg.data.objects <- .ddg.data.objects()
    write.csv(ddg.data.objects, fileout, row.names=FALSE)
  }

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
# annotate.functions (optional) - if TRUE, functions are annotated.

ddg.source <- function (file,  ddgdir = NULL, local = FALSE, echo = verbose, print.eval = echo,
    verbose = getOption("verbose"), max.deparse.length = 150, chdir = FALSE, encoding = getOption("encoding"),
    ignore.ddg.calls = TRUE, ignore.init = ignore.ddg.calls, force.console=ignore.init, annotate.functions = TRUE){

  # Store script number & name.
  snum <- .ddg.next.script.num()
  sname <- basename(file)

  if (snum == 0) {
    df <- data.frame(snum, sname, stringsAsFactors=FALSE)
  } else {
    df<- rbind(.ddg.sourced.scripts(), c(snum, sname))
  }
  .ddg.set(".ddg.sourced.scripts", df)

  # Get line numbers from source code.
  df <- .ddg.get.source.code.line.numbers(file, snum)

  # Set current source.parsed table.
  .ddg.set(".ddg.source.parsed", df)

  # Store in numbered data frame.
  df.name <- paste(".ddg.source.parsed-", snum, sep="")
  .ddg.set(df.name, df)

  # Push script number on stack.
  stack <- .ddg.script.num.stack()
  .ddg.push(stack, snum)
  .ddg.set(".ddg.script.num.stack", stack)

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

  # Source from modified script if breakpoints were set.
  orig.file <- file
  if (!is.null(ddg.list.breakpoints())) {
    file <- paste(.ddg.path(), "/script-", snum, ".r", sep="")
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
              encoding)
        }
        else expression()
      }
      else {
        parse(file, n = -1, NULL, "?", srcfile, encoding)
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
    .ddg.parse.commands(exprs, environ=envir, ignore.patterns=ignores, node.name=orig.file,
        echo = echo, print.eval = print.eval, max.deparse.length = max.deparse.length,
        run.commands = TRUE, annotate.functions = annotate.functions)

    # Save the DDG among other things, but don't return any
    # values, TODO - should we do this?
    # ddg.save()
    .ddg.set("from.source", prev.source)

    # Turn return console to previous state.
    if (!prev.on) ddg.console.off() else ddg.console.on()
  }

  # Pop script number from stack.
  stack <- .ddg.script.num.stack()
  .ddg.pop(stack)
  .ddg.set(".ddg.script.num.stack", stack)

  # Restore previous source.parsed table.
  snum <- stack[length(stack)]
  df.name <- paste(".ddg.source.parsed-", snum, sep="")
  df <- .ddg.get(df.name)
  .ddg.set(".ddg.source.parsed", df)

  invisible()
}

# ddg.display.ddg loads & displays the current DDG.

ddg.display <- function () {
  if (.ddg.is.set("ddg.path") & file.exists(paste(.ddg.path(), "/ddg.txt", sep=""))) {
    .ddg.loadDDG(.ddg.path())
  } else {
    print("DDG not available")
  }
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

# fnames - a list of one or more function names passed in as strings.

ddg.annotate.on <- function (fnames=NULL) {
  .ddg.set("ddg.annotate.on", fnames)
}

# ddg.annotate.off disables annotation for the specified functions.
# Functions not on this list are annotated.

# fnames - a list of one or more function names passed in as strings.

ddg.annotate.off <- function (fnames=NULL) {
  .ddg.set("ddg.annotate.off", fnames)
}

# ddg.flush.ddg removes all files from the DDG directory unless the
#   DDG directory is the working directory. If no DDG directory is
#   specified, the current DDG directory is assumed.

# ddg.path (optional) - path to DDG directory.

ddg.flush.ddg <- function(ddg.path=NULL) {
	if (is.null(ddg.path)) {
		ddg.path <- .ddg.path()
	}

	# Do not remove files unless ddg.path exists and is different
  # from the working directory.
  if (file.exists(ddg.path) && ddg.path != getwd()) {
    unlink(paste(ddg.path, "*.txt", sep="/"))
    unlink(paste(ddg.path, "*.csv", sep="/"))
    unlink(paste(ddg.path, "*.json", sep="/"))
    unlink(paste(ddg.path, "*.r", sep="/"))
    unlink(paste(ddg.path, "*.R", sep="/"))
    unlink(paste(ddg.path, ".ddghistory", sep="/"))
    unlink(paste(ddg.path,"[1-9]-*.*", sep="/"))
    unlink(paste(ddg.path,"[1-9][0-9]-*.*", sep="/"))
    unlink(paste(ddg.path,"[1-9][0-9][0-9]-*.*", sep="/"))
    unlink(paste(ddg.path,"[1-9][0-9][0-9][0-9]-*.*", sep="/"))
    unlink(paste(ddg.path,"[1-9][0-9][0-9][0-9][0-9]-*.*", sep="/"))
    unlink(paste(ddg.path,"[1-9][0-9][0-9][0-9][0-9][0-9]-*.*", sep="/"))
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
