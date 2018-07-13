# Copyright (C) President and Fellows of Harvard College and 
# Trustees of Mount Holyoke College, 2014, 2015, 2016, 2017, 2018.

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

######################## Exported Functions ########################

# The following functions are exported:

# ddg.init - intializes a new provenance graph
# ddg.save - saves the current provenance graph
# ddg.run - initiates execution of a script
# ddg.source - sources a script & collects provenance
# ddg.json - returns the current provenance graph as a prov-json string

#' @export ddg.init intializes a new provenance graph
#'
#' @param r.script.path (optional) - the full path to the R script file
#' that is being executed. If provided, a copy of the script will
#' be saved with the DDG.
#' @param ddgdir (optional) - the directory where the DDG should be saved.
#' If not provided, the DDG will be saved in a subdirectory called
#' "ddg" in the current working directory.
#' @param enable.console (optional) - if TRUE, console mode is turned on.
#' @param annotate.inside.functions (optional) - if TRUE, functions are annotated.
#' @param first.loop (optional) - the first loop to annotate in a for, while, or
#' repeat statement.
#' @param max.loops (optional) - the maximum number of loops to annotate in a for,
#' while, or repeat statement. If max.loops = -1 there is no limit.
#' If max.loops = 0, no loops are annotated.  If non-zero, if-statements
#' are also annotated.
#' @param max.snapshot.size (optional) - the maximum size for objects that
#' should be output to snapshot files. If 0, no snapshot files are saved.
#' If -1, all snapshot files are saved. Size in kilobytes.  Note that
#' this tests the size of the object that will be turned into a
#' snapshot, not the size of the resulting snapshot.
#' @paramoverwrite (optional) - default TRUE, if FALSE, generates
#' timestamp for ddg directory
#' @param save.hashtable (optional) - If TRUE, save ddg information to hashtable.json.
#' @param hash.algorithm (optional) - If save.hashtable is true, this allows the caller to 
#' select the hash algorithm to use.  This uses the digest function from the digest package.
#' The choices are md5, which is also the default, sha1, crc32, sha256, sha512, xxhash32, xxhash64 and murmur32.
#'
#' @return nothing

ddg.init <- function(r.script.path = NULL, ddgdir = NULL, overwrite = TRUE, enable.console = TRUE, annotate.inside.functions = TRUE, first.loop = 1, max.loops = 1, max.snapshot.size = 10,
                     save.hashtable = TRUE, hash.algorithm="md5") {
   .ddg.init.tables()

  # Save hash algorithm
   .ddg.set (".ddg.hash.algorithm", hash.algorithm)
  
  # Set directory for provenance graph. The base directory is set as follows:
  # (1) the directory specified by the user in the parameter ddgdir in ddg.init, or
  # (2) the directory specified by the user as an option for the value of
  # provdir, or (3) the R session temporary directory.  The provenance graph is stored
  # in a subdirectory called "prov_console" in console mode or "prov_[script name]" 
  # in script mode. If overwrite = FALSE, a timestamp is added to the directory name.

  # Directory specified by ddgdir in ddg.init
  if (!is.null(ddgdir)) {
    base.dir <- ddgdir
  
  # Directory specified as an option for prov.dir
  } else if (!is.null(getOption("prov.dir")) && getOption("prov.dir") != "") {
    base.dir <- getOption("prov.dir")

  # R session temporary directory
  } else {
    # Normalize path
    base.dir <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
  }

  # Remove final slash if present
  if (substr(base.dir, nchar(base.dir), nchar(base.dir)) == "/") base.dir <- substr(base.dir, 1, nchar(base.dir)-1)

  # Console mode
  if (is.null(r.script.path)) {
    ddg.path <- paste(base.dir, "/prov_console", sep="")
 
  # Script mode
  } else {
    ddg.path <- paste(base.dir, "/prov_", basename(file_path_sans_ext(r.script.path)), sep="")
  }

  # Add timestamp if overwrite = FALSE
  if (!overwrite) ddg.path <- paste(ddg.path, "_", .ddg.timestamp(), sep="")

  # Create directory if it does not exist
  if (!dir.exists(ddg.path)) dir.create(ddg.path, recursive = TRUE)

  .ddg.set("ddg.path", ddg.path)

  # Remove files from DDG directory
  ddg.flush.ddg()

  # Create DDG directories
  .ddg.init.environ()

  # Reset r.script.path if RMarkdown file

  if (!is.null(r.script.path) && file_ext(r.script.path) == "Rmd") {
    output.path <- paste(.ddg.path.scripts(), "/", basename(file_path_sans_ext(r.script.path)), ".R", sep = "")
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

    # Store maximum snapshot size.
    .ddg.set("ddg.max.snapshot.size", max.snapshot.size)
  }
  
  .ddg.init.loops (first.loop, max.loops)
  
  # Store time when script begins execution.
  .ddg.set("ddg.start.time", .ddg.timestamp())
  
  # Initialize the I/O tracing code
  .ddg.init.iotrace ()
  
  invisible()
}

#' @export ddg.save saves the current provenance graph
#'
#' @param r.script.path (optional) - Path to the R script.
#' @param save.debug (optional) - If TRUE, save debug files to debug directory.
#' Used in console mode.
#' @param quit (optional) - If TRUE, remove all DDG files from memory.
#'
#' @return nothing

ddg.save <- function(r.script.path = NULL, save.debug = FALSE, quit = FALSE) {
  if (!.ddg.is.init()) return(invisible())
  
  if (interactive() && .ddg.enable.console()) {
    # Get the final commands
    .ddg.console.node()
  }

  # If there are any connections still open when the script ends,
  # create nodes and edges for them.
  .ddg.create.file.nodes.for.open.connections ()

  # If there is a display device open, grab what is on the display
  if (length(dev.list()) >= 1) {
    #print("ddg.save: Saving graphics open at end of script")
    tryCatch (.ddg.capture.graphics(called.from.save = TRUE),
        error = function (e) print(e))
  }
  
  .ddg.stop.iotracing()

  # Save ddg.json to file.
  ddg.json.write()
  if (interactive()) print(paste("Saving ddg.json in ", .ddg.path(), sep=""))

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
  .ddg.init.statements ()

  # Clear loop information from ddg environment.
  .ddg.clear.loops ()

  # I don't think save is ever called with quit = TRUE, but we might want
  # to distinguish between the final call to ddg.save and a call the user
  # might make from the console.  Perhaps much of what is above should be
  # inside the quit branch instead.  Reconsider this when working ong 
  # console mode.
  # By convention, this is the final call to ddg.save.
  if (quit) {
    # Restore history settings.
    if (.ddg.is.set('ddg.original.hist.size')) Sys.setenv("R_HISTSIZE"=.ddg.get('ddg.original.hist.size'))

    # Delete temporary files.
    .ddg.delete.temp()

    # Shut down the DDG.
    .ddg.clear()
  }

  invisible()
}

#' @export ddg.run initiates execution of a script
#'
#' @param r.script.path (optional) - the full path to the R script.
#' If provided, a copy of the script will be saved with the DDG.
#' If only r.script.path is provided, the script is sourced using
#' ddg.source and a DDG is created for the script.
#' @param ddgdir (optional) - the directory where the DDG will be saved.
#' If not provided, the DDG will be saved in a directory called
#' "ddg" in the current working directory.
#' @param overwrite (optional) - if TRUE, the ddg is overwritten each time
#' the script is executed.
#' @param f (optional) - a function to run. If supplied, the function f
#' is executed with calls to ddg.init and ddg.save so that
#' provenance for the function is captured.
#' @param enable.console (optional) - if TRUE, console mode is turned on.
#' @param annotate.inside.functions (optional) - if TRUE, functions are annotated.
#' @param first.loop (optional) - the first loop to annotate in a for, while, or
#' repeat statement.
#' @param max.loops (optional) - the maximum number of loops to annotate in a for,
#' while, or repeat statement. If max.loops = -1 there is no limit.
#' If max.loops = 0, no loops are annotated.  If non-zero, if-statements
#' are also annotated.
#' @param max.snapshot.size (optional) - the maximum size for objects that
#' should be output to snapshot files. If 0, no snapshot files are
#' saved. If -1, all snapshot files are saved.  Size in kilobytes.
#' Note that this tests the size of the object that will be turned
#' into a snapshot, not the size of the resulting snapshot.
#' @param save.debug (optional) - If TRUE, save debug files to debug directory.
#' @param save.hashtable (optional) - If TRUE, save ddg information to hashtable.json.
#' @param hash.algorithm (optional) - If save.hashtable is true, this allows the caller to 
#' select the hash algorithm to use. This uses the digest function from the digest package.
#' The choices are md5, which is also the default, sha1, crc32, sha256, sha512, xxhash32, xxhash64 and murmur32.
#'
#' @return nothing

ddg.run <- function(r.script.path = NULL, ddgdir = NULL, overwrite = TRUE, f = NULL, enable.console = TRUE, annotate.inside.functions = TRUE, first.loop = 1, max.loops = 1, max.snapshot.size = 10, save.debug = FALSE, display = FALSE, 
                    save.hashtable = TRUE, hash.algorithm="md5") {
  
  # Initiate ddg.
  ddg.init(r.script.path, ddgdir, overwrite, enable.console, annotate.inside.functions, first.loop, max.loops, max.snapshot.size, save.hashtable, hash.algorithm)
  
  # Set .ddg.is.sourced to TRUE if script provided.
  .ddg.set(".ddg.is.sourced", !is.null(r.script.path))

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
        ddg.save(r.script.path)
        if(display==TRUE){
          ddg.display()
        }
      }
  )
  
  invisible()
}

#' @export ddg.source sources a script & collects provenance
#'
#' @param ddg.source reads in an R script and executes it in the provided
#' enviroment. ddg.source essentially mimics the behaviour of the
# 'R source command, having similar input parameters and results,
# 'but with additional parameters ignore.ddg.calls and ignore.init.
#' @param file - the name of the R script file to source.
#' @param ddgdir (optional) - the directory where the DDG will be saved.
#' If not provided, the DDG will be saved in a directory called "ddg"
#' in the current working directory.
#' @param local (optional) - the environment in which to evaluate parsed
#' expressions. If TRUE, the environment from which ddg.source is
#' called. If FALSE, the user's workspace (global environment).
#' @param echo (optional) - print each expression after parsing.
#' @param print.eval (optional) - print result of each evaluation.
#' @param verbose (optional) - print extra diagnostics.
#' @parammax.deparse.length (optional) - maximum number of characters
#' output for deparse of a single expression.
#' @param chdir (optional) - change R working directory temporarily to
#' the directory containing the file to be sourced.
#' @param encoding (optional) - encoding to be assumed when file is a
#' character string.
#' @param ignore.ddg.calls (optional) - if TRUE, ignore DDG function calls.
#' @param ignore.init (optional) - if TRUE, ignore ddg.init and ddg.run.
#' @param force.console (optional) - if TRUE, turn console mode on.
#'
#' @return nothing

ddg.source <- function (file,  ddgdir = NULL, local = FALSE, echo = verbose, print.eval = echo,
	verbose = getOption("verbose"), max.deparse.length = 150, chdir = FALSE, encoding = getOption("encoding"),
	ignore.ddg.calls = TRUE, ignore.init = ignore.ddg.calls, force.console=ignore.init){

	# Store script number & name.
  sname <- basename(file)
  snum <- .ddg.store.script.info (sname)
  
  # Save a copy of the script
  file.copy(file, paste(.ddg.path.scripts(), basename(sname), sep="/"))

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
	if (!missing(echo)) 
	{
		if (!is.logical(echo))
			stop("'echo' must be logical")
		if (!echo && verbose) 
		{
			warning("'verbose' is TRUE, 'echo' not; ... coercing 'echo <- TRUE'\n")
			echo <- TRUE
		}
	}

	# Print extra information about environment.
	if (verbose) 
	{
		cat("'envir' chosen:")
		print(envir)
	}

	# Parse input file and figure out encoding.
	ofile <- file
	from_file <- FALSE
	srcfile <- NULL
	if (is.character(file)) 
	{
		if (identical(encoding, "unknown")) 
		{
			enc <- utils::localeToCharset()
			encoding <- enc[length(enc)]
		}
		else enc <- encoding
		if (length(enc) > 1L) 
		{
			encoding <- NA
			owarn <- options("warn")
			options(warn = 2)
			for (e in enc) 
			{
				if (is.na(e))
					next
				zz <- file(file, encoding = e)
				res <- tryCatch(readLines(zz, warn = FALSE), error = identity)
				close(zz)
				if (!inherits(res, "error")) 
				{
					encoding <- e
					break
				}
			}
			options(owarn)
		}
		if (is.na(encoding))
			stop("unable to find a plausible encoding")
		if (verbose)
			cat(gettextf("encoding = \"%s\" chosen", encoding), "\n", sep = "")
		if (file == "") 
		{
			filename <- "stdin"
			file <- stdin()
			srcfile <- "<stdin>"
		}
		else 
		{
			filename <- file
			file <- file(filename, "r", encoding = encoding)
			on.exit(close(file))
			lines <- readLines(file, warn = FALSE)

			on.exit()
			close(file)
			srcfile <- srcfilecopy(filename, lines, file.info(filename)[1, "mtime"], isFile = TRUE)
		}
		loc <- utils::localeToCharset()[1L]
		encoding <- if (have_encoding)
				switch(loc, `UTF-8` = "UTF-8", `ISO8859-1` = "latin1", "unknown")
        	else "unknown"
	}

	else 
	{
		filename <- "Connection"
		lines <- readLines(file, warn = FALSE)

		srcfile <- srcfilecopy(deparse(substitute(file)), lines)
	}

	# Parse the expressions from the file.
	exprs <- if (!from_file) 
		{
			if (length(lines)) 
				parse(stdin(), n = -1, lines, "?", srcfile, encoding, keep.source=TRUE)
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
	if (chdir) 
	{
		if (is.character(ofile)) 
		{
			isURL <- length(grep("^(ftp|http|file)://", ofile)) > 0L
			if (isURL)
				warning("'chdir = TRUE' makes no sense for a URL")
			if (!isURL && (path <- dirname(ofile)) != ".") 
			{
				owd <- getwd()
				if (is.null(owd)) 
				{
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
	if(length(exprs) > 0) 
	{
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

#' @export ddg.json returns the current provenance graph as a prov-json string
#'
#' @return the current provenance graph
#' @returntype prov-json string

ddg.json <- function()
{
	# CONSTANTS
	TOOL.NAME <- "RDataTracker"
	JSON.VERSION <- "2.1"
	
	# contents of the prefix node
	PREFIX.NODE <- list( "prov" = "http://www.w3.org/ns/prov#" ,
						 "rdt" = "http://rdatatracker.org/" )
	
	# the namespace prefix appended to the name for each node or edge
	LABEL.PREFIX <- "rdt:"
	
	# the name/character denoting the type of node or edge
	LABEL.NAMES <- list( "agent" = "a" ,
						 "activity.proc" = "p" ,
						 "entity.data" = "d" ,
						 "entity.env" = "environment" ,
						 "entity.lib" = "l" ,
						 "entity.func" = "f" ,
						 "wasInformedBy.p2p" = "pp" ,
						 "wasGeneratedBy.p2d" = "pd" ,
						 "used.d2p" = "dp" ,
						 "used.f2p" = "fp" ,
						 "hadMember" = "m" )
	
	
	# this list is a container for each separate part that forms the json string
	json <- list( "prefix" = NA ,
				  "agent" = NA ,
				  "activity.proc" = NA ,
				  "entity.data" = NA , 
				  "entity.env" = NA , 
				  "entity.lib" = NA , 
				  "entity.func" = NA ,
				  "wasInformedBy.p2p" = NA ,
				  "wasGeneratedBy.p2d" = NA ,
				  "used.d2p" = NA , 
				  "used.f2p" = NA ,
				  "hadMember" = NA )
	
	# prefix
	json$prefix <- .ddg.json.prefix( PREFIX.NODE )
	
	# agent (about the tool that produced the json & the json version)
	json$agent <- .ddg.json.agent( TOOL.NAME , JSON.VERSION , LABEL.NAMES$agent , LABEL.PREFIX )
	
	# activity (proc nodes)
	json$activity.proc <- .ddg.json.proc( LABEL.NAMES$activity.proc , LABEL.PREFIX )
	
	# entity: data nodes
	json$entity.data <- .ddg.json.data( LABEL.NAMES$entity.data , LABEL.PREFIX )
	
	# entity: environment
	json$entity.env <- .ddg.json.env( LABEL.NAMES$entity.env , LABEL.PREFIX )
	
	
	# EDGE TABLE NODES
	edges <- subset( .ddg.edges() , ddg.num > 0 )
	
	# wasInformedBy (proc2proc)
	json$wasInformedBy.p2p <- .ddg.json.proc2proc( edges , LABEL.NAMES$wasInformedBy.p2p , LABEL.PREFIX )
	
	# wasGeneratedBy (proc2data)
	json$wasGeneratedBy.p2d <- .ddg.json.proc2data( edges , LABEL.NAMES$wasGeneratedBy.p2d , LABEL.PREFIX )
	
	
	# get function nodes
	calls <- .ddg.function.nodes()
	num.calls <- nrow(calls)
	
	
	# used: data2proc
	json$used.d2p <- .ddg.json.data2proc( edges , LABEL.NAMES$used.d2p , LABEL.PREFIX )
	
	
	# LIBRARY NODES - change row numbers
	libraries <- .ddg.installedpackages()
	rownames(libraries) <- c( 1 : nrow(libraries) )
	
	# PRINT TO JSON - LIBRARY NODES
	json$entity.lib <- .ddg.json.lib( libraries , LABEL.NAMES$entity.lib , LABEL.PREFIX )
	
	
	# FUNCTION NODES - get function numbers if there are any function nodes
	if( num.calls > 0 )
	{
		functions <- calls[ , 2:3]
		functions <- unique(functions)
		
		rownames(functions) <- c( 1 : nrow(functions) )
		
		# PRINT TO JSON - FUNCTION NODES
		json$entity.func <- .ddg.json.func( functions , LABEL.NAMES$entity.func , LABEL.PREFIX )
		
		
		# MERGE TABLES: function calls, functions, libraries
		# library nodes - change col names, add lnum column for merging
		colnames(libraries) <- c( "ddg.lib" , "ddg.lib.version" )
		libraries <- cbind( "ddg.lnum" = c(1:nrow(libraries)) , libraries )
		
		# function nodes - add fnum column for merging
		functions <- cbind( "ddg.fnum" = c(1:nrow(functions)) , functions )
		
		# function calls - add cnum column for ordering
		calls <- cbind( "ddg.cnum" = c(1:nrow(calls)) , calls )
		
		# merge tables
		calls <- merge( calls , libraries , by.x = "ddg.lib" )
		calls <- merge( calls , functions , by = c("ddg.fun","ddg.lib") )
		
		# order table by cnum
		calls <- calls[ order(calls$ddg.cnum) , ]
		rownames(calls) <- calls$ddg.cnum
		
		
		# PRINT TO JSON: func2proc
		json$used.f2p <- .ddg.json.func2proc( calls , LABEL.NAMES$used.f2p , LABEL.NAMES$entity.func , 
											  LABEL.NAMES$activity.proc , LABEL.PREFIX )
		
		# PRINT TO JSON: func2lib
		json$hadMember <- .ddg.json.lib2func( calls , LABEL.NAMES$hadMember , LABEL.NAMES$entity.lib , 
											  LABEL.NAMES$entity.func , LABEL.PREFIX )
	}	
	
	# COMBINE INTO COMPLETE JSON
	return(.ddg.json.combine(json) )
}

