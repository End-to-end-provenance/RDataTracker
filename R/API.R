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
#' @param hash.algorithm (optional) - If save.hashtable is true, this allows the caller to 
#' select the hash algorithm to use.  This uses the digest function from the digest package.
#' The choices are md5, which is also the default, sha1, crc32, sha256, sha512, xxhash32, xxhash64 and murmur32.
#'
#' @return nothing

ddg.init <- function(r.script.path = NULL, ddgdir = NULL, overwrite = TRUE, enable.console = TRUE, annotate.inside.functions = TRUE, first.loop = 1, max.loops = 1, max.snapshot.size = 10,
                     hash.algorithm="md5") {
  .ddg.init.tables()

  # Save hash algorithm
  .ddg.set (".ddg.hash.algorithm", hash.algorithm)
  
  .ddg.set.path (ddgdir, r.script.path, overwrite)
  
  # Remove files from DDG directory
  ddg.flush.ddg()

  # Create DDG directories
  .ddg.init.environ()

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
  .ddg.set.enable.console (enable.console)
  .ddg.set.details.omitted(FALSE)

  .ddg.init.history.file ()

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
  
  # Mark graph as initilized.
  .ddg.set(".ddg.initialized", TRUE)
  
  invisible()
}

#' Sets the path where the DDG will be stored.  It creates the directory if it
#' does not currently exist.
#' 
#' The base directory is set as follows:
#' (1) the directory specified by the user in the parameter ddgdir, or
#' (2) the directory specified by the user as the value of the option "prov.dir",
#' or (3) the R session temporary directory. If the directory specified by the user
#' is a period (.), the base directory is set to the current working directory.
#'
#' The provenance graph is stored in a subdirectory of the base directory called 
#' "prov_console" in console mode or "prov_[script name]" in script mode. If overwrite = 
#' FALSE, a timestamp is added to the directory name.
#' 
#' @param ddgdir name of directory.  This can be a directory name, ".", or NULL.
#' @param r.script.path the path to the R script.  If NULL, we are running from the console.
#' @param overwrite If FALSE, a timestamp is added to the directory name
#' @returnType string
#' @return the name of the directory where the ddg should be stored
.ddg.set.path <- function (ddgdir, r.script.path, overwrite) {
  
  # Directory specified by ddgdir parameter
  if (!is.null(ddgdir)) {
    if (ddgdir == ".") {
      base.dir <- getwd()
    } else {
      base.dir <- ddgdir
    }
  } 
  
  else {
    # Directory specified as an option for prov.dir
    prov.dir.option <- getOption("prov.dir")
    if (!is.null(prov.dir.option) && prov.dir.option != "") {
      if (prov.dir.option == ".") {
        base.dir <- getwd()
      } else {
        base.dir <- getOption("prov.dir")
      }
    } 
    
    # R session temporary directory
    else {
      # Normalize path
      base.dir <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
    }
  }
  
  # Remove final slash if present
  base.dir <- sub("/$", "", base.dir)
  
  # Console mode
  if (is.null(r.script.path)) {
    ddg.path <- paste(base.dir, "/prov_console", sep="")
    
    # Script mode
  } else {
    ddg.path <- paste(base.dir, "/prov_", basename(tools::file_path_sans_ext(r.script.path)), sep="")
  }
  
  # Add timestamp if overwrite = FALSE
  if (!overwrite) ddg.path <- paste(ddg.path, "_", .ddg.timestamp(), sep="")
  
  # Create directory if it does not exist
  if (!dir.exists(ddg.path)) dir.create(ddg.path, recursive = TRUE)
  
  .ddg.set("ddg.path", ddg.path)
}

#' ddg.flush.ddg removes all files from the DDG directories unless the
#'   the DDG directory is the working directory. If no DDG directory is
#'   specified, the current DDG directory is assumed.
#' 
#' @param ddg.path (optional) path to DDG directory.
#' 
.ddg.flush.ddg <- function(ddg.path=NULL) {
  # TODO:  When ddg.flush.ddg is removed (from Obsolete.R, we
  # can remove the ddg.path parameter and update the 
  # code below to always look up the path.
  
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

#' ddg.save saves the current provenance graph
#'
#' @param save.debug (optional) - If TRUE, save debug files to debug directory.
#' Used in console mode.
#' @param quit (optional) - If TRUE, remove all DDG files from memory.
#'
#' @return nothing
#' @export
ddg.save <- function(save.debug = FALSE, quit = FALSE) {
  if (!.ddg.is.init()) return(invisible())
  
  # Get the final commands
  .ddg.console.node()

  # By convention, this is the final call to ddg.save.  This is
  # called at the end of ddg.run, and the user should call ddg.save
  # with quit = true when they want to finalize a ddg run from
  # the console
  if (quit) {
    # If there are any connections still open when the script ends,
    # create nodes and edges for them.
    .ddg.create.file.nodes.for.open.connections ()
    
    # If there is a display device open, grab what is on the display
    if (length(grDevices::dev.list()) >= 1) {
      tryCatch (.ddg.capture.graphics(called.from.save = TRUE),
          error = function (e) print(e))
    }
    
    .ddg.stop.iotracing()
    
    # Restore history settings.
    .ddg.restore.history.size()

    # Delete temporary files.
    .ddg.delete.temp()

    # Shut down the DDG.
    #.ddg.clear()
    # Mark graph as initilized.
    .ddg.set(".ddg.initialized", FALSE)
    
  }
  
  # Save ddg.json to file.
  ddg.json.write()
  if (interactive()) print(paste("Saving ddg.json in ", .ddg.path(), sep=""))
  
  # Save debug files to debug directory.
  if (save.debug || .ddg.save.debug()) {
    .ddg.save.debug.files()
  }
  
  # Clear DDGStatements from ddg environment.  
  # Should this be only inside the quit if-statement?
  .ddg.init.statements ()
  
  # Clear loop information from ddg environment.
  # Should this be only inside the quit if-statement?
  .ddg.clear.loops ()

  invisible()
}

#' ddg.run initiates execution of a script, collecting provenance as it executes
#'
#' @param r.script.path (optional) - the full path to the R script.
#' If provided, a copy of the script will be saved with the DDG.
#' If only r.script.path is provided, the script is sourced using
#' ddg.source and a DDG is created for the script.
#' @param ddgdir (optional) - the directory where the DDG will be saved.
#' If not provided, the DDG will be saved in a directory inside R's
#' temporary directory
#' @param overwrite (optional) - if TRUE, the ddg is overwritten each time
#' the script is executed.  If FALSE, a timestamp is attached to the ddg
#' folder name so that the provenance of earlier runs is not overwritten.
#' @param f (optional) - a function to run. If supplied, the function f
#' is executed with calls to ddg.init and ddg.save so that
#' provenance for the function is captured.  Exactly one of f and r.script.path
#' should be provided.
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
#' @param hash.algorithm (optional) - If save.hashtable is true, this allows the caller to 
#' select the hash algorithm to use. This uses the digest function from the digest package.
#' The choices are md5, which is also the default, sha1, crc32, sha256, sha512, xxhash32, xxhash64 and murmur32.
#'
#' @return nothing
#' @export
ddg.run <- function(r.script.path = NULL, ddgdir = NULL, overwrite = TRUE, f = NULL, enable.console = TRUE, annotate.inside.functions = TRUE, first.loop = 1, max.loops = 1, max.snapshot.size = 10, save.debug = FALSE, display = FALSE, 
                    hash.algorithm="md5") {
  
  # Initialize ddg.
  ddg.init(r.script.path, ddgdir, overwrite, enable.console, annotate.inside.functions, first.loop, max.loops, max.snapshot.size, hash.algorithm)
  
  # Set .ddg.is.sourced to TRUE if script provided.
  .ddg.set(".ddg.is.sourced", !is.null(r.script.path))

  # Save debug files to debug directory.
  .ddg.set("ddg.save.debug", save.debug)

  # If an R error is generated, get the error message and close
  # the DDG.
  tryCatch(
    if (!is.null(r.script.path)) ddg.source(
         .ddg.get("ddg.r.script.path"),
          ignore.ddg.calls = FALSE,
          force.console = FALSE)
    else if (!is.null(f)) f()
    else stop("r.script.path and f cannot both be NULL"),

    finally={
      ddg.save(quit=TRUE)
      if(display==TRUE){
        ddg.display()
      }
    }
  )
  
  invisible()
}

#' ddg.source sources a script & collects provenance
#'
#' @param ddg.source reads in an R script and executes it in the provided
#' enviroment. ddg.source essentially mimics the behaviour of the
# 'R source command, having similar input parameters and results,
# 'but with additional parameters ignore.ddg.calls and force.console.
#' @param file - the name of the R script file to source.
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
#' @param force.console (optional) - if TRUE, turn console mode on.
#'
#' @return nothing
#' @export
ddg.source <- function (file,  local = FALSE, echo = verbose, print.eval = echo,
	verbose = getOption("verbose"), max.deparse.length = 150, chdir = FALSE, encoding = getOption("encoding"),
	ignore.ddg.calls = TRUE, force.console=TRUE){

	# Store script number & name.
  sname <- basename(file)
  snum <- .ddg.store.script.info (sname)
  
  # Save a copy of the script
  file.copy(file, paste(.ddg.path.scripts(), basename(sname), sep="/"))

	### CODE IN THIS SECTION IS A SLIGHT MODIFICATION OF A PORTION OF R's source FUNCTION ###
  # To see the current version of the source function source code, say:
  # getMethod("source", "ANY")
  # I don't know how to see the functions called from source, though.
  # An older version of the source code of R's source function can be found here:
  # https://github.com/SurajGupta/r-source/blob/master/src/library/base/R/source.R
  # Note that R's source function has more parameters than we allow.  This could
  # conceivably cause problems when we replace calls to source with calls to ddg.source.
  # The additional parameters are:
  # exprs
  # spaced
  # prompt.echo
  # width.cutoff
  # deparseCtrl
  # continue.echo
  # skip.echo
  # keep.source


	# Get the environment under which the script should be executed.
	envir <- if (isTRUE(local)) {
			parent.frame()
		}
		else if (isFALSE(local)) {
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
			owarn <- options(warn = 2)
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
			srcfile <- srcfilecopy(filename, lines, file.mtime(filename)[1], isFile = TRUE)
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
      if (grepl("^(ftp|http|file)://", ofile))
				warning("'chdir = TRUE' makes no sense for a URL")
			else if ((path <- dirname(ofile)) != ".") 
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
	# Ignore calculation of certain execution steps.
	ignores <- c("^library[(]RDataTracker[)]$",
		if(ignore.ddg.calls) "^ddg."
		else c("^ddg.init", "^ddg.run"))

	# Now we can parse the commands as we normally would for a DDG.
	if(length(exprs) > 0) 
	{
		# Turn on the console if forced to, keep track of previous
		# setting, parse previous commands if necessary.
		prev.on <- .ddg.is.init() && .ddg.enable.console()
		if (prev.on) .ddg.console.node()
		if (force.console) ddg.console.on()

		# Let library know that we are sourcing a file.
		prev.source <- .ddg.is.init() && .ddg.enable.source()

		# Initialize the tables for ddg.capture.
		.ddg.set("from.source", TRUE)

		# Parse the commands into a console node.
		.ddg.parse.commands(exprs, sname, snum, environ=envir, ignore.patterns=ignores, node.name=sname,
			echo = echo, print.eval = print.eval, max.deparse.length = max.deparse.length,
			run.commands = TRUE)

		# Restore previous state
    .ddg.set("from.source", prev.source)
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
	# This is a wrapper function.
	# Calls and returns the function with the bulk of the code in OutputJSON.R
	return( .ddg.json.string() )
}

.ddg.start.ddg.explorer <- function () {
  jar.path<- "/RDataTracker/java/DDGExplorer.jar"
  check.library.paths<- file.exists(paste(.libPaths(),jar.path,sep = ""))
  index<- min(which(check.library.paths == TRUE))
  ddgexplorer_path<- paste(.libPaths()[index],jar.path,sep = "")
  ddgjson.path<- paste(.ddg.path() ,"ddg.json",sep = "/")
  # ddgjson.path<- paste(getwd(), .ddg.path() ,"ddg.json",sep = "/")
  
  # -s flag starts DDG Explorer as a server.  This allows each new ddg to show
  # up in a new tab of an existing running DDG Explorer.
  # print("Starting DDG Explorer server")
  systemResult <- system2("java", c("-jar", ddgexplorer_path, ddgjson.path, "-port", .ddg.get(".ddg.explorer.port")), wait = FALSE)
  # print(paste("Starting java server return code:", systemResult))
}

# ddg.display loads & displays the current DDG.

ddg.display <- function () {
  
  # See if the server is already running
  # print("Opening socket connection")
  tryCatch ({
        con <- socketConnection(host= "localhost", port = .ddg.get(".ddg.explorer.port"), blocking = FALSE,
            server=FALSE, open="w", timeout=1)
        ddgjson.path<- paste(.ddg.path() ,"ddg.json",sep = "/")
        # ddgjson.path<- paste(getwd(), .ddg.path() ,"ddg.json",sep = "/")
        # print ("Socket open; writing to socket")
        writeLines(ddgjson.path, con)
        # print ("Wrote to socket")
        close(con)
      },
      warning = function(e) {
        # print("Warning!")
        .ddg.start.ddg.explorer()
      }
  )
  
  invisible()
}

