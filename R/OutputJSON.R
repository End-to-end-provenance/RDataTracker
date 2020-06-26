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
#
########################### OutputJSON.R ############################

# This file contains the functions that prepare the prov-json string.


#' .ddg.json.write writes the prov-json string to file
#' @return nothing
#' @noRd

.ddg.json.write <- function() 
{
	fileout <- paste(.ddg.path(), "/prov.json", sep="")
	json <- .ddg.json.string()
	write(json, fileout)
}

#' .ddg.json.string creates and returns the prov-json string for the current 
#' provenance graph
#' @return the prov-json string
#' @noRd

.ddg.json.string <- function()
{
	# Display message & return NULL if no provenance graph is available
	if (is.null(.ddg.path())) {
		cat("No provenance graph is available.\n")
		return(NULL)
	}

	# CONSTANTS
	JSON.VERSION <- "2.3"

	# tool name
	tool.name <- .ddg.tool.name()
	
	# contents of the prefix node
	PREFIX.NODE <- list( "prov" = "http://www.w3.org/ns/prov#" ,
						 "rdt" = "https://github.com/End-to-end-provenance/ExtendedProvJson/blob/master/JSON-format.md" )
	
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
	json$agent <- .ddg.json.agent( tool.name , JSON.VERSION , LABEL.NAMES$agent , 
	                               LABEL.PREFIX )
	
	# activity (proc nodes)
	json$activity.proc <- .ddg.json.proc( LABEL.NAMES$activity.proc , LABEL.PREFIX )
	
	# entity: data nodes
	json$entity.data <- .ddg.json.data( LABEL.NAMES$entity.data , LABEL.PREFIX )
	
	# entity: environment
	json$entity.env <- .ddg.json.env( LABEL.NAMES$entity.env , LABEL.PREFIX )
	
	
	# EDGE TABLE NODES
	edges <- .ddg.edges()
	edges <- edges[edges$ddg.num > 0, ]
	
	# wasInformedBy (proc2proc)
	json$wasInformedBy.p2p <- .ddg.json.proc2proc( edges , 
	                                               LABEL.NAMES$wasInformedBy.p2p , 
	                                               LABEL.PREFIX )
	
	# wasGeneratedBy (proc2data)
	json$wasGeneratedBy.p2d <- .ddg.json.proc2data( edges , 
	                                                LABEL.NAMES$wasGeneratedBy.p2d , 
	                                                LABEL.PREFIX )
	
	
	# get function nodes
	calls <- .ddg.function.nodes()
	num.calls <- nrow(calls)
	
	
	# used: data2proc
	json$used.d2p <- .ddg.json.data2proc( edges , LABEL.NAMES$used.d2p , LABEL.PREFIX )
	
	# LIBRARY NODES - change row numbers
	libraries <- .ddg.installedpackages()
	rownames(libraries) <- c(1 : nrow(libraries))
	
	# PRINT TO JSON - LIBRARY NODES
	json$entity.lib <- .ddg.json.lib( libraries , LABEL.NAMES$entity.lib , LABEL.PREFIX )
	
	
	# FUNCTION NODES - get function numbers if there are any function nodes
	if( num.calls > 0 )
	{
		# extract columns: ddg.fun, ddg.lib (function names and their library names)
		functions <- calls[ , 2:3]
		functions <- unique(functions)
		
		rownames(functions) <- c(1 : nrow(functions))
		
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
		json$used.f2p <- .ddg.json.func2proc( calls , LABEL.NAMES$used.f2p , 
		                                      LABEL.NAMES$entity.func , 
		                                      LABEL.NAMES$activity.proc , LABEL.PREFIX )
		
		# PRINT TO JSON: func2lib
		json$hadMember <- .ddg.json.lib2func( calls , LABEL.NAMES$hadMember , 
		                                      LABEL.NAMES$entity.lib ,  
		                                      LABEL.NAMES$entity.func , LABEL.PREFIX )
	}	
	
	# COMBINE INTO COMPLETE JSON
	return(.ddg.json.combine(json) )
}

# --- HELPER FUNCTIONS ------------------------- #

#' .ddg.json.prefix forms and returns the json string for the prefix node
#' @param node prefix node
#' @return the json string for the prefix node
#' @noRd

.ddg.json.prefix <- function( node )
{
	# let the prefix node be named
	node <- list(node)
	names(node) <- "prefix"
	
	# format to json
	# with proper indentation, without sq brackets around urls
	json <- jsonlite::toJSON(node, auto_unbox = TRUE)
	json <- jsonlite::prettify(json, indent = 4)
	
	# change '    ' into a tab (\t)
	json <- gsub( '    ' , '\t', json )
	
	# remove end close brace and return
	return( sub('\n}\n$', '', json) )
}

#' .ddg.json.agent forms and returns the json string for the agent node
#' @param tool name of provenance collection tool
#' @param json.version json version number
#' @param label node label
#' @param prefix node prefix
#' @return the json string for the agent node
#' @noRd

.ddg.json.agent <- function( tool, json.version, label, prefix )
{
	# EF EDITS
	# GET CONTENT FOR NODE
	node <- list("tool.name" = tool ,
				 "tool.version" = toString(utils::packageVersion(tool)) ,
				 "json.version" = json.version ,
				 "args.names" = NA ,
				 "args.values" = NA ,
				 "args.types" = NA)
	
	# run arguments: names and values
	run.args <- .ddg.get("ddg.run.args")
	node$args.names <- run.args$args.names
	node$args.values <- run.args$args.values
	node$args.types <- run.args$args.types
	
	# CONVERT TO JSON
	json <- .ddg.json.list(node, "a1", prefix)
	
	# form agent node and return
	return( .ddg.json.formNode("agent", json) )
}

#' .ddg.json.proc forms and returns the json string for the procedure nodes
#' @param label node label
#' @param prefix node prefix
#' @return the json string for the procedure node
#' @noRd

.ddg.json.proc <- function( label, prefix )
{
	nodes <- .ddg.proc.nodes()
	
	# extract and order required columns
	nodes <- nodes[ , c("ddg.name", "ddg.type", "ddg.time", "ddg.snum", 
					 "ddg.startLine", "ddg.startCol", "ddg.endLine", "ddg.endCol")]
	
	# base case: no procedure nodes 
	if( nrow(nodes) == 0 )
		return(NA)
	
	# convert '    ' or \t to escaped tab characters, if any
	nodes <- .ddg.json.df.escape.tabs( nodes )
	
	# convert elapsedTime to strings
	nodes["ddg.time"] <- mapply( format , nodes["ddg.time"][[1]] , nsmall = 1L )
	
	# column names
	col.names <- c( "name", "type", "elapsedTime", "scriptNum", 
					"startLine", "startCol", "endLine", "endCol" )
	col.names <- mapply( paste , prefix , col.names , sep='' , USE.NAMES = FALSE )
	
	# convert to json
	prefix <- paste( prefix , label , sep='' )
	json <- .ddg.json.dataframe( nodes , col.names , prefix )
	
	# form activity node and return
	return( .ddg.json.formNode("activity", json) )
}

#' .ddg.json.data forms and returns the json string for the data nodes
#' @param label node label
#' @param prefix node prefix
#' @return the json string for the data node
#' @noRd

.ddg.json.data <- function( label, prefix )
{
	nodes <- .ddg.data.nodes()
	
	# extract and order required columns
	nodes <- nodes[ , c("ddg.name", "ddg.value", "ddg.val.type", "ddg.type",
					 "ddg.scope", "ddg.from.env", "ddg.hash", "ddg.time", "ddg.loc")]
	
	# base case: no data nodes
	if( nrow(nodes) == 0 )
		return(NA)
	
	# convert '    ' or \t to escaped tab characters, if any
	nodes <- .ddg.json.df.escape.tabs( nodes )
	
	# column names
	col.names <- c( "name", "value", "valType", "type", "scope", "fromEnv", 
					"hash", "timestamp", "location" )
	col.names <- mapply( paste , prefix , col.names , sep='' , USE.NAMES = FALSE )
	
	# convert to json and return
	prefix <- paste( prefix , label , sep='' )
	return( .ddg.json.dataframe(nodes, col.names, prefix) )
}

#' .ddg.json.env forms and returns the json string for the environment node
#' @param label node label
#' @param prefix node prefix
#' @return the json string for the environment node
#' @noRd

.ddg.json.env <- function( label, prefix )
{
	# GET CONTENT FOR NODE
	fields <- list( "name" = "environment" ,
					"architecture" = NA ,
					"operatingSystem" = NA ,
					"language" = NA ,
					"langVersion" = NA ,
					"script" = NA ,
					"scriptTimeStamp" = NA ,
					"totalElapsedTime" = NA ,
					"sourcedScripts" = NA ,
					"sourcedScriptTimeStamps" = NA ,
					"workingDirectory" = NA ,
					"provDirectory" = NA ,
					"provTimestamp" = NA )
	
	# architecture, language, langVersion
	lang.version <- R.Version()
	
	fields$architecture <- utils::sessionInfo()$platform
	fields$language <- lang.version$language
	fields$langVersion <- lang.version$version
	
	# operating system
	fields$operatingSystem <- 
		if (.Platform$OS.type == "unix") utils::sessionInfo()$running
		else if (.Platform$OS.type == "windows") utils::win.version()
		else version$os
	
	# script variables
	script.path <- .ddg.r.script.path()
	
	if( ! is.null(script.path) )
	{
		fields$script <- script.path 
		fields$scriptTimeStamp <- .ddg.format.time( file.info(script.path)$mtime )
	}
	else
	{
		fields$script <- ""
		fields$scriptTimeStamp <- ""
	}
	
	sourced.scripts <- .ddg.json.sourced.scripts()
	fields$sourcedScripts <- sourced.scripts[[1]]
	fields$sourcedScriptTimeStamps <- sourced.scripts[[2]]
	
	# record total elapsed time
	fields$totalElapsedTime <- format(.ddg.total.elapsed.time(), nsmall = 1L)
	
	# working directory, ddg directory (escape any tab characters)
	fields$workingDirectory <- .ddg.json.escape.tabs( getwd() )
	fields$provDirectory <- .ddg.json.escape.tabs( .ddg.path() )
	
	# ddg timestamp
	fields$provTimestamp <- .ddg.get("ddg.start.time")
	
	# hash algorithm
	if (.ddg.is.set ("ddg.hash.algorithm")) {
		fields <- append (fields, list (hashAlgorithm = .ddg.get("ddg.hash.algorithm")))
	}
	
	# CONVERT TO JSON
	return( .ddg.json.list(fields, "environment", prefix) )
}

# .ddg.json.sourced.scripts return the names of other scripts that were sourced 
#' and their timestamps.
#' @return the names and timestamps of other sourced scripts
#' @noRd

.ddg.json.sourced.scripts <- function() 
{
	script.names <- ""
	script.times <- ""
	
	ss <- .ddg.sourced.scripts()
	
	# first row: main script
	if( (! is.null(ss)) && (nrow(ss) > 1) )
	{
		ss <- ss[ ss$snum > 1 , ]
		
		script.names <- sapply( ss[ , 2] , .ddg.json.escape.tabs )
		script.times <- ss[ , 3]
	}
	
	return( list(script.names, script.times) )
}

#' .ddg.json.lib forms and returns the json string for the library nodes
#' @param nodes library nodes
#' @param label node label
#' @param prefix node prefix
#' @return the json string for the library nodes
#' @noRd

.ddg.json.lib <- function( nodes, label, prefix )
{
	# change col names
	col.names <- c( "name" , "version" )
	
	# convert '    ' or \t to escaped tab characters, if any
	nodes <- .ddg.json.df.escape.tabs( nodes )
	
	# convert to json
	prefix <- paste( prefix , label , sep='' )
	json <- .ddg.json.dataframe( nodes , col.names , prefix )
	
	# append prov:type to json
	prov.type <- .ddg.json.collection()
	json <- gsub( '\n\t\t}' , prov.type , json )
	
	return( json )
}

#' .ddg.json.collection forms and returns the json string for the type node 
#' for a collection
#' @return the json string for the type node for a collection
#' @noRd

.ddg.json.collection <- function()
{
	prov.type <- list("prov:Collection", "xsd:QName")
	names(prov.type) <- c("$", "type")
	
	# let prov:type node be named
	prov.type <- list(prov.type)
	names(prov.type) <- "prov:type"
	
	# convert  to json
	json <- jsonlite::toJSON(prov.type, auto_unbox = TRUE)
	json <- jsonlite::prettify(json, indent = 4)
	
	# convert '    ' to tab
	json <- gsub( '    ' , '\t', json )
	
	# remove newline at the end
	json <- sub( '\n$' , '' , json )
	
	# indent by 2 levels
	json <- gsub( '\n' , '\n\t\t' , json )
	
	# change first `{` to a `,` character for appending to library nodes, return
	return( sub('^\\{', ',', json) )
}

#' .ddg.json.func forms and returns the json string for function nodes
#' @param nodes function nodes
#' @param label node label
#' @param prefix node prefix
#' @return the json string for function nodes
#' @noRd

.ddg.json.func <- function( nodes, label, prefix )
{
	# extract names of functions
	nodes <- nodes[ , "ddg.fun"]
	
	# convert to data frame
	nodes <- data.frame( nodes , stringsAsFactors = FALSE )
	
	# convert '    ' or \t to escaped tab characters, if any
	nodes <- .ddg.json.df.escape.tabs( nodes )
	
	# convert to json, return
	prefix <- paste( prefix , label , sep='' )
	return( .ddg.json.dataframe(nodes, "name", prefix) )
}

#' .ddg.json.proc2proc forms and returns the json string for nodes representing 
#' procedure-to-procedure edges
#' @param edges edge nodes
#' @param label node label
#' @param prefix node prefix
#' @return the json string for procedure-to-procedure edges
#' @noRd

.ddg.json.proc2proc <- function( edges, label, prefix )
{
	# extract procedure-to-procedure edges, where ddg.type is 'cf' (control flow)
	edges <- edges[edges$ddg.type == "cf", c("ddg.from", "ddg.to")]
	
	# case: no proc-to-proc edges
	if( nrow(edges) == 0 )
		return(NA)
	
	# add prefix to node numbers
	edges$ddg.from <- mapply( paste , prefix , edges$ddg.from , sep='' , USE.NAMES=FALSE )
	edges$ddg.to <- mapply( paste , prefix , edges$ddg.to , sep='' , USE.NAMES=FALSE )
	
	# column names
	col.names <- c("prov:informant", "prov:informed")
	
	# convert to json
	prefix <- paste( prefix , label , sep='' )
	json <- .ddg.json.dataframe( edges, col.names, prefix )
	
	# form wasInformedBy node, return
	return( .ddg.json.formNode("wasInformedBy", json) )
}

# .ddg.json.proc2data forms and returns the json string for nodes representing 
#' procedure-to-data edges
#' @param edges edge nodes
#' @param label node label
#' @param prefix node prefix
#' @return the json string for procedure-to-data edges
#' @noRd

.ddg.json.proc2data <- function( edges, label, prefix )
{
	# extract procedure-to-data edges, where ddg.type is 'df.out' (data flow out)
	edges <- edges[edges$ddg.type == "df.out", c("ddg.from", "ddg.to")]
	
	# case: no procedure-to-data edges
	if( nrow(edges) == 0 )
		return(NA)
	
	# add prefix to node numbers
	edges$ddg.from <- mapply( paste , prefix , edges$ddg.from , sep='' , USE.NAMES=FALSE )
	edges$ddg.to <- mapply( paste , prefix , edges$ddg.to , sep='' , USE.NAMES=FALSE )
	
	# column names
	col.names <- c("prov:activity", "prov:entity")
	
	# convert to json
	prefix <- paste( prefix , label , sep='' )
	json <- .ddg.json.dataframe( edges , col.names , prefix )
	
	# form wasGeneratedBy node, return
	return( .ddg.json.formNode("wasGeneratedBy", json) )
}

# .ddg.json.data2proc forms and returns the json string for nodes representing 
#' data-to-procedure edges
#' @param edges edge nodes
#' @param label node label
#' @param prefix node prefix
#' @return the json string for data-to-procedure edges
#' @noRd

.ddg.json.data2proc <- function( edges, label, prefix )
{
	# extract data-to-procedure edges, where ddg.type is 'df.in' (data flow in)
	edges <- edges[edges$ddg.type == "df.in", c("ddg.from", "ddg.to")]
	
	# case: no data-to-procedure edges
	if( nrow(edges) == 0 )
		return(NA)
	
	# add prefix to node numbers
	edges$ddg.from <- mapply( paste , prefix , edges$ddg.from , sep='' , USE.NAMES=FALSE )
	edges$ddg.to <- mapply( paste , prefix , edges$ddg.to , sep='' , USE.NAMES=FALSE )
	
	# column names
	col.names <- c("prov:entity", "prov:activity")
	
	# convert to json, return
	prefix <- paste( prefix , label , sep='' )
	return( .ddg.json.dataframe(edges, col.names, prefix) )
}

# .ddg.json.func2proc forms and returns the json string for nodes representing 
#' function-to-procedure edges
#' @param nodes edge nodes
#' @param label.edge edge label
#' @param label.func function label
#' @param label.proc procedure label
#' @param prefix node prefix
#' @return the json string for function-to-procedure edges
#' @noRd

.ddg.json.func2proc <- function( nodes, label.edge, label.func, label.proc, prefix )
{
	# extract columns
	edges <- subset( nodes , select = c("ddg.fnum", "ddg.pnum") )
	
	# add prefix to node numbers
	edges$ddg.fnum <- mapply( paste , prefix , label.func , edges$ddg.fnum , 
	                          sep='' , USE.NAMES=FALSE )
	edges$ddg.pnum <- mapply( paste , prefix , label.proc , edges$ddg.pnum , 
	                          sep='' , USE.NAMES=FALSE )
	
	# column names
	col.names <- c( "prov:entity" , "prov:activity" )
	
	# convert to json, return
	prefix <- paste( prefix , label.edge , sep='' )
	return( .ddg.json.dataframe(edges, col.names, prefix) )
}

# .ddg.json.lib2func forms and returns the json string for nodes linking functions 
#' to their libraries
#' @param nodes linking nodes
#' @param label.edge edge label
#' @param label.lib library label
#' @param label.func function label
#' @param prefix node prefix
#' @return the json string for nodes linking functions to their libraries
#' @noRd

.ddg.json.lib2func <- function( nodes, label.edge, label.lib, label.func, prefix )
{
	# extract columns
	nodes <- subset( nodes , select = c("ddg.lnum", "ddg.fnum") )
	
	# extract unique rows
	nodes <- unique(nodes)
	
	# order by fnum, then lnum
	nodes <- nodes[ order(nodes$ddg.fnum) , ]
	nodes <- nodes[ order(nodes$ddg.lnum) , ]
	
	# add prefix to node numbers
	nodes$ddg.lnum <- mapply( paste , prefix , label.lib , nodes$ddg.lnum , 
	                          sep='' , USE.NAMES=FALSE )
	nodes$ddg.fnum <- mapply( paste , prefix , label.func , nodes$ddg.fnum , 
	                          sep='' , USE.NAMES=FALSE )
	
	# column names
	col.names <- c( "prov:collection" , "prov:entity" )
	
	# convert to json
	prefix <- paste( prefix , label.edge , sep='' )
	json <- .ddg.json.dataframe( nodes , col.names , prefix )
	
	# form hadMember node, return
	return( .ddg.json.formNode("hadMember", json) )
}

#' .ddg.json.combine combines all json parts into 1 complete prov-json string
#' @param json list of json parts
#' @return the complete prov-json string
#' @noRd

.ddg.json.combine <- function( json )
{
	# remove all NA slots
	json[ is.na(json) ] <- NULL
	
	# combine into json list: entity and used nodes
	json <- .ddg.json.combine.node( json , "entity" )
	json <- .ddg.json.combine.node( json , "used" )
	
	
	# FINAL COMBINATION
	# for all sets of nodes but the last one, add comma and newline for appending
	num.parts <- length(json)
	
	if( num.parts > 1 )
	{
		json[1:(num.parts-1)] <- lapply ( json[1:(num.parts-1)] , 
										  function(str)
											return( paste(str, ',', sep="") )
										)
	}
	
	# add final close brace to last element
	json[num.parts] <- sub( '$' , '\n}' , json[num.parts] )
	
	# combine and return
	return( .ddg.json.combine.rec(json) )
}

#' .ddg.json.combine.node extracts and combines the different parts of the given node, 
#' then puts it back into the json list.
#' If there is more than 1 part to the node, it removes the extra slots in the list
#' previously assigned to those parts
#' @param json list of json parts
#' @param node.name node name
#' @return list of combined json parts
#' @noRd

.ddg.json.combine.node <- function( json, node.name )
{
	# get the indices which the node parts reside in the json list
	indices <- grep( node.name , names(json) )
	num.parts <- length(indices)
	
	# base case
	if( num.parts == 0 )
		return(json)
	
	# if there are more than 1 part, append commas to all parts but the last one
	if( num.parts > 1 )
	{
		parts <- json[ indices[1:(num.parts-1)] ]
		
		parts <- lapply ( parts , 
						  function(str) 
							return( paste(str, ',', sep="") )
						)
		
		json[ indices[1:(num.parts-1)] ] <- parts
	}
	
	# combines parts recursively
	node <- .ddg.json.combine.rec( json[indices] )
	
	# forms the json node
	node <- .ddg.json.formNode( node.name , node )
	
	# reassign the combined node to the index previously assigned to the first part
	json[ indices[1] ] <- node
	names(json)[ indices[1] ] <- node.name
	
	# remove other parts, if any
	if( num.parts > 1 )
		json[ indices[2:num.parts] ] <- NULL
	
	return( json )
}

# .ddg.json.combine.rec is a recursive function for combining all elements 
#' in the given list to 1 (divide and conquer)
#' @param list list of json parts
#' @return list of combined json parts
#' @noRd

.ddg.json.combine.rec <- function( list )
{
	length <- length(list)
	
	# base case
	if( length == 1 )
		return( list[[1]] )
	
	# recursively merge left and right halves of the list
	mid <- ceiling( length / 2 )
	
	left <- .ddg.json.combine.rec( list[1:mid] )
	right <- .ddg.json.combine.rec( list[(mid+1):length] )
	
	return( paste(left, right, sep="\n") )
}

# --- MULTIPLE-USE FUNCTIONS ------------------- #

#' .ddg.json.escape.quotes adds escape characters to double quotes within strings
#' @param string input string
#' @return string with double quotes escaped
#' @noRd

.ddg.json.escape.quotes <- function( string )
{
	return( gsub('\"', '\\"', string) )
}

#' .ddg.json.escape.tabs converts '    ' or \t to escaped tab characters in string
#' @param str input string
#' @return string with tabs escaped
#' @noRd

.ddg.json.escape.tabs <- function( str )
{
	return( gsub('(    |\t)', '\\\t', str) )
}

# .ddg.json.df.escape.tabs in a data frame, converts '    ' or \t to escaped tab 
#' characters in strings
#' @param dataframe input dataframe
#' @return dataframe with tabs escaped in strings
#' @noRd

.ddg.json.df.escape.tabs <- function( dataframe )
{
	# get column numbers where the type is 'character'
	colNums <- sapply( dataframe , typeof )
	colNums <- which( colNums == "character" )
	
	# base case - none of the columns are of type 'character'
	if( length(colNums) == 0 )
		return( dataframe )
	
	# convert '    ' or \t into escaped tab characters, if any
	dataframe <- lapply ( dataframe , 
						  function(col)
						  {
							if( is.character(col) )
								return( .ddg.json.escape.tabs(col) )
							else
								return(col)
						  }
						)
	
	# reform data frame, return
	dataframe <- data.frame( dataframe , stringsAsFactors = FALSE )
	return(dataframe)
}

#' .ddg.json.dataframe converts a data frame into a formatted json string
#' @param dataframe input dataframe
#' @param col.names column names
#' @param obj.prefix object prefix
#' @return a formatted json string
#' @noRd

.ddg.json.dataframe <- function( dataframe, col.names, obj.prefix )
{
	# PROCESS TABLE TO PREPARE FOR PRINTING
	# change column names, if applicable
	if( ! is.na(col.names[1]) )
		colnames(dataframe) <- col.names
	
	# change row numbers
	row.names <- c( 1 : nrow(dataframe) )
	
	# split data frame into list of rows
	dataframe <- split( dataframe , as.numeric(row.names) )
	
	# change element names for list to include object's prefix
	row.names <- mapply( paste , obj.prefix , row.names , sep='' , USE.NAMES=FALSE )
	names(dataframe) <- row.names
	
	# convert to json
	json <- jsonlite::toJSON( dataframe , na = "string" )
	json <- jsonlite::prettify( json , indent = 4 )
	
	
	# PROCESS JSON INTO CORRECT FORMAT
	# convert '    ' into tab
	json <- gsub( '    ' , '\t', json )
	
	# remove square brackets
	json <- gsub( '\\[\n\t\t\\{' , '\\{' , json )
	json <- gsub( '\n\t]' , '' , json )
	
	# add indentation to object names
	json <- gsub( '\n\t\"' , '\n\t\t\"' , json )
	
	# remove row names in objects
	json <- gsub( ',\r?\n\t+"_row": "([0-9A-Za-z]|_|\\.|-|/)*"\n' , '\n', json)
	
	# remove wrapping braces
	json <- sub( '^\\{' , '' , json )
	json <- sub( '\n}\n$' , '' , json )
	
	return( json )
}

#' .ddg.json.list converts a list into a formatted json string
#' @param list the list to be converted
#' @param node.name the name of the node
#' @param obj.prefix object prefix
#' @return a formatted json string
#' @noRd

.ddg.json.list <- function( list, node.name, obj.prefix )
{
	names(list) <- mapply( paste , obj.prefix , names(list) , sep='' , USE.NAMES = FALSE )
	
	# name the node before converting
	list <- list(list)
	names(list) <- paste( obj.prefix, node.name, sep = '' )
	
	# convert to json
	json <- jsonlite::toJSON( list , auto_unbox = TRUE )
	json <- jsonlite::prettify( json , indent = 4 )
	
	# convert '    ' into tab
	json <- gsub( '    ' , '\t', json )
	
	# remove top brace
	json <- sub( '^\\{' , '' , json )
	
	# remove bottom brace
	json <- sub( '\n}\n$' , '' , json )
	
	# indent by 1 level, return
	json <- gsub('\n', '\n\t', json)
	return( json )
}

#' .ddg.json.formNode forms a first-level prov-json node
#' e.g. activity, entity, used, etc.
#' @param node.name node name
#' @param node.content node content
#' @return a first-level prov-json node
#' @noRd

.ddg.json.formNode <- function( node.name, node.content )
{
	# form node name string
	node.name <- paste( '\n\t"' , node.name , '" : \\{\n' , sep = '' )
	
	# top: wrap with name of node
	node <- sub( '^\n' , node.name , node.content )
	
	# botton: wrap with close brace
	return( sub('$', '\n\t}', node) )
}