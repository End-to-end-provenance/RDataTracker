# @author Elizabeth Fong
# @version 2.1 (June 2018)

# writes json out to file
ddg.json.write <- function() 
{
	fileout <- paste(.ddg.path(), "/ddg.json", sep="")
	json <- ddg.json()
	write(json, fileout)
}

# forms the prov-json string
ddg.json <- function()
{
	library(jsonlite)
	
	# CONSTANTS
	TOOL.NAME <- "RDataTracker"
	JSON.VERSION <- 2.1
	
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
	#json <<- json
	return( .ddg.json.combine(json) )
}


# --- HELPER FUNCTIONS ------------------------- #

# forms and returns the json string for the prefix node
.ddg.json.prefix <- function( node )
{
	# let the prefix node be named
	node <- list(node)
	names(node) <- "prefix"
	
	# format to json
	# with proper indentation, without sq brackets around urls
	json <- toJSON(node, auto_unbox = TRUE)
	json <- prettify(json, indent = 4)
	
	# change '    ' into a tab (\t)
	json <- gsub( '    ' , '\t' , json )
	
	# remove end close brace and return
	return( sub('\n}\n$', '\n', json) )
}

# forms and returns the json string for the agent node
.ddg.json.agent <- function( tool , json.version , label , prefix )
{
	# get node content
	node <- data.frame( "tool.name" = tool ,
						"tool.version" = toString(packageVersion(tool)) ,
						"json.version" = json.version ,
						stringsAsFactors = FALSE )
	names(node) <- mapply( paste , prefix , names(node) , sep='' , USE.NAMES=FALSE )
	
	# convert to json
	prefix <- paste( prefix , label , sep='' )
	json <- .ddg.json.dataframe( node , NA , prefix , comment = "agent: this json file and the tool that produced this" )
	
	# form agent node and return
	return( .ddg.json.formNode("agent", json) )
}

# forms and returns the json string for the procedure nodes
.ddg.json.proc <- function( label , prefix )
{
	nodes <- .ddg.proc.nodes()
	
	# extract and order required columns
	nodes <- subset( nodes, ddg.num > 0, 
					 select = c(ddg.name, ddg.type, ddg.time, ddg.snum, 
					 ddg.startLine, ddg.startCol, ddg.endLine, ddg.endCol) )
	
	# base case: no procedure nodes 
	if( nrow(nodes) == 0 )
		return(NA)
	
	# escape double quotes in ddg.name, if any
	nodes$ddg.name <- sapply( nodes$ddg.name , .ddg.json.escape.quotes )
	
	# convert '    ' or \t to escaped tab characters, if any
	nodes <- .ddg.json.df.escape.tabs( nodes )
	
	# column names
	col.names <- c( "name", "type", "elapsedTime", "scriptNum", 
					"startLine", "startCol", "endLine", "endCol" )
	col.names <- mapply( paste , prefix , col.names , sep='' , USE.NAMES = FALSE )
	
	# convert to json
	prefix <- paste( prefix , label , sep='' )
	json <- .ddg.json.dataframe( nodes , col.names , prefix , comment = "procedure nodes" )
	
	# form activity node and return
	return( .ddg.json.formNode("activity", json) )
}

# forms and returns the json string for the data nodes
.ddg.json.data <- function( label , prefix )
{
	nodes <- .ddg.data.nodes()
	
	# extract and order required columns
	nodes <- subset( nodes, ddg.num > 0,
					 select = c(ddg.name, ddg.value, ddg.val.type, ddg.type,
					 ddg.scope, ddg.from.env, ddg.hash, ddg.time, ddg.loc) )
	
	# base case: no data nodes
	if( nrow(nodes) == 0 )
		return(NA)
	
	# escape double quotes in ddg.val.type, if any
	nodes$ddg.val.type <- sapply( nodes$ddg.val.type , .ddg.json.escape.quotes )
	
	# convert '    ' or \t to escaped tab characters, if any
	nodes <- .ddg.json.df.escape.tabs( nodes )
	
	# column names
	col.names <- c( "name", "value", "valType", "type", "scope", "fromEnv", 
					"MD5hash", "timestamp", "location" )
	col.names <- mapply( paste , prefix , col.names , sep='' , USE.NAMES = FALSE )
	
	# convert to json and return
	prefix <- paste( prefix , label , sep='' )
	return( .ddg.json.dataframe(nodes, col.names, prefix, comment = "data nodes") )
}

# forms and returns the json string for the environment node
.ddg.json.env <- function( label , prefix )
{
	# GET CONTENT FOR NODE
	fields <- list( "name" = "environment" ,
					"architecture" = NA ,
					"operatingSystem" = NA ,
					"language" = NA ,
					"rVersion" = NA ,
					"script" = NA ,
					"scriptTimeStamp" = NA ,
					"sourcedScripts" = NA ,
					"sourcedScriptTimeStamps" = NA ,
					"workingDirectory" = NA ,
					"ddgDirectory" = NA ,
					"ddgTimeStamp" = NA ,
					"rdatatrackerVersion" = NA ,
					"hashAlgorithm" = NA )
	
	# architecture, language, rVersion
	r.version <- R.Version()
	
	fields$architecture <- r.version$arch
	fields$language <- r.version$language
	fields$rVersion <- r.version$version
	
	# operating system
	fields$operatingSystem <- .Platform$OS.type
	
	# script variables
	script.path <- .ddg.get("ddg.r.script.path")
	
	if( ! is.null(script.path) )
	{
		fields$script <- script.path 
		fields$scriptTimeStamp <- .ddg.format.time( file.info(script.path)$mtime )
		
		sourced.scripts <- .ddg.json.sourced.scripts()
		fields$sourcedScripts <- sourced.scripts[[1]]
		fields$sourcedScriptTimeStamps <- sourced.scripts[[2]]
	}
	else
	{
		fields$script <- ""
		fields$scriptTimeStamp <- ""
		
		fields$sourcedScripts <- ""
		fields$sourcedScriptTimeStamps <- ""
	}
	
	# working directory, ddg directory (escape any tab characters)
	fields$workingDirectory <- .ddg.json.escape.tabs( getwd() )
	fields$ddgDirectory <- .ddg.json.escape.tabs( .ddg.path() )
	
	# ddg timestamp
	fields$ddgTimeStamp <- .ddg.get("ddg.start.time")
	
	# rdt version
	fields$rdatatrackerVersion <- toString( packageVersion("RDataTracker") )
	
	# hash algorithm
	fields$hashAlgorithm <- .ddg.get(".ddg.hash.algorithm")
	
	# add prefix to names of the list
	names(fields) <- mapply( paste , prefix , names(fields) , sep='' , USE.NAMES = FALSE )
	
	
	# TO JSON
	# let the node be named
	fields <- list(fields)
	names(fields) <- paste( prefix , "environment" , sep = '' )
	
	# convert to json
	json <- toJSON( fields , auto_unbox = TRUE )
	json <- prettify( json , indent = 4 )
	
	# convert '    ' into tab
	json <- gsub( '    ' , '\t' , json )
	
	# remove top brace, add comment
	json <- sub( '^\\{' , '\n\t// environment' , json )
	
	# remove bottom brace
	json <- sub( '\n}\n$' , '' , json )
	
	# indent by 1 level
	json <- gsub( '\n' , '\n\t' , json )
	
	# add 1 newline to end of node for appending, return
	return( sub('}$', '}\n', json) )
}

# return the names of other scripts that were sourced and their timestamps.
.ddg.json.sourced.scripts <- function() 
{
	script.names <- ""
	script.times <- ""
	
	ss <- .ddg.sourced.scripts()
	
	# first row: main script
	if( (! is.null(ss)) && (nrow(ss) > 1) )
	{
		ss <- ss[ ss$snum > 0 , ]
		
		script.names <- sapply( ss[ , 2] , .ddg.json.escape.tabs )
		script.times <- ss[ , 3]
	}
	
	return( list(script.names, script.times) )
}

# forms and returns the json string for the library nodes
.ddg.json.lib <- function( nodes , label , prefix )
{
	# change col names
	col.names <- c( "name" , "version" )
	
	# convert '    ' or \t to escaped tab characters, if any
	nodes <- .ddg.json.df.escape.tabs( nodes )
	
	# convert to json
	prefix <- paste( prefix , label , sep='' )
	json <- .ddg.json.dataframe( nodes , col.names , prefix , comment = "library nodes - prov collections" )
	
	# append prov:type to json
	prov.type <- .ddg.json.collection()
	json <- gsub( '\n\t\t}' , prov.type , json )
	
	return( json )
}

# forms and returns the json string for the type node for a collection
.ddg.json.collection <- function()
{
	prov.type <- list("prov:Collection", "xsd:QName")
	names(prov.type) <- c("$", "type")
	
	# let prov:type node be named
	prov.type <- list(prov.type)
	names(prov.type) <- "prov:type"
	
	# convert  to json
	json <- toJSON(prov.type, auto_unbox = TRUE)
	json <- prettify(json, indent = 4)
	
	# convert '    ' to tab
	json <- gsub( '    ' , '\t' , json )
	
	# remove newline at the end
	json <- sub( '\n$' , '' , json )
	
	# indent by 2 levels
	json <- gsub( '\n' , '\n\t\t' , json )
	
	# change first `{` to a `,` character for appending to library nodes, return
	return( sub('^\\{', ',', json) )
}

# forms and returns the json string for function nodes
.ddg.json.func <- function( nodes , label , prefix )
{
	# extract names of functions
	nodes <- nodes[ , "ddg.fun"]
	
	# convert to data frame
	nodes <- data.frame( nodes , stringsAsFactors = FALSE )
	
	# convert '    ' or \t to escaped tab characters, if any
	nodes <- .ddg.json.df.escape.tabs( nodes )
	
	# convert to json, return
	prefix <- paste( prefix , label , sep='' )
	return( .ddg.json.dataframe(nodes, "name", prefix, comment = "function nodes") )
}

# forms and returns the json string for nodes representing procedure-to-procedure edges
.ddg.json.proc2proc <- function( edges , label , prefix )
{
	# extract procedure-to-procedure edges, where ddg.type is 'cf' (control flow)
	edges <- subset(edges, ddg.type == "cf", select = c(ddg.from, ddg.to))
	
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
	json <- .ddg.json.dataframe( edges , col.names , prefix , comment = "procedure-to-procedure edges" )
	
	# form wasInformedBy node, return
	return( .ddg.json.formNode("wasInformedBy", json) )
}

# forms and returns the json string for nodes representing procedure-to-data edges
.ddg.json.proc2data <- function( edges , label , prefix )
{
	# extract procedure-to-data edges, where ddg.type is 'df.out' (data flow out)
	edges <- subset(edges, ddg.type == "df.out", select = c(ddg.from, ddg.to))
	
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
	json <- .ddg.json.dataframe( edges , col.names , prefix , comment = "procedure-to-data edges" )
	
	# form wasGeneratedBy node, return
	return( .ddg.json.formNode("wasGeneratedBy", json) )
}

# forms and returns the json string for nodes representing data-to-procedure edges
.ddg.json.data2proc <- function( edges , label , prefix )
{
	# extract data-to-procedure edges, where ddg.type is 'df.in' (data flow in)
	edges <- subset(edges, ddg.type == "df.in", select = c(ddg.from, ddg.to))
	
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
	return( .ddg.json.dataframe(edges, col.names, prefix, comment = "data-to-procedure edges") )
}

# forms and returns the json string for nodes representing function-to-procedure edges
.ddg.json.func2proc <- function( nodes , label.edge , label.func , label.proc , prefix )
{
	# extract columns
	edges <- subset( nodes , select = c("ddg.fnum", "ddg.pnum") )
	
	# add prefix to node numbers
	edges$ddg.fnum <- mapply( paste , prefix , label.func , edges$ddg.fnum , sep='' , USE.NAMES=FALSE )
	edges$ddg.pnum <- mapply( paste , prefix , label.proc , edges$ddg.pnum , sep='' , USE.NAMES=FALSE )
	
	# column names
	col.names <- c( "prov:entity" , "prov:activity" )
	
	# convert to json, return
	prefix <- paste( prefix , label.edge , sep='' )
	return( .ddg.json.dataframe(edges, col.names, prefix, comment = "function-to-procedure edges") )
}

# forms and returns the json string for nodes linking functions to their libraries
.ddg.json.lib2func <- function( nodes , label.edge , label.lib , label.func , prefix )
{
	# extract columns
	nodes <- subset( nodes , select = c("ddg.lnum", "ddg.fnum") )
	
	# extract unique rows
	nodes <- unique(nodes)
	
	# order by fnum, then lnum
	nodes <- nodes[ order(nodes$ddg.fnum) , ]
	nodes <- nodes[ order(nodes$ddg.lnum) , ]
	
	# add prefix to node numbers
	nodes$ddg.lnum <- mapply( paste , prefix , label.lib , nodes$ddg.lnum , sep='' , USE.NAMES=FALSE )
	nodes$ddg.fnum <- mapply( paste , prefix , label.func , nodes$ddg.fnum , sep='' , USE.NAMES=FALSE )
	
	# column names
	col.names <- c( "prov:collection" , "prov:entity" )
	
	# convert to json
	prefix <- paste( prefix , label.edge , sep='' )
	json <- .ddg.json.dataframe( nodes , col.names , prefix , comment = "groups function nodes with their library nodes" )
	
	# form hadMember node, return
	return( .ddg.json.formNode("hadMember", json) )
}

# combines all json parts into 1 complete prov-json string
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
		json[1:(num.parts-1)] <- lapply( json[1:(num.parts-1)] , .ddg.json.addComma )
	
	# add final close brace to last element
	json[num.parts] <- sub( '\n$' , '\n}' , json[num.parts] )
	
	# combine and return
	return( .ddg.json.combine.rec(json) )
}

# extracts and combines the different parts of the given node, 
# then puts it back into the json list
# if there are more than 1 part to the node, it removes the extra slots in the list
# previously assigned to those parts
.ddg.json.combine.node <- function( json , node.name )
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
		parts <- lapply( parts , .ddg.json.addComma )
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

# recursive function for combining all elements in the given list to 1 (divide and conquer)
.ddg.json.combine.rec <- function( list )
{
	length <- length(list)
	
	# base case
	if( length == 1 )
		return( list[1] )
	
	# recursively merge left and right halves of the list
	mid <- ceiling( length / 2 )
	
	left <- .ddg.json.combine.rec( list[1:mid] )
	right <- .ddg.json.combine.rec( list[(mid+1):length] )
	
	return( sub('\n$', right, left) )
}

# ddg.installedpackages() returns information on packages installed 
# at the time of execution and their versions.
.ddg.installedpackages <- function()
{
	packages <- devtools::session_info()
	packages <- packages[[2]]
	installed <- packages[packages[,2] == "*",]
	installed <- installed[ ,c(1,3)]
	return(installed)
}


# --- MULTIPLE-USE FUNCTIONS ------------------- #

# adds escape characters to double quotes within strings
.ddg.json.escape.quotes <- function( str )
{
	return( gsub('\"', '\\\\"', str) )
}

# converts '    ' or \t to escaped tab characters in string
.ddg.json.escape.tabs <- function( str )
{
	return( gsub('(    |\\t)', '\\\\t', str) )
}

# in a data frame, converts '    ' or \t to escaped tab characters in strings
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

# converts a data frame into a formatted json string
.ddg.json.dataframe <- function( dataframe , col.names , obj.prefix , comment = NULL )
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
	json <- toJSON( dataframe , na = "string" )
	json <- prettify( json , indent = 4 )
	
	
	# PROCESS JSON INTO CORRECT FORMAT
	# convert '    ' into tab
	json <- gsub( '    ' , '\t' , json )
	
	# add comment line to top of block, if any
	if( ! is.null(comment) )
	{
		comment <- paste( '{\n\t\t//' , comment )
		json <- sub( '^\\{' , comment , json )
	}
	
	# remove square brackets
	json <- gsub( '\\[\n\t\t\\{' , '\\{' , json )
	json <- gsub( '\n\t]' , '' , json )
	
	# add indentation to object names
	json <- gsub( '\n\t\"' , '\n\t\t\"' , json )
	
	# remove row names in objects
	json <- gsub( ',\r?\n\t+"_row": "([0-9A-Za-z]|_|\\.|-|/)*"\n' , '\n' , json)
	
	# replace wrapping braces with newline characters
	json <- sub( '^\\{' , '' , json )
	json <- sub( '}\n$' , '' , json )
	
	return( json )
}

# forms a first-level prov-json node
# e.g. activity, entity, used, etc.
.ddg.json.formNode <- function( node.name , node.content )
{
	# form node name string
	node.name <- paste( '\n\t"' , node.name , '" : \\{\n' , sep = '' )
	
	# top: wrap with name of node
	node <- sub( '^\n' , node.name , node.content )
	
	# botton: wrap with close brace
	return( sub('\n$', '\n\t}\n', node) )
}

# adds a comma and newline to the end of the given node for appending to other nodes
.ddg.json.addComma <- function( node )
{
	return( sub('\n$', ',\n\n', node) )
}