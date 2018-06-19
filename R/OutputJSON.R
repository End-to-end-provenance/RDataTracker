# @author Elizabeth Fong
# @version February 2018

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
	
	node.prefix <- "rdt:"
	
	# this list is a container for each separate part that forms the json string
	json <- list( "prefix" = NA ,
				  "activity" = NA ,
				  "entity.data" = NA , 
				  "entity.env" = NA , 
				  "entity.lib" = NA , 
				  "entity.func" = NA ,
				  "wasInformedBy" = NA ,
				  "wasGeneratedBy" = NA ,
				  "used.d2p" = NA , 
				  "used.f2p" = NA ,
				  "hadMember" = NA )
	
	# prefix
	json$prefix <- .ddg.json.prefix()
	
	# activity (proc nodes)
	json$activity <- .ddg.json.proc()
	
	# entity: data nodes
	json$entity.data <- .ddg.json.data()
	
	# entity: environment
	json$entity.env <- .ddg.json.env()
	
	
	# EDGE TABLE NODES
	edges <- subset( .ddg.edges() , ddg.num > 0 )
	
	# wasInformedBy (proc2proc)
	json$wasInformedBy <- .ddg.json.proc2proc( edges , node.prefix )
	
	# wasGeneratedBy (proc2data)
	json$wasGeneratedBy <- .ddg.json.proc2data( edges , node.prefix )
	
	
	# get function nodes
	calls <- .ddg.function.nodes()
	num.calls <- nrow(calls)
	
	
	# used: data2proc
	json$used.d2p <- .ddg.json.data2proc( edges , node.prefix )
	
	
	# LIBRARY NODES - change row numbers
	libraries <- .ddg.installedpackages()
	rownames(libraries) <- c( 1 : nrow(libraries) )
	
	# PRINT TO JSON - LIBRARY NODES
	json$entity.lib <- .ddg.json.lib( libraries )
	
	
	# FUNCTION NODES - get function numbers if there are any function nodes
	if( num.calls > 0 )
	{
		functions <- calls[ , 2:3]
		functions <- unique(functions)
		
		rownames(functions) <- c( 1 : nrow(functions) )
		
		# PRINT TO JSON - FUNCTION NODES
		json$entity.func <- .ddg.json.func( functions )
		
		
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
		json$used.f2p <- .ddg.json.func2proc( calls , node.prefix )
		
		# PRINT TO JSON: func2lib
		json$hadMember <- .ddg.json.lib2func( calls , node.prefix )
	}	
	
	# COMBINE INTO COMPLETE JSON
	return( .ddg.json.combine(json) )
}


# --- HELPER FUNCTIONS ------------------------- #

# forms and returns the json string for the prefix node
.ddg.json.prefix <- function()
{
	# the contents of the prefix node
	prefix <- list("http://www.w3.org/ns/prov#", "http://rdatatracker.org/")
	names(prefix) <- c("prov", "rdt")
	
	# to let the prefix node be named
	prefix <- list(prefix)
	names(prefix) <- "prefix"
	
	# format to json
	# with proper indentation, without sq brackets around urls
	json <- toJSON(prefix, auto_unbox = TRUE)
	json <- prettify(json, indent = 4)
	
	# change '    ' into a tab (\t)
	json <- gsub( '    ' , '\t' , json )
	
	# remove end close brace and return
	return( sub('\n}\n$', '\n', json) )
}

# forms and returns the json string for the procedure nodes
.ddg.json.proc <- function()
{
	nodes <- .ddg.proc.nodes()
	
	# base case: no procedure nodes 
	if( nrow(nodes) == 0 )
		return(NA)
	
	# extract and order required columns
	nodes <- subset(nodes, ddg.num > 0, 
					select = c(ddg.name, ddg.type, ddg.time, ddg.snum, 
					ddg.startLine, ddg.startCol, ddg.endLine, ddg.endCol))
	
	# escape double quotes in ddg.name, if any
	nodes$ddg.name <- sapply( nodes$ddg.name , .ddg.json.escape.quotes )
	
	# column names
	col.names <- c( "rdt:name", "rdt:type", "rdt:elapsedTime", 
					"rdt:scriptNum", "rdt:startLine", "rdt:startCol", 
					"rdt:endLine", "rdt:endCol" )
	
	# convert to json
	json <- .ddg.json.dataframe( nodes , col.names , 'p' , comment = "procedure nodes" )
	
	# form activity node and return
	return( .ddg.json.formNode("activity", json) )
}

# forms and returns the json string for the data nodes
.ddg.json.data <- function()
{
	nodes <- .ddg.data.nodes()
	
	# base case: no data nodes
	if( nrow(nodes) == 0 )
		return(NA)
	
	# extract and order required columns
	nodes <- subset(nodes, ddg.num > 0,
					select = c(ddg.name, ddg.value, ddg.val.type, ddg.type,
					ddg.scope, ddg.from.env, ddg.hash, ddg.time, ddg.loc))
  
	# escape double quotes in ddg.val.type, if any
	nodes$ddg.val.type <- sapply( nodes$ddg.val.type , .ddg.json.escape.quotes )
	
	# column names
	col.names <- c( "rdt:name", "rdt:value", "rdt:valType", 
					"rdt:type", "rdt:scope", "rdt:fromEnv", 
					"rdt:MD5hash", "rdt:timestamp", "rdt:location" )
	
	# convert to json and return
	return( .ddg.json.dataframe(nodes, col.names, 'd', comment = "data nodes") )
}

# forms and returns the json string for the environment node
.ddg.json.env <- function()
{
	# GET CONTENT FOR NODE
	fields <- list( "rdt:name" = "environment" ,
					"rdt:architecture" = NA ,
					"rdt:operatingSystem" = NA ,
					"rdt:language" = NA ,
					"rdt:rVersion" = NA ,
					"rdt:script" = NA ,
					"rdt:scriptTimeStamp" = NA ,
					"rdt:sourcedScripts" = NA ,
					"rdt:sourcedScriptTimeStamps" = NA ,
					"rdt:workingDirectory" = NA ,
					"rdt:ddgDirectory" = NA ,
					"rdt:ddgTimeStamp" = NA ,
					"rdt:rdatatrackerVersion" = NA ,
					"rdt:hashAlgorithm" = NA )
	
	# architecture, language, rVersion
	r.version <- R.Version()
	
	fields$`rdt:architecture` <- r.version$arch
	fields$`rdt:language` <- r.version$language
	fields$`rdt:rVersion` <- r.version$version
	
	# operating system
	fields$`rdt:operatingSystem` <- .Platform$OS.type
	
	# script variables
	script.path <- .ddg.get("ddg.r.script.path")
	
	if( ! is.null(script.path) )
	{
		fields$`rdt:script` <- script.path 
		fields$`rdt:scriptTimeStamp` <- .ddg.format.time( file.info(script.path)$mtime )
		
		sourced.scripts <- .ddg.json.sourced.scripts()
		fields$`rdt:sourcedScripts` <- sourced.scripts[[1]]
		fields$`rdt:sourcedScriptTimeStamps` <- sourced.scripts[[2]]
	}
	else
	{
		fields$`rdt:script` <- ""
		fields$`rdt:scriptTimeStamp` <- ""
		
		fields$`rdt:sourcedScripts` <- ""
		fields$`rdt:sourcedScriptTimeStamps` <- ""
	}
	
	# working directory, ddg directory
	fields$`rdt:workingDirectory` <- getwd()
	fields$`rdt:ddgDirectory` <- .ddg.path()
	
	# ddg timestamp
	fields$`rdt:ddgTimeStamp` <- .ddg.get("ddg.start.time")
	
	# rdt version
	fields$`rdt:rdatatrackerVersion` <- toString( packageVersion("RDataTracker") )
	
	# hash algorithm
	fields$`rdt:hashAlgorithm` <- .ddg.get(".ddg.hash.algorithm")
	
	
	# TO JSON
	# let the node be named
	fields <- list(fields)
	names(fields) <- "environment"
	
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
		
		script.names <- ss[ , 2]
		script.times <- ss[ , 3]
	}
	
	return( list(script.names, script.times) )
}

# forms and returns the json string for the library nodes
.ddg.json.lib <- function( nodes )
{
	# change col names
	col.names <- c( "name" , "version" )
	
	# convert to json
	json <- .ddg.json.dataframe( nodes , col.names , 'l' , comment = "library nodes - prov collections" )
	
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
	
	#2 levels
	json <- gsub( '\n' , '\n\t\t' , json )
	
	# change first `{` to a `,` character for appending to library nodes, return
	return( sub('^\\{', ',', json) )
}

# forms and returns the json string for function nodes
.ddg.json.func <- function( nodes )
{
	# extract names of functions
	nodes <- nodes[ , "ddg.fun"]
	
	# convert to data frame
	nodes <- data.frame( nodes , stringsAsFactors = FALSE )
	
	# convert to json, return
	return( .ddg.json.dataframe(nodes, "name", 'f', comment = "function nodes") )
}

# forms and returns the json string for nodes representing procedure-to-procedure edges
.ddg.json.proc2proc <- function( edges , prefix )
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
	json <- .ddg.json.dataframe( edges , col.names , 'pp' , comment = "procedure-to-procedure edges" )
	
	# form wasInformedBy node, return
	return( .ddg.json.formNode("wasInformedBy", json) )
}

# forms and returns the json string for nodes representing procedure-to-data edges
.ddg.json.proc2data <- function( edges , prefix )
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
	json <- .ddg.json.dataframe( edges , col.names , 'pd' , comment = "procedure-to-data edges" )
	
	# form wasGeneratedBy node, return
	return( .ddg.json.formNode("wasGeneratedBy", json) )
}

# forms and returns the json string for nodes representing data-to-procedure edges
.ddg.json.data2proc <- function( edges , prefix )
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
	return( .ddg.json.dataframe(edges ,col.names ,'dp' ,comment = "data-to-procedure edges") )
}

# forms and returns the json string for nodes representing function-to-procedure edges
.ddg.json.func2proc <- function( nodes , prefix )
{
	# extract columns
	edges <- subset( nodes , select = c("ddg.fnum", "ddg.pnum") )
	
	# add prefix to node numbers
	edges$ddg.fnum <- mapply( paste , prefix , 'f' , edges$ddg.fnum , sep='' , USE.NAMES=FALSE )
	edges$ddg.pnum <- mapply( paste , prefix , 'p' , edges$ddg.pnum , sep='' , USE.NAMES=FALSE )
	
	# column names
	col.names <- c( "prov:entity" , "prov:activity" )
	
	# convert to json, return
	return( .ddg.json.dataframe(edges, col.names, 'fp', comment = "function-to-procedure edges") )
}

# forms and returns the json string for nodes linking functions to their libraries
.ddg.json.lib2func <- function( nodes , prefix )
{
	# extract columns
	nodes <- subset( nodes , select = c("ddg.lnum", "ddg.fnum") )
	
	# extract unique rows
	nodes <- unique(nodes)
	
	# order by fnum, then lnum
	nodes <- nodes[ order(nodes$ddg.fnum) , ]
	nodes <- nodes[ order(nodes$ddg.lnum) , ]
	
	# add prefix to node numbers
	nodes$ddg.lnum <- mapply( paste , prefix , 'l' , nodes$ddg.lnum , sep='' , USE.NAMES=FALSE )
	nodes$ddg.fnum <- mapply( paste , prefix , 'f' , nodes$ddg.fnum , sep='' , USE.NAMES=FALSE )
	
	# column names
	col.names <- c( "prov:collection" , "prov:entity" )
	
	# convert to json
	json <- .ddg.json.dataframe( nodes , col.names , 'm' , comment = "groups function nodes with their library nodes" )
	
	# form hadMember node, return
	return( .ddg.json.formNode("hadMember", json) )
}

# combines all json parts into 1 complete prov-json string
.ddg.json.combine <- function( json )
{
	# ENTITY NODES
	# extract entity node parts which do not contain na into new variable
	entity.old <- json[3:6]
	entity <- entity.old[ which(!is.na(entity.old)) ]
	
	# for all sets of nodes but the last one, add comma and newline for appending
	len <- length(entity)
	entity[1:(len-1)] <- lapply( entity[1:(len-1)] , .ddg.json.addComma )
	
	# combine parts into entity node
	entity <- .ddg.json.combine.rec( entity )
	json[3] <- .ddg.json.formNode( "entity" , entity , last.node = FALSE )
	
	
	# USED NODES
	# extract used node parts which do not contain na into new variable
	used.old <- json[9:10]
	used <- used.old[ which(!is.na(used.old)) ]
	
	# form used node
	if( length(used) == 0 )		# no nodes
	{
		json[9] <- NULL
	}
	else if( length(used) == 1 )	# 1 set of nodes
	{
		json[9] <- .ddg.json.formNode( "used" , used )
	}	
	else 	# both sets of nodes present
	{
		# add comma and newline to first set of nodes for appending
		used[1] <- sub( '\n$' , ',\n\n' , used[[1]] )
		used <- sub( '\n$' , used[[2]] , used[[1]] )
		
		json[9] <- .ddg.json.formNode( "used" , used )
	}
	
	# since entity and used nodes parts are merged into 1,
	# remove other excess entity and used node slots in list
	json[10] <- NULL
	json[4:6] <- NULL
	
	
	# FINAL COMBINATION
	# extract nodes which are not na into new variable
	json.parts <- json[ which(!is.na(json)) ]
	
	# for all sets of nodes but the last one, add comma and newline for appending
	length <- length(json.parts)
	json.parts[1:(length-1)] <- lapply( json.parts[1:(length-1)] , .ddg.json.addComma )
	
	# add final close brace to last element
	json.parts[length] <- sub( '\n$' , '\n}' , json.parts[[length]] )
	
	# combine and return
	return( .ddg.json.combine.rec(json.parts) )
}

# recursive function for combining all elements in the given list to 1 (divide and conquer)
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
.ddg.json.escape.quotes <- function( string )
{
	return( gsub('\"', '\\\\"', string) )
}

# converts a data frame into a formatted json string
.ddg.json.dataframe <- function( dataframe , col.names , obj.prefix , comment = NULL )
{
	# PROCESS TABLE TO PREPARE FOR PRINTING
	# change column names
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
	json <- gsub( ',\n\t\t\t"_row": "[a-z]+[0-9]+"\n' , '\n' , json)
	
	# replace wrapping braces with newline characters
	json <- sub( '^\\{' , '' , json )
	json <- sub( '}\n$' , '' , json )
	
	return( json )
}

# forms a first-level prov-json node
# e.g. activity, entity, used, etc.
.ddg.json.formNode <- function( node.name , node.content , last.node = FALSE )
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