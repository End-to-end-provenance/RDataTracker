
ddg.json.write <- function() 
{
	fileout <- paste(.ddg.path(), "/ddg.json", sep="")
	json <- ddg.json()
	write(json, fileout)
}

ddg.json <- function( indent = 4 )
{
	library(jsonlite)
	
	data.nodes <<- subset( .ddg.data.nodes() , ddg.num > 0 )
	proc.nodes <<- subset( .ddg.proc.nodes() , ddg.num > 0 )
	edges <<- subset( .ddg.edges() , ddg.num > 0 )
	
	function.calls <<- .ddg.function.nodes()
	libraries <<- .ddg.installedpackages()
	
	
	node.prefix <- "rdt:"
	
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
	json$prefix <- .ddg.json.prefix( indent )
	
	# activity (proc nodes)
	json$activity <- .ddg.json.proc( indent )
	
	# entity: data nodes
	json$entity.data <- .ddg.json.data( indent )
	
	# entity: environment
	json$entity.env <- .ddg.json.env( indent )
	
	
	# EDGE TABLE NODES
	edges <- subset( .ddg.edges() , ddg.num > 0 )
	
	# wasInformedBy (proc2proc)
	json$wasInformedBy <- .ddg.json.proc2proc( edges , node.prefix , indent = indent )
	
	# wasGeneratedBy (proc2data)
	json$wasGeneratedBy <- .ddg.json.proc2data( edges , node.prefix , indent = indent )
	
	# used: data2proc
	json$used.d2p <- .ddg.json.data2proc( edges , node.prefix , indent = indent )
	
	
	# LIBRARY & FUNCTION NODES
	
	# LIBRARY NODES - change row numbers
	libraries <- .ddg.installedpackages()
	rownames(libraries) <- c( 1 : nrow(libraries) )
	
	# PRINT TO JSON - LIBRARY NODES
	libraries <<- libraries
	json$entity.lib <- .ddg.json.lib( libraries , indent = indent )
	
	
	# FUNCTION NODES - get function numbers
	calls <- .ddg.function.nodes()
	
	functions <- calls[ , 2:3]
	functions <- unique(functions)
	
	rownames(functions) <- c( 1 : nrow(functions) )
	
	# PRINT TO JSON - FUNCTION NODES
	functions <<- functions
	json$entity.func <- .ddg.json.func( functions , indent = indent )
	
	
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
	json$used.f2p <- .ddg.json.func2proc( calls , node.prefix , indent = indent )
	
	# PRINT TO JSON: func2lib
	json$hadMember <- .ddg.json.lib2func( calls , node.prefix , indent = indent )
	
	json <<- json
	
	# COMBINE INTO COMPLETE JSON
	combined.json <- .ddg.json.combine( json , indent = indent )
	return( combined.json )
}


.ddg.json.prefix <- function( indent = 4 )
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
	json <- prettify(json, indent = indent)
	
	# edit end close braces so to enable appending
	json <- sub( '\n}\n$' , ',\n\n' , json )
	
	return(json)
}


.ddg.json.proc <- function( indent = 4 )
{
	# extract nodes and required columns
	nodes <- .ddg.proc.nodes()
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
	json <- .ddg.json.dataframe ( nodes , col.names , 'p' , 
										comment = "procedure nodes" , 
										indent = indent )
	
	# form activity node
	json <- .ddg.json.formNode( "activity" , json , last.node = FALSE , indent = indent )
	
	return( json )
}

.ddg.json.data <- function( indent = 4 )
{
	# extract nodes and required columns
	nodes <- .ddg.data.nodes()
	nodes <- subset(nodes, ddg.num > 0,
					select = c(ddg.name, ddg.value, ddg.val.type, ddg.type,
					ddg.scope, ddg.from.env, ddg.hash, ddg.time, ddg.loc))
	
	# escape double quotes in ddg.val.type, if any
	nodes$ddg.val.type <- sapply( nodes$ddg.val.type , .ddg.json.escape.quotes )
	
	# column names
	col.names <- c( "rdt:name", "rdt:value", "rdt:valType", 
					"rdt:type", "rdt:scope", "rdt:fromEnv", 
					"rdt:MD5hash", "rdt:timestamp", "rdt:location" )
	
	# convert to json
	json <- .ddg.json.dataframe ( nodes , col.names , 'd' , 
										comment = "data nodes" ,
										indent = indent )
	
	# add comma and newline to last node for combining
	json <- sub( '\n$' , ',\n\n' , json )
	
	return( json )
}

.ddg.json.env <- function( indent = 4 )
{
	# GET CONTENT FOR NODE
	fields <- list( "rdt:name" = "environment" ,
					"rdt:architecture" = NA ,
					"rdt:operatingSystem" = NA ,
					"rdt:language" = NA ,
					"rdt:rVersion" = NA ,
					"rdt:script" = NA ,
					"rdt:sourcedScripts" = NA ,
					"rdt:scriptTimeStamp" = NA ,
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
		fields$`rdt:sourcedScripts` <- .ddg.json.sourced.script.names()
		fields$`rdt:scriptTimeStamp` <- .ddg.format.time( file.info(script.path)$mtime )
	}
	else
	{
		fields$`rdt:script` <- ""
		fields$`rdt:sourcedScripts` <- ""
		fields$`rdt:scriptTimeStamp` <- ""
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
	json <- prettify( json , indent = indent )
	
	# remove top brace, add comment
	json <- sub( '^\\{' , '\n    // environment' , json )
	
	# remove bottom brace, add comma to end of node
	json <- sub( '\n}\n$' , ',' , json )
	
	# indent by 1 level
	json <- gsub( '\n' , '\n    ' , json )
	
	# add 2 newlines to end of node for appending
	json <- sub( ',$' , ',\n\n' , json )
	
	return( json )
}


# no idea what this does (can't find an example), 
# but it doesn't look prov-json compliant
# tmp fix: change entire thing to a string
.ddg.json.sourced.script.names <- function() 
{
	ss <- .ddg.sourced.scripts()
	
	# First row is main script.
	if (nrow(ss) == 1) {
		output <- ""		# edit: made empty from "\"\"\n"
	} else {
		ss <- ss[ss$snum > 0, ]
		stimes <- file.info(ss$sname)$mtime
		stimes <- .ddg.format.time(stimes)
		
		scriptarray <- paste("\t{\"number\" : \"", ss[ , 1], "\",
                            	 \"name\" : \"",ss[ , 2], "\",
                            	 \"timestamp\" : \"",stimes, "\"}",
                        		 sep = "", collapse =",\n")
		output <- paste("\"[\n", scriptarray, " ]\"", sep = "")		# edit: wrap entire thing with quotes
	}
	return(output)
}



.ddg.json.lib <- function( nodes , indent = 4 )
{
	# change col names
	col.names <- c( "name" , "version" )
	
	# convert to json
	json <- .ddg.json.dataframe ( nodes , col.names , 'l' , 
										comment = "library nodes - prov collections" ,
										indent = indent )
	
	# append prov:type to json
	prov.type <- .ddg.json.collection( indent = indent )
	json <- gsub( '\n        }' , prov.type , json )
	
	# add comma and newline to last node for combining
	json <- sub( '\n$' , ',\n\n' , json )
	
	return( json )
}

.ddg.json.collection <- function( indent = 4 )
{
	prov.type <- list("prov:Collection", "xsd:QName")
	names(prov.type) <- c("$", "type")
	
	# let prov:type node be named
	prov.type <- list(prov.type)
	names(prov.type) <- "prov:type"
	
	# convert  to json
	json <- toJSON(prov.type, auto_unbox = TRUE)
	json <- prettify(json, indent = indent)
	
	# remove newline at the end
	json <- sub( '\n$' , '' , json )
	
	# indent 2 levels
	json <- gsub( '\n' , '\n        ' , json )
	
	# change first `{` to a `,` character for appending to library nodes
	json <- sub( '^\\{' , ',' , json )
	
	return( json )
}

.ddg.json.func <- function( nodes , indent = 4 )
{
	# extract names of functions only
	nodes <- nodes[ , "ddg.fun"]
	nodes <- unique(nodes)
	
	# convert to data frame
	nodes <- data.frame( nodes , stringsAsFactors = FALSE )
	
	# convert to json
	json <- .ddg.json.dataframe ( nodes , "name" , 'f' , 
										comment = "function nodes" ,
										indent = indent )
	return( json )
}

.ddg.json.proc2proc <- function( edges , prefix , indent = 4 )
{
	# extract procedure-to-procedure edges, where ddg.type is 'cf' (control flow)
	edges <- subset(edges, ddg.type == "cf", select = c(ddg.from, ddg.to))
	
	# add prefix to node numbers
	edges$ddg.from <- mapply( paste , prefix , edges$ddg.from , sep='' , USE.NAMES=FALSE )
	edges$ddg.to <- mapply( paste , prefix , edges$ddg.to , sep='' , USE.NAMES=FALSE )
	
	# column names
	col.names <- c("prov:informant", "prov:informed")
	
	# convert to json
	json <- .ddg.json.dataframe ( edges , col.names , 'pp' , 
										comment = "procedure-to-procedure edges" ,
										indent = indent )
	
	# form wasInformedBy node
	json <- .ddg.json.formNode( "wasInformedBy" , json , last.node = FALSE , indent = indent )
	
	return( json )
}

.ddg.json.proc2data <- function( edges , prefix , indent = 4 )
{
	# extract procedure-to-data edges, where ddg.type is 'df.out' (data flow out)
	edges <- subset(edges, ddg.type == "df.out", select = c(ddg.from, ddg.to))
	
	# add prefix to node numbers
	edges$ddg.from <- mapply( paste , prefix , edges$ddg.from , sep='' , USE.NAMES=FALSE )
	edges$ddg.to <- mapply( paste , prefix , edges$ddg.to , sep='' , USE.NAMES=FALSE )
	
	# column names
	col.names <- c("prov:activity", "prov:entity")
	
	# convert to json
	json <- .ddg.json.dataframe ( edges , col.names , 'pd' , 
										comment = "procedure-to-data edges" ,
										indent = indent )
	
	# form wasGeneratedBy node
	json <- .ddg.json.formNode( "wasGeneratedBy" , json , last.node = FALSE , indent = indent )
	
	return( json )
}

.ddg.json.data2proc <- function( edges , prefix , indent = 4 )
{
	# extract data-to-procedure edges, where ddg.type is 'df.in' (data flow in)
	edges <- subset(edges, ddg.type == "df.in", select = c(ddg.from, ddg.to))
	
	# add prefix to node numbers
	edges$ddg.from <- mapply( paste , prefix , edges$ddg.from , sep='' , USE.NAMES=FALSE )
	edges$ddg.to <- mapply( paste , prefix , edges$ddg.to , sep='' , USE.NAMES=FALSE )
	
	# column names
	col.names <- c("prov:entity", "prov:activity")
	
	# convert to json
	json <- .ddg.json.dataframe ( edges , col.names , 'dp' , 
										comment = "data-to-procedure edges" ,
										indent = indent )
	
	# add comma and newline to last node for combining
	json <- sub( '\n$' , ',\n\n' , json )
	
	return( json )
}

.ddg.json.func2proc <- function( nodes , prefix , indent = 4 )
{
	# extract columns
	edges <- subset( nodes , select = c("ddg.fnum", "ddg.pnum") )
	
	# add prefix to node numbers
	edges$ddg.fnum <- mapply( paste , prefix , 'f' , edges$ddg.fnum , sep='' , USE.NAMES=FALSE )
	edges$ddg.pnum <- mapply( paste , prefix , 'p' , edges$ddg.pnum , sep='' , USE.NAMES=FALSE )
	
	# column names
	col.names <- c( "prov:entity" , "prov:activity" )
	
	# convert to json
	json <- .ddg.json.dataframe ( edges , col.names , 'fp' , 
										comment = "function-to-procedure edges" ,
										indent = indent )
	return( json )
}

.ddg.json.lib2func <- function( nodes , prefix , indent = 4 )
{
	# extract columns
	nodes <- subset( nodes , select = c("ddg.lnum", "ddg.fnum") )
	
	# extract unique rows
	# order by fnum, then lnum
	nodes <- unique(nodes)
	nodes <- nodes[ order(nodes$ddg.fnum) , ]
	nodes <- nodes[ order(nodes$ddg.lnum) , ]
	
	# add prefix to node numbers
	nodes$ddg.lnum <- mapply( paste , prefix , 'l' , nodes$ddg.lnum , sep='' , USE.NAMES=FALSE )
	nodes$ddg.fnum <- mapply( paste , prefix , 'f' , nodes$ddg.fnum , sep='' , USE.NAMES=FALSE )
	
	# column names
	col.names <- c( "prov:collection" , "prov:entity" )
	
	# convert to json
	json <- .ddg.json.dataframe ( nodes , col.names , 'm' , 
										comment = "groups function nodes with their library nodes" ,
										indent = indent )
	
	# form hadMember node
	json <- .ddg.json.formNode( "hadMember" , json , last.node = TRUE , indent = indent )
	
	return( json )
}


.ddg.json.combine <- function( json , indent = 4 )
{
	# entity: merge content & wrap into node
	entity.1 <- sub( '\n$' , json$entity.env , json$entity.data )
	entity.2 <- sub( '\n$' , json$entity.func , json$entity.lib )
	
	entity <- sub( '\n$' , entity.2 , entity.1 )
	entity <- .ddg.json.formNode( "entity" , entity , last.node = FALSE , indent = indent )
	
	# used: merge content & wrap into node
	used <- sub( '\n$' , json$used.f2p , json$used.d2p )
	used <- .ddg.json.formNode( "used" , used , last.node = FALSE , indent = indent )
	
	
	# combine: prefix & activity nodes
	prefix.activity <- sub( '\n$' , json$activity , json$prefix )
	
	# combine: wasInformedBy & wasGeneratedBy
	informed.generated <- sub( '\n$' , json$wasGeneratedBy , json$wasInformedBy )
	
	# combine: used, hadMember
	used.hadMember <- sub( '\n$' , json$hadMember , used )
	
	
	# combine: prefix, activity, entity
	combined.1 <- sub( '\n$' , entity , prefix.activity )
	
	# combine: wasInformedBy, wasGeneratedBy, used, hadMember
	combined.2 <- sub( '\n$' , used.hadMember , informed.generated )
	
	
	# COMBINE ALL ELEMENTS & RETURN
	combined <- sub( '\n$' , combined.2 , combined.1 )
	
	json.full <<- combined
	return( combined )
}

# ddg.installedpackages() returns information on packages installed at the time of execution
# and their versions.
.ddg.installedpackages <- function()
{
	packages <- devtools::session_info()
	packages <- packages [[2]]
	installed <- packages[packages[,2] == "*",]
	installed <- installed[ ,c(1,3)]
	return(installed)
}



# UTILS

.ddg.json.escape.quotes <- function( string )
{
	return( gsub('\"', '\\\\"', string) )
}


.ddg.json.dataframe <- function( dataframe , col.names , obj.prefix , comment = NULL , indent = 4 )
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
	json <- prettify(json, indent = indent)
	
	
	# PROCESS JSON INTO CORRECT FORMAT
	# add comment line to top of block, if any
	if( ! is.null(comment) )
	{
		comment <- paste( '{\n        //' , comment )
		json <- sub( '^\\{' , comment , json )
	}
	
	# remove square brackets
	json <- gsub( '\\[\n        \\{' , '\\{' , json )
	json <- gsub( '\n    ]' , '' , json )
	
	# add indentation to object names
	json <- gsub( '\n    \"' , '\n        \"' , json )
	
	# remove row names in objects
	json <- gsub( ',\n            "_row": "[a-z]+[0-9]+"\n' , '\n' , json)
	
	# replace wrapping braces with newline characters
	json <- sub( '^\\{' , '' , json )
	json <- sub( '}\n$' , '' , json )
	
	return(json)
}

.ddg.json.formNode <- function( node.name , node.content , last.node = FALSE , indent = 4 )
{
	# form node name string
	node.name <- paste( '\n    "' , node.name , '" : \\{\n' , sep = '' )
	
	# top: wrap with name of node
	node <- sub( '^\n' , node.name , node.content )
	
	# botton: wrap with close brace
	if( ! last.node )
		node <- sub( '\n$' , '\n    },\n\n' , node )
	else
		node <- sub( '\n$' , '\n    }\n}' , node )	# ends the json file
	
	return( node )
}

