# @author Elizabeth Fong
# @version August 2018

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

read.as.string <- function( url )
{
	return( paste(readLines(url), collapse = "\n") )
}

# =========================================================================== #

# ::: not necessary for cran check case, apparently
context( "Unit Test for OutputJson.R" )

# test prefix
# general case only - it is almost a constant
# null case not possible
prefix.node <- list( "prov" = "http://www.w3.org/ns/prov#" ,
					 "rdt" = "http://rdatatracker.org/" )
expect_equal( as.character( RDataTracker:::.ddg.json.prefix(prefix.node) ),
			  read.as.string("expected/prefix.json") )


# test agent
# genearal case only - null case not possible

# procedure & data nodes - don't test
# they get their node tables in the functions themselves

# environment & sourced script - don't test
# they get their information in the function themselves 


# LIBRARIES
# null case is not possible

# min case 1: base in only library
expect_equal( as.character( RDataTracker:::.ddg.json.lib(
				read.csv("cases/lib_min1.csv", stringsAsFactors=FALSE),
				LABEL.NAMES$entity.lib,
				LABEL.PREFIX) ),
			  read.as.string("expected/lib_min1.json") )

# min case 2: base & RDT
expect_equal( as.character( RDataTracker:::.ddg.json.lib(
				read.csv("cases/lib_min2.csv", stringsAsFactors=FALSE),
				LABEL.NAMES$entity.lib,
				LABEL.PREFIX) ),
			  read.as.string("expected/lib_min2.json") )

# general case
expect_equal( as.character( RDataTracker:::.ddg.json.lib(
				read.csv("cases/lib_gen.csv", stringsAsFactors=FALSE),
				LABEL.NAMES$entity.lib,
				LABEL.PREFIX) ),
			  read.as.string("expected/lib_gen.json") )


# collection - it's a constant!
expect_equal( as.character( RDataTracker:::.ddg.json.collection() ),
			  read.as.string("expected/collection.json") )


# function nodes
# function not called when num.calls == 0


# proc2proc edges
# null case
expect_equal( RDataTracker:::.ddg.json.proc2proc(
				read.csv("cases/edges_null.csv", stringsAsFactors=FALSE), 
				NULL, NULL),
			  NA )

# proc2data edges
# null case 
expect_equal( RDataTracker:::.ddg.json.proc2data(
				read.csv("cases/edges_null.csv", stringsAsFactors=FALSE), 
				NULL, NULL),
			  NA )

# data2proc edges
# null case
expect_equal( RDataTracker:::.ddg.json.data2proc(
				read.csv("cases/edges_null.csv", stringsAsFactors=FALSE), 
				NULL, NULL),
			  NA )

# func2proc edges
# function not called when num.calls == 0

# lib2func edges
# function not called when num.calls == 0

# UTILITY TEST
# test escape quotes

# test escape tabs



