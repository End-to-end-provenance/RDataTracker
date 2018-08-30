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

context( "Unit Test for OutputJson.R" )

# edge case: when prov.json is run without initialising provenance capture
test_that( "provenance capture not initialised",
	{
		expect_warning( .ddg.json.write() )
		expect_true( is.null(.ddg.json.write()) )
		
		expect_warning( .ddg.json.string() )
		expect_true( is.null(.ddg.json.string()) )
	}
)

# prefix
test_that("prefix",
	{
		# general case only - it is almost a constant
		# null case not possible
		prefix.node <- list( "prov" = "http://www.w3.org/ns/prov#" ,
							 "rdt" = "http://rdatatracker.org/" )
		expect_equal( as.character( .ddg.json.prefix(prefix.node) ),
					  read.as.string("./testOutputJson/expected/prefix.json") )
	}
)

# agent - don't test
# it get its tool version internal to the function

# procedure nodes
test_that("procedure nodes",
	{
		# null case
		.ddg.set("ddg.proc.nodes",
				  read.csv("./testOutputJson/cases/proc_null.csv", stringsAsFactors=FALSE))
		expect_true( is.na(.ddg.json.proc(NULL, NULL)) )
		
		# general case: 1 node
		.ddg.set("ddg.proc.nodes",
				  read.csv("./testOutputJson/cases/proc_1.csv", stringsAsFactors=FALSE))
		expect_equal( as.character( .ddg.json.proc(
						LABEL.NAMES$activity.proc, 
						LABEL.PREFIX) ), 
					  read.as.string("./testOutputJson/expected/proc_1.json") )
		
		# general case: 3 nodes
		.ddg.set("ddg.proc.nodes",
				  read.csv("./testOutputJson/cases/proc_3.csv", stringsAsFactors=FALSE))
		expect_equal( as.character( .ddg.json.proc(
						LABEL.NAMES$activity.proc, 
						LABEL.PREFIX) ), 
					  read.as.string("./testOutputJson/expected/proc_3.json") )
	}
)

# data nodes
test_that("data nodes", 
	{
		# null case
		.ddg.set("ddg.data.nodes",
				  read.csv("./testOutputJson/cases/data_null.csv", stringsAsFactors=FALSE))
		expect_true( is.na(.ddg.json.data(NULL, NULL)) )
		
		# general case: 1 node
		nodes <- data.frame( "ddg.type" = "Data",
							 "ddg.num" = 1,
							 "ddg.name" = "f",
							 "ddg.value" = "#ddg.function",
							 "ddg.val.type" = "{\"container\":\"vector\", \"dimension\":[1], \"type\":[\"character\"]}",
							 "ddg.scope" = "R_GlobalEnv",
							 "ddg.from.env" = FALSE,
							 "ddg.time" = "",
							 "ddg.hash" = "",
							 "ddg.rw" = "",
							 "ddg.loc" = "",
							 stringsAsFactors = FALSE )
		.ddg.set("ddg.data.nodes", nodes)
		expect_equal( as.character( .ddg.json.data(
						LABEL.NAMES$entity.data, 
						LABEL.PREFIX) ), 
					  read.as.string("./testOutputJson/expected/data_1.json") )
		
		# general case: 3 nodes
		nodes <- data.frame( "ddg.type" = c("Data","Data","Exception"),
							 "ddg.num" = c(1,2,3),
							 "ddg.name" = c("f","a","error.msg"),
							 "ddg.value" = 
								c("#ddg.function", 1,
								  'Error in .ddg.console.node(): could not find function ".ddg.console.node"'),
							 "ddg.val.type" =
								c('{"container":"vector", "dimension":[1], "type":["character"]}',
								  '{"container":"vector", "dimension":[1], "type":["numeric"]}',
								  '{"container":"vector", "dimension":[1], "type":["character"]}'),
							 "ddg.scope" = c("R_GlobalEnv", "R_GlobalEnv", "ddg.library"),
							 "ddg.from.env" = c(FALSE, FALSE, FALSE),
							 "ddg.time" = c("","",""),
							 "ddg.hash" = c("","",""),
							 "ddg.rw" = c("","",""),
							 "ddg.loc" = c("","",""),
							 stringsAsFactors = FALSE )
		.ddg.set("ddg.data.nodes", nodes)
		expect_equal( as.character( .ddg.json.data(
						LABEL.NAMES$entity.data, 
						LABEL.PREFIX) ), 
					  read.as.string("./testOutputJson/expected/data_3.json") )
	}
)

# environment & sourced script - don't test
# they get their information in the function themselves 

# libraries
test_that("libraries",
	{
		# null case: not possible
		
		# min case 1: base in only library
		expect_equal( as.character( .ddg.json.lib(
						read.csv("./testOutputJson/cases/lib_min1.csv", stringsAsFactors=FALSE),
						LABEL.NAMES$entity.lib,
						LABEL.PREFIX) ),
					  read.as.string("./testOutputJson/expected/lib_min1.json") )

		# min case 2: base & RDT
		expect_equal( as.character( .ddg.json.lib(
						read.csv("./testOutputJson/cases/lib_min2.csv", stringsAsFactors=FALSE),
						LABEL.NAMES$entity.lib,
						LABEL.PREFIX) ),
					  read.as.string("./testOutputJson/expected/lib_min2.json") )

		# general case
		expect_equal( as.character( .ddg.json.lib(
						read.csv("./testOutputJson/cases/lib_gen.csv", stringsAsFactors=FALSE),
						LABEL.NAMES$entity.lib,
						LABEL.PREFIX) ),
					  read.as.string("./testOutputJson/expected/lib_gen.json") )
	}
)

# collection
test_that("collection",
	{
		# it's a constant!
		expect_equal( as.character( .ddg.json.collection() ),
					  read.as.string("./testOutputJson/expected/collection.json") )
	}
)

# function nodes
test_that("functions",
	{
		# function not called when num.calls == 0

		# general case: 1 function
		expect_equal( as.character( .ddg.json.func(
						read.csv("./testOutputJson/cases/func_1.csv", stringsAsFactors=FALSE),
						LABEL.NAMES$entity.func,
						LABEL.PREFIX) ),
					  read.as.string("./testOutputJson/expected/func_1.json") )

		# general case: 3 functions
		expect_equal( as.character( .ddg.json.func(
						read.csv("./testOutputJson/cases/func_3.csv", stringsAsFactors=FALSE),
						LABEL.NAMES$entity.func,
						LABEL.PREFIX) ),
					  read.as.string("./testOutputJson/expected/func_3.json") )
	}
)

# proc2proc edges
test_that("proc2proc edges",
	{
		# null case
		expect_equal( .ddg.json.proc2proc(
						read.csv("./testOutputJson/cases/edges_null.csv", stringsAsFactors=FALSE), 
						NULL, NULL),
					  NA )

		# general case: 1 edge
		expect_equal( as.character( .ddg.json.proc2proc(
						read.csv("./testOutputJson/cases/edges_1.csv", stringsAsFactors=FALSE),
						LABEL.NAMES$wasInformedBy.p2p,
						LABEL.PREFIX) ),
					  read.as.string("./testOutputJson/expected/proc2proc_1.json") )

		# general case: 3 edges
		expect_equal( as.character( .ddg.json.proc2proc(
						read.csv("./testOutputJson/cases/edges_3.csv", stringsAsFactors=FALSE),
						LABEL.NAMES$wasInformedBy.p2p,
						LABEL.PREFIX) ),
					  read.as.string("./testOutputJson/expected/proc2proc_3.json") )
	}
)

# proc2data edges
test_that("proc2data edges",
	{
		# null case 
		expect_equal( .ddg.json.proc2data(
						read.csv("./testOutputJson/cases/edges_null.csv", stringsAsFactors=FALSE), 
						NULL, NULL),
					  NA )

		# general case: 1 edge
		expect_equal( as.character( .ddg.json.proc2data(
						read.csv("./testOutputJson/cases/edges_1.csv", stringsAsFactors=FALSE),
						LABEL.NAMES$wasGeneratedBy.p2d,
						LABEL.PREFIX) ),
					  read.as.string("./testOutputJson/expected/proc2data_1.json") )

		# general case: 3 edges
		expect_equal( as.character( .ddg.json.proc2data(
						read.csv("./testOutputJson/cases/edges_3.csv", stringsAsFactors=FALSE),
						LABEL.NAMES$wasGeneratedBy.p2d,
						LABEL.PREFIX) ),
					  read.as.string("./testOutputJson/expected/proc2data_3.json") )
	}
)

# data2proc edges
test_that("data2proc edges",
	{
		# null case
		expect_equal( .ddg.json.data2proc(
						read.csv("./testOutputJson/cases/edges_null.csv", stringsAsFactors=FALSE), 
						NULL, NULL),
					  NA )

		# general case: 1 edge
		expect_equal( as.character( .ddg.json.data2proc(
						read.csv("./testOutputJson/cases/edges_1.csv", stringsAsFactors=FALSE),
						LABEL.NAMES$used.d2p,
						LABEL.PREFIX) ),
					  read.as.string("./testOutputJson/expected/data2proc_1.json") )

		# general case: 3 edges
		expect_equal( as.character( .ddg.json.data2proc(
						read.csv("./testOutputJson/cases/edges_3.csv", stringsAsFactors=FALSE),
						LABEL.NAMES$used.d2p,
						LABEL.PREFIX) ),
					  read.as.string("./testOutputJson/expected/data2proc_3.json") )
	}
)

# func2proc edges
test_that("func2proc edges",
	{
		# function not called when num.calls == 0
		
		# general case: 1 function
		expect_equal( as.character( .ddg.json.func2proc(
						read.csv("./testOutputJson/cases/func_1.csv", stringsAsFactors=FALSE),
						LABEL.NAMES$used.f2p,
						LABEL.NAMES$entity.func,
						LABEL.NAMES$activity.proc,
						LABEL.PREFIX) ),
					  read.as.string("./testOutputJson/expected/func2proc_1.json") )

		# general case: 3 functions
		expect_equal( as.character( .ddg.json.func2proc(
						read.csv("./testOutputJson/cases/func_3.csv", stringsAsFactors=FALSE),
						LABEL.NAMES$used.f2p,
						LABEL.NAMES$entity.func,
						LABEL.NAMES$activity.proc,
						LABEL.PREFIX) ),
					  read.as.string("./testOutputJson/expected/func2proc_3.json") )
	}
)

# lib2func edges
test_that("lib2func edges",
	{
		# function not called when num.calls == 0
		# general case: 1 function
		expect_equal( as.character( .ddg.json.lib2func(
						read.csv("./testOutputJson/cases/func_1.csv", stringsAsFactors=FALSE),
						LABEL.NAMES$hadMember,
						LABEL.NAMES$entity.lib,
						LABEL.NAMES$entity.func,
						LABEL.PREFIX) ),
					  read.as.string("./testOutputJson/expected/lib2func_1.json") )

		# general case: 3 functions
		expect_equal( as.character( .ddg.json.lib2func(
						read.csv("./testOutputJson/cases/func_3.csv", stringsAsFactors=FALSE),
						LABEL.NAMES$hadMember,
						LABEL.NAMES$entity.lib,
						LABEL.NAMES$entity.func,
						LABEL.PREFIX) ),
					  read.as.string("./testOutputJson/expected/lib2func_3.json") )
	}
)
