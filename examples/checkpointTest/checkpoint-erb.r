# require(gdata)

# The maximum number of checkpoints that can be associated with
# a ddg at a time
ddg.MAX_CHECKPOINTS <<- 10

# The number of checkpoints currently.
ddg.checkpoint.num <<- 0


# Remove the objects that were used to temporarily save the current ddg
# when restoring state from a checkpoint.  It is important that these 
# names do not exist so that when we do a restore they do not get 
# overwritten by values from an earlier execution
ddg.remove.saved.ddg.objects <- function() {
	remove(ddg.saved.type, inherits=TRUE)
	remove(ddg.saved.num, inherits=TRUE)
	remove(ddg.saved.name, inherits=TRUE)
	remove(ddg.saved.value, inherits=TRUE)
	remove(ddg.saved.time, inherits=TRUE)
	remove(ddg.saved.loc, inherits=TRUE)
	remove(ddg.saved.proc.nodes, inherits=TRUE)
	remove(ddg.saved.data.nodes, inherits=TRUE)
	remove(ddg.saved.pnum, inherits=TRUE)
	remove(ddg.saved.dnum, inherits=TRUE)
	remove(ddg.saved.text, inherits=TRUE)
	remove(ddg.saved.checkpoint.num, inherits=TRUE)
	remove(ddg.saved.debug, inherits=TRUE)
}

# Remove the special saved ddg objects if any were left 
# over from a previous loading of the library.
if (exists("ddg.saved.type")) {
	ddg.remove.saved.ddg.objects()
}

# Save the current R state in a file and add
# a checkpoint node to the ddg.  
# Returns the name of the file that the checkpoint is saved in.
ddg.checkpoint <- function(checkpoint.name="") {
	if (ddg.debug) print("Creating checkpoint")
	file.name <- paste(ddg.checkpoint.num, ".RData", sep="")
	file.path <- paste(ddg.path, "/", file.name, sep="")
	save.image(file.path)
	ddg.checkpoint.num <<- (ddg.checkpoint.num + 1) %% ddg.MAX_CHECKPOINTS
  if (checkpoint.name=="") checkpoint.name = "not recorded"
	ddg.proc.node("Checkpoint","ddg.checkpoint",checkpoint.name)	
	# create control flow edge from preceding procedure node
	ddg.proc2proc()
	checkpoint.file <- ddg.file.out("ddg.checkpoint", file.name, file.path)
	return (checkpoint.file)
}

# Save the current ddg state in new objects so that it does not
# get overwritten when restoring the R state.
ddg.save.ddg.state <- function() {
	ddg.saved.type <<- ddg.type
	ddg.saved.num <<- ddg.num
	ddg.saved.name <<- ddg.name
	ddg.saved.value <<- ddg.value
	ddg.saved.time <<- ddg.time
	ddg.saved.loc <<- ddg.loc
	ddg.saved.proc.nodes <<- ddg.proc.nodes
	ddg.saved.data.nodes <<- ddg.data.nodes
		
	ddg.saved.pnum <<- ddg.pnum
	ddg.saved.dnum <<- ddg.dnum
	
	ddg.saved.text <<- ddg.text
	
	ddg.saved.checkpoint.num <<- ddg.checkpoint.num
	
	ddg.saved.debug <<- ddg.debug
}

# Update the "current" attribute of the data nodes.  The "current" attribute
# is used to determine which value in the data node table corresponds to the
# use of a data item.  We want it to find the latest value before the 
# checkpoint.
#
# For data that was in the table when the checkpoint was made,
# the "current" attribute should be the same as in the checkpointed table.
# For data that was created after the checkpoint was made, the
# "current" attribute should be false.
#
# The entries for file nodes are not updated since the files themselves have
# whatever is in their current state.  They are not rolled back by a restore.
ddg.mark.stale.data <- function() {
	# Mark the data that was in the checkpointed table to be 
	# consistent with the checkpoint
	for (i in 1:ddg.dnum) {
		if (ddg.saved.data.nodes$ddg.type[i] != "File") {
			ddg.saved.data.nodes$ddg.current[i] <<- ddg.data.nodes$ddg.current[i]
		}
	}
	
	# Mark the data entries made after the checkpoint to be not current.
	for (i in (ddg.dnum+1):ddg.saved.dnum) {
		if (ddg.saved.data.nodes$ddg.type[i] != "File") {
			ddg.saved.data.nodes$ddg.current[i] <<- FALSE
		}
	}
}

# Restore the saved ddg information to be the current information.
ddg.restore.ddg.state <- function() {
	ddg.type <<- ddg.saved.type
	ddg.num <<- ddg.saved.num
	ddg.name <<- ddg.saved.name
	ddg.value <<- ddg.saved.value
	ddg.time <<- ddg.saved.time
	ddg.loc <<- ddg.saved.loc
	ddg.proc.nodes <<- ddg.saved.proc.nodes
	ddg.data.nodes <<- ddg.saved.data.nodes
	
	ddg.pnum <<- ddg.saved.pnum
	ddg.dnum <<- ddg.saved.dnum
	
	ddg.text <<- ddg.saved.text
	
	ddg.checkpoint.num <<- ddg.saved.checkpoint.num
	
	ddg.debug <<- ddg.saved.debug
}

# Restore the state saved in a checkpoint file.
# Update the ddg tables so that the ddg will get extended to
# include the actions between the chekcpoint and restore, but data edges
# will link to the data that existed at the time the checkpoint was made.
ddg.restore <- function(file.path) {
	
	# Explicitly giving file name.  If I let the ddg.procedure function look
	# it up, it also tries to bind the parameter, but there is no data node
	# corresponding to the parameter.
	checkpoint.name = deparse(substitute(file.path))
  ddg.proc.node("Restore","ddg.restore",checkpoint.name)	
	# create control flow edge from preceding procedure node
	ddg.proc2proc()
	# Remove the directories
	directories <- strsplit (file.path, "/")[[1]]
	file.name <- directories[length(directories)]
	# Remove the node number
	file.name.start <- regexpr("-", file.name) + 1
	file.name.end <- nchar(file.name)
	file.name <- substr(file.name, file.name.start, file.name.end)
	ddg.data.in("ddg.restore", file.name)
	
	if (ddg.debug) print(paste("Restoring from", file.path))
	ddg.save.ddg.state()	
	load (file.path, parent.env(environment()))
	ddg.mark.stale.data()
	ddg.restore.ddg.state()
	ddg.remove.saved.ddg.objects()
}