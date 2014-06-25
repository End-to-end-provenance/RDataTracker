
# The maximum number of checkpoints that can be associated with
# a ddg at a time
ddg.MAX_CHECKPOINTS <<- 10

# The number of checkpoints currently.
ddg.checkpoint.num <<- 0

ddg.checkpoints <<- data.frame(filename="", checkpoint.name="", stringsAsFactors=FALSE)

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
	remove(ddg.saved.checkpoints, inherits=TRUE)
	remove(ddg.saved.history.length, inherits=TRUE)
	remove(ddg.saved.history.file, inherits=TRUE)
	remove(ddg.saved.debug, inherits=TRUE)
}

# Remove the special saved ddg objects if any were left 
# over from a previous loading of the library.
if (exists("ddg.saved.type")) {
	ddg.remove.saved.ddg.objects()
}

ddg.checkpoint.file.node <- function(fname,dname, checkpoint.name) {
	# increment data counter
	ddg.dnum <<- ddg.dnum + 1
	
	# get checkpoint file location
	file.name <- basename(fname)
	file.loc <- dirname(normalizePath(fname, mustWork = FALSE))
	
	loc.value <- ""
	
	# add number to file name
	dfile <- paste(ddg.dnum,"-",file.name,sep="")
	# get path plus file name
	dpfile.out <- paste(ddg.path,"/",dfile,sep="")
	
	# Take the checkpoint
	if (ddg.debug) print(paste("Saving checkpoint in", dpfile.out))
	save.image(dpfile.out)

	# get timestamp
	dtime <- ddg.timestamp()
	# set the node label
	#if (is.null(dname)) dname <- file.name
	#if (!is.null(checkpoint.name)) dname <- paste(file.name, checkpoint.name)
	# create file node
	#ddg.text <<- paste(ddg.text, "CheckpointFile"," d",ddg.dnum," \"",ddg.dnum,"-",dname,"\" Value=\"",dpfile.out,"\" Time=\"",dtime,"\"",loc.value,";\n",sep="")
	ddg.text <<- paste(ddg.text, "CheckpointFile"," d",ddg.dnum," \"",ddg.dnum,"-",file.name,"\" Value=\"",dpfile.out,"\" Time=\"",dtime,"\"",loc.value,";\n",sep="")
	# record data node information
	ddg.record.data("Checkpoint",file.name,dfile,dtime,file.loc)
	# debug	
	if (ddg.debug) print(paste("file.copy: Checkpoint", fname))
	return (dpfile.out)
}

#ddg.checkpoint.out <- function(nodename, filename, checkpoint.name) {
ddg.checkpoint.out <- function(checkpoint.name, filename) {
	dname <- basename(filename)
	
	# create output file node where name = filename
	# copy file
	saved.file <- ddg.checkpoint.file.node(filename,dname, checkpoint.name)
	
	# create data flow edge from operation node to file node
	#ddg.proc2data(nodename, dname)
	ddg.proc2data(checkpoint.name, dname)
	
	return (saved.file)
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
	ddg.saved.checkpoints <<- ddg.checkpoints
	
	ddg.saved.history.file <<- ddg.history.file
	ddg.saved.history.length <<- ddg.history.length
	
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
	ddg.files.to.restore <- matrix('', nr=0, nc=2, dimnames=list(NULL, c('filename', 'original')))
	
	for (i in 1:ddg.dnum) {
		ddg.saved.data.nodes$ddg.current[i] <<- ddg.data.nodes$ddg.current[i]

		# Determine what the most recent version of each file is at the time the 
		# checkpoint was taken.
		if (ddg.saved.data.nodes$ddg.type[i] == "File") {
			nRow<-which(ddg.files.to.restore[,2] == ddg.saved.data.nodes$ddg.loc[i])
			if (length(nRow) > 0) {
				ddg.files.to.restore[nRow,1] <- paste(ddg.saved.data.nodes$ddg.num[i], "-", ddg.saved.data.nodes$ddg.name[i], sep="")
			}
			else {
				newfile = c(paste(ddg.saved.data.nodes$ddg.num[i], "-", ddg.saved.data.nodes$ddg.name[i], sep=""), ddg.saved.data.nodes$ddg.loc[i])
				ddg.files.to.restore <- rbind(ddg.files.to.restore,newfile)
			}
		}
		
		else if (ddg.saved.data.nodes$ddg.type[i] == "Checkpoint") {
			ddg.saved.data.nodes$ddg.current[i] <<- TRUE
		}
	}
	
	# Mark the data entries made after the checkpoint to be not current.
	if (ddg.dnum < ddg.saved.dnum) {
		for (i in (ddg.dnum+1):ddg.saved.dnum) {
			if (ddg.saved.data.nodes$ddg.type[i] != "Checkpoint") {
				ddg.saved.data.nodes$ddg.current[i] <<- FALSE
			}
		}
	}
	
	num.files.to.restore <- nrow(ddg.files.to.restore)
	if (num.files.to.restore > 0) {
		for (i in 1:num.files.to.restore) {
			original <- ddg.files.to.restore[i,2]
			saved <- paste(ddg.path, ddg.files.to.restore[i,1], sep="/")
			
			# Original file is newer than the one to be restored from the DDG.
			# Save the original file in a special place before restoring the DDG file.
			if (file.exists(original)) {
				modTime.original <- file.info(original)$mtime[1]
				modTime.saved <- file.info(saved)$mtime[1]
				if (modTime.original >= modTime.saved) {
					rescue.dir <- paste(getwd(), "/ddg.rescued.files/", sep="")
					dir.create(rescue.dir, showWarnings=FALSE)
					rescue.filename <- paste(rescue.dir, modTime.original, "-", basename(original), sep="")
					warning(paste("Saving ", original, " in ", rescue.filename, sep=""))
					file.copy(original, rescue.filename, overwrite=TRUE)
					file.copy(saved,original, overwrite=TRUE)
				}
			}
			
			# Original file no longer exists. Just copy in the DDG file.
			else {
				file.copy(saved,original)
			}
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
	ddg.checkpoints <<- ddg.saved.checkpoints
	
	ddg.history.file <<- ddg.saved.history.file
	ddg.history.length <<- ddg.saved.history.length
	
	ddg.debug <<- ddg.saved.debug
}

ddg.record.checkpoint <- function(filename, checkpoint.name) {
	# record procedure node information
	ddg.checkpoints[ddg.checkpoint.num,1] <<- filename
	ddg.checkpoints[ddg.checkpoint.num,2] <<- checkpoint.name
}

ddg.lookup.checkpoint.name <- function(filename) {
	nRow<-which(ddg.checkpoints[,1] == filename)
	return(ddg.checkpoints[nRow,2])
}

### User Functions

# Save the current R state in a file and add
# a checkpoint node to the ddg.  
# Creates a procedural node labeled ddg.checkpoint and a snapshot node labeled
# with the name of the checkpoint file created.
# Returns the full path to the file that the checkpoint is saved in.
# checkpoint.name (Optional) - the value associated with the checkpoint procedure node.
ddg.checkpoint <- function(checkpoint.name=NULL) {
	if (ddg.debug) print("Creating checkpoint")
	file.name <- paste(ddg.checkpoint.num, ".RData", sep="")
	file.path <- paste(ddg.path, "/", file.name, sep="")
	if (is.null(checkpoint.name)) checkpoint.name <- file.name
	#node.name <- paste("ddg.checkpoint", checkpoint.name)
	#ddg.proc.node("Checkpoint", node.name, checkpoint.name)	
	ddg.proc.node("Checkpoint", checkpoint.name)	
	# create control flow edge from preceding procedure node
	ddg.proc2proc()
	#checkpoint.file <- ddg.checkpoint.out(node.name, file.path, checkpoint.name)
	checkpoint.file <- ddg.checkpoint.out(checkpoint.name, file.path)
	ddg.checkpoint.num <<- (ddg.checkpoint.num + 1) %% ddg.MAX_CHECKPOINTS
	ddg.record.checkpoint(file.name, checkpoint.name)
	return (checkpoint.file)
}

# Restore the state saved in a checkpoint file.
# Creates a procedure node in the ddg labeled ddg.restore
# Creates a link from the file node representing the checkpointed file
# 	to the restore node.
# file.path - the name of the checkpoint file to restore
ddg.restore <- function(file.path) {
	# Remove the directories
	file.name <- basename(file.path)
	# Remove the node number
	file.name.start <- regexpr("-", file.name) + 1
	file.name.end <- nchar(file.name)
	file.name <- substr(file.name, file.name.start, file.name.end)
	
	# checkpoint.name = deparse(substitute(file.path))
	checkpoint.name <- ddg.lookup.checkpoint.name(file.name)
	#node.name <- paste("ddg.restore", checkpoint.name)
	#ddg.proc.node("Restore",node.name,checkpoint.name)	
	ddg.proc.node("Restore",checkpoint.name)	
	# create control flow edge from preceding procedure node
	ddg.proc2proc()
	#ddg.data.in(node.name, file.name)
	ddg.data.in(checkpoint.name, file.name)
	
	if (ddg.debug) print(paste("Restoring from", file.path))
	
	# Update the ddg tables so that the ddg will get extended to
	# include the actions between the chekcpoint and restore, but data edges
	# will link to the data that existed at the time the checkpoint was made.
	ddg.save.ddg.state()	
	load (file.path, parent.env(environment()))
	ddg.mark.stale.data()
	ddg.restore.ddg.state()
	ddg.remove.saved.ddg.objects()
}


