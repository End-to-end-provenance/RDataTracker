# Copyright (C) President and Fellows of Harvard College and 
# Trustees of Mount Holyoke College, 2014, 2015, 2016, 2017.

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

########### Checkpoint/Restore functions for DDGs #############

# The functions in this file can be used to save the state of
# an R environment to a file and restore it at a later time.
# The DDG is updated to reflect the actions of the checkpoint
# and restore operations so that data derivations are shown
# accurately.

# These functions cannot be included in RDataTracker because
# libraries are not allowed to change the user's environment,
# which the ddg.restore function needs to do.  To use these
# functions, the user must "source" this file.


# .ddg.checkpoint.file.node creates a checkpoint file node.
# @param fname name of checkpoint file
# @param dname not used
# @param checkpoint.name not used
# @return path and name of checkpoint file

.ddg.checkpoint.file.node <- function(fname, dname, checkpoint.name) {
	# Increment data counter.
	RDataTracker:::.ddg.inc("ddg.dnum")
	
	# Get checkpoint file location.
	file.name <- basename(fname)
	file.loc <- dirname(normalizePath(fname, winslash="/", mustWork = FALSE))
	
	# Add number to file name.
	ddg.dnum <- RDataTracker:::.ddg.dnum()
	dfile <- paste(ddg.dnum, "-", file.name, sep="")
	
	# Get path plus file name.
	dpfile.out <- paste(RDataTracker:::.ddg.path(), "/", dfile, sep="")
	
	if (RDataTracker:::.ddg.debug()) print(paste("Saving checkpoint in", dpfile.out))
	
	# Create checkpoint.
	toSave <- c (ls(envir = .GlobalEnv, all.names = TRUE)) 
	if (RDataTracker:::.ddg.debug()) { print("Saving"); print(toSave) }

	# toSave <- ls(envir=.GlobalEnv)
	# save (list = toSave, file = dpfile.out, envir = parent.frame(3))			
	save (list = toSave, file = dpfile.out)		
	ddg.dpfile.out <- paste(dpfile.out, "ddg", sep=".")
	if (RDataTracker:::.ddg.debug()) { print("Saving"); print(ls(RDataTracker:::.ddg.env, all.names=TRUE)) }
	save (list = ls(RDataTracker:::.ddg.env, all.names=TRUE), file=ddg.dpfile.out, envir=RDataTracker:::.ddg.env)
	
	# Create the node.
	dtime <- RDataTracker:::.ddg.timestamp()
	RDataTracker:::.ddg.append("CheckpointFile", " d", ddg.dnum, " \"", ddg.dnum, "-", file.name, "\" Value=\"", dpfile.out, "\" Time=\"", dtime, "\";\n", sep="")
	
	# Record data node information.
	RDataTracker:::.ddg.record.data("Checkpoint", file.name, dfile, "undefined", dtime, file.loc)
	
	if (RDataTracker:::.ddg.debug()) print(paste("file.copy: Checkpoint", fname))
	return (dpfile.out)
}

# .ddg.checkpoint.out creates a checkpoint node and data flow edge.
# @param checkpoint.name name of checkpoint
# @param filename name of checkpoint file
# @return path and name of checkpoint file

.ddg.checkpoint.out <- function(checkpoint.name, filename) {
	dname <- basename(filename)
	
	# Create output file node where name = filename.
	saved.file <- .ddg.checkpoint.file.node(filename, dname, checkpoint.name)
	
	# Create data flow edge from operation node to file node.
	RDataTracker:::.ddg.proc2data(checkpoint.name, dname)
	
	return (saved.file)
}

# .ddg.record.checkpoint records the procedure node information for 
# a checkpoint.
# @param filename name of checkpoint file
# @param checkpoint.name name of checkpoint
# @return nothing

.ddg.record.checkpoint <- function(filename, checkpoint.name) {
	ddg.checkpoint.num <- RDataTracker:::.ddg.get("ddg.checkpoint.num")
	ddg.checkpoints <- RDataTracker:::.ddg.get("ddg.checkpoints")
	ddg.checkpoints$filename[ddg.checkpoint.num] <- filename
	ddg.checkpoints$checkpoint.name[ddg.checkpoint.num] <- checkpoint.name
	RDataTracker:::.ddg.set("ddg.checkpoints", ddg.checkpoints)
}

# ddg.checkpoint saves the current R state in a file and adds a 
# checkpoint node to the DDG. It creates a procedural node labeled 
# with the checkpoint name and a snapshot node labeled with the name 
# of the checkpoint file created. It returns the full path to the 
# file that the checkpoint is saved in.
# checkpoint.name (optional) - the value associated with the 
#   checkpoint procedure node.
# @param checkpoint.name name of checkpoint
# @return path and name of checkpoint file

ddg.checkpoint <- function(checkpoint.name=NULL) {
	if (!RDataTracker:::.ddg.is.init()) return(invisible())
	
	if (RDataTracker:::.ddg.debug()) print("Creating checkpoint")
	ddg.checkpoint.num <- RDataTracker:::.ddg.get("ddg.checkpoint.num")
	file.name <- paste(ddg.checkpoint.num, ".RData", sep="")
	file.path <- paste(RDataTracker:::.ddg.path(), "/", file.name, sep="")
	if (is.null(checkpoint.name)) checkpoint.name <- file.name
	RDataTracker:::.ddg.proc.node("Checkpoint", checkpoint.name)	
	
	# Create control flow edge from preceding procedure node.
	RDataTracker:::.ddg.proc2proc()
	checkpoint.file <- .ddg.checkpoint.out(checkpoint.name, file.path)
	RDataTracker:::.ddg.set("ddg.checkpoint.num", (ddg.checkpoint.num + 1) %% RDataTracker:::ddg.MAX_CHECKPOINTS)
	.ddg.record.checkpoint(file.name, checkpoint.name)
	return (checkpoint.file)
}

# .ddg.lookup.checkpoint.name looks up the name of a checkpoint.
# @param filename name of checkpoint file
# @return name of checkpoint

.ddg.lookup.checkpoint.name <- function(filename) {
	ddg.checkpoints <- RDataTracker:::.ddg.get("ddg.checkpoints")
	nRow<-which(ddg.checkpoints$filename == filename)
	return(ddg.checkpoints$checkpoint.name[nRow])
}

# .ddg.mark.stale.data updates the "current" attribute of the data 
# nodes. The current attribute is used to determine which value in 
# the data node table corresponds to the use of a data item. We want 
# the latest value before the checkpoint to be found.
# For data that was in the table when the checkpoint was made, the 
# current attribute should be the same as in the checkpointed table. 
# For data that was created after the checkpoint was made, the 
# current attribute should be FALSE.
# The entries for files are examined to determine which was the 
# current version of each file at the time the checkpoint was taken. 
# If that file no longer exists, or has been modified, the version 
# of the file current at the time of the checkpoint is restored.
# @param saved.env name of saved environment
# @param checkpointed.env name of checkpoint environment
# @return nothing

.ddg.mark.stale.data <- function(saved.env, checkpointed.env) {
	ddg.files.to.restore <- matrix('', nrow=0, ncol=2, dimnames=list(NULL, c("filename", "original")))
	
	# Mark the data that was in the checkpointed table to be consistent 
	# with the checkpoint.
	ddg.dnum <- checkpointed.env[["ddg.dnum"]]
	ddg.data.nodes <- checkpointed.env[["ddg.data.nodes"]]
	ddg.saved.data.nodes <- saved.env[["ddg.data.nodes"]]
	for (i in 1:ddg.dnum) {
		ddg.saved.data.nodes$ddg.current[i] <- ddg.data.nodes$ddg.current[i]
		
		# Determine the most recent version of each file at the time the 
		# checkpoint was taken.
		if (ddg.saved.data.nodes$ddg.type[i] == "File") {
			nRow<-which(ddg.files.to.restore[,"original"] == ddg.saved.data.nodes$ddg.loc[i])
			if (length(nRow) > 0) {
				ddg.files.to.restore[nRow,"filename"] <- paste(ddg.saved.data.nodes$ddg.num[i], "-", ddg.saved.data.nodes$ddg.name[i], sep="")
			}
			else {
				newfile = c(paste(ddg.saved.data.nodes$ddg.num[i], "-", ddg.saved.data.nodes$ddg.name[i], sep=""), ddg.saved.data.nodes$ddg.loc[i])
				ddg.files.to.restore <- rbind(ddg.files.to.restore, newfile)
			}
		}
		
		else if (ddg.saved.data.nodes$ddg.type[i] == "Checkpoint") {
			ddg.saved.data.nodes$ddg.current[i] <- TRUE
		}
	}
	
	# Mark the data entries made after the checkpoint to be not current
	ddg.saved.dnum <- saved.env[["ddg.dnum"]]
	if (ddg.dnum < ddg.saved.dnum) {
		for (i in (ddg.dnum+1):ddg.saved.dnum) {
			if (ddg.saved.data.nodes$ddg.type[i] != "Checkpoint") {
				ddg.saved.data.nodes$ddg.current[i] <- FALSE
			}
		}
	}
	saved.env[["ddg.data.nodes"]] <- ddg.saved.data.nodes
	
	# Restore files that have been modified or deleted since the 
	# checkpoint.
	num.files.to.restore <- nrow(ddg.files.to.restore)
	if (num.files.to.restore > 0) {
		# ddg.path <- RDataTracker:::.ddg.path()
		ddg.path <- saved.env[["ddg.path"]]
		for (i in 1:num.files.to.restore) {
			original <- ddg.files.to.restore[i, 2]
			saved <- paste(ddg.path, ddg.files.to.restore[i, 1], sep="/")
			
			# Original file is newer than the one to be restored from the 
			# DDG. Save the original file in a special place before 
			# restoring the DDG file.
			if (file.exists(original)) {
				modTime.original <- file.info(original)$mtime[1]
				modTime.saved <- file.info(saved)$mtime[1]
				if (modTime.original >= modTime.saved) {
					
					# Windows does not allow colons in the names of directories
					modTime.original.str <- gsub(" ","T", gsub(":",".",as.character(modTime.original)))
					
					rescue.dir <- paste(getwd(), "/ddg.rescued.files/", sep="")
					dir.create(rescue.dir, showWarnings=FALSE)
					rescue.filename <- paste(rescue.dir, modTime.original.str, "-", basename(original), sep="")
					warning("Saving ", original, " in ", rescue.filename)
					file.copy(original, rescue.filename, overwrite=TRUE)
					file.copy(saved, original, overwrite=TRUE)
				}
			}
			
			# Original file no longer exists. Just copy in the DDG file.
			else {
				file.copy(saved, original)
			}
		}
	}
}

# .ddg.restore.ddg.state replaces the current information with the 
# saved DDG information.
# @param saved.env name of saved environment
# @param ddg.env name of ddg environment
# @return ddg environment

.ddg.restore.ddg.state <- function(saved.env, ddg.env) {	
	
	saved.names <- ls (saved.env, all.names = TRUE)
	for (saved.name in saved.names) {
		saved.value <- saved.env[[saved.name]]
		ddg.env[[saved.name]] <- saved.value
	}
	return (ddg.env)
}

# ddg.restore restores the state saved in a checkpoint file. It 
# creates a procedure node in the DDG labeled ddg.restore and a link 
# from the file node representing the checkpointed file to the 
# restore node.  The DDG tables are updated so the DDG will be 
# extended to include the actions between the checkpoint and restore 
# but the data flow edges will link to the data that existed when 
# the checkpoint was made. file.path - the name of the checkpoint 
# file to restore.
# @param file.path path and name of checkpoint file
# @return nothing

ddg.restore <- function(file.path) {
	if (!RDataTracker:::.ddg.is.init()) return(invisible())
	
	# Remove the directories.
	file.name <- basename(file.path)
	
	# Remove the node number.
	file.name.start <- regexpr("-", file.name) + 1
	file.name.end <- nchar(file.name)
	file.name <- substr(file.name, file.name.start, file.name.end)
	checkpoint.name <- .ddg.lookup.checkpoint.name(file.name)
	RDataTracker:::.ddg.proc.node("Restore", checkpoint.name)	
	
	# Create control flow edge from preceding procedure node.
	RDataTracker:::.ddg.proc2proc()
	RDataTracker:::.ddg.data2proc(file.name, "undefined", checkpoint.name)
	
	if (RDataTracker:::.ddg.debug()) print(paste("Restoring from", file.path))
	
	# Update the ddg tables so that the ddg will get extended to 
  # include the actions between the checkpoint and restore, but 
  # the data edges will link to the data that existed at the time 
  # the checkpoint was made.
	
	saved.ddg.env <- RDataTracker:::.ddg.env
	if (RDataTracker:::.ddg.debug()) load (file.path, .GlobalEnv, verbose=TRUE)
	else load (file.path, .GlobalEnv)
	ddg.file.path <- paste(file.path, "ddg", sep=".")
	checkpointed.ddg.env <- new.env()
	if (RDataTracker:::.ddg.debug()) load(ddg.file.path, checkpointed.ddg.env, verbose=TRUE)
	else load(ddg.file.path, checkpointed.ddg.env)
	.ddg.mark.stale.data(saved.ddg.env, checkpointed.ddg.env)
	assign("RDataTracker:::.ddg.env", .ddg.restore.ddg.state(saved.ddg.env, checkpointed.ddg.env))
	
	invisible()
}

