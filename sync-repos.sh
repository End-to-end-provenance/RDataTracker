#!/bin/bash

# This script keeps the rdtLite and rdt repositories in sync with RDataTracker.

# RDataTracker contains code used by both rdtLite and rdt.  It also does 
# extensive system-level testing using scripts.
#
# The rdtLite repository contains only code used in rdtLite, while the
# rdt repository contains only code used in RDataTracker.

# Running on athena.cs.mtholyoke.edu.  This is what is in the cron job:
#
# MAILTO=blerner@mtholyoke.edu
# 20 14 * * * /mnt/pool1/userhomes/MS/blerner/sync-repos.sh | tee -a /mnt/pool1/userhomes/MS/blerner/sync.log | mail -s 'athena rdt rsync' log $MAILTO
#
# This runs every day at 2:20 PM.  The output is appended to the sync.log file and 
# also emailed to me.

echo ""
echo "----------------------------------------------------------------"
echo ""
date

# Determines if the repository is already current with respect
# to the RDataTracker repository.  The first parameter is the 
# repository to copy to, the second is the branch to copy.
# The rdtLite and rdt repositories have a file called .commit.
# The file contains the commit number of the last update.  If this
# number is the same as RDataTracker's current commit,
# then the repository is already current and we do not need
# to update it.
#
# When called, we should be inside the RDataTracker repository.
function is_current {
  # Name the parameters
  TO_REPO=$1
  BRANCH=$2
  
  echo "*** Checking $TO_REPO $BRANCH"
  
  # Make sure both repos are on the desired branch
  git checkout $BRANCH
  STATUS=$(git status | head -n 1)
  if [ ! "$STATUS" = "On branch $BRANCH" ]
    then
      echo "Can't switch to $BRANCH of RDataTracker"
      cleanup
      exit 1
  fi
  cd ../$TO_REPO
  git checkout $BRANCH
  STATUS=$(git status | head -n 1)
  if [ ! "$STATUS" = "On branch $BRANCH" ]
    then
      echo "Can't switch to $BRANCH of $TO_REPO"
      cleanup
      exit 1
  fi
  
  cd ../RDataTracker

  # Get the latest commit number
  COMMIT=$(git show | head -n 1)
  echo "RDataTracker $BRANCH COMMIT is $COMMIT"

  # Compare to the commit # stored in $TO_REPO
  COMMIT_1=$(cat ../$TO_REPO/.commit)
  echo "TO_REPO last update was $COMMIT_1"

  # If they are the same we are done
  if [ "$COMMIT" == "$COMMIT_1" ] 
    then
      return 0
  fi
  
  return 1
}

# Copy just the files that we want in the rdtLite repository.  Some
# files need to be renamed when they are copied to remove _rdtLite
# from the name. 
function copy_rdtLite_files {
  echo "*** Copying files to rdtLite" 
  
  # Copy the shared files to rdtLite
  rsync -tv --perms inst/CITATION ../rdtLite/inst/
  rsync -tv --perms LICENSE ../rdtLite/
  rsync -tv --perms .Rbuildignore ../rdtLite/
  rsync -tv --perms tests.xml ../rdtLite/
  rsync -rtv --del --exclude */rdt/ --perms scriptTests ../rdtLite/
  
  # Copy rdtLite versions of files
  rsync -tv --perms DESCRIPTION_rdtLite ../rdtLite/DESCRIPTION
  rsync -tv --perms NAMESPACE_rdtLite ../rdtLite/NAMESPACE
  rsync -tv --perms README_rdtLite.md ../rdtLite/README.md
  rsync -tv --perms NEWS_rdtLite.md ../rdtLite/NEWS.md
  rsync -tv --perms .travis-rdtLite.yml ../rdtLite/.travis.yml
  rsync -rtv --del --perms man_rdtLite/ ../rdtLite/man
  rsync -rt --perms tests/test-all-rdtLite.R ../rdtLite/tests/test-all.R
  
  # Copy directories containing a mix of files 
  rsync -rtv --del --exclude "*_rdt.R" --perms R ../rdtLite/
  rsync -rtv --del --exclude "*_rdt*" --perms tests/testthat ../rdtLite/tests
}

# Copy just the files that we want in the rdt repository.  Some
# files need to be renamed when they are copied to remove _rdt
# from the name. 
function copy_rdt_files {
  echo "*** Copying files to rdt" 

  # Copy the shared files
  rsync -tv --perms inst/CITATION ../rdt/inst/
  rsync -tv --perms LICENSE ../rdt/
  rsync -tv --perms .Rbuildignore ../rdt/
  rsync -tv --perms tests.xml ../rdt/
  rsync -rtv --del --exclude */rdtLite/ --perms scriptTests ../rdt/
  
  # Copy the rdt versions of files
  rsync -tv --perms DESCRIPTION_rdt ../rdt/DESCRIPTION
  rsync -tv --perms NAMESPACE_rdt ../rdt/NAMESPACE
  rsync -tv --perms README_rdt.md ../rdt/README.md
  rsync -tv --perms NEWS_rdt.md ../rdt/NEWS.md
  rsync -tv --perms .travis-rdt.yml ../rdt/.travis.yml
  rsync -rtv --del --perms man_rdt/ ../rdt/man
  rsync -rt --perms tests/test-all-rdt.R ../rdt/tests/test-all.R
  
  # Copy files from directories that have a mix of files
  rsync -rtv --del --exclude "*_rdtLite.R" --perms R ../rdt/
  rsync -rtv --del --exclude "*_rdtLite*" --perms tests/testthat ../rdt/tests/
}

# Save the current commit number in the .commit file.
# If the .commit file is the only thing that changed, 
# we are done.  If not, we will commit the changes and
# push them.
function commit_repo {
  # Get the header of the latest commit
  if [ "$(git show | grep "Merge:" | wc | awk '{print $1;}')" = "1" ]
	then
  	  COMMIT_MSG=$(git show | head -n 6 | tail -n 1)
  else
  	 COMMIT_MSG=$(git show | head -n 5 | tail -n 1)
  fi

  # Update the .commit file
  cd ../$TO_REPO
  echo $COMMIT > .commit
  git add -A
  echo "git status for $TO_REPO"
  git status
  echo "end git status for $TO_REPO"

  NUM_MOD=$(git status | grep "modified:" | wc | awk '{print $1;}')
  NUM_DEL=$(git status | grep "deleted:" | wc | awk '{print $1;}')
  NUM_NEW=$(git status | grep "new file:" | wc | awk '{print $1;}')
  echo "Num modified = $NUM_MOD"
  echo "Num deleted = $NUM_DEL"
  echo "Num new = $NUM_NEW"

  if [ "$NUM_MOD" != "0" -o "$NUM_DEL" != "0" -o "$NUM_NEW" != "0" ]
    then
      echo "*** Committing and pushing the changes."
      git commit -m "$COMMIT_MSG"
      git push
  else 
  	  echo "*** Nothing to push."
  fi
  
  # Switch back to original directory
  cd ../RDataTracker
}

function cleanup {
	cd ..
	rm -rf RDataTracker rdtLite rdt
}

# Should first clone the three repositories
echo "*** Cloning the repositories"
git clone git@github.com:End-to-end-provenance/RDataTracker.git
git clone git@github.com:End-to-end-provenance/rdtLite.git
git clone git@github.com:End-to-end-provenance/rdt.git

cd RDataTracker

echo ""
if is_current "rdtLite" "master"
  then
    echo "***rdtLite master is current"
else 
    echo "***Updating rdtLite master"
    copy_rdtLite_files
    commit_repo "rdtLite"
fi

echo ""
if is_current "rdtLite" "development"
  then
    echo "rdtLite development is current"
else 
    echo "Updating rdtLite development"
    copy_rdtLite_files
    commit_repo "rdtLite"
fi

echo ""
if is_current "rdt" "master"
  then
    echo "rdt master is current"
else 
    echo "Updating rdt master"
    copy_rdt_files
    commit_repo "rdt"
fi

echo ""
if is_current "rdt" "development"
  then
    echo "rdt development is current"
else 
    echo "Updating rdt development"
    copy_rdt_files
    commit_repo "rdt"
fi

# Delete test repos
cleanup

echo "*** Done!"
