#!/bin/bash

# This script keeps the provR and rdt repositories in sync with RDataTracker.
# RDataTracker contains code used by both provR and rdt.  It also does 
# extensive system-level testing using scripts.
#
# The provR repository contains only code used in provR, while the
# rdt repository contains only code used in RDataTracker.

# Determines if the repository is already current with respect
# to the RDataTracker repository.  The first parameter is the 
# repository to copy to, the second is the branch to copy.
# The provR and rdt repositories have a file called .commit.
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
      exit 1
  fi
  cd ../$TO_REPO
  git checkout $BRANCH
  STATUS=$(git status | head -n 1)
  if [ ! "$STATUS" = "On branch $BRANCH" ]
    then
      echo "Can't switch to $BRANCH of $TO_REPO"
      cd ../RDataTracker
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

# Copy just the files that we want in the provR repository.  Some
# files need to be renamed when they are copied to remove _prov
# from the name. 
function copy_provR_files {
  echo "*** Copying files to provR" 
  
  # Copy the files we want to provR
  rsync -tv --perms inst/CITATION ../provR_test/inst/CITATION
  rsync -tv --perms DESCRIPTION_prov ../provR_test/DESCRIPTION
  rsync -tv --perms NAMESPACE_prov ../provR_test/NAMESPACE
  rsync -tv --perms LICENSE ../provR_test/LICENSE
  rsync -tv --perms README_prov.md ../provR_test/README.md
  rsync -tv --perms .Rbuildignore ../provR_test/.Rbuildignore
  rsync -rtv --del --exclude "*_rdt.R" --exclude "DDGCheckpoint.R" --perms R ../provR_test/R
  rsync -rtv --del --exclude "*_rdt*" --perms man_prov ../provR_test/man
  rsync -rtv --del --exclude "*_rdt*" --perms tests ../provR_test/tests
}

# Copy just the files that we want in the rdt repository.  Some
# files need to be renamed when they are copied to remove _rdt
# from the name. 
function copy_rdt_files {
  echo "*** Copying files to rdt" 

  # Copy the files we want to rdt
  rsync -tv --perms inst/CITATION ../rdt_test/inst/CITATION
  rsync -tv --perms DESCRIPTION_rdt ../rdt_test/DESCRIPTION
  rsync -tv --perms NAMESPACE_rdt ../rdt_test/NAMESPACE
  rsync -tv --perms LICENSE ../rdt_test/LICENSE
  rsync -tv --perms README_rdt.md ../rdt_test/README.md
  rsync -tv --perms .Rbuildignore ../rdt_test/.Rbuildignore
  rsync -rtv --del --exclude "*_prov.R" --exclude "DDGCheckpoint.R" --perms R ../rdt_test/R
  rsync -rtv --del --exclude "*_prov*" --perms man_prov ../rdt_test/man
  rsync -rtv --del --exclude "*_prov*" --perms tests ../rdt_test/tests
}

# Save the current commit number in the .commit file.
# If the .commit file is the only thing that changed, 
# we are done.  If not, we will commit the changes and
# push them.
function commit_repo {
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

  if [ "$NUM_MOD" != "1" -o "$NUM_DEL" != "0" -o "$NUM_NEW" = "0" ]
    then
      echo "*** Committing and pushing the changes."
      git commit -m "Commit"
      git push
  else 
  	  echo "*** Nothing to push."
  fi
  
  # Switch back to original directory
  cd ../RDataTracker
}

# Should first clone the three repositories
echo "*** Cloning the repositories"
git clone https://github.com/End-to-end-provenance/RDataTracker.git
git clone https://github.com/End-to-end-provenance/provR_test.git
git clone https://github.com/End-to-end-provenance/rdt_test.git
cd RDataTracker

echo ""
if is_current "provR_test" "master"
  then
    echo "***provR master is current"
else 
    echo "***Updating provR master"
    copy_test1_files
    commit_repo "provR_test"
fi

echo ""
if is_current "provR_test" "development"
  then
    echo "provR development is current"
else 
    echo "Updating provR development"
    copy_test1_files
    commit_repo "provR_test"
fi

echo ""
if is_current "rdt_test" "master"
  then
    echo "rdt master is current"
else 
    echo "Updating rdt master"
    copy_test2_files
    commit_repo "rdt_test"
fi

echo ""
if is_current "rdt_test" "development"
  then
    echo "rdt development is current"
else 
    echo "Updating rdt development"
    copy_test2_files
    commit_repo "rdt_test"
fi

# Delete test repo
cd ..
rm -rf RDataTracker provR_test rdt_test

echo "*** Done!"
