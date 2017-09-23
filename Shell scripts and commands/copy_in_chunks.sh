#!/bin/bash				# declare our environment
# When your copy command keeps hanging, try dividing it up into chunks.
# Solution via http://unix.stackexchange.com/questions/122499/bash-split-a-list-of-files

function chunk_copy
{

	# where are the files now?
	SRC=$1
	# first argument; originally, /Volumes/FullTextPdfs001 

	# where do you want them to be?
	DST=$2
	# second argument; originally, /Volumes/Seagate\ Backup\ Plus\ Drive/full-text\ dissertations/disc\ 1 

	if ! [ -d "$DST" ] ; then
		printf "Creating destination folder $DST ..."
		mkdir "$DST"
		if [ $? == 0 ] ; then echo "Done." ; fi
	fi

	# save current directory so we can come back afterward
	CURRENT_DIR="$PWD"

	# go to source directory
	cd "$SRC"

	# list files, divide them up into chunks of 25, copy them to destination.
	# (NB: tail only needed b/c I already grabbed some by hand; remove it.)
# 	\ls *.pdf | tail -n +166 | xargs -n 25 | xargs -0 -J {} cp -iv {} "$DST"
	\ls *.pdf | xargs -n 25 -0 -P 12 -J % cp -iv % "$DST" 
	
	# go back where we were.
	cd "$CURRENT_DIR"
}


# Set up loop for multiple directories
for i in {2..8}
do
	echo \ls *.pdf | xargs -P10 -n 1 -J % mv -iv % "../$DST" 
done
