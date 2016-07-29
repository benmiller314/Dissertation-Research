#!/bin/bash                             # declare our shell environment

## GOAL: xml-ify some dissertations

## STRATEGY: read in files from a directory; add xml open and close tags to surround each file; save into new directory.

# where are the text files to xmlify?
SRC=/Users/benmiller314/Documents/fulltext_dissertations/test_subset

# where should output files go?
DST=/Users/benmiller314/Documents/fulltext_dissertations/test_subset_output

# where can I find files that have the text of the open and close tags?
XMLROOT=/Users/benmiller314/Dropbox/coursework\,\ etc/dissertation/data\,\ code\,\ and\ figures/Dissertation\ Research/Shell\ scripts\ and\ commands

# CURRENT_DIR=$PWD

function xmlify {
	# start the loop
	while read line; do
		# run commands
	
		# store the current file name, so we can re-use it in the DST directory
		# WARNING: NO SPACES IN FILENAMES (or you may overwrite output files)
		FILENAME=`printf $line | awk 'BEGIN { FS="." } { print $1; }'`
		DST_FILE="$DST"/"$FILENAME"".xml"
		
		
		# add the open and close tags
		cat "$XMLROOT"/openxml.txt "$SRC"/$line "$XMLROOT"/closexml.txt > $DST_FILE

	# close the loop
	done  	# TO DO: as input for the while-loop, 
			# use the contents of the SRC directory
		
}

\ls "$SRC" | xmlify
