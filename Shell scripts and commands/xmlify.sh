#!/bin/bash                             # declare our shell environment

## GOAL: xml-ify some dissertations

## STRATEGY: read in files from a directory; add xml open and close tags to surround each file; save into new directory.

# where are the text files to xmlify?
SRC=/Users/benmiller314/Documents/fulltext_dissertations/text_realconsorts_only

# where should output files go?
DST=/Users/benmiller314/Documents/fulltext_dissertations/text_realconsorts_UTF8

	# Make sure we have a place to output to.
	if ! [ -d "$DST" ] ; then
		mkdir "$DST"
	fi

# where can I find files that have the text of the open and close tags?
XMLROOT=/Users/benmiller314/Dropbox/coursework\,\ etc/dissertation/data\,\ code\,\ and\ figures/Dissertation\ Research/Shell\ scripts\ and\ commands

# CURRENT_DIR=$PWD

function convert {
	# start the loop
	while read line; do
		
	# progress report
	printf "Converting $line to UTF-8 ... "   
	
	# convert the file  	
	iconv -f ISO_8859-1 -t UTF-8 "$SRC/$line" > "$DST/$line"
	
	# progress report
	if [ $? = 0 ] ; then printf "File made. \n" ; fi           	
	
	# close the loop
	done
}

function xmlify {
	# start the loop
	while read line; do
	
		# store the current file name, so we can re-use it in the DST directory
		# WARNING: NO SPACES IN FILENAMES (or you may overwrite output files)
		FILENAME=`printf $line | awk 'BEGIN { FS="." } { print $1; }'`
		echo $FILENAME
		
		DST_FILE="$DST"/"$FILENAME"".xml"
		
		
		# add the open and close tags
		cat "$XMLROOT"/openxml.txt "$SRC"/$line "$XMLROOT"/closexml.txt > $DST_FILE

	# close the loop
	done  			
}

# When the file is loaded, run the function on all files in the SRC directory
# (NB: the backslash at the start of the line tells the shell to ignore the aliases I've put on ls to tweak its output.)
# \ls "$SRC" *.txt | convert
\ls "$DST" *.txt | xmlify

# TO DO: instead of hard-coding SRC and DST, (optionally) assign them in arguments
