#!/bin/bash											# declare our shell environment

## Strategy: read in a textfile indexing various data subsets as output by R. 
## While items remain in the list, copy that subset to a new directory 
## to make analysis easier in AntConc and Mallet.

## Usage:
## subset_copy %dataset_name% %filetype%
## where %dataset_name% is something like "consorts" "noexcludes" etc
## and %filetype% is something like "txt" or "pdf"


# Where are the index files (from R)?
INDEX_DIR=~/'Dropbox/coursework, etc/dissertation/data, code, and figures/Dissertation Research/Shell scripts and commands'

# Function to convert Pub.numbers from the file into proper filenames, then copy the file
function subset_copy
{
	DATASET=$1
	
	# Where are the files to move? 
	# use clean text output by ben_clean_and_consolidate.sh
	SRC=~/'Documents/fulltext_dissertations/clean'
	

	# Make sure we have a place to copy the files to.
	DST="$SRC/../clean_""$DATASET""_only"
	
# 	ls "$DST"

	if ! [ -d "$DST" ] ; then
		printf "Creating destination folder $DST ..."
		mkdir "$DST"
		if [ $? == 0 ] ; then echo "Done." ; fi
	fi

	# Get the list of file numbers (generated by 'dataprep 2 - load data.R'
	# Convert file numbers to proper filenames, and copy to destination folder.
	j=0; k=0									# reset counters
	while read i; do							# start the loop
		FILE="cleaned_$i.txt"
		
		if [ -e "$SRC/$FILE" ] ; then
			printf "Creating symlink from $FILE in folder for ""$DATASET"" files only... "
			ln -s "$SRC/$FILE" "$DST/$FILE"
			if [ $? == 0 ] ; then 					# did it work? if so,
				echo "Done." 						# report back, and
				j=$((j+1))							# increment the counter.
			fi
		else 
			printf "$FILE not found."		
		fi
		k=$((k+1))
	done < "$INDEX_DIR/file list ""$DATASET"".txt"		# draw `i` from the file
												# (each line is one input)
	
	echo "Copied $j of $k files."				# final status report.
}

# subset_copy consorts
# subset_copy nonconsorts
# subset_copy noexcludes
subset_copy realconsorts

