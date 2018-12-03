#!/bin/bash                             # declare our shell environment

##### 
# clean_subset.sh
# 
# GOAL: given a big folder of txt files from which you want to build a topic model, 
# take out just the ones in a pre-specified list (e.g. just consortium dissertations) 
# and copy them to a new folder, cleaning the text of boilerplate as you go.
#
# STRATEGY: Read in a list of files. 
# While items remain in the list, run a series of commands on them, 
# saving the results into new files to avoid accidental overwriting.
# Based on scripts by Micki Kaufman (https://twitter.com/MickiKaufman).
#
# USAGE (at command line): sh ./clean_subset.sh %dataset_name%
#
#####

# The first parameter after calling this file is the dataset_name.
DATASET=$1
RSOURCELOC=~/Box\ Sync/research/dissertations/data,\ code,\ and\ figures/Dissertation-Research
FILELIST="$RSOURCELOC""/subsets/""$DATASET""_doc_ids.txt"

# Declare some basics: source and destination. 
SRC="/Volumes/Seagate_Backup_Plus_Drive/full-text_dissertations/all_txt"
DST="/Volumes/Seagate_Backup_Plus_Drive/full-text_dissertations/clean_""$DATASET""_only"


## for testing purposes
# line1="3298352.PDF"

# Make sure we have a place to output to.
	if ! [ -d "$DST" ] ; then
		mkdir -p "$DST"
	fi
	
## Get text that R can read. Run in the $SRC folder.
function clean ()
{   
	# Start the loop.
	while read line1; do
	
	FILENAME="$SRC/$line1.txt"

	## Step 1. Copy the file to a new directory, making changes as it goes
	echo "Cleaning from SRC $FILENAME to DST $DST/cleaned_$line1.txt"		# progress report
	
	# 1a. Convert text encoding from ISO 8859-1 (Latin-1) to UTF-8 (unicode standard)
	# 1b. Using tr, delete all non-line-break non-Western characters 
	#     OR IS THIS SOLVED BY CONVERTING TO UTF-8?
	# 1c. Using sed, delete the first page added by UMI (which starts in line 1, and 
	#	  usually ends with the zip code) TO DO: BE SMARTER ABOUT THIS REGEX. 
	#	  Try removing everything from INFORMATION to the line in 1d?
	# 1d. Using sed, remove all instances of the line below:
	#	  "Reproduced with permission of the copyright owner. Further reproduction
	#	   prohibited without permission."
	# 13. Save to a file in the destination directory.

	iconv -f ISO_8859-1 -t UTF-8 "$FILENAME" | \
# 	tr -cd '\11\12\40-\176' | \
	sed "1,/48106-1346/d" | \
    sed "s/INFORMATION TO USERS.*Further reproduction prohibited without permission.//" | \
    sed "s/Reproduced with permission of the copyright owner. Further reproduction prohibited without permission.//g"
	> "$DST/cleaned_$line1.txt"
	
	# catch the case where we've stripped too much (ie. the file has 0 bytes)
	# and do it again without sed
		if ! [ -s "$DST/cleaned_$line1" ] ; then
			iconv -f ISO_8859-1 -t UTF-8 "$FILENAME" | \
			tr -cd '\11\12\40-\176' > "$DST/cleaned_$line1.txt"
		fi
	
	# Close the loop.
	done
	
	# Close the function.
}

# echo "Currently DST folder is $DST"
# echo "and the SRC folder is $SRC"


## Go to the files, and run all the functions.
## IMPORTANT: Comment out those you don't need right now.
CURRENT_DIR=$PWD
cd "$SRC"                                         # Go to source directory
clean < "$FILELIST"                                  # Call 1st function
cd "$CURRENT_DIR"                                   # Go back where we were
