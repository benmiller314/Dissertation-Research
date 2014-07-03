#!/bin/bash											# declare our shell environment

## Strategy: read in a list of files in a directory. 
## While items remain in the list, run a series of commands on them, 
## saving the results into new files to avoid accidental overwriting.
## Based on scripts by Micki Kaufman (https://twitter.com/MickiKaufman)

## Declare some basics: source and destination.
PDF='/Users/benmiller314/Documents/fulltext dissertations/morepdfs'
SRC='/Users/benmiller314/Documents/fulltext dissertations/morepdfs/as text files'
DST='/Users/benmiller314/Documents/fulltext dissertations/morepdfs/clean'

## for testing purposes
# line1="3298352.PDF"


## Each function runs a loop, containing commands to apply; 
## the variable `line1` will be read in from ls (the directory listing),
## which causes the loop to execute on each file in the directory.
## The functions (extract, clean, combine, spellcount) are called at the bottom;
## comment out the ones you don't want to run.

## Zeroth function: extract text from pdf. Run in the $PDF folder.
#  NB: pdftotext is available for free from http://www.bluem.net/en/mac/packages/
function extract()
{
	# Make sure we have a place to output to.
	if ! [ -d "$SRC" ] ; then
		mkdir "$SRC"
	fi

	# Start the loop.
	while read line1; do
		PUB=`printf $line1 | awk 'BEGIN { FS="." } { print $1; }'`
		printf "Converting $line1 to $PUB.txt ... "				# progress report
		pdftotext "$PDF/$line1"									# convert the file.
		if [ $? = 0 ] ; then printf "File made " ; fi			# progress report
		mv "$PDF/$PUB.txt" "$SRC/$PUB.txt"						# move to txt folder.
		if [ $? = 0 ] ; then echo "and moved." ; fi				# progress report
	
	# Close the loop.
	done

	# Close the function.
}

## First function: Get text that R can read. Run in the $SRC folder.
function clean ()
{   
	# Make sure we have a place to output to.
	if ! [ -d "$DST" ] ; then
		mkdir "$DST"
	fi

	# Start the loop.
	while read line1; do

	## Step 1. Copy the file to a new directory, making changes as it goes
	echo "Cleaning from SRC $line1 to DST $DST/$line1"			# progress report
	
	# 1a. Convert text encoding from ISO 8859-1 (Latin-1) to UTF-8 (unicode standard)
	# 1b. Using tr, delete all characters except for line breaks and Western characters
	# 1c. Using sed, delete the first page added by UMI (which starts in line 1, and 
	#	  usually ends with the zip code)
	# 	1d. Using tr again, collapse multiple spaces to a newline
	#	  NB: changed my mind. Here's the code in case I want it again:
	#	  | tr -s ' ' '\n'
	# 	1e. Using tr yet again, replace newlines with spaces (get all text on one line)
	#	  NB: changed my mind. Here's the code in case I want it again:
	#	  | tr '\n' " "
	# 1f. Save to a file in the destination directory.

	iconv -f ISO_8859-1 -t UTF-8 "$SRC/$line1" | \
	tr -cd '\11\12\40-\176' | \
	sed "1,/-1346/d" > "$DST/cleaned_$line1"
	
		# catch the case where we've stripped too much (i.e. the file has 0 bytes)
		# and do it again without sed
		if ! [ -s "$DST/cleaned_$line1" ] ; then
			iconv -f ISO_8859-1 -t UTF-8 "$SRC/$line1" | \
			tr -cd '\11\12\40-\176' > "$DST/cleaned_$line1"
		fi
	
	# Close the loop.
	done
	
	# Close the function.
}

## Second function: Combine files into a big cumulative one. Here's how:
function combine ()
{	## Step 1. Outside the loop, create an empty file to hold the cumulative output. 
	if ! [ -e "$DST/cumulative.csv" ] ; then
		echo '' > "$DST/cumulative.csv"
	else 
		echo "ERROR: $DST/cumulative.csv already exists; aborting combine step."
		exit 1
	fi
	
	echo "Making cumulative file. Adding:"						# progress report
	
	
	
	## Step 2. Concatenate the cleaned file (from which we've removed line breaks 
	## above) and append it to the cumulative output file.
	while read line1; do
		echo "-- $line1"										# progress report
	
		# Step 2a. Strip '.txt' off the filename; this will help us join tables later.		
		PUB=`printf $line1 | awk 'BEGIN { FS="." } { print $1; }'`
	
		# Step 2b. String together the Pub.number, a comma, and the file contents;
		# if I'm lucky, this gives me a table that R can read in natively.
		CONTENTS=`cat "$DST/$line1"`						# unnecessary slowness?
		echo "$PUB, $CONTENTS" >> "$DST/cumulative.csv"
		
    done
    
    echo "All text saved to $DST/cumulative.csv."				# progress report
    echo ''
}


## Third function: Get data toward a conservative estimate of OCR accuracy.
##
## Strategy: for each file "$line1" in a directory index (produced by ls), 
## (1) find the wordcount
## (2) run a spellcheck, and save errors to a file
## (3) count the number of errors in that file
## (4) compile into a single file for further processing in R

function spellcount ()
{       
## (step 0 or 4a) Outside the loop, create a placeholder output file
if ! [ -d "$DST/spellstats" ] ; then
	mkdir "$DST/spellstats"
fi

if ! [ -e "$DST/spellstats/spellstats.csv" ] ; then
	echo 'Pub.Number, WordCount, ErrorCount' > "$DST/spellstats/spellstats.csv"
else 
	echo "spellstats.csv already exists; aborting script to avoid duplication."
	echo "To append, use new DST folder and concatenate later."
	exit 1
fi

	echo "Counting spelling errors..."

while read line1; do

## (step 1) Get wordcount, save to a variable.
	WC=`wc -w "$DST/$line1" | awk '{ print $1; }' - `

## (step 2) Find misspelled words, save to file in case we want to analyze later.
# NB: apparently this isn't included in OS X 10.7 (Lion). Boo. 
# To download the aspell command, you'll need something like
# Fink http://www.finkproject.org/download/srcdist.php and 
# Apple Developer Command Line Tools https://developer.apple.com/downloads/index.action
# and ftp://ftp.gnu.org/gnu/aspell/dict/en/aspell6-en-7.1-0.tar.bz2 for dictionaries.
# Once those are installed (no small feat), uncomment and run the commands in file
# "install aspell dictionary.txt" 

	aspell list < "$DST/$line1" > "$DST/spellstats/wordswrong_$line1"

## (step 3) count the lines in the wordswrong file; save the numbers in a variable.
	ERRS=`wc -l "$DST/spellstats/wordswrong_$line1" | awk '{ print $1; }' - `

## (step 4) combine files into a cumulative table. Here's how:
	# Step 4a. Outside the loop, create a placeholder output file. (See above.)
	
	# Step 4b. Strip '.txt' and 'cleaned_' off the filename; this will help us join tables later.		
	PUB=`printf $line1 | awk 'BEGIN { FS="." } { print $1; }' | awk 'BEGIN { FS="_" } { print $2; }'`
	# Step 4c. String together the Pub.number, the wordcount, and the errorcount; 
	# append to the output file.
	echo "-- checking $line1"
	echo "$PUB, $WC, $ERRS" >> "$DST/spellstats/spellstats.csv"

## Close the loop
done
	
	echo "Spelling counts saved to $DST/spellstats/spellstats.csv."
	echo ''
	
## Close the function
}


# echo "Currently DST folder is $DST"
# echo "and the SRC folder is $SRC"

## Go to the files, and run all the functions.
CURRENT_DIR=$PWD
# cd "$PDF"											# Go to pdf directory
# ls *.PDF | extract									# Call 0th function
cd "$SRC"											# Go to source directory
ls *.txt | clean									# Call 1st function
cd "$DST" 											# Go to output directory
# ls cleaned* | combine								# Call 2nd function
ls cleaned* | spellcount							# Call 3rd function
cd "$CURRENT_DIR"									# Go back where we were

## Tell us what we've got!
FILECOUNT=`wc -l "$DST/spellstats/spellstats.csv" | awk '{ print $1; }' - `
let FILECOUNT=$FILECOUNT-1							# account for titles in 1st line
echo "Finished. $FILECOUNT files processed."