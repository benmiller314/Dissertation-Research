#!/bin/bash                             # declare our shell environment

#####
# ben_clean_and_consolidate.sh
#
# GOAL: given pdf files from which you want to build a topic model, extract
# the full text and remove boilerplate that could skew your results.
#
# STRATEGY: Read in a list of files in a directory.
# While items remain in the list, run a series of commands on them,
# saving the results into new files to avoid accidental overwriting.
# Based on scripts by Micki Kaufman (https://twitter.com/MickiKaufman).
#
# Four functions are defined and then executed at the bottom: extract, clean,
# combine, spellcount. Each function runs a loop, containing commands to
# apply; the variable `line1` will be read in from ls (the directory
# listing), which causes the loop to execute on each file in the directory.
# The functions are called at the bottom; comment out the ones you don't want
# to run.
#####


# Declare some basics: source and destination.
# NB: these will likely change often!
DATASET=$1
# PDF="/Users/benmiller314/Documents/fulltext_dissertations/morepdfs"
# SRC="/Users/benmiller314/Documents/fulltext_dissertations/morepdfs/as text files"
DST="/Users/benmiller314/Documents/fulltext_dissertations/clean_""$DATASET""_only"
# PDF="/Users/benmiller314/Documents/topic_inference_test"
# SRC="/Users/benmiller314/Documents/topic_inference_test/txt"
# DST="/Users/benmiller314/Documents/topic_inference_test/clean"
# PDF="/Volumes/Seagate_Backup_Plus_Drive/full-text_dissertations/all_rescan_only"
# SRC="/Volumes/Seagate_Backup_Plus_Drive/full-text_dissertations/all_noexcludes_only"
# DST="/Volumes/Seagate_Backup_Plus_Drive/full-text_dissertations/clean_noexcludes2001_2015_only"
PDF="/Volumes/Seagate_Backup_Plus_Drive/full-text_dissertations/all_noexcludes_only"
SRC="/Volumes/Seagate_Backup_Plus_Drive/full-text_dissertations/all_txt"
# DST=$SRC

# Store cumulative data in its own directory
CUMUL=~/"Documents/fulltext_dissertations/cumulative"


## for testing purposes
# line1="3298352.PDF"


## Zeroth function: extract text from pdf. Run in the $PDF folder.
#  NB: pdftotext is standard on linux distributions; a mac port
#  is available for free from http://www.bluem.net/en/mac/packages/
function extract()
{
    # Make sure we have a place to output to.
    if ! [ -d "$SRC" ] ; then
        mkdir -p "$SRC"
    fi

    # Start the loop.
    while read line1; do
		# strip filename extension: awk creates a table divided at
		# the field separator (FS); we want the first "cell,"
		# i.e. whatever comes before the period.
        PUB=`printf $line1 | awk 'BEGIN { FS="." } { print $1; }'`

        # progress report
        printf "Converting $line1 to $PUB.txt ... "

        # convert the file.
        pdftotext "$PDF/$line1"

        # progress report
        if [ $? = 0 ] ; then printf "File made " ; fi

        # move to txt folder.
        mv "$PDF/$PUB.txt" "$SRC/$PUB.txt"

        # progress report
        if [ $? = 0 ] ; then echo "and moved." ; fi

    # Close the loop.
    done

    # Close the function.
}

## First function: Get text that R can read. Run in the $SRC folder.
function clean ()
{
	# Make sure we have a place to output to.
	if ! [ -d "$DST" ] ; then
		mkdir -p "$DST"
	fi

	# Start the loop.
	while read line1; do

	## Step 1. Copy the file to a new directory, making changes as it goes
	echo "Cleaning from SRC $line1 to DST $DST/$line1"		# progress report

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

	iconv -f ISO_8859-1 -t UTF-8 "$SRC/$line1" | \
# 	tr -cd '\11\12\40-\176' | \
	sed "1,/48106-1346/d" | \
    sed "1,/Further reproduction prohibited without permission./d" | \
    sed "/Reproduced with permission of the copyright owner. Further reproduction prohibited without permission./d"
	> "$DST/cleaned_$line1"

	# catch the case where we've stripped too much (ie. the file has 0 bytes)
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
{
    # Step 1. Outside the loop, create an empty file to hold the cumulative
    # output.
    if ! [ -e "$CUMUL/""$DATASET""_cumulative.txt" ] ; then
        printf '' > "$CUMUL/""$DATASET""_cumulative.txt"
    else
        echo "ERROR: $CUMUL/""$DATASET""_cumulative.txt already exists; aborting combine step."
        exit 1
    fi

    echo "Making cumulative file:"              # progress report

    # Step 2. Concatenate the cleaned file (after removing line breaks) and
    # append it to the cumulative output file in MALLET-ready format, as per
    # http://mallet.cs.umass.edu/import.php: "The first token of each line
    # (whitespace delimited, with optional comma) becomes the instance name,
    # the second token becomes the label, and all additional text on the
    # line is interpreted as a sequence of word tokens."

    while read line1; do
        printf "adding $line1..."               # progress report

        # 2a. Using awk, strip '.txt' and 'cleaned_' off the filename to get
        # the Pub.number of the diss. We'll use these as instance names in
        # MALLET.
        PUB=`printf $line1 | awk 'BEGIN { FS="." } { print $1; }' | awk 'BEGIN { FS="_" } { print $2; }'`
        # 2b. We don't have labels right now, but eventually we could use
        # methods exported from R.
        LAB="placeholder"

        # 2c. Using tr, remove all commas so we can get a clean csv,
        # then replace newlines with spaces (get all text on one line).
        CONTENTS=`cat "$DST/$line1" | tr -d "," | tr -s '\n' " "`

        # 2d. String together the instance names, labels, and the file
        # contents;
        echo "$PUB $LAB $CONTENTS" >> "$CUMUL/""$DATASET""_cumulative.txt"

        if [ $? = 0 ] ; then                    # progress report
            echo "done."
        fi
    done

    if [ $? = 0 ] ; then
        # progress report
        echo "All text saved to $CUMUL/""$DATASET""_cumulative.txt"
        echo ''
    fi
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
# (step 0 or 4a) Outside the loop, create a placeholder output file
if ! [ -d "$DST/_spellstats" ] ; then
    mkdir "$DST/_spellstats"
fi

if ! [ -e "$DST/_spellstats/spellstats.csv" ] ; then
    echo 'Pub.Number, WordCount, ErrorCount' > "$DST/_spellstats/spellstats.csv"
else
    echo "spellstats.csv already exists; aborting script to avoid duplication."
    echo "To append, use new DST folder and concatenate later."
    exit 1
fi

    echo "Counting spelling errors..."

while read line1; do

## (step 1) Get wordcount, save to a variable.
    WC=`wc -w "$DST/$line1" | awk '{ print $1; }' - `

## (step 2) Find misspelled words, save to file in case we want to analyze
## later.
# To install the aspell utility on a Mac, use a package manager like Homebrew.
# If you don't have Homewbrew, go to http://brew.sh and use the simple installation script.
# Then simply type this at a Terminal prompt (without quotes): "brew install aspell"

    aspell list < "$DST/$line1" > "$DST/_spellstats/wordswrong_$line1"

## (step 3) count the lines in the wordswrong file; save the numbers in a
## variable.
    ERRS=`wc -l "$DST/_spellstats/wordswrong_$line1" | awk '{ print $1; }' - `

## (step 4) combine files into a cumulative table. Here's how:
    # Step 4a. Outside the loop, create a placeholder output file. (See
    # above.)

    # Step 4b. Strip '.txt' and 'cleaned_' off the filename; this will help
    # us join tables later.
    PUB=`printf $line1 | awk 'BEGIN { FS="." } { print $1; }'` #| awk 'BEGIN { FS="_" } { print $2; }'`
    # Step 4c. String together the Pub.number, the wordcount, and the
    # errorcount; append to the output file.
    echo "-- checking $line1"
    echo "$PUB, $WC, $ERRS" >> "$DST/_spellstats/spellstats.csv"

## Close the loop
done

    # Final report: Tell us what we've got!
    echo "Spelling counts saved to $DST/_spellstats/spellstats.csv."

    FILECOUNT=`wc -l "$DST/_spellstats/spellstats.csv" | awk '{ print $1; }' - `
    let FILECOUNT=$FILECOUNT-1          # account for headers in 1st line
    echo "$FILECOUNT files processed."
    echo ''
## Close the function
}


# echo "Currently DST folder is $DST"
# echo "and the SRC folder is $SRC"


## Go to the files, and run all the functions.
## IMPORTANT: Comment out those you don't need right now.
CURRENT_DIR=$PWD
cd "$PDF"                                         # Go to pdf directory
\ls *.pdf | extract                                # Call 0th function
cd "$SRC"                                         # Go to source directory
\ls *.txt | clean                                  # Call 1st function
cd "$DST"                                           # Go to output directory
\ls cleaned* | combine                               # Call 2nd function
\ls cleaned* | spellcount                          # Call 3rd function
\ls *.txt | spellcount
cd "$CURRENT_DIR"                                   # Go back where we were
