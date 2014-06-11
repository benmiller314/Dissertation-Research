## Strategy: read in a list of files in a directory. 
## While items remain in the list, run a series of commands on them, 
## saving the results into new files to avoid accidental overwriting.

## Declare some basics: source and destination.
SRC='/Users/benmiller314/Documents/fulltext dissertations/15txt'
DST='/Users/benmiller314/Documents/fulltext dissertations/bashtest'


## Here be the loop function, containing the other commands. 
## Note that the variable `line1` will be read in from ls (the directory listing);
## this causes the loop to execute on each file in the directory.
function largeloop ()
{   while read line1; do
	echo $line1
	# echo "Copying from SRC $SRC/$line1 to DST $DST/$line1"

# 	## Step 0. Copy the file to a new directory. (Not sure this is necessary)
# 	echo "Copying from SRC $SRC/$line1 to DST $DST/$line1"
# 	cp "$SRC/$line1" "$DST/$line1"
	
	# 1a. Convert text encoding from ISO 8859-1 (Latin-1) to UTF-8 (unicode standard)
	# 1b. Using tr, delete all characters except for line breaks and Western characters
	# 1c. Using tr again, collapse multiple spaces to a newline
	# 1d. Using tr yet again, replace newlines with spaces (get all text on one line)
	# 1e. Save to a file in the destination directory.
	iconv -f ISO_8859-1 -t UTF-8 "$SRC/$line1" | tr -cd '\11\12\40-\176' | tr -s ' ' '\n' | tr '\n' " " > "$DST/cleaned_$line1"
	
	
	## Step 2. Combine files into a big cumulative one. Here's how:

	## Step 2a. Outside the loop, create a file that contains just a linefeed
	## and a file to hold the cumulative output. See below.

	## Step 2b. Copy "cumulative.txt" (output file) to "cumulative2.txt" (scratch space)
	cp $DST/cumulative.txt $DST/cumulative2.txt
		
	## Step 2c. Concatenate the scratch-space file, the linefeed, and the new file 
	## (from which we've removed line breaks above); save to the output file.
	cat "$DST/cumulative2.txt" "linefeed.txt" "$DST/cleaned_$line1" > "$DST/cumulative.txt"
	
    done
    
    cat "$DST/cumulative.txt" "linefeed.txt" > "$DST/cumulative2.txt"
}


## (this is step 2a)
if ! [ -e "linefeed.txt" ] ; then
	echo '' > linefeed.txt
fi

if ! [ -e "$DST/cumulative.txt" ] ; then
	echo '' > "$DST/cumulative.txt"
fi	


# echo "Currently DST folder is $DST"
# echo "and the SRC folder is $SRC"

## Run the loop.
ls "$SRC" | largeloop