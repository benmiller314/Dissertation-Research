## Strategy: read in a list of files in a directory. 
## While items remain in the list, run a series of commands on them, 
## saving the results into new files to avoid accidental overwriting.


## Here be the loop function, containing the other commands. 
## Note that the variable `line1` will be read in from ls (the directory listing);
## this causes the loop to execute on each file in the directory.
function largeloop ()
{   while read line1; do

	## Step 0. Copy the file to a new directory.
	cp ~/Documents/fulltext\ dissertations/15txt/$line1 ~/Documents/fulltext\ dissertations/bashtest/$line1
	
	
	## Step 1a. Convert text encoding from ISO 8859-1 (Latin-1) to UTF-8 (unicode standard)
	## 1b. using sed, do... something??... with lines 949 and 1000; write to standard output. 
	##   maybe write just lines 949 and 1000 to standard output?
	## 1c. using perl, replace (s/) all instances (/g) of carriage return (\r, octal \015) 
	##   with line feed (\n, octal \012).  *** NB: I have NO idea what the flags do! ***
	## 1d. using tr, replace all instances of line feed with space 
	## 1e. save the output to a new directory.
	iconv -f ISO_8859-1 -t UTF-8 ~/Documents/"fulltext dissertations"/bashtest/$line1 | tr -cd '\11\12\40-\176' | tr -s ' ' '\n' | tr '\n' " " > ~/Documents/"fulltext dissertations"/bashtest/cleaned_$line1
	#| perl -i -pe 's/\015/\012/g' \
	
	## Step 2. Combine files into a big cumulative one. Here's how:

	## Step 2a. Outside the loop, create a file that contains just a linefeed
	## and a file to hold the cumulative output. See below.
		
	## Step 2b. Concatenate the new line and the linefeed
	## and append to the cumulative output.
	cat "/Users/benmiller314/Documents/fulltext dissertations/bashtest/cleaned_$line1" '/Users/benmiller314/Documents/fulltext dissertations/bashtest/linefeed.txt' >> '/Users/benmiller314/Documents/fulltext dissertations/bashtest/cumulative.txt'
	
    done
}


## (this is step 2a)
if ! [ -e '/Users/benmiller314/Documents/fulltext dissertations/bashtest/linefeed.txt' ] ; then
	echo ' ' > '/Users/benmiller314/Documents/fulltext dissertations/bashtest/linefeed.txt'
fi

if ! [ -e !'/Users/benmiller314/Documents/fulltext dissertations/bashtest/cumulative.txt' ] ; then
	echo ' ' > '/Users/benmiller314/Documents/fulltext dissertations/bashtest/cumulative.txt'
fi	
		

## Run the loop.
ls ~/Documents/fulltext\ dissertations/15txt | largeloop