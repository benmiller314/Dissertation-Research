## Get data toward a conservative estimate of OCR accuracy
## Based on a script by Micki Kaufman (https://twitter.com/MickiKaufman)
##
## Strategy: for each file "$line1" in a directory index (produced by ls), 
## (1) convert text encoding to UTF-8
## (2) find the wordcount
## (3) run a spellcheck, and save errors to a file
## (4) count the number of errors in that file
## (5) compile into a single file for further processing in R



function largeloop ()
{       while read line1; do

## work in a new directory to avoid over-writing source files
#~/Desktop/KT-NewPull-AllDescs-cull/$line1 > ~/Desktop/int-outputs/$line1

## (step 1) get clean UTF-8 text 
# NB: already taken care of by ben_clean_and_consolidate
# TO DO: combine these files with a call to the other one?
#sed -n 949'p' ~/Desktop/KT-NewPull-AllDescs-ZAP/$line1 > ~/Desktop/cull-outputs/$line1.txt
# iconv -f ISO_8859-1 -t UTF-8 ~/KT-sources1/$line1 > ~/KT-clean1/$line1

## (step 2) get wordcount, save to file
wc -w ~/Documents/"fulltext dissertations"/bashtest/$line1 | awk 'BEGIN { OFS="\t" } { print $0; }' - > ~/Documents/"fulltext dissertations"/bashtest/wordcount_$line1

## (step 3) find misspelled words, save to file. 
# NB: apparently this isn't included in OS X 10.7 (Lion). Boo. 
# To download the aspell command, you'll need something like
# Fink http://www.finkproject.org/download/srcdist.php and 
# Apple Developer Command Line Tools https://developer.apple.com/downloads/index.action
# and ftp://ftp.gnu.org/gnu/aspell/dict/en/aspell6-en-7.1-0.tar.bz2 for dictionaries.
# Once those are installed (no small feat), uncomment and run the commands in file
# "install aspell dictionary.txt" 

## Micki's version
# spell ~/KT-clean1/$line1 > ~/KT-counts1/$line1-spellwords
## Ben's version
aspell list < ~/Documents/"fulltext dissertations"/bashtest/cleaned_$line1 > ~/Documents/"fulltext dissertations"/bashtest/wordswrong_$line1

## (step 4) count the lines in that file; save the numbers, one new file per old file.
wc -l ~/Documents/"fulltext dissertations"/bashtest/wordswrong_$line1 > ~/Documents/"fulltext dissertations"/bashtest/errorcount_$line1

## (step 5) combine files into a big cumulative one. Here's how:

	## Step 5a. Outside the loop, create a file that contains just a linefeed
	## and a file to hold the cumulative output. See below.

	## Step 5b. Copy "cumulative.txt" (output file) to "cumulative2.txt" (scratch space)
	cp '/Users/benmiller314/Documents/fulltext dissertations/bashtest/spellcheck.txt' '/Users/benmiller314/Documents/fulltext dissertations/bashtest/cumulative2.txt'
		
	## Step 5c. Concatenate the scratch-space file, the linefeed, and the new file 
	## (from which we've removed line breaks above); save to the output file.
	cat '/Users/benmiller314/Documents/fulltext dissertations/bashtest/cumulative2.txt' '/Users/benmiller314/Documents/fulltext dissertations/bashtest/linefeed.txt'  "/Users/benmiller314/Documents/fulltext dissertations/bashtest/errorcount_$line1"   "/Users/benmiller314/Documents/fulltext dissertations/bashtest/wordcount_$line1" > '/Users/benmiller314/Documents/fulltext dissertations/bashtest/spellcheck.txt'

    done
}

## (this is step 5a)
if ! [ -e '/Users/benmiller314/Documents/fulltext dissertations/bashtest/linefeed.txt' ] ; then
	echo ' ' > '/Users/benmiller314/Documents/fulltext dissertations/bashtest/linefeed.txt'
fi

if ! [ -e !'/Users/benmiller314/Documents/fulltext dissertations/bashtest/spellcheck.txt' ] ; then
	echo ' ' > '/Users/benmiller314/Documents/fulltext dissertations/bashtest/spellcheck.txt'
fi	

## (step 6) invoke the loop
CURRENT_DIR=$PWD
cd ~/Documents/fulltext\ dissertations/15txt
ls *.txt | largeloop
cd "$CURRENT_DIR"

cat '/Users/benmiller314/Documents/fulltext dissertations/bashtest/spellcheck.txt' '/Users/benmiller314/Documents/fulltext dissertations/bashtest/linefeed.txt' > '/Users/benmiller314/Documents/fulltext dissertations/bashtest/spellcheck2.txt'