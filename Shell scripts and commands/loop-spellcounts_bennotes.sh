## Get data toward a conservative estimate of OCR accuracy
## Based on a script by Micki Kaufman (https://twitter.com/MickiKaufman)
##
## Strategy: for each file "$line1" in a directory index (produced by ls), 
## (1) convert text encoding to UTF-8
## (2) find the wordcount
## (3) run a spellcheck, and save errors to a file
## (4) count the number of errors in that file
## (5) compile into a single file for further processing in R



function spellcount ()
{       while read line1; do

## work in a new directory to avoid over-writing source files
#~/Desktop/KT-NewPull-AllDescs-cull/$line1 > ~/Desktop/int-outputs/$line1

## (step 1) get clean UTF-8 text 
# NB: already taken care of by ben_clean_and_consolidate
# TO DO: combine these files with a call to the other one?
#sed -n 949'p' ~/Desktop/KT-NewPull-AllDescs-ZAP/$line1 > ~/Desktop/cull-outputs/$line1.txt
# iconv -f ISO_8859-1 -t UTF-8 ~/KT-sources1/$line1 > ~/KT-clean1/$line1

## (step 2) get wordcount, save to a variable.
WC=`wc -w ~/Documents/"fulltext dissertations"/bashtest/$line1 | awk '{ print $1; }' - `
# > ~/Documents/"fulltext dissertations"/bashtest/wordcount_$line1

## (step 3) find misspelled words, save to file in case we want to analyze later.
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

## (step 4) count the lines in the wordswrong file; save the numbers in a variable.
ERRS=`wc -l ~/Documents/"fulltext dissertations"/bashtest/wordswrong_$line1 | awk '{ print $1; }' - `
# > ~/Documents/"fulltext dissertations"/bashtest/errorcount_$line1

## (step 5) combine files into a big cumulative one. Here's how:
	# Step 5a. Outside the loop, create a placeholder output file. (See below.)
	
	# Step 5b. Strip '.txt' off the filename; this will help us join tables later.		
	PUB=`printf $line1 | awk 'BEGIN { FS="." } { print $1; }'`

	# Step 5c. String together the Pub.number, the wordcount, and the errorcount; 
	# append to the output file.
	
	echo "$PUB, $WC, $ERRS" >> '/Users/benmiller314/Documents/fulltext dissertations/bashtest/spellcheck.csv'

    done
}

## (this is step 5a)
if ! [ -e !'/Users/benmiller314/Documents/fulltext dissertations/bashtest/spellcheck.csv' ] ; then
	echo 'Pub.Number, WordCount, ErrorCount' > '/Users/benmiller314/Documents/fulltext dissertations/bashtest/spellcheck.csv'
fi	

## (step 6) invoke the loop
CURRENT_DIR=$PWD
cd ~/Documents/fulltext\ dissertations/15txt
ls *.txt | spellcount
cd "$CURRENT_DIR"

# echo ' ' >> '/Users/benmiller314/Documents/fulltext dissertations/bashtest/spellcheck.csv'
