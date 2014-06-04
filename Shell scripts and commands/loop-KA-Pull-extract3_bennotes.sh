## Strategy: read in a list of files in a directory. 
## While items remain in the list, run a series of commands on them, 
## saving the results into new files to avoid accidental overwriting.


## Here be the loop function, containing the other commands. 
## Note that the variable `line1` will be read in from ls (the directory listing);
## this causes the loop to execute on each file in the directory.
function largeloop ()
{       while read line1; do

## Step 0. Copy the file to a new directory.
#~/Desktop/KT-NewPull-AllDescs-cull/$line1 > ~/Desktop/int-outputs/$line1

## (old versions)
#sed -n 949'p' ~/Desktop/KT-NewPull-AllDescs-ZAP/$line1 > ~/Desktop/cull-outputs/$line1.txt

#974
#perl -pi -e 's/\rn?/\n/g'

## Step 1a. Convert text encoding from ISO 8859-1 (Latin-1) to UTF-8 (unicode standard)
## 1b. using sed, do... something??... with lines 949 and 1000; write to standard output. 
##   maybe write just lines 949 and 1000 to standard output?
## 1c. using perl, replace (s/) all instances (/g) of carriage return (\r, octal \015) 
##   with line feed (\n, octal \012).  *** NB: I have NO idea what the flags do! ***
## 1d. using tr, replace all instances of line feed with space 
## 1e. save the output to a new directory.
iconv -f ISO_8859-1 -t UTF-8 /Users/micki/Documents/Topic\ Modeling/KA-Pulls/KA_AllPulls/$line1 | sed -n 949,1000'p' |perl -i -pe 's/\015/\012/g' | tr "\n" " " > ~/Desktop/KA_Pull_Outputs/$line1.txt 

## Step 2. Combine files into a big cumulative one. Here's how:
## Step 2a. Copy "cumulative.txt" (output file) to "cumulative2.txt" (scratch space)
#cp ~/Desktop/KA_cumulative.txt ~/Desktop/KA_cumulative2.txt

## Step 2b. Outside the loop, create a file that contains just a linefeed.
## Step 2c. Serially concatenate the scratch-space file, the linefeed, and the new file 
## (from which we've removed line breaks above); save to the output file.
#cat ~/Desktop/KA_cumulative2.txt ~/Desktop/linefeed.txt ~/Desktop/outputs/$line1.txt > ~/Desktop/KA_cumulative.txt
        done
}

## Run the loop.
ls  /Users/micki/Documents/Topic\ Modeling/KA-Pulls/KA_AllPulls/ | largeloop