#!/bin/bash								# declare our shell environment

# GOAL: Use the OCRmyPDF tool from PyPI (https://pypi.python.org/pypi/ocrmypdf)
# to extract clean(ish) text from a specified directory. Make it parallelizable. 


## Declare some basics: source and destination. NB: these should be moved to the SLURM batch file when ready for deployment on the supercomputer.
DATASET=$1
# PDF=~/"Box\ Sync/research/dissertations/firstpages/""$DATASET""_only"
# TXT=~/"Box\ Sync/research/dissertations/firstpages/""$DATASET""_only_OCR"
PDF="/Volumes/Seagate_Backup_Plus_Drive/full-text_dissertations/all_rescan_20_only"
TXT="/Volumes/Seagate_Backup_Plus_Drive/full-text_dissertations/all_rescan_20_only_OCR"

CURRENT_DIR=$PWD

# Make sure we have a place to output to.
if ! [ -d "$TXT" ] ; then
	echo "Creating target directory $TXT..."
	mkdir "$TXT"
	if [ $? = 0 ] ; then echo "Success. " ; else echo "Failed."; fi           
fi


function extract ()
{
	# Start the loop.
	while read line; do
       
        # progress report
        printf "Rescanning $line to add text layer... "             
        
        # convert the file.
        ocrmypdf "$PDF/$line" "$TXT/$line" -r -d -c -i -v # -k
        
        # progress report
        if [ $? = 0 ] ; then echo "File made. " ; fi           
            
    # Close the loop.
    done

    # Close the function.
}

echo "pdf directory: $PDF"
cd "$PDF"
\ls *.pdf | extract
cd "$CURRENT_DIR"
