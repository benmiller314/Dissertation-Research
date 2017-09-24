#!/bin/bash
#SBATCH -p RM-shared
#SBATCH -N 1
#SBATCH -t 10:00:00
#SBATCH --ntasks-per-node 14
#echo commands to stdout
set -x


# copy input data from storage on pylon2 to working directory on pylon1
cp -rv /pylon2/hm4s81p/bmiller3/fulltext_dissertations/pdf /pylon1/hm4s81p/bmiller3/pdf

# go to that home directory on pylon1
cd /pylon1/hm4s81p/bmiller3

# Make sure we have a place to output to.
if ! [ -d txt ] ; then
	mkdir txt
fi

# first task: extract text from pdf
function extract()
{
  # strip filename extension
  PUB=`printf $1 | awk 'BEGIN { FS="." } { print $1; }'`
  # echo $PUB
        
  # convert the file.
  pdftotext -enc UTF-8 $1                                 
  
  # progress report
  if [ $? = 0 ] ; then printf "File made ($PUB.txt) "; fi           
  
  # move to txt folder.
  mv "$PUB.txt" "../txt/$PUB.txt"
  
  # progress report
  if [ $? = 0 ] ; then echo "and moved." ; fi             
    
# Close the function.
}

# run the extract function in parallel on all pdf files
cd pdf
\ls *.pdf | xargs -n1 -P0 extract

#copy output file to persistent space
if ! [ -d /pylon2/hm4s81p/bmiller3/fulltext_dissertations/txt ] ; then
	mkdir /pylon2/hm4s81p/bmiller3/fulltext_dissertations/txt
fi

cd ../txt
cp -vr *.txt /pylon2/hm4s81p/bmiller3/fulltext_dissertations/txt