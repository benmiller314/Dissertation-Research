#!/bin/bash

# Goal: aggregate all .txt files in the current directory 
# whose names start with "lesson" (TO DO: accept filename as regex)

# start clean
rm lessons_all.txt		
touch lessons_all.txt

# pass filenames to cat, append their text to outputfile.
ls lesson_*.txt | xargs -I {} cat {} >> lessons_all.txt

