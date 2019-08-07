#!/bin/bash									# declare our shell environment

/Users/benmiller314/Dropbox/coursework\,\ etc/dissertation/data\,\ code\,\ and\ figures/Dissertation\ Research/Shell\ scripts\ and\ commands/move_files_pdf.sh 

find /Users/benmiller314/Documents/fulltext_dissertations -type f -mindepth 2 -maxdepth 2 -regex .*/clean_.* -delete
