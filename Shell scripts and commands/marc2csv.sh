#!/bin/bash

##########
# Extract information from MARC records (e.g. ProQuest data index files) 
# as readable csv files. Note that you'll probably want to massage these in OpenRefine
# afterwards.
##########

go_venv_marc

# SRC=$1
# DST=$2

# python /Users/millerb/python_venv/marc2csv-master/marc2csv.py  $SRC > $DST

python /Users/millerb/python_venv/marc2csv-master/marc2csv.py \
       /Users/millerb/Box\ Sync/research/dissertations/MARCDATA_20130520.MRC > \
       /Users/millerb/Box\ Sync/research/dissertations/marcdata_pqdt_20130520.csv

deactivate
