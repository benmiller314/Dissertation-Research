# r2mallet.R
# 
# Code to control command-line Mallet from within R
# original by Ben Marwick (https://gist.github.com/benmarwick/4537873)
# forked for MacOS by Jeremiah Ory (https://gist.github.com/drlabratory/6198388)
# forked again, and currently, by Ben Miller (https://github.com/benmiller314)
#

# Set working directory
dir <- "~/mallet/tutorial" # adjust to suit
setwd(dir)

# configure variables and filenames for MALLET
## here using MALLET's built-in example data and
## variables from http://programminghistorian.org/lessons/topic-modeling-and-mallet

# folder containing txt files for MALLET to work on
importdir <- "~/mallet/sample-data/web/en"
# name of file for MALLET to train model on
output <- "tutorial.mallet"
# set number of topics for MALLET to use
ntopics <- 20
# set optimisation interval for MALLET to use
optint <-  20
# set file names for output of model, extensions must be as shown
outputstate <-  "topic-state.gz"
outputtopickeys <- "tutorial_keys.txt"
outputdoctopics <- "tutorial_composition.txt"
  
# combine variables into strings ready for windows command line
cd <- "cd ~/mallet" # location of the bin directory
import <- paste("bin/mallet import-dir --input", importdir, "--output", output, "--keep-sequence --remove-stopwords", sep = " ")
train  <- paste("bin/mallet train-topics  --input", output, "--num-topics", ntopics, "--optimize-interval",  optint, "--output-state", outputstate,  "--output-topic-keys", outputtopickeys, "--output-doc-topics", outputdoctopics, sep = " ")

# setup system enviroment for R
MALLET_HOME <- "~/mallet" # location of the bin directory
# Sys.setenv("MALLET_HOME" = MALLET_HOME)
# Sys.setenv(PATH = "c:/Program Files (x86)/Java/jre7/bin")

# send commands to the Windows command prompt
# watch results scroll by in R console...
#shell(shQuote(paste(cd, import, train, sep = " && ")), 
#         invisible = FALSE)

system(paste(cd, import, train, sep = " ; "), invisible = FALSE)
 
# inspect results
setwd(MALLET_HOME)
# outputstateresult <-  
outputtopickeysresult <- read.table(outputtopickeys, header=F, sep="\t")
outputdoctopicsresult <-read.table(outputdoctopics, header=F, sep="\t")

# manipulate outputdoctopicsresult to be more useful 
dat <- outputdoctopicsresult
l_dat <- reshape(dat, idvar=1:2, varying=list(topics=colnames(dat[,seq(3, ncol(dat)-1, 2)]), 
                                             props=colnames(dat[,seq(4, ncol(dat), 2)])), 
                direction="long")
library(reshape2)
w_dat <- dcast(l_dat, V2 ~ V3)
rm(l_dat) # because this is very big but not longer needed

# write reshaped table to CSV file for closer inspection
write.csv(w_dat, "topic_model_table.csv")
