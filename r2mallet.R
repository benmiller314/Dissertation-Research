# r2mallet.R
# 
# Code to control command-line Mallet from within R
# original by Ben Marwick (https://gist.github.com/benmarwick/4537873)
# forked for MacOS by Jeremiah Ory (https://gist.github.com/drlabratory/6198388)
# forked again, and currently, by Ben Miller (https://github.com/benmiller314)
#

# Set working environment
if (!exists(sourceloc)) {
	source("/Users/benmiller314/Dropbox/coursework, etc/dissertation/R experiments/Dissertation Research/dataprep.R")
}
setwd(sourceloc)

# folder containing txt files for MALLET to work on
importdir <- "/Users/benmiller314/Documents/fulltext_dissertations/clean_consorts_only"

# set number of topics for MALLET to use
ntopics <- 10

# setup system enviroment for R
MALLET_HOME <- "/Users/benmiller314/mallet-2.0.7"			# location of the bin directory

# Let's assume we're typically going to need more Java heap space;
# this sets the maximum allocation
    heap_param <- paste("-Xmx","2g",sep="") 
    options(java.parameters=heap_param)

# configure variables and filenames for MALLET
## here using MALLET's built-in example data and
## variables from http://programminghistorian.org/lessons/topic-modeling-and-mallet


# name of file for MALLET to train model on
output <- "test.mallet"
# set optimisation interval for MALLET to use
optint <-  20
# set file names for output of model, extensions must be as shown
outputstate <-  "topic-state.gz"
outputtopickeys <- "test_keys.txt"
outputdoctopics <- "test_composition.txt"
  
# combine variables into strings ready for MacOS Terminal command line
mallet_cmd <- paste0(MALLET_HOME, "/bin/mallet")
cd <- paste("cd", shQuote(MALLET_HOME)) 				
import <- paste(mallet_cmd, "import-dir --input", importdir, "--output", output, "--keep-sequence --remove-stopwords", sep = " ")
train  <- paste(mallet_cmd, "train-topics  --input", output, "--num-topics", ntopics, "--optimize-interval",  optint, "--output-state", outputstate,  "--output-topic-keys", outputtopickeys, "--output-doc-topics", outputdoctopics, sep = " ")


# send commands to the Terminal command prompt
# watch results scroll by in R console...
system(paste(cd, "ls", sep=" ; "))
system(import)
system(train)			# for consorts (1,754 documents ) and 10 topics, took 2h 46m 36s

 
# inspect results
setwd(MALLET_HOME)
# outputstateresult <-  
outputtopickeysresult <- read.table(outputtopickeys, header=F, sep="\t")
outputdoctopicsresult <- read.table(outputdoctopics, header=F, sep="\t")

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
