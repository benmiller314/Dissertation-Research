# r2mallet.R
# 
# Code to control command-line Mallet from within R
# original by Ben Marwick (https://gist.github.com/benmarwick/4537873)
# forked for MacOS by Jeremiah Ory (https://gist.github.com/drlabratory/6198388)
# forked again, and currently, by Ben Miller (https://github.com/benmiller314)
#

## Step 1. Set up parameters we might want to change often
#  1a. A folder containing txt files for MALLET to work on
dataset_name <- "consorts"
importdir <- paste0("/Users/benmiller314/Documents/fulltext_dissertations/clean_", dataset_name, "_only")

#  1b. Number of topics for MALLET to use
ntopics <- 10

#  1c. optimisation interval for MALLET to use
optint <-  20


## Step 2. Set up stable elements of the working environment
#  2a. Let's assume we're typically going to need more Java heap space;
#  this sets the maximum allocation
    heap_param <- paste("-Xmx","2g",sep="") 
    options(java.parameters=heap_param)

#   2b. Configure variables and filenames for MALLET
#   where is MALLET, and what is the command that runs it?
MALLET_HOME <- "/Users/benmiller314/mallet-2.0.7"			
mallet_cmd <- paste0(MALLET_HOME, "/bin/mallet")


#   2c. name of file for MALLET to train model on
output <- paste0(dataset_name, ".mallet")

#   2d. file names for output of model (extensions must be as shown)
outputstate <- paste0(dataset_name, "_topic-state.gz")
outputtopickeys <- paste0(dataset_name, "_keys.txt")
outputdoctopics <- paste0(dataset_name, "_composition.txt")
  
## Step 3. Let's get going. 
#  3a. Combine variables into strings ready for executing on the Terminal command line
cd <- paste("cd", shQuote(MALLET_HOME)) 				
import <- paste(mallet_cmd, "import-dir --input", importdir, "--output", output, "--keep-sequence --remove-stopwords", sep = " ")
train  <- paste(mallet_cmd, "train-topics  --input", output, "--num-topics", ntopics, "--optimize-interval",  optint, "--output-state", outputstate,  "--output-topic-keys", outputtopickeys, "--output-doc-topics", outputdoctopics, sep = " ")

#  3b. Send commands to the Terminal command prompt, and
#  watch results scroll by in R console... seemingly endlessly.
#  for consorts (1,754 documents) and 10 topics, training took 2h 46m 36s.
setwd(MALLET_HOME)

system(paste(cd, "ls", sep=" ; "))
system(import)
system(train)			


## Step 4. Inspect results
library(bigmemory)
library(biganalytics)

#  4a. The matrix that MALLET spits out is sorted by document, with pairs of columns for each topic number and (descending) topic weight. Call the reshapeMallet script in Python (by Rolf Fredheim) to re-sort the matrix by topic, so we can get a big picture.
setwd(paste0(sourceloc, "/Shell scripts and commands"))
system("ls -F")
system("python reshapeMallet.py")	
# TODO: make this script take arguments, so we can control the dataset... and put the dataset_name in the filename of the output

#  4b. Read in the output from reshapeMallet.py (currently all files will be called "reshapedMallet11.txt")
doc_topics_reshaped <- read.big.matrix("reshapedMallet11.txt", header=F, sep="\t")


# # outputstateresult <-  
# outputtopickeysresult <- read.big.matrix(outputtopickeys, header=F, sep="\t")
# outputdoctopicsresult <- read.big.matrix(outputdoctopics, header=F, sep="\t")

# head(outputtopickeysresult)

# write reshaped table to CSV file for closer inspection
write.big.matrix(doc.topics.reshaped, paste0(dataloc, "doc_topics_reshaped_consorts.csv"))


if (!exists(sourceloc)) {
	source("/Users/benmiller314/Dropbox/coursework, etc/dissertation/R experiments/Dissertation Research/dataprep.R")
}
setwd(sourceloc)
