# r2mallet.R
# 
# Code to control command-line Mallet from within R
# original by Ben Marwick (https://gist.github.com/benmarwick/4537873)
# forked for MacOS by Jeremiah Ory (https://gist.github.com/drlabratory/6198388)
# forked again, and currently, by Ben Miller (https://github.com/benmiller314)
#

	library(foreach)
		
	## Step 1. Set up parameters we might want to change often
	# 1a. A folder containing txt files for MALLET to work on
	dataset_name <- "consorts"
	
	importdir <- paste0("/Users/benmiller314/Documents/fulltext_dissertations/clean_", dataset_name, "_only")
	
	# 1b. How many topics? Set kseq to a sequence to try several options.
	kseq=seq(150, 200, by=50)
	
	# 1c. optimisation interval for MALLET to use
	# (These choices from Mimno's library(mallet))
	optint <- 20
	optburnin <- 50
	numiterations <- 220
	
	## Step 2. Set up stable elements of the working environment
	# 2a. Let's assume we're typically going to need more Java heap space;
	# this sets the maximum allocation
	    heap_param <- paste("-Xmx","2g",sep="") 
	    options(java.parameters=heap_param)
	
	# 2b. Configure variables and filenames for MALLET
	# where is MALLET, and what is the command that runs it?
	MALLET_HOME <- "/Users/benmiller314/mallet-2.0.7"			
	mallet_cmd <- paste0(MALLET_HOME, "/bin/mallet")
	
	
	# 3. Import the instance list. This will be stable for a given dataset,
	# regardless of the number of topics.
	output <- paste0(MALLET_HOME, "/", dataset_name, ".mallet")
	cd <- paste("cd", shQuote(MALLET_HOME)) 				
	import <- paste(mallet_cmd, "import-dir --input", importdir, "--output", output, "--keep-sequence --remove-stopwords")
	
	system(paste(cd, "ls", sep=" ; "))	# show the growing directory
	system(import)
	
	
	# 4. Train the model. Topic-number dependent.
	# 4a. Start looping. kseq is defined at the top of this file.
	foreach(k = kseq) %do% {
		ntopics <- k
			
	# 4b. File names for output of model (extensions must be as shown)
	outputstate <- paste0(dataset_name, "K", k, "_topic-state.gz")
	outputtopickeys <- paste0(dataset_name, "K", k, "_keys.txt")
	outputdoctopics <- paste0(dataset_name, "K", k, "_composition.txt")
	
	# 4c. String together command to send to MALLET via the shell  
	train <- paste(mallet_cmd, "train-topics  --input", output, "--num-topics", ntopics, "--optimize-interval",  optint, "--optimize-burn-in", optburnin, "--output-state", outputstate,  "--output-topic-keys", outputtopickeys, "--num-iterations", numiterations, "--output-doc-topics", outputdoctopics)
	
	# 4d. Run the command in the shell.	
	system(train)
	}

## Step 4. Inspect results
library(bigmemory)
library(biganalytics)

#  4a. The matrix that MALLET spits out is sorted by document, with pairs of columns for each topic number and (descending) topic weight. Call the reshapeMallet script in Python (by Rolf Fredheim) to re-sort the matrix by topic, so we can get a big picture.
setwd(paste0(sourceloc, "/Shell scripts and commands"))
system("ls -F")
system("python reshapeMallet.py")	
# TODO: make this script take arguments, so we can control the dataset... and put the dataset_name in the filename of the output

#  4b. Read in the output from reshapeMallet.py (currently all files will be called "reshapedMallet11.txt")
doc_topics_reshaped <- read.csv("reshapedMallet11.txt", header=F, sep="\t")
head(doc_topics_reshaped)

# write reshaped table to CSV file for closer inspection
write.big.matrix(doc.topics.reshaped, paste0(dataloc, "doc_topics_reshaped_consorts.csv"))

names(consorts)
consorts_doctopics <- merge(consorts[, c("Pub.number", "Title", "Subject", "KEYWORDS")], doc_topics_reshaped, by.y="V1", by.x="Pub.number", all.x=T, all.y=F)


lapply(2:ncol(doc_topics_reshaped), FUN=function(x) summary(doc_topics_reshaped[, x]))


if (!exists(sourceloc)) {
	source("/Users/benmiller314/Dropbox/coursework, etc/dissertation/R experiments/Dissertation Research/dataprep.R")
}
setwd(sourceloc)