#############################################################################
# r2mallet.R
# 
# Code to control command-line Mallet from within R. 
#
# original by Ben Marwick (https://gist.github.com/benmarwick/4537873)
# forked for MacOS by Jeremiah Ory
#     (https://gist.github.com/drlabratory/6198388)
# forked again, and currently, by Ben Miller 
#     (https://github.com/benmiller314)
#####

# Load required libraries
library(foreach)

## Set up stable elements of the working environment
# 	# Let's assume we're typically going to need more Java heap space;
# 	# this sets the maximum allocation for a 4GB MacBook Pro
# 	    heap_param <- paste("-Xmx","2g",sep="") 
# 	    options(java.parameters=heap_param)
	# now attempting 15GB for just under half a 32GB-RAM MacPro
# 	    heap_param <- paste("-Xmx","15g",sep="") 
# 	    options(java.parameters=heap_param)
#   Never mind, this is set by MALLET in $malletloc/bin/mallet, 
#   on line 10: "MEMORY=" etc.

	# What is the command that runs MALLET?
	mallet_cmd <- paste0(malletloc, "bin/mallet")
	
	# What's the directory within which to find text files for the corpus?
	importroot <- "~/Documents/fulltext_dissertations/"

	# What's the directory within which to output mallet files?
	outputroot <- "~/Documents/tm/"

	
# start of wrapper function	
r2mallet <- function(
				# Which datasets to examine?
					datasets = c("realconsorts"),
				
				# How many topics? Set kseq to a sequence to try several options.
					kseq = c(10, 30, 50, 100, 150, 200, 500),
					
				# What optimisation intervals for MALLET to use?
				# Default choices are from Mimno's library(mallet)
					optint = 20,
					optburnin = 50,
					numiterations = 250
) {		


	# Loop through each dataset and (1) import instances 
	# (2) make sure output files are available, then
	# then (3) build models w/varying numbers of topics.
	foreach(dataset_name = datasets) %do% {
		# 1a. Locate the folder containing txt files for MALLET to work on.
		importdir <-
			paste0(importroot, "clean_", dataset_name, "_only")
		
		# 1b. Locate the instance list. This will be stable for a given dataset,
		# regardless of the number of topics.
		output <- paste0(malletloc, dataset_name, "_instances.mallet")
		
		# Check to see if the instance list has already been created. If so,
		# then system(scope) will return 0; otherwise, run the import script now.
		# NB: This assumes you've already moved the files into their own directory.
		scope <- paste0("cd ", "~/'", substr(sourceloc, 3, nchar(sourceloc)), "'",
						"; cd 'Shell scripts and commands' ; ls ", output)
						
		if (system(scope)) {
			import <- paste(mallet_cmd, "import-dir --input", importdir,
						 "--output", output, 
						 "--keep-sequence --remove-stopwords")
			go <- readline(paste("About to import instance list.",
								 "Options set: keep sequence; remove stopwords.",
								 "Is that what you meant to do? (Y/N)\n"))
			if(tolower(go) != "y") { 
				stop("Never mind, then.") 
			} 
			
			print("Beginning import now...")
			if(! system(import)) { 
				print("Done.")      # If successful, report back.
			}
			
		} else {  # if system(scope) succeeds, it returns 0 and triggers this:
			print("Oh, good, the instance file exists. Moving on...")
		}	
		
		# Train the model. Topic-number dependent.
		# 3a. Start looping for each number of topics. 
		# kseq is defined at the top of this file.
		foreach(k = kseq) %do% {
			
			# 2a. File names for output of model (extensions must be as shown)
			outputstate <- paste0(outputroot, dataset_name, "k", k, "_topic-state.gz")
			outputtopickeys <- paste0(outputroot, dataset_name, "k", k, "_keys.txt")
			outputdoctopics <- paste0(outputroot, dataset_name, "k", k, "_composition.txt")
			wordtopics <- paste0(outputroot, dataset_name, "k", k, "_wordtopics.txt")
			
			# 2b. Check that the files above exist. If not, create blank ones.
			if (system(paste0("[ -s ", outputstate, " ]"))) {
				system(paste("touch -a", outputstate))
			} else {
			  	go <- readline(paste("Outputstate file already exists", 
									 "and is not empty. Overwrite (Y/N)?"))
				  if(tolower(go) != "y") { 
					  stop("Never mind, then.") 
				  } 			
			} 

			# 3b. String together command to send to MALLET via the shell  
			train <- paste(mallet_cmd, "train-topics  --input", output,
						 "--num-topics", k, 
						 "--optimize-interval",  optint, 
						 "--optimize-burn-in", optburnin, 
						 "--output-state", outputstate,  
						 "--output-topic-keys", outputtopickeys,
						 "--num-iterations", numiterations, 
						 "--output-doc-topics", outputdoctopics, 
						 "--word-topic-counts-file", wordtopics)
			
			# 3c. Run the command in the shell.	
			system(train)
		}
		
	} # close the loop of datasets

} # close the wrapper function

if(autorun) {
	r2mallet()
#     r2mallet("consorts")
#     r2mallet("real.consorts")
} else {
	message("Autorun is FALSE, so no action was taken.")
	message(paste("If you wish to create new topic models,", 
				  "check configuration, then set autorun to TRUE."))
}	
# ## Step 5. Inspect results
# library(bigmemory)
# library(biganalytics)

# #  5a. The matrix that MALLET spits out is sorted by document, with pairs of columns for each topic number and (descending) topic weight. Call the reshapeMallet script in Python (by Rolf Fredheim) to re-sort the matrix by topic, so we can get a big picture.
# setwd(paste0(sourceloc, "/Shell scripts and commands"))
# system("ls -F")
# system("python reshapeMallet.py")	
# # TODO: make this script take arguments, so we can control the dataset... and put the dataset_name in the filename of the output

# #  5b. Read in the output from reshapeMallet.py (currently all files will be called "reshapedMallet11.txt")
# doc_topics_reshaped <- read.csv("reshapedMallet11.txt", header=F, sep="\t")
# head(doc_topics_reshaped)

# # write reshaped table to CSV file for closer inspection
# write.big.matrix(doc.topics.reshaped, paste0(dataloc, "doc_topics_reshaped_consorts.csv"))

# names(consorts)
# consorts_doctopics <- merge(consorts[, c("Pub.number", "Title", "Subject", "KEYWORDS")], doc_topics_reshaped, by.y="V1", by.x="Pub.number", all.x=T, all.y=F)


# lapply(2:ncol(doc_topics_reshaped), FUN=function(x) summary(doc_topics_reshaped[, x]))

