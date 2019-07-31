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
library(parallel)

## Set up stable elements of the working environment
# 	# Let's assume we're typically going to need more Java heap space;
# 	# this sets the maximum allocation for a 4GB MacBook Pro
# 	    heap_param <- paste("-Xmx","2g",sep="") 
# 	    options(java.parameters=heap_param)
	# now attempting 15GB for just under half a 32GB-RAM MacPro
# 	    heap_param <- paste("-Xmx","15g",sep="") 
# 	    options(java.parameters=heap_param)
#   Never mind, this is set by MALLET in $malletloc/bin/mallet, 
#   on line 10: "MEMORY=" etc. But see also $malletloc/bin/mallet.bat, line 14, "set MALLET_MEMORY="

	
# start of wrapper function	
r2mallet <- function(
				# Which datasets to examine?
					datasets = c("knownprograms"),
				
				# How many topics? Set kseq to a sequence to try several options.
					kseq = c(10, 30, 50, 60, 100, 150),
				    # kseq = c(55),
					
				# What optimisation intervals for MALLET to use?
				# Default choices are from Mimno's library(mallet)
					optint = 20,
					optburnin = 50,
					numiterations = 250,
				
				# To make pseudo-random results exactly replicable, specify a seed value
				    # seed = 8675309,
				    seed = NULL,

                # To use with the command line, just output the command and exit
				    cmdonly = F,
				
				# What is the command that runs MALLET?
				    mallet_cmd = file.path(malletloc, "bin", "mallet"),
				
				# What's the directory within which to find text files for the corpus?
				    # importroot = "~/Documents/fulltext_dissertations/",
				    importroot = fulltextloc,
				
				# What's the directory within which to output mallet files?
				    outputroot = tmloc,
				
				# What counts as a word?
	    			
                    # token_regex = "'\\p{L}[\\p{L}\\p{P}]*\\p{L}|\\p{L}'" 	)
				# (letters possibly including punctuation; from Mallet website 20181130).
				
    				token_regex = "'\\p{L}+[\\p{L}\\p{Po}]*[-]?\\p{L}+'"    )
				# One or more letter, plus a string of zero or more letters, or punctuation characters that are not dashes,
				# bracket, quote or connectors, plus zero or one hyphens, plus one or more letters. (This per
				# http://www.regular-expressions.info/unicode.html.) Updated post-diss.
{

	# Loop through each dataset and (1) import instances 
	# (2) make sure output files are available, 
	# then (3) build models w/varying numbers of topics.
	foreach(dataset_name = datasets) %do% {
		# 1a. Locate the folder containing txt files for MALLET to work on.
		importdir <- file.path(importroot, paste0("clean_", dataset_name, "_only"))
		
		# 1b. Locate the instance list. This will be stable for a given dataset,
		# regardless of the number of topics.
		output <- file.path(outputroot, paste0(dataset_name, "_instances.mallet"))
		
		# Check to see if the instance list has already been created. If so,
		# then system(scope) will return 0; otherwise, run the import script now.
		# NB: This assumes you've already moved the files into their own directory.
		scope <- paste0("cd ", "~/'", sub(path.expand("~/"), "", sourceloc), "'",
						"; cd 'Shell scripts and commands' ; ls \"", output, "\"")
						
		if (system(scope)) {
		    import <- paste(mallet_cmd, "import-dir --input", importdir,
						 "--output", output, 
						 "--keep-sequence --remove-stopwords",
						 "--token-regex", token_regex)    
			# go <- readline(paste("About to import instance list.",
			# 					 "Options set: keep sequence; remove stopwords.",
			# 					 "Is that what you meant to do? (Y/N)\n"))
			# if(tolower(go) != "y") { 
			# 	stop("Never mind, then.") 
			# } 
			
			if (cmdonly) {
			    return(import)
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
		i <- 0       # TO DO: get smarter about iter_index: only advance if same k as previous
		foreach(k = kseq) %do% {
			i <- i + 1
			# 2a. File names for output of model (extensions must be as shown)
			# New naming convention: locate i within model name, e.g.
			  model_name <- paste0(dataset_name, "k", k, "_", i)
			  outputstate <- file.path(outputroot, paste0(model_name, "_topic-state.gz"))
			  outputtopickeys <- file.path(outputroot, paste0(model_name, "_keys.txt"))
			  outputdoctopics <- file.path(outputroot, paste0(model_name, "_composition.txt"))
			  wordtopics <- file.path(outputroot, paste0(model_name, "_wordtopics.txt"))
			  diagnostics <- file.path(outputroot, paste0(model_name, "_diagnostics.xml"))
			
			
			# 2b. Check that the files above exist. If not, create blank ones.
			if (system(paste0("[ -s ", outputstate, " ]"))) {
				system(paste("touch -a", outputstate))
			} else {
			#   	go <- readline(paste("Outputstate file already exists", 
			# 						 "and is not empty. Overwrite (Y/N)?"))
			# 	  if(tolower(go) != "y") { 
			# 		  stop("Never mind, then.") 
			# 	  } 			
			} 

			# 3b. String together command to send to MALLET via the shell  
			train <- paste(mallet_cmd, "train-topics  --input", output,
						 "--num-topics", k, 
						 "--optimize-interval", optint, 
						 "--optimize-burn-in", optburnin, 
						 "--output-state", outputstate,  
						 "--output-topic-keys", outputtopickeys,
						 "--num-iterations", numiterations, 
						 "--output-doc-topics", outputdoctopics, 
						 "--word-topic-counts-file", wordtopics,
						 "--num-threads", parallel::detectCores()-1,
						 "--diagnostics-file", diagnostics)
			if(!is.null(seed)) {
			    train <- paste(train, "--random-seed", seed)
			}
			
			if(cmdonly) { 
			    return(train)    
			}			 
			
			# 3c. Run the command in the shell.
			message(paste("Starting at", Sys.time(), "using this command: \n", train))
			system.time(	system(train)     )     # about 41 minutes for 3648 dissertations and 150 topics
			message(paste("Finished at", Sys.time()))
		}
		
	} # close the loop of datasets

} # close the wrapper function

# Testing area / examples
if(FALSE) {
	r2mallet(datasets=c("realconsorts"), kseq=c(55), numiterations=250)   # about 9 minutes
    r2mallet(datasets=c("noexcludes2001_2015"), kseq=c(150), cmdonly=F)
    r2mallet(datasets=c("realconsorts"), kseq=rep(55, times=10), seed=NULL, numiterations=250)
    # r2mallet(datasets=c("realconsorts"), kseq=c(55), num_iterations=1000)   # about 32 minutes
#     r2mallet("consorts")
#     r2mallet("real.consorts")
    r2mallet(datasets=c("noexcludes2001_2015"), kseq=c(50, 15, 23), cmdonly=F)
    
} else if (autorun) {
    train <- r2mallet(cmdonly=T)
    message("\n(r2mallet with foreach.R) ",
            "Current default instance-training command is: \n\n",
            train, "\n")
	message("Topic modeling is often a slow process, so no action was taken.")
}	

