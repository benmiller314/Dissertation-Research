## Topic modeling, take 2
# 
# The plan:
# 1. Compress cleaned noexcludes into a single file in MALLET-ready format
# 2. Set up MALLET to analyze that file, but don't run yet
#	 a. to import the instances from that single file, use system() to run MALLET outside of R
#		(as in Ben Warwick's 'R2MALLET.r); use my token regex, not his original one
#    b. to train the model, use library(mallet) and David Mimno's approach 
#	    (as in 'topic modeling with mallet package.R'), with his optimized (hyper)parameters
# 3. If we don't know number of topics, 
#	 a. choose a subset of the data to train on
#	 b. use foreach() to try a sequence from 10-200, interval 10
#    c. maximize log.likelihood/token, which MALLET outputs somewhere by default. (Find it!)
# 4. Run MALLET with the parameters set up in Step 2, with the topics as chosen in 1 or 3.
##

## 0. Establish the working environment.
	if (!exists(sourceloc)) { 
		source(file="/Users/benmiller314/Dropbox/coursework, etc/dissertation/R experiments/Dissertation Research/dataprep.R") 
	}
	setwd(sourceloc)
	
	# Assume we're typically going to need more Java heap space, set maximum allocation
	heap_param <- paste("-Xmx","2g",sep="") 
	options(java.parameters=heap_param)
	
## Step 1. Compress the files, if they haven't been compressed already
## NB: double-check which commands are commented out before running; this could take a while.
system("'./Shell scripts and commands/ben_clean_and_consolidate.sh'")



## Step 2. Set up MALLET to analyze that file, but don't run yet
## Step 2a. Run MALLET externally to read in that file as a list of instances.
# (This use of system() inspired by https://gist.github.com/benmarwick/4537873, 
# via https://gist.github.com/drlabratory/6198388). Instructions for MALLET import
# are online at http://mallet.cs.umass.edu/import.php.
ben.mallet.import <- function(remove_stopwords=T, extra_stopwords=F) {
	# 2a.1. where is MALLET, and what is the command that runs it?
	MALLET_HOME <- "/Users/benmiller314/mallet-2.0.7"			
	mallet_cmd <- paste0(MALLET_HOME, "/bin/mallet")
	
	# 2a.2. Where is the import file? (determined by the shell script in Step 1)
	import_file <- "/Users/benmiller314/Documents/fulltext_dissertations/cumulative/noexcludes_cumulative.txt"
	
	# 2a.3. Where should we save the instances created by the import? (we'll need this in Step 2b)
	imported_file <- "/Users/benmiller314/Documents/fulltext_dissertations/topicmodels/noexcludes_instances.mallet"
	
	# 2a.4. What counts as a token?
	token_regex <- "\\p{L}[-\\p{L}\\p{Po}]+\\p{L}"
		# NB: Instead of the default [A-Za-z]*, or Mimno's original p{P} (any punctuation) in the middle of the word, I modded the regex above to search for p{Po} -- that's "any kind of punctuation character that is not a dash, bracket, quote or connector," per http://www.regular-expressions.info/unicode.html -- plus hyphens. This was necessary to break words at em-dashes.
		# NB as well that this regex as structured defines words to be at least three characters long: a letter, plus a letter or punctuation, plus a letter. At some later point I may be curious about the use of the words "I," "me," "we," etc, and that would require a different regex.
	
	# 2a.5. Any other parameters for tokenizing?
		# stoplist_file: use in addition to the standard English stopwords. 
		# Optionally created by ben.mallet.tm(), below. 
		if (remove_stopwords) { 
			stop_options <- "--remove-stopwords" 
		} else {
			stop_options <- "" 
		}
		if (extra_stopwords) { 	
			stoplist_file <- paste0(MALLET_HOME, "/stoplists/top-and-bottom-plus.txt")
			stop_options <- paste(stopoptions, "--extra-stopwords", stoplist_file) 
		} else { 
			stop_options <- paste(stop_options, "") 
		}
	
	# 2a.6. Set the import command to include the parameters set above.
	import_cmd <- paste(mallet_cmd, "import-file --input", import_file, 
						"--output", imported_file, 
						"--token_regex", token_regex, 
						stop_options
					)
	
	# 2a.7. Trigger the import.
	do.call(system, import_cmd, quote=TRUE)

# close the mallet import function
}

# Step 2b. Use Mimno's library(mallet) to actually train the model on those instances.
ben.mallet.tm <- function(K=10, 						# how many topics?
						  imported_file,				# the file created by import_from_mallet above
						  curate_vocab=FALSE			# create new stoplist from top/bottom words?
						  top.cutoff.pct=10, 			# remove words in this % of documents or more		
						  num.top.words=7, 				# how to label topics
						  runlong=FALSE, 				# do extra iterations?
						  dataset_name="noexcludes"		# which subset of data to include?
						  # abstracts=FALSE				# use full text (default) or abstracts only?
						  ) 
{
	
	# 2b.1. Create a topic trainer object.
	# NB: It starts out uninteresting; the cool stuff happens when we run the operators on this Java object.
	topic.model <- MalletLDA(num.topics=K)
	
	# 2b.2. Load our documents from a saved 
	# instance list file that we build from [Mallet's] command-line tools.
	topic.model$loadDocuments(imported_file)
	
	# 2b.3. Get the vocabulary, and some statistics about word frequencies.
	# These may be useful in further curating the stopword list.
	# To save on memory in the vocabulary, word.freqs, etc, use the big.matrix format.
	library(bigmemory)
	vocabulary <- as.big.matrix(topic.model$getVocabulary(), type="character")
	# print(vocabulary)
	word.freqs <- as.big.matrix(mallet.word.freqs(topic.model), type="integer")
	# print(word.freqs)
	
	doc.freq.index <- morder(word.freqs, "doc.freq", decreasing=TRUE)
	word.freqs.sorted <- mpermute(word.freqs, order=doc.freq.index, cols="doc.freq")
	head(word.freqs.sorted, 30)		# inspect the words occurring in the most documents
	# tail(word.freqs.sorted, 100)		# inspect the words occurring in the least documents
	
	#### 2b.4. Optional: Curate the vocabulary
	# (Approach here based on Mimno 2012, pp. 4-5: he recommends removing top 5-10% and bottom 5 count)
	if (curate_vocab) {
		# 2b.4.a. Find words occurring in more than top.cutoff.pct of the documents. 
		# Take them out, but save for later.
		cutoff <- length(doc.freq.index) * (top.cutoff.pct/100)
		top.words.index <- mwhich(word.freqs.sorted, "doc.freq", list(cutoff), list("gt"))
		top.words <- word.freqs.sorted[top.words.index, ] 
			nrow(top.words) / length(vocabulary)
		
		# 2b.4.b. Find words occurring in fewer than 5 (count, not %) of the documents
		bottom.words.index <- mwhich(word.freqs.sorted, "doc.freq", list(5), list("lt"))
		bottom.words <- word.freqs.sorted[bottom.words.index, ]
		
		# 2b.4.c. Create a new stoplist
		tandb.stoplist <- word.freqs.sorted[c(top.words.index, bottom.words.index), "words"]
		tandb.stoplist <- sort(as.character(tandb.stoplist))
		write(tandb.stoplist, file=paste0(MALLET_HOME, "/stoplists/top-and-bottom.txt"))
		
		# 2b.4.d. Any other words that seem like they need pruning?
		extra.stoplist <- c(tandb.stoplist, "dissertation", "chapter", "UMI")
		extra.stoplist <- sort(as.character(extra.stoplist))
		write(extra.stoplist, file=paste0(MALLET_HOME, "/stoplists/top-and-bottom-plus.txt"))
		
	# end of stoplist vocabulary curation; we can pick it up again in another call to ben.mallet.import
	}
		

	## Now let's resume where Mimno left off... This is the actual model-training portion.
	# 2b.5. Set to optimize hyperparameters every 20 iterations, after 50 burn-in iterations.
	topic.model$setAlphaOptimization(20, 50)
	
	# 2b.6. Now train a model. Note that hyperparameter optimization is on, by default.
	# We can specify the number of iterations. Here we’ll use a large-ish round number.
	if(runlong) {
		topic.model$train(500)		# Even this is much smaller than Ben Marwick's default 1000!
	} else {
	topic.model$train(200)		
	}
	
	# 2b.7. Run through a few iterations where we pick the best topic for each token,
	# rather than sampling from the posterior distribution.
	topic.model$maximize(10)	
		
	# 2b.8. Get the probability of topics in documents and the probability of words in topics.
	# By default, these functions return raw word counts. Here we want probabilities,
	# so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
	
	# 2b.8.a. matrix with documents in rows, topics in columns; raw used only for testing.
	# These are huge files, so use big.matrix again.
	doc.topics <- as.big.matrix(mallet.doc.topics(topic.model, smoothed=T, normalized=T), backingfile=paste0(MALLET_HOME, "/", dataset_name, "K", K,  "_doc_topics"))
	# doc.topics.raw <- as.big.matrix(mallet.doc.topics(topic.model, smoothed=F, normalized=F))
	
	# 2b.8.b. matrix with topics in rows, words in columns; raw used only for testing.
	topic.words <- as.big.matrix(mallet.topic.words(topic.model, smoothed=T, normalized=T), backingfile=paste0(MALLET_HOME, "/", dataset_name, "K", K,  "_topic_words"))
	# topic.words.raw <- as.big.matrix(mallet.topic.words(topic.model, smoothed=F, normalized=F))

	# 2b.9  Label topics with most frequent words
	topic.labels <- mallet.top.words(topic.model, topic.words, num.top.words)

	# 2b.10. Ben again: instead of using mallet.top.words, I want to use discriminative words.
	# The helper function top.words.tfitf() is defined below.
	topic.labels.tfitf <- top.words.tfitf(topic.model, topic.words, num.top.words)

	# Now pass back the topic model itself, the labels for them, and the top words we removed:
	return <- list("topic.model" = topic.model,					# java object
				   "doc.topics" = doc.topics, 					# doc/topic big.matrix, filebacked
				   "topic.words" = topic.words,					# topic/word big.matrix, filebacked
				   "topic.labels" = topic.labels,				# most frequent words in each topic
				   "topic.labels.tfitf" = topic.labels.tfitf,	# most distinctive words in each topic
				   "top.words" = top.words						# the words we 
				   )
}

## **Helper function: top.words.tfitf**
# I'd like to get the top words in each topic ranked not by term frequency alone but by uniqueness to the topic -- i.e. term frequency * inverse topic frequency (as modeled on TF*IDF).
# These will then be used to determine topic subject matter.
top.words.tfitf <- function (topic.model, topic.words, num.top.words = 10) 
{
	# 1. for each term-topic pair, calculate term frequency = weight of the term in the topic divided by the total number of terms assigned to the topic. For a normalized topic, the sum should always be 1, so this is just the weight value at each location in the topic.words matrix.
	tf <- topic.words

	# 2. for each term, calculate inverse topic frequency = log(#topics / #topics assigned to that term). Number of topics K implicit in topic.words (and presumably topic.model, but I don't know what to call).
	K <- nrow(topic.words)
	itf <- apply(topic.words, 2, sum)
	itf <- log(K / itf)

	# 3. multiply TF by ITF. 
	# NB: R wants to line up the ITF vector vertically with the TF grid and snake it around columns, which is not what we want. Instead, transpose TF and then undo it afterwards. (For some reason in vector-logic, transposing ITF will generate an error.)

	tf.itf <- t(t(tf) * itf)
	dim(tf.itf)
	# d <- t(t(a) * b)
	# d[2,] <- d[2, order(d[2,], decreasing=T)]
	top.indices <- lapply(1:K, FUN=function(x) head(order(tf.itf[x,], decreasing=T), num.top.words))

	# NB: the vocabulary indices are the same as the indices used in each row of the topic.words matrix (and, thus, the tf.itf matrix).
	lapply(1:K, FUN=function(x) noquote(paste0(vocabulary[top.indices[[x]]], collapse=", ")))
}

