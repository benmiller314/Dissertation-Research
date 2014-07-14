require(mallet)
require(doBy)

## some params
malletloc <- "/Users/benmiller314/mallet-2.0.7"
textloc <- "/Users/benmiller314/Documents/fulltext dissertations/clean/test_subset"
K <- 10		# number of topics

# find out the current Java max heap size, since it's choking; then up it
getOption("java.parameters")			
j <- .jinit(parameters="-Xmx1024m", force.init=TRUE)

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



## NB: The following is based heavily on http://cran.r-project.org/web/packages/mallet/mallet.pdf by David Mimno (the 'not run' example for the MalletLDA function, starting on page 9), including most comments. This is where the topic modeling actually happens.

                  
# Ben: Turn most of this file into a function, so we can try different values of K
ben.mallet.tm <- function(K=10, 						# how many topics?
						  num.top.words=7, 				# how to label topics
						  runlong=FALSE, 				# do extra iterations?
						  top.cutoff.pct=10, 			# remove words in this % of documents or more
						  dataset_name="noexcludes"		# which subset of data to include?
						  ) 
{
	
	## 0. Create a wrapper for the data with elements for each column.
	# Note that "id" and "text" are special fields -- mallet will look there for input.

	## for abstracts:
	# documents <- list("id"=as.character(noexcludes$Pub.number), 
	                  # "text"=as.character(noexcludes$ABSTRACT)
	                  # )
	
	## for full text:
	documents <- mallet.read.dir(textloc)
		# cleanup step 1. Remove spellcounts sub-directory, which isn't a file
		documents <- documents[which(documents$id != paste0(textloc, "/spellstats")), ]
		
		# cleanup step 2. Every filename in documents$id has the same length,
		# and the Pub.number we want is always 7 characters long, starting 10 chars from the end.
		# Let's extract them, so we can merge with tag data further down the road.
		len <- nchar(documents$id[1])
		documents$id <- substr(documents$id, (len-10), (len-4))
	    rm(len)
	    
	    # cleanup step 3. Keep only files that are in the dataset we want.
		dataset <- get(dataset_name)
		dataset.index <- which(documents$id %in% dataset$Pub.number)
		documents <- documents[dataset.index, ]
		

	# 1. Create a mallet instance list object. Right now we have to specify the stoplist
	# as a file, can’t pass in a list from R. 
	## Ben adds: This creates a Java object required by the Java-like methods used with topic.model (hijacking R's '$' operator as if it's Java's '.' operator).
	mallet.instances <- mallet.import(id.array = documents$id,
		text.array = documents$text, 
		stoplist.file = paste0(malletloc,"/stoplists/en.txt"),
		# token.regexp = "\\p{L}+"
		token.regexp = "\\p{L}[-\\p{L}\\p{Po}]+\\p{L}"
		)
	## NB: Instead of Mimno's original p{P} (any punctuation) in the middle of the word, I modded the regex above to search for p{Po} -- that's "any kind of punctuation character that is not a dash, bracket, quote or connector," per http://www.regular-expressions.info/unicode.html -- plus hyphens. This was necessary to break words at em-dashes.
	## NB as well that this regex as structured defines words to be at least three characters long: a letter, plus a letter or punctuation, plus a letter. At some later point I may be curious about the use of the words "I," "me," "we," etc, and that would require a different regex.
	
	
	# 2. Create a topic trainer object.
	## Ben adds: It starts out uninteresting; the cool stuff happens when we run the operators on this Java object.
	topic.model <- MalletLDA(num.topics=K)
	
	# 3. Load our documents. We could also pass in the filename of a saved 
	# instance list file that we build from [Mallet's] command-line tools.
	topic.model$loadDocuments(mallet.instances)
	
	# 4. Get the vocabulary, and some statistics about word frequencies.
	# These may be useful in further curating the stopword list.
	vocabulary <- topic.model$getVocabulary()
	# print(vocabulary)
	word.freqs <- mallet.word.freqs(topic.model)
	# print(word.freqs)
	
	doc.freq.index <- order(word.freqs$doc.freq, decreasing=TRUE)
	word.freqs.sorted <- word.freqs[doc.freq.index, ]
	head(word.freqs.sorted, 10)
	tail(word.freqs.sorted, 200)
	
	####
	# 5. Ben: Let's curate that vocabulary! (Approach here based on Mimno 2012, pp. 4-5: he says 5-10%)
	# 5a. Find words occurring in more than 10% of the documents. Take them out, but save for later.
	cutoff <- length(documents$id) * (top.cutoff.pct/100)
	top.words.index <- which(word.freqs.sorted$doc.freq > cutoff)
	top.words <- word.freqs.sorted[top.words.index, ] 
		nrow(top.words) / length(vocabulary)
	
	# 5b. Find words occurring in fewer than 5 (count, not %) of the documents
	bottom.words.index <- which(word.freqs.sorted$doc.freq < 5)
	bottom.words <- word.freqs.sorted[bottom.words.index, ]
	
	# 5c. Create a new stoplist
	tandb.stoplist <- word.freqs.sorted[c(top.words.index, bottom.words.index), "words"]
	tandb.stoplist <- sort(as.character(tandb.stoplist))
	write(tandb.stoplist, file=paste0(malletloc,"/stoplists/top-and-bottom.txt"))
	
	# 5d. Concatenate the two stoplists
	en.stoplist <- readLines(paste0(malletloc,"/stoplists/en.txt"))
	new.stoplist <- c(en.stoplist, tandb.stoplist, "dissertation", "chapter")
	new.stoplist <- sort(new.stoplist)
	write(new.stoplist, file=paste0(malletloc,"/stoplists/en-plus-top-and-bottom.txt"))
	
	# 1-4, take two: Re-run the mallet.import, etc
	mallet.instances <- mallet.import(id.array = documents$id,
		text.array = documents$text, 
		stoplist.file = paste0(malletloc,"/stoplists/en-plus-top-and-bottom.txt"),
		# token.regexp = "\\p{L}[\\p{L}]+\\p{L}"
		token.regexp = "\\p{L}[-\\p{L}\\p{Po}]+\\p{L}"
		)
		
	rm(topic.model)												# maybe will help with heap usage?
	topic.model <- MalletLDA(num.topics=K)
	topic.model$loadDocuments(mallet.instances)
	vocabulary <- topic.model$getVocabulary()
	word.freqs <- mallet.word.freqs(topic.model)
	doc.freq.index <- order(word.freqs$doc.freq, decreasing=TRUE)
	word.freqs.sorted <- word.freqs[doc.freq.index, ]
	head(word.freqs.sorted, 25)
	
	#### 
	## Now let's resume where Mimno left off...
	# 6. Optimize hyperparameters every 20 iterations,
	# after 50 burn-in iterations.
	topic.model$setAlphaOptimization(20, 50)
	
	# 7. Now train a model. Note that hyperparameter optimization is on, by default.
	# We can specify the number of iterations. Here we’ll use a large-ish round number.
	if(runlong) {
		topic.model$train(500)
	} else {
	topic.model$train(200)		
	}
	
	# 8. NEW: run through a few iterations where we pick the best topic for each token,
	# rather than sampling from the posterior distribution.
	topic.model$maximize(10)	
		
	# 9. Get the probability of topics in documents and the probability of words in topics.
	# By default, these functions return raw word counts. Here we want probabilities,
	# so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
	
	# 9a. matrix with documents in rows, topics in columns; raw used only for testing.
	doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
	# doc.topics.raw <- mallet.doc.topics(topic.model, smoothed=F, normalized=F)
	
	# 9b. matrix with topics in rows, words in columns; raw used only for testing.
	topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)
	# topic.words.raw <- mallet.topic.words(topic.model, smoothed=F, normalized=F)

	# 10. Ben again: instead of using mallet.top.words, I want to use discriminative words
	topic.labels <- top.words.tfitf(topic.model, topic.words, num.top.words)

	# Now return the topic model itself, the labels for them, and the top words we removed:
	return <- list("topic.model" = topic.model,
				   "matrices" = list("doc.topics" = doc.topics, "topic.words" = topic.words),
				   "topic.labels" = topic.labels,
				   "top.words" = top.words)
}

debug(thresh)
big.nonconsorts <- thresh("nonconsorts")
big.nonconsorts <- big.nonconsorts$thresh.data

fulltext.tm.10 <- ben.mallet.tm(dataset_name="big.nonconsorts")
k10 <- data.matrix(fulltext.tm.10$topic.labels)
print(k10)
k10.top <- fulltext.tm.10$top.words
head(k10.top, 100)

abstracts.tm.16 <- ben.mallet.tm(K=16)
str(abstracts.tm.12)
k16 <- data.matrix(abstracts.tm.16$topic.labels)
print(k16)

abstracts.tm.10 <- ben.mallet.tm(K=10)
k10 <- data.matrix(abstracts.tm.10$topic.labels)
print(k10)

abstracts.tm.12 <- ben.mallet.tm(K=12)
k12_1 <- data.matrix(abstracts.tm.12$topic.labels)
abstracts.tm.12 <- ben.mallet.tm(K=12)
k12_2 <- data.matrix(abstracts.tm.12$topic.labels)
print(k12_1); print(k12_2)

abstracts.tm.12$top.words

mallet.topic.labels(abstracts.tm.12$topic.model, abstracts.tm.12$matrices$topic.words, num.top.words=8)
top.words.tfitf(abstracts.tm.12$topic.model, abstracts.tm.12$matrices$topic.words, num.top.words=8)
# plot(x=1:15, y=abstracts.tm$top.words$doc.freq[1:15]/nrow(abstracts.tm$matrices$doc.topics), ylog=F)
