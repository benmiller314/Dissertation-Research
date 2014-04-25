require(mallet)

## some params
malletloc <- "/Users/benmiller314/mallet-2.0.7"
textloc <- "/Users/benmiller314/Documents/fulltext dissertations/100txt/"
noexcludes$fulltextpointers <- paste0(textloc, noexcludes$Pub.number, ".txt")

# number of topics
K <- 10	



# ff based on http://cran.r-project.org/web/packages/mallet/mallet.pdf by David Mimno
# (the 'not run' example for MalletLDA, starting on page 9), including most comments

# Create a wrapper for the data with [two] elements, one for each column.
# R does some type inference, and will guess wrong, so give it hints with "colClasses".
# Note that "id" and "text" are special fields -- mallet will look there for input.
documents <- list("id"=as.character(noexcludes$Pub.number), 
                  "text"=as.character(noexcludes$ABSTRACT)
                  )

# Create a mallet instance list object. Right now we have to specify the stoplist
# as a file, can’t pass in a list from R. 
## Ben adds: This creates a Java object required by the Java-like methods used with  topic.model (hijacking R's '$' operator as if it's Java's '.' operator).
mallet.instances <- mallet.import(id.array = documents$id,
	text.array = documents$text, 
	stoplist.file = paste0(malletloc,"/stoplists/en.txt"),
	token.regexp = "\\p{L}[-\\p{L}\\p{Po}]+\\p{L}"
	)
## NB: Instead of Mimno's original p{P} (any punctuation) in the middle of the word, I modded the regex above to search for p{Po} -- that's "any kind of punctuation character that is not a dash, bracket, quote or connector," per http://www.regular-expressions.info/unicode.html -- plus hyphens. This was necessary to break words at  em-dashes.
## NB as well that this regex as structured defines words to be at least three characters long: a letter, plus a letter or punctuation, plus a letter. At some later point I may be curious about the use of the words "I," "me," "we," etc, and that would require a different regex.


# Create a topic trainer object.
## Ben adds: It starts out uninteresting; the cool stuff happens when we run the operators on this Java object.
topic.model <- MalletLDA(num.topics=K)

# Load our documents. We could also pass in the filename of a saved 
# instance list file that we build from [Mallet's] command-line tools.
topic.model$loadDocuments(mallet.instances)

# Get the vocabulary, and some statistics about word frequencies.
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
# Ben: Let's curate that vocabulary!
# Find words occurring in more than 10% of the documents.
cutoff <- length(documents$id) * .10
top.words.index <- which(word.freqs.sorted$doc.freq > cutoff)
top.words <- word.freqs.sorted[top.words.index, ]

# Find words occurring in fewer than 5 (count, not %) of the documents
bottom.words.index <- which(word.freqs.sorted$doc.freq < 5)
bottom.words <- word.freqs.sorted[bottom.words.index, ]

# Create a new stoplist
tandb.stoplist <- word.freqs.sorted[c(top.words.index, bottom.words.index), "words"]
tandb.stoplist <- sort(as.character(tandb.stoplist))
write(tandb.stoplist, file=paste0(malletloc,"/stoplists/top-and-bottom.txt"))

# Concatenate the two stoplists
en.stoplist <- readLines(paste0(malletloc,"/stoplists/en.txt"))
new.stoplist <- c(en.stoplist, tandb.stoplist, "dissertation", "chapter")
new.stoplist <- sort(new.stoplist)
write(new.stoplist, file=paste0(malletloc,"/stoplists/en-plus-top-and-bottom.txt"))

# Re-run the mallet.import, etc
mallet.instances <- mallet.import(id.array = documents$id,
	text.array = documents$text, 
	stoplist.file = paste0(malletloc,"/stoplists/en-plus-top-and-bottom.txt"),
	token.regexp = "\\p{L}[-\\p{L}\\p{Po}]+\\p{L}"
	)
topic.model <- MalletLDA(num.topics=K)
topic.model$loadDocuments(mallet.instances)
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
doc.freq.index <- order(word.freqs$doc.freq, decreasing=TRUE)
word.freqs.sorted <- word.freqs[doc.freq.index, ]
head(word.freqs.sorted, 25)

#### 
## Now let's resume where Mimno left off...
# Optimize hyperparameters every 20 iterations,
# after 50 burn-in iterations.
topic.model$setAlphaOptimization(20, 50)

# Now train a model. Note that hyperparameter optimization is on, by default.
# We can specify the number of iterations. Here we’ll use a large-ish round number.
topic.model$train(200)

# NEW: run through a few iterations where we pick the best topic for each token,
# rather than sampling from the posterior distribution.
topic.model$maximize(10)

# Get the probability of topics in documents and the probability of words in topics.
# By default, these functions return raw word counts. Here we want probabilities,
# so we normalize, and add "smoothing" so that nothing has exactly 0 probability.

# matrix with documents in rows, topics in columns
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
doc.topics.raw <- mallet.doc.topics(topic.model, smoothed=F, normalized=F)

# matrix with topics in rows, words in columns
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)
topic.words.raw <- mallet.topic.words(topic.model, smoothed=F, normalized=F)
# vocabulary[1:5]; topic.words.raw[, 1:5]


# I'd like to get the top words in each topic ranked not by term frequency alone but by uniqueness to the topic -- i.e. term frequency * inverse topic frequency (as modeled on TF*IDF).
# These will then be used to determine topic subject matter.

top.words.tfitf <- function (topic.model, topic.words, num.top.words = 10) 
{
	# 1. for each term-topic pair, calculate term frequency = weight of the term in the topic divided by the total number of terms assigned to the topic. For a normalized topic, the sum should always be 1, so this is just the weight value at each location in the topic.words matrix.
	tf <- topic.words

	# 2. for each term, calculate inverse topic frequency = log(#topics / #topics assigned to that term). Number of topics = K (defined above).
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
	lapply(1:K, FUN=function(x) paste0(vocabulary[top.indices[[x]]], collapse=", "))
}

topics <- top.words.tfitf(topic.model, topic.words, 6)
