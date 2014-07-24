# Topic exploration
#
# STRATEGY: 
# 1. Use Rolf Fredheim's reshapeMallet.py to get a matrix of texts and topics, with topic proportions
# 	 as the intersections.
# 2. Find overall top topics (maximum proportion).
# 3. Find top 5 docs for each overall top topic; get abstracts, come up with a tentative title.
# 4. Write some justification for the labels, and send it to Sondra.

# Step 0. Make sure we're in familar territory.
	if (!exists("tagnames")) { 
		source(file="/Users/benmiller314/Dropbox/coursework, etc/dissertation/R experiments/Dissertation Research/dataprep.R") 
	}
	setwd(sourceloc)
	if (!exists("noexcludes")) { 
		source(file="/Users/benmiller314/Dropbox/coursework, etc/dissertation/R experiments/Dissertation Research/dataprep 2 - load data.R") 
	}
	
# Step 1. Get the matrix of texts and topics
# Choose dataset, number of topics
dataset_name <- "consorts"
ntopics <- "150"
cutoff <- 50		# I want to set this high, b/c a lot of "top" topics are just academic discourse.							# one way around this would be to throw out non-nouns, but...

# 1. Edit the reshapeMallet.py script (in TextWrangler) to update filenames, 
#    then run it here and read in the output.
filename <- paste0(malletloc, "/", dataset_name, "k", ntopics, "_doc-all-topics.txt")
system("cd 'Shell scripts and commands'; python reshapeMallet.py; cd ..")
outputfile <- read.delim(filename, header=F)

# switch from 0-indexed to 1-indexed so the topic numbers in topic_keys.dt are the same as row numbers
# NB: this seems to be necessary to avoid searching for column "0"
head(outputfile)
names(outputfile) <- c("Pub.number", (1:(ncol(outputfile)-1)))
head(outputfile)
library(data.table)
outputfile.dt <- as.data.table(outputfile)
head(outputfile.dt)

# Step 2. Find overall top topics
colsums <- colSums(outputfile.dt)
names(colsums) <- names(outputfile.dt)
head(colsums)
colsums.sort <- colsums[order(colsums, decreasing=TRUE)]
head(colsums.sort)
plot(2:length(colsums), colsums.sort[2:length(colsums)], xlab="topic numbers (arbitrary)", ylab="sum of contributions", xaxt="n")
text(x=1+2:length(colsums), y=1+colsums.sort[2:length(colsums)], labels=names(colsums.sort[2:length(colsums)]))

# Oh, and what were those topics, again?
filename <- paste0(malletloc, "/", dataset_name, "k", ntopics, "_keys.txt")
topic_keys.dt <- as.data.table(read.delim(filename, header=F))
setnames(topic_keys.dt, c("V1", "V2", "V3"), c("topic", "alpha", "top_words"))
names(topic_keys.dt)
head(topic_keys.dt)

# switch from 0-indexed to 1-indexed so the topic numbers in topic_keys.dt are the same as row numbers
# NB: this seems to be necessary to avoid searching for column "0"
topic_keys.dt$topic <- 1:nrow(topic_keys.dt)
head(topic_keys.dt)
setkey(topic_keys.dt, topic)

# Step 3. Find top 5 docs for each overall top topic 
# to get a sense of what's "real" and what's "interesting"

	# Step 4. Find all the top-ranked topics for those docs: maybe that really popular topic isn't actually the main component of the docs that come up. We start with the doc-topic matrix from MALLET:
	filename <- paste0(malletloc, "/", dataset_name, "k", ntopics, "_composition.txt")
	doc_topics <- read.delim(filename, header=F, skip=1)
	head(doc_topics)
	
	# column 1 is an unneeded index; column 2 contains names of identical length, 
	# ending with a 7-digit Pub.number followed by ".txt"; final column is empty. Let's simplify.
	doc_topics[, "V1"] <- NULL 
	len <- nchar(as.character(doc_topics[1, "V2"]))
	doc_topics[, "V2"] <- substr(as.character(doc_topics[, "V2"]), (len-10), (len-4))
	if (is.na(all(doc_topics[, ncol(doc_topics)]))) { doc_topics[, ncol(doc_topics)] <- NULL}

	# Get findable column names
	colnames(doc_topics)[1] <- "Pub.number"
	colnames(doc_topics)[seq(2, ncol(doc_topics), 2)] <- paste0("top", seq(1, (ncol(doc_topics)-1)/2, 1))
	colnames(doc_topics)[seq(3, ncol(doc_topics), 2)] <- paste0("wgt", seq(1, (ncol(doc_topics)-1)/2, 1))
	head(colnames(doc_topics))
	
	# convert to 1-indexed from MALLET's 0-indexed, so everything matches
	doc_topics[, seq(2, ncol(doc_topics), 2)] <- (doc_topics[, seq(2, ncol(doc_topics), 2)]+1)
	
	# for some reason, it thinks the topic weights are characters. They're numbers.
	doc_topics[, seq(3, ncol(doc_topics), 2)] <- apply(doc_topics[, seq(3, ncol(doc_topics), 2)], 2, FUN=function(x) {
			x <- as.numeric(x)
	})

	doc_topics.dt <- as.data.table(doc_topics)
	setkey(doc_topics.dt, Pub.number)
	head(doc_topics.dt)
	
	noexcludes.dt <- as.data.table(noexcludes)
	setkey(noexcludes.dt, Pub.number)
	
get.topics4doc <- function(pubnum, doc_tops=doc_topics.dt, topic_keys=topic_keys.dt) {
		# pubnum <- "3051708"; doc_tops <- doc_topics.dt	# test values
		if (!is.character(pubnum)) { pubnum <- as.character(pubnum) }
		list("title" = noexcludes.dt[pubnum, c("Title", tagnames, pubnum), with=F],
			"doc_tops" = doc_tops[pubnum, paste0(c("top","wgt"), rep(1:5, each=2)), with=F],
			"keys" = topic_keys[as.numeric(doc_tops[pubnum, paste0("top", 1:5), with=F])],
			"abstract" = noexcludes.dt[pubnum, c("KEYWORDS", "ABSTRACT"), with=F]		
			)
}

# Browse through the top topics and their top-proportioned dissertations
top_topic_browser <- function(start.at=1) {
	# just in case
	require(data.table)
	
	# List the keys for the top N topics, where N = cutoff
	len <- min(length(colsums)-1, cutoff)
	ind <- as.integer(names(colsums.sort)[2:len])		# list of topics by rank; skip Pub.num
	message("Top ", cutoff, " topics:")
	print(topic_keys.dt[ind])							# top words for each topic

	
	
	# Loop through the top topics and their top-proportioned dissertations,  
	# optionally showing abstracts and top 5 topics for each of those dissertations
	for (i in start.at:len) {
		# i=5						# Test value
		# i gives the topic rank
		topic.num <- ind[i]	
		
		# Search outputfile for the dissertations with max proportion of that topic, and get the Pub.numbers
		row.ind <- order(outputfile[, which(names(outputfile)==topic.num)], decreasing=TRUE)[1:5]
		diss.ind <- outputfile[row.ind, "Pub.number"]

		if (remake_figs) { 	print(paste0("Topic of rank ", i, ":")) } 
		else { message("\nTopic of rank ", i, ":\n") }
		
		print(topic_keys.dt[topic.num])
		
		topdocs <- noexcludes.dt[as.character(diss.ind), c("Pub.number", "Title", tagnames), with=F]
		print(topdocs)
		if (!remake_figs) { a <- readline("Press <enter> for more detail on these docs, or S to skip to the next topic\n") } else { a <- ""}

		while (tolower(a) != "s") {
			for(i in topdocs$Pub.number) {
				print(get.topics4doc(pubnum=i))
				if (!remake_figs) { a <- readline("Press <enter> for next doc, U for previous doc, or S to skip to the next topic\n") } else { a <- ""}
				if (tolower(a) == "s") { break }
				else if (tolower(a) == "u") {i <- i-1}
			}
			a <- "s"
		}
	}
}

if (remake_figs) { 
	filename <- paste0(imageloc, "top topics, K", ntopics, ".txt")
	capture.output(top_topic_browser(), file=filename 
} else {
	top_topic_browser()
}



# Explore the clustering of topics, using code from Rolf Fredheim
# source("frameToD3.R")
# dt <- as.data.table(outputfile)
# groupVars <- c("Pub.number")	# Ben: this is the name of that first (ID) column. replace accordingly.
# dataVars <- colnames(dt)[!colnames(dt) %in% groupVars]	# Ben: any column that's not an ID is a datapoint 
# filename <- paste0(malletloc, "/", dataset_name, "k", ntopics, "_clusters.json")
# frameToJSON(outputfile,groupVars,dataVars,outfile=filename)
