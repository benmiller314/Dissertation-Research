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

# switch from 0-indexed to 1-indexed so the topic numbers in topic_keys are the same as row numbers
# NB: this seems to be necessary to avoid searching for column "0"
head(outputfile)
names(outputfile) <- c("Pub.number", (1:(ncol(outputfile)-1)))
head(outputfile)
library(data.table)
outputfile.dt <- as.data.table(outputfile)
head(outputfile.dt)

# Step 2. Find overall top topics
colsums <- colSums(outputfile.dt)
head(colsums)
colsums.sort <- colsums[order(colsums, decreasing=TRUE)]
head(colsums.sort)
plot(2:length(colsums.sort), colsums.sort[2:length(colsums.sort)], xlab="topic numbers (arbitrary)", ylab="sum of contributions")

# Oh, and what were those topics, again?
filename <- paste0(malletloc, "/", dataset_name, "k", ntopics, "_keys.txt")
topic_keys <- as.data.table(read.delim(filename, header=F))
setnames(topic_keys, c("V1", "V2", "V3"), c("topic", "alpha", "top_words"))
names(topic_keys)
head(topic_keys)

# switch from 0-indexed to 1-indexed so the topic numbers in topic_keys are the same as row numbers
# NB: this seems to be necessary to avoid searching for column "0"
topic_keys$topic <- 1:nrow(topic_keys)
head(topic_keys)
setkey(topic_keys, topic)

# Step 3. Find top 5 docs for each overall top topic 
# to get a sense of what's "real" and what's "interesting"
for (i in 1:min(length(colsums.sort)-1, cutoff)) {	# We're not looking at 100s of topics, so max out.	
	# i=1			# Test value
	# i gives the topic rank, not the column where it appears, b/c the 1st column is always Pub.number.
	# Add one to get the right column name, which is the topic number...
	topic.num <- as.integer(names(colsums.sort[i+1]))	
	
	# and add one again to skip the Pub.number column in outputfile
	col.ind <- topic.num + 1
	
	# Search outputfile for the dissertations with max proportion of that topic, and get the Pub.numbers
	row.ind <- order(outputfile[, col.ind], decreasing=TRUE)[1:5]
	diss.ind <- outputfile[row.ind, "Pub.number"]

	topdocs <- noexcludes[match(diss.ind, noexcludes$Pub.number), 	# TO DO: make noexcludes a data.table
						  c("Pub.number"
						  , "Title" 
						  , "Year"
						  # , "KEYWORDS"
						  # , "Subject" 
						  # , "ABSTRACT"
						  )]
	message("\nTopic of rank ", i, ":\n")
	print(topic_keys[topic.num])
	print(topdocs)
	# print(outputfile[row.ind, c("Pub.number", (col.ind-3):min((col.ind+3), length(colsums)))])
	readline("Press <enter> to continue\n")
}


# Explore the clustering of topics, using code from Rolf Fredheim
# source("frameToD3.R")
# dt <- as.data.table(outputfile)
# groupVars <- c("Pub.number")	# Ben: this is the name of that first (ID) column. replace accordingly.
# dataVars <- colnames(dt)[!colnames(dt) %in% groupVars]	# Ben: any column that's not an ID is a datapoint 
# filename <- paste0(malletloc, "/", dataset_name, "k", ntopics, "_clusters.json")
# frameToJSON(outputfile,groupVars,dataVars,outfile=filename)
