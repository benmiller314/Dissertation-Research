# Topic exploration
#
# STRATEGY: 
# 1. Use Rolf Fredheim's reshapeMallet.py to get a matrix of texts and topics, with topic proportions
# 	 as the intersections.
# 2. Find overall top topics (maximum proportion).
# 3. Find top 5 docs for each overall top topic; get abstracts, come up with a tentative title.
# 4. Also find top methods for each of those 5 docs
# 5. Write some justification for the labels, and send it to Sondra.
#
### TO DO: wrap this all up in a single function and clean up the file

# Step 0. Make sure we're in familar territory.
	if (!exists("tagnames")) { 
		source(file="/Users/benmiller314/Dropbox/coursework, etc/dissertation/R experiments/Dissertation Research/dataprep.R") 
	}
	setwd(sourceloc)
	if (!exists("noexcludes")) { 
		source(file="/Users/benmiller314/Dropbox/coursework, etc/dissertation/R experiments/Dissertation Research/dataprep 2 - load data.R") 
	}
	
# Step 1. Get the matrix of texts and topics
# # Choose dataset, number of topics -- now needed only for testing, so commented out
# dataset_name <- "consorts"
# ntopics <- 55
# cutoff <- 55		# I want to set this high, b/c a lot of "top" topics are just academic discourse.
					# # one way around this would be to throw out non-nouns, but...

## 1. Edit the reshapeMallet.py script (in TextWrangler) to update filenames, 
#    then run it here and read in the output.
get.doctopic.grid <- function(dataset_name="consorts", ntopics=55, doplot=F) {
	# get packages in case we've just restarted R
	require(data.table)
	
	filename <- paste0(malletloc, "/", dataset_name, "k", ntopics, "_doc-all-topics.txt")
	scope <- paste("cd", shQuote(sourceloc), "; cd 'Shell scripts and commands' ; ls ", filename)
	if (system(scope)) {
		command <- paste("cd", shQuote(sourceloc), "; cd 'Shell scripts and commands' python reshapeMallet.py")
		go <- readline("Have you updated reshapeMallet.py to reflect your current dataset/ntopics? (Y/N)\n")
		if(tolower(go) != "y") { stop("Better fix that, then") } 
		print("Converting topic/weight pairs into doc/topic grid...")
		if(! system(command)) { print("Done.") }
	} else { 
		print("Oh, good, the file exists. Moving on...")
	}
	outputfile <- read.delim(filename, header=F)
	# switch from 0-indexed to 1-indexed so the topic numbers in topic_keys.dt are the same as row numbers
	# NB: this seems to be necessary to avoid searching for column "0"
	head(outputfile)
	names(outputfile) <- c("Pub.number", (1:(ncol(outputfile)-1)))
	head(outputfile)
	library(data.table)
	outputfile.dt <- as.data.table(outputfile)
	head(outputfile.dt)
	## Step 2. Find overall top topics
	# Each cell gives the percentage the topic in that column contributes to the dissertation in that row.
	# Summing these percentages and sorting gives us a rank based on percentage points.
	colsums <- colSums(outputfile.dt)
	names(colsums) <- names(outputfile.dt)
	head(colsums)
	colsums.sort <- colsums[order(colsums, decreasing=TRUE)]
	head(colsums.sort)
	# Divide the percentage point totals by the number of dissertations to get an overall percent contribution
	colsums.sort.pct <- round((colsums.sort / nrow(outputfile)), 4) * 100
	if(remake_figs) { 
		filename <- paste0(imageloc, dataset_name, "k", ntopics, "_topic-ranks.csv")
		write.csv(colsums.sort.pct[2:length(colsums.sort.pct)], filename)
	}
	# Optionally get an overview of the topic sizes, as a scatterplot
	if(doplot) {
		plot(2:length(colsums), colsums.sort[2:length(colsums)], xlab="topic numbers (arbitrary)", ylab="sum of contributions", xaxt="n")
		text(x=1+2:length(colsums), y=1+colsums.sort[2:length(colsums)], labels=names(colsums.sort[2:length(colsums)]))
	}
	# Return with the goods
	list("colsums" = colsums,
		 "colsums.sort" = colsums.sort,
		 "colsums.sort.pct" = colsums.sort.pct,
		 "outputfile" = outputfile
		 )

}	# end of get.doctopic.grid()

# Oh, and what were those topics, again?
get.topickeys <- function(dataset_name="consorts", ntopics=55) {
	# get packages in case we've just restarted R
	require(data.table)
	
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
	return(topic_keys.dt)
}

# Step 3. Find top 5 docs for each overall top topic 
# to get a sense of what's "real" and what's "interesting"

	# Step 4. Find all the top-ranked topics for those docs: maybe that really popular topic isn't actually the main component of the docs that come up. We start with the doc-topic matrix from MALLET:
get.doc.composition <- function(dataset_name="consorts", ntopics=55) {
	# get packages in case we've just restarted R
	require(data.table)
	
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
	return(doc_topics.dt)
}
	
noexcludes.dt <- as.data.table(noexcludes)
setkey(noexcludes.dt, Pub.number)

# Helper function: retrieve top five topics for a given Pub.number
get.topics4doc <- function(pubnum, dataset_name="consorts", ntopics=55) {
		# get packages in case we've just restarted R
		require(data.table)
		
		# pubnum <- "3051708"; doc_tops <- doc_topics.dt	# test values
		if (!is.character(pubnum)) { pubnum <- as.character(pubnum) }
		
		doc_tops <- get.doc.composition(dataset_name, ntopics)
		topic_keys <- get.topickeys(dataset_name, ntopics)
		
		list("title" = noexcludes.dt[pubnum, c("Title", "Pub.number", tagnames), with=F],
			"doc_tops" = doc_tops[pubnum, paste0(c("top","wgt"), rep(1:5, each=2)), with=F],
			"keys" = topic_keys[as.numeric(doc_tops[pubnum, paste0("top", 1:5), with=F])],
			"abstract" = noexcludes.dt[pubnum, c("KEYWORDS", "ABSTRACT"), with=F]		
			)
}

# Browse through the top topics and their top-proportioned dissertations
top_topic_browser <- function(start.rank	 = 1, 				# assuming we're looping, start where?
								topic		 = NULL,			# alternately, browse one specified topic
								dataset_name = "consorts",
								ntopics		 = 55, 
								cutoff		 = get("ntopics")	# if lots of topics, where to stop?
	){
	# get packages in case we've just restarted R
	require(data.table)
	
	# load the data from the functions above
	doc_topics.dt <- get.doc.composition(dataset_name, ntopics)
	topic_keys.dt <- get.topickeys(dataset_name, ntopics)
	grids <- get.doctopic.grid(dataset_name, ntopics)
		colsums <- grids$colsums
		colsums.sort <- grids$colsums.sort
		outputfile <- grids$outputfile
	rm(grids)
	
	# List the keys for the top N topics, where N = cutoff
	len <- min(length(colsums)-1, cutoff)
	ind <- as.integer(names(colsums.sort)[2:(len+1)])		# list of topics by rank; skip Pub.num

	# If we specified a topic, show just that topic and exit.
	if (! is.null(topic)) {
		topic.num <- topic
		
		# find and display topic rank
		topic.rank <- which(ind %in% topic.num)
		if (remake_figs) { 	print(paste0("Topic of rank ", topic.rank, ":")) } 
		else { message("\nTopic of rank ", topic.rank, ":\n") }
			
		# get Pub.numbers for dissertations with the max proportion of that topic
		row.ind <- order(outputfile[, which(names(outputfile)==topic.num)], decreasing=TRUE)[1:5]
		diss.ind <- outputfile[row.ind, "Pub.number"]

		print(topic_keys.dt[topic.num])
		
		topdocs <- noexcludes.dt[as.character(diss.ind), c("Pub.number", "Title", tagnames), with=F]
		print(topdocs)
		if (!remake_figs) { a <- readline("Press <enter> for more detail on these docs, or S to skip to the next topic\n") } else { a <- ""}

		while (tolower(a) != "s") {
			for(i in topdocs$Pub.number) {
				print(get.topics4doc(i, dataset_name, ntopics))
				if (!remake_figs) { a <- readline("Press <enter> for next doc, D for more details, or S to skip to the next topic\n") } else { a <- ""}
				if (tolower(a) == "s") { break }
				else if (tolower(a) == "d") { 
					print(noexcludes.dt[i]) 
					a <- readline("Press <enter> for next doc or S to skip to the next topic\n")
				}
			}
			a <- "s"
		}

		
	} else {
		# Loop through the top topics and their top-proportioned dissertations,  
		# optionally showing abstracts and top 5 topics for each of those dissertations
	message("Top ", cutoff, " topics:")
	print(topic_keys.dt[ind])							# top words for each topic
		
	for (i in start.rank:len) {
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
				print(get.topics4doc(i))
				if (!remake_figs) { a <- readline("Press <enter> for next doc, D for more details, or S to skip to the next topic\n") } else { a <- ""}
				if (tolower(a) == "s") { break }
				else if (tolower(a) == "u") {i <- i-1}
				else if (tolower(a) == "d") { 
					print(noexcludes.dt[i]) 
					a <- readline("Press <enter> for next doc or S to skip to the next topic\n")
				}
			}
			a <- "s"
		}
	}}
}

## Run the big function above
# if (remake_figs) { 
	# filename <- paste0(imageloc, "top topics - ", dataset_name, ", K", ntopics, ".txt")
	# readline(paste("About to capture browser output as", filename,"- <enter> to continue or <esc> to abort."))
	# capture.output(top_topic_browser(), file=filename)
# } else {
	# top_topic_browser()
# }



# source("frameToD3.R")
# dt <- as.data.table(outputfile)
# groupVars <- c("Pub.number")	# Ben: this is the name of that first (ID) column. replace accordingly.
# dataVars <- colnames(dt)[!colnames(dt) %in% groupVars]	# Ben: any column that's not an ID is a datapoint 
# filename <- paste0(malletloc, "/", dataset_name, "k", ntopics, "_clusters.json")
# frameToJSON(outputfile,groupVars,dataVars,outfile=filename)
