##  get doctopic grid.R
#	GOAL: Read in a table of documents with all topic proportions for each.
#	Return both this table and total topic proportions across documents.
#
#	STRATEGY:
#	Edit the reshapeMallet.py script (in TextWrangler) to update filenames, 
#   then run it here and read in the output.


get.doctopic.grid <- function(dataset_name="consorts", ntopics=55, doplot=F) {
	# get packages in case we've just restarted R
	require(data.table)
	
	filename <- paste0(malletloc, "/", dataset_name, "k", ntopics, "_doc-all-topics.txt")
	scope <- paste("cd", shQuote(sourceloc), "; cd 'Shell scripts and commands' ; ls ", filename)
	if (system(scope)) {		# runs only if file not found, which returns a non-zero error value
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
	
	outputfile.dt <- as.data.table(outputfile)
	head(outputfile.dt)

	## Find overall top topics
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
		# barplot(colsums.sort[2:length(colsums)], xlab="topic numbers (arbitrary)", ylab="sum of contributions", xaxt="n", xpd=F)
		text(x=1+2:length(colsums), y=1+colsums.sort[2:length(colsums)], labels=names(colsums.sort[2:length(colsums)]))
	}
	
	# Return with the goods
	list("colsums" = colsums,
		 "colsums.sort" = colsums.sort,
		 "colsums.sort.pct" = colsums.sort.pct,
		 "outputfile" = outputfile
		 )

}	# end of get.doctopic.grid()
