## Goal: Find out the correlation of topic strengths to topic ranks within each document, **for a single topic**
#  i.e. how much of the document is the top topic? how much is the second? and so on,
#  aggregated over all documents, as a scatter plot of contribution (y-axis) vs. topic rank (x-axis).
#  Rationale: I want to know whether some topics with high overall rank are secretly low-but-consistent across lots of docs


strength_v_rank <- function(my.topic, 
							dataset_name = "consorts", 
							ntopics		 = 55,
							bad.topics	 = NULL
							) 
{
	# Exclude non-content-bearing topics
	if(is.null(bad.topics) && dataset_name="consorts" && ntopics=55) {
		bad.topics <- c("4", "47", "22", "2", "24", 	# bad OCR or ProQuest boilerplate
					"13", "50")						# language markers (Italian, Spanish)
	}
					
	if(my.topic %in% bad.topics) { warning(paste("Topic", my.topic, "has been identified as non-content-bearing")) }	
	
	require(data.table)	
	dataset <- get(dataset_name)
	
	# get all topics by document
	if(!exists("get.doctopic.grid", mode="function")) { source("get doctopic grid.R") }
	grid <- data.table(get.doctopic.grid(dataset_name, ntopics)$outputfile)
	# str(grid)
	# head(grid)


	grid <- grid[, !(names(grid) %in% c(bad.topics, "Pub.number")), with=F]
	# head(grid)

	# find rank of my.topic within one document
	rankit <- function(row) {
		o <- order(row, decreasing=TRUE)
		ranked.topics <- names(row[o])
		my.rank <- which(ranked.topics == my.topic)
		return(my.rank)
	}
	my.ranks <- apply(grid, 1, FUN=rankit)		# apply rankit function across rows of grid
											# (this will be our set of x-values, in order of documents)
		# head(my.ranks)

	
	# for y-values, just read down the column of our topic
	my.contribs <- grid[, names(grid) %in% my.topic, with=F][[1]]		# have to extract values from list with [[subset]]
		# str(my.contribs)
		# head(my.contribs)
		
	# set up the plot
	maintitle <- paste("Topic Contribution by Topic Rank")
	subtitle <- paste(dataset_name, ntopics, "topics")
	ymax <- max(grid)
	
		# Use get_topic_labels() to retrieve ranks
		if(!exists("get_topic_labels", mode="function")) { source(file="get topic labels.R") }
		topic.labels.dt <- get_topic_labels()
			# head(topic.labels.dt)
		overall.rank <- topic.labels.dt[Topic == my.topic, Rank]
		topic.label <- topic.labels.dt[Topic == my.topic, Label]
	legendtext <- paste0("topic: ", my.topic, "\n'",topic.label, "'\noverall rank: ", overall.rank)
	
	# plot it
	if(remake_figs) { filename <- paste0(imageloc, maintitle, " ", subtitle, ".pdf"); pdf(filename) }
	plot(my.ranks, 											# x values
		 my.contribs, 										# y values
		 main=maintitle, 									# title of figure
		 xlab="Topic Rank within Document", 				# x-axis label
		 ylab="Topic Contribution within Document", 		# y-axis label
		 ylim=c(0, ymax)									# use the same y-axis for better comparison between plots
	)
	mtext(subtitle)
	legend("topright", legendtext, bty="n")
	if(remake_figs) { dev.off() }
	
	invisible(list(ranks = my.ranks,
				contribs = my.contribs)
	)
	
}

# Okay, now I want to run that function on multiple topics

if(autorun) {
	# Use get_topic_labels() to retrieve ranks
	if(!exists("get_topic_labels", mode="function")) { source(file="get topic labels.R") }
	topic.labels.dt <- get_topic_labels()
	head(topic.labels.dt)
	
	# Exclude non-content-bearing topics
	bad.topics <- c("4", "47", "22", "2", "24", "13", "50")
	topic.labels.dt <- topic.labels.dt[!(Topic %in% bad.topics)]
	setkey(topic.labels.dt, Rank)
	topics.by.rank <- head(topic.labels.dt[, Topic], 10)
	
	lapply(topics.by.rank, strength_v_rank)
	strength_v_rank(my.topic=14)
	
	# tryCatch(lapply(c(10, 4, 15), strength_v_rank), err=function(e) e, finally="done")
	# strength_v_rank(4)
	
	topic.labels.dt[Topic==14, Label]
}
