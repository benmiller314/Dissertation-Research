#############################################################################
# get topic labels.R
# 
# GOAL: 
# read in topic labels, which you've composed elsewhere using 'top docs per
# topic.R'
#####

get_topic_labels <- function(dataset_name="consorts", ntopics=55, subset_name=NULL, iter_index="") 
{
	filename <- paste0(imageloc, "topic labeling - ", dataset_name, ", K",
						 ntopics, iter_index, ".csv")
	
	# get the labels derived from the larger (or only) set
	topic.labels.dt <- tryCatch(
		data.table(read.csv(filename), key="Topic"), 
		error = function(e) {
	  		message("File not found; using top words instead.")
	  		keys <- get.topickeys(dataset_name, ntopics)
	  		outfile <- paste0(webloc, "/", dataset_name, "k", ntopics,
	  						 "_clusters_topwords", iter_index, ".json")	
	  		return(data.table(Topic = 1:ntopics, 
	  						  Label = keys$top_words))
	  	},
	  	finally = { message("done.") }
	)
	
	# update the percent-contributed and rankings for the subset, as determined by 
	# get.doctopic.grid
	if(! is.null(subset_name)) {
	    filename <- paste0(imageloc, dataset_name, "k", ntopics, "--", subset_name, 
	                       "_topic-ranks", iter_index, ".csv")
	    ranked_topics <- read.csv(filename)
	    names(ranked_topics) <- c("Topic", "Pct.Contrib")
	    ranked_topics$Rank <- 1:nrow(ranked_topics)
	    head(ranked_topics)
	    
	    head(topic.labels.dt)
	    topic.labels.dt[ranked_topics$Topic]$Pct.Contrib <- ranked_topics$Pct.Contrib
	    topic.labels.dt[ranked_topics$Topic]$Rank <- ranked_topics$Rank
	}
	
	return(topic.labels.dt)
	
}

