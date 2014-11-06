# get topic labels.R
# GOAL: read in topic labels, which you've composed elsewhere using 'top docs per topic.R'

get_topic_labels <- function(dataset_name="consorts", ntopics=55) {
	filename <- paste0(imageloc, "topic labeling - ", dataset_name, ", K", ntopics, ".csv")
	topic.labels.dt <- tryCatch(
		data.table(read.csv(filename), key="Topic"), 
		error = function(e) {
	  		message("File not found; using top words instead.")
	  		keys <- get.topickeys(dataset_name, ntopics)
	  		outfile <- paste0(webloc, "/", dataset_name, "k", ntopics, "_clusters_topwords.json")	
	  		return(data.table(Topic=1:ntopics, Label=keys$top_words))
	  	},
	  	finally = { message("done.") }
	)
}

