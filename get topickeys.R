## get topickeys.R
#  GOAL: Given a dataset and number of topics, read in the top words for each topic in that topic model.

get.topickeys <- function(dataset_name="consorts", ntopics=55, iter_index="") {
	# get packages in case we've just restarted R
	require(data.table)
	
	filename <- file.path(tmloc, paste0(dataset_name, "k", ntopics, "_keys_", iter_index, ".txt"))
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

