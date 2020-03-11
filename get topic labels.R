#############################################################################
# get topic labels.R
# 
# GOAL: 
# read in topic labels, which you've composed elsewhere using 'top docs per
# topic.R'
#####

if(!exists("get.topickeys", mode="function")) { 
    source(file="get topickeys.R") 
}

get_topic_labels <- function(dataset_name="consorts", 
                             ntopics=55, 
                             subset_name=NULL, 
                             iter_index="",
                             bad.topics=NULL,
                             newnames=F     # where in the MALLET output filename does iter_index appear?
                                            # Set T if it's with the model, F if last in filename.
                                            # Gets used only if get.topickeys() is needed.
) {
# 	# original filename structure
#     filename <- file.path(imageloc, paste0("topic labeling - ", dataset_name, ", K",
# 						 ntopics, iter_index, ".csv"))
    # better filename structure
    filename <- file.path(imageloc, paste0("topic-labeling--", dataset_name, "k",
                         ntopics, "_", iter_index, ".csv"))
	
	# get the labels derived from the larger (or only) set
	topic.labels.dt <- tryCatch(
		data.table(read.csv(filename), key="Topic"), 
		error = function(e) {
	  		message("File not found; using top words instead.")
	  		keys <- get.topickeys(dataset_name=dataset_name, 
	  		                      ntopics=ntopics, 
	  		                      iter_index=iter_index, 
	  		                      newnames=newnames)
	  		outfile <- file.path(webloc, paste0(dataset_name, "k", ntopics,
	  						 "_clusters_topwords", iter_index, ".json"))	
	  		return(data.table(Topic = 1:ntopics, 
	  						  Label = keys$top_words))
	  	},
	  	finally = { message("done.") }
	)
	
	# update the percent-contributed and rankings for the subset, as determined by 
	# get.doctopic.grid
	if(! is.null(subset_name)) {
	    # original filename structure
	    filename <- file.path(imageloc, paste0(dataset_name, "k", ntopics, "--", subset_name, 
	                       "_topic-ranks", iter_index, ".csv"))
	    # better filename structure
	    filename <- file.path(imageloc, paste0("topic-labeling--", dataset_name, "k",
	                                           ntopics, "_", iter_index, "--", subset_name, ".csv"))
	    ranked_topics <- tryCatch(
	        read.csv(filename),
	        error = function(e) {
	            message("get topic labels.R: Can't find file to update topic ranks for subset:\n", filename)
	            message("defaulting to values for superset: ", dataset_name)
	            return(topic.labels.dt)
	        },
	        finally = { message("Updated topic ranks and percentages for subset: ", subset_name) }
	    )
	    if (! "Pct.Contrib" %in% names(ranked_topics)) {
	        names(ranked_topics) <- c("Topic", "Pct.Contrib")
	        ranked_topics$Rank <- 1:nrow(ranked_topics)
	        head(ranked_topics)    
	    }
	   
	    head(topic.labels.dt)
	    topic.labels.dt[ranked_topics$Topic]$Pct.Contrib <- ranked_topics$Pct.Contrib
	    topic.labels.dt[ranked_topics$Topic]$Rank <- ranked_topics$Rank
	if(!is.null(bad.topics)) {
	    index <- setdiff(topic.labels.dt$Topic, bad.topics)
	    topic.labels.dt <- topic.labels.dt[index]
	}
	}
	
	return(topic.labels.dt)
	
}

if(autorun) {
    remake_figs
    topic_labels <- get_topic_labels("noexcludes2001_2015", 
                                     ntopics=50, 
                                     iter_index=1, 
                                     newnames=T)
    topic_labels <- get_topic_labels("noexcludes2001_2015", 
                                     ntopics=50, 
                                     iter_index=1, 
                                     subset_name="knownprograms2001_2015", 
                                     newnames=T,
                                     bad.topics=c("3", "8", "12", "15", "30", "34", "36", "47", "50"))
}
