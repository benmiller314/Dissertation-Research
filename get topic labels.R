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

	    # now that we have ranks, let's sort again by topic number for ease of merge
	    ranked_topics <- ranked_topics[order(ranked_topics$Topic),]

	    # merge
	    head(topic.labels.dt)
	    # NB: the := syntax updates the source data.table, no need to assign
	    topic.labels.dt[, Pct.Contrib := ranked_topics$Pct.Contrib]
	    topic.labels.dt[, Rank := ranked_topics$Rank]

	    head(topic.labels.dt[order(Rank)])
	}

	if(!is.null(bad.topics)) {
	    index <- setdiff(topic.labels.dt$Topic, bad.topics)
	    topic.labels.dt <- topic.labels.dt[index]
	}

	if(remake_figs) {
	    if(!exists("build_plot_title", mode="function")) {
	        source(file="build_plot_title.R")
	    }
	    outfile_slug <- build_plot_title(dataset_name=dataset_name,
	                                   ntopics=ntopics,
	                                   iter_index=iter_index,
	                                   subset_name=subset_name,
	                                   bad.topics=bad.topics,
	                                   whatitis="topic-labels",
	                                   for.filename=T)
	    outfile <- file.path(imageloc, paste0(outfile_slug, ".csv"))
      if(file.exists(outfile)) {
          overwrite <- readline(paste("In 'get_topic_labels()': destination file already exists: \n", outfile, "\n",
                                      "Overwrite (O)? New file (N)? Skip (S)? (o/n/s) > "))
          if(tolower(overwrite) == "n") {
              outfile <- readline(paste("Enter new filename, including directory. ",
                                        "(Default directory is ", imageloc, ")> "))
          } else if (tolower(overwrite) == "s") {
              return(topic.labels.dt)
          } else if (tolower(overwrite) != "o") {
              outfile_slug <- paste0(outfile_slug, "+1")
              outfile <- file.path(imageloc, paste0(outfile_slug, ".csv"))
              warning(paste("Answer not understood. Saving with new filename: \n", outfile))
          }
      }
	    write.csv(topic.labels.dt, outfile, row.names = F)
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
