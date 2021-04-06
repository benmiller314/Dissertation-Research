#############################################################################
# variation of topic proportions.R
#
# Goal: Find out the curve of topic strengths within each document, i.e. how
# much of the document is the top topic? how much is the second? and so on,
# aggregated over all documents, as a boxplot of contribution (y-axis) sorted
# by topic rank (x-axis).
#
# Rationale: I want to know at what level to cut off "cotopics": what's a
# realistic scenario?
#
# Functions:
# 
# topic.proportions(dataset_name, ntopics, iter_index, ...): the boxplot 
#                   described above, showing distribution of topic weights.
# find.doc.by.topic.proportion(topic.weights, topic_rank, value, ...): given a
#                   distribution of topic weights, find a representative diss
#                   at high, low, or median values. Useful for demonstrating
#                   what single-topic focused or multi-topic disses look like.
# top_topic_histogram(topic.weights, topic_rank, ...): Histogram of values 
#                   for the nth-ranked topic; a zoom-in on topic.proportions().
# get_top_topics(dataset_name, ntopics, iter_index, ...): Find all the top topics 
#                   while preserving identity, so we can graph subsets
#                   and compare them to the corpus-wide distribution.
# top_topics_comparison(mytopics, top_topics, ...): Compare top-topic distribution 
#                   for one topic as compared to the rest of the dataset. 
#                   Useful for testing topical independence (see: writing centers)
# top_topic_assists(mytopics, cutoff, sort_by, ...): For dissertations with 
#                   a given rank1 topic, what are the other top-ranked topics 
#                   they most pair with? (See also cotopics.R)
# top_topic_assists2(mytopic, top_topics, topic_depth, n_per_rank, ...): now more
#                   specific results at rank2, 3, etc. (TO DO: graph as alluvial plot)
# top_topic_combos(topic_depth, ...): Topic-triples at the top, regardless of order.
# 
#####

topic.proportions <- function(dataset_name = "noexcludes2001_2015",
						  ntopics = 50,
						  iter_index = "1",
						  subset_name = "knownprograms2001_2015",
						  newnames = F,         # where in the MALLET output filename does iter_index appear?
													 # set T if it's with the model, F if last in filename.
													 # Gets passed into get.doctopic.grid.

						  # NB: if default dataset and ntopics are used,
						  # we'll use default bad.topics
						  bad.topics = NULL,

						  # Draw notch in barplot to check for overlap?
						  use.notch = FALSE,

						  # Use topic browser for outlier dissertations?
						  explore.outliers = FALSE,
						  explore.lowliers = FALSE,
						  lowlier.rank = 2,

						  # should we print everything to screen as we go?
						  verbose = FALSE,

						  # should we draw a line at the lower hinge of rank 2?
						  markcutoff = FALSE,

						  howmany = 3,    # how far into the ranks should we go?
						  
						  # if we have a doc-topic grid, pass for a speed boost
						  dtgrid = NULL
){
	require(data.table)
    
    if(is.null(dtgrid)) {
	    if(!exists("get.doctopic.grid", mode="function")) {
		    source("get doctopic grid.R")
	    }
	    dtgrid <- get.doctopic.grid(dataset_name = dataset_name,
	                          ntopics = ntopics,
	                          iter_index = iter_index,
	                          subset_name = subset_name,
							  newnames = newnames)$outputfile.dt
	    # str(grid)
	    head(dtgrid)
    }

	# Exclude non-content-bearing topics
	# If none are set in parameters, use defaults:
	if(is.null(bad.topics) && dataset_name=="consorts" && ntopics==55) {
		bad.topics <- c("4", "47", "22", "2", "24", "50", "13")
	}


	grid.clean <- dtgrid[, setdiff(names(dtgrid), c(bad.topics, "Pub.number")), with=F]
	if(verbose) {
    	print(head(grid.clean))
	}


	# decreasing sort across each row -- ignore column (i.e. topic) names
	grid.sorted <- t(apply(grid.clean, 1, FUN=function(x) {
								sort(x, decreasing=T)
							}))

		# each row is a dissertation; we lose topic numbers, but now column 1
		# is the weight of the top-ranked topic for that row, column 2 the
		# weight of the 2nd-ranked topic, and so on. Let's look at the 10
		# top-ranked topics for every dissertation.
	if(verbose) {
    	print(head(grid.sorted[, 1:10]))
	}

	# start empty, build up
	stats <- data.frame()
	for (i in 1:howmany) {

		# message(paste0("Stats for ", i,
		#					"-ranked topic within dissertations:"))
		stats <- rbind(stats, boxplot.stats(grid.sorted[, i])$stats)
	}

	# lower whisker, lower ???hinge???, median, upper ???hinge???, upper whisker
	names(stats) <- c("lower", "Lhinge", "median", "Uhinge", "upper")
	stats <- cbind("rank of topic within diss"=seq_len(howmany), stats)

    sum(stats$median[1:3]) # maybe a good cutoff for cumulative cluster reach?
	# we'll return the stats data.frame later.

    ## For a second way of exploring typical behavior, sort rows by top weight.
    #  Save the Pub.number first, so you can still look up information on each diss
    #  (i.e. don't assume the rows here are the same as in the original doctopic grid)
    grid.sorted2 <- data.frame(Pub.number=dtgrid$Pub.number, grid.sorted)
    grid.sorted2 <- grid.sorted2[order(grid.sorted2$X1, decreasing=T), ]



	## Time to make the plot
	if(!exists("build_plot_title", mode="function")) {
	    source(file="build_plot_title.R")
	}

	bigtitle <- build_plot_title(dataset_name=dataset_name, subset_name=subset_name,
	                         ntopics=ntopics, iter_index=iter_index,
	                         bad.topics=bad.topics,
	                         whatitis="Variation of Topic Proportions")
	maintitle <- paste0(strsplit(bigtitle, ",")[[1]][1], ", Top 10 Topics per Document")
	subtitle <- strsplit(bigtitle, ",")[[1]][2]
	subtitle <- paste0(subtitle, "   N=", nrow(dtgrid))

	if(remake_figs) {
	    filename <- file.path(imageloc, paste0(bigtitle, ".pdf"))
	    pdf(filename)
	}
		boxplot(grid.sorted[, 1:10],
				cex.axis = 1,
				las = 1,
				main = maintitle,
				sub = subtitle,
				xlab = "Topic Rank Within Document",
				ylab = "Portion of Document (scaled to 1)",
				yaxp = c(0, 1, 10),
				notch = use.notch
		)

		if(markcutoff) {
		## mark line covering top three quartiles for the 2nd-ranked topic,
		## but only the top quartile for 3rd
		    # abline(h=0.11, col="#99FF99")
		    abline(h=stats[2, "Lhinge"], col="#99FF99")
		    outside_legend("right",
		                   legend=paste("Lower hinge, 2nd-rank:",
		                                round(stats[2, "Lhinge"],2)),
                            col="#99FF99",
                            lwd=2)
		}

	if(remake_figs) {
		dev.off()
	}

	## Optionally extract top-topic outliers for further examination
	if(explore.outliers) {
		upper.whisker <- boxplot.stats(grid.sorted[, 1])$stats[5]

		# just look at #1 topic
		outliers.index <- which(grid.sorted[, 1] > upper.whisker)
		outliers <- cbind(dtgrid[outliers.index, "Pub.number", with=F],
						  grid.sorted[outliers.index, 1:10])
		outliers <- outliers[order(outliers$V1, decreasing=T), ]

		# boxplot(outliers[, 2:ncol(outliers)])

		#####
		# I have a hypothesis that these are mostly language-based topics.
		# Let's look at the top topics represented here. STRATEGY:
		# 1. For each Pub.number, get top-ranked topic number by finding the
		#    max within that row of `grid`.
		# 2. Make a table of these topic numbers.
		# 3. Retrieve the labels for each topic in the table.

		hightopics <- c()		# start empty and build up
		highvalues <- c()		# what are those high percent-of-text values?

		for (i in outliers$Pub.number) {
			row <- dtgrid[which(dtgrid$Pub.number==i), 2:ncol(dtgrid), with=F]
			mytopic <- which(row == max(row))
			hightopics <- c(hightopics, mytopic)
			highvalues <- c(highvalues, max(row))
		}

		# count 'em up
		hightopics.t <- table(hightopics)

		# get labels
		if(!exists("get_topic_labels", mode="function")) {
			source(file="get topic labels.R")
		}
		highlabels <- get_topic_labels(dataset_name, ntopics, subset_name, iter_index, newnames=newnames)
		highlabels.t <- highlabels[unique(hightopics), Label, key=Topic]

		# merge in the counts
		highlabels.t[, "Outlier Count"] <- hightopics.t

		# merge in the values
		b <- aggregate(data.frame(hightopics, highvalues), by=list(hightopics),
						 FUN=c)
		highlabels.t <- highlabels.t[b, ][,hightopics:=NULL]

		# sort by descending outlier frequency
		highlabels.t <- highlabels.t[order(hightopics.t, decreasing=T), ]

		# report back
		message("Upper outliers for top-ranked topics:")
		print(highlabels.t)
		message(paste("Total outliers for top-ranked topic:",
						sum(highlabels.t[, "Outlier Count", with=F])))

		# Okay,	my hypothesis is false! All sorts of topics here.
		# Interesting. Still, I may want to remove the dissertations with
		# top-ranked language topics beforehand, since they do tend to
		# dominate their dissertations.

		## Browse more details of these outlier dissertations

		if (!verbose) {
		    a <- "s"
		} else if (!remake_figs) {
		    if(!exists("get.topics4doc", mode="function")) {
		        source(file="top docs per topic.R")
		    }
			a <- readline(paste("Press <enter> for more detail on these ",
								"docs, or S to skip to the end\n"))
		} else {
			a <- ""
		}

		while (tolower(a) != "s") {
			for(i in outliers$Pub.number) {
				print(get.topics4doc(i, dataset_name, ntopics,
									 showlabels=TRUE))
				if (!remake_figs) {
					a <- readline(paste("Press <enter> for next doc,",
										"D for more details, or ",
										"S to skip to the end\n"))
				} else {
					a <- ""
				}

				if (tolower(a) == "s") {
					break
				} else if (tolower(a) == "d") {
					print(noexcludes.dt[i])
					a <- readline(paste("Press <enter> for next doc",
										"or S to skip to the next topic\n"))
				}
			}
			a <- "s"
		}

	# Browse low-liers (though my hypothesis there is that they're
	# mostly dissertations dominated by bad.topics; in which case the thing
	# TO DO is to eliminate those dissertations entirely from the topic
	# modeling dataset beforehand, and run the model again.)

	} # end if(explore.outliers)

	if(explore.lowliers) {

	        if(!( (lowlier.rank < 1) || (lowlier.rank > howmany) )) {

    	        lower.whisker <- stats[lowlier.rank, "lower"]

    	        lowliers.index <- which(grid.sorted2[, paste0("X", lowlier.rank)] < lower.whisker)
    	        lowliers <- grid.sorted2[lowliers.index, 1:11]

    	        # boxplot(lowliers[, 2:ncol(lowliers)])


    	        lowtopics <- c()		# start empty and build up to find dominant topics
    	        lowvalues <- c()		# what are the high percent-of-text values?

    	        for (i in lowliers$Pub.number) {
    	            row <- dtgrid[which(dtgrid$Pub.number==i), 2:ncol(dtgrid), with=F]
    	            mytopic <- which(row == max(row))
    	            lowtopics <- c(lowtopics, mytopic)
    	            lowvalues <- c(lowvalues, max(row))
    	        }

    	        # count 'em up
    	        lowtopics.t <- table(lowtopics)

    	        # get labels
    	        if(!exists("get_topic_labels", mode="function")) {
    	            source(file="get topic labels.R")
    	        }
    	        lowlabels <- get_topic_labels(dataset_name, ntopics, subset_name, iter_index)
    	        lowlabels.t <- lowlabels[unique(lowtopics), Label, key=Topic]

    	        # merge in the counts
    	        lowlabels.t[, "Outlier Count"] <- lowtopics.t

    	        # merge in the values
    	        b <- aggregate(data.frame(lowtopics, lowvalues), by=list(lowtopics),
    	                       FUN=c)
    	        lowlabels.t <- lowlabels.t[b, ][,lowtopics:=NULL]

    	        # sort by descending outlier frequency
    	        lowlabels.t <- lowlabels.t[order(lowtopics.t, decreasing=T), ]

    	        # report back
    	        message("Top-ranked topics of disses with lower outliers for topic ranked ", lowlier.rank, ":")
    	        print(lowlabels.t)
    	        message(paste("Total 'lowliers' for topic ranked", lowlier.rank, ":",
    	                      sum(lowlabels.t[, "Outlier Count", with=F])))

    	        # Yup, it's bad.topics for sure.

    	        message("Pub.numbers to re-OCR or remove from the model training set:")
    	        print(lowliers$Pub.number)
	        } else {
	            warning("Selected `lowlier.rank` parameter is out of bounds. \n",
	                    "Choose a value between 1 and `howmany` parameter (default = 3).")
	        }

    } # end if(explore.lowliers)


	message(paste("Stats for contributions of topics at various ranks",
					"within dissertations:"))
	print(stats)

	to.return <- list(stats = stats,
	                  weightsbydoc = grid.sorted2)

	if(explore.outliers) {
	    to.return$outliers <- outliers
	    to.return$outliers.table <- highlabels.t
	}

	if(explore.lowliers) {
		to.return$lowliers <- lowliers
		to.return$lowliers.table <- lowlabels.t
	}


	return(to.return)
}

# Select documents for analysis based on their topic-weight distribution
find.doc.by.topic.proportion <- function(topic_weights,   # as produced above
                                         topic_rank = 1,  # select by which-ranked topic?
                                         value="median",  # where in that rank's distribution?
                                         dataset_name = "noexcludes2001_2015",
                                         ntopics = 50,
                                         iter_index = 1,
                                         subset_name = "knownprograms2001_2015",
                                         offset = 0,     # want to explore adjacent disses?
                                         howmanytopics = 5,
                                         bad.topics = NULL

) {
    if(is.null(topic_weights)) {
        topic_weights <- topic.proportions(dataset_name = dataset_name,
                                           ntopics = ntopics,
                                           iter_index = iter_index,
                                           subset_name = subset_name,
                                           bad.topics = bad.topics)
    }

    wbd <- topic_weights$weightsbydoc
    mystats <- topic_weights$stats

    # Sort topic_weights by topic_rank of interest, for more effective offsets
    # NB mystats doesn't need reordering: it already summarizes each topic rank
    myorder <- order(wbd[, paste0("X", topic_rank)], decreasing = T)
    wbd <- wbd[myorder, ]


    if (value %in% names(mystats)) {
        if(topic_rank %in% seq_len(nrow(mystats))) {
            mystat <- mystats[topic_rank, value]
        } else {
            stop("find.doc.by.topic.proportion: topic_rank, ", topic_rank, ", out of bounds")
        }
    }

    myindex <- which.min(abs(wbd[, paste0("X", topic_rank)] - mystat))
    if ((myindex + offset) < 0) {
        warning("`find.doc.by.topic.proportion`#349: Offset would produce index outside of range; \n",
                "using minimum index")
        myindex <- 1
    } else if ((myindex + offset) > nrow(wbd)) {
        warning("`find.doc.by.topic.proportion`#349: Offset would produce index outside of range; \n",
                "using maximum index")
        myindex <- nrow(wbd)
    } else {
        myindex <- myindex + offset
    }

    mypub <- wbd[myindex, "Pub.number"]

    if(!exists("get.topics4doc", mode="function")) {
        source(file="top docs per topic.R")
    }

    get.topics4doc(pubnum = mypub,
                   dataset_name = dataset_name,
                   ntopics = ntopics,
                   iter_index = iter_index,
                   subset_name = subset_name,
                   showlabels = T,
                   howmany = howmanytopics,
                   columns = c("Pub.number", "Author", "Title", "School", "Department", "Year", "Pages", "Link"))

    # return(mypub)
}


## Histogram of weights of just the nth-ranked topic
top_topic_histogram <- function(topic_weights = NULL,   # as produced above
                                topic_rank = 1,  # top-ranked topic? 2nd?
                                dataset_name = "noexcludes2001_2015",
                                ntopics = 50,
                                iter_index = 1,
                                subset_name = "knownprograms2001_2015",
                                bad.topics = NULL,
                                useboxstats = FALSE)
{
    if(is.null(topic_weights)) {
        topic_weights <- topic.proportions(dataset_name = dataset_name,
                                           ntopics = ntopics,
                                           iter_index = iter_index,
                                           subset_name = subset_name,
                                           bad.topics = bad.topics)
    }

    wbd <- topic_weights$weightsbydoc
    mystats <- topic_weights$stats

    myrank <- wbd[, paste0("X", topic_rank)]

    if(!exists("build_plot_title", mode="function")) {
        source(file="build_plot_title.R")
    }

    bigtitle <- build_plot_title(dataset_name=dataset_name, subset_name=subset_name,
                                 ntopics=ntopics, iter_index=iter_index,
                                 bad.topics=bad.topics,
                                 whatitis=paste("Proportions of Text from Topic Ranked",
                                                topic_rank, "within Dissertation")
    )

    subtitle <- paste("N = ", nrow(wbd))

    if(remake_figs) {
        outfile <- file.path(imageloc, paste0(bigtitle, ".pdf"))
        pdf(outfile)
    }

    if(useboxstats) {
        breaks <- as.numeric(mystats[topic_rank, !(names(mystats) %in% c("rank of topic within diss"))])
        breaks <- c(0, breaks, 1)
    } else {
        breaks <- seq(0, 1, 0.05)
    }

    hist(myrank, breaks=breaks, xlab="Portion of Document (scaled to 1)",
         main=strsplit(bigtitle, ",")[[1]][1], sub=subtitle)

    if(remake_figs) {
        dev.off()
    }

}


# Find all the top topics while preserving identity, so we can graph subsets
# and compare them to the corpus-wide distribution
get_top_topics <- function(dataset_name = "noexcludes2001_2015",
                           ntopics = 50,
                           iter_index = 1,
                           subset_name = "knownprograms2001_2015",
                           newnames = F,
                           bad.topics = c("3", "8", "12", "15", "30", "34", "36", "47", "50"),
                           grid = NULL,   # pass for a speed boost
                           mytopic = NULL # if NULL, return all;
                                          # otherwise, limit to topic of interest
){
    if (is.null(grid)) {
        grid <- get.doctopic.grid(dataset_name=dataset_name, ntopics=ntopics,
                                      iter_index=iter_index, newnames=newnames)$outputfile.dt
    }

    grid <- na.omit(grid)


    # helper function: find top topic for a specific publication
    get_top_topic <- function(pubnum,
                              dataset_name = "noexcludes2001_2015",
                              ntopics = 50,
                              iter_index = 1,
                              subset_name = "knownprograms2001_2015",
                              newnames = F,
                              grid = NULL
    ) {
        if(!exists("get.topics4doc")) {
            source(file="top docs per topic.R")
        }

        top <- get.topics4doc(pubnum = pubnum,
                              dataset_name = dataset_name,
                              ntopics = ntopics,
                              iter_index = iter_index,
                              subset_name = subset_name,
                              grid = grid,
                              howmany = 1)$keys

        return(top)
    }   # end of helper function


    if(!is.null(subset_name)) {
        pubs <- get(subset_name)$Pub.number
    } else {
        pubs <- get(dataset_name)$Pub.number
    }

    # start empty, build up
    tops <- data.frame(topic=numeric(),
                       weight=numeric(),
                       alpha=numeric(),
                       top_words=character())
    bad.count <- 0
    bad.on.top <- c() # list of Pub.numbers for dissertations with top topic in bad.topics

    for (i in pubs) {    # use helper function on each pub
        row <- get_top_topic(pubnum = i,
                             dataset_name = dataset_name,
                             ntopics = ntopics,
                             iter_index = iter_index,
                             subset_name = subset_name,
                             grid=grid)
        if(row$topic %in% bad.topics) {
            bad.count <- bad.count + 1
            bad.on.top <- c(bad.on.top, i)
        }

        tops <- rbind(tops, row)

    }


    # preserve the Pub.number for verification / further inspection
    top_topics <- data.table(Pub.number=pubs, tops)
    setkey(top_topics, topic)

    # generally speaking, sort by topic; within topics, show largest proportion first
    setorder(top_topics, topic, -weight)

    # if we have a topic of interest, just return that
    if(! is.null(mytopic)) {
        top_topics <- subset(top_topics, topic==mytopic)
    }

    setkey(top_topics, "topic")

    if(bad.count > 0) {
        warning("Found ", bad.count, " dissertations with top topic in bad.topics.")
        if(exists("bad.on.top", envir=baseenv()) ) {
            a <- readline("Export list of Pub.numbers with top topic in bad.topics (as `bad.on.top`)? Y/N")
            if (substr(tolower(a), 1, 1) == 'y') {
                assign("bad.on.top", value = bad.on.top, envir = baseenv())
                message("bad.on.top exported")
            } else {
                message("bad.on.top not exported")
            }
        } else {
            assign("bad.on.top", value = bad.on.top, envir = baseenv())
            warning("Exported list of Pub.numbers with top topic in bad.topics (as `bad.on.top`)")
        }
    }

    return(top_topics)

}

# Compare top-topic distribution for one topic as compared to the rest of the dataset
top_topics_comparison <- function(mytopics,  # which topic or topics to focus on?
                                top_topics = NULL,   # as produced above
                                dataset_name = "noexcludes2001_2015",
                                ntopics = 50,
                                iter_index = 1,
                                subset_name = "knownprograms2001_2015",
                                bad.topics = NULL,
                                cull.bad.topics = T,
                                dt = NULL, # doc-topic grid. pass for speed boost.
                                do.boxplot = T,
                                do.qqplot = F
){

    if(is.null(top_topics)) {
        top_topics <- get_top_topics(dataset_name = dataset_name,
                                    ntopics = ntopics,
                                    iter_index = iter_index,
                                    subset_name = subset_name,
                                    bad.topics = bad.topics,
                                    grid = dt)
    }

    if(cull.bad.topics) {
        if(is.null(bad.topics)) {
            warning("top_topics_comparison(): Couldn't remove dissertations ",
                    "with non-content-bearing top topics; ",
                    "no list of bad.topics provided.")
        } else {
            bad.count <- nrow(top_topics[topic %in% bad.topics])
            top_topics <- top_topics[!topic %in% bad.topics]
            message("top_topics_comparison(): Removed ", bad.count, " dissertations ",
                    "with top topic marked as non-content-bearing (bad.topics)")
        }
    }

    # get topic labels if they don't exist
    if(!exists("topic_labels")) {
        if(!exists("get_topic_labels")) { source(file="get topic labels.R") }
        topic_labels <- get_topic_labels(dataset_name, ntopics, subset_name, iter_index)
        topic_labels <- data.table(topic_labels, key="Topic")
    }
    # TO DO: return summary stats for each of mytopics

    # loop through topics in mytopics
    for (mytopic in mytopics) {
        if(mytopic %in% bad.topics) {
            warning("top_topics_comparison: selected topic (", mytopic, ") has been marked as non-content-bearing")
            if(cull.bad.topics) { next }
        }

        samp <- top_topics[topic == mytopic, weight]
        comp <- top_topics[topic != mytopic, weight]

        tstats <- t.test(samp, comp)

        if (tstats$p.value < .05) {
            if(tstats$p.value < .001) {
                sig_level <- "highly significantly"
            } else {
                sig_level <- "significantly"
            }
        } else {
            sig_level <- "NOT significantly"
        }

        ## TO DO: also record the direction, based on which median is higher (or go with UHinge, since I think there may be one median tie)

        outfile <- build_plot_title(dataset_name = dataset_name,
                                    ntopics = ntopics,
                                    iter_index = iter_index,
                                    subset_name = subset_name,
                                    bad.topics = bad.topics,
                                    for.filename = remake_figs,
                                    whatitis = paste("top topic frequency for topic",
                                                     mytopic, "is", sig_level, "different",
                                                     "from the main distribution")
        )

        if(do.boxplot) {
            outfile <- paste0(outfile, "--boxplot")
        }
        if(do.qqplot) {
            outfile <- paste0(outfile, "--qqplot")
        }

        if(remake_figs) {
            outfile <- paste0(outfile, ".pdf")
            outfile <- file.path(imageloc, outfile)
            pdf(outfile)
        }

        if(do.boxplot && do.qqplot) {
            par(mfrow=c(1, 2))
        }

        if(do.boxplot) {
            boxplot(list(samp, comp),
                        # main = "Topic with Top Rank Within Document",
                        names = c(paste0("Sample (N=", length(samp), ")"),
                                  paste0("Complement (N=", length(comp), ")")),
                        xlab = paste0("T", mytopic, ": ", topic_labels[Topic == mytopic, Label]),
                        ylab = "Portion of Document (scaled to 1)",
                        ylim = c(0, 1),
                        frame.plot = F
            )
        }

        if(do.qqplot) {
            qqplot(comp, samp,
                   ylab = paste0("Sample (N=", length(samp), ")"),
                   xlab = paste0("Complement (N=", length(comp), ")"),
                   xlim = c(0, 1),
                   ylim = c(0, 1),
                   # main = "Quantile-Quantile Plot",
                   sub = paste0("T", mytopic, ": ", topic_labels[Topic == mytopic, Label]),
                   frame.plot = F
            )
            abline(a=0, b=1, col="#009900")
        }

        # title(main = outfile)

        outside_legend("topright",
                       legend=paste0("p-value = ",
                                     round(tstats$p.value, 4),
                                     "\n(", tstats$method, ")"),
                       bty="n"
        )

        if (remake_figs) {
            dev.off()
            message("File saved: ", outfile)
        } else {
            message(outfile)
            readline("Press <enter> for next plot")
        }

    } # end of for loop

}


# For dissertations with a given top topic, what are the other topics they most pair with?
# See also cotopics.R for topic pairings at any rank
top_topic_assists <- function(mytopics,            # which topic or topics to focus on?
                              cutoff = 0.05,       # drop topic weights below this
                              sortby = c("median", "count"),
                              top_topics = NULL,   # as produced above
                              dataset_name = "noexcludes2001_2015",
                              ntopics = 50,
                              iter_index = 1,
                              subset_name = "knownprograms2001_2015",
                              bad.topics = NULL,
                              cull.bad.topics = T,
                              dtgrid = NULL            # doc-topic grid. pass for speed boost.
){
    require(data.table)

    sortby <- match.arg(sortby)

    if(is.null(top_topics)) {
        top_topics <- get_top_topics(dataset_name = dataset_name,
                                     ntopics = ntopics,
                                     iter_index = iter_index,
                                     subset_name = subset_name,
                                     bad.topics = bad.topics,
                                     grid = dtgrid)
    }

    if(cull.bad.topics) {
        if(is.null(bad.topics)) {
            warning("top_topic_assists(): Couldn't remove dissertations ",
                    "with non-content-bearing top topics; ",
                    "no list of bad.topics provided.")
        } else {
            bad.count <- nrow(top_topics[topic %in% bad.topics])
            top_topics <- top_topics[!topic %in% bad.topics]
            message("top_topic_assists(): Removed ", bad.count, " dissertations ",
                    "with top topic marked as non-content-bearing (bad.topics)")

            # to do: avoid using with=F (may require updating data.table; be cautious)
            dtgrid <- dtgrid[, setdiff(colnames(dtgrid), bad.topics), with=F]
        }
    }

    for (mytopic in mytopics) {
        disses <- top_topics[topic == mytopic, Pub.number]
        topic_weights <- dtgrid[Pub.number %in% disses]

        # Cut off low-ranked values before calculating medians. This thing is skewed way low,
        # and otherwise normal values (i.e. anything above 5%) are all outliers

        topic_weights_long <- melt(topic_weights, id.vars=c("Pub.number"), variable.name = "Topic", value.name = "Weight")
        topic_weights_long <- topic_weights_long[Weight > cutoff]


        medians <- topic_weights_long[, median(Weight), by=Topic]   # a data table
        counts <- topic_weights_long[, .N, by=Topic]

        if(sortby == "median") {
            myorder <- order(medians$V1, decreasing=T)
            sortvalues <- medians[myorder]
        } else if (sortby == "count") {
            myorder <- order(counts$N, decreasing=T)
            sortvalues <- counts[myorder]
        } else {
            stop("top_topic_assists: sortby must be either 'median' or 'count' at this time.")
        }

        topic_weights_wide <- dcast(topic_weights_long, Pub.number ~ Topic, value.var="Weight")
        topic_weights_wide <- topic_weights_wide[, Pub.number:=NULL]
        topic_weights_wide <- topic_weights_wide[, ..myorder]


        # first column should now equal the chosen topic. drop it.
        if(names(topic_weights_wide[, 1]) == mytopic) {
            topic_weights_wide <- topic_weights_wide[, 1:=NULL]
            sortvalues <- sortvalues[Topic != mytopic]
        } else {
            warning("top_topic_assists(): Something's gone wrong: chosen topic ",
                    mytopic, " is not top-ranked in these dissertations.")
        }

        # Get human-readable labels if they don't exist
        if(!exists("topic_labels")) {
            if(!exists("get_topic_labels")) { source(file="get topic labels.R") }
            topic_labels <- get_topic_labels(dataset_name, ntopics, subset_name, iter_index)
            topic_labels <- data.table(topic_labels, key="Topic")
        }    

        # Draw plot
        boxplot(topic_weights_wide)

        # # TO DO: Add title, add human-readable labels to the plot itself
        # to.plot <- as.matrix(topic_weights_wide[, ncol(topic_weights_wide):1])
        # boxplot(to.plot,
        #         horizontal = T,
        #         xlab = topic_labels[as.integer(dimnames(to.plot)[[2]]), Label])


        # TO DO: Better format these (merge somehow)
        message("These topics tend to co-occur when top topic is ", mytopic, " (",
                 topic_labels[Topic == mytopic, Label], "): ")
        myresult <- topic_labels[Topic %in% names(topic_weights_wide), .(Topic, Label), ]
        myresult$Topic <- factor(myresult$Topic)
        myresult <- merge(myresult, sortvalues)

        if (sortby == "median") {
            setnames(myresult, "V1", "MedianWeight")
            setorder(myresult, -MedianWeight)
            myresult[, MedianWeight:=round(MedianWeight, 4)]
        } else if (sortby == "count") {

        }


        print(myresult)

        readline("Press <enter> for detailed stats")

        print(summary(topic_weights_wide))

        readline("Press <enter> for next topic")
    }

    # To do: return summary for all topics in loop, not just the last

    return(myresult)
}

# Now, what 2nd-ranked topics go with each 1st-ranked topic?
# 1. given a topic, find all dissertations with that top topic (see lines 721-764 above)
# 2. determine the 2nd and 3rd (and...) ranked topics for each diss, probably with get.topics4doc
# 3. count the number of times each topic appears at each calculated rank
# 4. TO DO: optionally visualize this as an alluvial chart

top_topic_assists2 <- function(mytopic,             # which topic to focus on?
                               top_topics = NULL,    # as produced by get_top_topics()
                               topic_depth = 3,      # lowest topic-rank per diss?
                               n_per_rank = NULL,    # should we cap our number of topics at each rank-point?
                               do.plot = F,          # if FALSE, just return the table
                               # maybe I could visualize in different ways?
                               # plot.as = c("alluvial", "sankey", "area")
                               dataset_name = "noexcludes2001_2015",
                               ntopics = 50,
                               iter_index = 1,
                               subset_name = "knownprograms2001_2015",
                               bad.topics = NULL,
                               cull.bad.topics = T,
                               showlabels = T,
                               topic_labels = NULL,  # pass in for speed boost
                               dtgrid = NULL         # doc-topic grid. pass for speed boost.
){
    require(data.table)
    
    if(topic_depth != 3) {
        warning("top_topic_assists2() is only implemented with 3 topics for now. Sorry!")
    }
    
    # plot.as <- match.arg(plot.as)
    
    if(is.null(top_topics)) {
        top_topics <- get_top_topics(dataset_name = dataset_name,
                                     ntopics = ntopics,
                                     iter_index = iter_index,
                                     subset_name = subset_name,
                                     bad.topics = bad.topics,
                                     grid = dtgrid)
    }
    
    if(cull.bad.topics) {
        if(is.null(bad.topics)) {
            warning("top_topic_assists2(): Couldn't remove dissertations ",
                    "with non-content-bearing top topics; ",
                    "no list of bad.topics provided.")
        } else if(mytopic %in% bad.topics) {
            stop("top_topic_assists2(): selected topic T", mytopic, " has been ",
                 "marked as non-content-bearing and cull.bad.topics = T.")
        }
    }
    
    # record some basic information
    disses <- top_topics[topic == mytopic, Pub.number]
    topic_weights <- dtgrid[Pub.number %in% disses]
    
    to.return <- list("top_topic" = mytopic,
                      "disscount" = length(disses))
    
    
    # optionally treat the topic label as basic information
    if(showlabels) {
        if(is.null(topic_labels)) {
            # get topic labels if they don't exist
            if(!exists("get_topic_labels")) { source(file="get topic labels.R") }
            topic_labels <- get_topic_labels(dataset_name, ntopics, subset_name, iter_index)
            topic_labels <- data.table(topic_labels, key="Topic")
        }
    }
    
    to.return$label <- as.character(topic_labels[mytopic, Label])
    
    
    if(!exists("get.topics4doc", mode="function")) {
        source(file="top docs per topic.R")
    }
    
    # get top topics for all disses
    # returning just topic numbers makes the returned matrix readable.
    disses_tops <- sapply(topic_weights$Pub.number,
                          FUN=function(x) {
                              get.topics4doc(x,
                                             grid = dtgrid,
                                             howmany = topic_depth,
                                             showlabels = F,
                                             topic_labels = topic_labels)$keys$topic
                          })
    
    
    # transpose disses_tops matrix to form triplets,
    # sort triplets so as to build compact Sankey flow from rank to rank...
    # or just to count and rank the unique combinations (regardless of order)
    
    disslets <- data.table(t(disses_tops))
    setkey(disslets) # without a second argument, sorts increasing on all columns
    disslets[, combo:=paste0(V1, "|", pmin(V2, V3), "|", pmax(V2, V3))] 
    disslets[, N:=.N, by=combo]
    disslets[order(N, decreasing=T)]
    setnames(disslets, old = c("V1", "V2", "V3", "N"), 
                       new = c("rank1topic", "rank2topic", "rank3topic", "combocount"))
    
    if(showlabels) {
        # add labels
        disslets[, `:=`(rank1label = topic_labels[disslets$rank1topic, Label],
                        rank2label = topic_labels[disslets$rank2topic, Label],
                        rank3label = topic_labels[disslets$rank3topic, Label])]
        # reorder columns for easier association of topic number and label
        disslets <- disslets[, .(combocount, rank1topic, rank1label, rank2topic, rank2label, rank3topic, rank3label, combo)]
    }
    
    # sort rows, keeping highest frequency on top, but combos together
    disslets <- disslets[order(-rank(combocount), combo)]
    
    # optionally export to file
    if(remake_figs) {
        filename <- build_plot_title(dataset_name = dataset_name,
                                     ntopics = ntopics,
                                     iter_index = iter_index,
                                     subset_name = subset_name,
                                     whatitis = paste("top_topic_assists, topic",
                                                      mytopic, "depth", topic_depth),
                                     for.filename = T)
        filename <- paste0(filename, ".csv")
        filename <- file.path(imageloc, filename)
        write.csv(disslets, file=filename, row.names=F)
        # Take CSV to another program: RawGraphs.io, Tableau, etc
    }
    
    # for working in R, we can now drop rank1topic for better side-by-side viewing
    to.return$combos <- disslets[, -c("rank1topic", "rank1label")]
    
    
    # FOR NOW: build a frequency table of each rank of interest
    if(!all(disses_tops[1,] == mytopic)) {
        stop("Somehow topic ", mytopic, " produced a list of disses ",
             "for which the top topics are not all equal to ", mytopic)
    } else {
        # loop through remaining ranks
        for(i in 2:topic_depth) {
            tmp <- sort(table(disses_tops[i, ]), decreasing = T)
            if(!is.null(n_per_rank)) {
                max_length <- min(n_per_rank, length(tmp))
                tmp <- tmp[1:max_length]
            }
            tmp <- data.table(Topic=as.integer(names(tmp)),
                              tmp)
            tmp[,V1:=NULL] # discard extra copy of Topic from names(tmp)
            tmp <- topic_labels[tmp][, list(Topic, Label, N, Pct=(round(100*(N/length(disses)), 2)), Agg_Rank=Rank)]
            
            
            to.return[[paste0("rank",i)]] <- tmp
         
            # TO CONSIDER: Maybe it's more interesting to just add up rank2 and rank3,
            # see which get the most total "votes" 
          
        } 
    }   # end of loop throgh topic depths
    return(to.return)
} # end of top_topic_assists2()


# This is basically top_topic_assists3, but I'm now looking at all disses
# and dropping my interest in which of the three top topics comes in first:
# I'm interested in topic-triples at the top, regardless of order.
top_topic_combos <- function(topic_depth = 3,
                             dataset_name = dataset_name,
                             ntopics = ntopics,
                             iter_index = iter_index,
                             subset_name = subset_name,
                             bad.topics = NULL,
                             cull.bad.topics = !is.null(bad.topics),
                             mygrid = NULL, # doc-topic grid
                             topic_labels = NULL,
                             showlabels = T,
                             anywhere = F) 
{
    if (is.null(mygrid)) {
        mygrid <- get.doctopic.grid(dataset_name=dataset_name, ntopics=ntopics,
                                    iter_index=iter_index, newnames=newnames)$outputfile.dt
    } 
    
    if (!is.null(subset_name)) {
        index <- get(subset_name)$Pub.number
        mygrid <- mygrid[Pub.number %in% index]
    }
    
    mygrid <- na.omit(mygrid)
    
    if(cull.bad.topics) {
        if(is.null(bad.topics)) {
            warning("top_topic_combos(): Couldn't remove dissertations ",
                    "with non-content-bearing top topics; ",
                    "no list of bad.topics provided.")
        } else {
            mygrid <- mygrid[, -(..bad.topics)]
        }
    }
    
    if(showlabels) {
        if(is.null(topic_labels)) {
            # get topic labels if they don't exist
            if(!exists("get_topic_labels")) { source(file="get topic labels.R") }
            topic_labels <- get_topic_labels(dataset_name, ntopics, subset_name, iter_index)
            topic_labels <- data.table(topic_labels, key="Topic")
        }
    }
    
    if(!exists("get.topics4doc", mode="function")) {
        source(file="top docs per topic.R")
    }
    
    # get top topics for all disses
    # returning just topic numbers makes the returned matrix readable.
    all_disses_tops <- sapply(mygrid$Pub.number,
                              FUN=function(x) {
                                  get.topics4doc(x,
                                                 grid = mygrid,
                                                 howmany = topic_depth)$keys$topic
                              })
    
    
    # transpose disses_tops matrix to form triplets,
    # sort triplets so as to build compact Sankey flow from rank to rank...
    # or just to count and rank the unique combinations (regardless of order)
    
    triplets <- data.table(t(all_disses_tops))
    setkey(triplets) # without a second argument, sorts increasing on all columns
    
    combos <- apply(triplets, 
                    MARGIN = 1, # by row
                    FUN = function(x) {
                        paste(sort(x), collapse="|")
                    })
    triplets[, combo:=..combos] 
    triplets[, N:=.N, by=combo]
    triplets[order(N, decreasing=T)]
    oldnames <- c(paste0("V", 1:topic_depth), "N")
    newnames <- c(paste0("rank", 1:topic_depth, "topic"), "combocount")
    setnames(triplets, oldnames, newnames)
    
    if(showlabels) {
        # add labels
        triplets[, `:=`(rank1label = topic_labels[triplets$rank1topic, Label],
                        rank2label = topic_labels[triplets$rank2topic, Label])]
        
        if(topic_depth == 3) {
            triplets[, `:=`(rank3label = topic_labels[triplets$rank3topic, Label])]
            # reorder columns for easier association of topic number and label
            triplets <- triplets[, .(combo, combocount, rank1topic, rank1label, rank2topic, rank2label, rank3topic, rank3label)]
        } else if (topic_depth == 2) {
            triplets <- triplets[, .(combo, combocount, rank1topic, rank1label, rank2topic, rank2label)]
        } else {
            warning("Sorry, I haven't implemented topics below rank3.")
        }
        
    }
    
    # sort rows, keeping highest frequency on top, but combos together
    triplets <- triplets[order(-rank(combocount), combo)]
    
    if(anywhere) {
        # For a given top topic, what is the total frequency with which 
        # another topic pairs with it, in *either* rank2 or rank3?
        # Try for a long format:
        all_topic_assists <- data.table("rank1topic" = integer(),
                                        "partnertopic" = integer(),
                                        "partnerfreq" = integer(),
                                        key = "rank1topic")
        
        for (x in setdiff(1:ntopics, bad.topics)) {
            xcombos <- triplets[rank1topic == x]
            xfreq <- sort(table(c(xcombos$rank2topic, xcombos$rank3topic)), decreasing =T)
            for(i in 1:length(xfreq)) {
                # message("i = ", i)
                myrow <- list(x, as.integer(names(xfreq[i])), xfreq[i])
                # print(myrow)
                all_topic_assists <- rbindlist(list(all_topic_assists, myrow))
            }
        } 
        
        if(showlabels) {
            all_topic_assists[, `:=`(rank1label = topic_labels[all_topic_assists$rank1topic, Label],
                                     partnerlabel = topic_labels[all_topic_assists$partnertopic, Label])]
        }
        
        all_topic_assists <- all_topic_assists[order(-partnerfreq)]
        
        # optionally, save to file
        if(remake_figs) {
            filename <- build_plot_title(dataset_name = dataset_name,
                                         ntopics = ntopics,
                                         iter_index = iter_index,
                                         subset_name = subset_name,
                                         whatitis = paste("top_topic_partners, anywhere in depth", topic_depth),
                                         for.filename = T)
            filename <- paste0(filename, ".csv")
            filename <- file.path(imageloc, filename)
            write.csv(all_topic_assists, 
                      file=filename)
            # Take CSV to another program: RawGraphs.io, Tableau, etc
        } else {
            print(all_topic_assists)
        }   
        
        return(all_topic_assists)
    } # end of if(anywhere)
    
    # report unique combinations of three topics, with frequencies
    unique_combos <- unique(triplets, by="combo")
    
    # optionally export to file
    if(remake_figs) {
        filename <- build_plot_title(dataset_name = dataset_name,
                                     ntopics = ntopics,
                                     iter_index = iter_index,
                                     subset_name = subset_name,
                                     whatitis = paste("top_topic_combos, depth", topic_depth),
                                     for.filename = T)
        filename <- paste0(filename, ".csv")
        filename <- file.path(imageloc, filename)
        write.csv(unique_combos, file=filename, row.names=F)
        # Take CSV to another program: RawGraphs.io, Tableau, etc
    }
    
    return(unique_combos)
}

if(!autorun) {
    message("Loaded the following functions from `variation of topic proportions.R`:\n",
            "  topic.proportions(dataset_name, ntopics, iter_index, ...): the boxplot\n",
            "                  described above, showing distribution of topic weights.\n",
            "  find.doc.by.topic.proportion(topic.weights, topic_rank, value, ...): given a\n",
            "                  distribution of topic weights, find a representative diss\n",
            "                  at high, low, or median values. Useful for demonstrating\n",
            "                  what single-topic focused or multi-topic disses look like.\n",
            "  top_topic_histogram(topic.weights, topic_rank, ...): Histogram of values\n",
            "                  for the nth-ranked topic; a zoom-in on topic.proportions().\n",
            "  get_top_topics(dataset_name, ntopics, iter_index, ...): Find all the top topics\n",
            "                  while preserving identity, so we can graph subsets\n",
            "                  and compare them to the corpus-wide distribution.\n",
            "  top_topics_comparison(mytopics, top_topics, ...): Compare top-topic distribution\n",
            "                  for one topic as compared to the rest of the dataset.\n",
            "                  Useful for testing topical independence (see: writing centers)\n",
            "  top_topic_assists(mytopics, cutoff, sort_by, ...): For dissertations with\n",
            "                  a given rank1 topic, what are the other top-ranked topics\n",
            "                  they most pair with? (See also cotopics.R)\n",
            "  top_topic_assists2(mytopic, top_topics, topic_depth, n_per_rank, ...): now more\n",
            "                  specific results at rank2, 3, etc. (TO DO: graph as alluvial plot)\n",
            "  top_topic_combos(topic_depth, ...): Topic-triples at the top, regardless of order.\n"
            )
}

# Using the analysis; a testing space.
if(FALSE) {
	remake_figs
	topic.proportions()
	topic.proportions(explore.outliers=T)
	# abline(0.11384, 0, col="#aa0000")


    dataset_name <- "noexcludes2001_2015"
    ntopics <- 50
    iter_index <- 1
    subset_name <- "knownprograms2001_2015"
    bad.topics <- c("3", "8", "12", "15", "30", "34", "36", "47", "50")
    # bad.topics <- NULL

    
    # the document-topic grid
    dtgrid <- get.doctopic.grid(dataset_name=dataset_name, ntopics=ntopics,
                                iter_index=iter_index)$outputfile.dt
    dtgrid <- na.omit(dtgrid)
    
    # topic weights per document, among other stats
    topic_weights <- topic.proportions(dataset_name = dataset_name,
                      ntopics = ntopics,
                      iter_index = iter_index,
                      subset_name = subset_name,
                      # subset_name = NULL,
                      bad.topics = bad.topics,
                      explore.outliers = T,
                      explore.lowliers = T,
                      newnames = F,
                      markcutoff = T,
                      howmany = 10,
                      lowlier.rank = 2)
    str(topic_weights)
    
        
    top_topics <- get_top_topics(dataset_name = dataset_name,
                                 ntopics = ntopics,
                                 subset_name = subset_name,
                                 bad.topics = bad.topics,
                                 grid = dtgrid)
    
    ## Confirm that upper / lower whiskers are real values, the last observed values
    ## i.e. within 1.5 * inter-quartile distance (IQR) of the hinges
    # IQR <- topic_weights$stats[1, "Uhinge"] - topic_weights$stats[1, "Lhinge"]
    # 1.5 * IQR
    # topic_weights$stats[1, "Uhinge"] + (1.5 * IQR)
    # topic_weights$stats[1, "upper"]
    # hinge_diss <- min(which(topic_weights$weightsbydoc[, "X1"] <= topic_weights$stats[1, "upper"]))
    # topic_weights$weightsbydoc[(hinge_diss-1):(hinge_diss+1), 1:11]
    # topic_weights$stats[1, "Lhinge"] - (1.5 * IQR)
        # topic_weights$stats[1, "lower"]
    # hinge_diss <- max(which(topic_weights$weightsbydoc[, "X1"] >= topic_weights$stats[1, "lower"]))
    # topic_weights$weightsbydoc[(hinge_diss-1):(hinge_diss+1), 1:11]

    remake_figs=F
    top_topic_histogram(topic_weights = topic_weights,
                        topic_rank = 1,
                        dataset_name = dataset_name,
                        ntopics = ntopics,
                        iter_index = iter_index,
                        subset_name = subset_name,
                        # subset_name = NULL,
                        bad.topics = bad.topics,
                        useboxstats = F)

    
    remake_figs = F
    top_topics_comparison(mytopics = c(1, 27),
                          top_topics = top_topics,
                          dataset_name = dataset_name,
                          ntopics = ntopics,
                          iter_index = iter_index,
                          subset_name = subset_name,
                          bad.topics = bad.topics,
                          dt = dtgrid)
    
    
    ## Show me the median dissertation, please!
    diss_mid <- find.doc.by.topic.proportion(topic_weights = topic_weights,
                                 value = "median",
                                 topic_rank = 1,
                                 offset = 0,
                                 howmanytopics = 15)
    sum(diss_mid$keys$weight)

    # and a high-end-of-normal diss?
    diss_high <- find.doc.by.topic.proportion(topic_weights = topic_weights,
                                 value = "upper",
                                 offset = -2,
                                 howmanytopics = 15)
    print(diss_high)
    sum(diss_high$keys[1:3]$weight)

    diss_low <- find.doc.by.topic.proportion(topic_weights = topic_weights,
                                 topic_rank = 3,
                                 value = "upper",
                                 offset = 8,
                                 howmanytopics = 15)
    print(diss_low)
    sum(diss_low$keys[1:3]$weight)

    disses_low <- c()
    for(i in -10:10) {
        disses_low[i+11] <- find.doc.by.topic.proportion(topic_weights = topic_weights,
                                                      topic_rank    = 3,
                                                      value         = "upper",
                                                      offset        = i)$title$Year
    }; print(disses_low)

    diss_lower <- find.doc.by.topic.proportion(topic_weights = topic_weights,
                                             topic_rank = 1,
                                             value = "lower",
                                             offset = 8,
                                             howmanytopics = 15)
    print(diss_lower)
    sum(diss_lower$keys[1:3]$weight)

    disses_lower <- c()
    for(i in -10:10) {
        disses_lower[i+11] <- find.doc.by.topic.proportion(topic_weights = topic_weights,
                                                         topic_rank    = 3,
                                                         value         = "upper",
                                                         offset        = i)$title$Year
    }; print(disses_lower)

    # Inspect topics for outlier dissertations: is this a pattern, or no?
    if(!exists("top_titles_table")) {
        source(file="top docs per topic.R")
    }

    for (topic in topic_weights$outliers.table$Topic) {
        top_topic_browser(dataset_name = dataset_name,
                          ntopics = ntopics,
                          iter_index = iter_index,
                          subset_name = subset_name,
                          showlabels = TRUE,
                          depth = 20,
                          topic = topic)
    }

    # Okay, if that looks good, go ahead
    outliers.title.table <- data.frame(topic=numeric(),
                                       topic_rank=numeric(),
                                       Titles=character())
    for (topic in topic_weights$outliers.table$Topic) {
        row <- top_titles_table(dataset_name = dataset_name,
                          ntopics = ntopics,
                          iter_index = iter_index,
                          subset_name = subset_name,
                          showlabels = TRUE,
                          depth = 5,
                          topic = topic)
        outliers.title.table <- rbind(outliers.title.table, row)
    }

    outliers.table <- merge(topic_weights$outliers.table,
                            outliers.title.table,
                            by.x="Topic",
                            by.y="topic")
    outliers.table$highvalues <- NULL
    setorder(outliers.table, -`Outlier Count`)

    if(remake_figs) {
        if(!exists("build_plot_title", mode="function")) {
            source(file="build_plot_title.R")
        }

        outfile <- build_plot_title(dataset_name = dataset_name,
                                    ntopics = ntopics, 
                                    iter_index = iter_index, 
                                    subset_name = subset_name,
                                    bad.topics = bad.topics,
                                    whatitis = paste("Proportions of Text from Upper Outliers, with titles"),
                                    for.filename = T
        )
        outfile <- paste0(outfile, ".csv")
        outfile <- file.path(imageloc, outfile)
        write.csv(outliers.table, outfile, row.names=F)
    } else {
        print(outliers.table)
    }


    ## Now show me the histograms of the top outlier topics (as opposed to full corpus)
    # UPDATE: Well, after all that (~5 poms, ~2 new functions), to an eyeball estimation
    # the distributions just aren't especially different from the corpus as a whole.
    # mytopic <- 1

    mygrid <- get.doctopic.grid(dataset_name=dataset_name, ntopics=ntopics,
                      iter_index=iter_index, newnames=F)$outputfile.dt
    # system.time(
        top_topics <- get_top_topics(dataset_name = dataset_name,
                   ntopics = ntopics,
                   iter_index = iter_index,
                   subset_name = subset_name,
                   bad.topics = bad.topics,
                   grid = mygrid)
    # ) for 1684 dissertations, elapsed time: 28.349, even with the speed boost

    # remove rows in which the top topic was a bad.topic
    top_topics <- top_topics[!.(as.numeric(bad.topics))]

    # compare the subset not to the whole, but to the complement (whole - subset);
    # see https://stats.stackexchange.com/questions/108820/comparison-of-two-means/108828#108828

    # get the topic labels, and make sure you don't overwrite the saved spreadsheet
    # by temporarily turning off remake_figs, even if it's currently true
    if(!exists("get_topic_labels")) {
        source(file="get topic labels.R")
    }
    topic_labels <- get_topic_labels(dataset_name, ntopics, subset_name, iter_index)
    topic_labels <- data.table(topic_labels, key="Topic")

    # Test function to see which topics are generally high-ranked below a given top topic
    top_topic_assists(mytopics = c(27, 1),
                      bad.topics = bad.topics,
                      dtgrid = dtgrid,
                      top_topics = top_topics)

    
    
    #### Report the number of times each topic is the top topic               ####
    #    TO DO: make this an independent function, varying how many to report    #
    #    Also, cull bad.topics                                                   #
    tops_as_top <- top_topics[, .N, by=topic]
    setnames(tops_as_top, "N", "times_on_top")
        # get topic labels if they don't exist
        if(!exists("topic_labels")) {
            if(!exists("get_topic_labels")) { source(file="get topic labels.R") }
            topic_labels <- get_topic_labels(dataset_name = dataset_name, 
                                             ntopics = ntopics, 
                                             subset_name = subset_name,
                                             iter_index = iter_index)
            topic_labels <- data.table(topic_labels, key="Topic")
        }

    tops_as_top <- merge(tops_as_top, topic_labels[, .(Topic, Label, Top.Words)], by.x="topic", by.y="Topic")
    tops_as_top <- tops_as_top[order(-rank(times_on_top))]
    if(cull.bad.topics) {
        tops_as_top <- tops_as_top[!(topic %in% bad.topics)]
    }
    if(remake_figs) {
        # If we're saving it, try to prep for print formatting
        to.print <- tops_as_top[, .("Label (Topic #)"=paste0(Label, " (", topic, ")"),
                                    "Top Words (TF-ITF)"=Top.Words,
                                    "Count as Top Topic (% of disses)"=paste0(times_on_top, " (", 
                                                        round(100*times_on_top/sum(times_on_top), 2), ")")
                                    )]
        
        filename <- build_plot_title(dataset_name = dataset_name,
                                     ntopics = ntopics,
                                     iter_index = iter_index,
                                     subset_name = subset_name,
                                     bad.topics = bad.topics,
                                     use.labels = T,
                                     whatitis = "Topics by Frequency as Top-Ranked in Dissertation",
                                     for.filename = T)
        filename <- paste0(filename, ".csv")
        filename <- file.path(imageloc, filename)
        write.csv(to.print, filename, row.names=T)
    } else {
        print(tops_as_top)
    }


    
    t32assists <- top_topic_assists2(mytopic = 32,
                       top_topics = top_topics,
                       bad.topics = bad.topics,
                       topic_labels = topic_labels,
                       dtgrid = dtgrid)
    
    t32assists$combos[combocount >= t32assists$combos$combocount[10]]
    
    top3assists <- sapply(topic_labels[Rank <= 3, Topic],
           FUN = function(x) {top_topic_assists2(x,
                                                 top_topics = top_topics,
                                                 bad.topics = bad.topics,
                                                 topic_labels = topic_labels,
                                                 dtgrid = dtgrid)$combos
                             }
           )
    
    
    
    

    remake_figs = T
    top3combos <- top_topic_combos(topic_depth = 3,
                     dataset_name = dataset_name,
                     ntopics = ntopics,
                     iter_index = iter_index,
                     subset_name = subset_name,
                     bad.topics = bad.topics,
                     mygrid = dtgrid,
                     topic_labels = topic_labels,
                     showlabels = T)
    
    top2or3partners <- top_topic_combos(topic_depth = 3,
                                   dataset_name = dataset_name,
                                   ntopics = ntopics,
                                   iter_index = iter_index,
                                   subset_name = subset_name,
                                   bad.topics = bad.topics,
                                   mygrid = dtgrid,
                                   topic_labels = topic_labels,
                                   showlabels = T,
                                   anywhere = T)
    

    
    top2combos <- top_topic_combos(topic_depth = 2,
                                   dataset_name = dataset_name,
                                   ntopics = ntopics,
                                   iter_index = iter_index,
                                   subset_name = subset_name,
                                   bad.topics = bad.topics,
                                   mygrid = dtgrid,
                                   topic_labels = topic_labels,
                                   showlabels = T)
   remake_figs = F
    
    
} # end of if(FALSE)
