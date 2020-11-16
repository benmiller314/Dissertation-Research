#############################################################################
# variation of topic proportions.R
#
# Goal: Find out the curve of topic strengths within each document, i.e. how
# much of the document is the top topic? how much is the second? and so on,
# aggregated over all documents, as a boxplot of contribution (y-axis) sorted
# by topic rank (x-axis).
#
#  Rationale: I want to know at what level to cut off "cotopics": what's a
#  realistic scenario?
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
						  
						  howmany = 3    # how far into the ranks should we go?
){
	require(data.table)
	if(!exists("get.doctopic.grid", mode="function")) {
		source("get doctopic grid.R")
	}
	grid <- get.doctopic.grid(dataset_name = dataset_name,
	                          ntopics = ntopics,
	                          iter_index = iter_index,
	                          subset_name = subset_name,
									  newnames = newnames)$outputfile.dt
	# str(grid)
	head(grid)

	# Exclude non-content-bearing topics
	# If none are set in parameters, use defaults:
	if(is.null(bad.topics) && dataset_name=="consorts" && ntopics==55) {
		bad.topics <- c("4", "47", "22", "2", "24", "50", "13")
	}


	grid.clean <- grid[, setdiff(names(grid), c(bad.topics, "Pub.number")), with=F]
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
    grid.sorted2 <- data.frame(Pub.number=grid$Pub.number, grid.sorted)
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
	subtitle <- paste0(subtitle, "   N=", nrow(grid))

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
		outliers <- cbind(grid[outliers.index, "Pub.number", with=F],
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
			row <- grid[which(grid$Pub.number==i), 2:ncol(grid), with=F]
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

	# TO DO: browse low-liers (though my hypothesis there is that they're
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
    	            row <- grid[which(grid$Pub.number==i), 2:ncol(grid), with=F]
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
                                         howmanytopics = 5
                                         
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
    
    for (i in pubs) {    # use helper function on each pub
        row <- get_top_topic(pubnum = i, 
                             dataset_name = dataset_name,
                             ntopics = ntopics,
                             iter_index = iter_index,
                             subset_name = subset_name,
                             grid=grid)
        if(row$topic %in% bad.topics) {
            bad.count <- bad.count + 1
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
    
    if(!exists("get_topic_labels", mode="function")) {
        source(file="get topic labels.R")
    }
    topic_labels <- data.table(get_topic_labels(dataset_name = dataset_name, 
                                                ntopics = ntopics, 
                                                iter_index = iter_index,
                                                subset_name = subset_name),
                               key="Topic")
    
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
            outfile <- paste0("boxplot_", outfile)
        }
        if(do.qqplot) {
            outfile <- paste0("qqplot_", outfile)
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
                              top_topics = NULL,   # as produced above
                              dataset_name = "noexcludes2001_2015",
                              ntopics = 50,
                              iter_index = 1,
                              subset_name = "knownprograms2001_2015",
                              bad.topics = NULL,
                              cull.bad.topics = T,
                              dt = NULL            # doc-topic grid. pass for speed boost.
){
    require(data.table)
    
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
            
            # to do: avoid using with=F (may require updating data.table; be cautious)
            dt <- dt[, setdiff(names(dt), bad.topics), with=F]
        }
    }
    
    for (mytopic in mytopics) {
        disses <- top_topics[topic == mytopic, Pub.number]
        topic_weights <- dt[Pub.number %in% disses]
        
        # Cut off low-ranked values before calculating medians. This thing is skewed way low, 
        # and otherwise normal values (i.e. anything above 5%) are all outliers
        
        topic_weights_long <- melt(topic_weights, id.vars=c("Pub.number"), variable.name = "Topic", value.name = "Weight")
        topic_weights_long <- topic_weights_long[Weight > cutoff]
        
        medians <- topic_weights_long[, median(Weight), by=Topic]   # a data table
        
        myorder <- order(medians$V1, decreasing=T)
        medians <- medians[myorder]
        
        topic_weights_wide <- dcast(topic_weights_long, Pub.number ~ Topic, value.var="Weight") 
        topic_weights_wide <- topic_weights_wide[, Pub.number:=NULL]
        topic_weights_wide <- topic_weights_wide[, ..myorder]
        
        ## TO DO: report *counts,* not just medians. Wait, does that just recreate the earlier cotopic graph? Sigh.
        
        # first column should now equal the chosen topic. drop it.
        if(names(topic_weights_wide[, 1]) == mytopic) {
            topic_weights_wide <- topic_weights_wide[, 1:=NULL]
            medians <- medians[Topic != mytopic]
        } else {
            warning("top_topic_assists(): Something's gone wrong: chosen topic ", 
                    mytopic, " is not top-ranked in these dissertations.")
        }
        
        # Get human-readable labels
        if(!exists("get_topic_labels", mode="function")) {
            source(file="get topic labels.R")
        }
        topic_labels <- data.table(get_topic_labels(dataset_name = dataset_name, 
                                                    ntopics = ntopics, 
                                                    iter_index = iter_index,
                                                    subset_name = subset_name),
                                   key="Topic")
        
        # Draw plot
        # TO DO: Add title, add human-readable labels to the plot itself
        boxplot(as.matrix(topic_weights_wide))
                
        
        # TO DO: Better format these (merge somehow)
        message("These topics tend to co-occur when top topic is ", mytopic, " (", 
                 topic_labels[Topic == mytopic, Label], "): ")
        myresult <- topic_labels[Topic %in% names(topic_weights_wide), .(Topic, Label), ]
        myresult$Topic <- factor(myresult$Topic)
        myresult <- merge(myresult, medians)
        setnames(myresult, "V1", "MedianWeight")
        setorder(myresult, -MedianWeight)
        myresult[, MedianWeight:=round(MedianWeight, 4)]
        print(myresult)
        
        readline("Press <enter> for detailed stats")
        
        print(summary(topic_weights_wide))
        
        readline("Press <enter> for next topic")
    }
    
    # To do: return summary for all topics in loop, not just the last
    
    return(myresult)
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
        
        outfile <- build_plot_title(dataset_name=dataset_name, subset_name=subset_name,
                                     ntopics=ntopics, iter_index=iter_index,
                                     bad.topics=bad.topics,
                                     whatitis=paste("Proportions of Text from Upper Outliers, with titles"),
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
    remake_figs_quo <- remake_figs
    remake_figs <- F
    topic_labels <- get_topic_labels(dataset_name, ntopics, subset_name, iter_index)    
    remake_figs <- remake_figs_quo
    rm(remake_figs_quo)

    
    # Test function to see which topics are generally high-ranked below a given top topic
    top_topic_assists(mytopics = c(27, 1), 
                      bad.topics = bad.topics, 
                      dt = dt, 
                      top_topics = top_topics)
        
    
}
