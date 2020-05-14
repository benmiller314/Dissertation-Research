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
    
    	        #####
    	        # I have a hypothesis that these are mostly language-based topics.
    	        # Let's look at the top topics represented here, to know what's 
    	        # so dominant in the #1 slot as to crowd out these #2 topics.
    	        # STRATEGY:
    	        # 1. For each Pub.number, get top-ranked topic number by finding the
    	        #    max within that row of `grid`.
    	        # 2. Make a table of these topic numbers.
    	        # 3. Retrieve the labels for each topic in the table.
    
    	        lowtopics <- c()		# start empty and build up
    	        lowvalues <- c()		# what are those high percent-of-text values?
    
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
    	        message("Top-ranked topics of disses with lower outliers for second-ranked topic:")
    	        print(lowlabels.t)
    	        message(paste("Total 'lowliers' for second-ranked topic:",
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

find.doc.by.topic.proportion <- function(topic_weights,   # as produced above
                                         topic_rank = 1,  # top-ranked topic? 2nd?
                                         value="median",  # where to pull from?
                                         dataset_name = "noexcludes2001_2015",
                                         ntopics = 50,
                                         iter_index = 1,
                                         subset_name = "knownprograms2001_2015",
                                         offset = 0,     # want to explore adjacent disses?
                                         howmanytopics = 5
                                         
) {
    wbd <- topic_weights$weightsbydoc
    
    mystats <- topic_weights$stats
    
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


## TO DO: histogram of weights of just the top topic
top.topic.histogram <- function(topic_weights,   # as produced above
                                topic_rank = 1,  # top-ranked topic? 2nd?
                                value="median",  # where to pull from?
                                dataset_name = "noexcludes2001_2015",
                                ntopics = 50,
                                iter_index = 1,
                                subset_name = "knownprograms2001_2015",
                                bad.topics = NULL,
                                useboxstats = FALSE)
{
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
                      howmany = 10)
    
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
    top.topic.histogram(topic_weights = topic_weights,
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
    sum(diss_high$keys[1:3]$weight)
    
}
