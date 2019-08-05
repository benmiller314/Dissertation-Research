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

topic.proportions <- function(dataset_name = "consorts", 
						  ntopics = 55, 
						  subset_name = NULL,
						  iter_index = "",
						  
						  # NB: if default dataset and ntopics are used, 
						  # we'll use default bad.topics 
						  bad.topics = NULL,  
						  
						  # Draw notch in barplot to check for overlap?
						  use.notch = FALSE,  
						  
						  # Use topic browser for outlier dissertations?
						  explore.outliers = FALSE) 
{
	require(data.table)
	if(!exists("get.doctopic.grid", mode="function")) { 
		source("get doctopic grid.R") 
	}
	grid <- get.doctopic.grid(dataset_name, ntopics, subset_name, iter_index)$outputfile.dt
	# str(grid)
	head(grid)
	
	# Exclude non-content-bearing topics
	# If none are set in parameters, use defaults:
	if(is.null(bad.topics) && dataset_name=="consorts" && ntopics==55) { 		
		bad.topics <- c("4", "47", "22", "2", "24", "50", "13") 
	} 
	
	
	grid.clean <- grid[, !(names(grid) %in% c(bad.topics, "Pub.number")),
						 with=F]
	print(head(grid.clean))

		
	# decreasing sort across each row -- ignore column (i.e. topic) names
	grid.sorted <- t(apply(grid.clean, 1, FUN=function(x) { 
								sort(x, decreasing=T) 
							}))

		# each row is a dissertation; we lose topic numbers, but now column 1
		# is the weight of the top-ranked topic for that row, column 2 the
		# weight of the 2nd-ranked topic, and so on. Let's look at the 10
		# top-ranked topics for every dissertation.
	print(head(grid.sorted[, 1:10]))
	
	# start empty, build up
	stats <- data.frame()		
	for (i in 1:3) {
	
		# message(paste0("Stats for ", i, 
		#					"-ranked topic within dissertations:"))
		stats <- rbind(stats, boxplot.stats(grid.sorted[, i])$stats)
	}
	
	# lower whisker, lower ‘hinge’, median, upper ‘hinge’, upper whisker
	names(stats) <- c("lower", "Lhinge", "median", "Uhinge", "upper")
	stats <- cbind("rank of topic within diss"=c(1, 2, 3), stats)
	
	# we'll return the stats data.frame later.

	
	## Time to make the plot
	if(!exists("build_plot_title", mode="function")) {
	    source(file="frameToD3.R")
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
		## mark line covering top three quartiles for the 2nd-ranked topic,
		## but only the top quartile for 3rd
		abline(h=0.11, col="#99FF99")

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
		
		mytopics <- c()		# start empty and build up
		myvalues <- c()		# what are those high percent-of-text values?
		
		for (i in outliers$Pub.number) {
			row <- grid[which(grid$Pub.number==i), 2:ncol(grid), with=F]
			mytopic <- which(row == max(row))
			mytopics <- c(mytopics, mytopic)
			myvalues <- c(myvalues, max(row))
		}

		# count 'em up
		mytopics.t <- table(mytopics)

		# get labels
		if(!exists("get_topic_labels", mode="function")) { 
			source(file="get topic labels.R") 
		}
		labels <- get_topic_labels(dataset_name, ntopics, subset_name, iter_index)
		labels.t <- labels[unique(mytopics), Label, key=Topic]

		# merge in the counts
		labels.t[, "Outlier Count"] <- mytopics.t		

		# merge in the values
		b <- aggregate(data.frame(mytopics, myvalues), by=list(mytopics),
						 FUN=c)
		labels.t <- labels.t[b, ][,mytopics:=NULL]
	
		# sort by descending outlier frequency
		labels.t <- labels.t[order(mytopics.t, decreasing=T), ]
		
		# report back
		message("Upper outliers for top-ranked topics:")
		print(labels.t)
		message(paste("Total outliers for top-ranked topic:", 
						sum(labels.t[, "Outlier Count", with=F])))
		
		# Okay,	my hypothesis is false! All sorts of topics here.
		# Interesting. Still, I may want to remove the dissertations with
		# top-ranked language topics beforehand, since they do tend to
		# dominate their dissertations.
		
		## Browse more details of these outlier dissertations
		if(!exists("get.topics4doc", mode="function")) { 
			source(file="top docs per topic.R") 
		}
		if (!remake_figs) { 
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
	
	message(paste("Stats for contributions of topics at various ranks",
					"within dissertations:"))	
	return(stats)
}

if(autorun) {
	remake_figs
	topic.proportions(subset_name="realconsorts")
	topic.proportions(explore.outliers=T)
	# abline(0.11384, 0, col="#aa0000")
}
