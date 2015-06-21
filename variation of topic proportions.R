## Goal: Find out the curve of topic strengths within each document, 
#  i.e. how much of the document is the top topic? how much is the second? and so on,
#  aggregated over all documents, as a boxplot of contribution (y-axis) sorted by topic rank (x-axis).
#  Rationale: I want to know at what level to cut off "cotopics": what's a realistic scenario?

topic.proportions <- function(dataset_name	   = "consorts", 
							  ntopics		   = 55, 
							  bad.topics	   = NULL, 			# uses defaults if default dataset and ntopics are used
							  use.notch		   = FALSE,			# draw notch in barplot to check for overlap?
							  explore.outliers = FALSE) 
{
	require(data.table)
	if(!exists("get.doctopic.grid", mode="function")) { source("get doctopic grid.R") }
	grid <- data.table(get.doctopic.grid()$outputfile, key="Pub.number")
	# str(grid)
	head(grid)
	
	# Exclude non-content-bearing topics
	if(is.null(bad.topics) && dataset_name=="consorts" && ntopics==55) { 		# defaults
		bad.topics <- c("4", "47", "22", "2", "24", "50", "13") 
	}
	grid.clean <- grid[, !(names(grid) %in% c(bad.topics, "Pub.number")), with=F]
	print(head(grid.clean))

		
	# decreasing sort across each row -- ignore column (i.e. topic) names
	grid.sorted <- t(apply(grid.clean, 1, FUN=function(x) { sort(x, decreasing=T) }))

	# each row is a dissertation; we lose topic numbers, but now column 1 is the weight of the top-ranked topic 
	# for that row, column 2 the weight of the 2nd-ranked topic, and so on. 
	# Let's look at the 10 top-ranked topics for every dissertation.
	print(head(grid.sorted[, 1:10]))
	
	stats <- data.frame()		# start empty, build up
	for (i in 1:3) {
		# message(paste0("Stats for ", i, "-ranked topic within dissertations:"))
		stats <- rbind(stats, boxplot.stats(grid.sorted[, i])$stats)
	}

	# lower whisker, lower ‘hinge’, median, upper ‘hinge’, upper whisker
	names(stats) <- c("lower", "Lhinge", "median", "Uhinge", "upper")
	stats <- cbind("rank of topic within diss"=c(1, 2, 3), stats)
	
	# we'll return the stats data.frame later.

	
	
	## Time to make the plot
	maintitle <- "Variation of Topic Proportions, Top 10 Topics per Document"
	subtitle <- paste0(dataset_name, ", N=", nrow(grid))

	if(remake_figs) { 
		pdf(file=paste0(imageloc, maintitle, ".pdf")) 
	}
		boxplot(grid.sorted[, 1:10], cex.axis=1, las=1, main=maintitle, sub=subtitle, xlab="Topic Rank", ylab="Portion of Document (scaled to 1)", yaxp=c(0, 1, 10), notch=T)
		# abline(h=0.12) 	# includes top three quartiles for the 2nd-ranked topic, but only the top quartile for 3rd
	if(remake_figs) { dev.off() }


	## Optionally extract top-topic outliers for further examination
	if(explore.outliers) {
		upper.whisker <- boxplot.stats(grid.sorted[, 1])$stats[5]
		outliers.index <- which(grid.sorted[, 1] > upper.whisker)		# just look at #1 topic
		outliers <- cbind(grid[outliers.index, "Pub.number", with=F], grid.sorted[outliers.index, 1:10])
		outliers <- outliers[order(outliers$V1, decreasing=T), ]
		
		# boxplot(outliers[, 2:ncol(outliers)])
		
		#####
		# I have a hypothesis that these are mostly language-based topics. Let's look at the top topics represented here. 
		# STRATEGY:
		# 1. For each Pub.number, get top-ranked topic number by finding the max within that row of `grid`.
		# 2. Make a table of these topic numbers.
		# 3. Retrieve the labels for each topic in the table.
		
		mytopics <- c()							# start empty and build up
		myvalues <- c()							# let's also see what those high percent-of-text values are
		for (i in outliers$Pub.number) {
			# i <- outliers$Pub.number[2] 		# testing value
			row <- grid[which(grid$Pub.number==i), 2:ncol(grid), with=F]
			mytopic <- which(row == max(row))
			mytopics <- c(mytopics, mytopic)
			myvalues <- c(myvalues, max(row))
		}

		# count 'em up
		mytopics.t <- table(mytopics)

		# get labels
		if(!exists("get_topic_labels", mode="function")) { source(file="get topic labels.R") }
		labels <- get_topic_labels(dataset_name, ntopics)
		labels.t <- labels[unique(mytopics), Label, key=Topic]

		# merge in the counts
		labels.t[, "Outlier Count"] <- mytopics.t		

		# merge in the values
		b <- aggregate(data.frame(mytopics, myvalues), by=list(mytopics), FUN=c)
		labels.t <- labels.t[b, ][,mytopics:=NULL]
	
		# sort by descending outlier frequency
		labels.t <- labels.t[order(mytopics.t, decreasing=T), ]
		
		# report back
		message("Upper outliers for top-ranked topics:")
		print(labels.t)
		message(paste("Total outliers for top-ranked topic:", sum(labels.t[, "Outlier Count", with=F])))
		
		# Okay,	my hypothesis is false! All sorts of topics here. Interesting. 
		# Still, I may want to remove the language topics, since they do tend to dominate their dissertations.
		
		
		
		
		## Browse details of these outlier dissertations
		if(!exists("get.topics4doc", mode="function")) { source(file="top docs per topic.R") }
		if (!remake_figs) { 
			a <- readline("Press <enter> for more detail on these docs, or S to skip to the end\n") 
		} else { 
			a <- ""
		}

		while (tolower(a) != "s") {
			for(i in outliers$Pub.number) {
				print(get.topics4doc(i, dataset_name, ntopics, showlabels=TRUE))
				if (!remake_figs) { 
					a <- readline("Press <enter> for next doc, D for more details, or S to skip to the end\n") 
				} else { 
					a <- ""
				}
				
				if (tolower(a) == "s") { 
					break 
				} else if (tolower(a) == "d") { 
					print(noexcludes.dt[i]) 
					a <- readline("Press <enter> for next doc or S to skip to the next topic\n")
				}
			}
			a <- "s"
		}

		
	} # end if(explore.outliers)
	
	message("Stats for contributions of topics at various ranks within dissertations:")	
	return(stats)
}

if(autorun) {
	remake_figs
	topic.proportions()
	topic.proportions(explore.outliers=T)
}
