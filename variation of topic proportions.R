## Goal: Find out the curve of topic strengths within each document, 
#  i.e. how much of the document is the top topic? how much is the second? and so on,
#  aggregated over all documents, as a boxplot of contribution (y-axis) sorted by topic rank (x-axis).
#  Rationale: I want to know at what level to cut off "cotopics": what's a realistic scenario?

topic.proportions <- function(dataset_name="consorts", ntopics=55, bad.topics=NULL, explore.outliers=F) {
	require(data.table)
	if(!exists("get.doctopic.grid", mode="function")) { source("get doctopic grid.R") }
	grid <- data.table(get.doctopic.grid()$outputfile, key="Pub.number")
	str(grid)
	head(grid)
	
	# Exclude non-content-bearing topics
	if(is.null(bad.topics) && dataset_name=="consorts" && ntopics==55) { 
		bad.topics <- c("4", "47", "22", "2", "24", "50", "13") 
	}
	grid.clean <- grid[, !(names(grid) %in% c(bad.topics, "Pub.number")), with=F]
	head(grid.clean)

		
	# decreasing sort across each row -- ignore column (i.e. topic) names
	grid.sorted <- t(apply(grid.clean, 1, FUN=function(x) { sort(x, decreasing=T) }))

	# each row is a dissertation; we lose topic numbers, but now column 1 is the weight of the top-ranked topic 
	# for that row, column 2 the weight of the 2nd-ranked topic, and so on. 
	# Let's look at the 10 top-ranked topics for every dissertation.
	head(grid.sorted[, 1:10])
	
	
	# Time to make the plot
	maintitle <- "Variation of Topic Proportions, Top 10 Topics per Document"
	subtitle <- paste0(dataset_name, ", N=", nrow(get(dataset_name)))

	if(remake_figs) { 
		pdf(file=paste0(imageloc, maintitle, ".pdf")) 
	}
		boxplot(grid.sorted[, 1:10], cex.axis=0.6, las=1, main=maintitle, sub=subtitle, xlab="Topic Rank", ylab="Portion of Document (scaled to 1)", yaxp=c(0, 1, 10))
		# abline(h=0.12) 	# includes top three quartiles for the 2nd-ranked topic, but only the top quartile for 3rd
	if(remake_figs) { dev.off() }


	## Optionally extract top-topic outliers for further examination
	if(explore.outliers) {
		upper.whisker <- boxplot.stats(grid.sorted[, 1])$stats[5]
		outliers.index <- which(grid.sorted[, 1] > upper.whisker)
		outliers <- cbind(grid[outliers.index, "Pub.number", with=F], grid.sorted[outliers.index, 1:10])
		outliers <- outliers[order(outliers$V1, decreasing=T), ]
		
		# boxplot(outliers[, 2:ncol(outliers)])
		## Browse details of these outlier dissertations
		if(!exists("get.topics4doc", mode="function")) { source(file="top docs per topic.R") }
		if (!remake_figs) { 
			a <- readline("Press <enter> for more detail on these docs, or S to skip to the end\n") 
		} else { 
			a <- ""
		}

		while (tolower(a) != "s") {
			for(i in outliers$Pub.number) {
				print(get.topics4doc(i, dataset_name, ntopics))
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

		
		
		
		
		
	}
}