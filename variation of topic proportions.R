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


}