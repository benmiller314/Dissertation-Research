## Topic/Method Correlation
#  GOAL: Given a topic of interest, find the methods used in disserations that focus on that topic.


methods_for_topic <- function(	dataset_name = "consorts",
								tagset_name  = "tagnames",
								ntopics		 = 55,		
								mytopic 	 = 32,		# the top topic, 'students in the classroom'
								cutoff 		 = 0.25) {	# what fraction of the diss must this topic contribute
														# for the diss to count as 'in' the topic?

	# Get the doc-topic grid
	if(!exists("get.doctopic.grid", mode="function")) { source(file="get doctopic grid.R") }

	
	grid <- data.table(get.doctopic.grid(dataset_name, ntopics)$outputfile)
		## Investigate topic-weight distribution, to verify that our cutoff makes sense
		# head(grid)
		# lapply(1:ntopics, FUN=function(x) { summary(grid[, x, with=F]) })
	setkey(grid, Pub.number)
	
	# Focus on one topic
	a1 <- grid[, c("Pub.number", as.character(mytopic)), with=F]
	
	# Extract only the docs with high levels of that topic
	index <- a1[, as.character(mytopic), with=F] > cutoff
	mydocs <- as.data.frame(a1)[index,]$Pub.number		# data.table is being super-stinky 
														# about subsetting; e.g. a1[index, ] 
														# is returning an error, even when 
														# a1 has only one column (mytopic)

	
	# Subset main dissertation data by this topic index, extract methods columns
	tagset <- get(tagset_name)
	subdata <- noexcludes.dt[Pub.number %in% mydocs, ]
	head(subdata)
	a4 <- subdata[, tagset, with=F]
	head(a4)
	
	# Sum the methods
	a5 <- apply(a4, 2, sum)
	message("Method tag frequency for ", dataset_name, " with >", cutoff*100, "% ", "topic ", mytopic, ":")
	print(a5)

	# Header information for the plot
	if(!exists("get_topic_labels")) { source(file="topics by year.R") }
	label <- get_topic_labels(dataset_name, ntopics)[mytopic, Label]
	main <- paste0("Frequency of Assigned Method Tags for Topic ", mytopic, " (", label, ")")


	if (remake_figs) { filename <- paste0(imageloc, main, ".pdf"); pdf(file=filename) }
		barplot(a5[order(a5)], horiz=TRUE, xpd=FALSE, las=1, axes=FALSE, main=main, col="gray80")
		text(x=rep(2.5, length(a5)), y=seq(from=0.7, to=length(tagset)+2.5, length.out=length(tagset)), labels=a5[order(a5)])
	if (remake_figs) { dev.off() }

	return(list("set"=subdata, "tags"=a5))
}

if (autorun) {
	lapply(c(32, 8, 48, 15, 10), FUN=function(x) { methods_for_topic(mytopic=x) })
}

##
# Side-by-side comparison of method ranks for two topics
if(!exists("compare_method_ranks", function)) { source(file="compare method ranks.R") }

tags.topic32 <- as.data.frame(methods_for_topic(mytopic=32)$set)
tags.topic8 <- as.data.frame(methods_for_topic(mytopic=8)$set)
undebug(compare_method_ranks)
compare_method_ranks("tags.topic32", "tags.topic8", pcts=F)