## Topic/Method Correlation
#  GOAL: Given a topic of interest, find the methods used in disserations that focus on that topic.


methods_for_topic <- function(	dataset_name = "consorts",
								tagset_name  = "tagnames",
								ntopics		 = 55,		
								mytopic 	 = 32,		# the top topic, 'students in the classroom', as numeric
								cutoff 		 = 0.25,	# what fraction of the diss must this topic contribute
														# for the diss to count as 'in' the topic?
								do.barplot   = TRUE) {	# should we plot the results, or compute silently?

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
	if(!exists("get_topic_labels", mode="function")) { source(file="get topic labels.R") }

	label <- get_topic_labels(dataset_name, ntopics)[mytopic, Label]
	main <- paste0("Frequency of Assigned Method Tags for Topic ", mytopic, " (", label, ")")

	if(autorun && do.barplot) {
		if (remake_figs) { filename <- paste0(imageloc, main, ".pdf"); pdf(file=filename) }
			barplot(a5[order(a5)], horiz=TRUE, xpd=FALSE, las=1, axes=FALSE, main=main, col="gray80")
			text(x      = rep(2.5, length(a5)), 
				 y      = seq(from=0.7, to=length(tagset)+2.5, length.out=length(tagset)), 
				 labels = a5[order(a5)])
		if (remake_figs) { dev.off() }
	}

	return(list("set"=subdata, "tags"=a5))
}

if (autorun) {
	lapply(c(32, 8, 48, 15, 10), FUN=function(x) { methods_for_topic(mytopic=x) })
}


# Side-by-side comparison of method ranks for two topics
if(autorun) {
	if(!exists("dataset_name")) { dataset_name <- "consorts"; ntopics <- 55} 
	if(!exists("compare_method_ranks", mode="function")) { source(file="compare method ranks.R") }
	if(!exists("get_topic_labels", mode="function")) { source(file="get topic labels.R") }

	label1 <- paste("32:", get_topic_labels(dataset_name, ntopics)[32, Label])
	label2 <- paste("8:", get_topic_labels(dataset_name, ntopics)[8, Label])

	tags.topic32 <- as.data.frame(methods_for_topic(mytopic=32, do.barplot=F)$set)
	tags.topic8 <- as.data.frame(methods_for_topic(mytopic=8, do.barplot=F)$set)

	compare_method_ranks(set1="tags.topic32", set2="tags.topic8", pcts=T, colorful=T, betterlabels=c(label1, label2))

	# topic cluster 1of10
	grp1of10 <- c(32, 1, 39, 41, 40)
	pairs <- combn(grp1of10, 2)
	
	for (i in 1:ncol(pairs)) {
		topic_1 <- pairs[, i][1]
		topic_2 <- pairs[, i][2]

		set_1 <- as.data.frame(methods_for_topic(mytopic=topic_1, do.barplot=F)$set)
		set_2 <- as.data.frame(methods_for_topic(mytopic=topic_2, do.barplot=F)$set)
		
		lab1 <- paste0(topic_1, ": ", get_topic_labels(dataset_name, ntopics)[topic_1, Label])
		lab2 <- paste0(topic_2, ": ", get_topic_labels(dataset_name, ntopics)[topic_2, Label])
		
		compare_method_ranks("set_1", "set_2", pcts=T, colorful=T, betterlabels=c(lab1, lab2))
	}

}


# Compare methods within cotopics
methods_in_cotopics <- function(dataset_name="consorts", ntopics=55, focal_topic=NULL, level=0.25, min=1) {
	if(!exists("get.cotopics", mode="function")) { source(file="cotopics.R") }
	if(!exists("compare_method_ranks", mode="function")) { source(file="compare method ranks.R") }
	if(!exists("get_topic_labels", mode="function")) { source(file="get topic labels.R") }

	# find out which topics co-occur within individual dissertations
	test <- get.cotopics(dataset_name, ntopics, level)
	
	# if we're focusing on one topic, find just that topic (whether in source or target)	
	if(!is.null(focal_topic)) { 
		left <- test[(source==focal_topic), ]
		right <- test[(target==focal_topic), ]
		right_flip <- data.table(source=right$target, target=right$source, weight=right$weight)
		test <- rbind(left, right_flip)
		test
		rm(left, right, right_flip)
	}

	# to reduce complexity, set a minimum number of co-occurrences
	test <- test[which(weight > min), ]

	setkey(test, source, weight)
	test[order(-weight, source)]
	print(test)

	
	for (i in nrow(test):1) {
		topic_1 <- test[i, source]
		topic_2 <- test[i, target]

		set_1 <- as.data.frame(methods_for_topic(mytopic=topic_1, do.barplot=F)$set)
		set_2 <- as.data.frame(methods_for_topic(mytopic=topic_2, do.barplot=F)$set)
		
		lab1 <- paste0(topic_1, ": ", get_topic_labels(dataset_name, ntopics)[topic_1, Label])
		lab2 <- paste0(topic_2, ": ", get_topic_labels(dataset_name, ntopics)[topic_2, Label])
		
		compare_method_ranks("set_1", "set_2", pcts=T, colorful=T, betterlabels=c(lab1, lab2))
		
		title(main="Method ranks in co-occurring topics", sub=paste("# of co-occurrences:", test[i, weight]))
	}
}
methods_in_cotopics(focal_topic=32)

methods_in_cotopics_3d <- function(dataset_name="consorts", tagset_name="tagnames", ntopics=55, focal_topic=NULL, level=0.25) {
	if(!exists("get.cotopics", mode="function")) { source(file="cotopics.R") }
	if(!exists("get_topic_labels", mode="function")) { source(file="get topic labels.R") }

	# find out which topics co-occur within individual dissertations
	test <- get.cotopics(dataset_name, ntopics, level)
	
	# if we're focusing on one topic, find just that topic (whether in source or target)
	if(!is.null(focal_topic)) { 
		left <- test[(source==focal_topic), ]
		right <- test[(target==focal_topic), ]
		right_flip <- data.table(source=right$target, target=right$source, weight=right$weight)
		test <- rbind(left, right_flip)
		setkey(test, weight)
		print(test)
		rm(left, right, right_flip)
	}

	# now that we know the dimensions of the data, let's build a matrix. We'll start blank, then loop.
	mat <- matrix(dimnames=list(tagnames), nrow=length(get(tagset_name)), ncol=(1+nrow(test)) )	
	dat <- data.table(NA)
	
		# add the focal topic, if there is one
		if(!is.null(focal_topic)) {
			mat[, ncol(mat)] <- methods_for_topic(mytopic=focal_topic, do.barplot=F)$tags
		}
		
		# now loop through the cotopics (or all topics--needs work)
		for (i in nrow(test):1) {
			topic <- test[i, target]
			mat[, i] <- methods_for_topic(mytopic=topic, do.barplot=F)$tags
			dat <- data.table(dat, topic=methods_for_topic(mytopic=topic, do.barplot=F)$tags)
			names(dat) <- c(test[1, source], test[, target])
		}
		
		lab1 <- paste0(topic_1, ": ", get_topic_labels(dataset_name, ntopics)[topic_1, Label])
		lab2 <- paste0(topic_2, ": ", get_topic_labels(dataset_name, ntopics)[topic_2, Label])
	
}
