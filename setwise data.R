## Get some basic output stats for a set of schools
# TO DO: merge with 'tags by school.R', since that's really what's going on here

setwise.data <- function(dataset_name="consorts") {
	require(doBy)
	
	dataset <- get(dataset_name)

	b <- summaryBy(. ~ School, data=dataset, FUN=length)[, 1:2]
	names(b) <- c("School", "Counts")
	b <- b[order(b$Counts, decreasing=T), ]
	set.mean <- mean(b$Counts)
	set.median <- median(b$Counts)
	set.total <- sum(b$Counts)

	return(list(total = set.total, 
				mean = set.mean, 
				median = set.median))	
}

if(autorun) {
	setwise.data("consorts")
	setwise.data("top.nonconsorts")	
	setwise.data("consorts.plus")
}



plot.outputs <- function(dataset_name="consorts.plus", show.stats=TRUE, show.labels=TRUE, label.top=6,
						 subset_name=NULL, subset.labels=FALSE, subset.color="red") {
	dataset <- get(dataset_name)
	
	cfreq <- table(factor(dataset$School))
	cfreq <- sort(cfreq, decreasing=TRUE)
	main <- paste(dataset_name, "by dissertation count")

	if(remake_figs) { 
		filename <- paste0(imageloc, "dissertation counts, ", dataset_name)
		if(!is.null(subset_name)) { filename <- paste0(filename, " with ", subset_name) }
		if(show.labels) { filename <- paste0(filename, ", ", dataset_name, " labeled") }
		if(subset.labels) { filename <- paste0(filename, ", ", subset_name, " labeled") }
		filename <- paste0(filename, ".pdf")
		pdf(filename) 
	}
			plot(cfreq, type="o", pch=18, bty="n", xlab="Schools", ylab="Number of Dissertations 2001-2010")
			title(main)

		if(show.stats) { 
			legend(x=0, y=mean(cfreq)+7,
				legend=c(paste("Mean =", round(mean(cfreq),2)),
					 paste("Median =",round(median(cfreq),2))), 
				bty="n"
			) 
			abline(h = median(cfreq), col="forestgreen")
			abline(h = mean(cfreq), col="blue")
		}
		
		if(show.labels) {
			text(cfreq[1:label.top], labels=paste0(names(cfreq[1:label.top])," (",cfreq[1:label.top],")"), pos=4, offset=1)
		}
		
		if(!is.null(subset_name)) {			
			subset <- get(subset_name)
			subsetfreq <- table(factor(subset$School))
			subsetfreq <- sort(subsetfreq, decreasing=TRUE)
			
			index <- which(names(cfreq) %in% names(subsetfreq))
			
			if(!is.null(subset.color)) {
				points(x=index, 
						y=subsetfreq, 
						pch=18,
						# add=TRUE,
						# inches=FALSE,
						col=subset.color
				)
				
				legend("topright", 
					 c("Consortium", "Non-Consortium"), 
					 fill=c("black", subset.color),
					 border=c("black", subset.color),
					 bty="n",
				)
				
			}

			if(subset.labels) {
				text(subsetfreq[1:label.top], 
					 labels=paste0(names(subsetfreq[1:label.top]), " (", subsetfreq[1:label.top], ")"), 
					 pos=4, 
					 offset=8
				)
			}
		}
		
	if(remake_figs) { dev.off() }
}

if(autorun) {
	remake_figs=F
	plot.outputs("consorts.plus", show.stats=F)
	plot.outputs("consorts.plus", subset_name="top.nonconsorts", show.stats=T, show.labels=F, subset.labels=F, subset.color="red")
}