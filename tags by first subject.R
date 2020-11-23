# GOAL: add 'First Subject' column to noexcludes, aggregate tags by it
# (This file assumes you have already run 'dataprep.R')

### make a wrap-around function for easier switching between (e.g.) noexcludes and consorts
subjects.heatplot <- function(data=noexcludes, dataname="All Schools") {
	# drill down to subjects, and make sure we can work with this data
	
	s <- data[,which(names(data) == "Subject")]
	if(!is.character(s)) { s <- as.character(s);}

## get the first subject terms
	# unsplit the subjects, store them as lists in a big list. indices are the same as in original data.
	s2 <- lapply(s, FUN=function(x) unlist(strsplit(x,"|",fixed=TRUE)));
	rm(s)		# save memory

	# now from each list-item in the big list, extract just the first subject terms. 
	# store those in a big list also.
	s3 <- sapply(s2, FUN=function(x) x[1])
	rm(s2)		# save memory

	# refactor the terms for ease of sorting further on
	s4 <- factor(s3)
				# don't delete s3, we'll need it for indexing the original data
	
## TO DO: aggregate method tags according to these first subjects
	# make our lives a little easier
	attach(data)
	
	# first, get a raw count
		agg <- function(tag) {
			output <- aggregate(get(tag) ~ s4, FUN=sum)
			names(output) <- c("First Subject Term", paste0(tag, ".sum"))
			output
		}
	
		s5 <- lapply(tagnames, FUN=function(x) agg(x))
			
		s6 <- list()
		for (i in 1:length(s5)) {
			if (i == 1) {
				s6 <- s5[i]
			} else {
				s6 <- merge(s6, s5[i], all.y=TRUE)
			}
		}
	
		head(s6)
		rm(s5)	# save memory

	# next, make a heat plot
		# convert the subject terms into row names, so we can get a numeric matrix for heatmap()
		row.names(s6) <- s6[,1]
		s7 <- s6[,2:ncol(s6)]
		s7 <- data.matrix(s7)
		
		require(cluster)
		require(RColorBrewer)
		
		# this first heatmap is just exploratory
		main <- paste("Exploratory Heatmap of Method Tag Counts,","\n","Aggregated by First Subject Tag")
		h <- heatmap(s7, hclustfun=function(d) agnes(d, method="ward"), scale="row", col=brewer.pal(9,"PuRd"), main=main, sub=dataname)
		h

		row.names(s7)[h$rowInd]


	# TO DO: figure out how to find diss.count and scale by it
		term <- s6[1,1]
		get_dc <- function(term) {
			index <- which(s3 == term)
			dc <- nrow(data[index,])
			return(dc)
		}
		
		dcs <- lapply(row.names(s7), FUN=function(x) get_dc(x))
		dcs.i <- as.integer(dcs)
		s8 <- cbind(s7, "DissCount"=dcs.i)
		head(s8)
		
		find_pcts <- function (tag, data=s8) {
			pcts <- (data[,tag] / data[,"DissCount"]) * 100
			pcts <- as.real(pcts)
			pcts <- round(pcts,0)
		
			data <- cbind(data, pcts)
			
			label <- paste0(tag,".pct")
			names(data)[ncol(data)] <- label
		}
				
		s9 <- lapply(sumnames,function(x) scale.by.dc(x, s8))   ## This is where I was up to ##
		head(s9)
		
		s10 <- list()
		for (i in 1:length(s9)) {
			if (i == 1) {
				s10 <- s9[i]
			} else {
				s10 <- merge(s10, s9[i], all.y=TRUE)
			}
		}
		head(s10)
		
	# and now that we're done with it...
	detach(data)


### close the wrap-around function and run it
}

subjects.heatplot(noexcludes, dataname="All Schools")
subjects.heatplot(consorts, dataname="Consortium Schools Only")