## Top 10 Lists, take 2

require(doBy)

# open wrapper function
toplists <- function(dataset_name="noexcludes", tagset_name="tagnames", howmany=5, threshold=5, rank_by_pcts=TRUE) {
	
	## 0. convert variable names to variables. we'll use the names later in the figure titles.
	dataset <- get(dataset_name)
	tagset <- get(tagset_name)


	## 1. find schools with more than 5 dissertations in 2006-2010

	# 1a. subset the data for these years
	d1 <- dataset[which(dataset$Year > 2005),]
	
	# 1b. summarize that data by school, counting rows
	d2 <- summaryBy( ~ School, data=d1, FUN=length)

	# 1c. find the schools in that time period with more than threshold (default=5)
	d3 <- d2[which(d2$School.length >= threshold), "School"]
	subtitle1 <- paste0("Found ", length(d3), " schools (out of ", nrow(d2), " ", dataset_name, ") with ", threshold, " or more dissertations in 2006-2010")

	## 2. get full 10-year tag data for those schools
	d4 <- dataset[which(dataset$School %in% d3),]

	if (!exists("schoolwise.data")) { source(file="tags by school.R") }
	a <- schoolwise.data("d4",tagset_name)

	# 2a. Star the schools in the consortium
	c <- which(a$counts$School %in% consorts$School)
	a$counts$School <- fix_factor(a$counts$School, paste0(a$counts$School[c], "*"), a$counts$School[c])


	## 3. for the schools that meet the cutoff, find the "howmany" highest real values of each tag

	# 3a. Create function to apply to each tag in the tagset
	toplist.onetag <- function(a, tag, rank_by) {	
		tag.mean <- paste0(tag, ".mean")
		tag.sum <- paste0(tag, ".sum")
		
		# rank by chosen tag
		if(rank_by) {
			a1 <- order(a$means[, tag.mean], decreasing=TRUE)
		} else {
			a1 <- order(a$sums[, tag.sum], decreasing=TRUE)
		}
		
		a2 <- head(a$counts[a1,], howmany)					# raw number of disses at top schools
		a3 <- head(a$sums[a1,tag.sum], howmany)				# number of disses with chosen tag
		a4 <- head(a$means[a1, tag.mean], howmany)			# pct of disses with chosen tag
		a4 <- round(100*a4, 0)								# cleaner percentage

		# combine per-tag count and pct					
		if(rank_by) {
			a5 <- paste0(a4, "% (", a3,")")
		} else {
			a5 <- paste0(a3, " (", a4, "%)")
		}

		a6 <- cbind(a2, tag = a5)							# combine raw number with per-tag data
		names(a6) <- c("School", "Total", tag)				# get cleaner column names

		# combine School and Total, then remove Total
		a6$School <- fix_factor(a6$School, paste0(a6$School, " (", a6$Total, ")"), a6$School)						
		a6$Total <- NULL
		
		return(a6)	
	}
	
	# 3b. Apply the function to each tag in the tagset
	b <- lapply(tagset, FUN=function(x) {toplist.onetag(a=a, tag=x, rank_by=rank_by_pcts)})

	if(rank_by_pcts) {
		title <- "Top 5 Schools by Methodological Focus (Ranked by Percentage)"
	} else {
		title <- "Top 5 Schools by Methodological Focus (Ranked by Number of Dissertations)"
	}
	subtitle2 <- "* indicates member of the Consortium of Doctoral Programs in Rhetoric and Composition"
	names(b) <- tagset
	writeLines(c(title, subtitle1, subtitle2))
	print(b)

	
# close wrapper function
}

# call function
toplists(howmany=10, threshold=1)
toplists(tagset_name="tagnames", rank_by_pcts=FALSE, howmany=20, threshold=1)

# clean up working variables (only needed during testing)
rm(d1, d2, d3, d4, a, a1, a2, a3, a4, a5, a6, b, c)
