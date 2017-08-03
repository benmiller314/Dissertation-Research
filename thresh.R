#############################################################################
# thresh.R
#
# Provides a function to subset schoolwise data by threshold number of dissertations 
# at that school in a given timespan
#####

thresh <- function(dataset_name = "noexcludes", 
				   tagset_name = "tagnames", 
				   threshold = 5, 
				   since = 2006, 
				   until = 2010) 
{
	# load required packages
	require(doBy)
	
	# 0. convert variable names to variables. we'll use the names later in
	# the figure titles.
	dataset <- get(dataset_name)
	tagset <- get(tagset_name)
	
	# 1. subset the data for the desired years
	d1 <- dataset[which((dataset$Year >= since) & (dataset$Year <= until)),]
	
	# 1b. summarize that data by school, counting rows
	d2 <- summaryBy(. ~ School, data=d1, FUN=length)

	# 1c. find the schools in that time period with more than threshold
	# (default=5)
	d3 <- d2[which(d2$Year.length >= threshold), "School"]
	thresh.report <- paste0("Found ", length(d3), " schools (out of ",
							 nrow(d2), " schools in ", dataset_name, 
							 " from ", since, "-", until, ") with ",
							 threshold, " or more dissertations.")

	## 2. get full 10-year tag data for those schools
	d4 <- dataset[which(dataset$School %in% d3),]
	
	return(list("thresh.data" = d4, 
				"thresh.report" = thresh.report))
}
