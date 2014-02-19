## Top 10 Lists, take 2

require(doBy)

# open wrapper function
topten2 <- function(dataset_name="noexcludes", tagset_name="tagnames") {
	
	## 0. convert variable names to variables. we'll use the names later in the figure titles.
	dataset <- get(dataset_name)
	tagset <- get(tagset_name)


	## 1. find schools with more than 5 dissertations in 2006-2010

	# 1a. subset the data for these years
	d1 <- dataset[which(dataset$Year > 2005),]
	
	# 1b. summarize that data by school, counting rows
	d2 <- summaryBy( ~ School, data=d1, FUN=length)

	# 1c. find the schools in that time period with more than 5
	d3 <- d2[which(d2$School.length > 5), "School"]
	print(paste("Found", length(d3), "schools in", dataset_name, "with more than 5 dissertations 2006-2010"))

	## 2. get full 10-year tag data for those schools
	d4 <- dataset[which(dataset$School %in% d3),]

	if (!exists("schoolwise.data") { source(file="tags by school.R") })
	a <- schoolwise.data("d4","tagnames")
	
	## 3. 

