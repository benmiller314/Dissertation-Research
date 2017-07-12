# pages.r
# GOAL: Find some simple stats about page counts

# Test values; later make these parameters
dataset_name <- "noexcludes"

# Get the data
dataset <- get(dataset_name)
pages <- dataset$Pages

# Make sure we have all numbers in our dataset
if(any(is.na(pages))) {
	pages <- pages[-which(is.na(pages))]
}

	# Okay, this is maddening: typeof(pages)=integer, but summary(pages) treats it as a factor.
	# And as.integer(pages) subtracts 80 for some reason (131 becomes 51, 207 becomes 127, etc).
	# So here's a workaround:
pages2 <- as.character(pages)
pages2 <- as.integer(pages2)
sum(pages2)
table(pages2)
summary(pages2)

# get some stats; maxsum is needed to avoid "other"ing
summary(pages2, maxsum=6000)
?boxplot(summary(pages2), horizontal=T, names=)

# bin these into ranges
cut(pages, breaks=seq(100, 1000, 100))

# Clean up the workspace (only needed for testing)
rm(dataset, dataset_name)
