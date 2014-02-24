## GOAL: Given a tagged set of dissertation data and a tagging schema, aggregate tag frequency and distribution at each school in the dataset. After building the function for the analysis, run it on various subsets of data and tags.

require(doBy)
require(cluster)
require(RColorBrewer)

# make sure we've run dataprep.R
if(!exists("imageloc")) {
	source(file="dataprep.R")
}

# set broad parameters
myCol <- brewer.pal(9, "PuRd")

# function for getting data
schoolwise.data <- function(dataset_name="consorts", tagset_name="tagnames") {
	
	# 0. convert variable names to variables. we'll use the names later in the figure titles.
	dataset <- get(dataset_name, envir=parent.frame())
	tagset <- get(tagset_name, envir=parent.frame())
	tagset.mean <- sapply(tagset, FUN=function(x) paste0(x,".mean"))
	tagset.sum <- sapply(tagset, FUN=function(x) paste0(x,".sum"))
	
	# 1. remove columns other than method tags and school
	dataset <- dataset[, which(names(dataset) %in% c("School", tagset))]
	
	# 2. do the summary of each method type for all schools. 
	d1 <- summaryBy(. ~ School, data=dataset, FUN=mean)
	d2 <- summaryBy(. ~ School, data=dataset, FUN=sum)
	d3 <- summaryBy( ~ School, data=dataset, FUN=length)
		
	return(list("means" = d1, "sums" = d2, "counts" = d3))
}

# function for graphing data
schoolwise <- function(dataset_name="noexcludes", tagset_name="tagnames", agfixedcols=NULL, difixedcols=NULL, agn=TRUE, hcl=TRUE, dia=TRUE) {
	
	# 0. convert variable names to variables. we'll use the names later in the figure titles.
	dataset <- get(dataset_name)
	tagset <- get(tagset_name)
	tagset <- sapply(tagset, FUN=function(x) paste0(x,".mean"))
	
	# 1-2 call the data-grabbing function
	m2 <- schoolwise.data(dataset_name, tagset_name)$means

	# 3. get more meaningful row names (and a purely numerical matrix, for heatmapping)
	# Note that the first column will always be the list of schools because of the query in step 2.
	row.names(m2) <- m2[,1]; m2 <- m2[,2:ncol(m2)]
	m2 <- data.matrix(m2)
	
	
	# 4. make the heatmap: use pre-determined columns if need be.

	# 4a. agglomerative clustering (agnes) first:
	if(agn) {
		filename <- paste0(imageloc, "tags by schools, ", dataset_name, ", N", nrow(dataset), ", ", tagset_name, ", agnes.pdf")
	maintitle <- paste0("Method Tag Averages by school, ", dataset_name, ", ", tagset_name)
	

	if(remake_figs) {pdf(file=filename)}
		if(!is.null(agfixedcols)) {
			ag <- heatmap.fixedcols(m2, myColInd=agfixedcols, hclustfun=function(d){agnes(d,method="ward")}, scale="row", col=myCol, main=maintitle)
		} else {
			ag <- heatmap(m2, hclustfun=function(d){agnes(d,method="ward")}, scale="row", col=myCol, main=maintitle)
		}
		mtext("Each cell gives the likelihood that a given dissertation from the school in row Y is tagged with the method in column X.",side=1)
	if(remake_figs) {dev.off()}
	}
	
	# 4b. now divisive clustering (diana):
	if(dia) {
	filename <- paste0(imageloc, "tags by schools, ", dataset_name, ", N", nrow(dataset), ", ", tagset_name, ", diana.pdf")
	maintitle <- paste0("Method Tag Averages by school, ", dataset_name, ", ", tagset_name)
	
	if(remake_figs) {pdf(file=filename)}
		if(!is.null(difixedcols)) {
			di <- heatmap.fixedcols(m2, myColInd=difixedcols, hclustfun=function(d){diana(d,metric="ward")}, scale="row", col=myCol, main=maintitle)
		} else {
			di <- heatmap(m2, hclustfun=function(d){diana(d,metric="ward")}, scale="row", col=myCol, main=maintitle)
		}
		mtext("Each cell gives the likelihood that a given dissertation from the school in row Y is tagged with the method in column X.",side=1)
	if(remake_figs) {dev.off()}
	}
		
		# 4c. agglomerative clustering via hclust:
	if(hcl) {
	filename <- paste0(imageloc, "tags by schools, ", dataset_name, ", N", nrow(dataset), ", ", tagset_name, ", hclust.pdf")
	maintitle <- paste0("Method Tag Averages by school, ", dataset_name, ", ", tagset_name)
	

	if(remake_figs) {pdf(file=filename)}
		if(!is.null(agfixedcols)) {
			hc <- heatmap.fixedcols(m2, myColInd=agfixedcols, scale="row", col=myCol, main=maintitle)
		} else {
			hc <- heatmap(m2, scale="row", col=myCol, main=maintitle)
		}
		mtext("Each cell gives the likelihood that a given dissertation from the school in row Y is tagged with the method in column X.",side=1)
	if(remake_figs) {dev.off()}
	}

	if(!exists("di", inherits=F)) di <- noquote("Not run")
	if(!exists("ag", inherits=F)) ag <- noquote("Not run")
	if(!exists("hc", inherits=F)) hc <- noquote("Not run")

	# save the row and column orders to allow for consistent sorting later
	return(list("ag" = ag, "di" = di, "hc" = hc))
	
# close wrapper function
}


# call the functions for all relevant datasets
schoolwise("consorts", "tagnames")
schoolwise("nonconsorts", "tagnames")
schoolwise("noexcludes", "tagnames")
# schoolwise("nonconsorts", "tagnames", agfixedcols=a$ag$colInd, difixedcols=a$di$colInd)

# next up: re-run with the simplified schema
schoolwise("consorts", "tagnames.simple")
schoolwise("nonconsorts", "tagnames.simple")
schoolwise("noexcludes", "tagnames.simple")
# schoolwise("consorts", "tagnames.simple", agfixedcols=c$ag$colInd, difixedcols=c$di$colInd)
# schoolwise("nonconsorts", "tagnames.simple", agfixedcols=c$ag$colInd, difixedcols=c$di$colInd)

# # explore the data
# d <- order(c$byschool$Aggreg.mean, decreasing=TRUE)
# schoolwise("consorts", "tagnames.simple", agfixedcols=d, difixedcols=d)

# c$byschool[c$di$rowInd, which(names(c$byschool) %in% sapply(tagnames.simple, FUN=function(x) paste0(x,".mean")))]
# which(rowsum(c$byschool, row.names(c$byschool)) == 0)
# ?rowsum
# # remove interim variables
# rm(m1, m2, m3, noex.by.school.m, nonconsorts.by.school.m, consorts.by.school.m)
