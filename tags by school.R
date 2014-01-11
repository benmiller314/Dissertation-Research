## GOAL: Given a tagged set of dissertation data and a tagging schema, aggregate tag frequency and distribution at each school in the dataset. After building the function for the analysis, run it on various subsets of data and tags.

require(doBy)
require(cluster)
require(RColorBrewer)

# make sure we've run dataprep.R
if(!exists("noexcludes")) {
	source(file="dataprep.R")
}

# set broad parameters
myCol <- brewer.pal(9, "PuRd")

# open wrapper function
schoolwise <- function(dataset_name, tagset_name, agfixedcols=NULL, difixedcols=NULL) {
	
	# 0. convert variable names to variables. we'll use the names later in the figure titles.
	dataset <- get(dataset_name)
	tagset <- get(tagset_name)
	
	# 1. do the summary (tag mean) of each method type for all schools. 
	d1 <- summaryBy(. ~ School, data=dataset)
	
	# 2. get more meaningful row names (and a purely numerical matrix, for heatmapping)
	# Note that the first column will always be the list of schools because of the query in step 1.
	row.names(d1) <- d1[,1]
	
	# 3. remove columns other than method tags, again for the heatmap, and matricize	
	m1 <- d1[,which(names(d1) %in% tagset)]
	m2 <- data.matrix(m1)
	
	# 4. make the heatmap: use pre-determined columns if need be.

	# 4a. agglomerative clustering (agnes) first:
	filename <- paste0(imageloc, "tags by schools, ", dataset_name, ", N", nrow(dataset), ", agnes.pdf")
	maintitle <- paste0("Method Tag Averages by school, ", dataset_name, ", ", tagset_name)
	
	if(remake_figs) {pdf(file=filename)}
		if(!is.null(agfixedcols)) {
			ag <- heatmap.fixedcols(m2, myColInd=agfixedcols, hclustfun=function(d){agnes(d,method="ward")}, scale="row", col=myCol, main=maintitle)
		} else {
			ag <- heatmap(m2, hclustfun=function(d){agnes(d,method="ward")}, scale="row", col=myCol, main=maintitle)
		}
		mtext("Each cell gives the likelihood that a given dissertation from the school in row Y is tagged with the method in column X.",side=1)
	if(remake_figs) {dev.off()}
	
	# 4b. now divisive clustering (diana):
	filename <- paste0(imageloc, "tags by schools, ", dataset_name, ", N", nrow(dataset), ", diana.pdf")
	maintitle <- paste0("Method Tag Averages by school, ", dataset_name, ", ", tagset_name)
	
	if(remake_figs) {pdf(file=filename)}
		if(!is.null(fixedcols)) {
			di <- heatmap.fixedcols(m2, myColInd=difixedcols, hclustfun=function(d){diana(d,metric="ward")}, scale="row", col=myCol, main=maintitle)
		} else {
			di <- heatmap(m2, hclustfun=function(d){diana(d,metric="ward")}, scale="row", col=myCol, main=maintitle)
		}
		mtext("Each cell gives the likelihood that a given dissertation from the school in row Y is tagged with the method in column X.",side=1)
	if(remake_figs) {dev.off()}
	

	# save the row and column orders to allow for consistent sorting later
	return(list("ag" = ag, "di" = di))
	
# close wrapper function
}


# call the functions for all relevant datasets
a <- schoolwise("consorts","meannames")
schoolwise("nonconsorts","meannames", agfixedcols=a$ag$colInd, difixedcols=a$di$colInd)
schoolwise("noexcludes", "meannames", agfixedcols=a$ag$colInd, difixedcols=a$di$colInd)

# next up: re-run with the simplified schema
b <- 



# remove interim variables
rm(m1, m2, m3, noex.by.school.m, nonconsorts.by.school.m, consorts.by.school.m)
