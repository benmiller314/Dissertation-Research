require(doBy)
require(cluster)
require(RColorBrewer)

if(!exists("noexcludes")) {
	source(file="dataprep.R")
}

## TO DO: create wrapper function that lets us specify data and tags in one location only, to avoid careless errors in the future as we add new data sets (e.g. with the simplified tagging system)

# temporarily fix parameters for testing
dataset_name <- "noexcludes"
tagset_name <- "meannames"
myCol <- brewer.pal(9, "PuRd")

# open wrapper function
schoolwise <- function(dataset_name, tagset_name, fixedcols=NULL) {
	
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
		if(!is.null(fixedcols)) {
			ag <- heatmap.fixedcols(m2, myColInd=fixedcols, hclustfun=function(d){agnes(d,method="ward")}, scale="row", col=myCol, main=maintitle)
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
			di <- heatmap.fixedcols(m2, myColInd=fixedcols, hclustfun=function(d){diana(d,metric="ward")}, scale="row", col=myCol, main=maintitle)
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


# # next step: just consortium schools
# summaryBy(. ~ School,data=consorts) -> consorts.by.school
# row.names(consorts.by.school) <- consorts.by.school[,1]
# consorts.by.school <- consorts.by.school[,2:ncol(consorts.by.school)]
# m2 <- consorts.by.school[,which(names(consorts.by.school) %in% meannames)]
# data.matrix(m2) -> consorts.by.school.m


# # and now just non-consortium schools
# summaryBy(. ~ School,data=nonconsorts) -> nonconsorts.by.school
# row.names(nonconsorts.by.school) <- nonconsorts.by.school[,1]
# nonconsorts.by.school <- nonconsorts.by.school[,2:ncol(consorts.by.school)]
# m3 <- nonconsorts.by.school[,which(names(nonconsorts.by.school) %in% meannames)]
# data.matrix(m3) -> nonconsorts.by.school.m


# ## And, finally, make figures. 

# # Consortium schools only
# consort.count <- nrow(consorts)

# filename <- paste0(imageloc, "tags by consortium schools, N",consort.count," agnes.pdf")
# if(remake_figs) {pdf(file=filename)}

# ag <- heatmap(consorts.by.school.m, hclustfun=function(d){agnes(d,method="ward")},scale="row",col=brewer.pal(9,"PuRd"),main="Method Tag Averages by School, consortium schools only")
# mtext("Each cell gives the likelihood that a given dissertation from the school in row Y is tagged with the method in column X.",side=1)
# if(remake_figs) {dev.off()}

	# # and, again, some divisive clustering just to compare
# filename <- paste0(imageloc, "tags by consortium schools, N",consort.count," diana.pdf")
# if(remake_figs) {pdf(file=filename)}
# di <- heatmap(consorts.by.school.m, hclustfun=function(d){diana(d,metric="ward")}, scale="row", col=brewer.pal(9,"PuRd"), main="Method Tag Averages by School, consortium schools only")
# mtext("Each cell gives the likelihood that a given dissertation from the school in row Y is tagged with the method in column X.",side=1)
# if(remake_figs) {dev.off()}

# # Non-Consortium schools only
# nonconsort.count <- nrow(nonconsorts)

filename <- paste0(imageloc, "tags by non-consortium schools, N",nonconsort.count," agnes.pdf")
if(remake_figs) {pdf(file=filename)}
heatmap.fixedcols(nonconsorts.by.school.m, myColInd=ag$colInd, hclustfun=function(d){agnes(d,method="ward")}, scale="row", col=brewer.pal(9,"PuRd"), main="Method Tag Averages by School, non-consortium schools only")
mtext("Each cell gives the likelihood that a given dissertation from the school in row Y is tagged with the method in column X.",side=1)
if(remake_figs) {dev.off()}

	# # and, again, some divisive clustering just to compare
# filename <- paste0(imageloc, "tags by non-consortium schools, N",nonconsort.count," diana.pdf")
# if(remake_figs) {pdf(file=filename)}
# heatmap.fixedcols(nonconsorts.by.school.m,myColInd=di$colInd, hclustfun=function(d){diana(d,metric="ward")}, scale="row", col=brewer.pal(9,"PuRd"), main="Method Tag Averages by School, non-consortium schools only")
# mtext("Each cell gives the likelihood that a given dissertation from the school in row Y is tagged with the method in column X.",side=1)
# if(remake_figs) {dev.off()}


# # All schools
# diss.count <- nrow(noexcludes)

# filename <- paste0(imageloc, "tags by all schools, N", diss.count, " agnes.pdf")
# if(remake_figs) {pdf(file=filename)}
	# # bind the first heatmap to a variable, so we can be consistent about the order
	# heatmap.fixedcols(noex.by.school.m, myColInd=ag$colInd, hclustfun=function(d){agnes(d,method="ward")},scale="row",col=brewer.pal(9,"PuRd"),main="Method Tag Averages by School, all schools")
	# mtext("Each cell gives the likelihood that a given dissertation from the school in row Y is tagged with the method in column X.",side=1)
# if(remake_figs) {dev.off()}

	# # and some divisive clustering just to compare
# filename <- paste0(imageloc, "tags by all schools, N",diss.count," diana.pdf")
# if(remake_figs) {pdf(file=filename)}
# heatmap.fixedcols(noex.by.school.m, myColInd=di$colInd, hclustfun=function(d){diana(d,metric="ward")},scale="row",col=brewer.pal(9,"PuRd"),main="Method Tag Averages by School, all schools")
# mtext("Each cell gives the likelihood that a given dissertation from the school in row Y is tagged with the method in column X.",side=1)
# if(remake_figs) {dev.off()}



# remove interim variables
rm(m1, m2, m3, noex.by.school.m, nonconsorts.by.school.m, consorts.by.school.m)
