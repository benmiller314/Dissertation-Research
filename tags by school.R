require(doBy)
require(cluster)
require(RColorBrewer)

if(!exists("noexcludes")) {
	source(file="dataprep.R")
}

# If remake_figs is true, new pdf files will be created; otherwise, they'll display on screen only.
if(!exists("remake_figs")) {
	remake_figs <- FALSE
}

# first we do the summary (tag mean) of each method type for all schools. 
summaryBy(. ~ School, data=noexcludes) -> noexcludes.by.school

	# get more meaningful row names (and a purely numerical matrix, for heatmapping)
row.names(noexcludes.by.school) <- noexcludes.by.school[,1]
noexcludes.by.school <- noexcludes.by.school[,2:ncol(noexcludes.by.school)]

	# remove columns other than method tags, again for the heatmap, and matricize	
m1 <- noexcludes.by.school[,which(!names(noexcludes.by.school) %in% c("Year.mean","Method.Count.mean","Exclude.Level.mean"))]
data.matrix(m1) -> noex.by.school.m

# next step: just consortium schools
summaryBy(. ~ School,data=consorts) -> consorts.by.school
row.names(consorts.by.school) <- consorts.by.school[,1]
consorts.by.school <- consorts.by.school[,2:ncol(consorts.by.school)]
m2 <- consorts.by.school[,which(!names(consorts.by.school) %in% c("Year.mean","Method.Count.mean","Exclude.Level.mean"))]
data.matrix(m2) -> consorts.by.school.m


# and now just non-consortium schools
summaryBy(. ~ School,data=nonconsorts) -> nonconsorts.by.school
row.names(nonconsorts.by.school) <- nonconsorts.by.school[,1]
nonconsorts.by.school <- nonconsorts.by.school[,2:ncol(consorts.by.school)]
m3 <- nonconsorts.by.school[,which(!names(consorts.by.school) %in% c("Year.mean","Method.Count.mean","Exclude.Level.mean"))]
data.matrix(m3) -> nonconsorts.by.school.m


## And, finally, make figures. 

# Consortium schools only
consort.count <- nrow(consorts)

filename <- paste0(imageloc, "tags by consortium schools, N",consort.count," agnes.pdf")
if(remake_figs) {pdf(file=filename)}

ag <- heatmap(consorts.by.school.m, hclustfun=function(d){agnes(d,method="ward")},scale="row",col=brewer.pal(9,"PuRd"),main="Method Tag Averages by School, consortium schools only")
mtext("Each cell gives the likelihood that a given dissertation from the school in row Y is tagged with the method in column X.",side=1)
if(remake_figs) {dev.off()}

	# and, again, some divisive clustering just to compare
filename <- paste0(imageloc, "tags by consortium schools, N",consort.count," diana.pdf")
if(remake_figs) {pdf(file=filename)}
di <- heatmap(consorts.by.school.m, hclustfun=function(d){diana(d,metric="ward")}, scale="row", col=brewer.pal(9,"PuRd"), main="Method Tag Averages by School, consortium schools only")
mtext("Each cell gives the likelihood that a given dissertation from the school in row Y is tagged with the method in column X.",side=1)
if(remake_figs) {dev.off()}

# Non-Consortium schools only
nonconsort.count <- nrow(nonconsorts)

filename <- paste0(imageloc, "tags by non-consortium schools, N",nonconsort.count," agnes.pdf")
if(remake_figs) {pdf(file=filename)}
heatmap.fixedcols(nonconsorts.by.school.m, myColInd=ag$colInd, hclustfun=function(d){agnes(d,method="ward")}, scale="row", col=brewer.pal(9,"PuRd"), main="Method Tag Averages by School, non-consortium schools only")
mtext("Each cell gives the likelihood that a given dissertation from the school in row Y is tagged with the method in column X.",side=1)
if(remake_figs) {dev.off()}

	# and, again, some divisive clustering just to compare
filename <- paste0(imageloc, "tags by non-consortium schools, N",nonconsort.count," diana.pdf")
if(remake_figs) {pdf(file=filename)}
heatmap.fixedcols(nonconsorts.by.school.m,myColInd=di$colInd, hclustfun=function(d){diana(d,metric="ward")}, scale="row", col=brewer.pal(9,"PuRd"), main="Method Tag Averages by School, non-consortium schools only")
mtext("Each cell gives the likelihood that a given dissertation from the school in row Y is tagged with the method in column X.",side=1)
if(remake_figs) {dev.off()}


# All schools
diss.count <- nrow(noexcludes)

filename <- paste0(imageloc, "tags by all schools, N", diss.count, " agnes.pdf")
if(remake_figs) {pdf(file=filename)}
	# bind the first heatmap to a variable, so we can be consistent about the order
	heatmap.fixedcols(noex.by.school.m, myColInd=ag$colInd, hclustfun=function(d){agnes(d,method="ward")},scale="row",col=brewer.pal(9,"PuRd"),main="Method Tag Averages by School, all schools")
	mtext("Each cell gives the likelihood that a given dissertation from the school in row Y is tagged with the method in column X.",side=1)
if(remake_figs) {dev.off()}

	# and some divisive clustering just to compare
filename <- paste0(imageloc, "tags by all schools, N",diss.count," diana.pdf")
if(remake_figs) {pdf(file=filename)}
heatmap.fixedcols(noex.by.school.m, myColInd=di$colInd, hclustfun=function(d){diana(d,metric="ward")},scale="row",col=brewer.pal(9,"PuRd"),main="Method Tag Averages by School, all schools")
mtext("Each cell gives the likelihood that a given dissertation from the school in row Y is tagged with the method in column X.",side=1)
if(remake_figs) {dev.off()}



# remove interim variables
rm(m1, m2, m3)