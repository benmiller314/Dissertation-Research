stop("This file has been deprecated. Instead of using 'top 10 lists.R', please call the toplists() function from source(file='top 10 lists take 2.R').")


## Goal: list the 10(e.g.) most frequently occurring schools overall and by method tag

# First, make sure the data exists
if(!exists("noexcludes")) {
	source(file="dataprep.R")
}

# Set broad parameters
cutoff <- 10

# sfreq = schools by frequency
sfreq <- table(noexcludes$School)
sfreq <- sort(sfreq, decreasing=TRUE)

par(mfrow=c(1,1))

# just for fun, let's visualize all of the schools' frequencies
main <- "Dissertation Output of Schools follows a Power Law"
filename <- paste0(imageloc, main, ", N", diss.count,".pdf")
if(remake_figs) {pdf(filename)}
	plot(sfreq, type="o", pch=18, bty="n", xlab="Schools", ylab="Number of Dissertations")
	title(main)
	abline(h = median(sfreq), col="red")
	abline(h = mean(sfreq), col="blue")
	legend(x=150,y=30,legend=c(paste("Mean = ",round(mean(sfreq),4)),paste("Median = ",round(median(sfreq),4))), lwd=2, col=c("blue","red"), bty="n")
if(remake_figs) {dev.off()}

	plot(, type="o", pch=18, bty="n", xlab="Schools", ylab="Number of Dissertations", xlog=T, ylog=T)



# and again for consortium schools only
cfreq <- table(factor(consorts$School))
cfreq <- sort(cfreq, decreasing=TRUE)
main <- "Top 6 Consortium Schools Way Over the Trend"
filename <- paste0(imageloc, main, ", consortium schools, N", nrow(consorts),".pdf")
pdf(filename)
	plot(cfreq, type="o", pch=18, bty="n", xlab="Schools", ylab="Number of Dissertations 2001-2010")
	title(main)
	# abline(h = median(cfreq), col="red")
	# abline(h = mean(cfreq), col="blue")
	legend(x=50,y=40,legend=c(paste("Mean = ",round(mean(cfreq),4)),paste("Median = ",round(median(cfreq),4))), bty="n")
	text(cfreq[1:5], labels=paste0(names(cfreq[1:5])," (",cfreq[1:5],")"), pos=4, offset=1)
	text(cfreq[6], labels=paste0(names(cfreq[6])," (",cfreq[6],")"), pos=4, offset=1)
	
	

dev.off()

# overall top 10 lists
print(sfreq[1:cutoff])
print(cfreq[1:cutoff])




# ## now for the tags
# # if we've run 'map by school 1 (setup).R', then we can use the doBy summaries created there
# if(!exists("tagsums.by.school")) {
	# source(file="map by school 1 (setup).R")
# }

# # variables created by the source above should be the most up to date; csv's are for quick reference
# # tagsums.by.school <- read.csv(file=paste0(dataloc, "../tagsums by school.csv"))
# # disses.by.school <- read.csv(file=paste0(dataloc, "../disses by school.csv"))

# by.school <- merge(disses.by.school, tagsums.by.school, by="School")
# by.school$X.x <- by.school$X.y <- NULL

# attach(by.school)

# # first up, let's do raw counts:
# toplist <- function (tag, cutoff) {
	# if (!exists(tag)) {
		# head(by.school[order(tag, decreasing=TRUE), c("School", tag)], cutoff)
	# } else {
		# head(by.school[order(get(tag), decreasing=TRUE), c("School", tag)], cutoff)	
	# }
# }
# toplists <- lapply(sumnames, FUN=function(x) toplist(x, cutoff))
# filename <- paste0(dataloc, "top ",cutoff," schools by method tag.csv")
# write.csv(toplists, filename)

# ## now let's add percentages to those numbers 
# # first, find the total dissertations at each school listed.

# # these functions are updated later in the file #
# # get_totals <- function(tag) {					
	# # index <- row.names(toplists[tag][[1]])
	# # dc <- by.school[index,]$DissCount
	# # return(dc)
# # }
# # add_dc <- function (tag) {
	# # dc <- get_totals(tag)
	# # toplists[tag][[1]] <- cbind(toplists[tag][[1]], dc)
	# # return(toplists[tag][[1]])
# # }

# weight.by.dc <- function (tag) {
	# if (is.null(toplists[tag][[1]]$dc)) {		# if there's no diss.count, add it
		# toplists[tag][[1]] <- add_dc(tag)
	# } 
	# if (is.null(toplists[tag][[1]]$pct)) {		# if there's no percent, add it
		# pct <- toplists[tag][[1]][,2] / toplists[tag][[1]]$dc
		# pct <- round(pct, 2)
		# toplists[tag][[1]] <- cbind(toplists[tag][[1]], pct)
	# }
	
	# return(toplists[tag][[1]])
# }

# toplists2 <- lapply(tagnames, FUN=function(x) weight.by.dc(x))

# ## but actually we want to calculate these percentages beforehand, and sort by them. so:

# my.round <- function(a) {		
	# print(a)
	# a <- a * 100
	# print(a)
	# if (round(a,0) == a) {
		# a <- round(a, 0)					# leave whole numbers as whole numbers
	# } else if (round(a*4,0) == a*4) {
		# a <- round(a, 2)					# give two decimal places for .25 and .75
	# } else {
		# a <- round(a, 1)					# round everything else to one decimal place
	# }
# }

# find_pcts <- function (tag, data=by.school) {
	# pcts <- data[,tag] / data$DissCount
# #	pcts <- lapply(pcts, FUN=function(x) my.round(x))
	# pcts <- as.real(pcts)
	# pcts <- round(pcts,0)

	# data <- cbind(data, pcts)
	
	# label <- paste0(tag,".pct")
	# names(data)[ncol(data)] <- label

	# head(data[order(pcts, decreasing=TRUE), c("School", label)], cutoff)	
# }

# top10.all <- lapply(sumnames, FUN=function(y) find_pcts(y))

# justcons <- by.school[which(by.school$School %in% conschools),]
# top10.consorts <- lapply(sumnames, FUN=function(y) find_pcts(y, data=justcons))


# # Add back in the diss counts for these schools
# get_totals <- function(tag="Case", data=top10.consorts) {					
	# index <- row.names(data[tag][[1]])
	# dc <- by.school[index,]$DissCount
	# return(dc)
# }

# add_dc <- function (tag="Case", data=top10.consorts) {
	# dc <- get_totals(tag, data)
	# dc <- as.real(dc)
	# data[tag][[1]] <- cbind(data[tag][[1]], dc)
	# return(data[tag][[1]])
# }

# top10.consorts <- lapply(tagnames, FUN=function(x) add_dc(tag=x))


# # Hmm, that leaves us with a lot of piddly little schools dominating the stage. Let's set a minimum.
# # TO DO: apply this minimum only to the last N years (where N equals, say, 5)
# minimum <- 5 	# use this to simplify matters

# set_minimum <- function (tag, data=by.school) {
	# if(is.double(tag)) { return(tag) }
	
	# d <- data[which(data[,tag] > 0), tag]
	# s <- summary(d)

	# if (length(d) > cutoff) {
		# minimum <- s["3rd Qu."]
		# # l <- data[d > s["3rd Qu."]), c("School", tag)]		
	# } else {
		# minimum <- min(d)
		# # l <- d	
	# }

	# return(minimum)
		
	# # l <- data[which(data[,tag] > minimum), c("School", tag)]
	# # o <- order(l[tag], decreasing=TRUE)
	# # head(l[o,], cutoff)
# }

# lapply(sumnames, FUN=function(tag) set_minimum(tag))


# find_pcts <- function (tag, data=by.school) {
	# pcts <- (data[,tag] / data$DissCount) * 100

	# # pcts <- lapply(pcts, FUN=function(x) my.round(x))
	# pcts <- as.real(pcts)
	# pcts <- round(pcts,0)

	# d <- cbind(data[,c("School", tag, "DissCount")], pcts)
	# label <- paste0(tag,".pct")
	# names(d) <- c("School", tag, "DissCount", label)
	# head(d, 15)
	
	# o <- order(c(pcts, d$DissCount), decreasing=TRUE)	

	# # ans <- head(data[o, c("School", label, tag, "DissCount")], cutoff)	
	# ans <- d[o, c("School", label, tag, "DissCount")]
	# # trim <- which(ans$DissCount >= set_minimum(tag))
	# trim <- which(ans$DissCount >= set_minimum(cutoff))
	# ans.trim <- head(ans[trim,], cutoff)
	
	# # add a star for Consortium schools
	# con.index <- which(ans.trim$School %in% consortium$University)
	# ans.trim$School <- as.character(ans.trim$School)
	# ans.trim[con.index, "School"] <- paste(ans.trim[con.index, "School"], "*")

	# # 

	# # and save this tag's list before we move on
	# filename <- paste0(dataloc, "top ", cutoff, " schools by method tag ", tag, ", minimum", minimum, "disses per school.csv")
	# write.csv(ans.trim, filename)
# }

# top.schools.by.tag <- lapply(sumnames, FUN=function(x) find_pcts(x))
# filename <- paste0(dataloc, "top ", cutoff, " schools by method tag, minimum ",minimum," disses per school.csv")
# lapply(top.schools.by.tag, FUN=function(x) write.csv(x, append=TRUE))

# # lapply(sumnames, FUN=function(x) find_pcts(x, justcons))	# not working yet
# # lapply(top10.consorts.trimmed, FUN=function(x) write.csv(x[[1]], filename, append=TRUE)) # not working yet

# data <- justcons		# a testing variable
# head(data)


# detach(by.school)

# ## finally, I want to list all schools alphabetically, with diss counts, and an extra star for consorts.
# # the first part is easy, because we did it already:
# head(disses.by.school)

# # now let's add those stars:
# consortium$University <- fix_factor(consortium$University, to.remove="University of Nebraska at Omaha", to.add="University of Nebraska-Lincoln")
# con.index <- which(disses.by.school$School %in% consortium$University)
# disses.by.school$School <- as.character(disses.by.school$School)
# disses.by.school[con.index, "School"] <- paste(disses.by.school[con.index, "School"], "*")

# # and sort by descending DissCount
# o <- order(disses.by.school$DissCount, disses.by.school$School, decreasing=T)
# print(disses.by.school[o,c("School","DissCount")])

# # success! save it and go:
# filename <- paste0(dataloc, "Appendix A - Schools and Dissertation Counts.csv")
# write.csv(disses.by.school[o,c("School","DissCount")], file=filename)


# # top 10 non-consortium schools by diss.count
# noncon.index <- !(1:nrow(disses.by.school) %in% con.index)
# noncons <- disses.by.school[noncon.index,]
# p <- order(noncons$DissCount, noncons$School, decreasing=T)
# filename <- paste0(dataloc, "Appendix - Top ", cutoff, " Non-Consortium Schools by Dissertation Count.csv")
# write.csv(head(noncons[p, c("School","DissCount")], cutoff), file=filename)

# # clean up the workspace
# rm(get_totals2, add_dc2, con.index, noncon.index, o, p)

