### Given method tags, collocate and construct a heat plot.
require(doBy)
require(RColorBrewer)

## If a dissertation is tagged X, how many times is it also tagged Y?
#  NB: diagonals in the resulting matrix are for solo tags, i.e. 
#  the number of times a dissertation tagged X is *only* tagged X.
#  The total number of times a dissertation is tagged X are returned separately.

sumbytags <- function(dataset_name="noexcludes", tagset_name="tagnames") {
	sum.by.tags <- total.counts <- solo.counts <- c()
	dataset <- get(dataset_name)
	tagset <- get(tagset_name)
	
	for (i in 1:length(tagset)) {
		# select the tag
		tag <- tagset[i]
		
		# sum columns where the tag is 0 and where it's 1; 
		# this produces an array with two rows.
		tagsum <- aggregate(dataset[, tagset], list(dataset[, tag]), FUN=sum)
		
		# save the row in which the tag is "on", i.e. row 2. 
		# First column is the on/off status, so get rid of it.
		sum.by.tags <- rbind(sum.by.tags, tagsum[2, 2:ncol(tagsum)])
		
		# Name the row we've just added by the tag we're currently summarizing.
		row.names(sum.by.tags)[i] <- tag
		
		# Now the diagonals will dominate, so find the solo count for the tag
		solosum <- sum(dataset[which(dataset$Method.Count==1), tag])
		solo.counts <- c(solo.counts, solosum)
		names(solo.counts)[i] <- tag
		
		# ... and replace the diagonal with that solo count (but save the true count)
		total.counts <- c(total.counts, sum.by.tags[i,i])
		names(total.counts)[i] <- tag
		sum.by.tags[i,i] <- solosum
		
	}
	# print(sum.by.tags)
	# print(total.counts)
			
	return (list("correlations" = sum.by.tags,
				 "solo.counts" = solo.counts,
				 "total.counts" = total.counts))
}
sum.by.tags <- sumbytags()


## Make a horizontal bar plot of total dissertation counts for each tag.
total.counts.barplot <- function (dataset_name="noexcludes", tagset_name="tagnames") {
	sum.by.tags <- sumbytags(dataset_name, tagset_name)
	total.counts <- sort(sum.by.tags$total.counts, decreasing=FALSE)
	length <- length(tagnames)
	diss.count <- nrow(get(dataset_name))

	main <- "Frequency of Assigned Method Tags"
	submain <- paste0(dataset_name, ", ", tagset_name)
	if(remake_figs) { 
			filename <- paste0(imageloc, main, ", N", diss.count, ".pdf")
			pdf(filename) 
	}
		barplot(total.counts, horiz=TRUE, xpd=FALSE, las=1, axes=FALSE)
		title(main)
		text(x=total.counts-30, y=seq(from=0.7,to=(length+2.5),length.out=length), labels=total.counts)
		mtext(submain, side=3)
		mtext(paste("Tags are non-exclusive, so sum will be greater than the", diss.count, "dissertations."), side=1)

	if(remake_figs) { dev.off() }
}
total.counts.barplot()

## Make a horizontal bar plot of solo dissertation counts for each tag.
solo.counts.barplot <- function (dataset_name="noexcludes", tagset_name="tagnames") {
	sum.by.tags <- sumbytags(dataset_name, tagset_name)
	solo.counts <- sort(sum.by.tags$solo.counts, decreasing=FALSE)
	length <- length(tagnames)
	diss.count <- nrow(get(dataset_name))

	main <- "Frequency of Exclusively Assigned Method Tags"
	submain <- paste0(dataset_name, ", ", tagset_name)
	if(remake_figs) { 
			filename <- paste0(imageloc, main, ", N", diss.count, ".pdf")
			pdf(filename) 
	}
		barplot(solo.counts, horiz=TRUE, xpd=FALSE, las=1, axes=FALSE)
		title(main)
		text(x=solo.counts-5, y=seq(from=0.7,to=(length+2.5),length.out=length), labels=solo.counts)
		mtext(submain, side=3)
		mtext(paste("For the subset of ", sum(solo.counts), "of", diss.count, "dissertations with only one tag."), side=1)

	if(remake_figs) { dev.off() }
}
solo.counts.barplot()



# finally, let's replace the ".sum" names with more symmetrical names:
names(sum.by.tags2) <- substr(names(sum.by.tags2),1,4)
row.names(sum.by.tags2) <- paste(substr(row.names(sum.by.tags2),1,4))
names(tag.totals) <- substr(names(tag.totals),1,4)
print(sum.by.tags2)

# now let's see how that looks as a heatmap...
filename <- paste0(imageloc, "Method Tag Co-Occurrence (R heatmap function), N",diss.count,".pdf")
if(remake_figs) { pdf(filename) }
data.matrix(sum.by.tags2) -> sum.by.tags.m			# heatmap needs a matrix, not a list
h1 <- heatmap(sum.by.tags.m,symm=TRUE,main="Method Tag Co-Occurrence", sub="raw counts; diagonals are solo methods", col=brewer.pal(9,"YlOrRd"))
if(remake_figs) { dev.off() }

# can I customize that?
sum.by.tags.s <- sum.by.tags2[h1$rowInd,h1$colInd] 		# sorted by similarity function in heatmap

# heatmap.ben function now saved in 'function scratchpad.R', so it loads during dataprep


# give it a spin!
filename <- paste0(imageloc, "Method Tag Co-Occurrence (ben heatmap), N", diss.count, ".pdf")
if(remake_figs) { pdf(filename) } 
	heatmap.ben(sum.by.tags.s, diags=TRUE)
	title(main="Method Tag Co-Occurrence", sub="diagonals are solo methods")
	mtext("A box in row X, column Y gives the number of \n dissertations tagged Y that are also tagged X", side=2)
if(remake_figs) { dev.off() }



# The most frequent tags hide other collocations, so let's norm down each column (which it weirdly thinks are the rows... some day, I'll figure out why --> UPDATE: it was just mislabeled b/c of i=x and j=y.)
sum.by.tags.n <- sum.by.tags.s
for (i in 1:nrow(sum.by.tags.n)) {
	# find the total to divide out by
	index <- names(sum.by.tags.n)[i]
	value <- tag.totals[which(index == names(tag.totals))]
	
	# divide
	sum.by.tags.n[i,] <- round((sum.by.tags.n[i,] / value), digits=4)
}

# to reduce clutter, I want to give these as whole-number percentages
sum.by.tags.n <- round(sum.by.tags.n * 100, 0)
print(sum.by.tags.n)

filename <- paste0(imageloc, "Method Tag Co-Occurrence (ben heatmap, normed, whole numbers), N",diss.count,".pdf")
if(remake_figs) { pdf(filename) }
	# heatmap(data.matrix(sum.by.tags.n),symm=TRUE,main="Method Tag Co-Occurrence", sub="normed by dividing intersection count by row total;\n diagonals are solo methods", col=brewer.pal(9,"YlOrRd"))
	heatmap.ben(sum.by.tags.n)
	title(main="Method Tag Co-Occurrence", sub="normed by dividing intersection count by row total;\n diagonals are solo methods")
	mtext("A box in row X, column Y gives the probability \n that a dissertation tagged X is also tagged Y", side=2)
if(remake_figs) { dev.off() }



# Old approach: Given a csv with method tag collocation data (e.g. as constructed from Google Refine), construct a heat plot.
# To do: figure out how to do the collocation in R.

# # Starts with data prep, as always:
# # filename <- readline("What's the name of the method tag collocation csv? ")
# # read.csv(file=filename) -> method.col
# read.csv(file="../method tag collocation noexcludes (N 1901).csv") -> method.col

# names(method.col)[1] -> subtitle

# row.names(method.col) <-method.col[,1]			# row names are in the first column
# method.col <- method.col[,2:ncol(method.col)]	# we can dump that column
# data.matrix(method.col) -> method.col.m			# heatmap needs a matrix, not a list


# # now let's do it all again with the data normed within each tag row
# read.csv(file="../method tag collocation normed within tag noexcludes (N 1901).csv") -> method.col.normed

# # names(method.col.normed)[1] -> subtitle.normed
# "Each cell (X,Y) gives the likelihood that a diss tagged X is also tagged Y" -> subtitle.normed

# row.names(method.col.normed) <-method.col.normed[,1]
# method.col.normed <- method.col.normed[,2:ncol(method.col.normed)]
# data.matrix(method.col.normed) -> method.col.normed.m

# # and finally, let's plot these babies:
# par(mfrow=c(1,2))
# heatmap(method.col.m,symm=TRUE,main="Method Tag Co-Occurrence",sub=subtitle, col=brewer.pal(9,"YlOrRd"))
# heatmap(method.col.normed.m,symm=TRUE,main="Method Tag Co-Occurrence, normed by tag",sub=subtitle.normed,col=brewer.pal(9,"YlOrRd"))