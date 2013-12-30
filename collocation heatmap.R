### Given method tags, collocate and construct a heat plot.
require(doBy)
require(RColorBrewer)

## TO DO: Figure out how to code sumbytags more efficiently using lapply

sumbytags <- function (tagarray) {
	# Strategy: for each tag in tagnames (ie. "Case", "Crit", etc), do a summaryBy 
	# and extract only the case in which the tag is active
	summaryBy(Case+Crit+Cult+Disc+Ethn+Expt+Hist+Intv+Meta+Modl+Phil+Poet+Pract+Rhet+Surv+Othr~Case,data=tagarray,FUN=sum) -> tag1.sum
	tag1.sum <- tag1.sum[which(tag1.sum$Case == 1),]
	row.names(tag1.sum) <- names(tag1.sum)[1]
	tag1.sum <- tag1.sum[2:ncol(tag1.sum)]
	
	# then repeat for the next tag
	summaryBy(Case+Crit+Cult+Disc+Ethn+Expt+Hist+Intv+Meta+Modl+Phil+Poet+Pract+Rhet+Surv+Othr~Crit,data=tagarray,FUN=sum) -> tag2.sum
	tag2.sum <- tag2.sum[which(tag2.sum$Crit == 1),]
	row.names(tag2.sum) <- names(tag2.sum)[1]
	tag2.sum <- tag2.sum[2:ncol(tag2.sum)]
	
	# and bind the rows together
	sum.by.tags <- rbind(tag1.sum,tag2.sum)
	
	# on to tag 3
	summaryBy(Case+Crit+Cult+Disc+Ethn+Expt+Hist+Intv+Meta+Modl+Phil+Poet+Pract+Rhet+Surv+Othr~Cult,data=tagarray,FUN=sum) -> tag3.sum
	tag3.sum <- tag3.sum[which(tag3.sum$Cult == 1),]
	row.names(tag3.sum) <- names(tag3.sum)[1]
	tag3.sum <- tag3.sum[2:ncol(tag3.sum)]
	
	# and bind the rows together again
	sum.by.tags <- rbind(sum.by.tags,tag3.sum)
	
	# at this point we can just copy and paste, changing only the tag. We're up to Disc
	summaryBy(Case+Crit+Cult+Disc+Ethn+Expt+Hist+Intv+Meta+Modl+Phil+Poet+Pract+Rhet+Surv+Othr~Disc,data=tagarray,FUN=sum) -> tag3.sum
	tag3.sum <- tag3.sum[which(tag3.sum$Disc == 1),]
	row.names(tag3.sum) <- names(tag3.sum)[1]
	tag3.sum <- tag3.sum[2:ncol(tag3.sum)]
	
	sum.by.tags <- rbind(sum.by.tags,tag3.sum)
	
	# Now we're on Ethn
	summaryBy(Case+Crit+Cult+Disc+Ethn+Expt+Hist+Intv+Meta+Modl+Phil+Poet+Pract+Rhet+Surv+Othr~Ethn,data=tagarray,FUN=sum) -> tag3.sum
	tag3.sum <- tag3.sum[which(tag3.sum$Ethn == 1),]
	row.names(tag3.sum) <- names(tag3.sum)[1]
	tag3.sum <- tag3.sum[2:ncol(tag3.sum)]
	sum.by.tags <- rbind(sum.by.tags,tag3.sum)
	
	# Now we're on Expt
	summaryBy(Case+Crit+Cult+Disc+Ethn+Expt+Hist+Intv+Meta+Modl+Phil+Poet+Pract+Rhet+Surv+Othr~Expt,data=tagarray,FUN=sum) -> tag3.sum
	tag3.sum <- tag3.sum[which(tag3.sum$Expt == 1),]
	row.names(tag3.sum) <- names(tag3.sum)[1]
	tag3.sum <- tag3.sum[2:ncol(tag3.sum)]
	sum.by.tags <- rbind(sum.by.tags,tag3.sum)
	
	# Now we're on Hist
	summaryBy(Case+Crit+Cult+Disc+Ethn+Expt+Hist+Intv+Meta+Modl+Phil+Poet+Pract+Rhet+Surv+Othr~Hist,data=tagarray,FUN=sum) -> tag3.sum
	tag3.sum <- tag3.sum[which(tag3.sum$Hist == 1),]
	row.names(tag3.sum) <- names(tag3.sum)[1]
	tag3.sum <- tag3.sum[2:ncol(tag3.sum)]
	sum.by.tags <- rbind(sum.by.tags,tag3.sum)
	
	# Now we're on Intv
	summaryBy(Case+Crit+Cult+Disc+Ethn+Expt+Hist+Intv+Meta+Modl+Phil+Poet+Pract+Rhet+Surv+Othr~Intv,data=tagarray,FUN=sum) -> tag3.sum
	tag3.sum <- tag3.sum[which(tag3.sum$Intv == 1),]
	row.names(tag3.sum) <- names(tag3.sum)[1]
	tag3.sum <- tag3.sum[2:ncol(tag3.sum)]
	sum.by.tags <- rbind(sum.by.tags,tag3.sum)
	
	# Now we're on Meta
	summaryBy(Case+Crit+Cult+Disc+Ethn+Expt+Hist+Intv+Meta+Modl+Phil+Poet+Pract+Rhet+Surv+Othr~Meta,data=tagarray,FUN=sum) -> tag3.sum
	tag3.sum <- tag3.sum[which(tag3.sum$Meta == 1),]
	row.names(tag3.sum) <- names(tag3.sum)[1]
	tag3.sum <- tag3.sum[2:ncol(tag3.sum)]
	sum.by.tags <- rbind(sum.by.tags,tag3.sum)
	
	# Now we're on Modl
	summaryBy(Case+Crit+Cult+Disc+Ethn+Expt+Hist+Intv+Meta+Modl+Phil+Poet+Pract+Rhet+Surv+Othr~Modl,data=tagarray,FUN=sum) -> tag3.sum
	tag3.sum <- tag3.sum[which(tag3.sum$Modl == 1),]
	row.names(tag3.sum) <- names(tag3.sum)[1]
	tag3.sum <- tag3.sum[2:ncol(tag3.sum)]
	sum.by.tags <- rbind(sum.by.tags,tag3.sum)
	
	# Now we're on Phil
	summaryBy(Case+Crit+Cult+Disc+Ethn+Expt+Hist+Intv+Meta+Modl+Phil+Poet+Pract+Rhet+Surv+Othr~Phil,data=tagarray,FUN=sum) -> tag3.sum
	tag3.sum <- tag3.sum[which(tag3.sum$Phil == 1),]
	row.names(tag3.sum) <- names(tag3.sum)[1]
	tag3.sum <- tag3.sum[2:ncol(tag3.sum)]
	sum.by.tags <- rbind(sum.by.tags,tag3.sum)
	
	# Now we're on Poet
	summaryBy(Case+Crit+Cult+Disc+Ethn+Expt+Hist+Intv+Meta+Modl+Phil+Poet+Pract+Rhet+Surv+Othr~Poet,data=tagarray,FUN=sum) -> tag3.sum
	tag3.sum <- tag3.sum[which(tag3.sum$Poet == 1),]
	row.names(tag3.sum) <- names(tag3.sum)[1]
	tag3.sum <- tag3.sum[2:ncol(tag3.sum)]
	sum.by.tags <- rbind(sum.by.tags,tag3.sum)
	
	# Now we're on Pract
	summaryBy(Case+Crit+Cult+Disc+Ethn+Expt+Hist+Intv+Meta+Modl+Phil+Poet+Pract+Rhet+Surv+Othr~Pract,data=tagarray,FUN=sum) -> tag3.sum
	tag3.sum <- tag3.sum[which(tag3.sum$Pract == 1),]
	row.names(tag3.sum) <- names(tag3.sum)[1]
	tag3.sum <- tag3.sum[2:ncol(tag3.sum)]
	sum.by.tags <- rbind(sum.by.tags,tag3.sum)
	
	# Now we're on Rhet
	summaryBy(Case+Crit+Cult+Disc+Ethn+Expt+Hist+Intv+Meta+Modl+Phil+Poet+Pract+Rhet+Surv+Othr~Rhet,data=tagarray,FUN=sum) -> tag3.sum
	tag3.sum <- tag3.sum[which(tag3.sum$Rhet == 1),]
	row.names(tag3.sum) <- names(tag3.sum)[1]
	tag3.sum <- tag3.sum[2:ncol(tag3.sum)]
	sum.by.tags <- rbind(sum.by.tags,tag3.sum)
	
	# Now we're on Surv
	summaryBy(Case+Crit+Cult+Disc+Ethn+Expt+Hist+Intv+Meta+Modl+Phil+Poet+Pract+Rhet+Surv+Othr~Surv,data=tagarray,FUN=sum) -> tag3.sum
	tag3.sum <- tag3.sum[which(tag3.sum$Surv == 1),]
	row.names(tag3.sum) <- names(tag3.sum)[1]
	tag3.sum <- tag3.sum[2:ncol(tag3.sum)]
	sum.by.tags <- rbind(sum.by.tags,tag3.sum)

	# Now we're on Othr
	summaryBy(Case+Crit+Cult+Disc+Ethn+Expt+Hist+Intv+Meta+Modl+Phil+Poet+Pract+Rhet+Surv+Othr~Othr,data=tagarray,FUN=sum) -> tag3.sum
	tag3.sum <- tag3.sum[which(tag3.sum$Othr == 1),]
	row.names(tag3.sum) <- names(tag3.sum)[1]
	tag3.sum <- tag3.sum[2:ncol(tag3.sum)]
	sum.by.tags <- rbind(sum.by.tags,tag3.sum)
	
	
	# That should be everything!
	return(sum.by.tags)
}
# run the function above when the file is loaded
sum.by.tags <- sumbytags(tagarray)
print(sum.by.tags)


# Now, the diagonals will totally dominate the heatmap, so let's get those separately
tag.totals <- c()
for(i in 1:ncol(sum.by.tags)) {
	tag.totals <- c(tag.totals,sum.by.tags[i,i])
}
names(tag.totals) <- tagnames
tag.totals.s <- sort(tag.totals)
print(tag.totals.s)

# While we're here, make a horizontal bar plot of tag totals
main <- "Frequency of Assigned Method Tags, All Schools"
filename <- paste0(main, ", N=",diss.count,".pdf")
pdf(filename)
	barplot(tag.totals.s, horiz=TRUE, xpd=FALSE, las=1, axes=FALSE)
	title(main)
	text(x=tag.totals.s-30, y=seq(from=0.7,to=18.7,length.out=16), labels=tag.totals.s)
	mtext("Tags are non-exclusive, so sum will be greater than the 2,711 dissertations.", side=1)
dev.off()

# And now let's replace the diagonals with solo-tag counts: the number of times
# that tag occurs as the only tag.
  # Step 1: find the subset of noexcludes that has MethodCount = 1
monomethodics <- noexcludes[which(noexcludes$MethodCount == 1),]
solotagsums <- sumbytags(monomethodics)
  # Step 2: swap the existing tag.totals with the monomethodic sumbytags
sum.by.tags2 <- sum.by.tags
for(i in 1:ncol(sum.by.tags)) {
	sum.by.tags2[i,i] <- solotagsums[i,i]
}

# finally, let's replace the ".sum" names with more symmetrical names:
names(sum.by.tags2) <- substr(names(sum.by.tags2),1,4)
row.names(sum.by.tags2) <- paste(substr(row.names(sum.by.tags2),1,4))
names(tag.totals) <- substr(names(tag.totals),1,4)
print(sum.by.tags2)

# now let's see how that looks as a heatmap...
filename <- paste("Method Tag Co-Occurrence (R heatmap function), N",diss.count,".pdf",collapse="")
pdf(filename)
data.matrix(sum.by.tags2) -> sum.by.tags.m			# heatmap needs a matrix, not a list
h1 <- heatmap(sum.by.tags.m,symm=TRUE,main="Method Tag Co-Occurrence", sub="raw counts; diagonals are solo methods", col=brewer.pal(9,"YlOrRd"))
dev.off()

# can I customize that?
sum.by.tags.s <- sum.by.tags2[h1$rowInd,h1$colInd] 		# sorted by similarity function in heatmap

# heatmap.ben function now saved in 'function scratchpad.R', so it loads during dataprep


# give it a spin!
filename <- paste("Method Tag Co-Occurrence (ben heatmap), N", diss.count, ".pdf", collapse="")
pdf(filename)
	heatmap.ben(sum.by.tags.s, diags=TRUE)
	title(main="Method Tag Co-Occurrence", sub="diagonals are solo methods")
	mtext("A box in row X, column Y gives the number of \n dissertations tagged Y that are also tagged X", side=2)
dev.off()



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

filename <- paste("Method Tag Co-Occurrence (ben heatmap, normed, whole numbers), N",diss.count,".pdf",collapse="")
pdf(filename)
	# heatmap(data.matrix(sum.by.tags.n),symm=TRUE,main="Method Tag Co-Occurrence", sub="normed by dividing intersection count by row total;\n diagonals are solo methods", col=brewer.pal(9,"YlOrRd"))
	heatmap.ben(sum.by.tags.n)
	title(main="Method Tag Co-Occurrence", sub="normed by dividing intersection count by row total;\n diagonals are solo methods")
	mtext("A box in row X, column Y gives the probability \n that a dissertation tagged X is also tagged Y", side=2)
dev.off()



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