# let's repeat the whole thing for just consortium schools! yes, there is a better way. no time now.
# NB: this file assumes you've already run 'collocation heatmap.R'

tagarray.c <- tagarray[consorts.index,]


# first find the basic sums
sum.by.tags.c <- sumbytags(tagarray[consorts.index,])
print(sum.by.tags.c)

# catch the diagonals
tag.totals.c <- c()
for(i in 1:ncol(sum.by.tags.c)) {
	tag.totals.c <- c(tag.totals.c,sum.by.tags.c[i,i])
}
names(tag.totals.c) <- tagnames
tag.totals.cs <- sort(tag.totals.c)
print(tag.totals.c)

# And now let's replace the diagonals with solo-tag counts: the number of times
# that tag occurs as the only tag.
  # Step 1: find the subset of consorts that has MethodCount = 1
monomethodics <- consorts[which(consorts$MethodCount == 1),]
solotagsums <- sumbytags(monomethodics)
  # Step 2: swap the existing tag.totals with the monomethodic sumbytags
sum.by.tags2 <- sum.by.tags.c
for(i in 1:ncol(sum.by.tags.c)) {
	sum.by.tags2[i,i] <- solotagsums.c[i,i]
}

# make a horizontal bar plot of tag totals
main <- "Frequency of Assigned Method Tags, Consortium Schools Only"
filename <- paste0(main, ", N=",consort.count,".pdf")
pdf(filename)
	barplot(tag.totals.cs, horiz=TRUE, xpd=FALSE, las=1, axes=FALSE)
	title(main)
	text(x=tag.totals.cs-30, y=seq(from=0.7,to=18.7,length.out=16), labels=tag.totals.cs)
	mtext(paste("Tags are non-exclusive, so sum will be greater than the",consort.count,"dissertations."), side=1)
dev.off()

# make a stacked horizontal bar plot of tag totals for consortium within all schools
main <- "Frequency of Assigned Method Tags"
filename <- paste0(main, ", N=",diss.count,".pdf")
pdf(filename)
	barplot(as.matrix(rbind(tag.totals.cs, tag.totals.s-tag.totals.cs)), horiz=TRUE, beside=FALSE, xpd=FALSE, las=1, axes=FALSE, col=c("gray80", "white"))
	title(main)
	text(x=tag.totals.cs-40, y=seq(from=0.7,to=18.7,length.out=16), labels=tag.totals.cs)
	a <- function(value) {
		if(value < 125) {value + 30} 
		else {value - 30}
	}
	b <- sapply(tag.totals.s, FUN=function(x) a(x))
	text(x=b, y=seq(from=0.7,to=18.7,length.out=16), labels=tag.totals.s)
	mtext(paste("Tags are non-exclusive, so sum will be greater than the",diss.count,"dissertations."), side=1)
	legend(x="bottomright", c("Total, Consortium and Other Schools","Consortium Schools Only"), fill=c("white","gray80"), bty="n")
	rm(a,b)
dev.off()