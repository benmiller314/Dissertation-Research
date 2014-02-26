## Frequency of Method Tags
#
# Strategy: 
# 1. given a dataset and tagset, use apply() to sum the tag columns
# 2. sort and plot as horizontal bars
# 3. find a way to do this for multiple datasets but the same tags for a combined figure

# Step 1. Sum the tag columns

get_tags <- function(x) {
	a1 <- x[, tagnames]
	a2 <- apply(a1, 2, sum)
	print(a2)
}

a <- get_tags(noexcludes)
b <- get_tags(consorts)
c <- get_tags(nonconsorts)

# Step 2. Graph 'em
main <- "Frequency of Assigned Method Tags"
filename <- paste0(imageloc, main, ".pdf")

if (remake_figs) { pdf(file=filename) }
	plot.new()
	par(mfrow=c(1,1))
	
	barplot(a[order(a)], horiz=TRUE, xpd=FALSE, las=1, axes=FALSE, main=main, col="gray80")
	text(x=a[order(a)]+30, y=seq(from=0.7,to=18.7,length.out=16), labels=a[order(a)])
	
	barplot(b[order(a)], las=1, horiz=TRUE, xpd=FALSE, axes=FALSE, col="white", add=TRUE)
	text(x=30, y=seq(from=0.7,to=18.7,length.out=16), labels=b[order(a)])
	
	# barplot(-c[order(a)], las=1, horiz=TRUE, xpd=FALSE, axes=FALSE, col="gray90")
	text(x=a[order(a)]-30, y=seq(from=0.7,to=18.7,length.out=16), labels=c[order(a)])
	
	mtext("Tags are non-exclusive, so sum will be greater than the 2,711 dissertations.", side=1)
if (remake_figs) { dev.off() }

## Step 3. Compare ranks of consorts vs. nonconsorts; leave out Othr
b0 <- b[!names(b) %in% "Othr"]
b1 <- names(b0)[order(b0, decreasing=T)]
b2 <- paste0(b1, " (", b0[order(b0, decreasing=T)], ")")

c0 <- c[!names(c) %in% "Othr"]
c1 <- names(c0)[order(c0, decreasing=T)]
c2 <- paste0(c1, " (", c0[order(c0, decreasing=T)], ")")

# set up a blank plot
plot.new()
plot(0:length(tagnames)+1, 0:length(tagnames)+1, axes=FALSE, type="n")

# arrange consorts in descending rank order on the left, nonconsorts on the right
text(labels=b2, rep(4,length(b0)), order(b0))
text(labels=c2, rep(length(tagnames)-4, length(c0)), order(c0))

# draw a line from each tag's position on the left to the one on the right
lapply(tagnames[!tagnames %in% "Othr"], FUN=function(tag) {
	segments(x0=5.7, y0=grep(tag, names(b0[order(b0)])),		 
	         x1=length(tagnames)-5.7, y1=grep(tag, names(c0[order(c0)]))
	)
})

# extend those lines to point horizontally to the tags, to remove ambiguity
lapply(1:length(tagnames)-1, FUN=function(y) {
	segments(x0=5.4, y0=y,
			 x1=5.7, y1=y)
	segments(x0=length(tagnames)-5.4, y0=y,
			 x1=length(tagnames)-5.7, y1=y)		
})

# label the two sides
text(labels=c("Consortium Schools", "Non-consortium Schools"), x=c(4,length(tagnames)-4), y=rep(length(tagnames)+1,2))

# # debugging
# (b)[order(b)]
# grep("Rhet",names(b)[order(b)])
# grep("Ethn",names(b)[order(b)])
# (c)[order(c)]
# grep("Ethn",names(c)[order(c)])
# grep("Rhet",names(c)[order(c)])

