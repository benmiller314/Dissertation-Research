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

## Step 3. Compare ranks of consorts vs. nonconsorts

# set up a blank plot
plot.new()
plot(0:length(tagnames)+1, 0:length(tagnames)+1, axes=FALSE, type="n")

# arrange consorts in descending rank order on the left, nonconsorts on the right
text(labels=names(b)[order(b, decreasing=T)], rep(1,length(b)), order(b))
text(labels=names(c)[order(c, decreasing=T)], rep(9,length(c)), order(c))

# draw a line from each tag's position on the left to the one on the right
lapply(tagnames, FUN=function(tag) {
	segments(x0=1.7, y0=grep(tag, names(b[order(b)])),		 
	         x1=8.3, y1=grep(tag, names(c[order(c)])))
})

# extend those lines to point horizontally to the tags
lapply(1:length(tagnames), FUN=function(y) {
	segments(x0=1.6, y0=y,
			 x1=1.7, y1=y)
	segments(x0=8.3, y0=y,
			 x1=8.4, y1=y)		
})

# label the two sides
text(labels=c("Consortium Schools", "Non-consortium Schools"), x=c(1,9), y=rep(length(tagnames)+1,2))