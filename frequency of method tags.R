## Frequency of Method Tags
#
# Strategy: 
# 1. given a dataset and tagset, use apply() to sum the tag columns
# 2. sort and plot as horizontal bars
# 3. find a way to do this for multiple datasets but the same tags for a combined figure

# Step 1. Sum the tag columns

get_tags <- function(x, tagset_name="tagnames") {
	tagset <- get(tagset_name)
	
	a1 <- x[, tagset]
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
	par(mfrow=c(1,1))
	
	barplot(a[order(a)], horiz=TRUE, xpd=FALSE, las=1, axes=FALSE, main=main, col="gray80")
	text(x=a[order(a)]+30, y=seq(from=0.7,to=18.7,length.out=16), labels=a[order(a)])
	
	barplot(b[order(a)], las=1, horiz=TRUE, xpd=FALSE, axes=FALSE, col="white", add=TRUE)
	text(x=30, y=seq(from=0.7,to=18.7,length.out=16), labels=b[order(a)])
	
	# barplot(-c[order(a)], las=1, horiz=TRUE, xpd=FALSE, axes=FALSE, col="gray90")
	text(x=a[order(a)]-c[order(a)]+30, y=seq(from=0.7,to=18.7,length.out=16), labels=c[order(a)])
	
	mtext("Tags are non-exclusive, so sum will be greater than the 2,711 dissertations.", side=1)
	
	legend(x="bottomright", c("Consortium Schools", "Non-Consortium Schools"), fill=c("white","gray80"), bty="n")
	
if (remake_figs) { dev.off() }

## Step 3. Compare ranks of consorts vs. nonconsorts; leave out Othr
# 3a. Version with raw counts
	b0 <- b[!names(b) %in% "Othr"]
	b1 <- names(b0)[order(b0, decreasing=T)]
	b2 <- paste0(b1, " (", b0[order(b0, decreasing=T)], ")")
	
	c0 <- c[!names(c) %in% "Othr"]
	c1 <- names(c0)[order(c0, decreasing=T)]
	c2 <- paste0(c1, " (", c0[order(c0, decreasing=T)], ")")

	filename <- paste0(imageloc, "Ranks of methods in consorts v nonconsorts, no Othr.pdf")
	
# 3b. Version with percentages
	b2 <- paste0(b1, " (", 
							round(100*b0[order(b0, decreasing=T)]/nrow(consorts), 0), 
					  ")")
	c2 <- paste0(c1, " (", 
							round(100*c0[order(c0, decreasing=T)]/nrow(nonconsorts), 0), 
					  ")")

	filename <- paste0(imageloc, "Ranks of methods in consorts v nonconsorts, no Othr, pcts.pdf")
	

if(remake_figs) { pdf(file=filename) }
	# set up a blank plot
	
	plot(x=0:length(tagnames)+1, y=0:length(tagnames)+1, axes=FALSE, type="n", xlab="", ylab="")
	
	# arrange consorts in descending rank order on the left, nonconsorts on the right
	text(labels=b2, x=rep(4,length(b0)), y=length(b2):1)
	text(labels=c2, x=rep(length(tagnames)-4, length(c0)), y=length(c2):1)
	
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
	text(labels=c(paste0("(N=",nrow(consorts),")"), paste0("(N=",nrow(nonconsorts),")")), x=c(4,length(tagnames)-4), y=rep(length(tagnames),2))

	
if (remake_figs) { dev.off() }

## Test some significance via Chi-Squared (or is it Fisher Exact?) Test

# # # first vector: Tag in consorts vs. nonTag in consorts
# x <- c(b["Expt"], 					# number of Tag in consorts
	   # c["Expt"])

# # second vector: Tag in consorts vs. nonTag in consorts
# y <- c(nrow(consorts)-b["Expt"]), 	# total consorts diss-count minus number of Tag in consorts
	   # nrow(nonconsorts)-c["Expt"])
	   
# chisq.test(as.matrix(x,y), correct=F)
# z <- data.matrix(c(50,50), c(400,400))
# chisq.test