## Frequency of Method Tags
#
# Strategy: 
# 1. given a dataset and tagset, use apply() to sum the tag columns
# 2. sort and plot as horizontal bars
# 3. find a way to do this for multiple datasets but the same tags for a combined figure


# Step 0. Make sure we're all set up.
if(!exists("thresh", mode="function")) {source(file="start here.R")}

# Step 1. Sum the tag columns
if(!exists("get_tags", mode="function")) { source(file="get tags.R") }

if(autorun) {
	a <- get_tags("noexcludes")
	b <- get_tags("consorts")
	d <- get_tags("nonconsorts")
	e <- thresh("nonconsorts")$thresh.data
	e <- get_tags("e")
	e[order(e)]
}
# Step 2. Graph 'em

methodfreq_combined <- function(bigset="noexcludes", smallset="consorts", diffset="nonconsorts") {
	main <- "Frequency of Assigned Method Tags"
	a=get_tags(bigset)
	b=get_tags(smallset)
	d=get_tags(diffset)
	if (remake_figs) { filename <- paste0(imageloc, main, ", ", smallset," within ", bigset, ".pdf"); pdf(file=filename) }
		# plot noexcludes as a baseline
		barplot(a[order(a)], horiz=TRUE, xpd=FALSE, las=1, axes=FALSE, main=main, col="gray80")
		text(x=a[order(a)]+30, y=seq(from=0.7,to=18.7,length.out=16), labels=a[order(a)])
		
		# plot consorts as an overlay
		barplot(b[order(a)], las=1, horiz=TRUE, xpd=FALSE, axes=FALSE, col="white", add=TRUE)
		text(x=30, y=seq(from=0.7,to=18.7,length.out=16), labels=b[order(a)])
		
		# no need to plot nonconsorts: that's the gap where the baseline shows through. just add labels.
		# barplot(-d[order(a)], las=1, horiz=TRUE, xpd=FALSE, axes=FALSE, col="gray90")
		text(x=a[order(a)]-d[order(a)]+30, y=seq(from=0.7,to=18.7,length.out=16), labels=d[order(a)])
		
		mtext(paste("Tags are non-exclusive, so sum will be greater than the", nrow(get(bigset)), "included dissertations.", side=1))
		legend(x="bottomright", c(smallset, diffset), fill=c("white","gray80"), bty="n")
	if (remake_figs) { dev.off() }
}

if(autorun) {
	remake_figs
	methodfreq_combined() 
	methodfreq_combined(bigset="consorts.plus", smallset="consorts", diffset="top.nonconsorts") 
}

## Step 3. Compare ranks of consorts vs. nonconsorts; leave out Othr
if(!exists("compare_method_ranks", mode="function")) { source(file="compare method ranks.R") }
