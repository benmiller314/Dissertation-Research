## Frequency of Method Tags
#
# Strategy: 
# 1. given a dataset and tagset, use apply() to sum the tag columns
# 2. sort and plot as horizontal bars
# 3. find a way to do this for multiple datasets but the same tags for a combined figure


# Step 0. Make sure we're all set up.
if(!exists("thresh", mode="function")) {source(file="start here.R")}

# Step 1. Sum the tag columns
get_tags <- function(dataset_name="noexcludes", tagset_name="tagnames") {
	dataset <- get(dataset_name)
	tagset <- get(tagset_name)
	
	a1 <- dataset[, tagset]
	a2 <- apply(a1, 2, sum)
	message("Method tag frequency for ", dataset_name, ":")
	print(a2)
}

a <- get_tags(noexcludes)
b <- get_tags(consorts)
c <- get_tags(nonconsorts)
e <- thresh("nonconsorts")
e <- get_tags(e$thresh.data)
e[order(e)]

# Step 2. Graph 'em

methodfreq_combined <- function(bigset="noexcludes", smallset="consorts", diffset="nonconsorts") {
	main <- "Frequency of Assigned Method Tags"
	a=get_tags(bigset)
	b=get_tags(smallset)
	d=get_tags(diffset)
	if (remake_figs) { filename <- paste0(imageloc, main, ".pdf"); pdf(file=filename) }
		# plot noexcludes as a baseline
		barplot(a[order(a)], horiz=TRUE, xpd=FALSE, las=1, axes=FALSE, main=main, col="gray80")
		text(x=a[order(a)]+30, y=seq(from=0.7,to=18.7,length.out=16), labels=a[order(a)])
		
		# plot consorts as an overlay
		barplot(b[order(a)], las=1, horiz=TRUE, xpd=FALSE, axes=FALSE, col="white", add=TRUE)
		text(x=30, y=seq(from=0.7,to=18.7,length.out=16), labels=b[order(a)])
		
		# no need to plot nonconsorts: that's the gap where the baseline shows through. just add labels.
		# barplot(-d[order(a)], las=1, horiz=TRUE, xpd=FALSE, axes=FALSE, col="gray90")
		text(x=a[order(a)]-d[order(a)]+30, y=seq(from=0.7,to=18.7,length.out=16), labels=d[order(a)])
		
		mtext("Tags are non-exclusive, so sum will be greater than the 2,711 dissertations.", side=1)
		
		legend(x="bottomright", c(smallset, diffset), fill=c("white","gray80"), bty="n")
	if (remake_figs) { dev.off() }
}

if(autorun) { 
	methodfreq_combined() 
}

## Step 3. Compare ranks of consorts vs. nonconsorts; leave out Othr
compare_method_ranks <- function(set1="consorts", set2="nonconsorts", pcts=TRUE) {
	b <- get_tags(set1)
	d <- get_tags(set2)
	
	# 3a. Version with raw counts
	b0 <- b[!names(b) %in% "Othr"]
	b1 <- names(b0)[order(b0, decreasing=T)]
	b2 <- paste0(b1, " (", b0[order(b0, decreasing=T)], ")")
	
	d0 <- d[!names(d) %in% "Othr"]
	d1 <- names(d0)[order(d0, decreasing=T)]
	d2 <- paste0(d1, " (", d0[order(d0, decreasing=T)], ")")

	filename <- paste0(imageloc, "Ranks of methods in ", set1, " v ", set2, ", no Othr.pdf")

	# 3b. Version with percentages
	if (pcts) {
		b2 <- paste0(b1, " (", 
								round(100*b0[order(b0, decreasing=T)]/nrow(get(set1)), 0), 
						  ")")
		d2 <- paste0(d1, " (", 
								round(100*d0[order(d0, decreasing=T)]/nrow(get(set2)), 0), 
						  ")")
	
		filename <- paste0(imageloc, "Ranks of methods in ", set1, " v ", set2, ", no Othr, pcts.pdf")
	}	
	
	if(remake_figs) { pdf(file=filename) }
		# set up a blank plot
		plot(x=0:length(tagnames)+1, y=0:length(tagnames)+1, axes=FALSE, type="n", xlab="", ylab="")
		
		# arrange consorts in descending rank order on the left, nonconsorts on the right
		text(labels=b2, 
			 x=rep(4,length(b0)), 
			 y=length(b2):1
			 )
		text(labels=d2, x=rep(length(tagnames)-4, length(d0)), y=length(d2):1)
		
		# draw a line from each tag's position on the left to the one on the right
		lapply(tagnames[!tagnames %in% "Othr"], FUN=function(tag) {
			segments(x0=5.7, y0=grep(tag, names(b0[order(b0)])),		 
			         x1=length(tagnames)-5.7, y1=grep(tag, names(d0[order(d0)]))
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
		text(labels=c(set1, set2), x=c(4,length(tagnames)-4), y=rep(length(tagnames)+1,2))
		text(labels=c(paste0("(N=",nrow(get(set1)),")"), paste0("(N=",nrow(get(set2)),")")), x=c(4,length(tagnames)-4), y=rep(length(tagnames),2))
	
		
	if (remake_figs) { dev.off() }
}

	
if (remake_figs) { dev.off() }

## Test significance
## Update: performed Fischer Exact Test of Independence using online calculators and Excel

# # # first vector: Tag in consorts vs. nonTag in consorts
# x <- c(b["Expt"], 					# number of Tag in consorts
	   # c["Expt"])

# # second vector: Tag in consorts vs. nonTag in consorts
# y <- c(nrow(consorts)-b["Expt"]), 	# total consorts diss-count minus number of Tag in consorts
	   # nrow(nonconsorts)-c["Expt"])
	   
# chisq.test(as.matrix(x,y), correct=F)
# z <- data.matrix(c(50,50), c(400,400))
# chisq.test