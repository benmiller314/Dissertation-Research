# Assumes we've already run dataprep.R.
# Extract keyword terms, make a frequency table and calculate some stats. 

# TO DO: convert the whole thing to a function, to allow for various datasets.
# See 'methodcount barplot.R'
keyword_barplot <- function(dataset_name="consorts.plus", 		# by default, show consortium + 5 or more in last 5 years
							how.many=NULL,						# number of keywords to plot; defaults to all
							horizontal=FALSE) {

	# First get the terms:
	dataset <- get(dataset_name)
	kw.list <- extract_subjects(dataset$KEYWORDS)
	
	# Get the frequency chart. maxsum is needed to avoid "othering" half the list.
	kw.table <- summary(kw.list, maxsum=10000)
	
	# Put the list in descending order by frequency, and chop out the term they all share
	kw.table <- sort(kw.table,decreasing=TRUE)
	
	kw.count <- length(kw.table)
	kw.mean <- mean(kw.table)
	kw.median <- median(kw.table)

	# if no cutoff is given, default to showing all keywords (even if that's a crazy long tail)
	if(is.null(how.many)) {
		how.many <- kw.count
	}

	how.wide <- 10 * ceiling(1 + max(kw.table)/10)
	
	filename <- paste0(imageloc, "keyword barplot, top ", how.many, ", N",diss.count,".pdf")
	pdf(file=filename)
	
	barplot(sort(kw.table[1:how.many],decreasing=FALSE),horiz=TRUE,main=paste("Top",how.many,"Keywords by Frequency"), axisnames=TRUE,width=c(10,10),space=0.4,las=1, pty="m")
	mai=c(5,10,8,5))
	
	for(i in 1:how.many) {
		# add a label where x = frequency and y = "device height" or something
			text(x=(kw.table[[i]] + 20), y=(par()$din[2]), labels=kw.table[[i]],pos=4)
	}
	
	mtext(paste(diss.count,"theses,",kw.count,"keywords, median =",kw.median,", mean =",kw.mean,collapse=" "))
	
	dev.off()
	
	filename <- paste0(imageloc, "keyword terms barplot, below",how.many,", above median, N",diss.count,".pdf")
	pdf(file=filename)
	
	barplot(kw.table[how.many+1:length(kw.table)/2],horiz=FALSE,main=paste0("Frequency of Subject Terms below ",how.many,", above median"), axisnames=TRUE,width=c(10,10),space=0.4,las=2, pty="m",mai=c(5,10,8,5))
	
	# if(remake_figs) { dev.off() }
}

if(autorun) {
	remake_figs=T
	keyword_barplot()
}

