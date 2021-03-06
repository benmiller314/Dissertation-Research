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
	kw.mean <- round(mean(kw.table), 2)
	kw.median <- median(kw.table)

	# if no cutoff is given, default to showing all keywords (even if that's a crazy long tail)
	if(is.null(how.many)) {
		how.many <- kw.count
	}

	how.wide <- 10 * ceiling(1 + max(kw.table)/10)
	
	if(remake_figs) {
		filename <- paste0(imageloc, "keyword barplot, top ", how.many, ", N", nrow(dataset), ".pdf")
		pdf(file=filename)
	}
	
	if(!horizontal) {
		barplot(kw.table, las=1, xlab="Keywords", ylab="Frequency", axisnames=F, main="Author-Provided Keywords by Frequency Have Very Little Overlap")
	} else {
		barplot(sort(kw.table[1:how.many],decreasing=FALSE), horiz=TRUE, main=paste("Top",how.many,"Keywords by Frequency"), axisnames=TRUE,width=c(10,10),space=0.4,las=1, pty="m")
	}
	# mai=c(5,10,8,5))
	
	# for(i in 1:how.many) {
		# # add a label where x = frequency and y = "device height" or something
			# text(x=(kw.table[[i]] + 20), y=(par()$din[2]), labels=kw.table[[i]],pos=4)
	# }
	
	mtext(paste(nrow(dataset), "dissertations,", kw.count, "keywords, median =", kw.median, ", mean =", kw.mean))
	
	if(remake_figs) { dev.off() }
	
	# if(remake_figs) {	
		# filename <- paste0(imageloc, "keyword terms barplot, below",how.many,", above median, N",diss.count,".pdf")
		# pdf(file=filename)
	# }	
	# barplot(kw.table[how.many+1:length(kw.table)/2],horiz=FALSE,main=paste0("Frequency of Subject Terms below ",how.many,", above median"), axisnames=TRUE,width=c(10,10),space=0.4,las=2, pty="m",mai=c(5,10,8,5))
	
	# if(remake_figs) { dev.off() }
	
	return(kw.table)
}

if(autorun) {
	remake_figs=T
	keyword_barplot()
} else {
    message("`keyword barplot.R` loaded the following function: \n",
            "keyword_barplot(dataset_name='consorts.plus', how.many=NULL, horizontal=FALSE) ")
}

