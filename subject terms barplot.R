# Assumes we've already run dataprep.R.
# Extract subject terms, make a frequency table and calculate some stats. 

# First get the terms:
extract_subjects(noexcludes$Subject) -> subj.list

# Get the frequency chart. maxsum is needed to avoid "othering" half the list.
summary(subj.list, maxsum=300) -> subj.table	

# Put the list in descending order by frequency, and chop out the term they all share
sort(subj.table,decreasing=TRUE) -> subj.table
subj.table[2:length(subj.table)] -> subj.table

subj.count <- length(subj.table)
subj.mean <- mean(subj.table)
subj.median <- median(subj.table)

how.many <- 30

filename <- paste("subject terms barplot, top ",how.many,", N",diss.count,".pdf",collapse="")

pdf(file=filename)

barplot(sort(subj.table[1:how.many],decreasing=FALSE),horiz=TRUE,main=paste("Top",how.many,"Subject Terms by Frequency"), axisnames=TRUE,width=c(10,10),space=0.4,las=1, pty="m",mai=c(5,10,8,5))

for(i in 1:how.many) {
	# if(subj.table[[i]] > 20) {
		# add a label where x = frequency and y = "device height" or something
		text(x=(subj.table[[i]] + 20), y=(par()$din[2]), labels=subj.table[[i]],pos=4)
	# } else {
		# text(subj.table[[i]] + 20, -i, subj.table[[i]])
	# }
}

mtext(paste(diss.count,"theses,",subj.count,"subjects, median =",subj.median,", mean =",subj.mean,collapse=" "))

dev.off()

filename2 <- paste("subject terms barplot, below",how.many,", above median, N",diss.count,".pdf",collapse="")

pdf(file=filename2)

# for some unknown reason, how.many+1 still includes the 30th item. :\
barplot(subj.table[how.many+2:length(subj.table)/2],horiz=FALSE,main=paste("Frequency of Subject Terms below",how.many,"above median"), axisnames=TRUE,width=c(10,10),space=0.4,las=2, pty="m",mai=c(5,10,8,5))

dev.off()