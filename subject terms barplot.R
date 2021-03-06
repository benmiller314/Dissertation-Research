#############################################################################
# subject terms barplot.R
#
# Goal: Extract subject terms, make a frequency table and calculate some
# stats. 
# Dependencies: `extract subjects.R` (sourced during `data prep.R`)
#####

subject.barplot <- function(dataset_name = "knownprograms2001_2015", 
						    top.many = 30, 		# Plot this many terms,
						    					# starting from the top
						    maxsum = NULL,		# Set high to avoid "other"
						    pct.deep = 0.5,		# How far into the list of 
						    					# terms should we go? 
						    					# Default of 0.5 means
						    					# halfway, i.e. to the
						    					# median; for all the way,
						    					# set pct.deep=1.
						    subj.column = "Subject.Terms"
						    ) 
{
	# Get the data
	dataset <- get(dataset_name)
	subj.list <- extract_subjects(dataset[,subj.column])

	# Get the frequency chart. 
	# maxsum is needed to avoid "othering" half the list.
	if(!is.null(maxsum)) {
    	subj.table <- summary(subj.list, maxsum=maxsum)
	} else {
	    subj.table <- summary(subj.list, maxsum=length(subj.list))
	}
	
	# Put the list in descending order by frequency, 
	# and --chop out the term they all share-- NO THANK YOU, PAST SELF
	subj.table <- sort(subj.table,decreasing=TRUE) 
	# subj.table <- subj.table[2:length(subj.table)] 

	subj.count <- length(subj.table)
	subj.mean <- mean(subj.table)
	subj.median <- median(subj.table)

	if(remake_figs) {
		filename <- paste0(imageloc, dataset_name, 
							" subject terms barplot, ",
							"top ", top.many, ", N", nrow(dataset), ".pdf")
		pdf(file=filename)
	} 
	
	barplot(sort(subj.table[1:top.many], decreasing = FALSE),
			horiz = TRUE,
			main = paste("Top", top.many, "Subject Terms by Frequency"),
			sub = paste0(dataset_name, ", N", nrow(dataset)),
			axisnames = TRUE,
			width = c(10,10),
			space = 0.4,
			las = 1, 
			pty = "m",
			mai = c(5,10,8,5)
	)

	for(i in 1:top.many) {
		# if(subj.table[[i]] > 20) {
			# add a label where x = frequency and y = "device height" or
			# something?
			text(x = (subj.table[[i]] + 20), 
			     y = (par()$din[2]), 
			     labels = subj.table[[i]],
			     pos = 4
			)
		# } else {
			# text(subj.table[[i]] + 20, -i, subj.table[[i]])
		# }
	}

	mtext(paste(nrow(dataset), "theses,", subj.count, 
			"subjects, median =", subj.median, ", mean =", subj.mean))

	if(remake_figs) {
		dev.off()
		
		## Now separately plot the remainder of the terms

		filename <- paste0(imageloc, dataset_name, 
					"subject terms barplot, below",
					 top.many, ", above median, N", nrow(dataset), ".pdf")

		pdf(file=filename)
	}

	# for some unknown reason, top.many+1 still includes the 30th item. :\
	barplot(subj.table[top.many+2:length(subj.table)*pct.deep], 
			horiz = FALSE,
			main = paste("Frequency of Subject Terms below", top.many, 
						 "but above median"),
			axisnames = TRUE,
			width = c(10,10),
			space = 0.4,
			las = 2, 
			pty = "m",
			mai = c(5,10,8,5)
	)
	
	if(remake_figs) {
		dev.off()
	}
	
	return(sort(subj.table[1:top.many], decreasing=T))	
	
} # end of wrapper function subject.barplot()

###### Function to limit results by subject term ######
subject.search <- function(term = NULL,
                           dataset_name = "knownprograms2001_2015", 
                           subj.column = "Subject.Terms",
                           ignore.case = TRUE ) {
    if(is.null(term)) {
        stop("subject.search(): term to search for is required.")
    }
    
    dataset <- get(dataset_name)
    index <- grep(term, dataset[, subj.column], ignore.case=ignore.case)
    
    return(dataset[index, ])
}


if(autorun) {
	remake_figs
	subject.barplot()
} else {
    message("`subject terms barplot.R` loaded the following functions: \n",
            "subject.barplot(dataset_name = 'knownprograms2001_2015', ...) \n",
            "subject.search(term, dataset_name = 'knownprograms2001_2015', ...)")
}
