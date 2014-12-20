########################
# compare method ranks.R
# GOAL: Find the difference in method frequency between two sets
#       by arranging method tags in two columns, and 
#		connecting matching methods with lines for ease of comparison

compare_method_ranks <- function(set1="consorts",			# Sets to compare. NB: use names, not variables,
								 set2="nonconsorts", 		# so we can use them to label the figure.
								 pcts=TRUE, 				# Label with percent of docs with that tag? 
								 							# otherwise, use real counts. 
								 colorful=FALSE, 			# Use multiple colors for lines?
								 betterlabels=NULL) {		# Optional vector of length 2, giving labels for sets.
	if(!exists("get_tags", mode="function")) { source(file="get tags.R") }
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
						  "%)")
		d2 <- paste0(d1, " (", 
								round(100*d0[order(d0, decreasing=T)]/nrow(get(set2)), 0), 
						  "%)")
	
		filename <- paste0(imageloc, "Ranks of methods in ", set1, " v ", set2, ", no Othr, pcts.pdf")
	}	
	
	if(remake_figs) { pdf(file=filename) }

		# set up a blank plot
		plot(x=0:length(b)+1, y=0:length(b)+1, axes=FALSE, type="n", xlab="", ylab="")
		
		# arrange set1 in descending rank order on the left, set2 on the right
		text(labels=b2, 
			 x=rep(4, length(b2)), 
			 y=length(b2):1
		)
		text(labels=d2, 
			x=rep(length(d)-4, length(d2)), 
			y=length(d2):1
		)
		
		## connect matching methods with lines for ease of comparison

		# optionally add color to lines to detangle spaghetti
		if(colorful) {
			require(RColorBrewer)
			mycol <- brewer.pal(4, "Dark2")
		} else {
			mycol <- c("#000000")
		}
		
		tag <- b1[1]
		lapply(b1, mycol=mycol, FUN=function(tag, mycol) {
			# locate each tag on the plot
			y.left  <- length(b2) - grep(tag, b1) + 1
			y.right <- length(b2) - grep(tag, d1) + 1
			col.index <- (y.left-1) %% length(mycol) + 1
			
			# draw a line from each tag's position on the left to the one on the right	
			segments(x0=5.7, 
					 y0=y.left,
			         x1=length(b)-5.7, 
			         y1=y.right,
			         col=mycol[col.index]
			)
			
			# extend those lines to point horizontally to the tags, to remove ambiguity
			segments(x0=5.4, y0=y.left,
					 x1=5.7, y1=y.left,
			         col=mycol[col.index])
			segments(x0=length(b)-5.4, y0=y.right,
					 x1=length(b)-5.7, y1=y.right,
			         col=mycol[col.index])		
		})

		# label the two columns
		if(!is.null(betterlabels)) { 
			if(length(betterlabels)==2) {
				text(labels=betterlabels, x=c(4,length(b)-4), y=rep(length(b)+1,2))
			} else {
				warning("Incorrect number of betterlabels: must be vector of length 2. Using set names.")
				text(labels=c(set1, set2), x=c(4,length(b)-4), y=rep(length(b)+1,2))
			}
		} else {
			text(labels=c(set1, set2), x=c(4,length(b)-4), y=rep(length(b)+1,2))			
		}
	
		text(labels=c(paste0("(N=",nrow(get(set1)),")"), paste0("(N=",nrow(get(set2)),")")), x=c(4,length(b)-4), y=rep(length(b),2))
	
		
	if (remake_figs) { dev.off() }
}
