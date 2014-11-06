########################
# compare method ranks.R
# GOAL: Find the difference in method frequency between two sets
#       by arranging method tags in two columns, and 
#		connecting matching methods with lines for ease of comparison

compare_method_ranks <- function(set1="consorts", set2="nonconsorts", pcts=TRUE, colors=FALSE) {
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
