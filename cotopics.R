# GOAL: find topics that co-occur within individual dissertations
# at a level greater than (say) 10 or 5%. Map these into a (non-directed) source-target edge table,
# for use in http://bl.ocks.org/mbostock/7607999 (hierarchical edge bundling).
#
# Strategy: 
# 1. in each row i of X, find all columns with X[i,j] > level; call that A.
# 2. For all combinations of two elements in A, create a new row in a source-target table called "cotopics."
	
	
get.cotopics <- function(dataset_name="consorts", ntopics=55, level=.10) {
	require(data.table)
		
	# NB: once it's all functions, we can source the file 'top docs per topic.R'. 
	# For now, though, just make sure we have get.doctopic.grid from that file.

	grid <- get.doctopic.grid(dataset_name, ntopics)$outputfile
		
	cotopics <- data.frame(row.names=c("source","target"))	# start empty, build up
	for (i in 1:nrow(grid)) { 
		A <- which(grid[i, 2:length(grid)] > level)
		if (length(A) >= 2) {								# can't combine just one thing
			cotopics <- cbind(cotopics, combn(A,2))
		} 
	}
	
	# the data.frame gave us a wide array; switch to a long one.
	cotopics <- t(cotopics)
	
	# as a data.table, we can do a fast sort and more besides
	cotopics <- data.table(cotopics, key=c("source", "target"))

	# for example, let's find unique source/target pairs, and count their occurrences! whee!
	cotopics <- cotopics[, list(weight=.N), by=list(source, target)]	

	# print and optionally save the result	
	print(cotopics)	
	
	if(remake_figs) { 
		# filename <- paste0(imageloc, dataset_name, "k", ntopics, "_edges_", level*100, ".json")
		# cat(toJSON(cotopics), file=filename)
		
		filename <- paste0(imageloc, "co-topic edge table, ", dataset_name, ", k", ntopics, ", ", level*100, "pct.csv")
		write.csv(cotopics, filename)
	}
	
	# and pass it back to the calling environment
	return(cotopics)
}

cotopics <- get.cotopics()
cotopics05 <- get.cotopics(level=0.05)
