# GOAL: find topics that co-occur within individual dissertations
# at a level greater than (say) 10 or 5%. Map these into a (non-directed) source-target edge table,
# for use in http://bl.ocks.org/mbostock/7607999 (hierarchical edge bundling).
#
# Strategy: 
# 1. in each row i of X, find all columns with X[i,j] > level; call that A.
# 2. For all combinations of two elements in A, create a new row in a source-target table called "cotopics."
	
	
get.cotopics <- function(dataset_name="consorts", 
						 ntopics=55, 
						 level=.12, 				# what fraction of the doc (out of 1) must each topic account for?
						 json=F, 					# export to JSON?
						 min=3,						# how many times must these topics co-occur to be "co-topics"?
						 bad.topics=c("2", "4", "22", "24", "47")	# exclude non-content-bearing topics
						 ) {
	require(data.table)

	if (!exists("get.doctopic.grid", mode="function")) { source(file="get doctopic grid.R") }
	grid <- get.doctopic.grid(dataset_name, ntopics)$outputfile
	head(grid)
	grid <- grid[, !names(grid) %in% bad.topics]
	head(grid)

	cotopics <- data.frame(row.names=c("source","target"))	# start empty, build up.
	for (i in 1:nrow(grid)) { 								# loop through the documents (rows). in each,
		A <- which(grid[i, 2:length(grid)] > level)			# find which topics (columns) make up a big chunk. 
		if (length(A) >= 2) {								# can't combine just one thing.
			A <- as.integer(names(grid[, 1+A]))				# don't forget to get topic names, not col numbers!
			cotopics <- cbind(cotopics, combn(A,2))			# find all pairs of those big-chunk topics.
		} 
	}

	
	# the data.frame gave us a wide array; switch to a long one.
	cotopics <- t(cotopics)
	
	# as a data.table, we can do a fast sort and more besides
	cotopics <- data.table(cotopics, key=c("source", "target"))

	# for example, let's find unique source/target pairs, and count their occurrences! in one line! whee!
	cotopics <- cotopics[, list(weight=.N), by=list(source, target)]
	
	# to reduce complexity, set a minimum number of co-occurrences
	cotopics <- cotopics[which(weight > min), ]

	# print and optionally save the result	
	if(autorun) { 
		print(cotopics) 
		
		if(remake_figs) { 
			if(json) {
				require(jsonlite)
				filename <- paste0(imageloc, dataset_name, "k", ntopics, "_edges_", level*100, ".json")
				cat(toJSON(cotopics), file=filename)
			} else {
			filename <- paste0(imageloc, "co-topic edge table, ", dataset_name, ", k", ntopics, ", ", level*100, "pct_nobads.csv")
			write.csv(cotopics, filename)
			}
	}
	}
	
	# and pass it back to the calling environment
	return(cotopics)
}

if(autorun) { get.cotopics(level=0.2, min=2) }

# # cotopics <- get.cotopics()
# cotopics20 <- get.cotopics(level=0.2)
# cotopics05 <- get.cotopics(level=0.05)


# ## convert one of these edge tables to a json object for hierarchical edge bundling
# #  (adapted from frameToD3.R)
# # cotopics20
# cotopics20[, paste("[{", list(.SD[,paste("[target:", target, "], [weight:", weight, "]}", collapse=", {")]), "]"), by=source]
# cotopics20[, .SD[, paste(target, weight, collapse=",")], by=source]
# # read.table(outfile)



  # # get cotopics, for hierarchical edge bundling
  # source(paste0(sourceloc, "cotopics.R"))
  # cotopics20 <- get.cotopics(level=0.2)
  # targets <- cotopics20[, .SD[, paste(target, collapse=",")], by=source]
  # targets <- strsplit(targets$V1, ",")
  # weights <- cotopics20[, .SD[, paste(weight, collapse=",")], by=source]
  # weights <- strsplit(weights$V1, ",")