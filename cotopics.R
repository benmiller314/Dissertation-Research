# `cotopics.R`
# GOAL: find topics that co-occur within individual dissertations at a level
# greater than (say) 10 or 5%. Map these into a (non-directed) source-target
# edge table, for use in http://bl.ocks.org/mbostock/7607999 (hierarchical
# edge bundling).
#
# Strategy:
# 1. in each row i of X, find all columns with X[i,j] > level; call that A.
# 2. For all combinations of two elements in A, create a new row in a
# source-target table called "cotopics."


get.cotopics <- function(dataset_name = "noexcludes2001_2015",
		 ntopics = 50,
		 subset_name = "knownprograms2001_2015",
		 iter_index = 1,
		 newnames = F,          # where in the MALLET output filename does iter_index appear?
		 						# set T if it's with the model, F if last in filename.
								# Gets passed into get.doctopic.grid.
		 level = .13, 			# what fraction of the doc (out of 1) must
		 						# each topic account for?
		 outfile = NULL,        # optionally pass a name; use defaults otherwise
		 json = T, 				# export to JSON?
		 min = 3,				# how many times must these topics co-occur
		 						# to be "co-topics"?
		 bad.topics = NULL,		# optionally exclude non-content-bearing topics
		 dt = NULL              # optionally pass doc-topic grid
		)
{

	require(data.table)

    if(is.null(dt)) {
    	if (!exists("get.doctopic.grid", mode="function")) {
    		source(file="get doctopic grid.R")
    	}
        
    	dt <- get.doctopic.grid(dataset_name=dataset_name, ntopics=ntopics,
    	                        subset_name=subset_name, iter_index=iter_index,
    							newnames=newnames)$outputfile.dt
    	
    	dt <- dt[, setdiff(names(dt), bad.topics), with=F]
    	dt <- na.omit(dt)
    	# head(dt)
    }

	# start empty, build up.
	cotopics <- data.frame(row.names=c("source","target"))
	for (i in 1:nrow(dt)) { 			# loop through the documents (rows).
	# for (i in 1:5) {  # test values
		# find which topics (columns) make up a big chunk.
		A <- which(dt[i, 2:length(dt)] > level)

		# can't combine just one thing.
		if (length(A) >= 2) {
			# don't forget to get topic names, not col numbers!
			A <- as.integer(names(dt[, 1+A, with=F]))

			# print(combn(A,2))
			
			# find all pairs of those big-chunk topics.
			cotopics <- cbind(cotopics, combn(A,2))
			
			# print(cotopics)
		}
	}


	# the data.frame gave us a wide array; switch to a long one.
	cotopics <- t(cotopics)

	# as a data.table, we can do a fast sort and more besides
	cotopics <- data.table(cotopics, key=c("source", "target"))

	# for example, let's find unique source/target pairs,
	# and count their occurrences! in one line! whee!
	cotopics <- cotopics[, list(weight=.N), by=list(source, target)]

	# to reduce complexity, set a minimum number of co-occurrences
	cotopics.over.min <- cotopics[which(weight >= min), ]

	# print and optionally save the result
	if(remake_figs) {
	    
	    if(!exists ("build_plot_title")) {
	        source(file="build_plot_title.R")
	    }
	    outfile_slug <- build_plot_title(dataset_name=dataset_name,
	                                ntopics=ntopics,
	                                iter_index = iter_index,
	                                subset_name = subset_name,
	                                whatitis = paste0("cotopic-edges_", level*100),
	                                for.filename = TRUE)
	    
		if(json) {
			require(jsonlite)
		    
		    if(is.null(outfile)) {
		        outfile <- file.path(imageloc, paste0(outfile_slug, ".json"))
		    } else {
		        if(substring(outfile, nchar(outfile)-4) != ".json")	{
		            warning("Output file does not use .json extension, but json option is selected")
		        }
			} 
		    
		    tryCatch(
		        expr = cat(toJSON(cotopics.over.min), file=outfile),
		        error = warning("file save failed:", match.call()),
		        finally = message("Cotopic edges file saved to ", outfile)
		    )
	    } else {
	        if(is.null(outfile)) {
	            outfile <- file.path(imageloc, paste0(outfile_slug, ".csv"))
	        } else {
	            if(substring(outfile, nchar(outfile)-3) != ".csv")	{
	                warning("Output file does not use .csv extension, but json option is not selected ",
	                        "and csv is the only other currently implemented option")
	            } 
	        }
	        tryCatch(
	            expr = write.csv(cotopics.over.min, filename),
	            error = warning("file save failed:", match.call()),
	            finally = message("Cotopic edges file saved to ", outfile)
	        )
					
		}
	} # end of remake_figs=T

	# and pass it back to the calling environment
	return(cotopics.over.min)
}

if(autorun) {
	get.cotopics(level=0.13, min=4)
}

# Other options
# # cotopics <- get.cotopics()
# cotopics20 <- get.cotopics(level=0.2)
# cotopics05 <- get.cotopics(level=0.05)


  # # get cotopics, for hierarchical edge bundling
  # source(paste0(sourceloc, "cotopics.R"))
  # cotopics20 <- get.cotopics(level=0.2)
  # targets <- cotopics20[, .SD[, paste(target, collapse=",")], by=source]
  # targets <- strsplit(targets$V1, ",")
  # weights <- cotopics20[, .SD[, paste(weight, collapse=",")], by=source]
  # weights <- strsplit(weights$V1, ",")
