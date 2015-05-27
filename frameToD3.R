# Forked from Rolf Fredheim at https://github.com/benmiller314/frameToD3/blob/master/frameToD3.r
# as discussed in http://quantifyingmemory.blogspot.com/2013/11/d3-without-javascript.html
# Most references to "my" below are his; I'll try to mark mine with "Ben:" as needed.

#My sample data is 1000 rows, with the first column a unique id, followed by 100 columns of data variables
# Ben: and it's called 'dt'; in this case the ID = a filename, and the data = topic weights


## Ben: Here be the function
frameToJSON <- function(dataset_name="consorts",
						ntopics=55,
						do.plot=TRUE,			# Ben: Use this the first time to find good cuts in the dendrogram.
					    groupVars=NULL, 		# Ben: If not provided by the calling environment,
					    dataVars=NULL, 			#  these 3 parameters will be set to defaults within the function.
					    outfile=NULL,			#  This should make the function more portable, I hope.
					    bad.topics= c("2", "4", "22", "24", "47", 	# exclude non-content-bearing topics
					    				"50", "13")					# (incl. Spanish and Italian languages)
					    ) {			

  #packages we will need:
  require(data.table)	
  require(jsonlite)		# Ben: was RJSONIO, but now jsonlite, as per http://bit.ly/1jXAC5M
  
  # Ben: Get topic weights for every document we have 
  if(!exists("get.doctopic.grid", mode="function")) { source(file="get doctopic grid.R") }
  dt <- as.data.table(get.doctopic.grid(dataset_name, ntopics)$outputfile)

  # Ben: Exclude non-content-bearing topics
  if(!is.null(bad.topics)) { dt <- dt[, !names(dt) %in% bad.topics, with=F] }

  # Set parameter defaults if needed
  if(is.null(groupVars)) {				
		groupVars <- c("Pub.number")	# Group by ID column
  }
  if(is.null(dataVars)) {	
  		dataVars <- colnames(dt)[!colnames(dt) %in% groupVars]	# any column that's not an ID is a datapoint
  }
  if(is.null(outfile)) {				# the desired location of the JSON file produced by the function
  		outfile <- paste0(webloc, "/", dataset_name, "k", ntopics, "_clusters_", ntopics-length(bad.topics), ".json")
  }

  
  #Rolf: Here you may want to sort by colSums() to keep only the most relevant variables.


  #Rolf: calculate the correlation matrix
  t <- cor(dt[, dataVars, with=F])

  #Rolf: calculate the hierarchical cluster structure from the correlation scores
  hc <- hclust(dist(t), "ward.D2")

  # Ben: I'm making this section optional, because it makes the most sense early on and has diminishing returns.
  if(do.plot) {  
	  #Rolf: take a look at your strucutre:
	  # Ben: optionally save clustering figure
	  main <- paste0("Cluster Dendrogram, ", dataset_name, ", ", ntopics - length(bad.topics), " topics")
	
	  # Ben: Try various cut levels until you find a set that seems interesting; 
	  # Then adjust the memb# variables below, accordingly.
	
	  # with 5 bad.topics removed	
	  if(dataset_name=="consorts" && ntopics==55 && length(bad.topics) == 5) {
	      if(remake_figs) { pdf(file=paste0(imageloc, main, ".pdf")) }
			plot(hc, main=main)
			abline(1.35, 0, col="#99FF99")			
			rect.hclust(hc, k=32, border="#99FF99")	
			abline(1.55, 0, col="#009900")			
			rect.hclust(hc, k=16, border="#009900")	
			abline(1.7, 0, col="#FF9999")
			rect.hclust(hc, k=12, border="#FF9999")
			abline(1.85, 0, col="#9999FF")
			rect.hclust(hc, k=7, border="#9999FF")
			abline(1.95, 0, col="#990099")
			rect.hclust(hc, k=6, border="#990099")
			abline(2.33, 0, col="#009999")
			rect.hclust(hc, k=4, border="#009999")
			abline(3.37, 0, col="#999900")
			rect.hclust(hc, k=2, border="#999900")
	 	  if(remake_figs) { dev.off() } 
		}
  	  # with 7 bad.topics removed	
  	  else if(dataset_name=="consorts" && ntopics==55 && length(bad.topics) == 7) {
	      if(remake_figs) { pdf(file=paste0(imageloc, main, ".pdf")) }
    		  plot(hc, main=main)
			  abline(1.45, 0, col="#99FF99")		
			  rect.hclust(hc, k=21, border="#99FF99")	
			  abline(1.73, 0, col="#009900")			
			  rect.hclust(hc, k=11, border="#009900")
			  abline(1.955, 0, col="#FF9999")
			  rect.hclust(hc, k=6, border="#FF9999")
			  rect.hclust(hc, k=4, border="#009999")
		  	  rect.hclust(hc, k=2, border="#999900")	  
	 	  if(remake_figs) { dev.off() } 
	  }		
	  
	  # TO DO: Find splits for model with 150 topics
	  else {
	      if(remake_figs) { pdf(file=paste0(imageloc, main, ".pdf")) }
			  plot(hc, main=main)
	 	  if(remake_figs) { dev.off() } 
		  
		  # If we're plotting, we probably wanted to locate splits. Exit the function here.
		  message("Exiting function.")
		  message("Using abline() and rect.hclust(), try various cut levels until you find a set that seems promising.")
		  return()
	  }  
  }	  # end of if(do.plot)
  
  #Rolf: now we split the data based on membership structure. We will take four levels:
  #(basically this means we will calculate which group each variable belongs in for different levels of the tree strucutre)
  ## Ben: so, essentially, we're going to look at plot(hc) and decide what the major branch points are,
  ## then cut the tree to find group assignments above/below those splits. NB cutree() also allows us to 
  ## split the tree at specific heights (on the y axis of that plot), if we don't want to count the groups.

	# Ben: splits for consorts with 55 topics (i.e. including bad.topics)
if(dataset_name=="consorts" && ntopics==55 && is.null(bad.topics)) {
  splits <- c(2, 5, 10, 22, 55)
  
  memb2 <- as.character(cutree(hc, k = 2))
  memb5 <- as.character(cutree(hc, k = 5))
  memb10 <- as.character(cutree(hc, k = 10))
  memb22 <- as.character(cutree(hc, k = 22))
  memb55 <- as.character(cutree(hc, k = 55))
}

	# Ben: splits for consorts with 50 topics (i.e. 5 bad.topics removed for bad OCR or boilerplate)
if(dataset_name=="consorts" && ntopics==55 && length(bad.topics) == 5) {
  splits <- c(2, 4, 6, 7, 12, 16, 32)

  memb2 <- as.character(cutree(hc, k = 2))
  memb4 <- as.character(cutree(hc, k = 4))
  memb6 <- as.character(cutree(hc, k = 6))
  memb7 <- as.character(cutree(hc, k = 7))
  memb12 <- as.character(cutree(hc, k = 12))
  memb16 <- as.character(cutree(hc, k = 16))
  memb32 <- as.character(cutree(hc, k = 32))
}  
  

	# Ben: splits for consorts with 48 topics (i.e. 7 bad.topics removed for bad OCR, boilerplate, or non-English lang)
if(dataset_name=="consorts" && ntopics==55 && length(bad.topics) == 7) {
  splits <- c(2, 4, 6, 11, 21)

  memb2 <- as.character(cutree(hc, k = 2))
  memb4 <- as.character(cutree(hc, k = 4))
  memb6 <- as.character(cutree(hc, k = 6))
  memb11 <- as.character(cutree(hc, k = 11))
  memb21 <- as.character(cutree(hc, k = 21))
}   

	# TO DO: Add splits for model with 150 topics

  # Make note of group names for later; same operation for all numbers of bad.topics
  membVars <- paste0("memb", splits)

  # Ben: get topic labels, which you've composed elsewhere using 'top docs per topic.R'
  if(!exists("get_topic_labels", mode="function")) { source(file="get topic labels.R") }
  topic.labels.dt <- get_topic_labels(dataset_name, ntopics)
	# str(topic.labels.dt)
	  
  # exclude non-content-bearing topics
  if(!is.null(bad.topics)) { topic.labels.dt <- topic.labels.dt[!Topic %in% bad.topics]	}
    
  #Rolf: Now put this information into a table, together with the labels and the order in which they should appear:
  # Ben adds: use gsub to remove spaces (this seems to help the d3 scrollover); add topic number to aid in merging w/ edge table later
  b <- data.table(sapply(membVars, FUN=function(var){ get(as.character(var)) } ), 
  		label = gsub(' ', '_', topic.labels.dt[, Label]), 
  		topic = topic.labels.dt[, Topic], 
  		topwords = topic.labels.dt[, Top.Words], 
  		rank = topic.labels.dt[, Rank], 
  		order = hc$order)

  #Rolf: We might want to know the size of each node. Let's add that
  # Ben: for a topic model, this will find the total %-point contribution of the topic to all docs;
  # that means we could divide by number of docs to scale to [0,1], but no need: it's proportional.
  b$size <- colSums(dt[,c(dataVars),with=F])
  b$scaledsize <- b$size/nrow(dt)
  
  #Rolf: sort the data so it aligns with the structure calculated using hclust()
  setkey(b,order)

  #Rolf: drop the order variable:
  b[,order:=NULL]
  
  # Ben: Save this data table to a csv for later inspection; this table will also be returned by the function.
  if(remake_figs) {
  	filename <- paste0(imageloc, "topic clusters - ", dataset_name, ", K", ntopics, "bad topics removed.csv")
  	write.csv(b, filename)
  } else {
  	# print(b)
  }


## Hierarchical Clustering of Topics by Similarity  
  #Rolf: we define a function which will create a nested list in JSON format:
  #From here: http://stackoverflow.com/questions/12818864/how-to-write-to-json-with-children-from-r
  # Ben: but see also, now, http://bit.ly/1jXAC5M

  makeList<-function(x){
    if(any(names(x) %in% membVars) && ncol(x)>2){
      listSplit<-split(x[-1],x[1],drop=T)
      grp <- names(x)[1]
      grpnum <- substr(grp, 5, nchar(grp))
      names(listSplit) <- paste0(names(listSplit), "of", grpnum)
      lapply(names(listSplit), function(y){list(name=y,children=makeList(listSplit[[y]]))})
    }else{
      lapply(seq(nrow(x[1])), function(y){
      	list(name=x[,"label"][y],
      		 size=x[,"size"][y],
      		 scaledsize=x[,"scaledsize"][y],
      		 topwords=x[,"topwords"][y],
      		 topic=x[,"topic"][y],
      		 rank=x[,"rank"][y]
      	)})
    }
  }

  #Rolf: This will not work on a data.table
  b.df <- data.frame(b)
  out <- makeList(b.df)
  # str(out)
  # toJSON(out)

  # Have a look at the structure this creates:
  if(autorun) { print (head(out)) }
  
  #Rolf: Basically we have made a list of lists containing the information from the tree diagram.
  #Finally we put everything into a list, convert this to json format and save it as data.json
  jsonOut<-toJSON(list(name="1of1",children=out), digits=6, pretty=TRUE)

  #Rolf: We use the cat function here, because in some cases you may want to add separators, or a prefix and suffix to make the formatting just right
	# Ben adds: to avoid overwriting, only save this file if remake_figs is TRUE
	if(remake_figs) {  cat(jsonOut,file=outfile)	}

  # Ben: Return the data.table for use in edge bundling, below
  return(b)
}

## Hierarchical Edge Bundling between (possibly unrelated) topics. This one's all Ben, but trying to
#  reconstruct a figure like Rolf's http://fredheir.github.io/dendroArcs/pages/hierarc/page.html.
#  Plan: From the combined hierarchical data structure above (named `b`), for each topic (row):
#	  1) pull out the "name" field that combines location in hierarchy with label information
#	  2) loop through the targets, and find the "name" corresponding to *that* target
#	  3) convert to JSON.	

cotopic_edges <- function(dataset_name="consorts", 
						  ntopics=55, 
						  level=0.12, 	# topic must constitute how much of each doc?
						  min=3, 		# how many times must a pair of topics co-occur?
						  outfile=NULL,
						  bad.topics= c("2", "4", "22", "24", "47", "50", "13")	# exclude non-content-bearing topics
						  ){
	# set default parameters if needed
	if(is.null(outfile)) {				# the desired location of the JSON file produced by the function
  		outfile <- paste0(webloc, "/", "edges_", dataset_name, "k", ntopics, "_", ntopics-length(bad.topics), "_", level*100, "pct_min", min, "_nobads.json")
	}

	# get co-occurring topics, for hierarchical edge bundling
	if(!exists("get.cotopics")) { source(paste0(sourceloc, "cotopics.R")) }
	cotopics <- get.cotopics(dataset_name, ntopics, level, min)

	# # exclude non-content-bearing topics		# UPDATE: now taken care of in cotopics.R, but check to make sure.
	# cotopics <- cotopics[!(source %in% bad.topics)]
	# cotopics <- cotopics[!(target %in% bad.topics)]
	  
	# that gives one-directional links; to ensure symmetry, flip source and target and combine.
	cotopics_flip <- data.table(source=cotopics$target, target=cotopics$source, weight=cotopics$weight)
	cotopics_both <- rbind(cotopics, cotopics_flip)
	  
	# aggregate all edges by source
	edges <- cotopics_both[, .SD[, list("targets"=paste(target, collapse=","), "weights"=paste(weight, collapse=","))], by=source]
	setkey(edges, source)
	# head(edges)
	
	# Bring in the node table
	b <- frameToJSON(dataset_name, ntopics, bad.topics=bad.topics, do.plot=F)
	setkey(b, topic)
	# head(b)
	
	# merge
	b <- edges[b, ]
	str(b)
		
	# Create a "name" column that collapses the hierarchical structure and topic label, 
	# as per http://fredheir.github.io/dendroArcs/pages/hierarc/test.JSON
	# This is what the d3 edge bundling code in packages.js will parse to recreate the hierarchy

	# first re-derive `membVars` from the names of b that include "memb"	
	membVars <- names(b)[grep("memb", names(b))]

	b$name <- sapply(1:nrow(b), FUN=function(x) { paste(b[x, c(membVars, "label"), with=F], collapse=".") })
	# b$name <- b[, paste(memb2, memb4, memb6, memb7, memb12, memb16, memb32, label, sep=".")]
	# b$name <- b[, paste(memb2, memb5, memb10, memb22, paste0("_",as.character(source)), sep=".")]
	 
	# We're going to build our JSON for edge bundling with a name, size, and (to take advantage of 
	# Mike Bostock's http://mbostock.github.io/d3/talk/20111116/packages.js) we'll call the edges "imports"
	# We start empty...
	edge_bund <- data.table(name=rep("NA", max(b$source)), topic=0, size=0.0, imports=list("NA"))
	
	# ... and then build up
	for (i in b$source) {
		edge_bund[i, "topic"] <- i
		edge_bund[i, "name"] <- b[source %in% i, name]
		edge_bund[i, "rank"] <- b[source %in% i, rank]
		edge_bund[i, "size"] <- b[source %in% i, size]
		edge_bund[i, "scaledsize"] <- b[source %in% i, scaledsize] * 100
		edge_bund[i, "topwords"] <- b[source %in% i, topwords]
		imports <- lapply(strsplit(b[source %in% i, targets], ","), FUN=function(x) {		# extract targets' topic numbers
				x <- as.integer(x)												# convert from string to numeric
				b[source %in% x, name]											# match topic numbers to sources
			})
		weights <- lapply(strsplit(b[source %in% i, weights], ","), FUN=function(x) {
				x <- as.integer(x)										# weights correspond by position in array
			})	
		if(!anyNA(imports[[1]])) { 
			edge_bund[i, "imports"] <- list(imports) 		# list edges if there are any
			edge_bund[i, "weights"] <- list(weights) 
		} else {
			edge_bund[i, "imports"] <- list(b[source %in% i, name])		# otherwise, make a loop back to itself
			edge_bund[i, "weights"] <- list(1)			# and call the weight "1"
		}		
	}

	# Now remove any empty rows introduced by cutting bad.topics
	edge_bund <- edge_bund[!(name %in% "NA")]

	jsonEdge <- toJSON(edge_bund, pretty=TRUE)
	cat(jsonEdge, file=outfile)
	return(jsonEdge)
}


if(autorun) { 
	remake_figs 
	# debug(frameToJSON)
	frameToJSON(do.plot=F)
	frameToJSON(ntopics=150, bad.topics=NULL)
}
if(autorun) { 
	cotopic_edges(level=0.12, min=1)		# 12% determined by `variation of topic proportions.R` to include
	cotopic_edges(level=0.12, min=2)		# nearly all primary topics and 3/4 of secondary topics;
	cotopic_edges(level=0.12, min=3)	# see `Variation of Topic Proportions, Top 10 Topics per Document.pdf`
	cotopic_edges(level=0.12, min=4)
	cotopic_edges(level=0.12, min=5)
}