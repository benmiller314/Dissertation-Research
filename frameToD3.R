# Forked from Rolf Fredheim at https://github.com/benmiller314/frameToD3/blob/master/frameToD3.r
# as discussed in http://quantifyingmemory.blogspot.com/2013/11/d3-without-javascript.html
# Most references to "my" below are his; I'll try to add mine with "Ben:" as needed.

#My sample data is 1000 rows, with the first column a unique id, followed by 100 columns of data variables
# Ben: and it's called 'dt'; in this case the ID = a filename, and the data = topic weights


## Ben: Here be the function
frameToJSON <- function(dataset_name="consorts",
						ntopics=55,
						do.plot=TRUE,			# Use this the first time to find good cuts in the dendrogram.
					    groupVars=NULL, 		# If not provided by the calling environment,
					    dataVars=NULL, 			# these 3 parameters will be set to defaults within the function.
					    outfile=NULL) {			# This should make the function more portable, I (Ben) hope.

  #packages we will need:
  require(data.table)	
  require(jsonlite)		# Ben: was RJSONIO, but now jsonlite, as per http://bit.ly/1jXAC5M
  
  # Ben: Get topic weights for every document we have 
  if(!exists("get.doctopic.grid", mode="function")) { source(file="get doctopic grid.R") }
  dt <- as.data.table(get.doctopic.grid(dataset_name, ntopics)$outputfile)

  # Set parameter defaults if needed
  if(is.null(groupVars)) {				
		groupVars <- c("Pub.number")	# Group by ID column
  }
  if(is.null(dataVars)) {	
  		dataVars <- colnames(dt)[!colnames(dt) %in% groupVars]	# any column that's not an ID is a datapoint
  }
  if(is.null(outfile)) {				# the desired location of the JSON file produced by the function
  		outfile <- paste0(webloc, "/", dataset_name, "k", ntopics, "_clusters.json")
  }

  
  #Rolf: Here you may want to sort by colSums() to keep only the most relevant variables.

  #Rolf: calculate the correlation matrix
  t <- cor(dt[, dataVars, with=F])

  #Rolf: calculate the hierarchical cluster structure from the correlation scores
  hc <- hclust(dist(t), "ward.D2")

  # Ben: I'm making this section optional, because it makes the most sense early on and has diminishing returns.
  if(do.plot) {  
	  #Rolf: take a look at your strucutre:
	  plot(hc)
	  
	  # Ben: Try various cut levels until you find a set that seems interesting; 
	  # Then adjust the memb# variables below, accordingly.
	  rect.hclust(hc, k=50)
	
	  rect.hclust(hc, h=1.5, border="blue")
	  abline(1.5, 0)
	  rect.hclust(hc, k=10, border="#99FF99")
	  rect.hclust(hc, k=50, border="#FF9999")
	  x <- identify(hc, n=13)
	  rect.hclust(hc, k=55, border="#9999FF")

  }
  
  #Rolf: now we split the data based on membership structure. We will take four levels:
  #(basically this means we will calculate which group each variable belongs in for different levels of the tree strucutre)
  ## Ben: so, essentially, we're going to look at plot(hc) and decide what the major branch points are,
  ## then cut the tree to find group assignments above/below those splits. NB cutree() also allows us to 
  ## split the tree at specific heights (on the y axis of that plot), if we don't want to count the groups.
  memb2 <- as.character(cutree(hc, k = 2))
  memb5 <- as.character(cutree(hc, k = 5))
  memb10 <- as.character(cutree(hc, k = 10))
  memb22 <- as.character(cutree(hc, k = 22))
  # memb55 <- as.character(cutree(hc, k = 55))
  
  # Ben: list these out so we can distinguish them from node and edge variables
  membVars <- c("memb2", "memb5", "memb10", "memb22"
   # , "memb55"
   )

  # Ben: get topic labels, which you've composed elsewhere using 'top docs per topic.R'
  if(!exists("get_topic_labels", mode="function")) { source(file="get topic labels.R") }
  topic.labels.dt <- get_topic_labels(dataset_name, ntopics)
  
  #Rolf: Now put this information into a table, together with the labels and the order in which they should appear:
  b <- data.table(memb2,memb5,memb10,memb22
  # , memb55
  ,label=gsub(' ', '_', topic.labels.dt[, Label]), topwords=topic.labels.dt[, Top.Words], order=hc$order)
  
  #Rolf: We might want to know the size of each node. Let's add that
  # Ben: for a topic model, this will find the total %-point contribution of the topic to all docs;
  # that means we could divide by number of docs to scale to [0,1], but no need: it's proportional.
  b$size <- colSums(outputfile.dt[,c(dataVars),with=F])
  b$scaledsize <- b$size/nrow(outputfile.dt)
  
  #Rolf: sort the data so it aligns with the structure calculated using hclust()
  setkey(b,order)

  #Rolf: drop the order variable:
  b[,order:=NULL]
  
  # Ben: Save this data table to a csv for later inspection; this table will also be returned by the function.
  if(remake_figs) {
  	filename <- paste0(imageloc, "topic clusters - ", dataset_name, ", K", ntopics, ".csv")
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
      	list(name=x[,1][y],
      		 size=x[,2][y]
      	)})
    }
  }

  #Rolf: This will not work on a data.table
  b.df <- data.frame(b)
  out <- makeList(b.df)
  # str(out)
  toJSON(out)

  # Have a look at the structure this creates:
  if(autorun) { print (head(out)) }
  
  #Rolf: Basically we have made a list of lists containing the information from the tree diagram.
  #Finally we put everything into a list, convert this to json format and save it as data.json
  jsonOut<-toJSON(list(name="1of1",children=out))

  #Rolf: We use the cat function here, because in some cases you may want to add separators, or a prefix and suffix to make the formatting just right
	# Ben adds: to avoid overwriting, only save this file if remake_figs is TRUE
	if(remake_figs) {  cat(jsonOut,file=outfile)	}

  # Ben: Return the data.table for use in edge bundling, below
  return(b)
}

## Hierarchical Edge Bundling between (possibly unrelated) topics. This one's all Ben, but trying to
#  reconstruct a figure like http://fredheir.github.io/dendroArcs/pages/hierarc/page.html.
#  Plan: From the combined hierarchical data structure above (named `b`), for each topic (row):
#	  1) pull out the "name" field that combines location in hierarchy with label information
#	  2) loop through the targets, and find the "name" corresponding to *that* target
#	  3) convert to JSON.	

cotopic_edges <- function(dataset_name="consorts", ntopics=55, level=0.1, min=1, outfile=NULL) {
	# set default parameters if needed
	if(is.null(outfile)) {				# the desired location of the JSON file produced by the function
  		outfile <- paste0(webloc, "/", "edges_", dataset_name, "k", ntopics, "_", level*100, "pct_min", min, ".json")
	}

	# get co-occurring topics, for hierarchical edge bundling
	if(!exists("get.cotopics")) { source(paste0(sourceloc, "cotopics.R")) }
	cotopics <- get.cotopics(dataset_name, ntopics, level, min)
	  
	# that gives one-directional links; to ensure symmetry, flip source and target and combine.
	cotopics_flip <- data.table(source=cotopics$target, target=cotopics$source, weight=cotopics$weight)
	cotopics_both <- rbind(cotopics, cotopics_flip)
	  
	# aggregate all edges by source
	edges <- cotopics_both[, .SD[, list("targets"=paste(target, collapse=","), "weights"=paste(weight, collapse=","))], by=source]
	setkey(edges, source)
	
	# Bring in the node table
	b <- frameToJSON(do.plot=F)
	
	# add a column of topic numbers to our node table, just to help merge with the edge table
	b$source <- 1:nrow(b)
	setkey(b, source)
	
	# merge
	b <- merge(b, edges, all.x=T) 
	
	# Create a "name" column that collapses the hierarchical structure and topic label, 
	# as per http://fredheir.github.io/dendroArcs/pages/hierarc/test.JSON
	
	b$name <- b[, paste(memb2, memb5, memb10, memb22, label, sep=".")]
	# b$name <- b[, paste(memb2, memb5, memb10, memb22, paste0("_",as.character(source)), sep=".")]
	 
	# We're going to build our JSON for edge bundling with a name, size, and (to take advantage of 
	# Mike Bostock's http://mbostock.github.io/d3/talk/20111116/packages.js) we'll call the edges "imports"
	# We start empty...
	edge_bund <- data.table(name=rep("NA", nrow(b)), size=0.0, imports=list("NA"))
	
	# ... and then build up
	for (i in b$source) {
		edge_bund[i, "name"] <- b[i, name]
		edge_bund[i, "size"] <- round(b[i, size], 0)
		imports <- lapply(strsplit(b[i, targets], ","), FUN=function(x) {
				x <- as.integer(x)
				b[x, name]
			})
		weights <- lapply(strsplit(b[i, weights], ","), FUN=function(x) {
				x <- as.integer(x)
			})	
		if(!anyNA(imports)) { 
			edge_bund[i, "imports"] <- list(imports) 		# list edges if there are any
			edge_bund[i, "weights"] <- list(weights) 
		} else {
			edge_bund[i, "imports"] <- list(b[i, name])		# otherwise, make a loop back to itself
			edge_bund[i, "weights"] <- list(1)			# and call the weight "1"
		}
	}

	jsonEdge <- toJSON(edge_bund, pretty=TRUE)
	cat(jsonEdge, file=outfile)
	return(jsonEdge)
}

if(autorun) { frameToJSON(dt,groupVars,dataVars,outfile="data.json") }
if(autorun) { 
	cotopic_edges(level=0.1, min=1)		# this is the default
	cotopic_edges(level=0.1, min=2)
	cotopic_edges(level=0.2, min=1)
	cotopic_edges(level=0.2, min=2)
	cotopic_edges(level=0.12, min=1)		# 12% determined by `variation of topic proportions.R` to include
	cotopic_edges(level=0.12, min=2)		# nearly all primary topics and 3/4 of secondary topics;
	a <- cotopic_edges(level=0.12, min=3)	# see `Variation of Topic Proportions, Top 10 Topics per Document.pdf`
	cotopic_edges(level=0.12, min=4)
}