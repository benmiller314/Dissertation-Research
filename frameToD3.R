# Forked from Rolf Fredheim at https://github.com/benmiller314/frameToD3/blob/master/frameToD3.r
# as discussed in http://quantifyingmemory.blogspot.com/2013/11/d3-without-javascript.html
# All references to "my" below are his; I'll add mine with "Ben:" as needed.

#My sample data is 1000 rows, with the first column a unique id, followed by 100 columns of data variables
# Ben: and it's called 'dt'; in this case the ID = a filename, and the data = topic weights

# 
# load("sampleData")
# head(dt)

# Ben: set params
dataset_name <- "consorts"
ntopics <- 55
cutoff <- 150	


# Ben: Get topic weights for every document we have
if(!exists("get.doctopic.grid")) { 
	autorun.bk <- autorun; autorun <- F; source("top docs per topic.R"); autorun <- autorun.bk; rm(autorun.bk)
}
grid <- get.doctopic.grid(dataset_name, ntopics)$outputfile
outputfile.dt <- as.data.table(grid)
rm(grid)


# Ben: group by ID column; any column that's not an ID is a datapoint 
groupVars <- c("Pub.number")	
dataVars <-  colnames(outputfile.dt)[!colnames(outputfile.dt) %in% groupVars]	

# Ben: desired output filename to pass to d3
webloc <- "/Users/benmiller314/Documents/Webdev/radial_clusters"
outfile <- paste0(webloc, "/", dataset_name, "k", ntopics, "_clusters.json")	

## Ben: Here be the function
frameToJSON <- function(dt="outputfile.dt", groupVars, dataVars, outfile) {
  #packages we will need:
  require(data.table)	# Ben: okay, this package is awesome, glad to know about it now!
  require(jsonlite)		# Ben: was RJSONIO, but now jsonlite, as per http://bit.ly/1jXAC5M
  
  #Here you may want to sort by colSums() to keep only the most relevant variables.

  #calculate the correlation matrix
  t <- cor(outputfile.dt[,c(!colnames(outputfile.dt) %in% groupVars),with=F])
  
  #calculate the hierarchical cluster structure from the correlation scores
  hc <- hclust(dist(t), "ward.D2")
  
  #take a look at your strucutre:
  plot(hc)
  rect.hclust(hc, k=50)

	
  rect.hclust(hc, h=1.5, border="blue")
  abline(1.5, 0)
  rect.hclust(hc, k=10, border="#99FF99")
  rect.hclust(hc, k=50, border="#FF9999")
  x <- identify(hc, n=13)
  rect.hclust(hc, k=55, border="#9999FF")

  
  #now we split the data based on membership structure. We will take four levels:
  #(basically this means we will calculate which group each variable belongs in for different levels of the tree strucutre)
  # Ben: so, essentially, we're going to look at plot(hc) and decide what the major branch points are,
  # then cut the tree to find group assignments above/below those splits. NB cutree() also allows us to 
  # split the tree at specific heights (on the y axis of that plot), if we don't want to count the groups.
  memb2 <- as.character(cutree(hc, k = 2))
  memb5 <- as.character(cutree(hc, k = 5))
  memb10 <- as.character(cutree(hc, k = 10))
  memb22 <- as.character(cutree(hc, k = 22))
  # memb55 <- as.character(cutree(hc, k = 55))
  
  # Ben: list these out so we can distinguish them from node and edge variables
  membVars <- c("memb2", "memb5", "memb10", "memb22"
   # , "memb55"
   )

  # get topic labels, which you've composed elsewhere using 'top docs per topic.R'
  filename <- paste0(imageloc, "topic labeling - ", dataset_name, ", K", ntopics, ".csv")
  topic.labels.dt <- tryCatch(
  		data.table(read.csv(filename), key="Topic"), 
  		error=function(e) {
  			message("File not found; using top words instead.")
  			keys <- get.topickeys(dataset_name, ntopics)
  			outfile <- paste0(webloc, "/", dataset_name, "k", ntopics, "_clusters_topwords.json")	
  			return(data.table(Topic=1:ntopics, Label=keys$top_words))
  		},
  		finally={message("done.")}
  )


  #Now put this information into a table, together with the labels and the order in which they should appear:
  b <- data.table(memb2,memb5,memb10,memb22
  # , memb55
  ,label=topic.labels.dt[, Label], topwords=topic.labels.dt[, Top.Words], order=hc$order)
  
  #We might want to know the size of each node. Let's add that
  # Ben: for a topic model, this will find the total %-point contribution of the topic to all docs;
  # that means we could divide by number of docs to scale to [0,1], but no need: it's proportional.
  b$size <- colSums(outputfile.dt[,c(dataVars),with=F])
  b$scaledsize <- b$size/nrow(outputfile.dt)
  
  # get co-occurring topics, for hierarchical edge bundling
  if(!exists("get.cotopics")) { source(paste0(sourceloc, "cotopics.R")) }
  cotopics <- get.cotopics(dataset_name, ntopics)
  edges <- cotopics[, .SD[, list("targets"=paste(target, collapse=","), "weights"=paste(weight, collapse=","))], by=source]
  setkey(edges, source)

  # # add a column of topic numbers to our node table, just to help merge with the edge table
  b$source <- 1:nrow(b)
  setkey(b, source)

  # # merge
  b <- merge(b, edges, all.x=T) 

  #sort the data so it aligns with the structure calculated using hclust()
  setkey(b,order)
  #drop the order variable:
  b[,order:=NULL]
  
  # drop the source variable
  b[,source:=NULL]

  # Save this data table to a csv for later inspection
  if(remake_figs) {
  	filename <- paste0(imageloc, "topic clusters - ", dataset_name, ", K", ntopics, ".csv")
  	write.csv(b, filename)
  } else {
  	print(b)
  }
  
  #we define a function which will create a nested list in JSON format:
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
      		 # , targets=x[,3][y],
      		 # weights=x[,4][y]
      	)})
    }
  }

  #This will not work on a data.table
  b <- data.frame(b)
  out <- makeList(b)
  str(out)
  toJSON(out)
  #Have a look at the structure this creates:
  print (head(out))
  
  #Basically we have made a list of lists containing the information from the tree diagram.
  #Finally we put everything into a list, convert this to json format and save it as data.json
  jsonOut<-toJSON(list(name="1of1",children=out))

  #We use the cat function here, because in some cases you may want to add separators, or a prefix and suffix to make the formatting just right
	# Ben adds: to avoid overwriting, only save this file if remake_figs is TRUE
	if(remake_figs) {  cat(jsonOut,file=outfile)	}
}

frameToJSON(dt,groupVars,dataVars,outfile="data.json")


