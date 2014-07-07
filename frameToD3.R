# Forked from Rolf Fredheim at https://github.com/benmiller314/frameToD3/blob/master/frameToD3.r
# as discussed in http://quantifyingmemory.blogspot.com/2013/11/d3-without-javascript.html
# All references to "my" below are his; I'll add mine with "Ben:" as needed.

#My sample data is 1000 rows, with the first column a unique id, followed by 100 columns of data variables
# Ben: and it's called 'dt'; in this case the ID = a filename, and the data = topic weights
load("sampleData")
head(dt)

groupVars <- c("path")	# Ben: this is the name of that first (ID) column. replace accordingly.
dataVars <-  colnames(dt)[!colnames(dt) %in% groupVars]	# Ben: any column that's not an ID is a datapoint 
outfile		# Ben: don't know what this is; presumably we set this to be the name of the JSON output below, 
			# but it's not set in the code RF posted.
frameToJSON <- function(dt, groupVars, dataVars, outfile) {
  #packages we will need:
  require(data.table)	# Ben: okay, this package is awesome, glad to know about it now!
  require(jsonlite)		# Ben: but should we now use jsonlite, as per http://bit.ly/1jXAC5M ?
  
  #Here you may want to sort by colSums() to keep only the most relevant variables.
  
  #calculate the correlation matrix
  t <- cor(dt[,c(!colnames(dt) %in% groupVars),with=F])
  
  #calculate the hierarchical cluster structure from the correlation scores
  hc <- hclust(dist(t), "ward")
  
  #take a look at your strucutre:
  plot(hc)
  
  #now we split the data based on membership structure. We will take four levels:
  #(basically this means we will calculate which group each variable belongs in for different levels of the tree strucutre)
  # Ben: so, essentially, we're going to look at plot(hc) and decide what the major branch points are,
  # then cut the tree to find group assignments above/below those splits. NB cutree() also allows us to 
  # split the tree at specific heights (on the y axis of that plot), if we don't want to count the groups.
  memb2 <- as.character(cutree(hc, k = 2))
  memb6 <- as.character(cutree(hc, k = 6))
  memb15 <- as.character(cutree(hc, k = 15))
  memb40 <- as.character(cutree(hc, k = 40))
  
  #Now put this information into a table, together with the labels and the order in which they should appear:
  b=data.table(memb2,memb6,memb15,memb40,label=hc$labels,order=hc$order)
  
  #We might want to know the size of each node. Let's add that
  # Ben: for a topic model, this will find the total %-point contribution of the topic to all docs;
  # that means we could divide by number of docs to scale to [0,1], but no need: it's proportional.
  b$size <- colSums(dt[,c(dataVars),with=F])
  
  #sort the data so it aligns with the structure calculated using hclust()
  setkey(b,order)
  #drop the order variable:
  b[,order:=NULL]
 
  #we define a function which will create a nested list in JSON format:
  #From here: http://stackoverflow.com/questions/12818864/how-to-write-to-json-with-children-from-r
  # Ben: but see also, now, http://bit.ly/1jXAC5M
  makeList<-function(x){
    if(ncol(x)>2){
      listSplit<-split(x[-1],x[1],drop=T)
      lapply(names(listSplit),function(y){list(name=y,imports=makeList(listSplit[[y]]))})
    }else{
      lapply(seq(nrow(x[1])),function(y){list(name=x[,1][y],size=x[,2][y])})
    }
  }
  
  #This will not work on a data.table

  b2 <- data.frame(b)
  out <- makeList(b2)
  #Have a look at the structure this creates:
  print (head(out))
  
  #Basically we have made a list of lists containing the information from the tree diagram.
  #Finally we put everything into a list, convert this to json format and save it as data.json
  jsonOut<-toJSON(list(name="Centre",children=makeList(b2)))
  head(jsonOut)
  #We use the cat function here, because in some cases you may want to add separators, or a prefix and suffix to make the formatting just right
  cat(jsonOut,file="outfile.json")
}

frameToJSON(dt,groupVars,dataVars,outfile="data.json")