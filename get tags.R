## get tags.R
#  GOAL: Given a dataset and tagset, extract the sum of each tag within that dataset

get_tags <- function(dataset_name="noexcludes", tagset_name="tagnames", verbose=T) {
	dataset <- get(dataset_name)
	tagset <- get(tagset_name)
	
	if("data.table" %in% class(dataset)) {
	    a1 <- dataset[, ..tagset]
	} else {
	    a1 <- dataset[, tagset]
	}
	a2 <- apply(a1, 2, sum)
	
	if(verbose) {
    	message("Method tag frequency for ", dataset_name, ":")
	    print(a2)
	} 
	
	return(a2)
}
