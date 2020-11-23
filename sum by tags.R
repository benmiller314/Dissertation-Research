## If a dissertation is tagged X, how many times is it also tagged Y?
#  NB: diagonals in the resulting matrix are for solo tags, i.e. 
#  the number of times a dissertation tagged X is *only* tagged X.
#  The total number of times dissertations are tagged X is returned separately.

sumbytags <- function(dataset_name="noexcludes", tagset_name="tagnames") {
    dataset <- get(dataset_name)
    tagset <- get(tagset_name)
    
    # make a fresh start	
    sum.by.tags <- total.counts <- solo.counts <- c()
    
    for (i in 1:length(tagset)) {
        # select the tag
        tag <- tagset[i]
        
        # sum columns where the tag is 0 and where it's 1; 
        # this produces an array with two rows.
        tagsum <- aggregate(dataset[, tagset], list(dataset[, tag]), FUN=sum)
        
        # save the row in which the tag is "on", i.e. row 2. 
        # First column is the on/off status, so get rid of it.
        sum.by.tags <- rbind(sum.by.tags, tagsum[2, 2:ncol(tagsum)])
        
        # Name the row we've just added by the tag we're currently summarizing.
        row.names(sum.by.tags)[i] <- tag
        
        # Now the diagonals will dominate, so find the solo count for the tag
        solosum <- sum(dataset[which(dataset$Method.Count==1), tag])
        solo.counts <- c(solo.counts, solosum)
        names(solo.counts)[i] <- tag
        
        # ... and replace the diagonal with that solo count (but save the true count, i.e. the total)
        total.counts <- c(total.counts, sum.by.tags[i,i])
        names(total.counts)[i] <- tag
        sum.by.tags[i,i] <- solosum
        
    }
    # print(sum.by.tags)
    # print(total.counts)
    
    return (list("correlations" = as.matrix(sum.by.tags),
                 "solo.counts" = solo.counts,
                 "total.counts" = total.counts))
}
