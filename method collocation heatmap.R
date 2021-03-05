
### 
# `method collocation heatmap.R`: Given method tags, collocate them and 
# construct a heat plot. That is, if a dissertation is tagged X, 
# how many times is it also tagged Y?

#
#  NB: diagonals in the resulting matrix are for solo tags, i.e. 
#  the number of times a dissertation tagged X is *only* tagged X.
#  The total number of times dissertations are tagged X is returned separately.

#  1. Calculate tag collocations, total dissertations per tag, 
#     and solo counts per tag.


sumbytags <- function(dataset_name = "noexcludes",
		tagset_name	= "tagnames",
		doplot = TRUE,
		normed = FALSE,	# should we divide by total dissertations per row?
  		dendro = FALSE	# should we output dendrograms showing method clusters?
){
	

	# get values from variable name; we'll use names later 
	# for filenames and figure titles
	
	dataset <- get(dataset_name)				
	tagset <- get(tagset_name)
	
	# Start with three empty containers, then build them up
	sum.by.tags <- total.counts <- solo.counts <- c()

	for (i in 1:length(tagset)) {
		# select the tag
		tag <- tagset[i]
		
		# debug
			# if(tag %in% c("Poet", "Prac")) { print(sum.by.tags) }
		
		# sum columns where the tag is 0 and where it's 1; 
		# this produces an array with two rows.
		
		tagsum <- aggregate(dataset[, tagset], list(dataset[, tag]), FUN=sum)
			# debug
			# if(tag %in% c("Poet", "Prac")) { print(sum.by.tags) }
		
		# Save the row in which the tag is "on" (i.e. set to 1). 
		# If no such row exists, fill with zeroes to avoid NA results.
		# First column is the on/off status, so leave it out.
		
		if (nrow(tagsum) == 1 && tagsum[, 1] == 0) { 
			sum.by.tags <- rbind(sum.by.tags, rep(0, ncol(tagsum)-1)) 
		} else { 
			sum.by.tags <- rbind(sum.by.tags, tagsum[which(tagsum[,1] == 1), 	
								 2:ncol(tagsum)]) 
		}
		
		# Name the row we've just added by the tag we're currently summarizing.
		row.names(sum.by.tags)[i] <- tag
		
		# Now the diagonals will dominate, so find the solo count for the tag
		solosum <- sum(dataset[which(dataset$Method.Count==1), tag])
		solo.counts <- c(solo.counts, solosum)
		
		names(solo.counts)[i] <- tag
		
		# ... and replace the diagonal with that solo count 
		# (but save the true count, i.e. the total)
		
		total.counts <- c(total.counts, sum.by.tags[i,i])
		
		names(total.counts)[i] <- tag
		
		sum.by.tags[i,i] <- solosum
	
	} # end for loop
	
	

	# print(sum.by.tags)
	
	# print(total.counts)


	to.return <- list("dataset_name" = dataset_name,
				 "tagset_name" = tagset_name,
				 "correlations" = as.matrix(sum.by.tags),
				 "solo.counts"  = solo.counts,
				 "total.counts" = total.counts)

    if(remake_figs) {
        # NB: all remake_figs file output for figures is now in heatmap.ben(), 
        # which will be called if doplot = T. So here we only need to save csv's
        # for analysis elsewhere, e.g. network maps in Gephi.
        
        dataset_slug <- paste0("--", dataset_name, "-", tagset_name, "--N", nrow(dataset))
        
        filename <- paste0("method-correlatons", dataset_slug, ".csv")
        safesave(write.csv, to.return$correlations, file.path(imageloc, filename))
        
        filename <- paste0("method-solo-counts", dataset_slug, ".csv")
        safesave(write.csv, to.return$solo.counts, file.path(imageloc, filename))
        
        filename <- paste0("method-total-counts", dataset_slug, ".csv")
        safesave(write.csv, to.return$total.counts, file.path(imageloc, filename))
        
    }
	
	if(doplot) {
        
		if(!exists("heatmap.ben", mode="function")) {
			source(file="heatmap_ben.R")
		}
	
	    # prep title based on whether or not we're norming
	    slug <- "Method Tag Co-Occurrence"
	    if(normed) {
	        slug <- paste(slug, "(normed by row)")
	    }
	    
	    # actually build the map
		mapvals <- heatmap.ben(dataset_name = dataset_name,
		            tagset_name = tagset_name,
		            sum.by.tags = to.return, 
		            rowscale = normed,
		            diags = TRUE, 
		            dendro = dendro,
		            mytitle = paste0(slug, ", ", dataset_name, " N", nrow(dataset)))
	
		to.return <- c(to.return, mapvals)
    } # end of if(doplot)

    return (to.return)

} # end of wrapper function sumbytags()


one_method_corrs_barplot <- function(onetag,
                                     dataset_name = "knownprograms2001_2015",
                                     tagset_name = "no_ped_tagnames",
                                     taggroups = no_ped_taggroups,
                                     normed = F,
                                     method_corrs = NULL # pass for speed boost if it exists
){
    if(is.null(method_corrs$rowInd)) {
        method_corrs <- sumbytags(dataset_name, tagset_name, 
                                  doplot=T, 
                                  normed=normed, 
                                  dendro=T)
    }
    
    myorder <- method_corrs$colInd
    
    tagset <- get(tagset_name)
    mygroups <- taggroups[names(taggroups) %in% tagset][myorder]
    
    # get group labels in the right order, assign them a color palette
    require(viridisLite)
    group_pal <- viridis(4+length(unique(mygroups)))
    if(length(group_pal) > 8) {
        group_pal <- c(group_pal[c(3, 6, 8, 9)], "#FFFFFF")
    } 
    
    if(!exists("get_tags", mode="function")) { source(file="get tags.R") }
    mysort <- order(get_tags(dataset_name=dataset_name, tagset_name=tagset_name), 
                    decreasing = T)
    names(group_pal) <- unique(taggroups[mysort])
    
    group_pal
    
    group_pal[mygroups[names(method_corrs$correlations[onetag, myorder])]]
    
    if(!exists("build_plot_title", mode="function")) {
        source(file = "build_plot_title.R")
    }
    maintitle <- build_plot_title(dataset_name = dataset_name,
                                  subset_name = tagset_name,
                                  ntopics = NULL,
                                  iter_index = NULL,
                                  whatitis = paste("Method correlations for", onetag),
                                  for.filename = remake_figs)
    maintitle <- sub(",", "\n", maintitle)
    
    barplot(method_corrs$correlations[onetag, myorder],
            col = group_pal[mygroups[names(method_corrs$correlations[onetag, myorder])]],
            main = maintitle)
    
}



if (!autorun) {
    message("The following functions have been loaded: \n",
            " sumbytags(dataset_name, tagset_name, doplot=T, normed=F, dendro=F)\n",
            " one_method_corrs_barplot(onetag, dataset_name, tagset_name, normed=F, method_corrs=NULL)")
}

# Testing area
if (FALSE) { 
	remake_figs=F
	
	dataset_name <- "knownprograms2001_2015"
	tagset_name <- "no_ped_tagnames"
	tagset <- get(tagset_name)
	taggroups <- no_ped_taggroups
	
	
    method_corrs <- sumbytags(dataset_name, tagset_name, 
              doplot=T, 
              normed=T, 
              dendro=T)
    
    for(tag in tagset) {
        one_method_corrs_barplot(onetag = tag,
                                 dataset_name = dataset_name,
                                 tagset_name = tagset_name,
                                 taggroups = taggroups,
                                 method_corrs = method_corrs)    
    }
    
    
    # test colors: viridis do distinguish methodological groups, magma for values
    require(viridisLite)
    mypal <- magma(19, direction = -1)
    mypal <- c("#FFFFFF", mypal)
    
    sumbytags("knownprograms2001_2015", "tagnames", 
              doplot=T, 
              normed=F, 
              dendro=T)
    
    
    # sum.by.tags <- sumbytags() 
	sumbytags("consorts.plus")
	sumbytags("consorts.plus", normed=T)
	sumbytags("top.nonconsorts")
	sumbytags("consorts", dendro=T, normed=T)
}
	