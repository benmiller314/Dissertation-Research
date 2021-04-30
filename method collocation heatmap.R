
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
		normed = FALSE,	 # should we divide by total dissertations per row?
  		dendro = FALSE,  # should we output dendrograms showing method clusters?
		savecsv = FALSE, # or replace with remake_figs when called
		rowInd = NULL,   # use for replicating row order across samples
		colInd = NULL,   # use for replicating column order across samples
		numCols = 100    # how many shades in the plot? passed to heatmap.ben.
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

    if(savecsv) {
        # NB: all remake_figs file output for *figures* is now in heatmap.ben(), 
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
		            mytitle = paste0(slug, ", ", dataset_name, " N", nrow(dataset)),
		            rowInd = rowInd,
		            colInd = colInd,
		            numCols = numCols
		)
	
		to.return <- c(to.return, mapvals)
    } # end of if(doplot)

    return (to.return)

} # end of wrapper function sumbytags()


method_corrs_one_row <- function(myrow,
                                 corr_type = c("method", "school"),
                                 dataset_name = "knownprograms2001_2015",
                                 tagset_name = "no_ped_tagnames",
                                 color_groups = T,
                                 taggroups = no_ped_taggroups,
                                 normed = F,
                                 corr_obj = NULL,  # the object with correlation data.
                                                   # pass for speed boost if it exists.
                                 colInd = NULL,    # column order, if you have it
                                 ...               # other graphing parameters
){
    corr_type <- match.arg(corr_type)
  
    require(data.table)
      
    # Use the method-method tag clustering order for columns regardless, 
    # because it's based on co-occurrence within actual dissertations
    
    if(is.null(colInd)) {
        myorder <- sumbytags(dataset_name = dataset_name,
                         tagset_name = tagset_name, 
                         doplot = T, 
                         normed = T, 
                         dendro = T,
                         savecsv = F)$colInd
    } else {
        myorder <- colInd
    }
    
    if(is.null(corr_obj)) {
        if(corr_type == "method") {
            corr_obj <- sumbytags(dataset_name = dataset_name,
                              tagset_name = tagset_name, 
                              doplot = F, 
                              normed = normed, 
                              dendro = F,
                              savecsv = F)
            
        } else if (corr_type == "school") {
            if(is.null(corr_obj)) {
                if(! exists("schoolwise.data", mode="function")) {
                    source(file = "method tags by school.R")
                }
                corr_obj <- schoolwise.data(dataset_name = dataset_name,
                                        tagset_name = tagset_name)
            }
            
        } else {
            stop("method_corrs_one_row(): Only 'method' and 'school' correlations are implemented.")
        }
    }
    
    # get group labels in the right order, assign them a color palette
    if(color_groups) {
        
        tagset <- get(tagset_name)
        mygroups <- taggroups[names(taggroups) %in% tagset][myorder]
        
        require(viridisLite)
        group_pal <- viridis(4+length(unique(mygroups)))
        if(length(group_pal) > 8) {
            group_pal <- c(group_pal[c(3, 6, 8, 9)], "#FFFFFF")
        } 
        
        if(!exists("get_tags", mode="function")) { source(file="get tags.R") }
        mysort <- order(get_tags(dataset_name=dataset_name, tagset_name=tagset_name, verbose=F), 
                        decreasing = T)
        names(group_pal) <- unique(taggroups[mysort])
        
        group_pal
    } else {
        group_pal <- NULL
    }
    
    if(!exists("build_plot_title", mode="function")) {
        source(file = "build_plot_title.R")
    }
    maintitle <- build_plot_title(dataset_name = dataset_name,
                                  subset_name = tagset_name,
                                  ntopics = NULL,
                                  iter_index = NULL,
                                  whatitis = paste0("method-", corr_type, 
                                                    " correlation ",
                                                    if(normed) "pcts" else "counts",
                                                    " for ", myrow),
                                  for.filename = remake_figs)
    
    
    if(corr_type == "method") {
        myrow.disscount <- corr_obj$total.counts[myrow]
        
        to_plot <- corr_obj$correlations[myrow, myorder]
        if(any(is.na(to_plot))) { stop("row '", myrow ,"' not found")}
        if(normed) {
            to_plot <- to_plot / myrow.disscount
        }
        
        
        
    } else if (corr_type == "school") {
        myrow.disscount <- corr_obj$totals[myrow]$N
        
        if(normed) {
            corr_obj <- corr_obj$normed
        } else {
            corr_obj <- corr_obj$counts
        }
        
        
        
        to_plot <- corr_obj[myrow, .SD, .SDcols=!c("School")]
            if(any(is.na(to_plot))) { stop("row '", myrow ,"' not found")}
        to_plot <- unlist(to_plot[, ..myorder])
        
    }
    
    if(remake_figs) {
        filename <- file.path(imageloc, paste0(maintitle, ".pdf"))
        pdf(filename)
    } 
    
        maintitle <- sub(",", "\n", maintitle)
        maintitle <- sub("--", "\n", maintitle)
        # maintitle <- gsub("_", " ", maintitle)
    
        myplot <- barplot(to_plot,
                          col = if(color_groups) group_pal[mygroups[names(to_plot)]] else NULL,
                          main = maintitle,
                          las = 2)
        
        # outside_
        legend(x = "top", 
                       legend = c(names(group_pal), paste("N =", myrow.disscount)), 
                       fill = c(group_pal, "white"),
                       border = c(rep("black", length(group_pal)), "white")
        )
        
        
    if(remake_figs) {
        dev.off()
    }
    
        
    invisible(myplot)
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
	dataset_name <- "nonrcws2001_2015sans_badtops"
	tagset_name <- "no_ped_tagnames"
	tagset <- get(tagset_name)
	taggroups <- no_ped_taggroups
	
	
    method_corrs <- sumbytags(dataset_name, tagset_name, 
              doplot=T, 
              normed=T, 
              dendro=T)
    
    remake_figs = T
    tagset <- c("Phil", "Ethn", "Disc", "Meta", "Modl")
    for(tag in tagset) {
        method_corrs_one_row(myrow = tag,
        # method_corrs_one_row(myrow = "Modl",
                             dataset_name = dataset_name,
                             tagset_name = tagset_name,
                             taggroups = taggroups,
                             # normed = F, # maybe normed axis, use text() to add count values?
                             normed = T,
                             color_groups = T,
                             colInd = method_corrs$colInd)    
    }
    
    remake_figs=F
    
    # test colors: viridis to distinguish methodological groups, magma for values
    require(viridisLite)
    mypal <- magma(19, direction = -1)
    mypal <- c("#FFFFFF", mypal)
    
    # Compare RCWS to nonRCWS
    method_corrs_rcws <- sumbytags("knownprograms2001_2015", "no_ped_tagnames", 
                                   doplot=T, 
                                   normed=T, 
                                   dendro=T)
    method_corrs_nonrcws <- sumbytags("nonrcws2001_2015sans_badtops", "no_ped_tagnames", 
                                      doplot=T, 
                                      normed=T, 
                                      dendro=T,
                                      rowInd = method_corrs_rcws$rowInd,
                                      colInd = method_corrs_rcws$colInd)
    
    
    # Find the differences between these two correlation matrices
    method_corr_diffs <- method_corrs_rcws$plottedData - method_corrs_nonrcws$plottedData
    
    # red-blue diverging palette from https://gka.github.io/palettes/#/25|d|b41f19|255590|1|1
    # (but I replaced the 0 value from )
    diverge_rb <- c('#4f69a5', '#6279b6', '#738ac6', '#859bd6', 
                    '#97ace6', '#a9bef7', '#c0d0fd', '#dbe3f9', 
                                    '#f5f5f5', 
                    '#f8ddd6', '#fbc5b5', '#feab93', '#f5957c', 
                    '#e78169', '#da6d57', '#cc5944', '#ba4635'
                    )
    require(gplots)
    
    maintitle <- "Method correlation heatmap (scaled by row): differences between RCWS and nonRCWS"
    subtitle <- "Values indicate percentage point increase in RCWS relative to nonRCWS"
    
    if(remake_figs) {
        outfile <- "Method Tag Co-Occurrence Diffs (normed by row) -- knownprograms2001_2015 relative to nonrcws2001_2015sans_badtops"
        outfile <- file.path(imageloc, paste0(outfile, ".pdf"))
        pdf(outfile)
    }
    
    heatmap.2(method_corr_diffs, 
              scale="none", 
              col=diverge_rb, 
              trace="none", 
              Colv=NULL, 
              Rowv=NULL, 
              dendrogram="none", 
              keysize = 1,
              density.info="none", 
              cellnote=100*round(method_corr_diffs, 2),
              notecex=0.7,
              notecol="black",
              main = maintitle,
              sub = subtitle)
    dev.off()
    
    # Compare clustering of method co-occurrence in and out of RCWS programs:
    knownprog_dend <- method_corrs_rcws$Colv
    nonrcws_dend <- method_corrs_nonrcws$Colv
    
    par(mfrow=c(2, 1))
        plot(knownprog_dend, main="confirmed rcws (N=1,684)")
        plot(rev(nonrcws_dend), main="confirmed non-rcws (N=733)")
        title(main="Clustering of method co-occurrence", outer=T)
        mtext("Normed by method totals", side=1, outer=T)
        
        
    
    
    
    # sum.by.tags <- sumbytags() 
	sumbytags("consorts.plus")
	sumbytags("consorts.plus", normed=T)
	sumbytags("top.nonconsorts")
	sumbytags("consorts", dendro=T, normed=T)
	
	
}
	
