#############################################################################
# get doctopic grid.R
# 
# GOAL: Read in a table of documents with all topic proportions for each.
# Return both this table and total topic proportions across documents.
#
# STRATEGY:
# Edit the reshapeMallet.py script (in TextWrangler) to update filenames,
# then run it here and read in the output.
#####

get.doctopic.grid <- function(dataset_name="consorts", 
                              ntopics=55, 
                              subset_name=NULL,
                              iter_index="",
                              doplot=F,
                              do_reshape=F
                              ) {
    # get packages in case we've just restarted R
    require(data.table)
    
    # Locate the doc/topic grid, or create it if it doesn't yet exist.
    filename <- file.path(tmloc, paste0(dataset_name, "k", ntopics, 
                        "_doc-all-topics", iter_index, ".txt"))
    
    if (!file.exists(filename)) {   
        if (do_reshape) { cmd <- "reshapeMallet.py" } else { cmd <- "mallet_composition2r.py" }
        command <- paste("cd", unixsourceloc, 
                "; cd 'Shell scripts and commands'; python", cmd)
        
        go <- readline(paste("Have you updated", cmd,
                        "to reflect your current dataset/ntopics? (Y/N)\n"))
        if(tolower(go) != "y") { 
            stop("Better fix that, then") 
        } 
        
        print("Converting topic/weight pairs into doc/topic grid...")
        if(! system(command)) {     # runs only if command exits successfully
            print("Done.") 
        }
        
    } else {    # file already exists
        print("Oh, good, the doc-all-topics file exists. Moving on...")
    }
    
    # Read in the doc/topic grid
    outputfile <- read.delim(filename, header=F)
    
    # switch from 0-indexed to 1-indexed, so the topic numbers in
    # topic_keys.dt are the same as row numbers
    # NB: this seems to be necessary to avoid searching for column "0"
    head(outputfile)
    names(outputfile) <- c("Pub.number", (1:(ncol(outputfile)-1)))
    head(outputfile)
    
    # If we specified a subset, filter the data tables before moving on
    if (! is.null(subset_name)) {
        subset_index <- as.integer(get(subset_name)$Pub.number)
        outputfile <- outputfile[subset_index,]
    }
    
    
    ## Find overall top topics
    # Each cell gives the percentage which the topic in that column
    # contributes to the dissertation in that row. Summing these percentages
    # and sorting gives us a rank based on percentage points.
    
    colsums <- colSums(outputfile)
    names(colsums) <- names(outputfile)
    head(colsums)
    colsums.sort <- colsums[order(colsums, decreasing=TRUE)]
    head(colsums.sort)
    
    # Divide the percentage point totals by the number of dissertations
    # to get an overall percent contribution
    # colsums.pct <- round((colsums / nrow(outputfile)), 4) * 100
    colsums.sort.pct <- round((colsums.sort / nrow(outputfile)), 4) * 100
    head(colsums.sort.pct)
    
    # Optionally save to file
    if(remake_figs) { 
        if(! is.null(subset_name)) {
            filename <- paste0(imageloc, dataset_name, "k", ntopics, "--", subset_name, 
                               "_topic-ranks", iter_index, ".csv")    
        } else {
            filename <- paste0(imageloc, dataset_name, "k", ntopics,
                             "_topic-ranks", iter_index, ".csv")
        }
        out <- colsums.sort.pct[2:length(colsums.sort.pct)]
        
        write.csv(out, filename)
    }
    
    # Optionally get an overview of the topic sizes, as a scatterplot
    if(doplot) {
        plot(x = 2:length(colsums), 
             y = colsums.sort[2:length(colsums)], 
             xlab = "topic numbers (arbitrary)", 
             ylab = "sum of contributions", 
             xaxt = "n"
        )
            
        # barplot(colsums.sort[2:length(colsums)], 
        #         xlab="topic numbers (arbitrary)", 
        #         ylab="sum of contributions", 
        #         xaxt="n", 
        #         xpd=F
        # )
        
        text(x = 1+2:length(colsums), 
             y = 1+colsums.sort[2:length(colsums)],
             labels = names(colsums.sort[2:length(colsums)])
        )
        
    }   # end of if(doplot)
    
    # Return with the goods
    list("colsums" = colsums,
         "colsums.sort" = colsums.sort,
         "colsums.sort.pct" = colsums.sort.pct,
         "outputfile" = outputfile
         )

}   # end of get.doctopic.grid()
