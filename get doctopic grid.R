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
    
    
    ################
    ## OH GOOD LORD -- encountering mysterious bugs on 2018-01-02, I realize that MALLET's output has changed:
    ## instead of spitting out alternating ranked columns of topic number, topic weight, topic number, topic weight 
    ## as my original reshaping scheme below assumed), at some point it started giving only topic weights, with all topics 
    ## in order. This change was not documented anywhere, as far as I can tell, except for this issue queue -- from way back
    ## in 2015 -- https://github.com/mimno/Mallet/issues/41. GRRRRRAGHH.
    ##
    ## The upshot is, we no longer need mallet_composition2r.py, because the composition file is already in a grid. 
    ################
    
    #
    # Locate the doc/topic grid, or create it if it doesn't yet exist.
    # filename <- file.path(tmloc, paste0(dataset_name, "k", ntopics, 
    #                     "_doc-all-topics_", iter_index, ".txt"))
    # 
    # if (!file.exists(filename)) {   
    #     if (do_reshape) { cmd <- "reshapeMallet.py" } else { cmd <- "mallet_composition2r.py" }
    #     command <- paste0("cd '", unixsourceloc, 
    #             "'; cd 'Shell scripts and commands'; python", cmd)
    #     
    #     go <- readline(paste("Have you updated", cmd,
    #                     "to reflect your current dataset/ntopics? (Y/N)\n"))
    #     if(tolower(go) != "y") { 
    #         stop("Better fix that, then") 
    #     } 
    #     
    #     print("Converting topic/weight pairs into doc/topic grid...")
    #     if(! system(command)) {     # runs only if command exits successfully
    #         print("Done.") 
    #     }
    #     
    # } else {    # file already exists
    #     print("Oh, good, the doc-all-topics file exists. Moving on...")
    # }
    
    # Read in the doc/topic grid. 
    # for colClasses:   Column 1 is an unneeded row index, so convert to NULL.
    #                   Column 2 is filenames; we'll clean below.
    #                   The remaining columns are the topics.
    filename <- file.path(tmloc, paste0(dataset_name, "k", ntopics, "_composition_", iter_index, ".txt"))
    if(file.exists(filename)) {
        doc_topics <- read.delim(filename, header=F, colClasses=c("NULL", "character", rep("numeric", ntopics)))        
    } else {
        stop("doc/topic grid does not exist: ", filename)
    }
    
    
    # The first row may contain a .DS_Store row, which we can toss out. 
    if (grepl("DS_Store", doc_topics[, "V2"][1])) {
        doc_topics <- doc_topics[-1,]
    }
    
    
    # Column V2 contains filenames ending with "cleaned_" plus a (usually 7-digit) Pub.number, 
    # followed by ".txt". Let's simplify. 
    get_pub <- function(cell) {
        last_underscore <- tail(gregexpr("_", cell)[[1]], 1)    
        val <- substr(cell, last_underscore + 1, nchar(cell)-4)
        return(val)
    }
    doc_topics[, "V2"] <- sapply(doc_topics[, "V2"], get_pub)
    head(doc_topics[, "V2"])
    
    
    # first remaining column is the text id; use Pub.number to match main data structure.
    # remaining columns are topics in order. To avoid searching for column "0" later,
    # switch from 0-indexed to 1-indexed, so the topic numbers in topic_keys.dt are the same as row numbers

    colnames(doc_topics) <- c("Pub.number", (1:ntopics))
    head(doc_topics,3)
     
    
    # If we specified a subset, filter the data tables before moving on
    if (! is.null(subset_name)) {
        subset_index <- as.integer(get(subset_name)$Pub.number)
        doc_topics <- doc_topics[subset_index,]
    }
    
    
    ## Find overall top topics
    # contributes to the dissertation in that row. Summing these percentages
    # Each cell gives the percentage which the topic in that column
    # and sorting gives us a rank based on percentage points.
    
    colsums <- colSums(doc_topics[,which(names(doc_topics) != "Pub.number")])
    str(colsums)
    colsums <- c(999999, colsums)
    names(colsums) <- names(doc_topics)
    head(colsums)
    colsums.sort <- colsums[order(colsums, decreasing=TRUE)]
    head(colsums.sort)
    
    # Divide the percentage point totals by the number of dissertations
    # to get an overall percent contribution
    # colsums.pct <- round((colsums / nrow(outputfile)), 4) * 100
    colsums.sort.pct <- round((colsums.sort / nrow(doc_topics)), 4) * 100
    head(colsums.sort.pct)
    
    # Optionally save to file
    if(remake_figs) { 
        if(! is.null(subset_name)) {
            filename <- file.path(imageloc, paste0(dataset_name, "k", ntopics, "--", subset_name, 
                               "_topic-ranks", iter_index, ".csv"))    
        } else {
            filename <- file.path(imageloc, paste0(dataset_name, "k", ntopics,
                             "_topic-ranks", iter_index, ".csv"))
        }
        out <- colsums.sort.pct[2:length(colsums.sort.pct)]
        head(out)
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
    
    doc_topics.dt <- as.data.table(doc_topics)
    setkey(doc_topics.dt, Pub.number)
    
    
    # Return with the goods
    list("colsums" = colsums,
         "colsums.sort" = colsums.sort,
         "colsums.sort.pct" = colsums.sort.pct,
         "outputfile" = doc_topics,
         "outputfile.dt" = doc_topics.dt
         )

}   # end of get.doctopic.grid()

if(FALSE) {         # this will never run on its own
    mygrid <- get.doctopic.grid("noexcludes2001_2015", 60, NULL, 4, doplot=F)
}
