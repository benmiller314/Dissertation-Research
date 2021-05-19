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

get.doctopic.grid <- function(dataset_name = "noexcludes2001_2015", 
                              ntopics = 50, 
                              subset_name = NULL,
                              iter_index = "1",
                              doplot = F,
                              do_reshape = F,
                              newnames = F       # newnames assumes iter_index is built into the model name;
                                                 # if the iter_index appears at the very end of a filename, use newnames=F
                              ) {

    require(data.table)

    # Read in the doc/topic grid. 
    # for colClasses:   Column 1 is an unneeded row index, so convert to NULL.
    #                   Column 2 is filenames; we'll clean below.
    #                   The remaining columns are the topics.
    
    if(iter_index != "") {
        if(newnames) {
            filename <- file.path(tmloc, paste0(dataset_name, "k", ntopics, "_iter", iter_index, "_composition.txt"))
        } else {
            filename <- file.path(tmloc, paste0(dataset_name, "k", ntopics, "_composition_", iter_index, ".txt"))
        }
    } else {
        filename <- file.path(tmloc, paste0(dataset_name, "k", ntopics, "_composition.txt"))
    }
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
     
    # Enable analysis on a subset of this grid, e.g. for one school.
    # Strategy: 
    # (1) add a parameter that's a list of Pub.numbers to include
    # (2) subset the rows of the doc-topic grid by using that list
    # (3) side benefit: we now have a list of docs to pass along to top_topic_browser, etc
    
    # If we specified a subset, filter the data tables before moving on
    if (!is.null(subset_name)) {
        if (length(subset_name) > 1) {          # it's the data itself
            subset <- subset_name
        } else if (length(subset_name) == 1) {  # it's just the label; get the data
            subset <- get(subset_name)
        } else {
            warning("get.doctopic.grid: subset_name was provided, but it has length < 1. Not subsetting.")
            subset <- NULL
        }
    
        if (is.null(subset)) {
            subset_index <- doc_topics$Pub.number
        } else if (is.atomic(subset) || is.vector(subset)) {         # it's a list of Pub.numbers
            subset_index <- subset
        } else {
            subset_index <- subset$Pub.number   # it's a full dataset
        }
        
        doc_topics <- doc_topics[(doc_topics$Pub.number %in% subset_index),]
    }
    
    
    
    ## Find overall top topics
    # Each cell gives the percentage which the topic in that column
    # contributes to the dissertation in that row. Summing these percentages
    # and sorting gives us a rank based on percentage points.
    # Note the workaround so we can keep Pub.numbers, for indexing:
    
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
        # what to export?
        out <- colsums.sort.pct[2:length(colsums.sort.pct)]
        head(out)
        
        # where to export it?
        if(!exists("build_plot_title", mode="function")) {
            source(file="build_plot_title.R")
        }
        outfile_slug <- build_plot_title(dataset_name=dataset_name,
                                         ntopics=ntopics,
                                         iter_index=iter_index,
                                         subset_name=subset_name,
                                         bad.topics=bad.topics,
                                         whatitis="topic-pct-contrib",
                                         for.filename=T)
        outfile <- file.path(imageloc, paste0(outfile_slug, ".csv"))
        
        # see .Rprofile
        safesave(write.csv, out, outfile, col.names = c("Topic", "Pct of Corpus"))
        
        # if(file.exists(outfile)) {
        #     overwrite <- readline(paste("In 'get.doctopic.grid()': destination file already exists: \n", outfile, "\n",
        #                                 "Overwrite (O)? New file (N)? Skip (S)? (o/n/s) > "))
        #     if(tolower(overwrite) == "n") {
        #         outfile <- readline(paste("Enter new filename, including directory. ",
        #                                   "(Default directory is ", imageloc, ")> "))
        #         write.csv(out, outfile)
        #     } else if (tolower(overwrite) == "s") {
        #         # just keep going. 
        #         # annoying to repeat the write.csv in three conditions, but no other way
        #         # to handle the "else" condition, where we do write.csv, and still skip here.
        #     } else if (tolower(overwrite) == "o") {
        #         write.csv(out, outfile)
        #     } else {
        #         outfile_slug <- paste0(outfile_slug, "+1")
        #         outfile <- file.path(imageloc, paste0(outfile_slug, ".csv"))
        #         warning(paste("Answer not understood. Saving with new filename: \n", outfile))
        #         write.csv(out, outfile)
        #     }
        # }
        
        
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
    remake_figs=T
    mygrid <- get.doctopic.grid("noexcludes2001_2015", ntopics=23, subset_name=NULL, 3, doplot=T)
    
    mygrid <- get.doctopic.grid("noexcludes2001_2015", ntopics=50, iter_index=1, 
                                subset_name="knownprograms2001_2015", 
                                doplot=T)
    
    mygrid <- get.doctopic.grid("noexcludes2001_2015", ntopics=50, iter_index=1, 
                                doplot=T)
    
    doctopics <- get.doctopic.grid("noexcludes2001_2015", ntopics=50, iter_index=1, 
                                subset_name="nonrcws2001_2015", 
                                doplot=T)
    remake_figs=F
}
