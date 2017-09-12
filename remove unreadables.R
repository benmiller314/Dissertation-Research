################################################################################
# remove unreadables.R
#
# A script to remove dissertations for which we have files, but the files aren't 
# sufficiently readable to scan -- either because they're not in English, and thus
# a topic model or keyword search shared with English texts wouldn't make sense, 
# or because the scan was bad enough that even the ocrmypdf tool couldn't get clean
# text out of it.
#
########

remove_unreadables <- function(dataset_name="noexcludes", 
                               more=NULL,                # manual list of Pub.Numbers to exclude 
                               level=0.25) {
    
    dataset = get(dataset_name)
    
    filename <- file.path(fulltextloc, paste0("all_", dataset_name, "_only"), 
                          "_spellstats", "spellstats.csv")
    if (file.exists(filename)) {
        spellstats <- read.csv(filename)
        spellstats$Ratio <- spellstats$ErrorCount / spellstats$WordCount
        unreadables <- c(which(spellstats$Ratio > level), which(is.na(spellstats$Ratio)))
        
        dataset[unreadables, "Exclude.Level"] <- 3
        
        if (!is.null(more)) {
            dataset[which(dataset$Pub.number %in% more), "Exclude.Level"] <- 3
        }
        
        message("Marked ", length(unreadables), " documents as unusable for full-text analyses,",
                " leaving ", length(dataset) - length(unreadables), ".")
        
    } else {
        warning("Could not find spelling statistics file; some full-text files may be unreadable.")
    }
    
    # TO DO: use system() to call the `ocr_again.sh` shell script on unreadables, 
    #        perhaps only if remake_figs == TRUE
    
    return(dataset)
}
