# Get titles for a subset of dissertations, save to a file for ease of recall later
find_topic_titles <- function(dataset_name = "consorts", 
                              ntopics      = 55, 
                              subset_name  = NULL,
                              iter_index   = "") 
{
    # run the loop in parallel, because it's annoyingly slow and each run is independent
    require(doParallel)
    require(foreach)
    
    cl <- makeCluster(detectCores()-1)
    registerDoParallel(cl)
    
    titles_all <- foreach (i=1:ntopics, 
                           .verbose=T,
                           .export=c("dataset_name", "ntopics", "subset_name", "realconsorts", 
                                     "get.doc.composition", "get.doctopic.grid", "get.topickeys", 
                                     "get.topics4doc", "imageloc", "noexcludes.dt", "remake_figs", 
                                     "subset_name", "tagnames", "tmloc", "top_topic_browser", 
                                     "unixsourceloc"),
                           .combine="rbind"
                           ) %dopar% {
        titles <- factor(top_topic_browser(for.bind=T, subset_name=subset_name, iter_index=iter_index, topic=i)$Title)
        one_topic_titles <- paste(titles, collapse=" || ")
        cbind(topic=i, top_titles=one_topic_titles)
    }
    
    stopCluster(cl)
 
    if(remake_figs) {
        filename <- paste0("top_titles_per_topic-", dataset_name, "k", ntopics, subset_name, iter_index, ".csv")
        write.csv(titles_all, paste0(imageloc, filename))
    } else {
        return(titles_all)
    }
}

if(autorun) {
    remake_figs
    find_topic_titles(subset_name = "realconsorts")    
}
