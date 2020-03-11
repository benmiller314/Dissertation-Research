# Get titles for a subset of dissertations, save to a file for ease of recall later
find_topic_titles <- function(dataset_name = "noexcludes2001_2015",
                              ntopics      = 50,
                              iter_index   = 1,
                              subset_name  = "knownprograms2001_2015",    # set NULL if not using
                              bad.topics   = c("3", "8", "12", "15", "30", "34", "36", "47", "50")
                              )
{
    # run the loop in parallel, because it's annoyingly slow and each run is independent
    require(doParallel)
    require(foreach)

    cl <- makeCluster(detectCores()-1)
    registerDoParallel(cl)

    if(!exists("get.topics4doc")) {
        source("top docs per topic.R")
    }

    topiclist <- 1:ntopics
    topiclist <- setdiff(topiclist, bad.topics)
    
    titles_all <- foreach (i=topiclist,
                           .verbose=T,
                           .export=c(dataset_name, subset_name, "realconsorts2001_2015", "noexcludes2001_2015", "knownprograms2001_2015", # update to include current likely data
                                     "get.doctopic.grid", "get.topickeys",
                                     "get.topics4doc", "imageloc", "noexcludes.dt", "remake_figs","tagnames", "tmloc", "top_topic_browser",
                                     "unixsourceloc"),
                           .combine="rbind"
                           ) %dopar% {
        titles <- factor(top_topic_browser(for.bind=T, dataset_name=dataset_name, ntopics=ntopics, subset_name=subset_name, iter_index=iter_index, topic=i)$Title)
        one_topic_titles <- paste(titles, collapse=" || ")
        data.frame(topic=i, top_titles=one_topic_titles)
    }

    stopCluster(cl)

    if(remake_figs) {
        if(!exists("build_plot_title", mode="function")) {
            source(file="build_plot_title.R")
        }
        outfile <- build_plot_title(dataset_name=dataset_name,
                                    ntopics=ntopics,
                                    iter_index=iter_index,
                                    subset_name=subset_name,
                                    bad.topics=bad.topics,
                                    whatitis="top titles per topic",
                                    for.filename=T)
        outfile <- file.path(imageloc, paste0(outfile, ".csv"))
        
        write.csv(titles_all, outfile, row.names=F)
        return(titles_all)
    } else {
        return(titles_all)
    }
}

if(autorun) {
    remake_figs
    titles <- find_topic_titles(dataset_name="noexcludes2001_2015",
                      ntopics=50,
                      iter_index=1,
                      subset_name="knownprograms2001_2015")
    titles <- find_topic_titles(dataset_name="noexcludes2001_2015",
                                ntopics=50,
                                iter_index=1,
                                subset_name=NULL)
}
