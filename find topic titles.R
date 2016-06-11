# Get titles for a subset of dissertations, save to a file for ease of recall later
find_topic_titles <- function(dataset_name = "consorts", 
                              ntopics      = 55, 
                              subset_name  = NULL) 
{
    titles_all <- data.frame(topic=1:ntopics)
    
    for (i in 1:ntopics) {
        titles <- top_topic_browser(for.bind=T, subset_name=subset_name, topic=i)$Title
        one_topic_titles <- ""
        for (j in titles) {
            print(j)
            one_topic_titles <- paste(one_topic_titles, j, sep="||")
        }
        titles_all[i, "titles"] <- one_topic_titles
    }
 
    if(remake_figs) {
        filename <- paste0(unixsourceloc, "top_titles_per_topic-", dataset_name, "k", ntopics, subset_name, ".csv")
        write.csv(titles_all, paste0(imageloc, filename))
    } else {
        return(titles_all)
    }
}
