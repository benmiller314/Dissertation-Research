# given a topic-word data.table as per "get topic word grid.R",
# calculate the most differentially interesting words for each topic
# by using a TF-IDF approach. Let TF, here, be the weight of that word in that topic,
# and IDF = ln(ntopics/topics-with-term). Multiply TF times IDF to get the new weight.


tfidf.for.topics <- function(nwords=20,
                  tw=NULL,        # if it exists, pass in for a speed boost
                  dataset_name="noexcludes2001_2015",
                  ntopics=50,
                  iter_index=1)
{
    if(!exists("topicword.probability.grid", mode="function")) { 
        source(file="get topic word grid.R") 
    }
    
    # if we don't have a topic-word matrix, let's get one here, 
    # so we don't have to do it twice (once for each topic)
    
    if(is.null(tw)) {
        tw <- build.topicword.table(dataset_name=dataset_name, 
                                    ntopics=ntopics, 
                                    iter_index=iter_index)
    }
        
    tw[, IDF:=log(max(topic)/.N), by=token]     # data.table has a built-in function for counting
                                    # frequencies across the whole table
    tw[, TFIDF:=weight*IDF]
    
    # # gather results
    # topic.topwords <- data.frame(topic=seq_len(ntopics),
    #                        by_tfidf="", 
    #                        by_prob="",
    #                        stringsAsFactors = FALSE)
    # 
    # for(i in seq_len(ntopics)) {
    #     by_tfidf <- tw[topic==i][order(-TFIDF)][1:20, token]
    #     by_prob <- tw[topic==i][order(-probability)][1:20, token]
    #     
    #     topic.topwords[i, "by_tfidf"][[1]] <- I(list(as.character(by_tfidf)))
    #     topic.topwords[i, "by_prob"][[1]]  <- I(list(as.character(by_prob))) 
    #     
    # }
    
    # inspect results
    topN <- tw[order(-TFIDF), .SD[1:nwords], by=topic]
    if(remake_figs) {
        tryCatch(
            {
                outfile <- paste0("top ", nwords, " topic topwords by tfidf, ", 
                              dataset_name, "k", ntopics, 
                              "_", iter_index, ".csv")
                outfile <- file.path(imageloc, outfile)
                message("Saving ", outfile, "...")
                write.csv(topN, file=outfile)
            },
            error = function(e) e,
            finally = message("done.")
        )
    } else {
        View(topN)
    }
    
    return(topN)
}
