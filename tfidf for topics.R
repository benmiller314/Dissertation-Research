# Given a topic-word data.table as per "get topic word grid.R",
# calculate the most differentially interesting words for each topic
# by using a TF-IDF approach. Let TF, here, be the weight of that word in that topic,
# and ITF = ln(ntopics/topics-with-term). Multiply TF times ITF to get the new weight.

# Returns both an updated topic-word data.table (as $tw) 
# and a three-column data.table of topic numbers and top words (as $topN)

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
        
    tw[, ITF:=log(max(topic)/.N), by=token]     # data.table has a built-in function for counting
                                    # frequencies across the whole table
    tw[, TFITF:=weight*ITF]
    
    # # gather results
    # topic.topwords <- data.frame(topic=seq_len(ntopics),
    #                        by_tfitf="", 
    #                        by_prob="",
    #                        stringsAsFactors = FALSE)
    # 
    # for(i in seq_len(ntopics)) {
    #     by_tfitf <- tw[topic==i][order(-TFITF)][1:20, token]
    #     by_prob <- tw[topic==i][order(-probability)][1:20, token]
    #     
    #     topic.topwords[i, "by_tfitf"][[1]] <- I(list(as.character(by_tfitf)))
    #     topic.topwords[i, "by_prob"][[1]]  <- I(list(as.character(by_prob))) 
    #     
    # }
    
    # Make simple version for data labeling, synonym-finding, etc
    # first by tfitf 
    topN <- tw[order(-TFITF), .SD[1:nwords], by=topic]
    topN <- topN[, .(by_tfitf=paste(token, collapse=" ")), by=topic][order(topic)]
    
    # but also save the old (probability) toplist, for comparison
    topM <- tw[order(-probability), .SD[1:nwords], by=topic]
    topM <- topM[, .(by_prob=paste(token, collapse=" ")), by=topic][order(topic)]
    
    # and export them together
    topN <- merge(topN, topM, by="topic")
    
    if(remake_figs) {
        tryCatch(
            {
                outfile <- paste0("top ", nwords, " topic topwords by tfitf, ", 
                              dataset_name, "k", ntopics, 
                              "_", iter_index, ".csv")
                outfile <- file.path(imageloc, outfile)
                message("Saving ", outfile, "...")
                write.csv(topN, file=outfile, row.names=F)
            },
            error = function(e) e,
            finally = message("done.")
        )
    } else {
        # View(topN)
    }
    
    
    return(list("tw" = tw,
                "topN" = topN))
}

if(FALSE) {
    tf <- tfidf.for.topics(tw=tw)    
}

compare_topic_vocablists <- function(mytopic, # just a number 
                                     tf)      # result from above
{
    topwords <- tf$topN[mytopic, by_prob]
    topwords <- sort(strsplit(topwords, " ")[[1]])
    
    itfwords <- tf$topN[mytopic, by_tfitf]
    itfwords <- sort(strsplit(itfwords, " ")[[1]])
    
    data.table(topwords, itfwords)
}

# testing area
if(FALSE) {
    # get cluster names (ksplits) from frameToD3.R
    # e.g. lists of topic names (w/prefixed numbers) with 20 clusters = k20
    i=20
    mynames <- k20[[i]]
    m <- gregexpr("^[0-9]", mynames)
    regmatches(mynames, m)

    
}
