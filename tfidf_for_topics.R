# Given a topic-word data.table as per "get_topic_word_grid.R",
# calculate the most differentially interesting words for each topic
# by using a TF-IDF approach. Let TF, here, be the weight of that word in that topic,
# and ITF = ln(ntopics/topics-with-term). Multiply TF times ITF to get the new weight.
#
# Returns both an updated topic-word data.table (as $tw) 
# and a three-column data.table of topic numbers and top words (as $topN)
#
# Some global variables I'm assuming:
# 1. remake_figs (boolean): should we save to file?
# 2. imageloc (character): a file.path to a directory where files should be saved

tfidf.for.topics <- function(nwords=20,   # how many top words to display?
                  tw=NULL,        # the topic-word data.table. 
                                  # If it exists, pass in for a (big) speed boost
                  dataset_name="noexcludes2001_2015",
                  ntopics=50,
                  iter_index=1)
{
    if(!exists("build.topicword.table", mode="function")) { 
        source(file="get_topic_word_grid.R") 
    }
    
    # if we don't have a topic-word matrix, let's get one here. 
    # should be stable for each topic model, so I recommend saving it, but ymmv.
    
    if(is.null(tw)) {
        tw <- build.topicword.table(dataset_name=dataset_name, 
                                    ntopics=ntopics, 
                                    iter_index=iter_index)
    }
    
        
    # IMPORTANT NOTE: because of how := works (by reference),
    # the data.table tw will change in the calling environment, too,
    # if it exists there.
    
    tw[, ITF:=log(ntopics/.N), by=token]     # .N is the number of rows in each group (determined by "by") 
                                             
    tw[, TFITF:=weight*ITF]                  # Note that the order is identical if using 
                                             # weight as TF or probability as TF
                                               
    
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

# side-by-side alphabetized lists of the two kinds of toplists, for a given topic
compare_topic_vocablists <- function(tf,        # result from above
                                     mytopic,    # a topic number
                                     alphasort = T)      
{
    topwords <- tf$topN[mytopic, by_prob]
    topwords <- strsplit(topwords, " ")[[1]]
    
    itfwords <- tf$topN[mytopic, by_tfitf]
    itfwords <- strsplit(itfwords, " ")[[1]]
    
    if(alphasort) {
        topwords <- sort(topwords)
        itfwords <- sort(itfwords)
    }
    
    data.table(topwords, itfwords)
}


# testing area / examples
if(FALSE) {
    remake_figs=T
    tf <- tfidf.for.topics()
    tf <- tfidf.for.topics(tw=tw)    
    compare_topic_vocablists(tf=tf, mytopic=1)
    compare_topic_vocablists(tf=tf, mytopic=1, alphasort=F)
    lapply(1:ntopics, function(x) {
        message("Topic:", x)
        print(compare_topic_vocablists(tf=tf, mytopic=x, alphasort=F))
        readline("<press any key for next topic>")
        })

    
    
}


message("(tfidf_for_topics.R) The following functions have been loaded:\n",
        " * tfidf.for.topics(nwords, tw, dataset_name, ntopics, iter_index)\n",
        " * compare_topic_vocablists(mytopic, tf)")
