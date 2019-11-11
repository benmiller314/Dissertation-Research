# Goal: given a topic clustering, see whether some schools are over-represented 
# in some clusters or branches within a cluster

schools_by_topic <- function(mytopic,
                             level = 0.12,     # percentage of a diss contributed by the topic
                                               # for it to "count"
                             howmany = 10,      # how many top schools to show?
                                               # use -1 for all.
                             dataset_name="noexcludes2001_2015",
                             ntopics=50,
                             subset_name="knownprograms2001_2015",  # set NULL if not using.
                             iter_index=1,     # suffix to differentiate repeat runs of same MALLET params.
                             outfile=NULL,     # if not provided, will be set to a default.
                             use.labels=FALSE, # replace topic numbers with labels
                                               # chosen using top_topic_browser()?
                             bad.topics= NULL, # exclude non-content-bearing topics
                             dt=NULL,          # doctopic grid. pass in for a speed boost.
                             quiet=FALSE)      # if TRUE, suppress messages
                             
{
    require(data.table)
    
    if(is.null(dt)) {
        
        if(!exists("get.doctopic.grid", mode="function")) { 
            source(file="get doctopic grid.R") 
        }
        dt <- get.doctopic.grid(dataset_name=dataset_name,
                                ntopics=ntopics,
                                subset_name=subset_name,
                                iter_index=iter_index)$outputfile.dt
        
        # Exclude any NA rows included accidentally by the index file
        dt <- na.omit(dt)
    }    
    
    # Check for non-content-bearing topics, but don't remove them
    if (is.null(bad.topics) && mytopic %in% bad.topics) {
        warning("Topic ", mytopic, " has been marked as non-content-bearing.")
    }
    
    # narrow to columns we need
    dt[, c("Pub.number", mytopic), with=F]
    mytopic.c <- as.character(mytopic)
    
    # Get the relevant documents from the doc-topic grid
    mypubs <- dt[dt[[mytopic.c]] > level, c("Pub.number", mytopic), with=F]
    setnames(mypubs, mytopic.c, "contrib")
    
    ## Use that document list to retrieve school/department info from the dataset
    if(!exists(dataset_name)) {
        source(file="dataprep 2 - load data.R")
    }
    
    # if using subset, make sure we limit to those.
    if(!is.null(subset_name) && nchar(subset_name) > 1) {
        dataset <- get(subset_name)
    } else {
        dataset <- get(dataset_name)
    }
    
    dataset <- as.data.table(dataset)
    setkey(dataset, "Pub.number")
    
    
    index <- which(dataset$Pub.number %in% mypubs$Pub.number)
    mydepts <- dataset[index, .(Pub.number, School, Department, realconsort, realrhetmap)]
    
    # Join diss data to topic contrib data. NB: mydepts may be smaller than mypubs, 
    # because some of mypubs may be outside of the subset.
    mydepts <- merge(mydepts, mypubs, by="Pub.number")
    
    # because department names can be finicky, and because we update from alumni lists
    # but don't always update departments, use  knownprogam bits to cluster instead
    mydepts[, knownprogram:=(realconsort | realrhetmap)]
    mydepts[, `:=`(realconsort=NULL, realrhetmap=NULL)]
    
    # Note that if the subset is a form of knownprograms, mydepts$knownprogram should 
    # all be TRUE. It may be possible to find the topics that maximize the ratio of 
    # TRUE to FALSE in this category, after returning this function with howmany=-1
    # UPDATE: now implemented as knownprogram_topical_ratio(), below.
    
    setkey(mydepts, School, knownprogram)
    
    # TO DO: get a disstotal from the larger dataset for each dept,
    #        to serve as denominator for a DissPct (topical focus)
    
    
    mydepts[, deptavg:=mean(contrib), by=.(School, knownprogram)]
    
    # View(mydepts[order(School)])
    toplist <- mydepts[, .N, by=.(School, knownprogram)][order(-N)]
    
    
    title <- paste("DissCount with topic", mytopic, ">", level)
    
    if(use.labels) {
        if(!exists("get_topic_labels")) { source(file="get topic labels.R") }
        tryCatch(expr={labels <- get_topic_labels(dataset_name=dataset_name, 
                                   ntopics=ntopics, 
                                   subset_name=subset_name, 
                                   iter_index=iter_index)
                        title <- paste0(title, "\n Topic label: '", 
                                        labels[mytopic, Label] ,"'")},
                error=function(e) e,
                finally=""
        )
    }
    
    # TO DO: save this info using remake_figs
    if(!quiet) { 
        message(title) 
    }
    if (howmany > 0) {
        toplist <- toplist[1:howmany]
    }
    
    return(toplist)
}

if(FALSE) {
    schools_by_topic(41)
    schools_by_topic(1, use.labels=T)
    table(schools_by_topic(1, subset_name=NULL, howmany=-1)$knownprogram)
}

knownprogram_topical_ratio <- function(level = 0.12,     # percentage of a diss contributed by the topic
                                       # for it to "count"
                                       howmany = 10,      # how many top schools to show?
                                       # use -1 for all.
                                       dataset_name="noexcludes2001_2015",
                                       ntopics=50,
                                       iter_index=1,     # suffix to differentiate repeat runs of same MALLET params.
                                       outfile=NULL,     # if not provided, will be set to a default.
                                       use.labels=TRUE, # replace topic numbers with labels
                                       # chosen using top_topic_browser()?
                                       bad.topics= NULL, # exclude non-content-bearing topics
                                       dt=NULL,          # doctopic grid. pass in for a speed boost.
                                       quiet=TRUE)      # if TRUE, suppress messages
{
    topic_list <- seq_len(ntopics)
    if(! is.null(bad.topics) ) {
        topic_list <- setdiff(topic_list, bad.topics)
    }
    
    ratios <- data.frame("topic.num"=numeric(),
                         "unknown.dept"=logical(),
                         "known.dept"=logical(),
                         "pct.known"=numeric())
    j <- 1
    for(i in topic_list) {
        result <- table(schools_by_topic(i,
                                         level=level,
                                         howmany=-1,
                                         dataset_name=dataset_name,
                                         ntopics=ntopics,
                                         iter_index=iter_index,
                                         subset_name=NULL,
                                         bad.topics=bad.topics,
                                         dt=dt, 
                                         quiet=TRUE)$knownprogram)
        ratios[j,] <- c(i, 
                        result[["FALSE"]], 
                        result[["TRUE"]], 
                        result[["TRUE"]]/(result[["TRUE"]]+result[["FALSE"]]))
        j <- j+1
    }
    
    if(use.labels) {
        if(!exists("get_topic_labels")) { source(file="get topic labels.R") }
        tryCatch(expr={labels <- get_topic_labels(dataset_name=dataset_name, 
                                                  ntopics=ntopics, 
                                                  subset_name=NULL, 
                                                  iter_index=iter_index)
            ratios <- merge(ratios, labels[, .(Topic, Label)], by.x="topic.num", by.y="Topic")
        },
        error=function(e) e,
        finally=""
        )
    }
    
    
    myorder <- order(ratios$pct.known, decreasing = T)
    ratios <- ratios[myorder,]
    row.names(ratios) <- NULL
    
    return(ratios)
}

if(FALSE) {
    ratios <- knownprogram_topical_ratio(bad.topics=bad.topics, dt=dt, use.labels=T)
}
    
# TO DO: finish this function, which should find differences among schools_by_topic 
# for topics within a given hclust.rect drawn on a hierarchical clustering plot
schools_by_topic_cluster <- function(mytopics,
                                     dataset_name="noexcludes2001_2015",
                                     ntopics=50,
                                     subset_name="realconsorts2001_2015",  # set NULL if not using.
                                     iter_index=1,     # suffix to differentiate repeat runs of same MALLET params.
                                     outfile=NULL,     # if not provided, will be set to a default.
                                     use.labels=FALSE, # replace topic numbers with labels
                                     # chosen using top_topic_browser()?
                                     bad.topics= NULL, # exclude non-content-bearing topics
                                     tw = NULL)        # topic-word matrix. If it exists,
                                                       # pass it in for a speed boost.)    
{
    
    
    # For all topics in a cluster, we first need a cluster
    if(!exists("topic_distance_matrix", mode="function")) { 
        source(file="topic_term_synonyms.R") 
    }
    
    
    twm <- topic_distance_matrix(dataset_name = dataset_name, 
                               ntopics = ntopics, 
                               iter_index = iter_index,
                               dist_method = "jensen-shannon",
                               tw = tw,
                               bad.topics = bad.topics)
    
    # topic_clusters() is also from topic_term_synonyms.R
    ag <- topic_clusters(twm, do.plot=F, use.labels=use.labels)
    
    hc <- as.hclust(ag)

}

if(!autorun) {
    message("`schools by topic.R`: The following functions have been loaded: \n",
            "* schools_by_topic(mytopic, ...) \n",
            "* knownprogram_topical_ratio(...) \n",
            "* schools_by_topic_cluster(mytopics, ...)")
}
