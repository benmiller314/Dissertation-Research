
# load the WordNet dictionary of synsets
require(data.table)
require(wordnet)        # NB: if this crashes R, try updating Java: 
                        # https://stackoverflow.com/questions/26252591/mac-os-x-and-multiple-java-versions#answer-47699905
setDict(file.path(WNHOME, "dict"))


# # dev values
# dataset_name <- "noexcludes2001_2015"
# ntopics <- 50
# iter_index <- 1
# subset_name <- "knownprograms2001_2015"
# topic_a <- 1    # Tech Comm
# topic_b <- 11   # Genre and Move Analysis


# Find synonyms for one word
get_syns <- function(word) {
    syns <- synonyms(word, "NOUN")
    syns <- c(syns, synonyms(word, "VERB"))
    syns <- c(syns, synonyms(word, "ADJECTIVE"))
    syns <- c(syns, synonyms(word, "ADVERB"))
    return(syns)
}

# Given one word, look for synonyms (including exact matches) in a list of words
match_one_word <- function(word, wordlist) {
    match_index <- which(wordlist %in% get_syns(word))
    if(any(match_index)) {
        return(list(A=word, B=wordlist[match_index]))
    } else {
        return(NULL)
    }
}
# match_one_word("design", a_words)

# Given two lists of words, find all synonyms between them (including exact matches) 
match_lists <- function(listA, listB) {
    matchlist <- data.frame("A"=list(), "B"=list())
    for (i in seq_along(listA)) {
        matches <- match_one_word(listA[i], listB)
        if(length(matches$B) > 1) {
            matches$B <- list(matches$B)
        }
        # print(matches)
        matchlist <- rbindlist(list(matchlist, matches))
    }
    return(matchlist)
}

# if(FALSE) {
# test <- match_lists(c("design", "information", "reading"), c("plan", "genres", "project"))
# # test <- match_lists(a_words, b_words)
# # rm(test)
# }

two_topic_synonyms <- function(topic_a,
                               topic_b,
                               term_source = c("by_tfitf", "by_prob"),
                               dataset_name = "noexcludes2001_2015", 
                               ntopics = 50,
                               iter_index = 1,
                               subset_name = "",
                               tw = NULL)    # if topic-word matrix already exists,
                                             # pass it in for a major speed boost
{
    
    term_source <- match.arg(term_source)
    
    # use topic-word table
    if(!exists("tfidf.for.topics", mode="function")) {
        source("tfidf for topics.R")
    }
    
    keys <- tfidf.for.topics(dataset_name=dataset_name, ntopics=ntopics,
                             iter_index=iter_index, tw=tw)$topN
    
    if (term_source == "by_tfitf") {
        keys <- keys$by_tfitf
    } else if(term_source == "by_prob") {
        keys <- keys$by_prob 
    } 
        
        a_words <- strsplit(as.character(keys[topic_a]), " ")[[1]]
        b_words <- strsplit(as.character(keys[topic_b]), " ")[[1]]
    
    # For each term in topic A, check for synonyms in topic B
    matchlist <- match_lists(a_words, b_words)
    if(nrow(matchlist)) {
        colnames(matchlist) <- paste("Topic", c(topic_a, topic_b))
    } else {
        warning("No matching terms or synonyms using ", getDict()@jclass)
    }
    return(matchlist)
}


#### To do: 
#       * calculate distances between two topics
#       * calculate distances between all pairs of topics above a threshold weight
#       * if distance is below a certain threshold, find synonyms
#       * add topic labels to output, not just topic numbers
#  DONE * use new topic-word tables from "get topic word grid.R" instead of default top 20 words


# Calculate distances between two topics
two_topic_distance <- function(topic_a,     # just a number in 1:ntopics
                               topic_b,     # just a number in 1:ntopics
                               dist_method="jensen-shannon",
                                            # anything in philentropy::distance
                               dataset_name="noexcludes2001_2015", 
                               ntopics=50,
                               iter_index=1,
                               tw=NULL)     # if we have a topic-word matrix,
                                            # don't waste time rebuilding it
{
    # load required functions
    require(philentropy)
    
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
    
    # extract just equal-length probability vectors (sorted by token_ind)
    tw.grid <- topicword.probability.grid(tw)
    
    # grab our two topics
    mycols <- c(topic_a, topic_b)
    myframe <- as.data.frame(tw.grid[, ..mycols])
    myframe <- t(myframe)             # distance() wants vectors in rows
    
    # and measure 'em
    d <- distance(myframe, method = dist_method)    
    return(d)
}

topic_distance_matrix <- function(dataset_name="noexcludes2001_2015", 
                                  ntopics=50,
                                  iter_index=1,
                                  dist_method="jensen-shannon", 
                                               # anything in philentropy::distance.
                                  tw=NULL,     # if we have a topic-word matrix,
                                               # don't waste time rebuilding it.
                                  bad.topics=NULL)  # for use in visualizations,
                                                    # eg in frameToD3.R
{
    # load required functions
    require(philentropy)
    
    if(!exists("topicword.probability.grid", mode="function")) { 
        source(file="get topic word grid.R") 
    }
    
    # if we don't have a topic-word matrix, let's get one here, 
    # so we don't have to do it twice (once for each topic)
    
    if(is.null(tw)) {
        tw <- build.topicword.table(dataset_name=dataset_name, 
                                    ntopics=ntopics, 
                                    iter_index=iter_index,
                                    bad.topics=bad.topics)
    }
    
    # extract just equal-length probability vectors (sorted by token_ind)
    tw.grid <- topicword.probability.grid(tw)
    
    if(!is.null(bad.topics)) {
        tw.grid <- tw.grid[, !(..bad.topics)]
    }
    
    # convert to matrix and transpose, because philentropy expects vectors as rows
    tw.matrix <- t(as.matrix(tw.grid))
    
    # now we can calculate a distance matrix
    d <- distance(tw.matrix, method=dist_method)
    dimnames(d)[[1]] <- dimnames(d)[[2]] <- dimnames(tw.matrix)[[1]]
    
    return(d)
}

topic_clusters <- function(twm = NULL,               # a topic-distance matrix, as above
                           agnes_method = "ward",   # from package(cluster)    
                           do.plot = TRUE,
                           use.labels = FALSE,
                           dataset_name = "noexcludes2001_2015",
                           ntopics = 50,
                           iter_index = 1,
                           subset_name = NULL,
                           bad.topics = NULL)        
{    
    require(cluster)
    
    if(is.null(twm)) {
        twm <- topic_distance_matrix(dataset_name = dataset_name,
                              ntopics = ntopics,
                              iter_index = iter_index,
                              bad.topics = bad.topics)
    }
    
    ag <- agnes(twm, diss=TRUE, method=agnes_method)
    
    if(use.labels) {
        if(!exists("get_topic_labels", mode="function")) { 
            source(file="get topic labels.R") 
        }
        labs <- get_topic_labels(dataset_name=dataset_name, ntopics=ntopics,
                                 subset_name=subset_name, iter_index=iter_index)
        setkey(labs, Topic)
        
        ag$order.lab <- paste(ag$order.lab, 
                              labs[as.numeric(ag$order.lab), Label])
        rm(labs)
    }
    
    if(do.plot) {
        pltree(ag)    
    }
    
    return(ag)
}
    
# members <- c(18, 42, 49, 35, 27)
topic_distances_in_cluster <- function(members,    # a vector of topic numbers,
                                         # representing (e.g.) topics in a given cluster
                                         dataset_name="noexcludes2001_2015", 
                                         ntopics=50,
                                         iter_index=1,
                                         bad.topics=NULL,
                                         twm=NULL)     # if we have a topic distance matrix,
    # don't waste time rebuilding it
{
    
    if(is.null(twm)) {
        topic_distance_matrix(dataset_name=dataset_name,
                              ntopics=ntopics,
                              iter_index=iter_index,
                              bad.topics=bad.topics)
    }
    
    clust_dist <- twm[members, members]
    
    return(list("m" = clust_dist,
                "max" = max(clust_dist),
                "min" = min(clust_dist[which(clust_dist > 0)]),
                "avg" = mean(clust_dist[which(clust_dist > 0)])
                )
           )
}
    
get_cluster_names <- function(hcr,            # result of an hclust.rect()
                              position=NULL)  # which cluster within the list?
{
    namelist <- sapply(hcr, function(i) {names(i) })
    
    if (is.null(position)) { 
        return (namelist) 
    } else {
        return(namelist[position][[1]])
    }
}


if(autorun) {
    if(is.null(tw)) {
        tw <- build.topicword.table(dataset_name=dataset_name, 
                                    ntopics=ntopics, 
                                    iter_index=iter_index,
                                    bad.topics=bad.topics)
    }
    # 44: Online Circulation and Social Media;  45: Digital Media Affordances
    two_topic_synonyms(topic_a=44, topic_b=45, tw=tw)
    two_topic_distance(44, 45, tw=tw)      # 0.3655386
    
    
    # 11: Genre and Move Analysis;  22: Linguistics
    two_topic_distance(11, 22, tw=tw)      # 0.337527
    two_topic_synonyms(11, 22, tw=tw)      # no matches in top 20
    
    # 1: Tech Comm   40: Scientific Discourse
    two_topic_distance(1, 40, tw=tw)      # 0.3968118

    # 42: K12 teaching of writing    49: Scenes of Teaching
    two_topic_distance(42, 49, tw=tw)     # 0.2912036
    two_topic_synonyms(42, 49, tw=tw)
    

    twm <- topic_distance_matrix(tw=tw)
    ag <- topic_clusters(twm, use.labels=T)
    k20
}



if(FALSE) {   # testing
    jsd <- JSD(tw.rows)
    ag <- agnes(jsd, diss=TRUE)
    pltree(ag)
    my.hellinger <- distance(tw.rows, method="hellinger")
    heatmap(my.hellinger)
    colnames(jsd)
    
    JSD(t(as.matrix(tw.grid[, 1:2])))
    JSD(rbind(tw_a, tw_2))
    tw_2 <- tw.grid[["2"]]
}
