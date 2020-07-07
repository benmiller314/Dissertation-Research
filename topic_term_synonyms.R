
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

    
    a_keys <- keys[topic == topic_a]
    b_keys <- keys[topic == topic_b]
    
    a_words <- strsplit(as.character(a_keys[, ..term_source]), " ")[[1]]
    b_words <- strsplit(as.character(b_keys[, ..term_source]), " ")[[1]]

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
#  DONE * use new topic-word tables from "get_topic_word_grid.R" instead of default top 20 words


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
        source(file="get_topic_word_grid.R")
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
                                  bad.topics=NULL,  # for use in visualizations,
                                                    # eg in frameToD3.R
                                  tw.grid=NULL) # pass explicitly to allow for combined grids from several iter_index runs
{
    # load required functions
    require(philentropy)

    if(!exists("topicword.probability.grid", mode="function")) {
        source(file="get_topic_word_grid.R")
    }

    # if we have tw.grid, skip ahead to getting distances. otherwise, build tw.grid.
    if(is.null(tw.grid)) {
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

        # remove bad.topics (works even if bad.topics is NULL)
        tw.grid <- tw.grid[, setdiff(names(tw.grid), bad.topics), with=F]

    }

    # convert to matrix and transpose, because philentropy expects vectors as rows
    tw.matrix <- t(as.matrix(tw.grid))

    # now we can calculate a distance matrix
    d <- distance(tw.matrix, method=dist_method)
    dimnames(d)[[1]] <- dimnames(d)[[2]] <- dimnames(tw.matrix)[[1]]

    return(d)
}

topic_clusters <- function(twm = NULL,               # a topic-distance matrix, as above
                           tw = NULL,                # a topic-word table. if it exists, gives a big speed boost.
                           clust.method = c("agnes", "diana"),
                                                     # use agglomerative (cluster::agnes) or
                                                     # divisive (cluster::diana) clustering?
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
                              bad.topics = bad.topics,
                              tw=tw)
    }

    clust.method <- match.arg(clust.method)
    clust <- switch(clust.method,
                    agnes = agnes(twm, diss=TRUE, method="ward"),
                    diana = diana(twm, diss=TRUE)
           )


    if(use.labels) {
        if(!exists("get_topic_labels", mode="function")) {
            source(file="get topic labels.R")
        }
        labs <- get_topic_labels(dataset_name=dataset_name, ntopics=ntopics,
                                 subset_name=subset_name, iter_index=iter_index)
        setkey(labs, Topic)

        clust$order.lab <- paste(clust$order.lab,
                              labs[as.numeric(clust$order.lab), Label])
        rm(labs)
    }

    if(do.plot) {
        plot(clust)
    }

    return(clust)
}

# members <- c(18, 42, 49, 35, 27)
topic_distances_in_cluster <- function(members,    # a vector of topic numbers,
                                         # representing (e.g.) topics in a given cluster
                                         dataset_name="noexcludes2001_2015",
                                         ntopics=50,
                                         iter_index=1,
                                         bad.topics=NULL,
                                         twm=NULL,
                                         tw=NULL)     # don't waste time rebuilding if we have them

{

    if(is.null(twm)) {
        topic_distance_matrix(dataset_name=dataset_name,
                              ntopics=ntopics,
                              iter_index=iter_index,
                              bad.topics=bad.topics,
                              tw=tw)
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

# assessing clusters
tw_cluster_weight <- function(mytopics,  # a set of topic numbers (presumed to be members of a given cluster)
                              dt)        # a doc-topic (NB: NOT topic-word) matrix
{
    if(anyNA(as.numeric(mytopics))) {
        stop("mytopics should be a set of numbers corresponding to topics")
    } else {
        mytopics <- as.numeric(mytopics)
    }

    allweights <- colSums(dt[, which(names(dt) != "Pub.number"), with=F])
    myweights <- colSums(dt[, which(names(dt) %in% mytopics), with=F])
    # myweights / sum(allweights)

    tab <- data.table(topic = mytopics,
                      topicweight = myweights/sum(allweights))
    tab[, clusterweight := sum(topicweight) ]

    return(tab)

    # tw[topic %in% mytopics, .(topicweight = sum(.SD$weight)/sum(tw$weight)), by=topic][, .(topic, topicweight, clusterweight=sum(topicweight))]
}

tree_summary <- function(nclust,
                         twm = NULL,    # a topic-word distance matrix
                         tw = NULL,     # a topic-word table
                         tf = NULL,     # a table of top words for each topic,
                                        # as per 'tfidf for topics.R'
                         dt = NULL,     # a doc-topic table
                         dataset_name = "noexcludes2001_2015",
                         ntopics = 150,
                         iter_index = 1,
                         subset_name = NULL,
                         newnames=F,         # where in the MALLET output filename does iter_index appear?
                                             # set T if it's with the model, F if last in filename.
                                             # Gets passed into get.doctopic.grid.
                         bad.topics = NULL,
                         slow=F,                # optionally go one cluster at a time
                         internal.distances=F,
                         clust.method=c("agnes", "diana"))
{

    # we have to re-build the tree to ensure we're not using labels, which would break the tree_summary
    clust.method <- match.arg(clust.method)
    clust <- topic_clusters(twm = twm,
                         tw = tw,
                         dataset_name = dataset_name,
                         ntopics = ntopics,
                         iter_index = iter_index,
                         bad.topics = bad.topics,
                         subset_name = subset_name,
                         use.labels = FALSE,
                         clust.method = clust.method)

    # also, let's make sure we have a by_tfitf column
    if(is.null(tf)) {
        if(!exists("tfidf.for.topics", mode="function")) {
            source(file="tfidf for topics.R")
        }
        tf <- tfidf.for.topics(tw = tw,
                               dataset_name = dataset_name,
                               ntopics = ntopics,
                               iter_index = iter_index)
    }

    # and we'll want the doc-topic table for topic weights, too
    if(is.null(dt)) {
        if(!exists("get.doctopic.grid", mode="function")) {
            source(file="get doctopic grid.R")
        }
        dt <- get.doctopic.grid(dataset_name=dataset_name, ntopics=ntopics,
                                subset_name=subset_name, iter_index=iter_index,
                                newnames=newnames)$outputfile.dt

        # Ben: Exclude any NA rows included accidentally by the index file
        dt <- na.omit(dt)
    }

    if(remake_figs) {
        filename <- paste0("topic-cluster-dendrogram--",
                                  dataset_name,
                                  "k", ntopics,
                                  "_iter", iter_index,
                                  subset_name, "--",
                                  clust.method, ".pdf")
        filename <- file.path(imageloc, filename)
        pdf(filename)
    }

    pltree(clust)

    if(remake_figs) { dev.off() }

    hc <- as.hclust(clust)
    rhc <- rect.hclust(hc, k=nclust)

    # TO DO: store cluster membership and weight as you go, return that table

    to.return <- data.table("cluster"=seq_len(nclust),
                            "members"=rep(as.list(NA), nclust),
                            "size"=rep(as.numeric(NA), nclust)
                            )
    setkey(to.return, cluster)

    for(i in seq_along(rhc)) {
        mytopics <- names(rhc[[i]])
        mytopics <- as.numeric(mytopics)

        twcw <- tw_cluster_weight(mytopics=mytopics, dt=dt)
        pct <- round(100*mean(twcw$clusterweight), 2)

        message("Cluster ", i, " (% of corpus: ", pct, "):")

        to.return[i, `:=`(members = list(paste(mytopics, collapse=", ")),
                          size = pct)]

        tops <- tf$topN[topic %in% mytopics, by_tfitf]
        print(cbind(twcw, tops))

        if(slow) {
            rect.hclust(hc, k=nclust, which=i, border="blue")
            rect.hclust(hc, k=nclust, which=i, border="blue")
            rect.hclust(hc, k=nclust, which=i, border="blue")
            adv <- readline("Press <enter> for next cluster")
            pltree(ag)
            rect.hclust(hc, k=nclust)
        }

        if(internal.distances) {
            message("Distances between topic-word vectors for this cluster:")
            topic_distances_in_cluster(members=mytopics,
                                       twm=twm,
                                       tw=tw,
                                       dataset_name=dataset_name,
                                       ntopics=ntopics,
                                       iter_index=iter_index,
                                       bad.topics=bad.topics)
        }

    }

    return(to.return)
}


if(autorun) {
    dataset_name <- "noexcludes2001_2015"
    ntopics <- 150
    iter_index <- 1
    bad.topics <- NULL

    tw <- build.topicword.table(dataset_name=dataset_name,
                                    ntopics=ntopics,
                                    iter_index=iter_index,
                                    bad.topics=bad.topics)

    twm <- topic_distance_matrix(dataset_name = dataset_name,
                                 ntopics = ntopics,
                                 iter_index = iter_index,
                                 bad.topics = bad.topics,
                                 tw=tw)

    clust <- topic_clusters(twm=twm,
                         tw=tw,
                         dataset_name = dataset_name,
                         ntopics = ntopics,
                         iter_index = iter_index,
                         bad.topics = bad.topics,
                         clust.method = "d")

    if(!exists("tfidf.for.topics")) { source(file="tfidf for topics.R") }
    tf <- tfidf.for.topics(tw=tw)


    tree_summary(ag=ag, tw=tw, nclust=10, slow=F)

    clust.method = "agnes"
    clust2 <- topic_clusters(twm=twm,
                             tw=tw,
                             dataset_name = dataset_name,
                             ntopics = ntopics,
                             iter_index = iter_index,
                             bad.topics = bad.topics,
                             clust.method = clust.method,
                             use.labels = T,
                             do.plot = F) 
        
        if(remake_figs) {
            dendrogram.outfile <- build_plot_title(dataset_name = dataset_name,
                                                   ntopics = ntopics,
                                                   iter_index = iter_index,
                                                   subset_name = subset_name,
                                                   whatitis = paste0("topic cluster dendrogram--", clust.method),
                                                   for.filename = TRUE
            )
            dendrogram.outfile <- paste0(dendrogram.outfile, ".pdf")
            dendrogram.outfile <- file.path(imageloc, dendrogram.outfile)
            pdf(dendrogram.outfile)
        }
    plot(clust2, which.plots = 2)
    if(remake_figs) { 
        dev.off()
    }
    
    
    
    #*******************************************#
    #.      noexcludes2001_2015k150_iter1      .#
    #.                          ^^^            .#
    #...........................................#
    # top words by topic number
    tf$topN$by_tfitf[c(1, 5, 119, 75, 8, 139)]     # digital media cluster
    tf$topN$by_tfitf[c(2, 17, 61, 57, 14, 54, 105, 30)] # rhetoric & philosophy cluster
    tf$topN$by_tfitf[c(2, 17, 61, 57, 14, 54, 105, 30)]
    tf$topN$by_tfitf[c(27, 133, 118, 7)]  # bad ocr cluster
    tw[topic %in% c(3, 6, 86, 22, 130, 65, 34, 146, 88, 101, 124, 141), sum(.SD$weight)/sum(tw$weight), by=topic]

    # new way to calculate cluster weight:

    tw_cluster_weight(td=td, mytopics=c(2, 17, 61, 57, 14, 54, 105, 30))
    tw_cluster_weight(td=td, mytopics=c(3, 6, 86, 22, 130, 65, 34, 146, 88, 101, 124, 141))
    tw_cluster_weight(td=td, mytopics=c(27, 133, 118, 7))


    #*******************************************#
    #.       noexcludes2001_2015k50_iter1      .#
    #...........................................#

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
    clust <- topic_clusters(twm, use.labels=T)
    k20

    tw_cluster_weight(tw=tw, mytopics=c(18,42,39,35,27))

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
