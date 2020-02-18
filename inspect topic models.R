###### Inspect Topic Models #####
#
# Assembles the basic data structures I'll need often
# to look at the results of a topic model by means of the various other files and scripts.
#
#################################

summarize_topic_clusters <- function(
    ############ Set Parameters ############

    ## Name the model ##
    dataset_name = "noexcludes2001_2015",
    ntopics = 50,
    iter_index = 1,
    newnames = F,         # where in the MALLET output filename does iter_index appear?
                        # set T if it's with the model, F if last in filename.
                        # Gets passed into get.doctopic.grid.

    ## Narrow within it?
    subset_name = "knownprograms2001_2015",
    bad.topics = c(3, 12, 50, 47, 34, 36, 30, 8, 15),

    ## A dissertation is "in" a cluster if it contains more than
    #  what cumulative level of words from all topics in the cluster?
    extent_level = 0.12,

    ## And how should we cluster those dissertations:
    # use agglomerative (cluster::agnes) or
    # divisive (cluster::diana) clustering?
    clust.method = "diana",

    ## In output files, should we include topic names?
    use.labels = TRUE,

    ## How many clusters to find?
    nclust = 20,

    ## Where to save?
    outfile_slug = paste0("topic-cluster-summary--",
                                         dataset_name, "k",
                                         ntopics, "_iter", iter_index,
                                         subset_name, "--",
                                         clust.method),

    ## Any one topic to inspect while we're here?
    onetopic = NULL,

    ## If you've done this already with this dataset, save some time
    tw = NULL

){

    ############# Bind the Data #############

    # Doc-topic grid, to establish weight of corpus/subcorpus
    source("get doctopic grid.R")
    dt <- get.doctopic.grid(dataset_name=dataset_name, ntopics=ntopics,
                    iter_index=iter_index, subset_name=subset_name,
                    newnames=newnames)$outputfile.dt
    dt <- na.omit(dt)
    dt <- dt[, setdiff(names(dt), bad.topics), with=F]

    # Topic-word tables, to establish clusters of topics
    if (is.null(tw)) {
        source(file="get topic word grid.R")
        tw <- build.topicword.table(dataset_name=dataset_name,
                                    ntopics=ntopics,
                                    iter_index=iter_index,
                                    bad.topics=bad.topics)
    }
    source(file="tfidf for topics.R")
    tf <- tfidf.for.topics(tw=tw)


    source(file="topic_term_synonyms.R")
    twm <- topic_distance_matrix(dataset_name = dataset_name,
                                 ntopics = ntopics,
                                 iter_index = iter_index,
                                 bad.topics = bad.topics,
                                 tw=tw)

    clust <- topic_clusters(twm=twm,
                         dataset_name = dataset_name,
                         ntopics = ntopics,
                         iter_index = iter_index,
                         subset_name = subset_name,
                         bad.topics = bad.topics,
                         clust.method = clust.method,
                         use.labels=use.labels)

    ############ Inspect Tree, Set Number of Clusters ############
    nclust <- 20    # TO DO: iterate this

    ############ Summarize Topic Clusters ############
    cl <- tree_summary(tw = tw,
                       tf = tf,
                       dt = dt,
                       dataset_name = dataset_name,
                       ntopics = ntopics,
                       iter_index = iter_index,
                       subset_name = subset_name,
                       nclust = nclust,
                       bad.topics = bad.topics,
                       slow = F,
                       clust.method = clust.method)
    cl[, nobads.size := round(100*size/sum(size), 2)]
    # cl
    # cl[order(-nobads.size)]
    # sum(cl$nobads.size)

    pltree(clust)


    extent <- sapply(seq_len(nrow(cl)), FUN=function(x) {
            cluster.strength(my.topics = strsplit(cl[x, members][[1]], ", ")[[1]],
                     dataset_name = dataset_name,
                     ntopics = ntopics,
                     iter_index = iter_index,
                     subset_name = subset_name,
                     bad.topics = bad.topics,
                     cumulative = TRUE,
                     level = extent_level,
                     grid = dt)
            })

    extent.vector <- round(100*unlist(extent["percentage", ]), 2)

    cl[, extent := extent.vector]

    message("Summary of topic clusters, ",
            paste0(dataset_name, "k", ntopics, "_iter", iter_index, subset_name, ", using ", clust.method, ":"))
    print(cl[order(-extent)])
    message("NB: `extent` refers to the percentage of dissertations in the corpus ",
            "with topics in this cluster \ncontributing at least ", extent_level*100,
            " percent of the diss (cumulatively). The column will sum to more than 100%:",
            " consider the example of a topic that contributes 12% of every document.",
            " That one topic would have a scaled size of 12, but an extent of 100.")

    if(remake_figs) {
        outfile <- file.path(imageloc,
                             paste0(outfile_slug, "--", nclust, "clusters.csv"))
        out <- cl[order(-extent)]
        out[, members:=as.character(members)]
        tryCatch(expr={
                    write.csv(out, outfile, row.names=F)
                 },
                 error=function(e) e,
                 finally=message("Topic cluster summary saved to ", outfile, ".")
        )
    }

    if(!is.null(onetopic)) {
        if (onetopic < ntopics || onetopic < 1) {
            source(file="top docs per topic.R")
            ttb <- top_topic_browser(topic = onetopic,
                              dataset_name = dataset_name,
                              ntopics = ntopics,
                              iter_index = iter_index,
                              subset_name = subset_name,
                              depth=20,
                              for.bind = TRUE)
            ttb[, Title]
        } else {
            message("Not inspecting topic ", onetopic, "; outside possible range ",
                    "in a model with ", ntopics, " topics.")
        }

    }
    return(list("cl"=cl,
                "extent"=extent))
}

name_clusters <- function(cl_summary,  # result of summarize_topic_clusters (above)
                          cluster_to_inspect = NULL,  # a number. if NULL, loop over all.
                          depth = 10
) {
    if(FALSE) { # test value
        cl_summary <- cl50
        cluster_to_inspect <- 5
    }


    # limit documents to the top 10 (or whatever) for this cluster
    # (NB: the sort of these documents happens in cluster.strength() from `topic cluster reach.R`)
    my.docs <- cl_summary$extent[, cluster_to_inspect]$docs[seq_len(depth)]
    my.levels <- cl_summary$extent[, cluster_to_inspect]$doc_levels[seq_len(depth)]
    my.topics <- strsplit(cl_summary$cl[cluster_to_inspect,]$members[[1]], ", ")[[1]]

    message("Now inspecting cluster ", cluster_to_inspect,
            " (topics ", cl_summary$cl[cluster_to_inspect,]$members[[1]], ")\n")
    message("Top cluster levels: \n")
    print(round(100*my.levels, 2))

    # and display abstracts etc for those docs
    # tops <-
        top_topic_browser(dataset_name = dataset_name,
                      ntopics = ntopics,
                      iter_index = iter_index,
                      subset_name = my.docs,
                      showlabels = TRUE,
                      topic = my.topics,
                      depth = depth
    #                   ,
    #                   for.bind=TRUE
    )
    #
    # setkey(tops, "Pub.number")
    # tops[my.docs, cluster_weight:=my.levels]
    # print(tops)
}


if(FALSE) {
    cl50 <- summarize_topic_clusters(ntopics=50)
    cl50$extent[]

    cl150 <- summarize_topic_clusters(ntopics=150, iter_index=2, bad.topics=NULL, subset_name=NULL)
}
