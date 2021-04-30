#############################################################################
# topic cluster reach.R
#
# GOAL: given a cluster of topics identified through frameToD3.R, find out
# how many dissertations include at least one topic in that cluster at a
# level of over 12% (or whatever).
#####


## helper function: define clusters of topics
# TO DO: save an index of defined clusters for a given topic model
name_topic_clusters <- function(dataset_name="noexcludes2001_2015",
                                ntopics=50,
                                iter_index=1,
                                subset_name=NULL) {

    if(dataset_name=="consorts" && ntopics==55 && iter_index=="" && is.null(subset_name)) {
        # The Teaching of Writing
        Teaching.of.Writing <- c(1, 32, 30, 3, 9, 39, 41, 40, 45, 6, 25, 8)

        # Theories of Meaning-Making
        Theories.of.Meaning.Making <- c(21, 18, 48, 14, 26, 53, 31, 29)

        # Audience and Context for Composing
        Audience.and.Context <- c(35, 49, 55, 27, 43, 46, 44)

        # Performative Identities, past and present
        Performative.Identities <- c(23, 10, 16, 33, 15, 11, 7, 37)

        # Politics and Power
        Politics.and.Power <- c(36, 20, 28, 54, 17, 52)

        # other
        Other <- c(5, 12, 42, 38, 51, 34, 19)

        all_clusters <- c("Teaching.of.Writing", "Theories.of.Meaning.Making",
                          "Audience.and.Context", "Performative.Identities",
                          "Politics.and.Power", "Other")

        # The Teaching of Writing subcluster that's especially classroom-y
        Teaching.of.Writing.1 <- c(1, 32, 30, 3, 9, 39, 41, 40)

        # The Teaching of Writing subcluster that's a little more administrative
        WPA <- c(45, 6, 25, 8)
    }

    if(dataset_name=="noexcludes2001_2015" && ntopics==50 && iter_index==1 && !is.null(subset_name) && subset_name=="realconsorts2001_2015") {
        Teaching.of.Writing <- c(18, 37, 6, 42, 16, 26, 41, 49, 28, 32, 35)

    }


    if(dataset_name=="noexcludes2001_2015" && ntopics==50 && iter_index==1
       # && !is.null(subset_name) && subset_name=="knownprograms2001_2015"
       ) {

        # stable clusters under both diana and agnes:
        
        small.teaching <- c(18, 35, 42, 49) # limits it to writing process (18), K-12 writing pedagogy (42),
                                            # scenes of teaching (49), and institutional supports (35)
                                            # (last to join is topic 35; could cut)
        digital.media <- c(44, 45)          # digital media and online circulation minicluster
        big.teaching <- c(1, 6, 11, 18, 27, # includes online / digital / tech-comm topics. a superset
                          35, 37, 40, 41,   # of small.teaching and digital.media clusters.
                          42, 44, 45, 49) 
        politics.law <- c(9, 13, 19, 29, 31) # politics and law cluster:
                                             # 9 Historical Public Rhetoric, esp of Women; 13 Politics, Empire, Radicalism;
                                             # 19 Presidential Discourse; 29 Writing in Law and Government;
                                             # 31 Labor Unions and Economic Activism
        
        ethnicities <- c(5, 17, 23)          # race, ethnicity, nationality cluster
        justice <- c(4, 20, 5, 17, 23, 10)   # superset of the ethnicities cluster;
                                             # leaves out 38 (reading through feminisms) b/c it
                                             # clusters with rhetoric under diana
        classics <- c(14, 25, 39)            # poetics/homiletics/classics cluster
        
        reading.rhetoric <- c(33, 48) # 33 Rhetorical Frameworks and 48 Reading Rhetoricians, Interpreting Philosophy
        bodies.and.power <- c(2, 21)  # microcluster of 2 Image, Body, Mind and 21 Rhetorics of Power, Conflict, Politics
        disciplinary.literacies <- c(28, 32) # microcluster of 28 Literacy Practices and 32 Disciplinary Formations
        big.rhetoric <- c(2, 21, 48, 33, 28, 32)  # superset of previous three clusters
        
        narrative <- c(16, 26)               # 16 creative writing/reflection 26 personal narr. + oral history
        entertainment <- c(7, 43)            # 7 popular media (film, tv, music) + 43 sports
        
        # clusters actually used in book:
        Theoretical.Orientations <- c(2, 21, 48, 33, 28, 32)
        Teaching.and.Administration <- c(18, 35, 42, 49)
        Public.Rhetorics.and.Political.Writing <- c(9, 13, 19, 29, 31)
        Narrative.and.Reflection <- c(16,26)
        People.and.Places <- c(4, 20, 5, 17, 23, 10)
        Poetics.and.Oratory <- c(14, 25, 39)
        Popular.Entertainment <- c(7, 43)
        Unstable.Group <- c(1, 41, 40, 46, 6, 37, 11, 22, 44, 45)
        Applied.Composition.Combo <- c(1, 40, 44, 45, 6, 11, 18, 42, 49, 35, 41, 37, 27)
        
        
        all_clusters <- c("small.teaching", "politics.law", "justice", "classics",
                          "big.rhetoric", "narrative")
        all_clusters_used <- c("Theoretical.Orientations",
                               "Teaching.and.Administration",
                               "Public.Rhetorics.and.Political.Writing",
                               "Narrative.and.Reflection",
                               "People.and.Places",
                               "Poetics.and.Oratory",
                               "Popular.Entertainment",
                               "Unstable.Group",
                               "Applied.Composition.Combo")
                               
    }

    # return variables created by this function
    newvars <- setdiff(ls(), formalArgs(name_topic_clusters))

    mydf <- data.frame()
    for (i in seq_along(newvars)) {
        mydf[i, "name"] <- newvars[i]
        mydf[i, "topics"] <- paste(get(newvars[i]), collapse=" ")
    }

    return(mydf)
}

## main wrapper function
cluster.strength <- function (my.topics=NULL,      # either pass in a list of topic numbers, or
                            my.topics_name=NULL,   # a string alias for a list of topic numbers; see above
                            dataset_name = "noexcludes2001_2015",
                            ntopics      = 50,
                            iter_index   = "1",
                            newnames=F,         # where in the MALLET output filename does iter_index appear?
                                                # set T if it's with the model, F if last in filename.
                                                # Gets passed into get.doctopic.grid.
                            bad.topics   = NULL,
                            level        = 0.12,
                            cumulative   = TRUE, # can several topics add up to meet that level?
                            subset_name  = NULL, # a named subset of data, either all columns or just Pub.numbers.
                            grid = NULL,         # a doc-topic grid. if it exists, offers a speed boost.
                            labels = NULL,       # result of get_topic_labels, if you want to label single topics
                            use.labels = FALSE   # set TRUE if you want to pass labels
){
    # Which topics are in the cluster?
    # to do: combine my.topics and my.topics_name and more gracefully detect which is which
    if(is.null(my.topics)) {
        if(is.null(my.topics_name)) {
            stop("cluster.strength() requires you to specify a cluster by passing either my.topics or my.topics_name.")
        } else {
            my.topics <- with(name_topic_clusters(dataset_name, ntopics, iter_index, subset_name),
                              get(my.topics_name))
        }
    } else if(is.null(my.topics_name)) {
        if (length(my.topics) == 1) {
            if (use.labels) {   # makes the most sense to spell out a topic when we only have 1
                if (!is.null(labels)) {
                    my.topics_name <- as.character(labels[my.topics, Label])
                } else {
                    if(!exists("get_topic_labels")) { source(file="get topic labels.R") }
                    tryCatch(expr={
                            labels <- get_topic_labels(dataset_name=dataset_name,
                                                      ntopics=ntopics,
                                                      subset_name=subset_name,
                                                      iter_index=iter_index)
                            my.topics_name <- as.character(labels[my.topics, Label])
                        },
                        error=function(e) e,
                        finally=""
                    )
                }
            } else {    # not using labels? stick with topic number
                my.topics_name <- paste0("T", my.topics)
            }
        } else {        # for multiple topics, just use topic numbers for brevity
            my.topics_name <- paste("topics", paste(my.topics, collapse=", "))
        }
    }

    # if we don't yet have a doctopic grid, get one
    if(is.null(grid)) {
        if(!exists("get.doctopic.grid", mode="function")) {
            source("get doctopic grid.R")
        }
        grid <- get.doctopic.grid(dataset_name=dataset_name, ntopics=ntopics,
                              iter_index=iter_index, subset_name=subset_name,
                              newnames=newnames)$outputfile.dt
    } else if (!is.null(subset_name)){
        if (nrow(get(subset_name)) != nrow(grid) ) {
            warning("`topic cluster reach.R`#125: Using an existing doc-topic grid and subset_name, but \n",
                "grid and subset have a different number of rows. Recalculating grid for subset.")
            if(!exists("get.doctopic.grid", mode="function")) {
                source("get doctopic grid.R")
            }
            grid <- get.doctopic.grid(dataset_name=dataset_name, ntopics=ntopics,
                                      iter_index=iter_index, subset_name=subset_name,
                                      newnames=newnames)$outputfile.dt
        }
    }

    # str(grid)
    # head(grid)

    # Save Pub.numbers to filter and return for further inspection
    pubs <- grid$Pub.number


    # Exclude non-content-bearing topics
    if(is.null(bad.topics) && dataset_name == "consorts" && ntopics == 55) {
        bad.topics <- c("4", "47", "22", "2", "24",   # bad OCR or ProQuest boilerplate
                        "13", "50")                   # language markers (Italian, Spanish)
    }

    grid <- grid[, setdiff(names(grid), c(bad.topics, "Pub.number")), with = F]
    # head(grid)

    if(any(my.topics %in% bad.topics)) {
        warning(paste("At least one topic in your list has been",
                      "identified as non-content-bearing"))
    }


    # Narrow our doctopic grid to the topics in question
    my.contribs <- grid[, names(grid) %in% my.topics, with = F]

    if (length(my.topics) == 1) {
        # If we're only looking at one topic, then my.contribs is a vector, and we can
        # find "winners" directly. Otherwise (see below), we'll need to consolidate 
        # several columns into a vector for comparison with our minimum target `level`.
        win.dex <- my.contribs
    } else if (!cumulative) {
        # If `cumulative` is false, at least one individual topic in the cluster must
        # be represented at higher than the minimum level set by `level` for a dissertation
        # to be counted within this cluster's reach. Use the max value of our cluster's topics.
        win.dex <- sapply(1:nrow(my.contribs), FUN = function(x) {
                             max(my.contribs[x]) } )
    } else {
        # If `cumulative` is true, combine the contributions from all topics
        # within the cluster before comparing to the minimum target level.
        win.dex <- sapply(1:nrow(my.contribs), FUN = function(x) {
                             sum(my.contribs[x]) } )
    }
        # Either way, max or sum, we want to be above the target level.
        winners <- which(win.dex >= level)
        win.count <- length(winners)
        win.possible <- nrow(my.contribs)
        win.pct <- win.count / win.possible

        # And we'll want to know which is more "in" the cluster than what.
        my.sort <- order(win.dex, decreasing = T)[seq_len(win.count)]

        # Get a list of pub.numbers that strongly represent this cluster
        win.pubs <- pubs[winners]
        win.pubs.sorted <- pubs[my.sort]

        win.pubs.level <- win.dex[my.sort]
        
        message(paste0("The number of dissertations made up of at least ",
                      level*100, " percent of words from this cluster ",
                      "of topics (", my.topics_name, ", cumulative=",
                      cumulative, ") is ", win.count, " of ",
                      win.possible, ", or ",
                      round(win.pct * 100, 2), "% of the corpus."))

        invisible(list("grid" = my.contribs,
                    "count" = win.count,
                    "percentage" = win.pct,
                    "docs" = win.pubs.sorted,
                    "doc_levels" = win.pubs.level,
                    "name" = my.topics_name,
                    "topics" = my.topics))

} # end of cluster_strength()


## Get ranks by topic reach over all (individual) non-bad topics
#  (Find extent_level from mystats using find.doc.by.topic.proportion() in `variation of topic proportions.R`;
#   mystats$Lhinge[1:5] means the 25th percentile for the top five topic-ranks)
topic.reach <- function(my.topic = NULL, # one or several numbers. if NULL, uses 1:ntopics.
                        dataset_name = "noexcludes2001_2015",
                        ntopics = 50,
                        iter_index = 1,
                        subset_name = "knownprograms2001_2015",
                        extent_level = c(0.05, 0.06, 0.09, 0.13, 0.22, 0.33, 0.4, 0.5, 0.66, 0.75),
                        use.labels = T,
                        labels = NULL,  # pass topic_labels if it exists    
                        grid = NULL,    # pass doc-topic matrix (usually called dt) if it exists
                        newnames = F)   # only used if grid is NULL
{
    require(data.table)
    topic_reach <- data.table()
    
    # make sure we have what we need
    if (is.null(extent_level)) {
        tryCatch(extent_level <- sort(round(mystats$Lhinge[1:5], 2)),
                 e = stop("get mystats from find.doc.by.topic.proportion()"),
                 finally = ""
        )
    }
    
    if (use.labels && is.null(labels)) {
        source(file = "get topic labels.R")
        topic_labels <- get_topic_labels(dataset_name = dataset_name,
                                         ntopics = ntopics,
                                         iter_index = iter_index,
                                         subset_name = subset_name,
                                         bad.topics = bad.topics,
                                         newnames = newnames)
    }
    
    if (is.null(grid)) {
        source(file = "get doctopic grid.R")
        grid <- get.doctopic.grid(dataset_name = dataset_name,
                                  ntopics = ntopics,
                                  iter_index = iter_index,
                                  subset_name= subset_name,
                                  newnames = newnames)
    }
    
    # okay, so what are we looking at? 
    if (is.null(my.topic)) {           # all topics
        my.topics <- setdiff(1:ntopics, bad.topics)
    } else {                           # one or more specified topics
        my.topics <- my.topic
    }
    
    # output holder
    topic_reach <- data.table()
    
    # main loop
    for (lvl in extent_level) {
        extent_all <- sapply(my.topics, FUN=function(x) {
            cluster.strength(my.topics = x,
                             dataset_name = dataset_name,
                             ntopics = ntopics,
                             iter_index = iter_index,
                             subset_name = subset_name,
                             bad.topics = bad.topics,
                             cumulative = FALSE,
                             level = lvl,
                             grid = dt,
                             use.labels = T,
                             labels = topic_labels)     # from get_topic_labels()
        })
        
        extent_all.vector <- round(100*unlist(extent_all["percentage", ]), 2)
        extent_all["grid", ]
        colname <- quote(paste0("extent_all", lvl*100))
        topic_reach[, eval(colname)] <- extent_all.vector    
        topic_reach$label <- unlist(extent_all["name", ])
        topic_reach$topic <- unlist(extent_all["topics", ])
    }
    
    topic_reach <- topic_reach[order(topic_reach[, 1], decreasing = T)]
    setcolorder(topic_reach, c("topic", "label"))
    
    if(remake_figs) {
        whatitis <- trimws(paste("Topic Reach", paste(my.topics, collapse="-")))
        outfile_slug <- build_plot_title(dataset_name = dataset_name,
                                         ntopics = ntopics,
                                         iter_index = iter_index,
                                         subset_name = subset_name,
                                         bad.topics = bad.topics,
                                         whatitis = whatitis,
                                         for.filename = TRUE)
        outfile <- file.path(imageloc, paste0(outfile_slug, ".csv"))
        write.csv(topic_reach, outfile, row.names = F)
    }
    
    return(topic_reach)
}



# testing zone
if(FALSE) {
    # cluster.strength("Teaching.of.Writing")
    # cluster.strength("Theories.of.Meaning.Making")
    # cluster.strength("Audience.and.Context")
    # cluster.strength("Performative.Identities")
    # cluster.strength("Politics.and.Power")
    # cluster.strength("Other")

    # all clusters together, more stringent test
    sapply(all_clusters, FUN=function(x) cluster.strength(x, level=0.25, subset=realconsorts$Pub.number))


    # cluster.strength("Teaching.of.Writing.1")
    # cluster.strength("WPA")

    # both together now, more stringent test
    sapply(c("Teaching.of.Writing.1", "WPA"), FUN=function(x) {
         cluster.strength(x, level=0.25) }
    )

    # Test subsetting function by using individual schools
    cuny.pubs <- realconsorts[which(realconsorts$School=="CUNY Graduate School and University Center"), "Pub.number"]
    pitt.pubs <- realconsorts2001_2015[which(realconsorts2001_2015$School=="University of Pittsburgh-Pittsburgh Campus"), "Pub.number"]
    cluster.strength("Teaching.of.Writing", subset_pubs = cuny.pubs, cumulative=F)
    cluster.strength(my.topics_name="Teaching.of.Writing",
                     dataset_name=dataset_name,
                     ntopics=ntopics,
                     iter_index=iter_index,
                     bad.topics=bad.topics,
                     subset_pubs = realconsorts2001_2015$Pub.number,
                     cumulative=F)

    # my.topics <- c(6, 11, 18, 42, 49, 35, 41, 37, 27) # smaller diana teaching cluster
    # my.topics <- c(4, 20, 5, 17, 23, 10) # social justice / identities cluster


    # pick one
    subset_name = "knownprograms2001_2015"
    subset_name = "nonrcws2001_2015sans_badtops"
    
    cluster_list <- name_topic_clusters(dataset_name = dataset_name,
                                        ntopics = ntopics,
                                        iter_index = iter_index,
                                        subset_name = subset_name)
    
    

    if(!exists("stretch", mode="function")) {
        source(file="squish_numbers.R")
    }

#TO DO: Wrap this in a convenient function
    

    # recover scoped variables from name_topic_clusters. choose which subset:
    cluster_list <- cluster_list[(cluster_list$name != "all_clusters"), ]
    # or
    cluster_list <- data.table(cluster_list, key="name")
    cluster_list <- cluster_list[all_clusters_used]
    cluster_list <- data.frame(cluster_list)
    
    for (i in 1:nrow(cluster_list)) {
        assign(cluster_list[i, "name"], stretch(cluster_list[i, "topics"]))
    }
    
    extent_level <- c(0.05, 0.06, 0.09, 0.13, 0.22, 0.33, 0.4, 0.5, 0.66, 0.75)
    # subset_name <- "nonrcws2001_2015sans_badtops"
    subset_name <- "knownprograms2001_2015"
    
    for (lvl in extent_level) {
        extent <- sapply(seq_len(nrow(cluster_list)), FUN=function(x) {
            cluster.strength(my.topics_name = cluster_list[x, "name"],
                             dataset_name = dataset_name,
                             ntopics = ntopics,
                             iter_index = iter_index,
                             subset_name = subset_name,
                             bad.topics = bad.topics,
                             cumulative = TRUE,
                             level = lvl,
                             grid = dt)
        })

        extent.vector <- round(100*unlist(extent["percentage", ]), 2)
        colname <- quote(paste0("extent", lvl*100))
        cluster_list[, eval(colname)] <- extent.vector
    }

    cluster_list <- cluster_list[order(cluster_list[,"extent5"], decreasing = T), ]
    
    if (remake_figs) {
        if( !exists("build_plot_title")) {
            source(file = "build_plot_title.R")
        }
        outfile_slug <- build_plot_title(dataset_name = dataset_name,
                                    ntopics = ntopics,
                                    iter_index = iter_index,
                                    subset_name = subset_name,
                                    bad.topics = NULL,
                                    whatitis = "cluster reach",
                                    for.filename = T)
        outfile <- file.path(imageloc, paste0(outfile_slug, ".csv"))
        i <- 1
        while (file.exists(outfile)) {
            outfile <- file.path(imageloc, paste0(outfile_slug, "_", i, ".csv"))
            i <- i + 1
        } 
        write.csv(cluster_list, outfile, row.names = F)                     
        
    } # end of if(remake_figs)



    if(! exists("top_topic_browser", mode="function")) {
        source(file="top docs per topic.R")
    }

    for (cluster in cluster_list$name) {
        # cluster <- "small.teaching"
        # cluster <- "justice"
        mydocs <- cluster.strength(my.topics_name = cluster,
                     dataset_name = dataset_name,
                     ntopics = ntopics,
                     iter_index = iter_index,
                     bad.topics = bad.topics,
                     subset_name = "knownprograms2001_2015",
                     cumulative = T,
                     level = 0.22)$docs

        top_topic_browser(dataset_name = dataset_name,
                          ntopics = ntopics,
                          iter_index = iter_index,
                          subset_name = "mydocs",
                          showlabels = T,
                          depth = 10)

        inversedocs <- setdiff(get(subset_name)$Pub.number, mydocs)


        top_topic_browser(dataset_name = dataset_name,
                          ntopics = ntopics,
                          iter_index = iter_index,
                          subset_name = "inversedocs",
                          showlabels = T,
                          depth = 10)



        }



    # TO DO: make a scatter plot with X-axis = level and Y-axis = cumulative
    # cluster strength, and a dataseries for each cluster (all on the same
    # graph)
    

    cluster.strength(my.topics = c(7, 43),
                     my.topics_name = "entertainment",
                     bad.topics = bad.topics,
                     level = 0.05,
                     subset_name = subset_name)
    
    
    

}
