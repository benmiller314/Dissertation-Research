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
    
    if(dataset_name=="noexcludes2001_2015" && ntopics==50 && iter_index==1 && subset_name=="realconsorts2001_2015") {
        Teaching.of.Writing <- c(18, 37, 6, 42, 16, 26, 41, 49, 28, 32, 35)
        
    }
}
    
## main wrapper function
cluster.strength <- function (my.topics=NULL,      # either pass in a list of topic numbers, or
                            my.topics_name=NULL,   # a string alias for a list of topic numbers; see above
                            dataset_name = "noexcludes2001_2015", 
                            ntopics      = 50,
                            iter_index   = "1",
                            bad.topics   = NULL,
                            level        = 0.12,
                            cumulative   = TRUE, # can several topics add up to meet that level?
                            subset_name  = NULL, # a named subset of data, either all columns or just Pub.numbers. 
                            grid = NULL)         # a doc-topic grid. if it exists, offers a speed boost.
{
    # Which topics are in the cluster?
    if(is.null(my.topics)) {
        if(is.null(my.topics_name)) {
            stop("cluster.strength() requires you to specify a cluster by passing either my.topics or my.topics_name.")
        } else {
            my.topics <- with(name_topic_clusters(dataset_name, ntopics, iter_index, subset_name),
                              get(my.topics_name))
        }
    } else if(is.null(my.topics_name)) {
        my.topics_name <- paste("topics", paste(my.topics, collapse=", "))
    }
    
    # if we don't yet have a doctopic grid, get one
    if(is.null(grid)) {
        if(!exists("get.doctopic.grid", mode="function")) { 
            source("get doctopic grid.R") 
        }
        grid <- get.doctopic.grid(dataset_name=dataset_name, ntopics=ntopics,
                              iter_index=iter_index, subset_name=subset_name)$outputfile.dt
    } else if (!is.null(subset_name)){
        warning("cluster.strength: Using an existing doc-topic grid, but subset_name is not null. \n",
                "Check that the grid parameter being passed has the correct number of rows.")
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
    
    grid <- grid[, setdiff(names(grid), c(bad.topics, "Pub.number")), with=F]
    # head(grid)
    
    if(any(my.topics %in% bad.topics)) { 
        warning(paste("At least one topic in your list has been",
                      "identified as non-content-bearing")) 
    }
    
    
    # Narrow our doctopic grid to the topics in question
    my.contribs <- grid[, names(grid) %in% my.topics, with=F]

    if(!cumulative) {
        # If `cumulative` is false, at least one individual topic in the cluster must
        # be represented at higher than the minimum level set by `level` for a dissertation
        # to be counted within this cluster's reach.
        win.dex <- sapply(1:nrow(my.contribs), FUN = function(x) {
                             max(my.contribs[x]) } )
    } else {
        # Otherwise, check whether the combined contributions from several topics
        # within the cluster add up to the minimum level or beyond.
        win.dex <- sapply(1:nrow(my.contribs), FUN = function(x) {
                             sum(my.contribs[x]) } )
    }
        # Either way, max or sum, we want to be above the target level.
        winners <- which(win.dex >= level)
        win.count <- length(winners)
        win.pct <- win.count / nrow(my.contribs)
        
        # And we'll want to know which is more "in" the cluster than what.
        my.sort <- order(win.dex, decreasing = T)[seq_len(win.count)]
        
        # Get a list of pub.numbers that strongly represent this cluster
        win.pubs <- pubs[winners]
        win.pubs.sorted <- pubs[my.sort]
    
        
    
        message(paste0("The number of dissertations made up of at least ",
                      level*100, " percent of words from this cluster ",
                      "of topics (", my.topics_name, ", cumulative=",
                      cumulative, ") is ", win.count, " of ",
                      nrow(my.contribs), ", or ", 
                      round(win.pct * 100, 2), "% of the corpus."))
        
        invisible(list("grid" = my.contribs,
                    "number" = win.count,
                    "percentage" = win.pct,
                    "docs" = win.pubs.sorted))
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
    
    clust6.ind <- cluster.strength(my.topics = my.topics,
                     dataset_name = dataset_name,
                     ntopics = ntopics,
                     iter_index = iter_index,
                     bad.topics = bad.topics,
                     subset_name = "knownprograms2001_2015",
                     cumulative = T)$docs
    
    if(! exists("top_topic_browser", mode="function")) {
        source(file="top docs per topic.R")
    }
    
    top_topic_browser(dataset_name = dataset_name,
                      ntopics = ntopics,
                      iter_index = iter_index,
                      subset_name = "clust6.ind",
                      showlabels = T,
                      depth = 10)
    
    # TO DO: make a scatter plot with X-axis = level and Y-axis = cumulative
    # cluster strength, and a dataseries for each cluster (all on the same
    # graph)
}


