#############################################################################
# topic cluster reach.R
#
# GOAL: given a cluster of topics identified through frameToD3.R, find out
# how many dissertations include at least one topic in that cluster at a
# level of over 12% (or whatever).
#####


## define clusters of topics
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

    
    
## main wrapper function
cluster.strength <- function (my.topics_name, # an aliased list of topic numbers; see below
                            dataset_name = "consorts", 
                            ntopics      = 55,
                            bad.topics   = NULL,
                            level        = 0.12,
                            cumulative   = TRUE, # can several topics add up to meet that level?
                            subset_pubs  = NULL) 
{
    # Exclude non-content-bearing topics
    if(is.null(bad.topics) && dataset_name == "consorts" && ntopics == 55) {
        bad.topics <- c("4", "47", "22", "2", "24", 
                        # bad OCR or ProQuest boilerplate
                    "13", "50")
                    # language markers (Italian, Spanish)
    }
    
    my.topics <- get(my.topics_name)
                    
    if(any(my.topics %in% bad.topics)) { 
        warning(paste("At least one topic in your list has been",
                      "identified as non-content-bearing")) 
    }
    if(!exists("get.doctopic.grid", mode="function")) { 
        source("get doctopic grid.R") 
    }
    grid <- data.table(get.doctopic.grid(dataset_name, ntopics)$outputfile)
    # str(grid)
    # head(grid)

    # TO DO: Enable analysis on a subset of this grid, e.g. for one school.
    # Strategy: 
    # (1) add a parameter that's a list of Pub.numbers to include
    # (2) subset the rows of the doc-topic grid by using that list
    if(!is.null(subset_pubs)) {
        grid <- grid[(grid$Pub.number %in% subset_pubs), ]
    }
    
    grid <- grid[, !(names(grid) %in% c(bad.topics, "Pub.number")), with=F]
    # head(grid)
    
    my.contribs <- grid[, names(grid) %in% my.topics, with=F]

    if(!cumulative) {
        # If `cumulative` is false, at least one individual topic in the cluster must
        # be represented at higher than the minimum level set by `level` for a dissertation
        # to be counted within this cluster's reach.
        individuals <- sapply(1:nrow(my.contribs), FUN = function(x) {
                             any(my.contribs[x] >= level) } )
        winners <- which(individuals > level)
        
    } else {
        # Otherwise, check whether the combined contributions from several topics
        # within the cluster add up to the minimum level or beyond.
        totals <- sapply(1:nrow(my.contribs), FUN = function(x) {
                             sum(my.contribs[x]) } )
        winners <- which(totals > level)
    }   
        win.count <- length(winners)
        win.pct <- win.count / nrow(my.contribs)
    
        message(paste0("The number of dissertations made up of at least ",
                      level*100, " percent of words from this cluster ",
                      "of topics (", my.topics_name, ", cumulative=",
                      cumulative, ") is ", win.count, " of ",
                      nrow(my.contribs), ", or ", 
                      round(win.pct * 100, 2), "% of the corpus."))
        
        invisible(list("number" = win.count,
                    "percentage" = win.pct))
        
        # TO DO: Somehow make return values chainable, I don't know 
}

if(autorun) {
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
    pitt.pubs <- realconsorts[which(realconsorts$School=="University of Pittsburgh-Pittsburgh Campus"), "Pub.number"]
    cluster.strength("Teaching.of.Writing", subset_pubs = cuny.pubs, cumulative=F)
    cluster.strength("Teaching.of.Writing", subset_pubs = realconsorts$Pub.number, cumulative=T)
    
    # TO DO: make a scatter plot with X-axis = level and Y-axis = cumulative
    # cluster strength, and a dataseries for each cluster (all on the same
    # graph)
}


