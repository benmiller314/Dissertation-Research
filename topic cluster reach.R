#############################################################################
# topic cluster reach.R
#
# GOAL: given a cluster of topics identified through frameToD3.R, find out
# how many dissertations include at least one topic in that cluster at a
# level of over 12% (or whatever).
#####


# get all topics by document
cluster.strength <- function (my.topics_name, 
                            dataset_name = "consorts", 
                            ntopics      = 55,
                            bad.topics   = NULL,
                            level        = 0.12,
                            cumulative   = TRUE
                            ) 
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

    grid <- grid[, !(names(grid) %in% c(bad.topics, "Pub.number")), with=F]
    # head(grid)
    
    my.contribs <- grid[, names(grid) %in% my.topics, with=F]

    if(!cumulative) {
        individuals <- sapply(1:nrow(my.contribs), FUN = function(x) {
                             any(my.contribs[x] >= level) } )
        winners <- which(individuals > level)
        
    } else {
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
}

if(autorun) {
    # The Teaching of Writing
    Teaching.of.Writing <- c(1, 32, 30, 3, 9, 39, 41, 40, 45, 6, 25, 8)
    cluster.strength("Teaching.of.Writing")
    
    # Theories of Meaning-Making
    Theories.of.Meaning.Making <- c(21, 18, 48, 14, 26, 53, 31, 29)         
    cluster.strength("Theories.of.Meaning.Making")
    
    # Audience and Context for Composing
    Audience.and.Context <- c(35, 49, 55, 27, 43, 46, 44)
    cluster.strength("Audience.and.Context")    
    
    # Performative Identities, past and present
    Performative.Identities <- c(23, 10, 16, 33, 15, 11, 7, 37)
    cluster.strength("Performative.Identites")
    
    # Politics and Power
    Politics.and.Power <- c(36, 20, 28, 54, 17, 52)
    cluster.strength("Politics.and.Power")
    
    # other
    Other <- c(5, 12, 42, 38, 51, 34, 19)                                   
    cluster.strength("Other")
    
    # all together now, more stringent test
    cluster_names <- c("Teaching.of.Writing", "Theories.of.Meaning.Making",
                         "Audience.and.Context", "Performative.Identities",
                         "Politics.and.Power", "Other")
    sapply(cluster_names, FUN=function(x) cluster.strength(x, level=0.25))

    
    # The Teaching of Writing subcluster that's especially classroom-y
    Teaching.of.Writing.1 <- c(1, 32, 30, 3, 9, 39, 41, 40)     
    cluster.strength("Teaching.of.Writing.1")
    
    # The Teaching of Writing subcluster that's a little more administrative
    WPA <- c(45, 6, 25, 8)
    cluster.strength("WPA")

    # both together now, more stringent test    
    sapply(c("Teaching.of.Writing.1", "WPA"), FUN=function(x) {
         cluster.strength(x, level=0.25) } 
    )

    # TO DO: make a scatter plot with X-axis = level and Y-axis = cumulative
    # cluster strength, and a dataseries for each cluster (all on the same
    # graph)
}


