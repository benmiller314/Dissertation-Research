# find topics by word.R
#
# GOAL: enter a search term, and find the topics for which
#       that word has the maximum probability.
#
# PLAN: load the topic-word grid and grep; return ranked list of topics

topic_by_word <- function(

    ## Name the model ##
    dataset_name = "noexcludes2001_2015",
    ntopics = 50,
    iter_index = 1,
    newnames = F,   # where in the MALLET output filename does iter_index appear?
                    # set T if it's with the model, F if last in filename.
    
    ## Narrow within it? Set either to NULL if not using.
    subset_name = "knownprograms2001_2015",          
    bad.topics = c(3, 12, 50, 47, 34, 36, 30, 8, 15),
    
    ## In topic results, should we include (human created) topic names?
    #  If false, defaults to top words.
    use.labels = TRUE,
    
    ## How many topics to show? Set less than 1 to show all.
    howmany = 10,
    
    ## If it exists, pass the topic-word matrix to save (a lot of) time
    tw = NULL

){
    
    ############# Bind the Data #############
    
    # Topic-word tables
    if (is.null(tw)) {
        source(file="get_topic_word_grid.R")
        tw <- build.topicword.table(dataset_name=dataset_name,
                                    ntopics=ntopics,
                                    iter_index=iter_index,
                                    newnames=newnames,
                                    bad.topics=bad.topics)
    }
    
}

