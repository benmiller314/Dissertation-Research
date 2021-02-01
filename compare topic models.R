# INCOMPLETE!

# GOAL: given two iterations of a topic model, suggest probable pairs of topics that seem the same.
# 
# PLAN: use tf-itf to get more topic-specific topwords;
#       for a given topic from iter_a, use match_lists() from topic_term_synonyms.R to find the most similar lists in iter_b
#       loop over topics

require(data.table)
require(wordnet)        # NB: if this crashes R, try updating Java: 
                        # https://stackoverflow.com/questions/26252591/mac-os-x-and-multiple-java-versions#answer-47699905


# Get the machine oriented
if (!exists("tmloc", mode="character")) {
    source(file="dataprep.R")
}


if(FALSE) {     
    # test values
    dataset_name <- "noexcludes2001_2015"
    subset_name <- NULL
    ntopics <- 50
    iter_index_list <- c(1, 3)
}

# Let's start by assuming everything's the same except iter_index, since that seems easier
compare_two_topic_models <- function(iter_index_list = c(1, 3), 
                                 dataset_name = "noexcludes2001_2015",
                                 subset_name = NULL,
                                 ntopics = 50
                                 )
{
    # Load dependencies
    if (!exists("tfidf.for.topics", mode="function")) {
        source(file="tfidf for topics.R")
    }
    
    if (!exists("build.topicword.table", mode="function")) {
        source(file="get_topic_word_grid.R")
    }
    
    if (!exists("match_lists", mode="function")) {
        source(file="topic_term_synonyms.R")
    }
    
    for (i in iter_index_list) {
        # save topic-word tables for later use
        assign(paste0("tw_", i), build.topicword.table(dataset_name=dataset_name,
                                                       ntopics=ntopics,
                                                       iter_index=i)
               )

        # get topwords
        assign(paste0("tf_", i), tfidf.for.topics(dataset_name=dataset_name,
                                                  ntopics=ntopics,
                                                  iter_index=i,
                                                  tw=get(paste0("tw_", i))
                                                  )
        )
        
                
        # get topic-word probability vectors
        assign(paste0("tw_", i, ".grid"), topicword.probability.grid(get(paste0("tw_", i))))
        
        # and mark them 
        
        # str(tw_1.grid)
    }    
    
    topwords <- cbind(sort(tf_1$topN$by_tfitf),
                      sort(tf_3$topN$by_tfitf)
                      )
    topwords[1:3,]
        # merge
        tw.grid.combined <- merge(tw_1.grid, tw_3.grid)
        
        topic_distance_matrix(tw.grid = tw.grid.combined)
        

        
        # cluster
        assign(paste0("twm_", i), topic_distance_matrix(dataset_name=dataset_name,
                                                        ntopics=ntopics,
                                                        iter_index=i,
                                                        tw=get(paste0("tw_", i))
                                                        )
        )
        
        assign(paste0("ag_", i), topic_clusters(dataset_name=dataset_name,
                                                ntopics=ntopics,
                                                iter_index=i,
                                                twm=get(paste0("twm_", i))
                                                )
               )
        
    
    
    
        
    comparison <- two_topic_synonyms(listA = get(mylists[1]), listB = get(mylists[2]))

}
