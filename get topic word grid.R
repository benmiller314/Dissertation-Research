# Get word/topic grid

just.value <- function(key.value) {
    as.integer(strsplit(as.character(key.value), ":")[[1]][2])
}

get.wordtopic.grid <- function(dataset_name="noexcludes2001_2015", 
                               ntopics=50, 
                               iter_index=1)
{
    require(data.table)
    filename <- paste0(dataset_name, "k", ntopics, "_wordtopics_", iter_index, ".txt")
    
    # oneline <- readLines(file.path(tmloc, filename), n=1)
    dt <- read.table(file.path(tmloc, filename), header=FALSE, fill=TRUE,
                col.names=c("index", "token", paste0("TopicRanked", 1:ntopics))
    )
    dt <- data.table(dt)
    
    
    return(dt)
}

# The format of the wordtopic dt is given by col.names, above: each row is a token,
# and columns after the first two are key/value pairs, consisting of
# a *zero-indexed* topic number, a colon, and a probability score / weight. 
# Not sure how probability is represented; I think it's actually raw counts?
# see https://stackoverflow.com/questions/33251703/how-to-get-a-probability-distribution-for-a-topic-in-mallet#comment69702638_33251703)



# Make grid smaller by trimming rows 
# for which the top-ranked topic is really low weight

trim.wordtopic.grid <- function(dt,            # wordtopic grid, as above
                                threshold=5)   # minimum word-weight in top-ranked topic
{
    topword_values <- sapply(dt[[column]], just.value)
    cutindex <- which(topword_values < threshold)
    return(dt[-cutindex])
}

# To compare topics to one another, we want to reshape this so as to 
# index by topic.                 

find.topic.in.one.col <- function(topic,     # search by topic number
                                  rank.col,  # name columns by topic.ranked.X
                                  dt)        # a wordtopic grid, as above
{
    colname <- paste0("TopicRanked", rank.col)
    my_expr <- paste0("^", topic-1, ":")    # note the offset (convert 0-index to 1-index)
    index <- grep(my_expr, dt[[colname]])
    key.value.pairs <- dt[index, ..colname]
    
    values <- sapply(key.value.pairs[[1]], just.value)
    
    data.table(token_ind=as.integer(index), weight=as.integer(values))
}


find.topic.in.all.cols <- function(topic,
                                   ntopics=50,
                                   dt,             # a wordtopic grid, as above
                                   threshold=5)    # minimum weight per token
{
    topic_word_vec <- data.table(token_ind=numeric(), weight=numeric())
    for (column in seq_len(ntopics)) {
        topic_word_vec <- rbindlist(list(topic_word_vec,
                                         find.topic.in.one.col(topic, column, dt)
                                        )
                                   )
    }
    
    # trim
    topic_word_vec <- topic_word_vec[weight >= threshold]
    
    # index and sort by token
    setkey(topic_word_vec, token_ind)
    
    # add useful info
    topic_word_vec[, token:=dt[topic_word_vec$token_ind, token]]
    topic_word_vec[, probability:=weight/sum(weight)]
    
    return(topic_word_vec)
}

build.topicword.grid <- function(dt=NULL,    # if it exists, pass it for speed boost
                                 per.topic.threshold=300,
                                 top.topic.threshold=5,
                                 dataset_name="noexcludes2001_2015",
                                 ntopics=50,
                                 iter_index=1)
{
    if(is.null(dt)) {
        dt <- get.wordtopic.grid(dataset_name, ntopics, iter_index)
        dt <- trim.wordtopic.grid(dt, top.topic.threshold)
    }
    
    tw <- data.table(topic=numeric(),
                     token_ind=numeric(),
                     weight=numeric(),
                     token=character(),
                     probability=numeric())
    
    for (i in seq_len(ntopics)) {
       tryCatch(
           expr = {   
               message("Building word vector for topic ", i, "...")
               tw_i <- find.topic.in.all.cols(topic=i,
                                          ntopics=ntopics,
                                          dt=dt,
                                          threshold=per.topic.threshold)
    
               tw_i[,topic:=i]
               tw <- rbindlist(list(tw, tw_i), use.names=TRUE)
           },
           error = function(e) { e },
           finally = message("done.")
       )
    }
    
    return(tw)
}


if(FALSE) { #test
    dt <- get.wordtopic.grid()      # 1,616,842 tokens
    dt <- trim.wordtopic.grid(dt)   #   254,092 tokens
    topic=1    
    rank.col=1
    t1_words <- find.topic.in.all.cols(topic=1, ntopics=50, dt)
    t1_words[order(-probability)]
    t4_words <- find.topic.in.all.cols(topic=11, ntopics=50, dt=dt, threshold=300)
    
    # put it all together
    tw <- build.topicword.grid(dt, ntopics=50)
    
    # to extract one topic/word vector (for inspection, comparison, distance, etc):
    tw[topic==1]
    tw[topic==1][order(-probability)]
    head(tw[topic==1][order(-probability)][, token], 20)
    
}
