# GOAL: From MALLET's output of words and topics,
# assemble a matrix of topic-word probability vectors

# The code below assumes the following:
# 1. tmloc (character): a file.path to a directory of topic modeling files
# 2. MALLET output files are in that directory, named by dataset, number of topics,
#    and a numeric indicator of the particular iteration of the model
#    for that dataset and ntopics. See lines 20-21 for what I mean.
# 3. When running MALLET, you used the --word-topic-counts-file option
#    My MALLET settings are in the file "r2mallet with foreach.R"



# Load the MALLET output file
get.wordtopic.grid <- function(dataset_name = "noexcludes2001_2015",
                               ntopics = 50,
                               iter_index = 1,
                               newnames = F        # where in the MALLET output filename does iter_index appear?
                                                   # set T if it's with the model, F if last in filename.
{
    require(data.table)

    if(newnames) {
         filename <- paste0(dataset_name, "k", ntopics, "_iter", iter_index, "wordtopics.txt")
    } else {
         filename <- paste0(dataset_name, "k", ntopics, "_wordtopics_", iter_index, ".txt")
    }
    filename <- file.path(tmloc, filename)

    if(file.exists(filename)) {
        # oneline <- readLines(file.path(tmloc, filename), n=1)
        wt <- read.table(filename, header=FALSE, fill=TRUE,
                    col.names=c("index", "token", paste0("TopicRanked", 1:ntopics))
        )
        wt <- data.table(wt)
    } else {
        stop("'get topic word grid.R': could not load word-topic pairs from file ",
             filename)
    }

    return(wt)
}

# The format of the wordtopic wt is given by col.names, above: each row is a token,
# and columns after the first two are key/value pairs, consisting of
# a *zero-indexed* topic number, a colon, and a probability score / weight.
# Not 100% sure how probability is represented; I think it's actually raw counts?
# see https://stackoverflow.com/questions/33251703/how-to-get-a-probability-distribution-for-a-topic-in-mallet#comment69702638_33251703)


# extract weight value from a topic:weight pair (MALLET's format)
just.value <- function(key.value) {
    as.integer(strsplit(as.character(key.value), ":")[[1]][2])
}


# Make grid smaller by trimming rows
# for which the top-ranked topic is really low weight
trim.wordtopic.grid <- function(wt,            # wordtopic grid, as above
                                threshold=5)   # minimum word-weight in top-ranked topic
{
    topword_values <- sapply(wt[["TopicRanked1"]], just.value)
    cutindex <- which(topword_values < threshold)
    return(wt[-cutindex])
}


# To compare topics to one another, we want to reshape this so as to
# index by topic. We'll start by assembling one topic vector, then compile.
find.topic.in.one.col <- function(topic,     # search by topic number
                                  rank.col,  # name columns by topic.ranked.X
                                  wt)        # a wordtopic grid, as above
{
    colname <- paste0("TopicRanked", rank.col)
    my_expr <- paste0("^", topic-1, ":")    # note the offset (convert 0-index to 1-index)
    index <- grep(my_expr, wt[[colname]])
    key.value.pairs <- wt[index, ..colname] # this syntax requires data.table 1.10.2 or higher;
                                            # see https://github.com/Rdatatable/data.table/blob/master/NEWS.md#changes-in-v1102--31-jan-2017

    values <- sapply(key.value.pairs[[1]], just.value)

    data.table(token_ind=as.integer(index), weight=as.integer(values))
}


find.topic.in.all.cols <- function(topic,
                                   ntopics=50,
                                   wt,             # a wordtopic grid, as above
                                   threshold=5)    # minimum weight per token
{
    topic_word_vec <- data.table(token_ind=numeric(), weight=numeric())
    for (column in seq_len(ntopics)) {
        topic_word_vec <- rbindlist(list(topic_word_vec,
                                         find.topic.in.one.col(topic, column, wt)
                                        )
                                   )
    }

    # trim
    topic_word_vec <- topic_word_vec[weight >= threshold]

    # index and sort by token
    setkey(topic_word_vec, token_ind)

    # add useful info
    topic_word_vec[, token:=wt[topic_word_vec$token_ind, token]]
    topic_word_vec[, probability:=weight/sum(weight)]

    return(topic_word_vec)
}

# Main function of this file; it'll call the others as needed. Returns a data.table
# that you can filter on to view tokens and probabilities for each topic.
# Other functions, including those for TF-ITF, will require its output.
# NB you should be able to re-create the top N "keys" for each topic from here
# (though I forgot to grab the alpha for each topic...).
build.topicword.table <- function(wt=NULL,    # if it exists, pass it for speed boost
                                 per.topic.threshold=300,
                                 top.topic.threshold=5,
                                 dataset_name="noexcludes2001_2015",
                                 ntopics=50,
                                 iter_index=1,
                                 newnames = F,        # where in the MALLET output filename does iter_index appear?
                                                    # set T if it's with the model, F if last in filename.
                                 bad.topics=NULL) # optional sequence of (1-indexed)
                                                  # topic numbers to leave out (bad OCR, etc)
{
    if(is.null(wt)) {
        wt <- get.wordtopic.grid(dataset_name=dataset_name, ntopics=ntopics,
                              iter_index=iter_index, newnames=newnames)
        wt <- trim.wordtopic.grid(wt, top.topic.threshold)
    }

    tw <- data.table(topic=numeric(),
                     token_ind=numeric(),
                     weight=numeric(),
                     token=character(),
                     probability=numeric())


    topic_list <- setdiff(seq_len(ntopics), bad.topics) # still works if bad.topics is NULL

    for (i in topic_list) {
       tryCatch(
           expr = {
               message("Building word vector for topic ", i, "...");
               tw_i <- find.topic.in.all.cols(topic=i,
                                          ntopics=ntopics,
                                          wt=wt,
                                          threshold=per.topic.threshold);
               tw_i[,topic:=i];
               tw <- rbindlist(list(tw, tw_i), use.names=TRUE)
           },
           error = function(e) { e },
           finally = message("done.")
       )
    }

    return(tw)
}

# But sometimes you really just want the probabilities,
# not the tokens they correspond to.
topicword.probability.grid <- function(tw)         # topicword.table as built above
{
    topic_list <- unique(tw$topic)

    # start empty, with a row for each included token
    tw.grid <- data.table(token_ind=as.numeric(levels(factor(tw$token_ind))))

    # and add one column for each topic
    for (i in topic_list) {
        tw.grid <- merge(tw.grid,
                         tw[topic==i, .(token_ind, i=probability)],
                         by="token_ind",
                         all=TRUE)                 # merge all=T introduces NA's;
                                                   # we'll deal with them later
        setnames(tw.grid, "i", as.character(i))
    }

    # replace NA's with vanishingly small (but non-zero) value
    for (column in names(tw.grid)) {
        set(tw.grid, which(is.na(tw.grid[[column]])), column, 1e-20)

        # # and normalize so it still adds up to 1
        # col_sum <- sum(tw.grid[[column]])
        # set(tw.grid, i=NULL, j=column, value=(tw.grid[[column]]/col_sum)
        # confirm that we're still adding up essentially to 1
        if (sum(tw.grid[[column]]) != 1 & column != "token_ind") {
            warning("Sum of probabilities for column ", column, " is ", sum(tw.grid[[column]]))
        }
    }

    # drop token_ind, so the matrix is just a set of probability vectors
    # (so we can calculate distances). Weirdly, we don't have to re-assign tw.grid,
    # because we're assigning by reference.
    tw.grid[, token_ind:=NULL]

    return(tw.grid)
}

one.tw.probability.vector <- function(mytopic,     # just a number in 1:ntopics
                                      dataset_name="noexcludes2001_2015",
                                      ntopics=50,
                                      iter_index=1,
                                      tw=NULL)     # if we have a topic-word matrix,
                                                   # don't waste time rebuilding it
{
    # if we don't have a topic-word matrix, let's get one
    if(is.null(tw)) {
        # Get the topic-word info
        if(!exists("build.topicword.table", mode="function")) {
            source(file="get topic word grid.R")
        }

        tw <- build.topicword.table(dataset_name=dataset_name,
                                   ntopics=ntopics,
                                   iter_index=iter_index)
    }

    # pull out just the topic we need
    tw_a <- tw[topic==mytopic][order(-probability)]     # make sure it looks okay
    setkey(tw_a, token_ind)
    vector_a <- tw_a$probability
    names(vector_a) <- tw_a$token_ind                   # prep for bind by name

    return(vector_a)
}


# Testing space / demo
if(FALSE) {
    wt <- get.wordtopic.grid()      # 1,616,842 tokens
    wt <- trim.wordtopic.grid(wt)   #   254,092 tokens
    topic=1
    rank.col=1
    t1_words <- find.topic.in.all.cols(topic=1, ntopics=50, wt)
    t1_words[order(-probability)]
    t4_words <- find.topic.in.all.cols(topic=11, ntopics=50, wt=wt, threshold=300)

    # put it all together
    tw <- build.topicword.table(wt=wt)

    # to extract one topic/word vector (for inspection, comparison, distance, etc):
    tw[topic==1]
    tw[topic==1][order(-probability)]
    head(tw[topic==1][order(-probability)][, token], 20)

}
