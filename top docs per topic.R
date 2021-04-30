#############################################################################
# top docs per topic.R
#
# Tools for topic exploration
#

# Provides five functions:
#         * get.topics4doc(pubnum, dataset_name, ntopics, howmany,
#           showlabels): retrieves top `howmany` topics for a document
#           specified by `pubnum`.
#         * top_topic_browser(...): for a specified topic or range of topics,
#           shows the top `howmany` documents and their method tags, with
#           optional detail view showing top topics for each document at a
#           time. See below for parameters. By default, uses an interactive mode,
#           but can also output basic info for a topic by using the for.bind option.
#         * shareable_topic(topic, ...): Given a topic of interest,
#           get clean data to share with others about the top "depth" docs
#           time. See below for parameters.
#         * top_titles_table(..., howmany, depth): for a specified dataset_name
#           ntopics, iter_index, and subset_name, show the top `depth` titles
#           for the top `howmany` topics, concatenated with the weight of that topic
#           in each doc.
#         * cluster_titles_table(cluster_members, cluster_name, ..., howmany, depth): 
#           like top_titles_table, but for a combined set of topics, treated cumulatively.
#   Legacy support:
#         * This used to define the function get.doc.composition(), but it ended up being a
#           fairly simple wrapper on get.doctopic.grid, so I've now factored it out.
#####


# Step 1. Get the matrix of texts and topics
if(!exists("get.doctopic.grid", mode="function")) {
    source(file="get doctopic grid.R")
}

# 2. Oh, and what were those topics, again?
if(!exists("get.topickeys", mode="function")) {
    source(file="get topickeys.R")
}

# Step 3. Find top 5 docs for each overall top topic
# to get a sense of what's "real" and what's "interesting"

    # Step 4. Find all the top-ranked topics for those docs: maybe that
    # really popular topic isn't actually the main component of the docs that
    # come up.



### Helper function: retrieve top five topics for a given Pub.number
get.topics4doc <- function(pubnum,
                           dataset_name = "noexcludes2001_2015",
                           ntopics = 50,
                           subset_name = "knownprograms2001_2015",
                           iter_index = 1,
                           newnames=F,         # where in the MALLET output filename does iter_index appear?
                                               # set T if it's with the model, F if last in filename.
                                               # Gets passed into get.doctopic.grid.
                           howmany = 5,
                           showlabels = FALSE,
                           topic_labels = NULL,
                           columns = c("Title", "Pub.number", tagnames, "realconsort", "realrhetmap"),
                           grid = NULL,         # if it exists, pass in for a speed boost
                           verbose = F)
{
    # get packages in case we've just restarted R
    require(data.table)

    dataset.dt <- data.table(get(dataset_name), key="Pub.number")

    # pubnum <- "3051708"; doc_tops <- doc_topics.dt    # test values

    if (!is.character(pubnum)) {
        pubnum <- as.character(pubnum)
    }

    # start with the doc/topic grid
    if (is.null(grid)) {
        doc_tops <- get.doctopic.grid(dataset_name=dataset_name, ntopics=ntopics,
                                  iter_index=iter_index, newnames=newnames)$outputfile.dt
    } else {
        doc_tops <- grid
        doc_tops <- na.omit(doc_tops)
    }

    # narrow to the doc named by the pubnum; skip the pubnum for sorting by descending weight
    this_doc_tops <- doc_tops[pubnum, -"Pub.number"]
    this_doc_tops <- this_doc_tops[, order(this_doc_tops, decreasing = T), with=F][,1:howmany, with=F]

    if(showlabels) {
        if(is.null(topic_labels)) {
            if(!exists("get_topic_labels", mode="function")) {
                source(file="get topic labels.R")
            }
            topic_labels <- data.table(get_topic_labels(dataset_name = dataset_name,
                                                        ntopics = ntopics,
                                                        iter_index = iter_index,
                                                        subset_name = subset_name),
                                       key="Topic")
        }

        topic_keys <- topic_labels[as.numeric(names(this_doc_tops))]
        setnames(topic_keys, old = c("Topic", "Label","Pct.Contrib", "Top.Words"),
                             new = c("topic", "current_label", "weight", "top_words"))

        # reorder and simplify
        topic_keys <- topic_keys[, list(topic, weight, current_label, top_words)]

    } else {

        # merge weights with keys: from all keys, narrow to just the top howmany in this doc
        topic_keys <- data.table(get.topickeys(dataset_name = dataset_name,
                                               ntopics = ntopics,
                                               iter_index = iter_index,
                                               newnames = newnames))
        topic_keys <- topic_keys[as.numeric(names(this_doc_tops))]
        topic_keys[,weight:=as.numeric(this_doc_tops)]

        # reorder columns for ease of presentation
        topic_keys <- topic_keys[, list(topic, weight, alpha, top_words)]
    }

    to.return <- list("title" = dataset.dt[pubnum, ..columns],
        "keys" = topic_keys,
        "abstract" = dataset.dt[pubnum, c("KEYWORDS", "ABSTRACT"), with=F]
        )

    if(verbose) {
        print(to.return)
    }

    return(to.return)

# close helper function get.topics4doc
}

### Browse through the top topics and their top-proportioned dissertations
top_topic_browser <- function(
                              # where will our topic assignments come from?
                              dataset_name = "noexcludes2001_2015",
                              ntopics = 50,

                              # if we've run the params above many times, which one now?
                              iter_index = 1,

                              # do we want to show the full dataset, or a subset?
                              subset_name = NULL,

                              # where in the MALLET output filename does iter_index appear?
                              # set T if it's with the model, F if last in filename.
                              # Gets passed into get.doctopic.grid.
                              newnames = FALSE,

                              # assuming we're looping, start where?
                              start.rank = 1,

                              # alternately, browse one specified topic
                              topic = NULL,

                              # if lots of topics, where to stop?
                              cutoff = get("ntopics"),

                              # how many docs to show for each topic?
                              depth = 5,

                              # show current topic labels for indiv. docs?
                              showlabels = FALSE,

                              # invisibly return results and exit early?
                              for.bind = FALSE
                              )
{
    # get packages in case we've just restarted R
    require(data.table)



    # load the topic model data
    if(! exists("get.doctopic.grid", mode = "function")) {
        source(file=file.path(sourceloc, "get doctopic grid.R"))
    }


    grids <- get.doctopic.grid(dataset_name=dataset_name, ntopics=ntopics, subset_name=subset_name, iter_index=iter_index, newnames=newnames)
        colsums <- grids$colsums
        colsums.sort <- grids$colsums.sort
        outputfile <- grids$outputfile
        doc_topics.dt <- grids$outputfile.dt
    rm(grids)

    # get document data
    dataset.dt <- data.table(get(dataset_name), key="Pub.number")

    if(! exists("get.topickeys", mode = "function")) {
        source(file=file.path(sourceloc, "get topickeys.R"))
    }

    topic_keys.dt <- get.topickeys(dataset_name, ntopics, iter_index=iter_index, newnames=newnames)

    # List the keys for the top N topics, where N = cutoff
    len <- min(length(colsums)-1, cutoff)

    # list of topics by rank; skip Pub.num
    ind <- as.integer(names(colsums.sort)[2:(len+1)])

    # If we specified one topic, show just that topic and exit.
    if (! is.null(topic)) {
        if(length(topic) == 1) {
            topic.num <- topic
        } else if(length(topic > 1)) {
    # If we specified more than one, assume it's a cluster;
    # for now, use the top-ranked topic of the set,
    # but TO DO find a way to compile the sum and then work from there.
            topic.num <- topic[which.max(match(topic, names(colsums.sort)))]
            topic.num <- as.integer(topic.num)
            msg <- paste0("top_topic_browser: Multiple topics specified (",
                    paste(topic, collapse=", "), "); ",
                    "using top-ranked topic from that list (", topic.num, ").")
            message(msg)
            warning(msg)
            rm(msg)
        }

        # find and display topic rank
        topic.rank <- which(ind %in% topic.num)
        if (remake_figs) {
            print(paste0("Topic of rank ", topic.rank, ":"))
        } else {
            message("\nTopic of rank ", topic.rank, ":\n")
        }

        # get Pub.numbers for dissertations with the max proportion of that
        # topic
        row.ind <- order(outputfile[, which(names(outputfile)==topic.num)],
                         decreasing=TRUE)[1:depth]
        diss.ind <- outputfile[row.ind, "Pub.number"]

        print(topic_keys.dt[as.integer(topic.num)])

        # list of top 1:depth documents for this topic
        topdocs <- dataset.dt[as.character(diss.ind),
                                c("Pub.number", "Title", tagnames), with=F]


            # add a column with the weights this topic has in these docs

            weights <- ranks <- c()
            for(j in 1:length(diss.ind)) {
                myrow <- doc_topics.dt[diss.ind[j]]          # returns a list of named topic weights
                topic.col <- match(topic.num, names(myrow))  # index of topic within the list
                weights[j] <- myrow[[topic.col]]             # value at that index is the weight

                justnumbers <- doc_topics.dt[diss.ind[j],as.character(1:ntopics), with=F]
                decr <- order(as.numeric(justnumbers), decreasing=TRUE)
                ranked_row <- doc_topics.dt[diss.ind[j], as.character(decr), with=F]
                ranks[j] <- match(topic.num, names(ranked_row))
            }
            topdocs[, topic_weight:=unlist(weights)]
            topdocs[, rank_in_doc:=unlist(ranks)]
            topdocs <- topdocs[, c("Pub.number", "Title", "topic_weight",
                                   "rank_in_doc", tagnames), with=F]


        # if we're just looking at one topic, maybe we want to save that list
        # of docs and their metadata, and exit.
        if(for.bind) {
            topdocs[, topic:=topic]
            topdocs[, topic_rank:=topic.rank]
            return(topdocs)
        } else {
            print(topdocs)
        }

        # if we're saving all output, automatically cycle through everything.
        # but by default, prompt the user.
        if (remake_figs) {
            a <- ""
        } else {
            a <- readline(paste("Press <enter> for more detail on",
                                "these docs, or S to skip to the next topic: \n"))
        }


        while (tolower(a) != "s") {
            for(i in topdocs$Pub.number) {
                print(get.topics4doc(i, dataset_name, ntopics,
                                    showlabels = showlabels, iter_index=iter_index))
                if (!remake_figs) {
                    a <- readline(paste("Press <enter> for next doc,",
                        "D for more details, or",
                        "S to skip to the next topic: \n"))
                } else {
                    a <- ""
                }

                if (tolower(a) == "s") {
                    break
                } else if (tolower(a) == "d") {
                    print(dataset.dt[i])
                    a <- readline(paste("Press <enter> for next doc",
                                    "or S to skip to the next topic: \n"))
                }
            } # end of loop through Pub.numbers
            a <- "s"
        }

    } else {
        # If we haven't pre-specified a topic, loop through the top topics
        # and their top-proportioned dissertations, optionally showing
        # abstracts and top 5 topics for each of those dissertations
    message("Top ", cutoff, " topics:")
    print(topic_keys.dt[ind])               # top words for each topic

    if(for.bind) {
        # set up a container for docs to return
        to.return <- data.table()
    }

    for (i in start.rank:len) {
        # i gives the topic rank
        topic.num <- ind[i]

        # Search outputfile for the dissertations with max proportion of that
        # topic, and get the Pub.numbers
        row.ind <- order(outputfile[, which(names(outputfile)==topic.num)],
                         decreasing=TRUE)[1:depth]
        diss.ind <- outputfile[row.ind, "Pub.number"]

        if (remake_figs) {
            print(paste0("Topic of rank ", i, ":"))
        } else {
            message("\nTopic of rank ", i, ":\n")
        }

        print(topic_keys.dt[as.integer(topic.num)])

        # list of top 1:depth documents for this topic
        topdocs <- dataset.dt[as.character(diss.ind),
                        c("Pub.number", "Title", tagnames), with=F]

            # add a column with the weights this topic has in these docs

            weights <- ranks <- c()
            for(j in 1:length(diss.ind)) {
                myrow <- doc_topics.dt[diss.ind[j]]          # returns a list of named topic weights
                topic.col <- match(topic.num, names(myrow))  # index of topic within the list
                weights[j] <- myrow[[topic.col]]             # value at that index is the weight

                justnumbers <- doc_topics.dt[diss.ind[j],as.character(1:ntopics), with=F]
                decr <- order(as.numeric(justnumbers), decreasing=TRUE)
                ranked_row <- doc_topics.dt[diss.ind[j], as.character(decr), with=F]
                ranks[j] <- match(topic.num, names(ranked_row))
            }

            topdocs[, topic_weight:=unlist(weights)]
            topdocs[, rank_in_doc:=unlist(ranks)]
            topdocs <- topdocs[, c("Pub.number", "Title", "topic_weight",
                                 "rank_in_doc", tagnames), with=F]



        if(for.bind) {
            # presumably we'll want to distinguish one topic from another
            topdocs[, topic:=topic.num]
            topdocs[, topic_rank:=i]
            setcolorder(topdocs, c("topic", "topic_rank"))

            # but still have the option to return multiple topics
            to.return <- rbind(to.return, topdocs)

            if(i > cutoff) {
                stop("Well, this is embarrassing. Somehow we got above the cutoff. Please
                        debug(top_topic_browser).")

            } else if (i == cutoff) {
                return(to.return)

            } else {
                # continue to next topic in loop
                a <- "s"
            }
        } else if (remake_figs) {
            # if outputting to file, get all the details
            print(topdocs)

            a <- ""

        } else {
            # if neither binding to a variable or outputting to file,
            # assume we're in interactive mode and prompt for what to do next.
            print(topdocs)

            a <- readline(paste("Press <enter> for more detail",
                                "on these docs, or S to skip to the next topic: \n"))
        }

        while (tolower(a) != "s") {
            for(i in topdocs$Pub.number) {
                print(get.topics4doc(pubnum=i, dataset_name=dataset_name,
                                     ntopics=ntopics, howmany=depth,
                                     showlabels=showlabels, iter_index=iter_index))
                if (!remake_figs) {
                    a <- readline(paste("Press <enter> for next doc,",
                                        "D for more details, or",
                                        "S to skip to the next topic: \n"))
                } else {
                    a <- ""
                }
                if (tolower(a) == "s") {
                    break
                } else if (tolower(a) == "u") {
                    i <- i-1
                } else if (tolower(a) == "d") {
                    print(dataset.dt[i])
                    a <- readline(paste("Press <enter> for next doc or",
                                        "S to skip to the next topic: \n"))
                }
            }

            a <- "s"
        }   # end of while loop (of documents)
    }   # end of for loop (of topics)
    }   # end of if/else for specific topic or all topics

}   # end of wrapper function top_topic_browser()


# TO DO:
# New function: Get list of filenames for top docs in a given topic, to find Keywords in Context (KWIC), etc.
# Strategy: start like top_topic_browser(), but extract Pub.number, add filetype extension, and save to text file

# get.docs4topic <- function(       rank         = NULL,            # option 1: browse by topic rank
                                # topic      = NULL,            # option 2: browse by topic number
                                # dataset_name = "consorts",        # which topic model to use?
                                # ntopics        = 55,              # which topic model to use?
                                # level      = 0.12             # how much of a doc must the topic account for
                                                                    # # to be included?
    # ){

    # # load the data from the functions defined or imported above
    # topic_keys.dt <- get.topickeys(dataset_name, ntopics)
    # grids <- get.doctopic.grid(dataset_name, ntopics)
        # colsums <- grids$colsums
        # colsums.sort <- grids$colsums.sort
        # outputfile <- grids$outputfile
    # rm(grids)

    # # List the keys for the top N topics, where N = cutoff
    # len <- min(length(colsums)-1, cutoff)
    # ind <- as.integer(names(colsums.sort)[2:(len+1)])     # list of topics by rank; skip Pub.num

    # # If we specified a topic, show just that topic and exit.
    # if (!is.null(topic))
    # {
        # topic.num <- topic

        # # find and display topic rank
        # topic.rank <- which(ind %in% topic.num)
        # if (remake_figs) {    print(paste0("Topic of rank ", topic.rank, ":")) }
        # else { message("\nTopic of rank ", topic.rank, ":\n") }

        # # get Pub.numbers for dissertations with the max proportion of that topic
        # row.ind <- order(outputfile[, which(names(outputfile)==topic.num)], decreasing=TRUE)[1:depth]
        # diss.ind <- outputfile[row.ind, "Pub.number"]

        # print(topic_keys.dt[topic.num])

        # topdocs <- dataset.dt[as.character(diss.ind), c("Pub.number", "Title", tagnames), with=F]

        # print(topdocs)



# }
# source("frameToD3.R")
# dt <- as.data.table(outputfile)
# groupVars <- c("Pub.number")  # Ben: this is the name of that first (ID) column. replace accordingly.
# dataVars <- colnames(dt)[!colnames(dt) %in% groupVars]    # Ben: any column that's not an ID is a datapoint
# filename <- file.path(tmloc, paste0(dataset_name, "k", ntopics, "_clusters.json")
# frameToJSON(outputfile,groupVars,dataVars,outfile=filename)

########
## Given a topic of interest, get clean data to share with others about the top N docs
shareable_topic <- function(  # where will our topic assignments come from?
                              dataset_name = "consorts",
                              ntopics = 55,

                              # if we've run this model multiple times, which iteration?
                              iter_index = "",

                              # do we want to show the full dataset, or a subset?
                              subset_name = "realconsorts",

                              # must specify one topic
                              topic,

                              # how many docs to show for each topic?
                              depth = 10,

                              # show current topic labels for indiv. docs?
                              showlabels = TRUE


) {
    a <- top_topic_browser(topic=topic,
                           dataset_name=dataset_name,
                           ntopics=ntopics,
                           subset_name=subset_name,
                           depth=depth,
                           showlabels=showlabels,
                           iter_index=iter_index,
                           for.bind=T)

    index <- as.character(a$Pub.number)
    ignore.cols <- c(tagnames, tagnames.simple, "Method.Terms", "Flag.notes", "Method.Count", "Exclude.Level", "Counts.simple")

    dataset.dt <- data.table(get(dataset_name), key="Pub.number")

    docs <- dataset.dt[index]
    docs <- docs[, !(names(dataset.dt) %in% ignore.cols), with=F]
    # names(docs)
    # names(a)
    docs$topic_weight <- a$topic_weight
    docs$rank_in_doc <- a$rank_in_doc

    new.col.order <- c("Pub.number", "Author", "Title", "topic_weight","rank_in_doc",
                       "School", "Advisor.Name", "Degree", "Department", "Year", "Pages",
                       "Subject", "KEYWORDS", "ABSTRACT")
    docs <- docs[, new.col.order, with=F]
    head(docs, 1)

    if(remake_figs) {
        # filename <- file.path(imageloc, paste0("top ", depth, " documents for topic ", topic, ", ",
        #                dataset_name, "k", ntopics, subset_name, iter_index, ".csv"))
        if(!exists("build_plot_title")) {
            source(file="build_plot_title.R")
        }
        filename <- build_plot_title(dataset_name=dataset_name,
                                     ntopics=ntopics,
                                     iter_index=iter_index,
                                     subset_name=subset_name,
                                     whatitis=paste("top", depth, "documents for topic", topic),
                                     for.filename = T)

        write.csv(docs, filename)
    } else {
        warning("This function usually saves to .csv, but remake_figs is off.")
        warning("Printing to screen only.")
        print(docs)
    }

    return(docs)
}

# convenience function to show the top terms and documents for the top @howmany topics
top_titles_table <- function(dataset_name="noexcludes2001_2015",
                            ntopics=50,
                            iter_index=1,
                            subset_name="knownprograms2001_2015",
                            howmany=ntopics,  # how many topics to show?
                            topic=NULL,
                            depth=3,        # how many titles per topic?
                            showlabels=TRUE)
{

    # if(!is.null(topic)) {
    #     howmany = 1
    # }

    ttt <- top_topic_browser(dataset_name=dataset_name,
                           ntopics=ntopics,
                           subset_name=subset_name,
                           showlabels=showlabels,
                           iter_index=iter_index,
                           depth=depth,
                           topic=topic,
                           cutoff=howmany,
                           for.bind=T)

    ttt <- ttt[, list(topic, topic_rank, Title, topic_weight, rank_in_doc)]
    head(ttt)

    ttt <- ttt[, .(Titles = paste(paste0(Title, " (", 100*round(topic_weight, 2), "%)"), collapse=" || ")), by=list(topic, topic_rank)]
    head(ttt)

    # TO DO:
    # add in the top keywords by_tfitf (or make it an option to do by_prob),
    # so I don't have to merge by hand in Excel

    if(remake_figs) {
        if(!exists ("build_plot_title")) {
            source(file="build_plot_title.R")
        }

        outfile <- build_plot_title(dataset_name=dataset_name,
                                    ntopics=ntopics,
                                    iter_index = iter_index,
                                    subset_name = subset_name,
                                    whatitis = paste("top", depth, "titles for top", howmany, "topics"),
                                    for.filename = TRUE)
        outfile <- file.path(imageloc, paste0(outfile, ".csv"))
        write.csv(ttt, outfile, row.names = F)
    }

    return(ttt)
}

# build a table for a cluster of topics using the function above
# TO DO: just combine these functions and handle multiple named topics
cluster_titles_table <- function(cluster_members=NULL,    # vector of topic numbers
                                 cluster_name=NULL,       # what should we call it?
                                 dataset_name="noexcludes2001_2015",
                                 ntopics=50,
                                 iter_index=1,
                                 subset_name="knownprograms2001_2015",
                                 depth=3,        # how many titles per topic?
                                 use.labels=TRUE,
                                 mygrid = NULL,    # pass for slight speed boost
                                 verbose = F,
                                 level = 0.5)
{
    if (! exists("cluster.strength", mode="function")) {
        source(file = "topic cluster reach.R")
    }

    this_cluster <- cluster.strength(my.topics = cluster_members,
                     my.topics_name = cluster_name,
                     dataset_name = dataset_name,
                     ntopics = ntopics,
                     iter_index = iter_index,
                     subset_name = subset_name,
                     use.labels = use.labels,
                     level = level,
                     grid = mygrid)

    mypubs <- head(this_cluster$docs, depth)
    myweights <- head(this_cluster$doc_levels, depth)

    mytable <- data.table(Pub.number = mypubs,
               ClusterWeight = myweights)

    dataset <- data.table(get(dataset_name), key = "Pub.number")

    mytable[, Title:=dataset[Pub.number, Title]]
    mytable[, "Weight":=round(ClusterWeight, 4) * 100]

    to.return <- mytable[order(-ClusterWeight), list(Title, Weight)]

    if(verbose) {
        setkey(mytable, "Pub.number")
        for (pubnum in mypubs) {
            message("Pubnum ", pubnum, ", with cluster at ", mytable[pubnum, Weight])
            get.topics4doc(pubnum = pubnum,
                           showlabels = T,
                           verbose = T
            )
            readline("<press any key to continue>")
        }
    }

    if (remake_figs) {
        if(!exists ("build_plot_title")) {
            source(file="build_plot_title.R")
        }

        outfile <- build_plot_title(dataset_name=dataset_name,
                                    ntopics=ntopics,
                                    iter_index = iter_index,
                                    subset_name = subset_name,
                                    whatitis = paste("top titles for topics in cluster", cluster_name),
                                    for.filename = TRUE)
        outfile <- file.path(imageloc, paste0(outfile, ".csv"))
        write.csv(to.return, outfile, row.names = F)
    }

    return(to.return)

}



## Run the big browser function above
if (autorun) {
    # dataset_name <- "consorts"
    # subset_name <- "realconsorts"
    # ntopics <- 55
    if (remake_figs) {
        filename <- paste0(imageloc, "top topics - ", dataset_name, ", K", ntopics, subset_name, "topic 8.txt")
        readline(paste("About to capture browser output as", filename,"- <enter> to continue or <esc> to abort."))
    } else {
        top_topic_browser(subset_name=subset_name)
        top_topic_browser(subset_name="realconsorts")
    }
} else {
    message(paste("top docs per topic.R loaded the following functions: \n",
                  " * get.doc.composition(dataset, ntopics): retrieves a pre-existing matrix, \n",
                  "   output by MALLET, with topic proportions for each document in corpus \n",
                  " * get.topics4doc(pubnum, dataset_name, ntopics, howmany, showlabels): \n",
                  "   retrieves top `howmany` topics for a document specified by `pubnum`. \n",
                  " * top_topic_browser(...): for a specified topic or range of topics,   \n",
                  "   shows the top `howmany` documents and their method tags, with   \n",
                  "   optional detail view showing top topics for each document at a time. \n",
                  " * top_titles_table(..., howmany, depth): for a specified dataset_name,  \n",
                  "   ntopics, iter_index, and subset_name, show the top `depth` titles \n",
                  "   for the top `howmany` topics, concatenated with the weight of that topic \n",
                  "   in each doc. \n",
                  " * cluster_titles_table(cluster_members, cluster_name, ..., howmany, depth): \n",
                  "   like top_titles_table, but for a combined set of topics, treated cumulatively."
    ))
}

if(FALSE) {
    dataset_name <- "noexcludes2001_2015"
    # subset_name <- "knownprograms2001_2015"
    subset_name <- "nonrcws2001_2015"
    ntopics <- 50
    iter_index <- 1
    bad.topics <- c("3", "8", "12", "15", "30", "34", "36", "47", "50")
    
    remake_figs
    top_topic_browser(dataset_name="noexcludes2001_2015", ntopics=50, iter_index=1,
                      depth=10)
    top_topic_browser(dataset_name="noexcludes2001_2015", ntopics=50, iter_index=1,
                      depth=3, subset_name="knownprograms2001_2015", showlabels=T)
    toptopic <- shareable_topic(dataset_name = "noexcludes2001_2015",
                                ntopics=50,
                                iter_index=1,
                                subset_name = "knownprograms2001_2015",
                                topic=32)
    toptitles <- top_titles_table(dataset_name="noexcludes2001_2015",
                                  ntopics=50,
                                  iter_index=1,
                                  subset_name="knownprograms2001_2015",
                                  ## how many topics to show?
                                  howmany=ntopics,
                                  # howmany=10,
                                  depth=3,        # how many titles per topic
                                  showlabels=TRUE
                      )

    bad.topics.table <- cluster_titles_table(dataset_name="noexcludes2001_2015",
                                         ntopics=50,
                                         iter_index=1,
                                         subset_name="knownprograms2001_2015",
                                         depth=3,        # how many titles per topic
                                         showlabels=TRUE,
                                         cluster_members = bad.topics
    )

    remake_figs
    cluster_titles_table(cluster_members = c(9, 19, 29, 13, 31),
                         cluster_name = "public rhetoric")


    # Reconsidering language-based topics. What if they're real?
    shareable_topic(dataset_name = "noexcludes2001_2015",
                    ntopics = 50,
                    iter_index = 1,
                    subset_name = "knownprograms2001_2015",
                    topic = 47)   # spanish language
    shareable_topic(dataset_name = "noexcludes2001_2015",
                    ntopics = 50,
                    iter_index = 1,
                    subset_name = "knownprograms2001_2015",
                    topic = 30)   # german language
    
    #### non-rcws analyses
    toptitles2 <- top_titles_table(dataset_name = "noexcludes2001_2015",
                                  ntopics = 50,
                                  iter_index = 1,
                                  subset_name = "nonrcws2001_2015",
                                  ## how many topics to show?
                                  # howmany = ntopics,
                                  howmany = 10,
                                  depth = 3,        # how many titles per topic
                                  showlabels = TRUE
    )
    
    
    
    for (mytopic in c(35, 49, 18, 42)) { # cluster_name = "Teaching.and.Administration" 
        tab1 <- top_titles_table(subset_name = "nonrcws2001_2015",
                         topic = mytopic,
                         depth = 8,
                         showlabels = TRUE)
        tab2 <- top_titles_table(subset_name = "knownprograms2001_2015",
                                 topic = mytopic,
                                 depth = 8,
                                 showlabels = TRUE)
        
        report <- merge(tab1, tab2, by="topic", all=T)
        names(report) <- c("Topic", "Non-RCWS Rank", "Non-RCWS Titles", "RCWS Rank", "RCWS Titles")
        
        print(topic_labels[mytopic, Label])
        print(report)
        
        invisible(readline("Press <enter> to continue.\n"))
    }
        
    toptitles3 <- top_titles_table(cluster_name = "Teaching.and.Administration",
                                       cluster_members = c(18, 35, 42, 49),
                                       subset_name = "knownprograms2001_2015")
    
    toptitles4 <- cluster_titles_table(cluster_name = "Teaching.and.Administration",
                                       cluster_members = c(18, 35, 42, 49),
                                       subset_name = "nonrcws2001_2015sans_badtops")
    
}
