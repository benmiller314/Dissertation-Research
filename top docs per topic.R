#############################################################################
# top docs per topic.R
#
# Tools for topic exploration
#

# Provides three functions:
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
#   Legacy support:
#         * This used to define a function get.doc.composition, but it ended up being a 
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
                           dataset_name = "consorts", 
                           ntopics = 55, 
                           howmany = 5, 
                           showlabels = FALSE,
                           iter_index = "") 
{
    # get packages in case we've just restarted R
    require(data.table)
        
    # pubnum <- "3051708"; doc_tops <- doc_topics.dt    # test values
        
    if (!is.character(pubnum)) { 
        pubnum <- as.character(pubnum) 
    }
    
    # start with the doc/topic grid
    doc_tops <- get.doctopic.grid(dataset_name, ntopics, iter_index=iter_index)$outputfile.dt
    
    # narrow to the doc named by the pubnum; skip the pubnum for sorting by descending weight
    this_doc_tops <- doc_tops[pubnum,2:ntopics+1, with=F]
    this_doc_tops <- this_doc_tops[, order(this_doc_tops, decreasing = T), with=F][,1:howmany, with=F]
    
    # merge weights with keys: from all keys, narrow to just the top howmany in this doc
    topic_keys <- data.table(get.topickeys(dataset_name, ntopics, iter_index=iter_index))
    topic_keys <- topic_keys[as.numeric(names(this_doc_tops))] 
    topic_keys[,weight:=as.numeric(this_doc_tops)]
    
    # reorder columns for ease of presentation
    topic_keys <- topic_keys[, list(topic, weight, alpha, top_words)]

    if(showlabels) { 
        if(!exists("get_topic_labels", mode="function")) { 
            source(file="get topic labels.R") 
        }
        topic_labels <- data.table(get_topic_labels(dataset_name, ntopics, iter_index=iter_index),
                                                    key="Topic")
        topic_keys[, current_label:=topic_labels[topic_keys$topic, Label]]
        topic_keys <- topic_keys[, list(topic, weight, alpha, current_label,
                                        top_words)]
        topic_keys
    }
    list("title" = noexcludes.dt[pubnum, c("Title", "Pub.number", tagnames),
                                 with=F],
        "keys" = topic_keys,
        "abstract" = noexcludes.dt[pubnum, c("KEYWORDS", "ABSTRACT"), with=F]       
        )
    
# close helper function get.topics4doc  
}   

### Browse through the top topics and their top-proportioned dissertations
top_topic_browser <- function(
                              # assuming we're looping, start where?
                              start.rank = 1,

                              # alternately, browse one specified topic
                              topic = NULL,
                              
                              # where will our topic assignments come from?          
                              dataset_name = "consorts",
                              ntopics = 55, 
                              
                              # do we want to show the full dataset, or a subset?
                              subset_name = NULL,
                              
                              # if we've run the params above many times, which one now?
                              iter_index = "",
                              
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
    
    grids <- get.doctopic.grid(dataset_name=dataset_name, ntopics=ntopics, subset_name=subset_name, iter_index=iter_index)
        colsums <- grids$colsums
        colsums.sort <- grids$colsums.sort
        outputfile <- grids$outputfile
        doc_topics.dt <- data.table(outputfile, key = "Pub.number")
    rm(grids)
    
    
    if(! exists("get.topickeys", mode = "function")) {
        source(file=file.path(sourceloc, "get topickeys.R"))
    }
    
    topic_keys.dt <- get.topickeys(dataset_name, ntopics, iter_index=iter_index)
    
    # List the keys for the top N topics, where N = cutoff
    len <- min(length(colsums)-1, cutoff)
    
    # list of topics by rank; skip Pub.num
    ind <- as.integer(names(colsums.sort)[2:(len+1)])       

        # If we specified a topic, show just that topic and exit.
    if (! is.null(topic)) {
        topic.num <- topic
        
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
        topdocs <- noexcludes.dt[as.character(diss.ind), 
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
        # print(topdocs)

        # if we're just looking at one topic, maybe we want to save that list
        # of docs and their metadata, and exit.
        if(for.bind) {
            return(topdocs)
        }

        # if we're saving all output, automatically cycle through everything.
        # but by default, prompt the user.
        if (!remake_figs) { 
            a <- readline(paste("Press <enter> for more detail on", 
                        "these docs, or S to skip to the next topic\n"))
        } else { 
            a <- ""
        }


        while (tolower(a) != "s") {
            for(i in topdocs$Pub.number) {
                print(get.topics4doc(i, dataset_name, ntopics, 
                                    showlabels = showlabels, iter_index=iter_index))
                if (!remake_figs) { 
                    a <- readline(paste("Press <enter> for next doc,", 
                        "D for more details, or", 
                        "S to skip to the next topic\n"))
                } else { 
                    a <- ""
                }
                
                if (tolower(a) == "s") { 
                    break 
                } else if (tolower(a) == "d") { 
                    print(noexcludes.dt[i]) 
                    a <- readline(paste("Press <enter> for next doc", 
                                    "or S to skip to the next topic\n"))
                }
            }
            a <- "s"
        }
        
    } else {
        # If we haven't pre-specified a topic, loop through the top topics
        # and their top-proportioned dissertations, optionally showing
        # abstracts and top 5 topics for each of those dissertations
    message("Top ", cutoff, " topics:")
    print(topic_keys.dt[ind])               # top words for each topic
        
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
        topdocs <- noexcludes.dt[as.character(diss.ind), 
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
            
        print(topdocs)
        
        if (!remake_figs) { 
            a <- readline(paste("Press <enter> for more detail", 
                        "on these docs, or S to skip to the next topic\n")) 
        } else { 
            a <- ""
        }

        while (tolower(a) != "s") {
            for(i in topdocs$Pub.number) {
                print(get.topics4doc(i, showlabels=showlabels, iter_index=iter_index))
                if (!remake_figs) { 
                    a <- readline(paste("Press <enter> for next doc,",
                                        "D for more details, or",
                                        "S to skip to the next topic\n"))
                } else { 
                    a <- ""
                }
                if (tolower(a) == "s") { 
                    break 
                } else if (tolower(a) == "u") {
                    i <- i-1
                } else if (tolower(a) == "d") { 
                    print(noexcludes.dt[i]) 
                    a <- readline(paste("Press <enter> for next doc or",
                                        "S to skip to the next topic\n"))
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
        
        # topdocs <- noexcludes.dt[as.character(diss.ind), c("Pub.number", "Title", tagnames), with=F]

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
shareable_topic <- function(  topic,
                              
                              # where will our topic assignments come from?          
                              dataset_name = "consorts",
                              ntopics = 55, 
                              
                              # do we want to show the full dataset, or a subset?
                              subset_name = "realconsorts",
                              
                              # how many docs to show for each topic?
                              depth = 10,            
                              
                              # show current topic labels for indiv. docs?  
                              showlabels = TRUE,
                              
                              # if we've run this model multiple times, which iteration?
                              iter_index = ""
) {
    a <- top_topic_browser(topic=topic, 
                           dataset_name=dataset_name, 
                           ntopics=ntopics, 
                           subset_name=NULL, 
                           depth=depth,
                           showlabels=showlabels,
                           iter_index=iter_index,
                           for.bind=T)
    
    index <- as.character(a$Pub.number)
    ignore.cols <- c(tagnames, tagnames.simple, "Method.Terms", "Flag.notes", "Method.Count", "Exclude.Level", "Counts.simple")
    
    
    docs <- noexcludes.dt[index]
    docs <- docs[, !(names(noexcludes.dt) %in% ignore.cols), with=F]
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
        filename <- paste0(imageloc, "top ", depth, " documents for topic ", topic, ", ", 
                       dataset_name, "k", ntopics, subset_name, iter_index, ".csv")
        write.csv(docs, filename)
    } else {
        warning("This function usually saves to .csv, but remake_figs is off.")
        warning("Printing to screen only.")
        print(docs)
    }
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
                  "   optional detail view showing top topics for each document at a time."))
}

if(FALSE) {
    top_topic_browser(topic=35, 
                      dataset_name="noexcludes2001_2015", ntopics=60, iter_index=4,
                      depth=10)
}
