#############################################################################
# top docs per topic.R
#
# Tools for topic exploration
#
# Provides three functions:
#         * get.doc.composition(dataset, ntopics): retrieves a pre-existing 
#           matrix, output by MALLET, with topic proportions for each 
#           document in corpus
#         * get.topics4doc(pubnum, dataset_name, ntopics, howmany, 
#           showlabels): retrieves top `howmany` topics for a document 
#           specified by `pubnum`. 
#         * top_topic_browser(...): for a specified topic or range of topics, 
#           shows the top `howmany` documents and their method tags, with 
#           optional detail view showing top topics for each document at a 
#           time. See below for parameters.
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
    
    # We start with the doc-topic matrix from MALLET:
get.doc.composition <- function(dataset_name="consorts", ntopics=55) 
{
    # get packages in case we've just restarted R
    require(data.table)
    
    filename <- file.path(tmloc, paste0(dataset_name, "k", ntopics,
                      "_composition.txt"))
    doc_topics <- read.delim(filename, header=F, skip=1)
    head(doc_topics)
    
    # column 1 is an unneeded index; column 2 contains names of identical
    # length, ending with a 7-digit Pub.number followed by ".txt"; final
    # column is empty. Let's simplify.
    doc_topics[, "V1"] <- NULL 
    len <- nchar(as.character(doc_topics[1, "V2"]))
    doc_topics[, "V2"] <- substr(as.character(doc_topics[, "V2"]), 
                                 (len-10), (len-4))
    if (is.na(all(doc_topics[, ncol(doc_topics)]))) { 
        doc_topics[, ncol(doc_topics)] <- NULL
    }

    # Get findable column names
    colnames(doc_topics)[1] <- "Pub.number"
    colnames(doc_topics)[seq(2, ncol(doc_topics), 2)] <- paste0("top", 
            seq(1, (ncol(doc_topics)-1)/2, 1))
    colnames(doc_topics)[seq(3, ncol(doc_topics), 2)] <- paste0("wgt", 
            seq(1, (ncol(doc_topics)-1)/2, 1))
    head(colnames(doc_topics))
    
    # convert to 1-indexed from MALLET's 0-indexed, so everything matches
    doc_topics[, seq(2, ncol(doc_topics), 2)] <- 
                        (doc_topics[, seq(2, ncol(doc_topics), 2)]+1)
    
    # for some reason, it thinks the topic weights are characters. They're
    # numbers.
    doc_topics[, seq(3, ncol(doc_topics), 2)] <- 
            apply(doc_topics[, seq(3, ncol(doc_topics), 2)], 2,
                  FUN=function(x) { x <- as.numeric(x) })

    doc_topics.dt <- as.data.table(doc_topics)
    setkey(doc_topics.dt, Pub.number)
    head(doc_topics.dt)
    
    return(doc_topics.dt)
}

# Run `get.doc.composition()` when file is sourced, so we don't have to
# recreate this multiple times for the same dataset if we're running
# `top_topic_browser()` using the `for.bind` option.
# TO DO: Make this happen within get.doc.composition() -- i.e. give the
# function the side effect of creating this object -- so it's responsive to
# dataset_name and ntopics.
doc_topics_consorts_55.dt <- get.doc.composition("consorts", 55)

    
### Helper function: retrieve top five topics for a given Pub.number
get.topics4doc <- function(pubnum, 
                           dataset_name = "consorts", 
                           ntopics = 55, 
                           howmany = 5, 
                           showlabels = FALSE) 
{
    # get packages in case we've just restarted R
    require(data.table)
        
    # pubnum <- "3051708"; doc_tops <- doc_topics.dt    # test values
        
    if (!is.character(pubnum)) { 
        pubnum <- as.character(pubnum) 
    }
    
    doc_tops <- get.doc.composition(dataset_name, ntopics)
    topic_keys <- data.table(get.topickeys(dataset_name, ntopics))
    topic_keys <- topic_keys[as.numeric(doc_tops[pubnum, paste0("top",
                                                 1:howmany), with=F])]
    topic_keys[,weight:=as.numeric(doc_tops[pubnum, paste0("wgt", 1:howmany),
                                                             with=F])]
    topic_keys <- topic_keys[, list(topic, weight, alpha, top_words)]

    if(showlabels) { 
        if(!exists("get_topic_labels", mode="function")) { 
            source(file="get topic labels.R") 
        }
        topic_labels <- data.table(get_topic_labels(dataset_name, ntopics),
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
    
    # load the data from the functions defined or imported above
    doc_composition <- paste0("doc_topics_", dataset_name, "_", 
                             ntopics, ".dt");
    if(!exists(doc_composition)) { 
        doc_topics.dt <- get.doc.composition(dataset_name, ntopics) 
    } else { 
        doc_topics.dt <- get(doc_composition) 
    }
        
    topic_keys.dt <- get.topickeys(dataset_name, ntopics)
    
    grids <- get.doctopic.grid(dataset_name=dataset_name, ntopics=ntopics, subset_name=subset_name)
        colsums <- grids$colsums
        colsums.sort <- grids$colsums.sort
        outputfile <- grids$outputfile
    rm(grids)
    
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

        print(topic_keys.dt[topic.num])

        # list of top 1:depth documents for this topic
        topdocs <- noexcludes.dt[as.character(diss.ind), 
                                c("Pub.number", "Title", tagnames), with=F]         

            # add a column with the weights this topic has in these docs
            doc_tops <- get.doc.composition(dataset_name, ntopics)
            weights <- ranks <- c()
            for(j in 1:length(diss.ind)) {
                topic.col <- match(topic.num, 
                                   doc_tops[as.character(diss.ind)][j])
                weights[j] <- doc_tops[as.character(diss.ind)][j, 
                                         (topic.col+1), with=F]
                ranks[j] <- topic.col/2
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
                                    showlabels = showlabels))
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
        
        print(topic_keys.dt[topic.num])
        
        # list of top 1:depth documents for this topic
        topdocs <- noexcludes.dt[as.character(diss.ind), 
                        c("Pub.number", "Title", tagnames), with=F]

            # add a column with the weights this topic has in these docs
            doc_tops <- get.doc.composition(dataset_name, ntopics)
            weights <- ranks <- c()
            for(j in 1:length(diss.ind)) {
                topic.col <- match(topic.num, 
                                    doc_tops[as.character(diss.ind)][j])
                weights[j] <- doc_tops[as.character(diss.ind)][j, 
                                    (topic.col+1), with=F]
                ranks[j] <- topic.col/2
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
                print(get.topics4doc(i, showlabels=showlabels))
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

## Run the big browser function above
if (autorun) {
    dataset_name <- "consorts"
    subset_name <- "realconsorts"
    ntopics <- 55
    if (remake_figs) { 
        filename <- paste0(imageloc, "top topics - ", dataset_name, ", K", ntopics, subset_name, "topic 8.txt")
        readline(paste("About to capture browser output as", filename,"- <enter> to continue or <esc> to abort."))
        
        
    } else {
        top_topic_browser(subset_name=subset_name)
    }
}

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
    # doc_topics.dt <- get.doc.composition(dataset_name, ntopics)
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
# filename <- paste0(tmloc, "/", dataset_name, "k", ntopics, "_clusters.json")
# frameToJSON(outputfile,groupVars,dataVars,outfile=filename)

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
                              showlabels = TRUE
) {
    a <- top_topic_browser(topic=topic, 
                           dataset_name=dataset_name, 
                           ntopics=ntopics, 
                           subset_name=NULL, 
                           depth=depth,
                           showlabels=showlabels,
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
                       "School", "Advisor.Name", "Degree", "Department", "Year", "pages",
                       "Subject", "KEYWORDS", "ABSTRACT")
    docs <- docs[, new.col.order, with=F]
    head(docs, 1)
    
    filename <- paste0(imageloc, "top ", depth, " documents for topic ", topic, ", ", 
                       dataset_name, "k", ntopics, subset_name, ".csv")

    write.csv(docs, filename)
}
