# Goal: given a topic clustering, see whether some schools are over-represented 
# in some clusters or branches within a cluster

schools_by_topic <- function(mytopic,
                             level = 0.12,     # percentage of a diss contributed by the topic
                                               # for it to "count"
                             howmany = 10,      # how many top schools to show?
                                               # use -1 for all.
                             dataset_name="noexcludes2001_2015",
                             ntopics=50,
                             subset_name="realconsorts2001_2015",  # set NULL if not using.
                             iter_index=1,     # suffix to differentiate repeat runs of same MALLET params.
                             outfile=NULL,     # if not provided, will be set to a default.
                             use.labels=FALSE, # replace topic numbers with labels
                                               # chosen using top_topic_browser()?
                             bad.topics= NULL) # exclude non-content-bearing topics
                             
{
    require(data.table)
    
    if(!exists("get.doctopic.grid", mode="function")) { 
        source(file="get doctopic grid.R") 
    }
    dt <- get.doctopic.grid(dataset_name=dataset_name,
                            ntopics=ntopics,
                            subset_name=subset_name,
                            iter_index=iter_index)$outputfile.dt
    
    # Exclude any NA rows included accidentally by the index file
    dt <- na.omit(dt)
    
    # Check for non-content-bearing topics, but don't remove them
    if (is.null(bad.topics) && mytopic %in% bad.topics) {
        warning("Topic ", mytopic, " has been marked as non-content-bearing.")
    }
    
    # narrow to columns we need
    dt[, c("Pub.number", mytopic), with=F]
    mytopic.c <- as.character(mytopic)
    
    # Get the relevant documents from the doc-topic grid
    mypubs <- dt[dt[[mytopic.c]] > level, c("Pub.number", mytopic), with=F]
    setnames(mypubs, mytopic.c, "contrib")
    
    # Use that document list to retrieve school/department info from the dataset
    if(!exists(dataset_name)) {
        source(file="dataprep 2 - load data.R")
    }
    
    if(!is.null(subset_name) && nchar(subset_name) > 1) {
        dataset <- get(subset_name)
    } else {
        dataset <- get(dataset_name)
    }
    
    index <- which(dataset$Pub.number %in% mypubs$Pub.number)
    mydepts <- dataset[index, c("Pub.number", "School", "Department")]
    mydepts <- as.data.table(mydepts)
    mydepts <- merge(mydepts, mypubs, by="Pub.number")
    
    setkey(mydepts, School, Department)
    
    mydepts[, avg:=mean(contrib), by=.(School, Department)]
    toplist <- mydepts[, .N, by=.(School)][order(-N)]
    
    # TO DO: divide each DissCount (topical output) 
    #        by disstotal from that dept for the larger dataset
    #        to get a DissPct (topical focus)
    
    title <- paste("DissCount with topic", mytopic, ">", level)
    
    if(use.labels) {
        if(!exists("get_topic_labels")) { source(file="get topic labels.R") }
        tryCatch(expr={labels <- get_topic_labels(dataset_name=dataset_name, 
                                   ntopics=ntopics, 
                                   subset_name=subset_name, 
                                   iter_index=iter_index)
                        title <- paste0(title, "\n Topic label: '", 
                                        labels[mytopic, Label] ,"'")},
                error=function(e) e,
                finally=""
        )
    }
    
    message(title)
    if (howmany > 0) {
        toplist <- toplist[1:howmany]
    }
    
    return(toplist)
}

if(FALSE) {
    schools_by_topic(41)
    schools_by_topic(1, use.labels=T, ntopics=150)
}
    
