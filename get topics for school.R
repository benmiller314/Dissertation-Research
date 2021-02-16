# Given a schoolname of interest,
# (1) find all dissertations at that school
# (2) sum the topic contributions of all those dissertations
# (3) rank the resulting topics, with current labels
get.topics4school <- function(schoolname,
                              dataset_name = "noexcludes2001_2015",
                              ntopics = 50,
                              iter_index = 1,
                              subset_name = NULL,
                              newnames = F,       # where in the MALLET output filename does iter_index appear?
                              # set T if it's with the model, F if last in filename.
                              # Gets passed into get.doctopic.grid.
                              howmany = 10,
                              showlabels = TRUE,
                              use.clusters = FALSE,  # call cluster.reach()?
                              clusterlevel = 0.13)   # if so, at what level?
{
    # Get the data
    if (! is.null(subset_name)) {
        dataset <- get(subset_name)
    } else {
        dataset <- get(dataset_name)
    }
    
    # 1a. Confirm school name
    # search for school
    school.list <- factor(dataset[grep(schoolname, dataset$School, ignore.case=T), "School"])
    
    # condense results
    school.list <- levels(school.list)
    
    if(length(school.list) == 1) {
        school <- school.list
    } else if (length(school.list) > 1) {
        message("More than one matching school found.")
        print(school.list)
        
        a <- as.integer(readline("Search again using row number... "))
        school <- school.list[a]
    } else {
        stop(paste("School not found within the specified dataset:", dataset_name, subset_name))
    }
    message(paste("Finding topics for school: ", school))
    
    # 2a. Get list of Pub.numbers for the named school
    pubnums <- dataset[grep(school, dataset$School, ignore.case=T), "Pub.number"]
    
    # If using clusters, just call that function with this school as the subset
    if(use.clusters) {
        if(!exists("cluster.strength", mode="function")) {
            source(file="topic cluster reach.R")
        }
        
        all_clusters <- name_topic_clusters(dataset_name = dataset_name,
                                            ntopics = ntopics,
                                            iter_index = iter_index,
                                            subset_name = subset_name)
        
        if(length(grep("all_clusters", all_clusters$name))) {
            all_clusters <- all_clusters[grep("all_clusters", all_clusters$name),
                                         "topics"]
            all_clusters <- strsplit(all_clusters, " ")[[1]]
        }
        
        
        clusters <- sapply(all_clusters, FUN = function(x) {
            cluster.strength(my.topics_name = x,
                             dataset_name = dataset_name,
                             ntopics = ntopics,
                             subset_name = pubnums,
                             level = clusterlevel)
        })
        
        msg <- paste("\nUsing a model of", dataset_name, subset_name, "with", ntopics, "topics.\n",
                     "Out of the", length(pubnums), "dissertations at", school, ",",
                     "here are the numbers of dissertations with at least", 100*clusterlevel,
                     "percent of words from the specified topic clusters:")
        message(msg)
        return(clusters)
    }
    
    # 2b. Get doc-topic grid for this dataset
    if(!exists("get.doctopic.grid", mode="function")) {
        source(file="get doctopic grid.R")
    }
    grid <- get.doctopic.grid(dataset_name=dataset_name, ntopics=ntopics,
                              subset_name=subset_name, iter_index=iter_index,
                              newnames=newnames)$outputfile
    
    # 2c. Filter grid by pubnums
    my.grid <- grid[which(grid$Pub.number %in% pubnums), ]
    
    # 2d. Sum columns
    colsums <- colSums(my.grid[, 2:ncol(my.grid)])
    
    # Optionally add labels
    if(showlabels) {
        if(!exists("get_topic_labels", mode="function")) {
            source(file="get topic labels.R")
        }
        topic_labels <- data.table(get_topic_labels(dataset_name=dataset_name,
                                                    ntopics=ntopics, iter_index=iter_index),
                                   key="Topic")
        names(colsums) <- paste0(names(colsums), ": ", topic_labels[as.integer(names(colsums)), Label])
    }
    
    # 3. Sort by sums, find percentages of this sub-corpus
    colsums.sort <- sort(colsums, decreasing=T)
    head(colsums.sort)
    colsums.sort.pct <- round((colsums.sort / nrow(my.grid)), 4) * 100
    head(colsums.sort.pct)
    
    msg <- paste("Using model of", dataset_name, subset_name, "with", ntopics, "topics,",
                 "these are the top", howmany, "topics \nand contributions",
                 "(as a percentage of words in", nrow(my.grid), "dissertations):")
    message(msg)
    print(head(colsums.sort, howmany))
    
    invisible(list(grid = my.grid,
                   sums = colsums,
                   pcts = colsums.sort.pct)
    )
}

if(autorun) {
    a <- get.topics4school("CUNY", howmany=55)
    sum(a$pcts)
    
    get.topics4school("CUNY", use.clusters = T)
}
