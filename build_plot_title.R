# Ben: helper function to build a filename / main title for topic-modeling figures
build_plot_title <- function(dataset_name, ntopics, iter_index, subset_name=NULL,
                             bad.topics=NULL, use.labels=FALSE,
                             whatitis="Cluster Dendrogram",
                             for.filename=FALSE) {
    if(! is.null(subset_name)) { subset_part <- paste0("--", subset_name)
    } else { subset_part <- "" }

    if(length(bad.topics > 0)) { bad_topics_part <- paste0(", ", length(bad.topics), " bad topics hidden")
    } else { bad_topics_part <- "" }

    if(iter_index > 0) { iter_part <- paste0("_iter", iter_index)
    } else { iter_part <- "" }

    if(use.labels) { label_part <- ", topics labeled"
    } else { label_part <- "" }

    main <- paste0(whatitis, ", ",                                       # type of plot/file
                   dataset_name, "k", ntopics, iter_part,                # which topic model
                   subset_part, bad_topics_part,                         # parts of that model
                   label_part)                                           # topics named or numbered

    if(for.filename) {
        main <- gsub(", ", "--", main)
  	    main <- gsub(" ", "_", main)
    }
    
    return(main)
}
