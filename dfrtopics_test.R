#############################################################################
# 
# GOAL: use Andrew Goldstone's {dfrtopics} package to analyze / visualize
# a pre-existing topic model built in MALLET using, e.g., 
# my own `r2mallet with foreach.R` or `topic modeling 3.R` scripts
#
#######

# 15GB for just under half a 32GB-RAM MacPro, 3GB for an 8GB-RAM MacBook Air
heap_param <- paste("-Xmx","15g",sep="")
options(java.parameters=heap_param)

library(dfrtopics)
library(mallet)
library(parallel)
library(dplyr)
library(foreach)
library(bigtabulate)

# # hard-coded params for testing; later, make a wrapper function
# dataset_name <- "consorts"
# ntopics <- 55
# iter_index <- ""
# iter_index <- paste0("_", c(1:7))


dfrt_native <- function(dataset_name = "noexcludes2001_2015",
                        ntopics      = 60,
                        iter_index   = 4
                       )
{
    
}

dfrt_from_outside_model <- function(dataset_name = "noexcludes2001_2015",
                 ntopics      = 60,
                 iter_index   = 4
                 )
{

    dataset <- get(dataset_name)
    filename <- file.path(tmloc, paste0(dataset_name, "_doc_ids.txt"))
    if(file.exists(filename)) {
        dataset_ids <- read.table(filename)$V1    
    } else {
        stop("dfrtopics_test.R: could not find file ", filename)
    }
    
    
    
      
    foreach(i = iter_index) %do% {
        mstate <- file.path(tmloc, paste0(dataset_name, "k", ntopics, "_topic-state_", i, ".gz"))
        if(!file.exists(mstate)) {
            stop(paste("dfrtopics_test.R: could not find file ", mstate))
        }
    
        simple_state <- path.expand(file.path(tmloc, paste0(dataset_name, "k", ntopics, "_simplestate_", i, ".csv")))
        
        ilist <- file.path(tmloc, paste0(dataset_name, "_instances.mallet"))
        if(!file.exists(ilist)) {
            stop(paste("dfrtopics_test.R: could not find file ", ilist))
        }
        
        # Convert state file to dfrtopics-style mallet_model object, in prep for exporting to dfrbrowser
        message(paste("Starting at", Sys.time(), "from state file:\n ", mstate))
        m <- load_from_mallet_state(mstate, simplified_state_file=simple_state, instances_file=ilist)
        message(paste("Done at", Sys.time()))        # about 19 minutes for 60 topics and 2568 dissertations with 15GB RAM
        
        # check what we got
        summary(m)
        
        
        
        # save in dfr's preferred format for faster loading next time
        mydir <- paste0("dfrtest_", dataset_name, "k", ntopics, "_", iter_index)
        
        file.path(tmloc, mydir)
        
        system.time(
            write_mallet_model(m, file.path(tmloc, mydir))
        )
        
        # confirm that it worked
        m <- load_mallet_model(doc_ids_file = file.path(tmloc, mydir, "doc_ids.txt"),
                               doc_topics_file = file.path(tmloc, mydir, "doc_topics.csv"),
                               params_file = file.path(tmloc, mydir, "params.txt"),
                               state_file = file.path(tmloc, mydir, "state.csv"),
                               top_words_file = file.path(tmloc, mydir, "top_words.csv"),
                               topic_words_file = file.path(tmloc, mydir, "topic_words.csv"),
                               vocab_file = file.path(tmloc, mydir, "vocabulary.txt")
                               )
        
        # assign metadata (which doesn't work because...??)
        md <- dataset[which(dataset$Pub.number %in% dataset_ids),] %>%
            transmute(id = Pub.number, 
                      title = Title, 
                      authors = Author, 
                      university = School, 
                      date = as.Date(as.character(Year), "%Y")
            )
        
        metadata(m) <- md
        
        # if(!file.exists(simple_state)) {
        #     message(paste("Creating simplified state file:\n ", simple_state))
        #     message(paste("Starting at", Sys.time(), "from state file:\n ", mstate))
        #     simplify_state(mstate, simple_state)
        #     message(paste("Done at", Sys.time()))   # about 8 minutes for 60 topics and 2568 dissertations with 15GB RAM
        # }
        # 
    }    
}

if(autorun) {
    # dfrt()
}

if(false) {   # scratch space. stuff in here will never run.

    write_mallet_model(m, output_dir=file.path(tmloc, paste0("dfrtest", dataset_name, "k", ntopics, "_", i)))
    
    # from ?export_browser_data: 
    # If you are working with non-JSTOR documents, the one file that will reflect
    # this is the exported metadata. dfr-browser expects seven metadata columns: 
    # id,title,author,journaltitle,volume,issue,pubdate,pagerange.
    # Note that you can adjust the metadata held on the model object by assigning to
    # metadata(m) before exporting the browser data. In particular, if you have many
    # documents, you may wish to conserve space by eliminating metadata columns that
    # are not used by the visualization: for example, metadata(m)$publisher <- NULL.
    
    md <- get(dataset_name) %>%
        transmute(id = Pub.number, 
                  title = Title, 
                  authors = Author, 
                  university = School, 
                  date = as.Date(as.character(Year), "%Y")
                  )
    metadata(m) <- md    # fails; not sure why. No matter, we'll just save straight to the hard drive and zip manually.
    
    # Locations
    # browser_dir <- file.path(tmloc, paste0("dfrtest", i), "browser")
    browser_dir <- file.path("~", "Box Sync", "research", "dissertations", "dfr-browser", "data_2")
    mdfile <- file.path(browser_dir, "meta.csv")
    
    
    # Check for supporting_files
    if(file.exists(file.path(browser_dir, "..", "src", "VIS.js"))) {
        sf <- F
    } else {
        sf <- T
    }
    
    # Export
    export_browser_data(m, out_dir=browser_dir, supporting_files=sf, overwrite=T)
    write.csv(md, mdfile, row.names=F, col.names=F)
    zip(paste0(mdfile, ".zip"), path.expand(mdfile)) 
    # to do: delete the non-zipped file, move all this functionality into a function
    
    # Finish up
    message(paste("Browser data exported to", browser_dir, "at", Sys.time()))
    


m <- load_mallet_model(doc_topics_file = file.path(tmloc, "dfrtest", "doc_topics.csv"),
                        top_words_file = file.path(tmloc, "dfrtest", "top_words.csv"),
                        params_file = file.path(tmloc, "dfrtest", "params.txt"),
                        topic_words_file = file.path(tmloc, "dfrtest", "topic_words.csv"),
                        vocab_file = file.path(tmloc, "dfrtest", "vocabulary.txt"),
                        doc_ids_file = file.path(tmloc, paste0(dataset_name, "_doc_ids.txt"))
                        # doc_ids_file = file.path(".", "Shell scripts and commands", "file list realconsorts.txt")
)

summary(m)

topic_labels(m, n=6)
m$doc_ids




top_docs(m2, 3) %>%
    filter(topic == 4) %>%
    select(-topic) %>%
    mutate(citation=cite_articles(metadata(m2)[doc, ]))




d <- read_diagnostics(file.path(tmloc, paste0(dataset_name, "k", ntopics, "diagnostics.xml", "_", iter_index)))
which.min(d$topics$corpus_dist)

}
