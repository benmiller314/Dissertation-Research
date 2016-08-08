#############################################################################
# 
# GOAL: use Andrew Goldstone's {dfrtopics} package to analyze / visualize
# a pre-existing topic model built in MALLET using, e.g., 
# my own `r2mallet with foreach.R` or `topic modeling 3.R` scripts
#
#######

library(dfrtopics)
library(mallet)
library(parallel)
library(dplyr)
library(foreach)
library(bigtabulate)

# hard-coded params for testing; later, make a wrapper function
dataset_name <- "consorts"
ntopics <- 55
iter_index <- ""
iter_index <- paste0("_", c(1:7))
instance_list <- file.path(tmloc, paste0(dataset_name, "_instances.mallet"))
ilist <- system(paste("echo", instance_list), intern=T)
file.exists(ilist)

i = iter_index
simplified_state_file <- file.path("~", "Documents", "tm", paste0(dataset_name, "K", ntopics, "_simplestate", i, ".csv"))
simple_state <- system(paste("echo", simplified_state_file), intern=T)
file.exists(simple_state)

# 15GB for just under half a 32GB-RAM MacPro, 3GB for an 8GB-RAM MacBook Air
	    heap_param <- paste("-Xmx","15g",sep="")
	    options(java.parameters=heap_param)

foreach(i = iter_index) %do% {
    mstate <- file.path("~", "Documents", "tm", paste0(dataset_name, "K", ntopics, "_topic-state", i, ".gz"))
    mstate <- system(paste("echo", mstate), intern=T)
    message(paste("Starting at", Sys.time(), "with", mstate))
    
    # about 12 minutes on desktop using 15GB RAM
    m <- load_from_mallet_state(mstate, simplified_state_file=simple_state, instances_file=ilist)
    
    # test whether the output problem is a Python2 / Python3 issue
    simplify_state_ben <- function (state_file, outfile) 
    {
        if (Sys.which("python") == "") {
            stop("This function requires python to run.")
        }
        scpt <- file.path(path.package("dfrtopics"), "python", "simplify_state.py")
        system2("python", args = c(scpt, state_file), stdout = outfile)
    }
    
    simplify_state_ben(mstate, simple_state)
    
    write_mallet_model(m, output_dir=file.path(tmloc, paste0("dfrtest", dataset_name, "K", ntopics, i)))
    
    # from ?export_browser_data: 
    # If you are working with non-JSTOR documents, the one file that will reflect
    # this is the exported metadata. dfr-browser expects seven metadata columns: 
    # id,title,author,journaltitle,volume,issue,pubdate,pagerange.
    # Note that you can adjust the metadata held on the model object by assigning to
    # metadata(m) before exporting the browser data. In particular, if you have many
    # documents, you may wish to conserve space by eliminating metadata columns that
    # are not used by the visualization: for example, metadata(m)$publisher <- NULL.
    
    metadata(m) <- get(dataset_name) %>%
        transmute(id = Pub.number, 
                  title = Title, 
                  authors = Author, 
                  university = School, 
                  date = as.Date(as.character(Year), "%Y")
                  )
    
    browser_dir <- file.path(tmloc, paste0("dfrtest", i), "browser")
    browser_dir <- file.path("~", "Box Sync", "research", "dissertations", "dfr-browser", "data")
    export_browser_data(m, out_dir=browser_dir, supporting_files = F, overwrite=T)
    message(paste("Browser data exported to", browser_dir, "at", Sys.time()))
}

m <- load_mallet_model(doc_topics_file = file.path(tmloc, "dfrtest", "doc_topics.csv"),
                        top_words_file = file.path(tmloc, "dfrtest", "top_words.csv"),
                        params_file = file.path(tmloc, "dfrtest", "params.txt"),
                        topic_words_file = file.path(tmloc, "dfrtest", "topic_words.csv"),
                        vocab_file = file.path(tmloc, "dfrtest", "vocabulary.txt"),
                        doc_ids_file = file.path(".", "Shell scripts and commands", "file list realconsorts.txt")
)

summary(m)

topic_labels(m, n=6)
m$doc_ids




top_docs(m2, 3) %>%
    filter(topic == 4) %>%
    select(-topic) %>%
    mutate(citation=cite_articles(metadata(m2)[doc, ]))




d <- read_diagnostics(file.path(tmloc, paste0(dataset_name, "k", ntopics, "diagnostics.xml")))
which.min(d$topics$corpus_dist)