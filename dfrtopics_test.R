#############################################################################
#
# GOAL: use Andrew Goldstone's {dfrtopics} package to analyze / visualize
# a pre-existing topic model built in MALLET using, e.g.,
# my own `r2mallet with foreach.R` or `topic modeling 3.R` scripts
#
#######


## Set up stable elements of the working environment
# 	# Let's assume we're typically going to need more Java heap space;
#   NB: this is set within MALLET in $malletloc/bin/mallet,
#   on line 10: "MEMORY=" etc and in $malletloc/bin/mallet.bat, line 14, "set MALLET_MEMORY="
#   But you do seem to need to set it with R, too.

# IMPORTANT: set heap limit before loading mallet, bc mallet loads java,
# and once the VM is set, it's stuck.
heap_param <- paste("-Xmx","15g",sep="")
options(java.parameters=heap_param)

# install.packages("devtools")
# devtools::install_github("agoldst/dfrtopics")
library(dfrtopics)
library(mallet) # did you raise the heap limit?
library(dplyr)
library(XML)
library(parallel)
library(foreach)
library(bigtabulate)


dfrt_native <- function(dataset_name = "noexcludes2001_2015",
                        ntopics      = 60,
                        iter_index   = 4
                       )
{

}

dfrt_from_outside_model <- function(dataset_name = "noexcludes2001_2015",
                 ntopics      = 60,
                 iter_index   = 4,
                 test_mode    = FALSE,
                 newnames     = FALSE       # where in the MALLET output filename
                                            # does iter_index appear?
                                     # set T if it's with the model, F if last in filename.
                                     # Gets passed into get.doctopic.grid.
                 )
{
    # figure out which files/Pub.numbers are in this model
    dataset <- get(dataset_name)
    filename <- file.path(sourceloc, "subsets", paste0(dataset_name, "_doc_ids.txt"))
    if(file.exists(filename)) {
        dataset_ids <- read.table(filename)$V1
        length(dataset_ids)
    } else {
        stop("dfrtopics_test.R: could not find doc_ids file ", filename)
    }

    # For each iteration, read in state file and convert to dfrtopics format
    foreach(i = iter_index) %do% {

        model_name <- paste0(dataset_name, "k", ntopics, "_", i)

        # locate state file from Mallet, which is required
        if(newnames) { mstate <- file.path(tmloc, paste0(model_name, "_topic-state.gz"))
        } else { mstate <- file.path(tmloc, paste0(dataset_name, "k", ntopics, "_topic-state_", i, ".gz")) }

        if(!file.exists(mstate)) {
            stop(paste("dfrtopics_test.R: could not find file ", mstate))
        }

        # locate Mallet's instances list, i.e. the pre-model list of tokens
        ilist <- file.path(tmloc, paste0(dataset_name, "_instances.mallet"))
        if(!file.exists(ilist)) {
            stop(paste("dfrtopics_test.R: could not find file ", ilist))
        }

        # locate simplified state file, which dfrtopics creates for a speed bonus
        if(newnames) { simple_state <- path.expand(file.path(tmloc, paste0(model_name, "_simplestate.csv")))
        } else { simple_state <- path.expand(file.path(tmloc, paste0(dataset_name, "k", ntopics, "_simplestate_", i, ".csv"))) }
        if(!file.exists(simple_state)) {
            stop(paste("dfrtopics_test.R: could not find file ", simple_state))
        }

        # locate doc-topic matrix
        dfr_doc_topics <- get.doctopic.grid(dataset_name=dataset_name, ntopics=ntopics, iter_index=iter_index, newnames=newnames)$outputfile

        # locate topic-words matrix
        if(newnames) { topic_words <- path.expand(file.path(tmloc, paste0(model_name, "_wordtopics.txt")))
        } else { topic_words <- path.expand(file.path(tmloc, paste0(dataset_name, "k", ntopics, "_wordtopics_", i, ".txt")))}
        if(!file.exists(topic_words)) {
            stop(paste("dfrtopics_test.R: could not find file ", topic_words))
        }

        # locate file with top words per topic
        if(newnames) { keys <- path.expand(file.path(tmloc, paste0(model_name, "_keys.txt")))
        } else { keys <- path.expand(file.path(tmloc, paste0(dataset_name, "k", ntopics, "_keys_", i, ".txt")))}


        # # if simple state file exists, load from that
        # if(file.exists(simple_state)) {
        #     message(paste("Starting at", Sys.time(), "from simplified state file:\n ", simple_state))
        #         system.time(
        #         m <- load_mallet_model(state_file = simple_state,
        #                                doc_topics_file = doc_topics,
        #                                vocab = ilist,
        #                                doc_ids_file = dataset_ids,
        #                                topic_words_file = topic_words,
        #                                top_words_file = keys
        #                                )
        #         )
        #     message(paste("Done at", Sys.time()))
        # } else {

            # If not, make it now: convert full (gzipped) state file to dfrtopics-style mallet_model object,
            # in prep for exporting to dfrbrowser
            message(paste("Starting at", Sys.time(), "from state file:\n ", mstate))
            system.time(
                m <- load_from_mallet_state(mstate, simplified_state_file=simple_state, instances_file=ilist)
            )
            message(paste("Done at", Sys.time()))        # about 19 minutes for 60 topics and 2568 dissertations with 15GB RAM

        # }
        # check what we got
        summary(m)


        # affiliate metadata
        md <- dataset[which(dataset$Pub.number %in% dataset_ids),] %>%
            transmute(id = Pub.number,
                      title = Title,
                      authors = Author,
                      university = School,
                      date = as.Date(as.character(Year), "%Y"),
                      methods = Method.Terms,
                      abstract = ABSTRACT,
                      url = Link
            )

        metadata(m) <- md

        # export browser data so we don't have to take 40 minutes every time!

        # from ?export_browser_data:
        # If you are working with non-JSTOR documents, the one file that will reflect
        # this is the exported metadata. dfr-browser expects seven metadata columns:
        # id,title,author,journaltitle,volume,issue,pubdate,pagerange.
        # Note that you can adjust the metadata held on the model object by assigning to
        # metadata(m) before exporting the browser data. In particular, if you have many
        # documents, you may wish to conserve space by eliminating metadata columns that
        # are not used by the visualization: for example, metadata(m)$publisher <- NULL.

        # Locations
        browser_dir <- file.path(home_dir, "research", "dissertations", "dfr-browser", model_name)
        mdfile <- file.path(browser_dir, "meta.csv")

        # Check for supporting_files
        if(file.exists(file.path(browser_dir, "..", "src", "VIS.js"))) {
            sf <- F
        } else {
            sf <- T
        }

        # Export
        if(remake_figs) {
            overwrite <- T
        } else {
            overwrite <- F
        }

        system.time(
            export_browser_data(m, out_dir=browser_dir, supporting_files=sf, overwrite=T)
        )

        write.csv(md, path.expand(mdfile), row.names=F, col.names=F)
        # zip(paste0(mdfile, ".zip"), c(path.expand(mdfile)))
        # to do: delete the non-zipped file, move all this functionality into a function
        # NB: This zip fails, recreating the full folder path inside the .zip archive. For now, manually zip in Finder.

        # Finish up
        message(paste("Browser data exported to", browser_dir, "at", Sys.time()))




        # output source files in dfr's preferred format for faster loading next time
        mydir <- file.path(tmloc, paste0("dfrtest_", model_name))

        system.time(
            write_mallet_model(m, mydir, simplify_state = F)      # we already saved the simple state via load_from_mallet_state
                                                        # (though we'll need to change the default location to get it later)
        )

        if(test_mode) {
            # confirm that it worked
            system.time(
            m2 <- load_mallet_model(doc_ids_file = file.path(tmloc, mydir, "doc_ids.txt"),
                               doc_topics_file = file.path(tmloc, mydir, "doc_topics.csv"),
                               params_file = file.path(tmloc, mydir, "params.txt"),
                               state_file = file.path(tmloc, mydir, "state.csv"),
                               top_words_file = file.path(tmloc, mydir, "top_words.csv"),
                               topic_words_file = file.path(tmloc, mydir, "topic_words.csv"),
                               vocab_file = file.path(tmloc, mydir, "vocabulary.txt")
                               )
            ) # end of system.time

            summary(m2)
        }



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

######################################################################
##### scratch space. stuff below here will never run on its own. #####
######################################################################

if(false) {

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
    # TO DO: DESIGNATE MORE MEANINGFUL/ITERABLE DATA FOLDER NAMES
    #        (browser can be pointed to them using src/VIS.js)
    # browser_dir <- file.path(tmloc, paste0("dfrtest", i), "browser")
    browser_dir <- file.path(home_dir, "research", "dissertations", "dfr-browser", "data_2")
    mdfile <- file.path(browser_dir, "meta.csv")


    # Check for supporting_files
    if(file.exists(file.path(browser_dir, "..", "src", "VIS.js"))) {
        sf <- F
    } else {
        sf <- T
    }

    # Export
    export_browser_data(m, out_dir=browser_dir, supporting_files=sf, overwrite=T)
    write.csv(md, path.expand(mdfile), row.names=F, col.names=F)
    zip(paste0(mdfile, ".zip"), c(path.expand(mdfile)))
    # to do: delete the non-zipped file, move all this functionality into a function

    # Finish up
    message(paste("Browser data exported to", browser_dir, "at", Sys.time()))



m <- load_mallet_model(doc_topics_file = file.path(tmloc, "dfrtest", "doc_topics.csv"),
                        top_words_file = file.path(tmloc, "dfrtest", "top_words.csv"),
                        params_file = file.path(tmloc, "dfrtest", "params.txt"),
                        topic_words_file = file.path(tmloc, "dfrtest", "topic_words.csv"),
                        vocab_file = file.path(tmloc, "dfrtest", "vocabulary.txt"),
                        doc_ids_file = file.path(sourceloc, "subsets", paste0(dataset_name, "_doc_ids.txt"))
                        # doc_ids_file = file.path(".", "Shell scripts and commands", "file list realconsorts.txt")
)

summary(m)

topic_labels(m, n=6)
m$doc_ids




top_docs(m2, 3) %>%
    filter(topic == 4) %>%
    select(-topic) %>%
    mutate(citation=cite_articles(metadata(m2)[doc, ]))



diag_file <- file.path(tmloc, paste0(dataset_name, "k", ntopics, "_diagnostics_", iter_index, ".xml"))
d <- read_diagnostics(diag_file)
which.min(d$topics$corpus_dist)





# where'd you write_mallet_model files in dfr's preferred format?
mydir <- file.path(tmloc, paste0("dfrtest_", model_name))

if(!file.exists(file.path(mydir, "doc_ids.txt"))) {
    writeLines(levels(dataset_ids), file.path(mydir, "doc_ids.txt"))
}

# try loading them from there
system.time(
    m2 <- load_mallet_model_directory(mydir, load_topic_words = T)
)







}
