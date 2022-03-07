################################################################################
#         `re-run all analyses.R`: This file is a run-down of the programs
#     that could re-create my figures from prepared data. For initial data
#     cleaning, I used GoogleRefine (now OpenRefine); see files in the
#     OpenRefine directory (Appendix A).
#
##

#####   Preparing the Working Environment  #####

## Set working directory to the location of R script files.
    setwd("~/Box Sync/research/dissertations/data, code, and figures/Dissertation-Research")

## Reload helper functions from personal RStudio setup
    source(file=".Rprofile")

## Specify the dataset and subsets you intend to work with when generating figures.
#
dataset_name <- "noexcludes2001_2015"
rcws_subset_name <- "knownprograms2001_2015"
nonrcws_subset_name <- "nonrcws2001_2015sans_badtops"
ntopics <- 50
iter_index <- 1
newnames <- FALSE

## Specify topic numbers determined to be non-content-bearing
bad.topics <- c("3", "8", "12", "15", "30", "34", "36", "47", "50")


## Specify method tags to use
tagset_name <- "no_ped_tagnames"

##
# Global variables called in many functions.
#       remake_figs: If TRUE, save new files for figures;
#                    if FALSE, display on screen only.
        remake_figs <- FALSE

#       autorun: If TRUE, call the functions when files are sourced;
#                if FALSE, load functions but do not call.
        autorun <- FALSE


##
# Global variables controlling how data is loaded and parsed initially.
# These condition how `dataprep.R` and `dataprep 2 - load data.R` behave.
#       dual_source: Set TRUE if merging an old datafile with a new datafile;
#                    set FALSE if loading an already-merged file
#                    (e.g. an exported version of noexcludes with realconsorts updated)
        dual_source <- FALSE

#       useped: Set TRUE if Pedagogical Projection should be counted as an
#               independent method; set FALSE if this move is not considered
#               part of the methodology schema or is folded in with Prac or Phil.
#               For the book, I set this to FALSE.
#
        useped <- FALSE

#       update_realconsorts: If TRUE, overwrite file list index of dissertations from
#                real consortium program dissertations, for text mining purposes.
        update_realconsorts <- FALSE

#       Are we working at home or at the office?
#       Some file locations and memory settings will vary.
        # which_computer <- "work"
        which_computer <- "laptop"

#       Try not to run out of memory, would you? Need to set this before loading the rJava VM.
        if (which_computer == "work") {
            heap_param <- paste("-Xmx","15g",sep="")
        } else if (which_computer == "laptop") {
            heap_param <- paste("-Xmx","3g",sep="")
        }
        options(java.parameters=heap_param)

##
# `dataprep.R`: prepares working environment by loading helper functions
#         and setting key variables (such as tagset).
#
# `dataprep 2 - load data.R`: loads in a .csv file of tagged
#     spreadsheet data, generates a tag array, and defines various
#     subsets. You will be prompted to select the file via file.choose().
#  Dependencies: "extract subjects.R", "Factor-Bug fixing.R",
#        "heatmap_ben.R", "heatmap fixedcols.R", "method tag array.R",
#        "thresh.R", "simplifying the schema.R", "check count.R",
#        library(data.table)
source(file="dataprep.R")
source(file="dataprep 2 - load data.R")



#####   Functions for Chapter Two: Topic Modeling

# Doc-topic grid, to establish weight of corpus/subcorpus
if(!exists("get.doctopic.grid")) { source(file="get doctopic grid.R") }
dtgrid <- get.doctopic.grid(dataset_name=dataset_name, ntopics=ntopics,
                            iter_index=iter_index)$outputfile.dt
dtgrid <- na.omit(dtgrid)
dtgrid <- dtgrid[, setdiff(names(dtgrid), bad.topics), with=F]

# Topic-word tables, used to establish clusters of topics and as input for TF-ITF
if(!exists("build.topicword.table")) { source(file="get_topic_word_grid.R") }
tw <- build.topicword.table(dataset_name=dataset_name,
                            ntopics=ntopics,
                            iter_index=iter_index,
                            newnames=newnames,
                            bad.topics=bad.topics)

# TF-ITF weighting of keywords, for topic labels
if(!exists("tfidf.for.topics")) { source(file="tfidf for topics.R") }
tf <- tfidf.for.topics(tw=tw)



### Figure 2.2. Multiple topics are typically needed ###
#  to account for even half of a dissertation's text.

if(!exists("topic.proportions", mode="function")) { source(file="variation of topic proportions.R") }
topic.proportions(dataset_name = dataset_name,
                  ntopics = ntopics,
                  iter_index = iter_index,
                  subset_name = rcws_subset_name,
                  bad.topics = bad.topics,
                  filetype = ".tiff")


### Figure 2.3. Writing Centers as top topic ###

if(!exists("get_top_topics") || !exists("top_topics_comparison")) {
    source(file="variation of topic proportions.R")
}
top_topics_rcws <- get_top_topics(dataset_name = dataset_name,
                             ntopics = ntopics,
                             subset_name = rcws_subset_name,
                             bad.topics = bad.topics,
                             grid = dtgrid)

top_topics_comparison(mytopics = 27,
                      top_topics = top_topics_rcws,
                      dataset_name = dataset_name,
                      ntopics = ntopics,
                      iter_index = iter_index,
                      subset_name = rcws_subset_name,
                      bad.topics = bad.topics,
                      dt = dtgrid,
                      filetype = ".tiff")


### Figure 2.4. Technical Communication as top topic ###
if(!exists("top_topics_comparison")) { source(file="variation of topic proportions.R") }

top_topics_comparison(mytopics = 1,
                      top_topics = top_topics_rcws,
                      dataset_name = dataset_name,
                      ntopics = ntopics,
                      iter_index = iter_index,
                      subset_name = rcws_subset_name,
                      bad.topics = bad.topics,
                      dt = dtgrid,
                      filetype = ".tiff")


### Figure 2.5. Agglomerative clustering of topics ###

# Distance between clusters
if(!exists("topic_distance_matrix", mode="function")) {
    source(file="topic_term_synonyms.R")
    source(file="topic_term_synonyms.R") # here twice to set the WordNet 'dict' directory
}
twm <- topic_distance_matrix(dataset_name = dataset_name,
                             ntopics = ntopics,
                             iter_index = iter_index,
                             bad.topics = bad.topics,
                             tw=tw)



i(!exists("frameToJSON")) { source(file = "frameToD3.R") }

# NB: The output will need to be cleaned up in Illustrator: rotate and fix label overlap
frameToJSON(dataset_name = dataset_name,
            ntopics = ntopics,
            iter_index = iter_index,
            subset_name = rcws_subset_name,
            clust.method = "agnes",
            do.plot = T,
            use.labels = T,
            tw = tw,
            twm = twm,
            dt = dtgrid)

### Figure 2.6. Divisive clustering of topics ###

# NB: The output will need to be cleaned up in Illustrator: rotate and fix label overlap
frameToJSON(dataset_name = dataset_name,
            ntopics = ntopics,
            iter_index = iter_index,
            subset_name = rcws_subset_name,
            clust.method = "diana",
            do.plot = T,
            use.labels = T,
            tw = tw,
            twm = twm,
            dt = dtgrid)


#####   Chapter Three: Dissertation Methods    #####

require(viridisLite)

colorscheme <- list(name = "magma", values = c("#FFFFFF", magma(99, dir=-1)))

if(!exists("sumbytags", mode="function")) {
    source(file = "method collocation heatmap.R")
}

# method-method correlation matrix, to order method variables consistently by cluster
method_corrs_rcws <- sumbytags(rcws_subset_name,
                               tagset_name,
                               doplot=T,
                               normed=T,
                               dendro=T)

# method-school correlation matrix

if(!exists("schoolwise.data", mode="function")) {
    source(file = "method tags by school.R")
}
school_corrs <- schoolwise.data(rcws_subset_name, tagset_name)



###    Figure 3.2. Method tag count for a program with higher-than-average empirical focus    ###

if(!exists("method_corrs_one_row", mode="function")) {
    source(file = "method collocation heatmap.R")
}

method_corrs_one_row(myrow = "New Mexico State University-Main Campus",
                     corr_type = "school",
                     dataset_name = rcws_subset_name,
                     tagset_name = tagset_name,
                     color_groups = T,
                     taggroups = no_ped_taggroups,
                     normed = F,
                     corr_obj = school_corrs,
                     colInd = method_corrs_rcws$colInd,
                     include_legend = TRUE,      # same legend as Figure 3.1
                     include_count = FALSE,
                     filetype = ".tiff")

###    Figure 3.3. Method tag count for a program with higher-than-average dialectical focus    ###

if(!exists("method_corrs_one_row", mode="function")) {
    source(file = "method collocation heatmap.R")
}

method_corrs_one_row(myrow = "Pennsylvania State University-Main Campus",
                     corr_type = "school",
                     dataset_name = rcws_subset_name,
                     tagset_name = tagset_name,
                     color_groups = T,
                     taggroups = no_ped_taggroups,
                     normed = F,
                     corr_obj = school_corrs,
                     colInd = method_corrs_rcws$colInd,
                     include_legend = TRUE,      # same legend as Figure 3.1
                     include_count = FALSE,
                     filetype = ".tiff")

###    Figure 3.5. Normed heatmap of methodological focus within schools    ###


if(!exists("schoolwise", mode="function")) {
    source(file = "method tags by school.R")
}

require(viridisLite)
colorscheme <- list(name = "magma", values = c("#FFFFFF", magma(99, dir=-1)))

schoolwise(dataset_name = "knownprograms2001_2015",
           tagset_name = "no_ped_tagnames",
           show.totals = T,
           measure = "normed",
           myclustfun = "diana",
           mycolorder = method_corrs_rcws$Colv,
           myCol = if(is.null(colorscheme$values)) colorscheme[[i]]$values else colorscheme$values,
           filename_suffix = if(is.null(colorscheme$name)) colorscheme[[i]]$name else colorscheme$name,
           min_disses = 5
)

###    Figure 3.7. Method tag spread across schools    ###

if(!exists("method_spread_across_schools", mode="function")) {
    source(file = "method collocation heatmap.R")
}
method_spread_across_schools(subset_name,
                             tagset_name,
                             horizontal = F,
                             show_counts = F,
                             show_legend = F,
                             filetype = ".tiff")



###    Figure 3.8. Raw counts of method correlations within dissertations    ###

if(!exists("sumbytags", mode="function")) { source(file = "method collocation heatmap.R") }

# Need to take this to Illustrator
sumbytags(rcws_subset_name,
          tagset_name,
          doplot=T,
          normed=F,
          dendro=T,
          legend=F,
          filetype = ".pdf")


###    Figure 3.9. Scaled counts of method correlations within dissertations    ###

if(!exists("sumbytags", mode="function")) { source(file = "method collocation heatmap.R") }

# Need to take this to Illustrator
sumbytags(rcws_subset_name,
          tagset_name,
          doplot=T,
          normed=T,
          dendro=T,
          legend=F,
          filetype = ".pdf")

###    Figure 3.10. Rates of Method Co-occurrence for Dissertations tagged Philosophical / Theoretical    ###

method_corrs_one_row(myrow = "Phil",
                     corr_type = "method",
                     dataset_name = rcws_subset_name,
                     tagset_name = tagset_name,
                     taggroups = taggroups,
                     normed = T,
                     color_groups = T,
                     colInd = method_corrs_rcws$colInd,
                     include_legend = T,
                     include_count = F,
                     filetype = ".tiff")


###    Figure 3.11. Rates of Method Co-occurrence for Dissertations tagged Meta-Analytical / Discipliniographic    ###

method_corrs_one_row(myrow = "Meta",
                     corr_type = "method",
                     dataset_name = rcws_subset_name,
                     tagset_name = tagset_name,
                     taggroups = taggroups,
                     normed = T,
                     color_groups = T,
                     colInd = method_corrs_rcws$colInd,
                     include_legend = T,
                     include_count = F,
                     filetype = ".tiff")

###    Figure 3.12. Rates of Method Co-occurrence for Dissertations tagged Model-Building    ###

method_corrs_one_row(myrow = "Modl",
                     corr_type = "method",
                     dataset_name = rcws_subset_name,
                     tagset_name = tagset_name,
                     taggroups = taggroups,
                     normed = T,
                     color_groups = T,
                     colInd = method_corrs_rcws$colInd,
                     include_legend = T,
                     include_count = F,
                     filetype = ".tiff")


##### Chapter 4: Comparing RCWS and non-RCWS #####

###    Figure 4.1. Method tag frequency comparison    ###

if(!exists("compare_method_ranks")) { source(file = "compare method ranks.R") }

compare_method_ranks(set1 = rcws_subset_name,
                     set2 = nonrcws_subset_name,
                     betterlabels = c("Confirmed RCWS dissertations",
                                    "Confirmed non-RCWS dissertations"),
                     tagset_name = tagset_name,
                     verbose = F,
                     filetype = ".pdf")  # add colors in Illustrator


###    Figure 4.2. Scaled counts of method correlations in non-RCWS dissertations    ###

if(!exists("sumbytags", mode="function")) { source(file = "method collocation heatmap.R") }

# Need to take this to Illustrator
method_corrs_nonrcws <- sumbytags(dataset_name = nonrcws_subset_name,
                                  tagset_name = tagset_name,
                                  doplot=T,
                                  normed=T,
                                  dendro=F,
                                  legend=F,
                                  rowInd = method_corrs_rcws$rowInd,
                                  colInd = method_corrs_rcws$colInd,
                                  filetype = ".pdf")

###    Figure 4.3. Change in Methodological Pairing Likelihood, RCWS vs non-RCWS    ###

if(!exists("method_corr_diffs", mode="function")) { source(file = "method collocation heatmap.R") }

method_corr_diffs(set1 = "knownprograms2001_2015",
                  set2 = "nonrcws2001_2015sans_badtops",
                  tagset_name = "no_ped_tagnames")


##### Chapter 5: Things Change Over Time #####

###    Figure 5.1. Three topics converging    ###

if(!exists("topics.by.year", mode="function")) { source(file = "topics by year.R") }

topics.by.year(smoothing = 1/2, 
               to.plot = c(32,  # disciplinary formations (decreasing)
                           35,  # institutional supports, barriers, constraints (stable) 
                           44), # online circulation and social media (increasing)
               legendloc_init="topright",
               filetype = ".tiff")


#####   Functions for Determining the Scope of the Data    #####

#   `schools per year.R`: for each year, find the number of
#         institutions and number of dissertations; optionally plot these
#         numbers as a line graph (defaults to true). Provides one function:
#         peryear(dataset_name, do.plot)
source(file="schools per year.R")

##
#   `map by school 4 (comp-rhet superimposed on carnegie
#         schools).R`: Produces a geographical map of three kinds of data
#         points: schools with a Carnegie Classification of doctoral
#         institution; schools with programs in the Consortium of Doctoral
#         Programs in Rhetoric and Composition; and schools where one of
#         the 2,711 dissertations in my dataset were completed.
#   Dependencies: package(maps), package(mapdata),
#         package(mapplots), package(maptools), package(scales), map by
#         school 1 (setup).R, carnegie 1 (setup).R, geocode.R
source(file="map by school 4 (comp-rhet superimposed on carnegie schools).R")



#####   Programs for Analyzing Dissertation Methods     #####

##
#   `tags by school.R`: generates heat plots of methods used in
#     dissertations, aggregated by school. Provides two functions:
#       * schoolwise.data(dataset_name, tagset_name): returns a list of
#            tag means, sums, and counts, each aggregated by school.
#       * schoolwise(dataset_name, tagset_name, ...): make one or more
#        heatplots from the output of schoolwise.data(). Dependencies:
#        library(doBy), library(cluster), library(RColorBrewer)
source(file="tags by school.R")

##
#   `methodcount barplot.R`: produces a bar plot of method-tag
#         counts per dissertation, for a given method tagset.
#         Provides one function:
#       * methods.barplot(dataset_name, tagset_name)
source(file="methodcount barplot.R")

##
#   `subject terms barplot.R`: produces a bar plot of author-provided
#     subject terms counts, by overall frequency. Provides one function:
#       * subject.barplot(dataset_name, how.many, ...): graphs the top
#           how.many
#   `keyword barplot.R`: produces a bar plot of author-provided
#     keyword-tag counts, by overall frequency. Median frequency turns out
#     to be 1, making this figure visually not so different from empty axes.
source(file="subject terms barplot.R")
source(file="keyword barplot.R")

##
#   `frequency of method tags.R`: tabulates and plots the number
#         of times a dissertation is tagged with each method. Provides
#         three functions:
#       * get_tags(dataset_name, tagset_name): returns a named
#            vector of frequencies for each method in the tagset
#       * methodfreq_combined(bigset, smallset, diffset): plots an
#            overlaid horizontal bar graph of method frequencies; by
#            default the three sets are noexcludes, consorts, and
#            nonconsorts, respectively (but others are possible).
#       * compare_method_ranks(set1, set2, ...): creates a
#             side-by-side plot of methods in descending rank order, with
#             lines connecting the same methods to quickly reveal changes
#             in rank across the two sets.
source(file="frequency of method tags.R")


##
#   `top schools by method.R`: For each method in a given
#         tagset, produces a list of the top X schools by either
#         methodological output (number of dissertations using that method
#         at that school) or methodological focus (percentage of
#         dissertations using that method at that school). Provides one
#         function:
#       * toplists(dataset_name, tagset_name, howmany, threshold, ...)
source(file="top schools by method.R")


##
#   `method collocation heatmap.R`: If a dissertation is tagged X, how
#         many times is it also tagged Y? Provides one function:
#           * sumbytags(dataset_name, tagset_name, doplot, normed, dendro):
#         Aggregates methods tags by each method tag, with an option to
#         norm by dividing the sums by the aggregating method's total
#         count. Optionally plots a heatmap of results as an adjacency
#         matrix.
#   Dependencies: heatmap_ben.R
source(file="method collocation heatmap.R")


#####       Functions for Topic Modeling        #####
#   generate a (series of) topic model(s); more documentation in each file
source(file="r2mallet with foreach.R")
source(file="topic modeling 3.R")


## Tools for topic exploration ##

        ## TO DO: Add new stuff since summer 2019, e.g. `inspect topic models.R`


##
#   `top docs per topic.R`: browse topics to generate labels. Provides four
#   functions:
#         * get.doc.composition(dataset, ntopics): retrieves a pre-existing
#           matrix, output by MALLET, with topic proportions for each
#           document in corpus
#         * get.topics4doc(pubnum, dataset_name, ntopics, howmany,
#           showlabels): retrieves top `howmany` topics for a document
#           specified by `pubnum`.
#         * top_topic_browser(...): for a specified topic or range of topics,
#           shows the top `howmany` documents and their method tags, with
#           optional detail view showing top topics for each document at a
#           time. Parameters include start.rank, topic, dataset_name, ntopics,
#			depth, showlabels, etc.
#         * shareable_topic(topic, ...): Given a topic of interest, compiles
#           clean data to share with others about the top `depth` docs from
#           that topic; outputs a .csv file when remake_figs is true.
#   Dependencies: get doctopic grid.R, get topickeys.R, get topic labels.R
source(file="top docs per topic.R")


#   retrieve topic information about a dissertation by author name
source(file="get topics for author.R")

#   find topics that co-occur within documents
source(file="cotopics.R")

#   get weights of every topic for all documents
source(file="get doctopic grid.R")

#   find dissertations with high levels of a cluster of topics
source(file="topic cluster reach.R")

##
#   `frameToD3.R`: outputs JSON file of topic model data for
#         interactive visualizations. Provides two functions:
#       * frameToJSON(dataset_name, ntopics, do.plot, groupVars, dataVars,
#         outfile, bad.topics): given a topic model as generated by
#         'r2mallet with foreach.R', returns a hierarchical clustering of
#         topics in JSON. For each topic, includes the following metadata:
#         name, size, scaledsize, topwords, topic, rank.
#       * cotopic_edges(dataset_name, ntopics, level, min, outfile,
#         bad.topics): given a topic model as generated by 'r2mallet with
#         foreach.R', returns weighted edges between topics and the same
#         hierarchical clustering as above.
source(file="frameToD3.R")

##
# `topics by year.R`: rank topics overall, aggregated per year.
#           Provides two functions:
#       * topics.by.year(dataset_name, ntopics, to.plot, do.plot,
#        per.plot): charts the rising and falling contributions to the
#        corpus of each topic, or topics specified in to.plot, over time.
#        Invisibly returns a dataframe of these contributions (as `df`)
#        and a list of topics by descending order of total contribution
#        (as `rank.order`).
#       * topic.variation(dataset_name, ntopics, to.plot): creates a barplot of
#        yearly variation of topics.
#
#   Dependencies: get doctopic grid.R, get topic labels.R
source(file="topics by year.R")

##
# `variation of topic proportions.R`: Find out the curve of topic
# strengths within each document, i.e. how much of the document is the top
# topic? how much is the second? and so on. Provides one function:
#       * topic.proportions(dataset_name, ntopics, bad.topics,
#         use.notch, explore.outliers): produces a boxplot of contribution
#         (y-axis) sorted by topic rank (x-axis), aggregated over all
#         documents. If explore.outliers is true, prints a table of upper
#         outlier values, the topics generating them, and their labels,
#         then starts a browser for dissertations represented in that
#         table. Returns boxplot statistics for the top three topics.
#   Dependencies:
#         package(data.table), get doctopic grid.R, get topic labels.R,
#         top docs per topic.R
source(file="variation of topic proportions.R")


##
# `single topic strength vs rank.R`: are overall top topics
# high-ranked in few documents, or evenly spread out? Provides one
# function:
#       * strength_v_rank(my.topic, dataset_name, ntopics, bad.topics):
#         produces a scatterplot of one selected topic's contributions, with
#         percent of dissertation on the y-axis and rank within dissertation on
#         the x-axis.
#   Dependencies: package(data.table), package(RColorBrewer),
#         get doctopic grid.R, get topic labels.R
source(file="single topic strength vs rank.R")


#####       Other Functions       #####

##
# `word_counts.R`: how long is the average abstract? has it changed over time?
# Provides three functions:
#       * wc(f): counts words in f by split()ing at spaces.
#       * wc_column(dataset, column, do.plot, print.summary): makes a boxplot
#         of word counts in one column of dataset. Not recommended for non-text columns.
#       * wc_timeplot(dataset, column, rawavg, smoothavg): assuming that dataset has
#         a column "Year", produces a scatterplot of wc_column data divided out by year.
#         optionally overlays a trendline, using raw means or smoothed medians (the default).
source(file="word_counts.R")
