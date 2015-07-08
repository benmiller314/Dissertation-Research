################################################################################
# Overview of analyses 
# This file is a run-down of the programs that could re-create my figures from prepared data.
# For initial data cleaning, I used GoogleRefine (now OpenRefine); see files in the OpenRefine directory.


# Set working directory to the location of R script files.
setwd("/Users/benmiller314/Dropbox/coursework, etc/dissertation/data, code, and figures/Dissertation Research")

## 
# Global variables called in many functions.
# remake_figs: If TRUE, save new files for figures; if FALSE, display on screen only.
# autorun: If TRUE, call the functions when files are sourced; if FALSE, load functions but do not call.
remake_figs <- FALSE; 
autorun <- FALSE;

## 
# `dataprep`: prepares working environment by loading helper functions and setting key variables (such as tagset).
# `dataprep 2`: loads in a .csv file of tagged spreadsheet data, generates a tag array, and defines various subsets.
# 	You will be prompted to select the file via file.choose().
# Dependencies:
#	"extract subjects.R", "Factor-Bug fixing.R", "heatmap_ben.R", "heatmap fixedcols.R", "method tag array.R", "thresh.R", "simplifying the schema.R", "check count.R", library(data.table)
# NB: These two functions can be called jointly via `source(file="start here.R")`
source(file="dataprep.R")
source(file="dataprep 2 - load data.R")

## 
# `tags by school`: generates heat plots of methods used in dissertations, aggregated by school.
# 	Provides two functions:
#	schoolwise.data(dataset_name, tagset_name): returns a list of tag means, sums, and counts,
#		each aggregated by school.
#	schoolwise(dataset_name, tagset_name, ...): make one or more heatplots from the output of schoolwise.data().
# Dependencies:
#	library(doBy), library(cluster), library(RColorBrewer)
source(file="tags by school.R")

## 
# `methodcount barplot`: produces a bar plot of method-tag counts per dissertation, for a given method tagset.
# `keyword barplot`: produces a bar plot of author-provided keyword-tag counts per dissertation. Not so useful.
source(file="methodcount barplot.R")
# source(file="keyword barplot.R")

## 
# `frequency of method tags`: tabulates and plots the number of times a dissertation is tagged with each method.
# Provides three functions:
#	get_tags(dataset_name, tagset_name): returns a named vector of frequencies for each method in the tagset
#	methodfreq_combined(bigset, smallset, diffset): plots an overlaid horizontal bar graph of method frequencies;
#		by default the three sets are noexcludes, consorts, and nonconsorts, respectively (but others are possible).
# 	compare_method_ranks(set1, set2, ...): creates a side-by-side plot of methods in descending rank order, 
#		with lines connecting the same methods to quickly reveal changes in rank across the two sets.
source(file="frequency of method tags.R")


## 
# `top schools by method`: For each method in a given tagset, produces a list of the top X schools by either 
# methodological output (number of dissertations using that method at that school) or methodological focus 
# (percentage of dissertations using that method at that school). Provides one function: 
#	toplists(dataset_name, tagset_name, howmany, threshold, ...)
source(file="top schools by method.R")	


## 
# `collocation heatmap`: If a dissertation is tagged X, how many times is it also tagged Y? 
# Provides one function:
#   sumbytags(dataset_name, tagset_name, doplot, normed, dendro): Aggregates methods tags by each method tag,
#	   with an option to norm by dividing the sums by the aggregating method's total count. 
#	   Optionally plots a heatmap of results as an adjacency matrix. 
# Dependencies:
#   ben_heatmap.R
source(file="collocation heatmap.R")

# `schools per year`: for each year, finds the number of institutions and number of dissertations.
# Optionally plots these numbers as a line graph
source(file="schools per year.R") 

## 
# `map by school 4 (comp-rhet superimposed on carnegie schools).R`: The title says what it does. This is one of the 
#	first files I created, and runs automatically when called, including spitting out some side-effect variables.
#	This situation needs fixing.
# Dependencies:
#	package(maps), package(mapdata), package(mapplots), package(maptools), package(scales),
#	map by school 1 (setup).R, carnegie 1 (setup).R, geocode.R
source(file="map by school 4 (comp-rhet superimposed on carnegie schools).R")

#####  Topic Modeling functions  #####
source(file="r2mallet with foreach.R")				# generate a (series of) topic model(s)
source(file="top docs per topic.R")					# browse topics to generate labels
source(file="get topics for author.R")				# retrieve topic information about a dissertation by author name
source(file="cotopics.R")							# find topics that co-occur within documents
source(file="get doctopic grid.R")					# get weights of every topic for all documents

##
# `frameToD3`: outputs JSON file of topic model data for interactive visualizations.
# Provides two functions:
#	frameToJSON(dataset_name, ntopics, do.plot, groupVars, dataVars, outfile, bad.topics): given a topic model 
#	   as generated by 'r2mallet with foreach.R', returns a hierarchical clustering of topics in JSON. 
#	   For each topic, includes the following metadata: name, size, scaledsize, topwords, topic, rank.
#	cotopic_edges(dataset_name, ntopics, level, min, outfile, bad.topics): given a topic model as generated 
#	   by 'r2mallet with foreach.R', returns weighted edges between topics and the same hierarchical clustering as above. 
source(file="frameToD3.R")							

##
# `topics by year`: rank topics overall, aggregated per year. 
# Provides two functions:
#   topics.by.year(dataset_name, ntopics, to.plot, do.plot, per.plot): charts the rising and falling contributions 
#	   to the corpus of each topic, or topics specified in to.plot, over time. Invisibly returns a dataframe of 
#	   these contributions (as `df`) and an list of topics by descending order of total contribution (as `rank.order`).
#   topic.variation(dataset_name, ntopics, to.plot): creates a barplot of yearly variation of topics.
# Dependencies:
#   get doctopic grid.R, get topic labels.R
source(file="topics by year.R")						

##
# `variation of topic proportions`: Find out the curve of topic strengths within each document, 
# i.e. how much of the document is the top topic? how much is the second? and so on. 
# Provides one function:
#   topic.proportions(dataset_name, ntopics, bad.topics, use.notch, explore.outliers): produces a boxplot 
#	   of contribution (y-axis) sorted by topic rank (x-axis), aggregated over all documents. If explore.outliers
#	   is true, prints a table of upper outlier values, the topics generating them, and their labels, then starts
#	   a browser for dissertations represented in that table. Returns boxplot statistics for the top three topics.
# Dependencies:
#   package(data.table), get doctopic grid.R, get topic labels.R, top docs per topic.R
source(file="variation of topic proportions.R")		# distribution of topic strength for all documents' top 10 topics 

## 
# `single topic strength vs rank.R`: are overall top topics high-ranked in few documents, or evenly spread out?
# Provides one function:
#   strength_v_rank(my.topic, dataset_name, ntopics, bad.topics): produces a scatterplot of one selected topic's
#	   contributions, with percent of dissertation on the y-axis and rank within dissertation on the x-axis.
# Dependencies:
#   package(data.table), package(RColorBrewer), get doctopic grid.R, get topic labels.R
source(file="single topic strength vs rank.R")

