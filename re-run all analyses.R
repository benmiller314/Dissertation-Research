##
# Overview of analyses 
# This file is a run-down of the programs that could re-create my figures from prepared data.
# For initial data cleaning, I used GoogleRefine (now OpenRefine); see files in the OpenRefine directory.


# Set working directory to the location of R script files.
setwd("/Users/benmiller314/Dropbox/coursework, etc/dissertation/R experiments/Dissertation Research")

# Global variables called in many functions.
# remake_figs: If TRUE, save new files for figures; if FALSE, display on screen only.
remake_figs <- FALSE; 

# dataprep: prepares working environment by loading helper functions and setting key variables (such as tagset).
# dataprep 2: loads in a .csv file of tagged spreadsheet data, generate a tag array, and define various subsets.
# 	You will be prompted to select the file via file.choose().
# NB: These two functions can be called jointly via source(file="start here.R")
source(file="dataprep.R")
source(file="dataprep 2 - load data.R")

# tags by school: generates heat plots of methods used in dissertations, aggregated by school.
# 	Provides two functions:
#	schoolwise.data(dataset_name, tagset_name): returns a list of tag means, sums, and counts,
#		each aggregated by school.
#	schoolwise(dataset_name, tagset_name, ...): make one or more heatplots from the output of schoolwise.data().
source(file="tags by school.R")

# methodcount barplot: produces a bar plot of method-tag counts per dissertation, for a give method tagset.
# keyword barplot: produces a bar plot of author-provided keyword-tag counts per dissertation. Not so useful.
source(file="methodcount barplot.R")
# source(file="keyword barplot.R")

# frequency of method tags: tabulates and plots the number of times a dissertation is tagged with each method
# 	Provides three functions:
#	get_tags(dataset_name, tagset_name): returns a named vector of frequencies for each method in the tagset
#	methodfreq_combined(bigset, smallset, diffset): plots an overlaid horizontal bar graph of method frequencies;
#		by default the three sets are noexcludes, consorts, and nonconsorts, respectively (but others are possible).
# 	compare_method_ranks(set1, set2, ...): creates a side-by-side plot of methods in descending rank order, 
#		with lines connecting the same methods to quickly reveal changes in rank.
source(file="frequency of method tags.R")


source(file="top 10 lists take 2.R")



# source(file="collocation heatmap.R")
source(file="schools per year.R") 

# source(file="map by school.R")		# per-method maps	# look for the individual files instead
source(file="map by school 4 (comp-rhet superimposed on carnegie schools).R")