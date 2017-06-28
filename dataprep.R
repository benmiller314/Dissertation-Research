################################################################################
# dataprep.R
#
# A file to configure my usual working directories, variables, and functions.
# Follow up by running 'dataprep2 - load data.R'; see 'run all analyses.R' for further steps.
########

# define some broad parameters, since this file will always be run first
# make a shortcut for retrieving the last entered value
ans <- function() {
	.Last.value
	}
	
# set the working directories, taking into account the GitHub setup
sourceloc <- "~/Dropbox/coursework, etc/dissertation/data, code, and figures/Dissertation Research/"
unixsourceloc <- "~/'Dropbox/coursework, etc/dissertation/data, code, and figures/Dissertation Research/'"
if(getwd() != sourceloc) {
	setwd(sourceloc)
}

imageloc <- paste0(sourceloc, "../Dissertation Research - Figures/")
dataloc <- paste0(sourceloc, "../")
newdataloc <- file.path("~", "Box Sync", "research", "dissertations")
malletloc <- "~/Applications/mallet"
webloc <- "~/Documents/Webdev/datavis_testing"
tmloc <- "~/Documents/tm"


# name the method tags most of these analyses are interested in
tagnames <- c("Clin","Crit",
			  # "Cult",
			  "Disc","Ethn","Expt","Hist","Intv","Meta","Modl","Phil","Poet","Prac","Rhet","Surv","Othr", "Ped")

tagnames.long <- c( "Clinical / Case Study", 
					"Critical / Hermeneutical",
					# "Cultural-Critical",
					"Discourse or Text Analytical",
					"Ethnographic",
					"Experimental / Quasi-Experimental",
					"Historical / Archival",
					"Interview / Focus Group",
					"Meta-Analytical / Discipliniographic",
					"Model-Building",
					"Philosophical / Theoretical",
					"Poetic / Fictive / Craft-Based",
					"Practitioner / Teacher-Research",
					"Rhetorical Analytical",
					"Survey",
					"Other", 
					"Pedagogical Projection"
					)
					
# provide a function to convert tag column labels to real tag names					
realtags <- function(tag, tagset_name="tagnames") {
	tagset <- get(tagset_name)
	index <- grep(tag, tagset, ignore.case=TRUE)
	tagset.long <- get(paste0(tagset_name, ".long"))

	return(tagset.long[index])
}


sumnames <- sapply(tagnames, FUN=function(x) paste0(x,".sum"))
meannames <- sapply(tagnames, FUN=function(x) paste0(x,".mean"))
topnames <- sapply(tagnames, FUN=function(x) as.list(tolower(paste0("top.",x))))
topnames <- lapply(topnames, FUN=function(x) substr(x,1,8))

# If remake_figs is true (e.g. if set by 'rerun all analyses.R'), new pdf files will be created; 
# otherwise, they'll display on screen only.
if(!exists("remake_figs")) {
	remake_figs <- FALSE
}
if(!exists("autorun")) {
	autorun <- FALSE
}

## prep some useful functions
# source(file="function scratchpad.R")
source(file="extract subjects.R")
source(file="Factor-Bug fixing.R")
source(file="heatmap_ben.R")
source(file="heatmap fixedcols.R")
source(file="method tag array.R")
source(file="thresh.R")
source(file="simplifying the schema.R")
source(file="advisor relations.R")
