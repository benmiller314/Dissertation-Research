## define some broad parameters, since this file will always be run first
# make a shortcut for retrieving the last entered value
ans <- function() {
	.Last.value
	}
	
# name the method tags most of these analyses are interested in
tagnames <- c("Clin","Crit",
# "Cult",
"Disc","Ethn","Expt","Hist","Intv","Meta","Modl","Phil","Poet","Prac","Rhet","Surv","Othr")

sumnames <- sapply(tagnames, FUN=function(x) paste0(x,".sum"))
meannames <- sapply(tagnames, FUN=function(x) paste0(x,".mean"))
topnames <- sapply(tagnames, FUN=function(x) as.list(tolower(paste0("top.",x))))
topnames <- lapply(topnames, FUN=function(x) substr(x,1,8))

# set the working directories, taking into account the GitHub setup
setwd("/Users/benmiller314/Dropbox/coursework, etc/dissertation/R experiments/Dissertation Research")
imageloc <- "../Dissertation Research - Figures/"
dataloc <- "../"

# If remake_figs is true (e.g. if set by 'rerun all analyses.R'), new pdf files will be created; 
# otherwise, they'll display on screen only.
if(!exists("remake_figs")) {
	remake_figs <- FALSE
}

## prep some useful functions
# source(file="function scratchpad.R")
source(file="extract subjects.R")
source(file="Factor-Bug fixing.R")
source(file="heatmap_ben.R")
source(file="heatmap fixedcols.R")
source(file="method tag array.R")
source(file="simplifying the schema.R")
source(file="thresh.R")

