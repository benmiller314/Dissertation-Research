################################################################################
# dataprep.R
#
# A file to configure my usual working directories, variables, and functions.
# Follow up by running 'dataprep2 - load data.R'; see 'run all analyses.R' for further steps.
########

# Global variables called in many functions. See `re-run all analyses.R` for descriptions.
if (!exists("remake_figs")) {
    remake_figs <- FALSE
    autorun <- FALSE
    update_realconsorts <- FALSE
}

if (!exists("which_computer")) {
    # which_computer <- "work"
    which_computer <- "laptop"
}

### define some broad parameters, since this file will always be run first

# set the working directories, taking into account the GitHub setup
sourceloc <- file.path(home_dir, "research", "dissertations", "data, code, and figures", "Dissertation-Research")
unixsourceloc <- sourceloc
if(getwd() != sourceloc) {
	setwd(sourceloc)
}

imageloc <- normalizePath(file.path(sourceloc, "../Dissertation Research - Figures"))
dataloc <- normalizePath(file.path(sourceloc, ".."))
malletloc <- normalizePath(file.path("~", "Applications", "mallet"))
webloc <- normalizePath(file.path("~", "Documents", "Webdev", "datavis_testing"))
fulltextloc <- file.path("/Volumes/ExPostFATo/full-text_dissertations")
if (which_computer == "work") {
    tmloc <- normalizePath(file.path(fulltextloc, "..", "topic_modeling"))
} else if (which_computer == "laptop") {
    tmloc <- normalizePath(file.path(dataloc, "..", "tm"))
}
WNHOME <- file.path("/usr", "local", "Cellar", "wordnet", "3.1")

# name the method tags most of these analyses are interested in
tagnames <- c("Clin","Crit",
			  # "Cult",
			  "Disc","Ethn","Expt","Hist","Intv","Meta","Modl","Phil","Poet","Prac","Rhet","Surv","Othr", "Ped")
taggroups <- c("Phenomenological", # Clin
               "Dialectical",      # Crit
               "Aggregable",       # Disc
               "Phenomenological", # Ethn
               "Aggregable",       # Expt
               "Dialectical",      # Hist
               "Aggregable",       # Intv
               "Aggregable",       # Meta
               "Dialectical",      # Modl
               "Dialectical",      # Phil
               "Performance-Based",# Poet
               "Performance-Based",# Prac
               "Dialectical",      # Rhet
               "Aggregable",       # Surv
               "Other",              # Othr
               "Dialectical"       # Ped
)
names(taggroups) <- tagnames

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

no_ped_tagnames <- tagnames[!(tagnames %in% "Ped")]
no_ped_taggroups <- taggroups[!(names(taggroups) %in% "Ped")]
no_ped_tagnames.long <- tagnames.long[!(tagnames.long) %in% "Pedagogical Projection"]

# color palette from viridisLite, determined while working on `method collocation heatmap.R`
group_pal <- c("#3B528BFF", # dark blue
               "#27AD81FF", # teal
               "#AADC32FF", # green
               "#FDE725FF", # yellow
               "#FFFFFF")   # white
names(group_pal) <- c("Dialectical",
                      "Phenomenological",
                      "Aggregable",
                      "Performance-Based",
                      "Other")

# provide a function to convert tag column labels to real tag names
realtags <- function(tag, tagset_name="tagnames") {
	tagset <- get(tagset_name)
	index <- grep(tag, tagset, ignore.case=TRUE)
	tagset.long <- get(paste0(tagset_name, ".long"))

	return(tagset.long[index])
}

# # TO DO: move these to where they're actually needed,
# # so we don't clutter the environment every time.
# sumnames <- sapply(tagnames, FUN=function(x) paste0(x,".sum"))
# meannames <- sapply(tagnames, FUN=function(x) paste0(x,".mean"))
# topnames <- sapply(tagnames, FUN=function(x) as.list(tolower(paste0("top.",x))))
# topnames <- lapply(topnames, FUN=function(x) substr(x,1,8))


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
source(file="remove unreadables.R")
