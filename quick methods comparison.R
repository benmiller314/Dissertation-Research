##############################################################################
# GOAL: compare method tag distribution in 2010-2015 vs. 2001-2010
#
# PLAN: use  compare_method_ranks from `frequency of method tags.R`

if (!exists("tagnames")) {
    source(file=file.path("~","Dropbox", "coursework, etc", "dissertation", "data, code, and figures", "Dissertation Research", "dataprep.R"))
}

source(file="frequency of method tags.R")

summary(noexcludes$Advisor.type)
summary(noexcludes$X500)

summer2017 <- noexcludes[which(is.na(noexcludes$Advisor.type)),]
originals <- noexcludes[which(is.na(noexcludes$X500)),]

nrow(summer2017)
nrow(originals)

compare_method_ranks("originals", "summer2017", colorful=T, pcts=T)
# NB: this produces a corrected p < .001 for Hist even though it's 25% of both datasets. 
Could I be miscalcluating?? Boooo