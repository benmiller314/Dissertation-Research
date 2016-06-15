
################################################################################
# dataprep 2 - load data.R
#
# A file to read in dissertation metadata from a csv file. Binds key subsets 
# of data to variables and encodes method tags for easier analysis.
#
# NB: To identify schools in the Consortium of Doctoral Programs in Rhetoric and
# Composition, requires a separate csv file listing those schools.
########

if (!exists("tagnames")) {
	source(file="~/Dropbox/coursework, etc/dissertation/data, code, and figures/Dissertation Research/dataprep.R")
}

## now get the data 
# The most recent file of dissertation metadata
invisible(readline("Select the most recent file of dissertation metadata. (Press <Enter> to continue.)"))
bigarray <- read.csv(file=file.choose())

# parse the method tags... including for the collapsed schema
bigarray <- parse_tags(bigarray)
bigarray <- short_schema(bigarray)


# filter out false positives
noexcludes <- bigarray[bigarray$Exclude.Level==0,] 
justexcludes <- bigarray[bigarray$Exclude.Level>0,]

diss.count <- nrow(noexcludes) 
false.positives <- nrow(justexcludes)


message(paste("In this data set, there are",diss.count,"dissertations, not counting",false.positives,"false positives."))

# refactor levels for noexcludes alone
refactor.index <- which(names(noexcludes) %in% c("Subject","KEYWORDS","School","Advisor.type","Advisor.Name","Degree","Method.Terms","pages","Flag.notes"))
for (i in refactor.index) {
	noexcludes[,i] <- factor(noexcludes[,i])
}


# redefine methods that are all "check" or "check?" as "Other," and recalculate "Method.Count"
source(file=paste0(sourceloc, "check count.R"))

# get tag index columns on their own, for simplicity down the road
# TO DO: See whether we still need this
tagarray <- noexcludes[,tagnames]
row.names(tagarray) <- noexcludes[,"Author"]
data.matrix(tagarray) -> tagarray.m

# tag.totals <- tagtotals(tagarray, skip=0)
# barplot(tag.totals)

## Store reference variables for schools

consortium <- read.csv(file=paste0(dataloc,"doctoral-consortium-schools-programs, reconciled to carnegie.csv"))
conschools <- factor(consortium$University)

source(file="update realconsorts.R")

realconsorts.index <- which(noexcludes$realconsort == 1)
realconsorts <- noexcludes[realconsorts.index,]
consorts.index <- which(noexcludes$School %in% conschools)
consorts <- noexcludes[consorts.index,]
conschoolsfound <- factor(realconsorts$School)
consort.count <- nrow(realconsorts)

# print("Consortium Schools Found:")
# print(levels(conschoolsfound))
# print("Did you remember to reconcile schools?")

## figure out which consortium schools are not showing up
missing_conschools <- setdiff(levels(conschools),levels(conschoolsfound))
non_conschools <- setdiff(levels(noexcludes$School),levels(conschools))
nonconsorts <- noexcludes[(which(noexcludes$School %in% non_conschools)),]

## confirm that nonconsorts gets all the schools not in consorts
# setequal(nonconsorts, (noexcludes[-consorts.index,]))

# find top nonconsorts
top.nonconsorts <- thresh("nonconsorts")$thresh.data
consorts.plus <- rbind(consorts, top.nonconsorts)

# re-factor all factor columns in all data subsets
realconsorts <- refactor.all("realconsorts")
consorts <- refactor.all("consorts")
nonconsorts <- refactor.all("nonconsorts")
top.nonconsorts <- refactor.all("top.nonconsorts")
consorts.plus <- refactor.all("consorts.plus")

# make noexcludes easy to index and search
library(data.table)
noexcludes.dt <- as.data.table(noexcludes)
setkey(noexcludes.dt, Pub.number)


## Export file lists for subsets of data
if(remake_figs || update_realconsorts) {
    write(levels(factor(noexcludes$Pub.number)), file=paste0(sourceloc, "Shell scripts and commands/file list noexcludes.txt"), sep="\n")
    write(levels(factor(consorts$Pub.number)), file=paste0(sourceloc, "Shell scripts and commands/file list consorts.txt"), sep="\n")
    write(levels(factor(nonconsorts$Pub.number)), file=paste0(sourceloc, "Shell scripts and commands/file list nonconsorts.txt"), sep="\n")
    write(levels(factor(realconsorts$Pub.number)), file=paste0(sourceloc, "Shell scripts and commands/file list realconsorts.txt"), sep="\n")
}
# TO DO (maybe): split out multiple advisors

# if "function scratchpad.R" is being used, clean up unneeded variables
# rm(datawrangle, best_sort, filter_data, get_tags, matcharray, reorder_ser, rowdiff, rowmatch, shuffle, tagdiff, tagmatch, refactor.index)
