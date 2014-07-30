if (!exists("tagnames")) {
	source(file="/Users/benmiller314/Dropbox/coursework, etc/dissertation/R experiments/Dissertation Research/dataprep.R")
}

## now get the data 
# The most recent file of dissertation metadata
message("Select the most recent file of dissertation metadata.")
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
source(file=paste0(sourceloc, "/check count.R"))

# get tag index columns on their own, for simplicity down the road
# TO DO: See whether we still need this
tagarray <- noexcludes[,tagnames]
row.names(tagarray) <- noexcludes[,"Author"]
data.matrix(tagarray) -> tagarray.m

# tag.totals <- tagtotals(tagarray, skip=0)
# barplot(tag.totals)

consortium <- read.csv(file=paste0(dataloc,"doctoral-consortium-schools-programs, reconciled to carnegie.csv"))
conschools <- factor(consortium$University)
consorts.index <- which(noexcludes$School %in% conschools)
consorts <- noexcludes[consorts.index,]
conschoolsfound <- factor(consorts$School)
consort.count <- nrow(consorts)

# print("Consortium Schools Found:")
# print(levels(conschoolsfound))
# print("Did you remember to reconcile schools?")

# figure out which consortium schools are not showing up
missing_conschools <- setdiff(levels(conschools),levels(conschoolsfound))
non_conschools <- setdiff(levels(noexcludes$School),levels(conschools))
nonconsorts <- noexcludes[(which(noexcludes$School %in% non_conschools)),]

# confirm that nonconsorts gets all the schools not in consorts
setequal(nonconsorts, (noexcludes[-consorts.index,]))

# make noexcludes easy to index and search
library(data.table)
noexcludes.dt <- as.data.table(noexcludes)
setkey(noexcludes.dt, Pub.number)


## Export file lists for subsets of data
write(levels(factor(noexcludes$Pub.number)), file=paste0(sourceloc, "/Shell scripts and commands/file list noexcludes.txt", sep="\n"))
write(levels(factor(consorts$Pub.number)), file=paste0(sourceloc, "/Shell scripts and commands/file list consorts.txt", sep="\n"))
write(levels(factor(nonconsorts$Pub.number)), file=paste0(sourceloc, "/Shell scripts and commands/file list nonconsorts list.txt", sep="\n"))

# TO DO (maybe): split out multiple advisors

# if "function scratchpad.R" is being used, clean up unneeded variables
# rm(datawrangle, best_sort, filter_data, get_tags, matcharray, reorder_ser, rowdiff, rowmatch, shuffle, tagdiff, tagmatch, refactor.index)