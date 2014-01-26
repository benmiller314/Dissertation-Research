if (!exists("tagnames")) {
	source(file="dataprep.R")
}

## now get the data 
# The most recent file of dissertation metadata
print(noquote("Select the most recent file of dissertation metadata."))
bigarray <- read.csv(file=file.choose())

# parse the method tags... including for the collapsed schema
bigarray <- parse_tags(bigarray)
bigarray <- short_schema(bigarray)


# filter out false positives
noexcludes <- bigarray[bigarray$Exclude.Level==0,] 
justexcludes <- bigarray[bigarray$Exclude.Level>0,]

diss.count <- nrow(noexcludes) 
false.positives <- nrow(justexcludes)


print(noquote(paste("In this data set, there are",diss.count,"dissertations, not counting",false.positives,"false positives.")))

# refactor levels for noexcludes alone
refactor.index <- which(names(noexcludes) %in% c("Subject","KEYWORDS","School","Advisor.type","Advisor.Name","Degree","Method.Terms","pages","Flag.notes"))
for (i in refactor.index) {
	noexcludes[,i] <- factor(noexcludes[,i])
}


# redefine methods that are all "check" or "check?" as "Other"
source(file="check count.R")

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


# split out multiple advisors?

# if "function scratchpad.R" is being used, clean up unneeded variables
# rm(datawrangle, best_sort, filter_data, get_tags, matcharray, reorder_ser, rowdiff, rowmatch, shuffle, tagdiff, tagmatch, refactor.index)