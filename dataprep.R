## define some broad parameters, since this file will always be run first
# make a shortcut for retrieving the last entered value
ans <- function() {
	.Last.value
	}
	
tagnames <- c("Clin","Crit","Cult","Disc","Ethn","Expt","Hist","Intv","Meta","Modl","Phil","Poet","Prac","Rhet","Surv","Othr")
sumnames <- sapply(tagnames, FUN=function(x) paste0(x,".sum"))
topnames <- sapply(tagnames, FUN=function(x) as.list(tolower(paste0("top.",x))))
topnames <- lapply(topnames, FUN=function(x) substr(x,1,8))


# prep some useful functions
# source(file="function scratchpad.R")
source(file="extract subjects.R")
source(file="Factor-Bug fixing.R")
source(file="heatmap_ben.R")
source(file="method tag array.R")
source(file="heatmap fixedcols.R")

## now get the data 
# The most recent file of dissertation metadata
print(noquote("Select the most recent file of dissertation metadata."))
bigarray <- read.csv(file=file.choose())

# parse the method tags
bigarray <- parse_tags(bigarray)

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
tagarray <- noexcludes[,tagnames]
row.names(tagarray) <- noexcludes[,"Author"]
data.matrix(tagarray) -> tagarray.m

# tag.totals <- tagtotals(tagarray, skip=0)
# barplot(tag.totals)

consortium <- read.csv(file="../doctoral-consortium-schools-programs, reconciled to carnegie.csv")
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