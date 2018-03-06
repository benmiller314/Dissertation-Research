
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
    source(file=file.path("~","Box Sync", "research", "dissertations", "data, code, and figures", "Dissertation-Research", "dataprep.R"))
}

## now get the data 
# The original file of dissertation metadata (built from 2012-2015, covering years 2001-2010)
# oldarray <- read.csv(file=file.path(dataloc, "Rhetoric-or-Composition-12-adding-depts.csv"))
oldarray <- read.csv(file=file.path(dataloc, "Diss-data-collected-pre-Pitt_short-tags.csv"))

# The new file of dissertation metadata
invisible(readline("Select the most recent file of dissertation metadata. (Press <Enter> to continue.)"))
newarray <- read.csv(file=file.choose())



# parse the method tags... including for the collapsed schema
oldarray <- parse_tags(oldarray, tagstyle="short", excludecol="Flags")
oldarray <- short_schema(oldarray)
# names(oldarray)
# oldarray[tagnames]

newarray <- parse_tags(newarray, tagstyle="short", excludecol="Flags")
newarray <- short_schema(newarray)


# Let's merge them, why not
bigarray <- merge(oldarray, newarray, all=T)
names(bigarray)



# filter out false positives
noexcludes <- bigarray[bigarray$Exclude.Level==0,] 
justexcludes <- bigarray[bigarray$Exclude.Level>0,]

diss.count <- nrow(noexcludes) 
false.positives <- nrow(justexcludes)

message(paste("In this data set, there are",diss.count,"dissertations, not counting",false.positives,"false positives."))

# mark noexcludes that will remain in for abstracts, but that 
# we'll leave out of full-text analysis (non-English, bad scans);
# these will now have Exclude.Level=3
noexcludes <- remove_unreadables("noexcludes")
    
# refactor levels for noexcludes alone
refactor.index <- which(names(noexcludes) %in% c("Subject","KEYWORDS","School","Advisor.type","Advisor.Name","Degree","Method.Terms","pages","Flag.notes"))
for (i in refactor.index) {
	noexcludes[,i] <- factor(noexcludes[,i])
}


# redefine methods that are all "check" or "check?" as "Other," and recalculate "Method.Count"
source(file=file.path(sourceloc, "check count.R"))

# get tag index columns on their own, for simplicity down the road
# TO DO: See whether we still need this
# tagarray <- noexcludes[,tagnames]
# row.names(tagarray) <- noexcludes[,"Author"]
# data.matrix(tagarray) -> tagarray.m

# tag.totals <- tagtotals(tagarray, skip=0)
# barplot(tag.totals)

## Store reference variables for schools

consortium <- read.csv(file=file.path(dataloc, "doctoral-consortium-schools-programs, reconciled to carnegie.csv"))
conschools <- factor(consortium$University)

source(file="update realconsorts.R")
noexcludes <- realconsorts_by_list("noexcludes")

consorts.index <- which(noexcludes$School %in% conschools)
consorts <- noexcludes[consorts.index,]
conschoolsfound <- factor(consorts$School)
consort.count <- nrow(consorts)

realconsorts.index <- which(noexcludes$realconsort == 1)
realconsorts <- noexcludes[realconsorts.index,]
realconsort.count <- nrow(realconsorts)

message("Of ", consort.count, " dissertations at Consortium schools, ",
        realconsort.count, " are confirmed from Consortium programs.")

rhetmaplist <- read.csv(file=file.path(dataloc, "rhetmap-doctoral-programs-list.csv"))
rhetmapschools <- factor(rhetmaplist$Carnegie2010_name)
rhetmapschools[-which(rhetmapschools %in% consortium$University)]
consortium$University[-which(consortium$University %in% rhetmapschools)]
rhetmap.index <- which(noexcludes$School %in% rhetmapschools)
rhetmaps <- noexcludes[rhetmap.index,]


# index of disses that need to be checked for realconsort status
# (no_alumni_list is created as a side effect of `update realconsorts.R`)
maybeconsorts.index <- which(consorts$School %in% no_alumni_list)
maybeconsorts.index <- which(!consorts[maybeconsorts.index, "realconsort"] %in% c(0,1))
maybeconsorts <- consorts[maybeconsorts.index, ]

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

# limit to 2001-2015
noexcludes2001_2015 <- noexcludes[which(noexcludes$Year %in% seq(2001, 2015, 1)),]
consorts2001_2015 <- consorts[which(consorts$Year %in% seq(2001, 2015, 1)),]
realconsorts2001_2015 <- realconsorts[which(realconsorts$Year %in% seq(2001, 2015, 1)),]
nonconsorts2001_2015 <- noexcludes[which(noexcludes$Year %in% seq(2001, 2015, 1)) &&
                                   which(!noexcludes$realconsort != 1),]

# or just the new stuff
noexcludes2011_2015 <- noexcludes[which(noexcludes$Year %in% seq(2011, 2015, 1)),]
consorts2011_2015 <- consorts[which(consorts$Year %in% seq(2011, 2015, 1)),]
realconsorts2011_2015 <- realconsorts[which(realconsorts$Year %in% seq(2011, 2015, 1)),]
nonconsorts2011_2015 <- noexcludes[which(noexcludes$Year %in% seq(2011, 2015, 1)) &&
                                       which(!noexcludes$realconsort != 1),]


# re-factor all factor columns in all data subsets
realconsorts <- refactor.all("realconsorts")
consorts <- refactor.all("consorts")
nonconsorts <- refactor.all("nonconsorts")
top.nonconsorts <- refactor.all("top.nonconsorts")
consorts.plus <- refactor.all("consorts.plus")
noexcludes2001_2015 <- refactor.all("noexcludes2001_2015")
consorts2001_2015 <- refactor.all("consorts2001_2015")
realconsorts2001_2015 <- refactor.all("realconsorts2001_2015")
nonconsorts2001_2015 <- refactor.all("nonconsorts2001_2015")
noexcludes2011_2015 <- refactor.all("noexcludes2011_2015")
consorts2011_2015 <- refactor.all("consorts2011_2015")
realconsorts2011_2015 <- refactor.all("realconsorts2011_2015")
nonconsorts2011_2015 <- refactor.all("nonconsorts2011_2015")
maybeconsorts <- refactor.all("maybeconsorts")

# make noexcludes easy to index and search
library(data.table)
noexcludes.dt <- as.data.table(noexcludes)
setkey(noexcludes.dt, Pub.number)

## Export file lists for subsets of data
if(remake_figs || update_realconsorts) {
    write(levels(factor(noexcludes$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list noexcludes.txt"), sep="\n")
    write(levels(factor(noexcludes2001_2015$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list noexcludes_2001_2015.txt"), sep="\n")
    write(levels(factor(consorts2001_2015$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list consorts_2001_2015.txt"), sep="\n")
    write(levels(factor(realconsorts2001_2015$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list realconsorts_2001_2015.txt"), sep="\n")
    write(levels(factor(noexcludes2011_2015$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list noexcludes_2011_2015.txt"), sep="\n")
    write(levels(factor(consorts2011_2015$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list consorts_2011_2015.txt"), sep="\n")
    write(levels(factor(realconsorts2011_2015$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list realconsorts_2011_2015.txt"), sep="\n")
    write(levels(factor(consorts$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list consorts.txt"), sep="\n")
    write(levels(factor(nonconsorts$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list nonconsorts.txt"), sep="\n")
    write(levels(factor(realconsorts$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list realconsorts.txt"), sep="\n")
    write(levels(factor(maybeconsorts$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list maybeconsorts.txt"), sep="\n")
}
# TO DO (maybe): split out multiple advisors

# if "function scratchpad.R" is being used, clean up unneeded variables
# rm(datawrangle, best_sort, filter_data, get_tags, matcharray, reorder_ser, rowdiff, rowmatch, shuffle, tagdiff, tagmatch, refactor.index)
