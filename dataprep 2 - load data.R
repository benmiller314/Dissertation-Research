
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

if(dual_source) {
    # The original file of dissertation metadata (built from 2012-2015, covering years 2001-2010)
    # oldarray <- read.csv(file=file.path(dataloc, "Rhetoric-or-Composition-12-adding-depts.csv"))
    oldarray <- read.csv(file=file.path(dataloc, "Diss-data-collected-pre-Pitt_short-tags.csv"))
    
    # parse the method tags... including for the collapsed schema. parse_tags() is from `method tag array.R`.
    oldarray <- parse_tags(oldarray, tagstyle="short", excludecol="Flags")
    oldarray <- short_schema(oldarray)
    
}

# The new file of dissertation metadata
invisible(readline("Select the most recent file of dissertation metadata. (Press <Enter> to continue.)"))
newdatafile <- file.choose()
newarray <- read.csv(file=newdatafile)



# parse the method tags... including for the collapsed schema. parse_tags() is from `method tag array.R`.
newarray <- parse_tags(newarray, tagstyle="short", excludecol="Flags")
newarray <- short_schema(newarray)


if(dual_source) {
    # Let's merge them, why not
    bigarray <- merge(oldarray, newarray, all=T)
    names(bigarray)
} else {
    bigarray <- newarray
}

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

# TO DO: Make this into a function without weird side effects; 
# 'check count.R' seems to be misaligning Method.Terms, so the tagarray doesn't line up afterwards
# # redefine methods that are all "check" or "check?" as "Other," and recalculate "Method.Count"
# source(file=file.path(sourceloc, "check count.R"))


## Store reference variables for schools
consortium <- read.csv(file=file.path(dataloc, "doctoral-consortium-schools-programs, reconciled to carnegie.csv"))
conschools <- factor(consortium$University)

rhetmaplist <- read.csv(file=file.path(dataloc, "rhetmap-doctoral-programs-list.csv"))
rhetmapschools <- factor(rhetmaplist$Carnegie2010_name)
rhetmapschools <- rhetmapschools[order(rhetmapschools)]

# schools in rhetmap but not consortium (19)
rhetmaponly <- rhetmapschools[-which(rhetmapschools %in% conschools)]

# schools in consortium but not rhetmap  (1)
consortsonly <- conschools[-which(conschools %in% rhetmapschools)]     

# verify departmental information, so we can know what's really produced by a consortium or rhetmap school
source(file="update realconsorts.R")
if(update_realconsorts) {
    noexcludes <- realconsorts_by_list("noexcludes") # use default
}
    
# noexcludes <- realconsorts_by_list("noexcludes", manual_file = file.path(dataloc, "unconfirmed_consorts_2018-07-05.csv"))

# get tag index columns on their own, for simplicity down the road
# TO DO: See whether we still need this
# tagarray <- noexcludes[,tagnames]
# row.names(tagarray) <- noexcludes[,"Author"]
# data.matrix(tagarray) -> tagarray.m

# tag.totals <- tagtotals(tagarray, skip=0)
# barplot(tag.totals)


#### update subsets
# naive filter, by school only (without department)
consorts.index <- which(noexcludes$School %in% conschools)
consorts <- noexcludes[consorts.index,]
consort.count <- nrow(consorts)

rhetmaps.index <- which(noexcludes$School %in% rhetmapschools)
rhetmaps <- noexcludes[rhetmaps.index,]
rhetmap.count <- nrow(rhetmaps)

# so much for schools. what about actual programs? 
realconsorts.index <- which(noexcludes$realconsort == 1)
realconsorts <- noexcludes[realconsorts.index,]
realconsort.count <- nrow(realconsorts)
fakeconsorts <- consorts[-which(consorts$Pub.number %in% realconsorts$Pub.number),]

realrhetmaps.index <- which(noexcludes$realrhetmap == 1)
realrhetmaps <- noexcludes[realrhetmaps.index,]
realrhetmap.count <- nrow(realrhetmaps)

knownprograms.index <- union(realconsorts.index, realrhetmaps.index)
knownprograms <- noexcludes[knownprograms.index,]
unknownprograms.index <- setdiff(union(consorts.index, rhetmaps.index), knownprograms.index)
unknownprograms <- noexcludes[unknownprograms.index, ]
# write.csv(unknownprograms, file=file.path(dataloc, paste0("maybeconsorts_", Sys.Date(), ".csv")), row.names=F, na="")
unknownprograms.count <- length(union(consorts.index, rhetmaps.index)) - length(knownprograms.index)

# report back what we've found

message("Of ", consort.count, " dissertations at Consortium schools, ",
        realconsort.count, " are confirmed from Consortium programs, ",
        "and ", length(which(noexcludes$realconsort == 0)), " confirmed to be otherwise.")
message("Of ", rhetmap.count, " dissertations at Rhetmap schools, ",
        realrhetmap.count, " are confirmed from Rhetmap programs, ",
        "and ", length(which(noexcludes$realrhetmap == 0)), " confirmed to be otherwise.")
message("This brings the total count confirmed from known programs to ", nrow(knownprograms), ", ",
        "with ", unknownprograms.count, " programs at these schools remaining to be tracked down.")


# index of disses that need to be checked for realconsort status
# (no_alumni_list is created as a side effect of `update realconsorts.R`)
# maybeconsorts.index <- which(consorts$School %in% no_alumni_list)
# maybeconsorts.index <- which(!consorts[maybeconsorts.index, "realconsort"] %in% c(0,1))
# maybeconsorts <- consorts[maybeconsorts.index, ]


## figure out which consortium schools are not showing up
conschoolsfound <- factor(consorts$School)
# print("Consortium Schools Found:")
# print(levels(conschoolsfound))
# print("Did you remember to reconcile schools?")


missing_conschools <- setdiff(levels(conschools),levels(conschoolsfound))
non_conschools <- setdiff(levels(noexcludes$School),levels(conschools))
nonconsorts <- noexcludes[(which(noexcludes$School %in% non_conschools)),]

## confirm that nonconsorts gets all the schools not in consorts
# setequal(nonconsorts, (noexcludes[-consorts.index,]))

# find top nonconsorts
until <- max(nonconsorts$Year)
since <- until - 5
top.nonconsorts <- thresh(dataset="nonconsorts", until=until, since=since)$thresh.data
consorts.plus <- rbind(consorts, top.nonconsorts)
rm("until", "since")

# limit to 2001-2015
noexcludes2001_2015 <- noexcludes[which(noexcludes$Year %in% seq(2001, 2015, 1)),]
consorts2001_2015 <- consorts[which(consorts$Year %in% seq(2001, 2015, 1)),]
realconsorts2001_2015 <- realconsorts[which(realconsorts$Year %in% seq(2001, 2015, 1)),]
# nonconsorts2001_2015 <- noexcludes[which(noexcludes$Year %in% seq(2001, 2015, 1) &
#                                    noexcludes$realconsort != 1 & 
#                                    noexcludes$realrhetmap != 1),]
rhetmaps2001_2015 <- rhetmaps[which(rhetmaps$Year %in% seq(2001, 2015, 1)),]
realrhetmaps2001_2015 <- realrhetmaps[which(realrhetmaps$Year %in% seq(2001, 2015, 1)),]
knownprograms2001_2015 <- knownprograms[which(knownprograms$Year %in% seq(2001, 2015, 1)),]
nonconsorts2001_2015 <- noexcludes[noexcludes$Pub.number %in% setdiff(noexcludes2001_2015$Pub.number, knownprograms2001_2015$Pub.number),]


# or just the new stuff
noexcludes2011_2015 <- noexcludes[which(noexcludes$Year %in% seq(2011, 2015, 1)),]
consorts2011_2015 <- consorts[which(consorts$Year %in% seq(2011, 2015, 1)),]
realconsorts2011_2015 <- realconsorts[which(realconsorts$Year %in% seq(2011, 2015, 1)),]
nonconsorts2011_2015 <- noexcludes[which(noexcludes$Year %in% seq(2011, 2015, 1)) &&
                                       which(!noexcludes$realconsort != 1),]
rhetmaps2011_2015 <- rhetmaps[which(rhetmaps$Year %in% seq(2011, 2015, 1)),]
realrhetmaps2011_2015 <- realrhetmaps[which(realrhetmaps$Year %in% seq(2011, 2015, 1)),]
knownprograms2011_2015 <- knownprograms[which(knownprograms$Year %in% seq(2011, 2015, 1)),]

# better: make a subset for figuring out what you did for cccc grant in 2017ff
new_noexcludes <- noexcludes[-which(is.na(noexcludes$Link)), ]

# five-year bins
subset_by_year <- function(dataset_name, 
                           start_year, 
                           end_year,
                           autoskip = FALSE,
                           autooverwrite = FALSE) {
    
    dataset <- get(dataset_name)
    new_subset_name <- paste0(dataset_name, start_year, "_", end_year)
    if(autooverwrite || !exists(new_subset_name)) {
        assign(new_subset_name, 
           dataset[which(dataset$Year %in% seq(start_year, end_year, 1)), ],
           envir= .GlobalEnv)
    } else if (autoskip) {
        message(new_subset_name, "already exists; autoskipping.")
        return()
    } else {
        message(new_subset_name, " already exists with ", 
                nrow(get(new_subset_name)), " rows. (O)verwrite or (S)kip?")
        a <- readline(" ")
        while(!(tolower(a) %in% c("o", "s"))) {
            message("Please answer 'o' for overwrite or 's' for skip.")
            a <- readline(" ")
        }
        if(tolower(a) == "o") {
            assign(new_subset_name, 
                   dataset[which(dataset$Year %in% seq(start_year, end_year, 1)), ],
                   envir= .GlobalEnv)
        } else if (tolower(a) == "s") {
            message("overwrite of ", new_subset_name, " skipped.")
            return()
        }
    }
    message(new_subset_name, " created with ", nrow(get(new_subset_name)), " rows.")
    invisible(get(new_subset_name))
}

subset_list <- c("noexcludes", "knownprograms", "top.nonconsorts", "realconsorts", "realrhetmaps")
for(subset in subset_list) {
    subset_by_year(subset, 2001, 2015, autooverwrite =T)
    subset_by_year(subset, 2001, 2005, autooverwrite = T)
    subset_by_year(subset, 2006, 2010, autooverwrite =T)
    subset_by_year(subset, 2011, 2015, autooverwrite = T)
}

# re-factor all factor columns in all data subsets
## TO DO: use loops and assign() to *build* these subsets with less redundancy

subset_list <- c("consorts", "nonconsorts", "realconsorts", "top.nonconsorts", "consorts.plus", 
                 "noexcludes2001_2015", "consorts2001_2015", "realconsorts2001_2015", "nonconsorts2001_2015",
                 "noexcludes2001_2005", "realconsorts2001_2005", "realrhetmaps2001_2005",
                 "noexcludes2006_2010", "realconsorts2006_2010", "realrhetmaps2006_2010",
                 "noexcludes2011_2015", "consorts2011_2015", "realconsorts2011_2015", "nonconsorts2011_2015",
                 "new_noexcludes", "rhetmaps", "rhetmaps2001_2015", "rhetmaps2011_2015",
                 "realrhetmaps", "realrhetmaps2001_2015", "realrhetmaps2011_2015",
                 "knownprograms", "knownprograms2001_2015", "knownprograms2011_2015",
                 "maybeconsorts", "unknownprograms")

for (subset in subset_list) {
    if(exists(subset)) {
        assign(subset, refactor.all(subset))
    }
}

# make noexcludes easy to index and search
library(data.table)
noexcludes.dt <- as.data.table(noexcludes)
setkey(noexcludes.dt, Pub.number)

## Export file lists for subsets of data

# TO DO: use filename convention from dfrtopics, namely paste(dataset_name, "_doc_ids.txt")

export_file_list <- function(dataset_names) {
    for (dataset_name in dataset_names) {
        dataset <- get(dataset_name)
        pubs <- dataset$Pub.number
        write(levels(factor(pubs)), file=file.path(sourceloc, "subsets", paste0(dataset_name, "_doc_ids.txt")))
    }
}

if(remake_figs || update_realconsorts) {
    export_file_list(c("noexcludes",
                   "noexcludes2001_2015",
                   "knownprograms2001_2015"))
}

#### The next 10 lines or so now replaced by export_file_list, above ####
#     write(levels(factor(noexcludes$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list noexcludes.txt"), sep="\n")
#     write(levels(factor(noexcludes2001_2015$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list noexcludes_2001_2015.txt"), sep="\n")
#     write(levels(factor(consorts2001_2015$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list consorts_2001_2015.txt"), sep="\n")
#     write(levels(factor(realconsorts2001_2015$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list realconsorts_2001_2015.txt"), sep="\n")
#     write(levels(factor(noexcludes2011_2015$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list noexcludes_2011_2015.txt"), sep="\n")
#     write(levels(factor(consorts2011_2015$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list consorts_2011_2015.txt"), sep="\n")
#     write(levels(factor(realconsorts2011_2015$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list realconsorts_2011_2015.txt"), sep="\n")
#     write(levels(factor(consorts$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list consorts.txt"), sep="\n")
#     write(levels(factor(nonconsorts$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list nonconsorts.txt"), sep="\n")
#     write(levels(factor(realconsorts$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list realconsorts.txt"), sep="\n")
#     write(levels(factor(maybeconsorts$Pub.number)), file=file.path(sourceloc, "Shell scripts and commands/file list maybeconsorts.txt"), sep="\n")
# }


# TO DO (maybe): split out multiple advisors

# if "function scratchpad.R" is being used, clean up unneeded variables
# rm(datawrangle, best_sort, filter_data, get_tags, matcharray, reorder_ser, rowdiff, rowmatch, shuffle, tagdiff, tagmatch, refactor.index)

