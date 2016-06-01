########
#
# `update realconsorts.R`: Runs during `dataprep 2 - load data.R`. Adds a column
#  to noexcludes as an index to realconsorts, i.e. real consortium program 
#  dissertations. After verifying the departments/programs of dissertations at
#  consortium schools, update the file in line 15.
#
###

# get the department-matching data
a <- read.csv(file=paste0(dataloc, "department-gathering2.csv"))
    
# read out just the Pub.numbers from confirmed dissertations in Consortium programs
index.by.pub <- a[which(a$Consortium == "yes"), "Pub.number"]
    
# use those numbers to update noexcludes
noexcludes[noexcludes$Pub.number %in% index.by.pub, "realconsort"] <- 1 
noexcludes[!noexcludes$Pub.number %in% index.by.pub, "realconsort"] <- 0
    
# sanity check: confirm that we're only getting dissertations at Consortium schools
if (all(noexcludes[noexcludes$Pub.number %in% index.by.pub, "School"] %in% conschools)) {
    message(paste("Found", length(index.by.pub), "dissertations from Consortium programs."))
} else {
    warning("`update realconsorts.R` indexes non-Consortium schools. Time to debug!")
}

