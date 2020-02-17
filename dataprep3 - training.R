################################################################################
# dataprep 3 - training set for methods schema
#
# GOAL: For each method tag, find dissertations with only that tag, and 
#       extract a sample subset of rows and relevant columns
#       so that new research partners can practice tagging new abstracts.
#
########

# Make sure we have data to work with.
if (!exists("tagnames")) {
    source(file="~/Dropbox/coursework, etc/dissertation/data, code, and figures/Dissertation Research/start here.R")
}

# Isolate rows with only one method
mono.index <- which(realconsorts$Method.Count == 1)
monomethodics <- realconsorts[mono.index, ]

# Bug hunting
error.index <- which(rowSums(monomethodics[,tagnames]) != 1)
monomethodics[error.index, c("Method.Terms", tagnames, "Method.Count")]

error.index <- which((realconsorts$Method.Count == 1) & (rowSums(realconsorts[,tagnames]) != 1))
realconsorts[error.index, c("Method.Terms", tagnames, "Method.Count")]

# So weird! just fix it. TO DO: look for bugs in file="update realconsorts.R"
realconsorts <- parse_tags(realconsorts)

# Let's try that again. Sigh.
mono.index <- which(realconsorts$Method.Count == 1)
monomethodics <- realconsorts[mono.index, ]


# Ignore questionable rows 
nonflag.index <- grep("~flag|~wait, what", monomethodics$Method.Terms, invert=T)
monomethodics <- monomethodics[nonflag.index,]

    # make sure that worked
    monomethodics$Method.Terms
    
    # and left us with enough to work with
    mono.sums <- colSums(monomethodics[,tagnames])
    message("Methods of unflagged single-method dissertations:")
    print(mono.sums)



# Limit returned values to columns that you'll use in the tagging spreadsheet
my.cols <- c("Pub.number", "Author", "Title", "Pages", "School", "Department", "Degree", "Year", "Advisor.Name", "Subject", "ABSTRACT", "Method.Terms", "Flag.notes")
    
# Function for extracting rows using only one given method
get.sample <- function(my.method = "Meta", size=3, random=T, testsize=0) {
    size <- min(size, mono.sums[my.method])
    index <- which(monomethodics[my.method] == 1)
    if(random) { 
        my.sample <- sample(index, size) 
    } else {
        my.sample <- head(index, size)
    }
    
    rows <- monomethodics[my.sample, my.cols]
    if(testsize > 0) {
        if(size == 1) {
            rows$Train.or.Test <- 0
        } else if (size == 2) {
            rows$Train.or.Test <- c(0,0)
        } else {
            zeros <- rep(0, min(testsize, size-1))
            rows$Train.or.Test <- c(zeros, rep(1, size-length(zeros)))    
        }
    }
    return(rows)
}   
# # test it
# a = get.sample()
# rbind(a, a)
# rm(a)

# start empty, build up
to.return <- c()
for (i in tagnames) {
    to.return <- rbind(to.return, get.sample(i, random=F, size=10))
}

nrow(to.return)

filename <- file.path(dataloc, "sample data for training2.csv")
write.csv(to.return, filename)





