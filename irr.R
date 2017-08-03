################################################################################
# irr.R
# 
# Inter-rater reliability testing
# 
# GOAL: Given a set of texts that have been tagged separately by two coders, 
#       find out how often they agree on which methods those texts use.
#  
# PLAN: 
#   1.  Read in a csv file with columns for Method_Terms, Flags, and Notes by each of 
#       two coders. 
#   2.  Transform the Method.Terms (and Exclude tag in the Flags column)
#       into a square matrix with the tags along both columns and rows,
#       so rows are Coder1's tags and columns are Coder2's tags.
#   3.  Parse tags and increment the value in the appropriate row or column
#   4.  Do some math
################################################################################

if (!exists("tagnames")) {
    source(file="~/Dropbox/coursework, etc/dissertation/data, code, and figures/Dissertation Research/dataprep.R")
}
source("method tag array.R")
source("simplifying the schema.R")
require(irr)

# invisible(readline("Select the csv file with metadata coded independently by two coders. (Press <Enter> to continue.)"))
trainingset <- read.csv(file=file.path(newdataloc, "inter-rater reliability test data.csv"), header=TRUE)

coder1 <- "Sandra"
coder2 <- "Andrew"

tagarray1 <- parse_tags(trainingset, 
                        tagstyle="short", 
                        tagcol=paste0("Method.Terms_", coder1), 
                        excludecol=paste0("Flags_", coder1),
                        standalone=T)
tagarray1 <- short_schema(tagarray1)

tagarray2 <- parse_tags(trainingset, 
                        tagstyle="short", 
                        tagcol=paste0("Method.Terms_", coder2), 
                        excludecol=paste0("Flags_", coder2),
                        standalone=T)
tagarray2 <- short_schema(tagarray2)
names(tagarray2)


tagsums1 <- colSums(tagarray1[tagnames])
tag <- colSums(tagarray2[tagnames])

# compare simple schema
simple1 <- colSums(tagarray1[tagnames.simple])
simple2 <- colSums(tagarray2[tagnames.simple])

simplegrid <- rbind(simple1, simple2)

# Compare excludes
exclude_table <- table(cbind(tagarray1["Exclude.Level"], tagarray2["Exclude.Level"]))
kripp.alpha(exclude_table, "nominal")

sum(tagsums1)
sum(tagsums2)

## work up manually to agreement
# empty array
agree_array <- matrix(nrow=length(tagnames), ncol=length(tagnames),
                      dimnames=list(tagnames, tagnames),
                      data=0L)
agree_array <- as.data.frame(agree_array, stringsAsFactors = F)

# plan: go row-wise through the dissertations; 
#       where both coders agreed on a tag (either to include or to exclude it),
#       increment the diagonal for that tag. 
#       where they disagree, increment the appropriate row or column off the diagonal.

## UPDATE: This won't work with non-exclusive tagging. Need a new strategy entirely ##

x <- "Crit"

cor(tagarray1[tagnames], tagarray2[tagnames])
i = 1
for (i in 1:nrow(tagarray1)) {
    agreeindex <- which(tagarray1tags[i,] == tagarray2tags[i,])
    disagreeindex <- which(tagarray1tags[i,] != tagarray2tags[i,])
    for (j in agreeindex) {
        agree_array[j,j] <- agree_array[j,j] + tagarray1tags[i,j]
    }
    for (j in disagreeindex) {
        agree_array[i,j] <- tagarray1tags[i,j]
        agree_array[j,i] <- 
    }
    
    agree_array[1, tagnames] <- agree_array[1, tagnames] + tagarray2[1, tagnames]
    agree_array[tagnames, 1] <- agree_array[tagnames, 1] + t(tagarray1[1, tagnames])
}

