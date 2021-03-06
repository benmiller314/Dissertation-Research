## TO DO: make this a function that takes dataset and flag_column as a parameter

m <- noexcludes$Method.Terms
m1 <- as.character(m)
m2 <- sapply(m1, FUN=function(x) strsplit(x,"|",fixed=TRUE))

# find everything that might need checking
checkcount <- sapply(m2, FUN=function(x) length(grep("~check", x, ignore.case=TRUE)))
sum(checkcount)

# filter out the ones that are maybe worth checking from those that def need checking
checkmaybes <- sapply(m2, FUN=function(x) length(grep("~check \\?", x, ignore.case=TRUE)))
sum(checkmaybes)
m5 <- noexcludes[which(checkmaybes < checkcount),]
nrow(m5)

# save the file that still needs checking
filename <- file.path(dataloc, paste0("noexcludes in need of checking ", Sys.Date(), ".csv"))
write.csv(m5, file=filename)



## test
# data.frame(noexcludes[1:20, which(names(noexcludes) %in% c("Method.Terms"))], as.factor(checkcount[1:20]), row.names=NULL)

allchecks <- which(checkcount == noexcludes$Method.Count)
backup <- noexcludes[allchecks,]

levels(noexcludes$Method.Terms) <- levels(factor(c(levels(noexcludes$Method.Terms),"Other")))
noexcludes[allchecks, which(names(noexcludes) %in% c("Method.Terms"))] <- "Other"

## Make sure that worked!
# noexcludes[allchecks, which(names(noexcludes) %in% c("Method.Terms","ABSTRACT"))]
# noexcludes$Method.Terms

print(paste("Converted questionable method terms in",length(allchecks),"rows. Row indices affected:"))
print(as.numeric(allchecks))

## To restore replaced rows:
# levels(noexcludes$Method.Terms) <- levels(factor(m))
# noexcludes[allchecks,] <- backup
# noexcludes[2697,]


# Now add an "Other" column
m3 <- as.character(noexcludes$Method.Terms)			# recalculate with new Others

other.index <- grep("Other", m3, ignore.case=TRUE)
noexcludes$Othr <- 0
noexcludes[other.index,]$Othr <- 1

# And, finally, let's recalculate method counts. 
m4 <- sapply(m3, FUN=function(x) unlist(strsplit(x,"|",fixed=TRUE)))
noexcludes$Method.Count <- sapply(m4, FUN=length)




# not sure why this didn't work, but whatevs
# others <- sapply(m3, FUN=function(x) if (grep("Other", x, ignore.case=TRUE) return(1) else return(0)))

# remove interim variables to save memory
rm(m, m1, m2, m3, m4, m5, allchecks, backup, other.index)
