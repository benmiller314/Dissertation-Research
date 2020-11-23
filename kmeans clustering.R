## K-Means Clustering. The plan:
# 1. Get a random subset of dissertation data to work with
# 2. Extract just the tag array
# 3. Try several reasonable guesses for number of clusters; optimize 

# Set up-front parameters
sample_size <- 200
num_clusters <- 10
verbose <- TRUE
set.seed(123)		

# Check that we've already run dataprep.R; if not, run it.
if(!exists("noexcludes")) {
	source(file="dataprep.R")
}


# Step 1: get the subset
rand.index <- sample(1:nrow(noexcludes),sample_size)
rand <- noexcludes[rand.index,]

# Step 2: extract the tags
rand.tags <- get_tags(rand)

# Step 3, part 1: try kmeans using num_clusters
fit <- kmeans(rand.tags, num_clusters, nstart=500)
if(verbose){print(fit)}

fit.c <- data.frame(fit$centers)
if(verbose){print(fit.c)}
heatmap.ben(fit.c)

# sort.list(fit.c)

fit.index <- fit$cluster[order(fit$cluster)]

if(verbose) {
	for (i in 1:num_clusters) { 
		a <- which(fit.index == i)
		writeLines(paste("\t\t-- Cluster",i,"--"))
		print(noexcludes[names(fit.index[a]),c("Title")])
		writeLines("\n")
		print(100*round(fit.c[i,],2))	
		writeLines("\n")
	}
}

# Step 3, part 2: try kmeans using all values from 2:num_clusters 
# so as to find the optimal number of clusters
# (not yet implemented; apparently there's a tool for this in package(fpc))

# d <- dist(rand.tags)
# library(fpc)
# clusterboot(rand.tags, seed=123, clustermethod=kmeansCBI, krange=1:2)

# nselectboot(rand.tags,clustermethod=kmeansCBI,krange=9:10, seed=123)







# ?dist
# dist(rand.tags)








## Older approaches
# # this file assumes you've already run "collocation heatmap.R"

# head(sum.by.tags.n)

# # K-Means Cluster Analysis
# set.seed(123)
# fit <- kmeans(sum.by.tags.n, 7, nstart=500)		# assumes 7 clusters
# fit$cluster[order(fit$cluster)]


# # get cluster means
# agg <- aggregate(sum.by.tags.n,by=list(fit$cluster),FUN=function(d){round(mean(d),2)})
# agg <- agg[,2:length(agg)]
# heatmap.ben(agg)


# # append cluster assignment
# mydata <- data.frame(mydata, fit$cluster) 