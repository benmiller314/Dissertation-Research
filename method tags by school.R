#############################################################################
# method tags by school.R
#
# GOAL: Given a tagged set of dissertation data and a tagging schema,
# aggregate tag frequency and distribution at each school in the dataset.
# After building the function for the analysis, run it on various subsets of
# data and tags.
#####

# load required packages
require(doBy)
require(cluster)
require(RColorBrewer)

# make sure we've run dataprep.R
if(!exists("imageloc")) {
    source(file="start here.R")
}


# function for getting data
schoolwise.data <- function(dataset_name="consorts", tagset_name="tagnames") {
    
    # 0. convert variable names to variables. we'll use the names later in
    # the figure titles.
    dataset <- get(dataset_name, envir=parent.frame())
    tagset <- get(tagset_name, envir=parent.frame())
    tagset.mean <- sapply(tagset, FUN=function(x) paste0(x,".mean"))
    tagset.sum <- sapply(tagset, FUN=function(x) paste0(x,".sum"))
    
    # 1. remove columns other than method tags and school
    dataset <- dataset[, which(names(dataset) %in% c("School", tagset))]
    
    # 2. do the summary of each method type for all schools. 
    d1 <- summaryBy(. ~ School, data=dataset, FUN=mean)
    d2 <- summaryBy(. ~ School, data=dataset, FUN=sum)
    d3 <- summaryBy(. ~ School, data=dataset, FUN=length)
        
    return(list("means" = d1, "sums" = d2, "counts" = d3))
}

# function for graphing data
schoolwise <- function(dataset_name="noexcludes", tagset_name="tagnames", 
            agn=TRUE,        # run agglomerative clustering (using agnes)?
            hcl=TRUE,        # run hierarchical clustering (using hclust)?
            dia=TRUE,        # run divisive clustering (using diana)?
            counts=FALSE,    # label each row with the number of
                             #   dissertations per school?
            agfixedcols=NULL,   # optional pre-set order of columns for 
                                #   comparison btwn agnes plots
            difixedcols=NULL,   # optional pre-set order of columns for 
                                #   comparison btwn diana plots
            myCol=NULL)         # optional color palette
    {
    
    # if colors are not provided, default to black and white
    require(RColorBrewer)
    if(is.null(myCol)) { myCol <- brewer.pal(9, "Greys") }
    
    # 0. convert variable names to variables. we'll use the names later in
    # the figure titles.
    dataset <- get(dataset_name)
    tagset <- get(tagset_name)
    tagset <- sapply(tagset, FUN=function(x) paste0(x,".mean"))
    
    # 1-2 call the data-grabbing function
    m1 <- schoolwise.data(dataset_name, tagset_name)
    m2 <- m1$means

    # 3. get more meaningful row names (and a purely numerical matrix, for
    # heatmapping) Note that the first column will always be the list of
    # schools because of the query in step 2.
    if (counts) {
        row.names(m2) <- paste0(m2$School, " (", 
                                m1$counts$School.length, ")") 
    } else {
        row.names(m2) <- m2$School
    }
    m2 <- m2[,2:ncol(m2)]
    m2 <- data.matrix(m2)
    head(m2)
    
    # try this old approach to finding the best sort method
        # agn_methods <- c("average","single","complete","ward","weighted");
        # agn <- lapply(agn_methods, FUN=function(x) { 
        #   agnes(m2, diss=F, metric=x)
        # })
        # agn_best.index <- max(c(agn[[1]]$ac, agn[[2]]$ac, agn[[3]]$ac,
        #                         agn[[4]]$ac, agn[[5]]$ac))
    
    # 4. make the heatmap: use pre-determined columns if need be.

    # 4b. divisive clustering (diana):
    if(dia) {
    filename <- file.path(imageloc, paste0("tags by schools, ", dataset_name, 
                    ", N", nrow(dataset), ", ", tagset_name, ", diana.pdf"))
    maintitle <- paste0("Method Tag Averages by school, ", dataset_name, 
                    ", ", tagset_name, ", diana")
    
        if(remake_figs) {
            pdf(file = filename)
        }
        
        if(!is.null(difixedcols)) {
            di <- heatmap.fixedcols(m2, 
                        myColInd = difixedcols,
                        hclustfun = function(d){ diana(d, metric="ward") },
                        scale = "row", 
                        col = myCol, 
                        main = maintitle, 
                        margins = c(5,10))
        } else {
            di <- heatmap(m2, 
                        hclustfun = function(d){ diana(d, metric="ward") },
                        scale = "row", 
                        col = myCol, 
                        main = maintitle, 
                        margins = c(5,10)
            )
        }
        mtext(paste("Each cell gives the likelihood that a given", 
                "dissertation from the school in row Y is tagged with the",
                "method in column X.", side = 1))
                
        if(remake_figs) {
            dev.off()
        }
    } # end of if(dia)
        
    # 4a. agglomerative clustering (agnes):
    if(agn) {
        filename <- file.path(imageloc, paste0("tags by schools, ", dataset_name, 
                ", N", nrow(dataset), ", ", tagset_name, ", agnes.pdf"))
        maintitle <- paste0("Method Tag Averages by school, ", dataset_name,
                ", ", tagset_name, ", agnes")
    
        if(remake_figs) {
            pdf(file = filename)
        }
        
        if(!is.null(agfixedcols)) {
            ag <- heatmap.fixedcols(m2, 
                        myColInd = agfixedcols,
                        hclustfun = function(d){ agnes(d, method="ward") },
                        scale = "row", 
                        col = myCol, 
                        main = maintitle, 
                        margins = c(5,10)
            )
        } else {
            ag <- heatmap(m2, 
                        hclustfun = function(d){ agnes(d,method="ward") }, 
                        scale = "row", 
                        col = myCol, 
                        main = maintitle, 
                        margins = c(5,10)
            )
        }
        mtext(paste("Each cell gives the likelihood that",
                    "a given dissertation from the school in row Y",
                    "is tagged with the method in column X.", side = 1))
    
        if(remake_figs) {
            dev.off()
        }
    }   # end of if(agn)

        # 4c. agglomerative clustering via hclust:
    if(hcl) {
        filename <- file.path(imageloc, paste0("tags by schools, ", dataset_name, 
                    ", N", nrow(dataset), ", ", tagset_name, ", hclust.pdf"))
        maintitle <- paste0("Method Tag Averages by school, ", dataset_name,
                    ", ", tagset_name, ", hclust")
    
        if(remake_figs) {
            pdf(file = filename)
        }
        
        if(!is.null(agfixedcols)) {
            hc <- heatmap.fixedcols(m2, 
                        myColInd = agfixedcols, 
                        scale = "row", 
                        col = myCol, 
                        main = maintitle, 
                        margins = c(5,10)
            )
        } else {
            hc <- heatmap(m2, 
                        scale = "row", 
                        col = myCol, 
                        main = maintitle, 
                        margins = c(5,10)
            )
        }
        mtext(paste("Each cell gives the likelihood",
                    "that a given dissertation from the school in row Y",
                    "is tagged with the method in column X.", side = 1))
                    
        if(remake_figs) {
            dev.off()
        }
    }   # end of if(clust)

    if(!exists("di", inherits=F)) di <- noquote("Not run")
    if(!exists("ag", inherits=F)) ag <- noquote("Not run")
    if(!exists("hc", inherits=F)) hc <- noquote("Not run")

    # save the row and column orders to allow for consistent sorting later
    return(list("di" = di, "ag" = ag, "hc" = hc))
    
# close wrapper function schoolwise()
}

if (FALSE) {
    remake_figs=F
    # call the functions for all relevant datasets
    
    schoolwise("knownprograms2001_2015", "no_ped_tagnames")
    
    
    schoolwise("consorts", "tagnames", agn=T, hcl=F, dia=F)
    schoolwise("nonconsorts", "tagnames", agn=T, hcl=F, dia=F)
    schoolwise("top.nonconsorts", "tagnames", agn=T, hcl=F, dia=F)
    schoolwise("noexcludes", "tagnames")
    # schoolwise("nonconsorts", "tagnames", agfixedcols=a$ag$colInd,
    #        difixedcols=a$di$colInd)
    schoolwise("consorts.plus", agn=T, hcl=F, dia=F)
    schoolwise(dataset_name = "noexcludes2001_2015", tagset_name = "no_ped_tagnames")
    schoolwise("consorts2001_2015", "no_ped_tagnames")
    schoolwise("realconsorts2001_2015", "no_ped_tagnames")

    schoolwise("noexcludes2011_2015", "tagnames")
    schoolwise("consorts2011_2015", "tagnames")
    schoolwise("realconsorts2011_2015", "tagnames")
    
    
    # next up: re-run with the simplified schema
    schoolwise("consorts", "tagnames.simple")
    schoolwise("nonconsorts", "tagnames.simple")
    schoolwise("noexcludes", "tagnames.simple")
    schoolwise("noexcludes2001_2015", "tagnames.simple")
    schoolwise("consorts2001_2015", "tagnames.simple")
    schoolwise("realconsorts2001_2015", "tagnames.simple")
    # schoolwise("consorts", "tagnames.simple", agfixedcols=c$ag$colInd,
    #        difixedcols=c$di$colInd)
    # schoolwise("nonconsorts", "tagnames.simple", agfixedcols=c$ag$colInd,
    #        difixedcols=c$di$colInd)
} else {
    message("Loaded the following functions:")
    message("    schoolwise.data()    # helper function to load data")
    message("    schoolwise()         # function to graph data")
}


# # explore the data
# d <- order(c$byschool$Aggreg.mean, decreasing=TRUE)
# schoolwise("consorts", "tagnames.simple", agfixedcols=d, difixedcols=d)

# c$byschool[c$di$rowInd, which(names(c$byschool) %in% 
# sapply(tagnames.simple, FUN=function(x) paste0(x,".mean")))]
# which(rowsum(c$byschool, row.names(c$byschool)) == 0)
# ?rowsum
# # remove interim variables
# rm(m1, m2, m3, noex.by.school.m, nonconsorts.by.school.m, 
# consorts.by.school.m)