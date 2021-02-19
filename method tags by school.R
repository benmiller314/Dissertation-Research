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
require(data.table)

# make sure we've run dataprep.R
if(!exists("imageloc")) {
    source(file="start here.R")
}


# function for getting data
schoolwise.data <- function(dataset_name="knownprograms2001_2015", tagset_name="no_ped_tagnames") {
    
    # 0. convert variable names to variables. we'll use the names later in
    # the figure titles.
    dataset <- get(dataset_name, envir=globalenv())
    tagset <- get(tagset_name, envir=globalenv())
    
    # 1. remove columns other than method tags and school; convert to data table
    tagarray <- dataset[, which(names(dataset) %in% c("School", tagset))]
    tagarray <- data.table(tagarray, key="School")
    
    
    # 2. do the summary of each method type for all schools.
    totals <- tagarray[, .N, by="School"]
    counts <- tagarray[, lapply(.SD, sum), by="School"]
    
    normed <- round((counts[, ..tagset] / totals$N) * 100, 2)
    normed$School <- totals$School
    setkey(normed, School)
    setcolorder(normed)
    
    return(list("totals" = totals, "counts" = counts, "normed" = normed))
}

# function for graphing data
schoolwise <- function(dataset_name="knownprograms2001_2015", tagset_name="no_ped_tagnames", 
            agn=TRUE,        # run agglomerative clustering (using agnes)?
            hcl=TRUE,        # run hierarchical clustering (using hclust)?
            dia=TRUE,        # run divisive clustering (using diana)?
            show.totals=FALSE,    # label each row with the number of
                             #   dissertations per school?
            measure=c("normed", "counts"), # how to present tag data for each school: 
                             # as methodological focus (normed by school totals), or 
                             # as methodological output (raw counts of each tag)?
            agfixedcols=NULL,   # optional pre-set order of columns for 
                                #   comparison btwn agnes plots
            difixedcols=NULL,   # optional pre-set order of columns for 
                                #   comparison btwn diana plots,
            hcfixedcols=NULL,   # optional pre-set order of columns for 
                                #   comparison btwn hclust plots,
            myCol=NULL)         # optional color palette
    {
    
    # if colors are not provided, default to black and white
    require(RColorBrewer)
    if(is.null(myCol)) { myCol <- brewer.pal(9, "Greys") }
    
    # 0. convert variable names to variables. we'll use the names later in
    # the figure titles.
    dataset <- get(dataset_name)
    tagset <- get(tagset_name)
    
    # 1-2 call the data-grabbing function
    m1 <- schoolwise.data(dataset_name, tagset_name)
    measure <- match.arg(measure)
    m2 <- m1[[measure]]

    # 3. get more meaningful row names (and a purely numerical matrix, for
    # heatmapping) Note that the first column will always be the list of
    # schools because of the query in step 2; every tag.length in m1$counts 
    # should be the same.
    m3 <- data.matrix(m2[, -("School")])
    head(m3)
    
    if (show.totals) {
        row.names(m3) <- paste0(m2$School, " (", 
                                m1$totals$N, ")") 
    } else {
        row.names(m3) <- m2$School
    }
    
    colnames(m3) <- colnames(m2[, -("School")])
    
    head(m3)
    
    
    # try this old approach to finding the best sort method
        # agn_methods <- c("average","single","complete","ward","weighted");
        # agn <- lapply(agn_methods, FUN=function(x) { 
        #   agnes(m2, diss=F, metric=x)
        # })
        # agn_best.index <- max(c(agn[[1]]$ac, agn[[2]]$ac, agn[[3]]$ac,
        #                         agn[[4]]$ac, agn[[5]]$ac))
    
    # 4. make the heatmap: use pre-determined columns if need be.
    if(!exists("heatmap.fixedcols", mode="function")) {
        source(file="headmap fixedcols.R")
    }
    
    # reset the plot device
    plot.new()
    
    # 4b. divisive clustering (diana):
    if(dia) {
    filename <- file.path(imageloc, paste0("method tags by school, ", dataset_name, 
                    ", N", nrow(dataset), ", ", tagset_name, " (", measure, "), diana.pdf"))
    maintitle <- paste0("Method Tag Averages by school, ", dataset_name, 
                    ", ", tagset_name, ", diana")
    
        if(remake_figs) {
            pdf(file = filename)
        }
        
        if(!is.null(difixedcols)) {
            di <- heatmap.fixedcols(m3, 
                        myColInd = difixedcols,
                        hclustfun = function(d){ diana(d, metric="ward") },
                        scale = "row", 
                        col = myCol, 
                        main = maintitle, 
                        margins = c(5,10))
        } else {
            di <- heatmap(m3, 
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
                ", N", nrow(dataset), ", ", tagset_name, " (", measure, "), agnes.pdf"))
        maintitle <- paste0("Method Tag Averages by school, ", dataset_name,
                ", ", tagset_name, ", agnes")
    
        if(remake_figs) {
            pdf(file = filename)
        }
        
        if(!is.null(agfixedcols)) {
            ag <- heatmap.fixedcols(m3, 
                        myColInd = agfixedcols,
                        hclustfun = function(d){ agnes(d, method="ward") },
                        scale = "row", 
                        col = myCol, 
                        main = maintitle, 
                        margins = c(5,10)
            )
        } else {
            ag <- heatmap(m3, 
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
                    ", N", nrow(dataset), ", ", tagset_name, " (", measure, "), hclust.pdf"))
        maintitle <- paste0("Method Tag Averages by school, ", dataset_name,
                    ", ", tagset_name, ", hclust")
    
        if(remake_figs) {
            pdf(file = filename)
        }
        
        if(!is.null(hcfixedcols)) {
            hc <- heatmap.fixedcols(m3, 
                        myColInd = hcfixedcols, 
                        scale = "row", 
                        col = myCol, 
                        main = maintitle, 
                        margins = c(5,10)
            )
        } else {
            hc <- heatmap(m3, 
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
    
    agn = T
    hcl = T
    dia = T
    
    kp_order <- schoolwise(dataset_name = "knownprograms2001_2015", 
                           tagset_name = "no_ped_tagnames",
                           show.totals = T,
                           agn = agn,
                           hcl = hcl,
                           dia = dia)
    
    schoolwise(dataset_name = "knownprograms2001_2015",
               tagset_name = "no_ped_tagnames",
               # agfixedcols = ifelse(agn, kp_order$ag$colInd, NULL),
               # difixedcols = ifelse(hcl, kp_order$di$colInd, NULL),
               # hcfixedcols = ifelse(dia, kp_order$hc$colInd, NULL),
               show.totals = T,
               measure = "counts",
               agn = agn,
               hcl = hcl,
               dia = dia)
    
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
