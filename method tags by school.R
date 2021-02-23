#############################################################################
# method tags by school.R
#
# GOAL: Given a tagged set of dissertation data and a tagging schema,
# aggregate tag frequency and distribution at each school in the dataset.
# After building the function for the analysis, run it on various subsets of
# data and tags.
#####

## load required packages
# require(doBy)
require(cluster)
require(data.table)
require(gplots)

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
            myclustfun = c("agnes", "diana", "hclust"),
                                # run agglomerative clustering (using agnes)?
                                # run divisive clustering (using diana)?
                                # run hierarchical clustering (using hclust)?
            show.totals=FALSE,    # label each row with the number of
                             #   dissertations per school?
            measure=c("normed", "counts"), # how to present tag data for each school: 
                             # as methodological focus (normed by school totals), or 
                             # as methodological output (raw counts of each tag)?
            myCol=NULL,         # optional color palette
            mycex = 0.5,        # scaling factor for label font size
            myroworder = TRUE,  # if you have schools or tags sorted from a prior run,
            mycolorder = TRUE   # you can pass the Rowv (schools) or Colv (tags) here 
                                # to use that ordering. TRUE means use the clustering method 
                                # to recalculate the ordering now.
                          
    ){
    
    # if colors are not provided, default to black and white
    require(grDevices)
    if(is.null(myCol)) { myCol <- gray.colors(9, start=0, end=1, rev=T) }
    
    # make sure we're using a legal clustering function
    myclustfun <- match.arg(myclustfun, several.ok = TRUE)
    
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
    
    # 4a. base names (add clust method and .pdf to the ends)
    filenamebase <- file.path(imageloc, paste0("method tags by school, ", dataset_name, 
                          ", N", nrow(dataset), ", ", tagset_name, " (", measure, ")"))
    if(measure == "normed") {
        title_keyword <- "Focus"
    } else {
        title_keyword <- "Output"
    }
    maintitlebase <- paste0("Methodological ", title_keyword," (", measure, ") by school, ",
                            dataset_name, ", ", tagset_name)
    
    
    # 4b. iterate through possible clustering methods
    
    to.return <- list()
    
    for (fun_name in myclustfun) {
        fun <- match.fun(fun_name)
        if(fun_name %in% c("agnes", "diana")) {
            myfun <- function(d) { fun(d, metric="ward") }
        } else {
            myfun <- fun
        }
        
        filename <- paste0(filenamebase, " -- ", fun_name, ".pdf")
        maintitle <- paste0(maintitlebase, " -- ", fun_name)
        
        if(remake_figs) {
            pdf(file = filename)
        }
        
        tmp <- heatmap(m3, 
                        hclustfun = myfun,
                        scale = "none", 
                        col = myCol, 
                        main = maintitle, 
                        margins = c(5,10),
                        cexRow = mycex,
                        Rowv = myroworder,
                        Colv = mycolorder,
                        keep.dendro = T
                )
        
        mtext(paste("Each cell gives the ", measure, "frequency",
                    "that a given dissertation from the school in row Y",
                    "is tagged with the method in column X.", side = 1))
        
        if(remake_figs) {
            dev.off()
        }
        
        to.return[fun_name] <- list(tmp)
    } # end of for loop: repeat for each clustering method 
    

    # save the row and column orders to allow for consistent sorting later
    return(to.return)
    
# close wrapper function schoolwise()
}

if (FALSE) {
    remake_figs=T
    # call the functions for all relevant datasets
    
    agn = F
    hcl = T
    dia = F
    
    kp_order <- schoolwise(dataset_name = "knownprograms2001_2015", 
                           tagset_name = "no_ped_tagnames",
                           show.totals = T,
                           measure = "normed",
                           myclustfun = c("di"),
                           myCol = gray.colors(20, start = 0, end = 1, rev=T))
    
    schoolwise(dataset_name = "knownprograms2001_2015",
               tagset_name = "no_ped_tagnames",
               show.totals = T,
               measure = "counts",
               myclustfun = c("di"),
               myroworder = kp_order$diana$Rowv,
               myCol = gray.colors(20, start = 0, end = 1, rev=T))
    
    remake_figs=F
    require(RColorBrewer)
    schoolwise(dataset_name = "knownprograms2001_2015", 
               tagset_name = "no_ped_tagnames",
               show.totals = T,
               measure = "normed",
               agn = F,
               hcl = T,
               dia = F,
               myCol = c("#FFFFFF", brewer.pal(8, "YlOrRd"), "#000000"))
    
    schoolwise(dataset_name = "knownprograms2001_2015", 
               tagset_name = "no_ped_tagnames",
               show.totals = T,
               measure = "normed",
               agn = F,
               hcl = T,
               dia = F)
    
    
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
