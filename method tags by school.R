#############################################################################
# method tags by school.R
#
# GOAL: Given a tagged set of dissertation data and a tagging schema,
# aggregate tag frequency and distribution at each school in the dataset.
# After building the function for the analysis, run it on various subsets of
# data and tags.
# 
# TO DO: Update to use heatmap.2 to automatically incorporate color labels
# See https://www.youtube.com/watch?v=T7_j444LMZs for an example
# 
# MAYBE: add a sidecolor for subcategories of schools, 
# e.g. size / methodological range, as in my annotated printout?
# 
#####

## load required packages
# require(doBy)
require(cluster)     # for divisive and agglomerative clustering
require(data.table)  # for fast indexing and aggregating by school
require(gplots)      # for heatmap.2, to get sidecolors

# make sure we've run dataprep.R
if(!exists("imageloc")) {
    source(file="start here.R")
}


# function for getting data
schoolwise.data <- function(dataset_name = "knownprograms2001_2015", 
                            tagset_name = "no_ped_tagnames",
                            spread_threshold = 1) {
    
    # 0. convert variable names to variables. we'll use the names later in
    # the figure titles.
    dataset <- get(dataset_name, envir=parent.frame())
    tagset <- get(tagset_name, envir=parent.frame())
    
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
    
    # 3. how many methods are represented at each school?
    schoolspread <- counts[, rowSums(.SD >= spread_threshold), .SDcols=-"School", by="School"]
    names(schoolspread) <- c("School", "MethodCount")
    
    # 4. at how many schools is each method represented?
    methodspread <- counts[, colSums(.SD >= spread_threshold), .SDcols=-"School"]
    
    
    return(list("totals" = totals, 
                "counts" = counts, 
                "normed" = normed, 
                "methodspread" = methodspread,
                "schoolspread" = schoolspread))
}

# function for graphing data
schoolwise <- function(dataset_name="knownprograms2001_2015", 
            tagset_name="no_ped_tagnames", 
            myclustfun = c("agnes", "diana", "hclust"),
                                # run agglomerative clustering (using agnes)?
                                # run divisive clustering (using diana)?
                                # run hierarchical clustering (using hclust)?
            show.totals=FALSE,    # label each row with the number of
                             #   dissertations per school?
            measure=c("normed", "counts"), # how to present tag data for each school: 
                             # as methodological focus (normed by school totals), or 
                             # as methodological output (raw counts of each tag)?
            mycex = 0.4,        # scaling factor for label font size
            myroworder = TRUE,  # if you have schools or tags sorted from a prior run,
            mycolorder = TRUE,  # you can pass the Rowv (schools) or Colv (tags) here 
                                # to use that ordering. TRUE means use the clustering method 
                                # to recalculate the ordering now.
            myCol=NULL,         # optional color palette
            filename_suffix = NULL,  # used primarily for indicating color scheme
            min_disses = 1,       # how many dissertations per school before we include it?
            thresh_start = 2001, # start of range within which to achieve threshold counts
            thresh_end = 2015    # end of range within which to achieve threshold counts
                          
    ){
    
    # if colors are not provided, default to black and white
    require(grDevices)
    if(is.null(myCol)) { myCol <- gray.colors(9, start=0, end=1, rev=T) }
    
    # make sure we're using a legal clustering function
    myclustfun <- match.arg(myclustfun, several.ok = TRUE)
    
    # better filenames
    if(!is.null(filename_suffix) && substr(filename_suffix, 2, 2) != "-") {
        filename_suffix <- paste0(" -- ", filename_suffix)
    }
    
    # 0. convert variable names to variables. we'll use the names later in
    # the figure titles. UPDATE: do we need this? REPLY: it helps debug, and does little harm
    dataset <- get(dataset_name)
    tagset <- get(tagset_name)
    nrow(dataset)
    
    # 1. Thresh the data to ensure we're looking at active programs
    if (!exists("thresh", mode="function")) {
        source(file = "thresh.R")
    }
    
    m0 <- thresh(dataset_name, tagset_name,
                 threshold = min_disses,
                 since = thresh_start,
                 until = thresh_end)$thresh.data
    nrow(m0)
    
    # 2. Extract just the relevant tag data
    m1 <- schoolwise.data("m0", tagset_name)
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
    
    # 4a. base names (we'll later add clust method and .pdf to the ends)
    filenamebase <- file.path(imageloc, paste0("method tags by school, ", dataset_name, 
                          ", N", nrow(m0), ", ", tagset_name, " (", measure, "), min ",
                          min_disses, ", ", thresh_start, "-", thresh_end))
    if(measure == "normed") {
        title_keyword <- "Focus"
    } else {
        title_keyword <- "Output"
    }
    maintitlebase <- paste0("Methodological ", title_keyword," (", measure, ") by school,\n",
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
        
        filename <- paste0(filenamebase, " -- ", fun_name, filename_suffix, ".pdf")
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
                        mar = c(5, 5, 10, 10),
                        cexRow = mycex,
                        Rowv = myroworder,
                        Colv = mycolorder,
                        keep.dendro = T,
                        cex.main = 0.7
        )
        
        mtext(paste("Each cell gives the", measure, "frequency",
                    "that a given dissertation from the school in row Y",
                    "is tagged with the method in column X."), side = 1)
        
        if(remake_figs) {
            dev.off()
        }
        
        to.return[fun_name] <- list(tmp)
    } # end of for loop: repeat for each clustering method 
    

    # save the row and column orders to allow for consistent sorting later
    return(to.return)
    
# close wrapper function schoolwise()
}

# What methods are most and least distributed across schools?
method_spread_across_schools <- function(dataset_name = "knownprograms2001_2015",
                                         tagset_name = "no_ped_tagnames",
                                         min_disses = 1,       # how many dissertations per school before we include it?
                                         thresh_start = 2001, # start of range within which to achieve threshold counts
                                         thresh_end = 2015,    # end of range within which to achieve threshold counts
                                         do.plot = T,
                                         color_palette = group_pal,
                                         spread_threshold = 1,
                                         pcts = T
                                         
){
    
    # 1. Thresh the data to ensure we're looking at active programs
    if (!exists("thresh", mode="function")) {
        source(file = "thresh.R")
    }
    
    m0 <- thresh(dataset_name, tagset_name,
                 threshold = min_disses,
                 since = thresh_start,
                 until = thresh_end)$thresh.data
    
    
    school_corrs <- schoolwise.data("m0",
                                    tagset_name,
                                    spread_threshold)
    
    methodspread.index <- order(school_corrs$methodspread, decreasing = T)
    
    if(remake_figs) { 
        outfile <- paste0("method-spread--", dataset_name, "--", tagset_name,
                          "--tagmin", spread_threshold, "--schoolmin", min_disses)
        outfile <- paste0(outfile, ".pdf")
        pdf(file.path(imageloc, outfile)) 
    }
    
    if(do.plot) {
        
        to.plot <- school_corrs$methodspread[methodspread.index] 
        if(pcts) {
            to.plot <- to.plot / nrow(school_corrs$counts)
        }
        
        p <- barplot(to.plot,
                     las = 2,
                     col = color_palette[taggroups[names(school_corrs$methodspread)[methodspread.index]]],
                     yaxp = if(pcts) c(0, 1, 5) else c(0, nrow(school_corrs$counts), 5),
                     yaxs = "r",
                     # main = "Presence of methods across schools \nis broader than raw counts would suggest",
                     main = paste0(if(pcts) {"Percentage"} else {"Number"}, " of programs with dissertations\nusing methods in the schema"),
                     sub = paste0(dataset_name, ", N = ", nrow(school_corrs$counts), " schools with at least ",
                                  min_disses, " dissertation", if(min_disses > 1) "s", ", ", thresh_start, "-", thresh_end, ";\n",
                                  "A school is counted if at least ", spread_threshold, " diss", if(spread_threshold > 1) "es",
                                  " there ", if(spread_threshold > 1) "are" else "is", " tagged with the method"
                     )
        )
    }
    
    outside_legend(x = "topright", 
                   legend = names(color_palette), 
                   fill = color_palette, 
                   bty="n"
    )
    
    if(remake_figs) { dev.off() }
    
    return(school_corrs$methodspread)
}   # close function method_spread_across_schools() 



if (FALSE) {
    remake_figs=F
    # call the functions for all relevant datasets
    
    require(RColorBrewer)
    require(viridisLite)
    
    # test color palettes; UPDATE: viridisLite::magma is the clear winner!
    # colorscheme <- list(name = "grayscale", values = gray.colors(20, start = 0, end = 1, rev=T))
    # colorscheme <- list(colorscheme,
                            # list(name = "YlOrRd", values = c("#FFFFFF", brewer.pal(9, "YlOrRd"))),
    colorscheme <-          list(name = "magma", values = c("#FFFFFF", magma(99, dir=-1)))
                        # )
    
    if(!exists("sumbytags", mode="function")) {
        source(file = "method collocation heatmap.R")
    }
    
    method_corrs <- sumbytags(dataset_name, tagset_name, 
                              doplot=T, 
                              normed=T, 
                              dendro=T)
    
    school_corrs <- schoolwise.data(dataset_name, tagset_name)
    
    remake_figs <- T
    
    for (clustfun in c("diana"
                       # , "agnes", "hclust"
                       )) {
    # for (clustfun in c("hclust")) {
        for(i in seq_along(colorscheme)) {
        
            kp_order <- schoolwise(dataset_name = "knownprograms2001_2015", 
                           tagset_name = "no_ped_tagnames",
                           show.totals = T,
                           measure = "normed",
                           myclustfun = clustfun,
                           mycolorder = method_corrs$Colv,
                           myCol = if(is.null(colorscheme$values)) colorscheme[[i]]$values else colorscheme$values,
                           filename_suffix = if(is.null(colorscheme$name)) colorscheme[[i]]$name else colorscheme$name,
                           min_disses = 5
            )
    
            schoolwise(dataset_name = "knownprograms2001_2015",
                       tagset_name = "no_ped_tagnames",
                       show.totals = T,
                       measure = "counts",
                       myclustfun = clustfun,
                       myroworder = kp_order[[clustfun]]$Rowv,
                       mycolorder = method_corrs$Colv,
                       myCol = if(is.null(colorscheme$values)) colorscheme[[i]]$values else colorscheme$values,
                       filename_suffix = if(is.null(colorscheme$name)) colorscheme[[i]]$name else colorscheme$name,
                       min_disses = 5
            )
        }
    }
    
    remake_figs <- F
    
    if(!exists("method_corrs_one_row", mode="function")) {
        source(file = "method collocation heatmap.R")
    }
    
    remake_figs = T
    
    myrows <- c("Pennsylvania State University-Main Campus",
                "New Mexico State University-Main Campus",
                "University of Pittsburgh-Pittsburgh Campus")
    
    myrows <- c("Indiana University of Pennsylvania",
                "University of Arizona",
                "Purdue University-Main Campus")
    
    row <- "University of California-Irvine"
    
    for(row in myrows) {
        method_corrs_one_row(myrow = row,
                         corr_type = "school",
                         dataset_name = "knownprograms2001_2015",
                         tagset_name = "no_ped_tagnames",
                         color_groups = T,
                         taggroups = no_ped_taggroups,
                         normed = F,
                         # normed = T,   # maybe normed axis, use text() to add count values?
                         corr_obj = school_corrs,  # the object with correlation data.
                         colInd = method_corrs$colInd)
    }                                 
    
    remake_figs=F

    
    # Is there a correlation between school size and method spread?
    school_corrs <- schoolwise.data(dataset_name, tagset_name)
    x <- school_corrs$totals$N
    y <- school_corrs$schoolspread$MethodCount
    
    if(remake_figs) { 
        outfile <- paste0("method_count-v-diss_count--", dataset_name, 
                          "--", tagset_name, ".pdf")
        outfile <- file.path(imageloc, outfile)
        pdf(outfile)
    }
    
    plot(x, y, pch=1, bty="n", las=1,
         xlab = "Confirmed RCWS Dissertations at school, 2001-2015",
         ylab = "Methods represented (out of 15)") 
    
    # answer: yes, inverse power curve. Schools below the curve are more focused,
    # schools above the curve are 
    model <- lm(y ~ x + I(x^(1/6)))
    summary(model)
    myPredict <- predict(model) 
    myIndex <- order(x)
    lines(x[myIndex], myPredict[myIndex],
          col=2, lwd=2)
    coeff <- round(model$coefficients , 2)
    text(70, 6, paste0("Fit curve: ", coeff[1], " + ", coeff[2], "x^(1/6) \n",
                      "Adjusted R-squared: = ",round(summary(model)$adj.r.squared,2)))
    
    if (remake_figs) {
        dev.off()
    }
    
    
    
    # Add shading for the scatter plot above (Methods ~ N) based on school position in
    # the major clusters of the diana heatmap 
    
    mydend <- as.hclust(kp_order$diana$Rowv)
    plot(mydend)
    rect.hclust(mydend, height=)
    school_clusters <- cutree(mydend, k=7)    
    names(school_clusters) <- sapply(names(school_clusters), function (d) {
        strsplit(d, " [(]")[[1]][1]
    })
    school_clusters
    # NEXT: grep for Chapel Hill, use that clusterlabel (2) also for the grep results for 
    # San Antonio (7), East Carolina (3) 
    # [DONE]
    
    
    plot(x, y, pch=16, bty="n", las=1, 
         col=c("gray", "gray", "gray", "gray", "black")[school_clusters],
         xlab = "Confirmed RCWS Dissertations at school, 2001-2015",
         ylab = "Methods represented (out of 15)") 
    lines(x[myIndex], myPredict[myIndex],
          col=2, lwd=2)
    
    # UPDATE: not actually interesting! All the major divisions are distributed
    # pretty equally to the left and right (equivalently: above and below) the line.
    # Huh. So... maybe all I'm really showing here is just a fantasy, and school size
    # is the main determining factor? But no -- *which* methods appear still varies.
    # And that's still important.
    
    #####
    # What happens if we try the heatmap with tagnames.simple?
    
    dataset_name <- "knownprograms2001_2015"
    tagset_name <- "tagnames.simple"
    
    if(!exists("sumbytags", mode="function")) {
        source(file = "method collocation heatmap.R")
    }
    
    method_corrs_simple <- sumbytags(dataset_name, tagset_name, 
                                     doplot=T, 
                                     normed=T, 
                                     dendro=T)
    
    colorscheme <- list(name = "magma", values = c("#FFFFFF", magma(99, dir=-1)))
    clustfun <- "diana"
    
    remake_figs <- T
    
    heatmap_simple <- schoolwise(dataset_name = "knownprograms2001_2015", 
               tagset_name = "tagnames.simple",
               show.totals = T,
               measure = "normed",
               myclustfun = clustfun,
               mycolorder = method_corrs_simple$Colv,
               myCol = if(is.null(colorscheme$values)) colorscheme[[i]]$values else colorscheme$values,
               filename_suffix = if(is.null(colorscheme$name)) colorscheme[[i]]$name else colorscheme$name,
               min_disses = 5)
    
    remake_figs <- F
    
    heatmap_simple_counts <- schoolwise(dataset_name = "knownprograms2001_2015", 
                                        tagset_name = "tagnames.simple",
                                        show.totals = T,
                                        measure = "counts",
                                        myclustfun = clustfun,
                                        mycolorder = method_corrs_simple$Colv,
                                        myroworder = heatmap_simple$diana$Rowv,
                                        myCol = if(is.null(colorscheme$values)) colorscheme[[i]]$values else colorscheme$values,
                                        filename_suffix = if(is.null(colorscheme$name)) colorscheme[[i]]$name else colorscheme$name,
                                        min_disses = 5)
    
    school_clusters_simple <- as.hclust(heatmap_simple$diana$Rowv)
    plot(school_clusters_simple)
    
    # Huh. So this is interesting: the methodology most likely to be left out isn't 
    # Aggregable, but Enactment-based: the practitioner studies are more concentrated
    # than I'd have expected. No school has zero Dialectical; 5 have no Phenomenological;
    # 3 schools have no Aggregable studies; and 13 of 85 schools have zero dissertations
    # that include any Enactment-based method. (And many others have low values.)
    
    
    rect.hclust(school_clusters_simple, k=13)
        
    ######
    # What methods are most and least distributed across schools?
    remake_figs = T
    method_spread_across_schools()
    method_spread_across_schools(min_disses = 5)
    method_spread_across_schools(spread_threshold = 2)
    method_spread_across_schools(spread_threshold = 3)
    remake_figs = F
    
    
    # is that the same as overall distribution?
    # interestingly not! Surv and Meta are higher than would be expected
    
    
    kp2001_2015_thresh5 <- thresh("knownprograms2001_2015", tagset_name = "no_ped_tagnames",
           threshold = 5, since = 2001, until = 2015)$thresh.data
    school_corrs_thresh5 <- schoolwise.data(dataset_name = "kp2001_2015_thresh5",
                                    tagset_name = "no_ped_tagnames")
    sort(school_corrs_thresh5$schoolspread, decreasing = T)
    
    
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
    message("    schoolwise.data()    # helper function to load data: methods per school")
    message("    schoolwise()         # function to graph data as heatplot")
    message("    method_spread_across_schools()  # barplot of columns for schoolwise's heatplot")
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
