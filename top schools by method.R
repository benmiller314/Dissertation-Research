#############################################################################
# top schools by method.R
#
# GOAL: For each method in a given tagset, produce a list of the top X
# schools by either  methodological output (number of dissertations using
# that method at that school) or methodological focus (percentage of
# dissertations using that method at that school)
#####

# load required packages
require(doBy)


# open wrapper function
toplists <- function(dataset_name = "noexcludes", 
                     tagset_name = "tagnames", 
                     
                     # How many schools in the list for each method?
                     howmany = 5,   
                     
                     # Set a minimum number of dissertations per school...
                     threshold = 5,                 
                     
                     # ... over a specified span of years. (gets passed to
                     # thresh() from `thresh.R`
                     since = 2006,          
                     until = 2010,          
                     
                     # Use methodological focus (T) or raw output (F)?
                     rank_by_pcts = TRUE,
                     
                     # Display focus and output in one column (T) or two (F)?
                     combine = TRUE)            
{
    
    ## 0. convert variable names to variables. we'll use the names later in
    ## the figure titles.
    dataset <- get(dataset_name)
    tagset <- get(tagset_name)


    ## 1. find schools with more than (by default) 5 dissertations in
    ## 2006-2010 
    if(!exists("thresh", mode="function")) { 
        source(file="thresh.R") 
    }
    
    d <- thresh(dataset_name, tagset_name, threshold, since, until)
    d1 <- d$thresh.data
    subtitle1 <- d$thresh.report

    if (!exists("schoolwise.data")) { 
        source(file="tags by school.R") 
    }
    a <- schoolwise.data("d1", tagset_name)

    # 2 Star the schools in the consortium
    c <- which(a$counts$School %in% consorts$School)
    a$counts$School <- fix_factor(a$counts$School, 
                                  to.add = paste0(a$counts$School[c], "*"),
                                  to.remove = a$counts$School[c])

    ## 3. for the schools that meet the cutoff, find the "howmany" highest
    ## real values of each tag

    # 3a. Create function to apply to each tag in the tagset
    toplist.onetag <- function(a, tag, rank_by) {   
        tag.mean <- paste0(tag, ".mean")
        tag.sum <- paste0(tag, ".sum")
        
        # rank by chosen tag
        if(rank_by) {
            a1 <- order(a$means[, tag.mean], decreasing=TRUE)
        } else {
            a1 <- order(a$sums[, tag.sum], decreasing=TRUE)
        }
        
        # raw number of disses at top schools
        a2 <- head(a$counts[a1,], howmany)  
        
        # ignore columns from other tags, if they've snuck in
        a2 <- a2[,c(grep("School", names(a2)), grep(tag, names(a2)))]
        
        # number of disses with chosen tag
        a3 <- head(a$sums[a1,tag.sum], howmany) 
        
        # pct of disses with chosen tag         
        a4 <- head(a$means[a1, tag.mean], howmany)
        
        # cleaner percentage
        a4 <- round(100*a4, 0)                              

        if(combine) {
            # combine per-tag count and pct                 
            if(rank_by) {
                a5 <- paste0(a4, "% (", a3,")")
            } else {
                a5 <- paste0(a3, " (", a4, "%)")
            }
        
            # combine raw number with per-tag data
            a6 <- cbind(a2, tag = a5)
            
            # get cleaner column names
            names(a6) <- c("School", "Total", tag)              
    
            # combine School and Total, then remove Total
            a6$School <- fix_factor(a6$School, 
                            to.add=paste0(a6$School, " (", a6$Total, ")"),
                            to.remove=a6$School)                        
            a6$Total <- NULL
        } else {
            # leave per-tag count, per-tag pct, total count, and school as
            # separate columns
            a5 <- cbind(a2, "P"=a4, "D"=a3, "T"=a2$School.length)
            if(rank_by) {
                a6 <- a5[, c("School", "P", "D", "T")]  
                filename <- paste0(imageloc, "Top ", howmany, 
                                    " Schools by Methodological Focus ",
                                    "(Ranked by Percentage), ",
                                    dataset_name, ", ", tagset_name, ".csv")
            } else {
                a6 <- a5[, c("School", "T", "D", "P")]  
                filename <- paste0(imageloc, "Top ", howmany, 
                                    " Schools by Methodological Focus ",
                                    "(Ranked by Number of Dissertations), ", 
                                    dataset_name, ", ", tagset_name, ".csv")
            }
                
            # export as tab-delimited
            # TO DO: check if the file exists, prompt to overwrite or abort
            if(remake_figs) {
                # label for file
                names(a6)[1] <- realtags(tag, tagset_name)      
                write(t(names(a6)), ncolumns=4, filename, sep=",",
                     append=TRUE)
                write(t(a6), ncolumns=4, filename, sep=",", append=TRUE)
                write("", ncolumns=4, filename, sep=",", append=TRUE)   
                names(a6)[1] <- "School"    # label for screen
            }   
        }
        
        return(a6)  
    }   # end of toplist.onetag()
    
    # 3b. Apply the function to each tag in the tagset
    b <- lapply(tagset, FUN=function(x) {
                    toplist.onetag(a=a, tag=x, rank_by=rank_by_pcts)
                })

    if(rank_by_pcts) {
        title <- paste0("Top ", howmany, 
                " Schools by Methodological Focus (Ranked by Percentage)")
    } else {
        title <- paste0("Top ", howmany, 
                " Schools by Methodological Output ",
                "(Ranked by Number of Dissertations)")
    }
    subtitle2 <- paste("* indicates member of the Consortium of Doctoral",
                        "Programs in Rhetoric and Composition")
    names(b) <- tagset
    writeLines(c(title, subtitle1, subtitle2))
    print(b)

    
# close wrapper function toplists()
}

# call function
if(autorun) {
    remake_figs
    toplists(rank_by_pcts=T, combine=F)
    toplists("realconsorts", rank_by_pcts=F)
}
# TO DO: Write the output to a file for easier porting to Word, Scrivener,
# etc.


# # clean up working variables (only needed during testing)
# rm(d1, d2, d3, d4, a, a1, a2, a3, a4, a5, a6, b, c)
