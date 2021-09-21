# large goal: do method combinations predict realconsorts?
# small goal: how many different combinations (levels) of method classes actually happen? 

tag_levels <- function(dataset_name, tagset_name) {
    dataset <- get(dataset_name)
    tagset <- get(tagset_name)
    
    tagarray <- as.data.frame(dataset[tagset])
    rowcount <- nrow(tagarray)
    colcount <- ncol(tagarray)
    for (i in 1:rowcount) { 
        myval <- ""
        for (j in 1:colcount) {
            myval <- paste0(myval, tagarray[i,j])
        }
        tagarray[i, "combined"] <- myval
    }
    
    tagarray$combined <- factor(tagarray$combined)
    
    str(tagarray)
    invisible(list(array = tagarray,
                   levels = tagarray$combined))
}

if(FALSE) {     # test area; will not run on its own
    mydata <- noexcludes2001_2015 # make a copy, to be safe
    mydata$tag_levels <- tag_levels("mydata", "tagnames.simple")$levels

    fit1 <- lm(realconsort ~ tag_levels, mydata)
    summary(fit1)
    
    fit2 <- lm(realconsort ~ Aggreg + Phenom + Dialec + Techne, mydata)
    summary(fit2)
    
    library(rms)
    fit3 <- lrm(realconsort ~ Aggreg + Phenom + Dialec + Techne, mydata)
    summary(fit3)
}
