############################################################################## methodcount barplot.R
#
# Produces a bar plot of method-tag counts per dissertation
#####

methods.barplot <- function(dataset_name="noexcludes",
                            tagset_name="tagnames") 
{
    if(tagset_name=="tagnames" || tagset_name=="tagnames.long") {
        data <- get(dataset_name)$Method.Count
    } else if(tagset_name=="tagnames.simple") {
        data <- get(dataset_name)$Counts.simple
    } else {
        tryCatch(expr = {
                dataset <- get(dataset_name)
                mycols <- dataset[, get(tagset_name)]
                data <- apply(mycols, MARGIN=1, FUN=sum)
                message("Method counts recalculated for tagset `",
                        tagset_name, "`")
            },
            error = function(e) {e;
                stop("Error in methods.barplot(): I can't parse tagset '", tagset_name, "'.")}
         )
        
    }
    
    data.t <- table(data)
    rows <- length(data)
    
    if (remake_figs) { 
        filename <- paste0(imageloc, "method count barplot, ", 
                            dataset_name, " (N ", rows, ").pdf")    
        pdf(file = filename) 
    }
    
    # basic barplot
    barplot(data.t, 
            main = "Most Dissertations use Multiple Methods",
            sub = dataset_name,
            xlab = "Method Tags Assigned",
            ylab = "Dissertations",
            las = 1     # labels always horizontal
        )
    
    # add labels inside tall bars but above short bars
    for(i in 1:length(data.t)) {
        if(data.t[[i]] > 25) {
            text(1.2*i-0.5, data.t[[i]] - 20, data.t[[i]])      
        } else {
            text(1.2*i-0.5, data.t[[i]] + 20, data.t[[i]])          
        }
    }
    
    data.mean <- mean(data)
    data.sd <- sd(data)
    data.median <- median(data)

    # add some stats
    mtext(side = 4, 
        las = 1, 
        adj = 1, 
        text = paste("mean =", round(data.mean,2), "\n", 
                     "sd =",round(data.sd,2), "\n", 
                     "N =", rows)
        )

    if (remake_figs) {dev.off()}
    
    return(list("table" = data.t,
                "pcts" = round((data.t / rows) * 100, 2),
                "mean" = data.mean,
                "median" = data.median,
                "sd" = data.sd,
                "N" = rows))
}

if(!autorun) {
    message("Function loaded: methods.barplot(dataset_name, tagset_name)")
}

if (FALSE) {
    remake_figs
    methods.barplot("knownprograms2001_2015", "no_ped_tagnames")
    methods.barplot("noexcludes")
    methods.barplot("noexcludes2001_2015")
    methods.barplot("consorts")
    methods.barplot("realconsorts")
    methods.barplot("nonconsorts")
#   methods.barplot("top.nonconsorts", "blah")
    methods.barplot("consorts.plus")
} 
