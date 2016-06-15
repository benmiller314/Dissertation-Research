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
        stop("Error: I don't know tagset '", tagset_name, "'.")
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

    # add some stats
    mtext(side = 4, 
        las = 1, 
        adj = 1, 
        text = paste("mean =", round(data.mean,2), "\n", 
                     "sd =",round(data.sd,2), "\n", 
                     "N =", rows)
        )

    if (remake_figs) {dev.off()}
}

if (autorun) {
    remake_figs
    methods.barplot("noexcludes")
    methods.barplot("consorts")
    methods.barplot("nonconsorts")
#   methods.barplot("top.nonconsorts", "blah")
    methods.barplot("consorts.plus")
} 
