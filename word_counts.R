# count words in a given column (abstracts, eg.) of a data.frame

wc <- function(f) {
    if(!typeof(f) == "character") {
        f <- as.character(f)
    }
    
    # split by spaces, count how many items that produces
    count <- length(unlist(strsplit(f, " ")))
    
    return(count)
}

wc_column <- function(dataset, column, do.plot=T, print.summary=T) {
    col <- dataset[[column]]
    
    counts <- sapply(col, wc)
    if (do.plot) { 
        boxplot(counts) 
    }
    if(print.summary) {
        print(summary(counts))    
    }
    
    return(counts)
}

# for dissertations with knownprograms (as of 2018-07-12), n 1359 years 1993-2015
# > wc_column(knownprograms, "Title")
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.0    10.0    12.0    12.5    15.0    37.0 
# 
# > wc_column(knownprograms, "ABSTRACT")
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 58.0   240.0   312.0   294.2   347.0   670.0 

# Does it change over time?
wc_timeplot <- function(dataset, column, rawavg=F, smoothavg=T) {
    my_title <- paste("Length of", tolower(column), "in", deparse(substitute(dataset)))
    
    if (remake_figs) {
        filename <- file.path(imageloc, paste0(my_title, ".pdf"))
        pdf(file=filename)
    }
    
    # plot.new()
    plot(dataset[["Year"]], wc_column(dataset, column, do.plot=F, print.summary=F),
         xlab="Year", ylab="Word count", bty="l")
    years <- sort(unique(dataset[["Year"]]))
    yearlyavgs <- c()
    
    for (my_year in years) {
        yearlyavg <- mean(wc_column(dataset[which(dataset[["Year"]] == my_year),], column, do.plot=F, print.summary=F))
        yearlyavgs <- c(yearlyavgs, yearlyavg)
    }
    
    legend_fill <- legend_text <- c()
    
    if(rawavg) {
        lines(years, yearlyavgs, col="#3300cc")
        legend_fill <- "#3300cc"
        legend_text <- "yearly average"
    }
    
    if(smoothavg) {
        lines(years, runmed(yearlyavgs, 3, "median"), col="#cc0033")
        legend_fill <- c(legend_fill, "#cc0033")
        legend_text <- c(legend_text, "average (smoothed by running median, bin=3)")
    }
    
    if (rawavg | smoothavg) {
        outside_legend("bottomleft", col=legend_fill, lty=1, legend=legend_text, cex=0.8, bty="n")
    }
    
    title(my_title)
    
    if(remake_figs) {
        dev.off()
    }

       
}

if(autorun) {
    remake_figs
    wc_column(knownprograms, "ABSTRACT")
    wc_timeplot(knownprograms, "ABSTRACT")
    wc_timeplot(justexcludes, "ABSTRACT")
    wc_column(knownprograms, "Title")
    wc_timeplot(knownprograms, "Title")
    wc_timeplot(justexcludes, "Title")
}
