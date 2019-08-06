#############################################################################
# topics by year.R
#
# GOAL: To graph the rising and falling contributions to the corpus of each
# (or specified) topic over time.
#
# Provides two functions: 
#      * topics.by.year(dataset_name, ntopics, ..., per.plot): draws line
#        graphs showing the proportions of the full corpus accounted for by
#        each topic. Allows up to `per.plot` topics to be graphed in the same
#        figure.
#      * topic.variation(dataset_name, ntopics, ...): plots adjacent
#        box-and-whisker plots showing the range of year-to-year proportions
#        of the full corpus for each topic. In a sense, then, compresses all
#        the graphs produced by topics.by.year into a single graph, allowing
#        for easier comparison of the variability of topic contributions.
#####

topics.by.year <- function(dataset_name = "consorts", 
                        ntopics = 55,
                        subset_name = NULL,
                        iter_index = "",
                        to.plot = NULL,   # any pre-set topics to plot?
                        do.plot = TRUE,   # should we draw it, or just
                                          # return the dataframe?
                        per.plot = 5,     # maximum how many lines per plot?
                        bad.topics = NULL
                        )
{
require(data.table)
require(RColorBrewer)

    # Get topic weights for every document we have
    if(!exists("get.doctopic.grid", mode="function")) { 
        source("get doctopic grid.R") 
    }
    grid <- get.doctopic.grid(dataset_name=dataset_name, ntopics=ntopics, 
                              subset_name=subset_name, iter_index=iter_index)$outputfile.dt
    
    # Get ready to merge
    grid$Pub.number <- as.factor(grid$Pub.number)
    setkey(grid, Pub.number)
    
    # Merge with noexcludes to add Year data to the topic data
    grid <- merge(grid, noexcludes.dt[, list(Pub.number, Year)], all.x=T)
    
    
    # Re-key by year
    setkey(grid, Year)
    
    # Get some stats for topics within each year
    topic.year.avg <- grid[, lapply(.SD, mean), by=Year]
    topic.year.avg <- topic.year.avg[, !names(topic.year.avg) %in%
                                     "Pub.number", with=F]
    # str(topic.year.avg)       # It's a data.table
    
    df <- as.data.frame(topic.year.avg)
    

    # Get topic labels, which you've composed elsewhere using `top docs per
    # topic.R`, to use in figure legends
    if(!exists("get_topic_labels", mode="function")) { 
        source(file="get topic labels.R") 
    }
    topic.labels.dt <- get_topic_labels(dataset_name=dataset_name, ntopics=ntopics, 
                                        subset_name=subset_name, iter_index=iter_index)
    head(topic.labels.dt)
    
    # Exclude non-content-bearing topics
    if (is.null(bad.topics) && dataset_name=="consorts" && ntopics==55) {
        bad.topics <- c("4", "47", "22", "2", "24", "13", "50")
    }
    topic.labels.dt <- topic.labels.dt[!(Topic %in% bad.topics)]
    setkey(topic.labels.dt, Rank)
    head(topic.labels.dt)
    
    # We'll use plot.me in the other function, so figure it out even if we're
    # not actually plotting.
    if (!is.null(to.plot)) {            # any pre-set topics to plot?
        plot.me <- to.plot 
    } else {
        # plot.me <- c(51, 26, 46, 27, 43)  # A range of topics to plot
        # plot.me <- 2:ncol(df)-1           # gives 1:ntopics by topic number
        
        # gives all topics in order of rank
        plot.me <- topic.labels.dt[order(Rank), Topic]              
    }

    # Set graphing parameters; see
    # http://www.statmethods.net/graphs/line.html.
    if(do.plot) {
        # X axis will be years
        xrange <- range(df$Year)                    
        
        # Y axis will be % of the corpus contributed by topic
        yrange <- c(0, max(df[, !names(df) %in% "Year"]))           

        # Use different colors for each plot
        mycol <- brewer.pal(n=per.plot, name="Dark2")
        
        # Use different symbols for each plot?
        # plotchar <- seq(18, 18+length(plot.me), 1)                
        
        # Nah, use same symbols for each plot
        plotchar <- rep(20, length(plot.me))                        
        maintitle <- "Average Topic Proportions over Time"
        
        
        ## Draw `per.plot` (by default, 5) lines on the same plot, then start
        ## a new plot and repeat.
        
        # start recording to file if desired
        if(remake_figs) {
            if(is.null(to.plot)) {
                filename <- file.path(imageloc, paste0(maintitle, ", ", dataset_name, ntopics, subset_name, ", Topics ranked ",
                             i, "-", (i+per.plot-1), ".pdf"))
            } else {
                filename <- file.path(imageloc, paste0(maintitle, ", ", dataset_name, ntopics, subset_name, ", Topics ",
                             to.plot[i], "-", to.plot[i+per.plot-1], ".pdf"))
            }
            
            pdf(filename)
        }
        
        # set a target for when to repeat
        enough <- seq(1, length(plot.me), by=per.plot)
        
        for (i in 1:length(plot.me)) {
            # Get a per-plot index to rotate through colors
            j <- ((i-1) %% per.plot) + 1
            
            
            # After each set of `per.plot` topics, start a new plot
            if (i %in% enough) {
                
                # make sure we don't print extra nulls at the end
                if((length(plot.me) - i) > per.plot) {
                    legend.offset <- (per.plot-i) %% per.plot
                } else {
                    legend.offset <- length(plot.me) - i
                }
                
                if(remake_figs) {
                    # close an open file connection from the previous loop,
                    # if there is one
                    dev.off()       
                    
                    # update filename
                    if(is.null(to.plot)) {
                        filename <- file.path(imageloc, paste0(maintitle, ", ", dataset_name, ntopics, subset_name, 
                                           ", Topics ranked ", i, "-",
                                     (i+legend.offset), ".pdf"))
                    } else {
                        filename <- file.path(imageloc, paste0(maintitle, ", ", dataset_name, ntopics, subset_name, 
                                           ", Topics ", to.plot[i], "-",
                                     to.plot[(i+legend.offset)], ".pdf"))
                    }
                    
                    # start writing a new file
                    pdf(filename)   
                }
                
                # set up a blank plot in a standard size
                plot(x = df$Year, 
                     y = rep(yrange, length((df$Year))/2), 
                     type = "n", 
                     xaxs = "r", 
                     ylab = "Portion of corpus (scaled to 1)", 
                     xlab = "Year", 
                     bty = "n", 
                     main = maintitle)
        
                # add a legend for up to five values
                if(i <= 10) { 
                    legendloc <- "bottomright" 
                } else { 
                    legendloc <- "topright" 
                }
                
                if(is.null(to.plot)) { 
                    legend(legendloc, title = paste0("Topics, ranked ", i,
                             "-",(i+legend.offset), " of ",
                              nrow(topic.labels.dt)),
                        legend = paste0(plot.me[seq(i, (i+legend.offset), 
                            1)], ": ", topic.labels.dt[Topic %in%
                            plot.me[i:(i+legend.offset)], Label]), 
                        fill=mycol[j:(j+legend.offset)],
                            border=mycol[j:(j+legend.offset)], 
                        bty="n", 
                        cex=0.8
                    )   
                } else {
                    legend(legendloc, title="Topics",
                        legend = paste0(plot.me[seq(i, (i+legend.offset), 
                            1)], ": ", topic.labels.dt[Topic %in%
                            plot.me[i:(i+legend.offset)], Label]), 
                        fill = mycol[j:(j+legend.offset)], 
                        border = mycol[j:(j+legend.offset)], 
                        bty = "n", 
                        cex = 0.8
                    )
                }
            }   # end new plot + legend
        
            # draw the line and loop back
            lines(x = df$Year, 
                  y = df[,as.character(plot.me[i])], 
                  type = "l", 
                  pch = plotchar[j], 
                  col = mycol[j]
            )
        } # end of for loop
        
        # now that we're done looping, close the final file connection
        if(remake_figs) {dev.off()}
    } # end if(do.plot)
    
    invisible(list("df"=df, "rank.order"=plot.me))
    
} # end of wrapper function topics.by.year()

## Second function: find year-to-year peak variation for each topic
topic.variation <- function(dataset_name = "consorts", 
                            ntopics = 55,
                            subset_name = NULL,
                            to.plot = NULL,     # any pre-set topics to plot?
                            notch   = FALSE,
                            bad.topics = NULL,
                            use.labels = F
                            ) {
# okay, this is interesting
    df <- topics.by.year(dataset_name=dataset_name, ntopics=ntopics, 
                         subset_name=subset_name, iter_index=iter_index,
                         to.plot=to.plot, do.plot=FALSE)
    rank.order <- df$rank.order
    df <- df$df
    
    if(! is.null(bad.topics)) {
        df <- df[!names(df) %in% bad.topics]
        rank.order <- rank.order[!rank.order %in% bad.topics]
    }
    
    

    
    maintitle <- paste("Yearly Variation of Topic Proportions",
                        "Generally Preserves Topic Rank")
    if(dataset_name == "consorts" && is.null(subset_name)) {
        subtitle <- paste0("Consortium School dissertations, N", nrow(grid), 
                            ", years 2001-2010") 
    } else if(dataset_name == "consorts" && subset_name == "realconsorts") {
        subtitle <- paste0("Consortium Program dissertations, N", nrow(grid), 
                           ", years 2001-2010") 
    } else { 
        subtitle <- paste0(dataset_name, " dissertations, N", nrow(grid), 
                            ", years 2001-2010") 
    }
    
        
    # Get topic labels, which you've composed elsewhere using 'top docs per
    # topic.R', to use in figure legends
    if(!exists("get_topic_labels", mode="function")) { 
        source(file="get topic labels.R") 
    }
    topic.labels.dt <- get_topic_labels(dataset_name=dataset_name, ntopics=ntopics, 
                                        subset_name=subset_name, iter_index=iter_index)
    head(topic.labels.dt)
    
    # Exclude non-content-bearing topics
    if (is.null(bad.topics) && dataset_name=="consorts" && ntopics==55) {
        bad.topics <- c("4", "47", "22", "2", "24", "13", "50")
    }
    topic.labels.dt <- topic.labels.dt[!(Topic %in% bad.topics)]
    setkey(topic.labels.dt, Rank)
    head(topic.labels.dt)


    # draw the plot 
    if(remake_figs) { 
        filename <- file.path(imageloc, paste0(maintitle, ".pdf"))
        pdf(filename) 
    }
    
    boxplot(df[!names(df) %in% "Year"][, rank.order], 
        main = maintitle, 
        # xlab="Topic Number, Arranged by Overall Rank within Corpus",
        # cex.axis=0.6, las=2, 
        ylab = "Portion of Corpus (scaled to 1)",
        xaxt = "n",
        notch = notch
        )
    
    if(use.labels) {    
        axis(1, 
             at = seq_along(df[!names(df) %in% c("Year", bad.topics)][, rank.order]),
             labels = topic.labels.dt[Topic %in% rank.order, Label], 
             las = 2,
             ps = 6,
             lheight=0.5
        )
    } else {
        axis(1, 
             at = seq_along(df[!names(df) %in% c("Year", bad.topics)][, rank.order]),
             labels = topic.labels.dt[Topic %in% rank.order, Topic], 
             las = 2, 
             ps = 6,
             lheight=0.5
        )
    }
    
    mtext(subtitle, side=3) 
    # abline(v=(0.5+seq(from=5,to=length(plot.me), by=5)), lty="dotdash")
    
    if(remake_figs) {
        dev.off()
    }
}

if(autorun) {
    remake_figs
    topics.by.year(dataset_name=dataset_name, ntopics=ntopics, 
                   subset_name=subset_name, iter_index=iter_index,
                   bad.topics=bad.topics)
    topic.variation(dataset_name=dataset_name, ntopics=ntopics, 
                   subset_name=subset_name, iter_index=iter_index,
                   bad.topics=bad.topics, 
                   show.outliers=F,
                   use.labels=T)
    
    topic.variation(subset_name = "realconsorts")
}
