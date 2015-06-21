## topics by year.R
## GOAL: To graph the rising and falling contributions to the corpus of each (or specified) topic over time.


topics.by.year <- function(dataset_name = "consorts", 
							ntopics		= 55,
							to.plot		= NULL,		# any pre-set topics to plot?
							do.plot		= TRUE,		# should we draw it, or just return the dataframe?
							per.plot	= 5			# how many lines per plot, at maximum?
							)
{
require(data.table)
require(RColorBrewer)

	# Get topic weights for every document we have
	if(!exists("get.doctopic.grid", mode="function")) { source("get doctopic grid.R") }
	grid <- data.table(get.doctopic.grid()$outputfile)
	
	# Get ready to merge
	grid$Pub.number <- as.factor(grid$Pub.number)
	setkey(grid, Pub.number)
	
	# Merge with noexcludes to add Year data to the topic data
	grid <- merge(grid, noexcludes.dt[, list(Pub.number, Year)], all.x=T)
	
	
	# Re-key by year
	setkey(grid, Year)
	
	# Get some stats for topics within each year
	topic.year.avg <- grid[, lapply(.SD, mean), by=Year]
	topic.year.avg <- topic.year.avg[, !names(topic.year.avg) %in% "Pub.number", with=F]
	# str(topic.year.avg)		# It's a data.table
	
	df <- as.data.frame(topic.year.avg)
	

	# Get topic labels, which you've composed elsewhere using 'top docs per topic.R', to use in figure legends
	if(!exists("get_topic_labels", mode="function")) { source(file="get topic labels.R") }
	topic.labels.dt <- get_topic_labels("consorts", 55)
	head(topic.labels.dt)
	
	# Exclude non-content-bearing topics
	bad.topics <- c("4", "47", "22", "2", "24", "13", "50")
	topic.labels.dt <- topic.labels.dt[!(Topic %in% bad.topics)]
	setkey(topic.labels.dt, Rank)
	head(topic.labels.dt)
	


	# We'll use plot.me in the other function, so figure it out even if we're not actually plotting.
	if (!is.null(to.plot)) { 							# any pre-set topics to plot?
		plot.me <- to.plot 
	} else {
		# plot.me <- c(51, 26, 46, 27, 43)							# Which topics to plot
		# plot.me <- 2:ncol(df)-1									# gives 1:ntopics
		plot.me <- topic.labels.dt[order(Rank), Topic]				# gives all topics in order of rank
	}

	# Set graphing parameters; see http://www.statmethods.net/graphs/line.html.
	if(do.plot) {
		xrange <- range(df$Year)									# X axis will be years
		yrange <- c(0, max(df[, !names(df) %in% "Year"]))			# Y axis will be % of the corpus contributed by topic
		mycol <- brewer.pal(n=per.plot, name="Dark2")				# Use different colors for each plot
		# plotchar <- seq(18, 18+length(plot.me), 1)				# Use different symbols for each plot?
		plotchar <- rep(20, length(plot.me))						# Nah, use same symbols for each plot
		maintitle <- "Average Topic Proportions over Time"
		
		
		
		## Draw `per.plot` (by default, 5) lines on the same plot, then start a new plot and repeat.
		
		# start recording to file if desired
		if(remake_figs) {
			if(is.null(to.plot)) {
				filename <- paste0(imageloc, maintitle, ", Topics ranked ", i, "-", (i+per.plot-1), ".pdf")
			} else {
				filename <- paste0(imageloc, maintitle, ", Topics ", to.plot[i], "-", to.plot[i+per.plot-1], ".pdf")
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
					dev.off()		# close an open file connection from the previous loop, if there is one
					if(is.null(to.plot)) {
						filename <- paste0(imageloc, maintitle, ", Topics ranked ", i, "-", (i+legend.offset), ".pdf")
					} else {
						filename <- paste0(imageloc, maintitle, ", Topics ", to.plot[i], "-", to.plot[(i+legend.offset)], ".pdf")
					}
					pdf(filename)	# start writing a new file
				}
				# set up a blank plot in a standard size
				plot(df$Year, rep(yrange, length((df$Year))/2), type="n", xaxs="r", ylab="Portion of corpus (scaled to 1)", xlab="Year", bty="n", main=maintitle)
		
				# add a legend for up to five values
				if(i <= 10) { legendloc <- "bottomright" } else { legendloc <- "topright" }
				
				
				
				if(is.null(to.plot)) { 
					legend(legendloc, title=paste0("Topics, ranked ", i, "-",(i+legend.offset), " of ", nrow(topic.labels.dt)),
						legend=paste0(plot.me[seq(i, (i+legend.offset), 1)], ": ", 
						topic.labels.dt[Topic %in% plot.me[i:(i+legend.offset)], Label]), 
						fill=mycol[j:(j+legend.offset)], border=mycol[j:(j+legend.offset)], bty="n", cex=0.8)
				} else {
					legend(legendloc, title="Topics", legend=paste0(plot.me[seq(i, (i+legend.offset), 1)], ": ", 
						topic.labels.dt[Topic %in% plot.me[i:(i+legend.offset)], Label]), 
						fill=mycol[j:(j+legend.offset)], border=mycol[j:(j+legend.offset)], bty="n", cex=0.8)
				}
			}	# end new plot + legend
		
			# draw the line and loop back
			lines(x=df$Year, y=df[,as.character(plot.me[i])], type="l", pch=plotchar[j], col=mycol[j])
		}
		
		# now that we're done looping, close the final file connection
		if(remake_figs) {dev.off()}
	} # end if(do.plot)
	
		# draw the line and loop back
		lines(x=df$Year, y=df[,as.character(plot.me[i])], type="l", pch=plotchar[j], col=mycol[j])
	}
	
	# now that we're done looping, close the final file connection
	if(remake_figs) {dev.off()}
	
	invisible(df)
}

# ## Find year-to-year peak variation for each topic
# r1 <- apply(df[!names(df) %in% c("Year", bad.topics)], 2, FUN=function(x) {max(x)-min(x)} )
# r2 <- r1[order(r1, decreasing=T)]
# text(1:length(r2), r2, r2)

topic.variation <- function(dataset_name = "consorts", 
							ntopics = 55,
							to.plot = NULL,		# any pre-set topics to plot?
							notch	= FALSE
							) {
# okay, this is interesting
	df <- topics.by.year(dataset_name, ntopics, to.plot, do.plot=F)
	
	maintitle <- "Yearly Variation of Topic Proportions Generally Preserves Topic Rank"
	if(dataset_name == "consorts") {
		subtitle <- paste0("Consortium dissertations, N", nrow(grid), ", years 2001-2010") 
	} else { 
		subtitle <- paste0(dataset_name, " dissertations, N", nrow(grid), ", years 2001-2010") 
	}
	
	if(remake_figs) { filename <- paste0(imageloc, maintitle, ".pdf"); pdf(filename) }
		boxplot(df[!names(df) %in% "Year"][, plot.me], cex.axis=0.6, las=2, main=maintitle, xlab="Topic Number, Arranged by Overall Rank within Corpus", ylab="Portion of Corpus (scaled to 1)")
		mtext(subtitle)	
	if(remake_figs) {dev.off()}
}

if(autorun) {
	topics.by.year()
	topic.variation()
}
