## topics by year.R
## GOAL: To graph the rising and falling contributions to the corpus of each (or specified) topic over time.

# hard-code parameters during testing; later pass to wraparound function
dataset_name <- "consorts"
ntopics <- 55

require(data.table)
require(RColorBrewer)

# Get topic weights for every document we have
if(!exists("get.doctopic.grid")) { 
	if(exists("autorun")){ autorun.bk <- autorun; } 
	autorun <- F;
	source("top docs per topic.R"); 
	if(exists("autorun.bk")){ autorun <- autorun.bk; rm(autorun.bk) }
}

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
topic.year.avg$Pub.number <- NULL
# str(topic.year.avg)		# It's a data.table

df <- as.data.frame(topic.year.avg)


# get topic labels, which you've composed elsewhere using 'top docs per topic.R', to use in figure legends
filename <- paste0(imageloc, "topic labeling - ", dataset_name, ", K", ntopics, ".csv")
topic.labels.dt <- tryCatch(
	data.table(read.csv(filename), key="Topic"), 
	error = function(e) {
  		message("File not found; using top words instead.")
  		keys <- get.topickeys(dataset_name, ntopics)
  		outfile <- paste0(webloc, "/", dataset_name, "k", ntopics, "_clusters_topwords.json")	
  		return(data.table(Topic=1:ntopics, Label=keys$top_words))
  	},
  	finally = { message("done.") }
)
head(topic.labels.dt)

# Exclude non-content-bearing topics
bad.topics <- c("4", "47", "22", "2", "24")
topic.labels.dt <- topic.labels.dt[!(Topic %in% bad.topics)]
setkey(topic.labels.dt, Rank)
head(topic.labels.dt)

# Set graphing parameters; see http://www.statmethods.net/graphs/line.html
if (!is.null(to.plot)) { 							# any pre-set topics to plot?
	plot.me <- to.plot 
} else {
	# plot.me <- c(51, 26, 46, 27, 43)							# Which topics to plot
	# plot.me <- 2:ncol(df)-1									# gives 1:ntopics
	plot.me <- topic.labels.dt[order(Rank), Topic]				# gives all topics in order of rank
}
xrange <- range(df$Year)									# X axis will be years
yrange <- c(0, max(df[, !names(df) %in% "Year"]))			# Y axis will be % of the corpus contributed by topic
mycol <- brewer.pal(n=(5-length(plot.me) %% 5 + 1), name="Dark2")		# Use different colors for each plot
# plotchar <- seq(18, 18+length(plot.me), 1)					# Use different symbols for each plot?
plotchar <- rep(20, length(plot.me))						# Nah, use same symbols for each plot
maintitle <- "Average Topic Proportions over Time"
}


## Draw 5 lines on the same plot, then start a new plot and repeat.

# start recording to file if desired
if(remake_figs) {filename <- paste0(imageloc, maintitle, ", Topics ", i, "-", (i+4), ".pdf"); pdf(filename)}

enough <- seq(1, length(plot.me), by=5)

for (i in 1:length(plot.me)) {
	# Get a per-plot index to rotate through colors
	j <- ((i-1) %% 5) + 1
	
	# After each set of 5 topics, start a new plot
	if (i %in% enough) {
		if(remake_figs) {
			dev.off()		# cancel open file connection from the previous loop, if there is one
			filename <- paste0(imageloc, maintitle, ", Topics ranked ", i, "-", (i+4), ".pdf")
			pdf(filename)	# start writing a new file
		}
		# set up a blank plot in a standard size
		plot(df$Year, rep(yrange, length((df$Year))/2), type="n", xaxs="r", ylab="Portion of corpus (scaled to 1)", xlab="Year", bty="n", main=maintitle)

		# add a legend for up to five values
		if(i <= 10) { legendloc <- "bottomright" } else { legendloc <- "topright"}
		legend(legendloc, title=paste0("Topics, ranked ", i, "-",(i+4), " of ", nrow(topic.labels.dt)), legend=paste0(plot.me[seq(i, i+4, 1)], ": ", topic.labels.dt[i:(i+4), Label]), fill=mycol[j:(j+4)], border=mycol[j:(j+4)], bty="n", cex=0.8)
	}

	# draw the line and loop back
	lines(x=df$Year, y=df[,as.character(plot.me[i])], type="l", pch=plotchar[j], col=mycol[j])
}

# now that we're done looping, close the final file connection
if(remake_figs) {dev.off()}


## Find year-to-year peak variation for each topic
r1 <- apply(df[!names(df) %in% c("Year", bad.topics)], 2, FUN=function(x) {max(x)-min(x)} )
r2 <- r1[order(r1, decreasing=T)]
text(1:length(r2), r2, r2)

# okay, this is interesting
maintitle <- "Yearly Variation of Topic Proportions, 2001-2010"
if(remake_figs) {filename <- paste0(imageloc, maintitle, ".pdf"); pdf(filename)}
boxplot(df[!names(df) %in% "Year"][, plot.me], cex.axis=0.6, las=2, main=maintitle, xlab="Topic Number", ylab="Portion of Corpus (scaled to 1)")		
if(remake_figs) {dev.off()}
