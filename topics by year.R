require(data.table)
require(RColorBrewer)

# Get topic weights for every document we have
if(!exists("get.doctopic.grid")) { source("top docs per topic.R") }
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
topic.year.avg

names(topic.year.avg)

# see http://www.statmethods.net/graphs/line.html

df <- as.data.frame(topic.year.avg)


# Set graphing parameters
plot.me <- c(51, 26, 46, 27, 43)							# Which topics to plot
# plot.me <- 2:ncol(df)-1											# gives 1:ntopics
xrange <- range(df$Year)									# X axis will be years
yrange <- c(0, max(df[, !names(df) %in% "Year"]))			# Y axis will be % of the corpus contributed by topic
# mycol <- brewer.pal(n=length(plot.me), name="Paired")		# Use different colors for each plot
# plotchar <- seq(18, 18+length(plot.me), 1)					# Use different symbols for each plot?
plotchar <- rep(20, length(plot.me))						# Nah, use same symbols for each plot

}
# par(mfrow=c(1,1))
plot(df$Year, rep(yrange, length((df$Year))/2), type="n", ylab="", main="Average Topic Proportions over Time")

if (length(plot.me) > 10) {
	enough <- seq(1, length(plot.me), by=5)
	for (i in 1:length(plot.me)) {
		if (i %in% enough) {
			plot(df$Year, rep(yrange, length((df$Year))/2), type="n", ylab="", bty="n")
			legend("bottom", title="Topics", legend=i:(i+4), col=mycol[i:(i+4)], bty="n", cex=0.2)
		}
		
		lines(x=df$Year, y=df[,as.character(plot.me[i])], type="l", pch=plotchar[i], col=mycol[1+i%%5])				
	}
} else {
	# Plot away
	for (i in 1:length(plot.me)) {
		lines(x=df$Year, y=df[,as.character(plot.me[i])], type="l", pch=plotchar[i], col=mycol[i])
	}
	legend("topright", legend=plot.me, bty="n", fill=mycol)
}

