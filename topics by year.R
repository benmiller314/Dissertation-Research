library(data.table)

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

# see http://www.statmethods.net/graphs/line.html

df <- as.data.frame(topic.year.avg)
lapply(1:5, FUN=function(y) { plot(df$Year, df[,as.character(y)]) })
length(topic.year.avg$Year)
length(as.data.frame(topic.year.avg)[,as.character(y)])