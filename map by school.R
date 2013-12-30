
require(RColorBrewer)

## Plan of Attack

# for each school, determine a latitude and longitude (solved by geocode.R)
# create a table with columns for school, lat, lng, each tagname sum, total disses
# use map() to define a map area
# use add.pie() to plot a point for each school, with colors set by RColorBrewer, z values set by tagname sums, and radius = sqrt of total disses


## Build the Table

require(doBy)
# sum each method type for all schools. NB: noexcludes created by dataprep.R
tagsums.by.school <- summaryBy(Case+Crit+Cult+Disc+Ethn+Expt+Hist+Intv+Meta+Modl+Phil+Poet+Pract+Rhet+Surv~School,data=noexcludes, FUN=sum)

# count total dissertations for each school
disses.by.school <- summaryBy(Year~School, data=noexcludes,FUN=length)
names(disses.by.school) <- c("School","DissCount")

# load file with school names and lat/lng data, created by geocode.R; 
# NB: diss.count created by dataprep.R
filename <- paste("geocoding by school, N",diss.count,".csv",collapse="")
all_schools <- read.csv(filename)
all_schools <- data.frame(all_schools[,2:ncol(all_schools)])
names(all_schools) <- c("School","Lat","Lng")

# stitch 'em together, inner join to eliminate schools left over from false positives
schools.geo <- merge(all_schools,disses.by.school, by="School")
schools.geo <- merge(schools.geo,tagsums.by.school, by="School")


## Define a Map Region
require(maps)
require(mapdata)
require(mapplots)
require(maptools)
require(scales)

attach(schools.geo)

# okay, basic map of the data, focusing on the US and Canada
filename <- paste("all dissertations, US map, N",diss.count,".pdf",collapse="")
pdf(file=filename)

map("worldHires",c("usa","Canada","Mexico"), xlim=c(-135,-53), ylim=c(23,58), col="gray90", fill=TRUE)
points(x=Lng,y=Lat,cex=sqrt(DissCount/2),col="gray10",pch=21,bg=alpha("gray20",0.5))
leg.scale <- c(1,5,10,50,100)
legend(x="bottomright",legend="Dissertations per school",pch=21,bg=alpha("gray20",0.5), bty="n", col="gray10")
dev.off()

# now let's do one more zoomed out
filename <- paste("all dissertations, world map, N",diss.count,".pdf",collapse="")
pdf(file=filename)
map("worldHires", col="gray20", fill=FALSE)
points(x=Lng,y=Lat,pch=20,col=brewer.pal(9,"Spectral"))
dev.off()


## define groups
crit <- which(Crit.sum>0); critsum <- sum(Crit.sum)
cult <- which(Cult.sum>0); cultsum <- sum(Cult.sum)
disc <- which(Disc.sum>0); discsum <- sum(Disc.sum)
ethn <- which(Ethn.sum>0); ethnsum <- sum(Ethn.sum)
expt <- which(Expt.sum>0); exptsum <- sum(Expt.sum)
hist <- which(Hist.sum>0); histsum <- sum(Hist.sum)
modl <- which(Modl.sum>0); modlsum <- sum(Modl.sum)
phil <- which(Phil.sum>0); philsum <- sum(Phil.sum)
poet <- which(Poet.sum>0); poetsum <- sum(Poet.sum)
pract <- which(Pract.sum>0); practsum <- sum(Pract.sum)
rhet <- which(Rhet.sum>0); rhetsum <- sum(Rhet.sum)

## plot 'em
filename <- paste("US map, crit 2001-2010, US map, N",diss.count,".pdf",collapse="")
pdf(filename)
map("worldHires",c("usa","Canada","Mexico"), xlim=c(-135,-53), ylim=c(23,58), col="gray90", fill=TRUE)
points(x=Lng[crit],y=Lat[crit],col="gray10",pch=21,bg=alpha("gray20",0.5))
title("Critical/Hermeneutical sites", sub=paste(critsum,"dissertations, 2001-2010"))
dev.off()

filename <- paste("US map, cult 2001-2010, US map, N",diss.count,".pdf",collapse="")
pdf(filename)
map("worldHires",c("usa","Canada","Mexico"), xlim=c(-135,-53), ylim=c(23,58), col="gray90", fill=TRUE)
points(x=Lng[cult],y=Lat[cult],cex=sqrt(DissCount),col="gray10",pch=21,bg=alpha("gray20",0.5))
title("Cultural / Critical sites", sub=paste(cultsum,"dissertations, 2001-2010"))
dev.off()

filename <- paste("US map, disc 2001-2010, US map, N",diss.count,".pdf",collapse="")
pdf(filename)
map("worldHires",c("usa","Canada","Mexico"), xlim=c(-135,-53), ylim=c(23,58), col="gray90", fill=TRUE)
points(x=Lng[disc],y=Lat[disc],cex=sqrt(DissCount),col="gray10",pch=21,bg=alpha("gray20",0.5))
title("Discourse or Text Analysis sites", sub=paste(discsum,"dissertations, 2001-2010"))
dev.off()

filename <- paste("US map, ethn 2001-2010, US map, N",diss.count,".pdf",collapse="")
pdf(filename)
map("worldHires",c("usa","Canada","Mexico"), xlim=c(-135,-53), ylim=c(23,58), col="gray90", fill=TRUE)
points(x=Lng[ethn],y=Lat[ethn],cex=sqrt(DissCount),col="gray10",pch=21,bg=alpha("gray20",0.5))
title("Ethnographic sites", sub=paste(ethnsum,"dissertations, 2001-2010"))
dev.off()

filename <- paste("US map, expt 2001-2010, US map, N",diss.count,".pdf",collapse="")
pdf(filename)
map("worldHires",c("usa","Canada","Mexico"), xlim=c(-135,-53), ylim=c(23,58), col="gray90", fill=TRUE)
points(x=Lng[expt],y=Lat[expt],cex=sqrt(DissCount),col="gray10",pch=21,bg=alpha("gray20",0.5))
title("Experimental sites", sub=paste(exptsum,"dissertations, 2001-2010"))
dev.off()

filename <- paste("US map, hist 2001-2010, US map, N",diss.count,".pdf",collapse="")
pdf(filename)
map("worldHires",c("usa","Canada","Mexico"), xlim=c(-135,-53), ylim=c(23,58), col="gray90", fill=TRUE)
points(x=Lng[hist],y=Lat[hist],cex=sqrt(DissCount),col="gray10",pch=21,bg=alpha("gray20",0.5))
title("Historical / Archival sites", sub=paste(histsum,"dissertations, 2001-2010"))
dev.off()

filename <- paste("US map, modl 2001-2010, US map, N",diss.count,".pdf",collapse="")
pdf(filename)
map("worldHires",c("usa","Canada","Mexico"), xlim=c(-135,-53), ylim=c(23,58), col="gray90", fill=TRUE)
points(x=Lng[modl],y=Lat[modl],cex=sqrt(DissCount),col="gray10",pch=21,bg=alpha("gray20",0.5))
title("Model-Building sites", sub=paste(modlsum,"dissertations, 2001-2010"))
dev.off()

filename <- paste("US map, phil 2001-2010, US map, N",diss.count,".pdf",collapse="")
pdf(filename)
map("worldHires",c("usa","Canada","Mexico"), xlim=c(-135,-53), ylim=c(23,58), col="gray90", fill=TRUE)
points(x=Lng[phil],y=Lat[phil],cex=sqrt(DissCount),col="gray10",pch=21,bg=alpha("gray20",0.5))
title("Philosophical sites", sub=paste(philsum,"dissertations, 2001-2010"))
dev.off()

filename <- paste("US map, poet 2001-2010, US map, N",diss.count,".pdf",collapse="")
pdf(filename)
map("worldHires",c("usa","Canada","Mexico"), xlim=c(-135,-53), ylim=c(23,58), col="gray90", fill=TRUE)
points(x=Lng[poet],y=Lat[poet],cex=sqrt(DissCount),col="gray10",pch=21,bg=alpha("gray20",0.5))
title("Poetic / Fictive sites", sub=paste(poetsum,"dissertations, 2001-2010"))
dev.off()

filename <- paste("US map, pract 2001-2010, US map, N",diss.count,".pdf",collapse="")
pdf(filename)
map("worldHires",c("usa","Canada","Mexico"), xlim=c(-135,-53), ylim=c(23,58), col="gray90", fill=TRUE)
points(x=Lng[pract],y=Lat[pract],cex=sqrt(DissCount),col="gray10",pch=21,bg=alpha("gray20",0.5))
title("Practitioner / Teacher Research sites", sub=paste(practsum,"dissertations, 2001-2010"))
dev.off()

filename <- paste("US map, rhet 2001-2010, US map, N",diss.count,".pdf",collapse="")
pdf(filename)
map("worldHires",c("usa","Canada","Mexico"), xlim=c(-135,-53), ylim=c(23,58), col="gray90", fill=TRUE)
points(x=Lng[rhet],y=Lat[rhet],cex=sqrt(DissCount),col="gray10",pch=21,bg=alpha("gray20",0.5))
title("Rhetorical Analysis sites", sub=paste(rhetsum,"dissertations, 2001-2010"))
dev.off()




# require(ggplot2)

# # load us map data
# all_states <- map_data("state")

# # start a ggplot. it won't plot til we type p
# p <- ggplot()  

# # add U.S. states outlines to ggplot
# p <- p + geom_polygon(data=all_states, aes(x=long, y=lat, group = group),
     # colour="grey", fill="white" )
# p
# p + geom_point(data=subset(schools.geo,Lng>=-135 && Lng<=-53), aes(x=Lng,y=Lat,size=sqrt(DissCount),colour=alpha("gray20",0.5)))


# center <- geocode("united states")
# # map1 <- qmap(center, zoom = 3)
# # map1 + geom_point(aes(x=Lng,y=Lat, data=schools.geo, size=sqrt(DissCount), alpha=0.5))
# map1 <- qmplot(x=Lng,y=Lat, data=schools.geo, source="google",zoom=2,location="Kansas")
# map1 + aes(size=sqrt(DissCount),alpha=0.5,col=DissCount)

# map2 <- get_map(location=c(center$lon,center$lat), maptype="roadmap", color="bw", source="google", zoom=3)
# map2 <- ggmap(map2)
# map2
# map2 + data=schools.geo, aes(x=Lng,y=Lat)
# map2 + geom_point(size=sqrt(DissCount), alpha=0.5)

