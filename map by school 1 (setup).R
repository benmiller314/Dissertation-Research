## Plan of Attack
#
# A. for each school, determine a latitude and longitude (solved by geocode.R)
# B. create a table with columns for school, lat, lng, each tagname sum, total disses (this file)
# C. use map() to define a map area
# D. use add.pie() to plot a point for each school, with colors set by RColorBrewer, z values set by tagname sums, and radius = sqrt of total disses


## Set requirements
require(doBy)

# check that we have all the source files we'll need
if(!exists("noexcludes")) {source(file="dataprep.R")}

# set temporary values for the wrapper function -- comment out when done
dataset_name <- "noexcludes"
tagset_name <- "tagnames"


## B. Build the Table

# Begin wrapper function
maptags1 <- function (dataset_name="noexcludes", tagset_name="tagnames") {

	# 0. convert parameters into useable values (we'll use the names later, for saving files)
	dataset <- get(dataset_name)
	tagset <- get(tagset_name)
	
	# 1. sum each method type for all schools.
	a1 <- summaryBy(.~School,data=dataset, FUN=sum)
	
	# move school names from first column into row names
	row.names(a1) <- a1$School
	a1 <- a1[,2:ncol(a1)]
	
	# limit output columns to those in the relevant tagset
	sumnames <- paste0(tagset, ".sum")
	a1 <- a1[,which(names(a1) %in% sumnames)]
	
	# save the output
	filename <- paste0(dataloc, tagset_name, " tagsums by school, ", dataset_name, ", N", nrow(dataset), ".csv")
	write.csv(a1, file=filename)
	
	
	# 2. count total dissertations for each school
	a2 <- summaryBy(Year~School, data=dataset,FUN=length)
	names(a2) <- c("School","DissCount")
	head(a2)
	filename <- paste0(dataloc, "disses by school, ", dataset_name, ", N", nrow(dataset), ".csv")
	write.csv(a2, file=filename)
	
	# 3. load file with school names and lat/lng data, created by geocode.R; 
	# NB: diss.count created by dataprep.R
	filename <- paste0(dataloc, "geocoding by school, N", diss.count, ".csv")
	all_schools <- read.csv(filename)
	
	# trim the first column, which is just the row number
	all_schools <- data.frame(all_schools[,2:ncol(all_schools)])
	
	# get clean column names
	names(all_schools) <- c("School","Lat","Lng","City","State")
	
	# 4. stitch together steps 1-3, inner join to eliminate schools left over from false positives.
	# this should give us a geocoded index of schools with columns for total disscount and for counts of each tag in the tagset.
	a4 <- merge(all_schools, a1, by="School")
	a4 <- merge(a4, a2, by="School")
	
	# 5. return the merged table, since that should be enough to make maps
	return(a4)
	
	
	## Old material: saves specific versions of the output table. Removed because too large.
	# noexcludes.geo <- merge(dataset, schools.geo,by="School")
	# consorts.geo.index <- which(schools.geo$School %in% conschools)
	# consorts.geo <- schools.geo[consorts.geo.index,]
	
	## Save this stitched-up file for stuff that's easier in Excel
	# filename <- paste0(dataloc, tagset_name, " tagsums and disscounts by school, N", nrow(dataset), ".csv")
	# write.csv(noexcludes.geo, file=filename)
	# head(noexcludes.geo[which(names(noexcludes.geo) %in% c("School", "DissCount", sumnames))])

# Close wrapper function maptags1
}


attach(schools.geo)


## Define a Map Region
require(maps)
require(mapdata)
require(mapplots)
require(maptools)
require(scales)

require(RColorBrewer)

## Define color scale for fill (comment out unused versions)
mycol <- alpha(brewer.pal(5,"YlOrRd"),0.5)					# color!
# mycol <- brewer.pal(5,"Spectral")[quantile(DissCount)]	# pre-scaled color!
# mycol <- alpha("gray20",0.5)								# greyscale!

bins <- cut(DissCount,c(1,2,5,10,50,100),right=FALSE)
disses.all.fields <- cdoc2010$PROF_D + cdoc2010$SOC_D + cdoc2010$STEM_D + cdoc2010$HUM_D
cc.bins <- cut(disses.all.fields,c(1,2,5,10,50,100),right=FALSE)

grays <- gray(length(levels(bins)):0 / length(levels(bins)))

realcolors <- c()
for (i in 1:length(bins)) {
	bin.index <- which(levels(bins)==bins[i])
	realcolors <- c(realcolors,grays[bin.index])
}

cc.realcolors <- c()
for (i in 1:length(cc.bins)) {
	bin.index <- which(levels(cc.bins) == cc.bins[i])
	cc.realcolors <- c(cc.realcolors,grays[bin.index])
}

par(mfrow=c(1,1))

# okay, basic map of the data, focusing on the US and Canada
filename <- paste0(imageloc, "all dissertations, US map, N",diss.count,"2.pdf")
pdf(file=filename)

map("worldHires",c("usa","Canada","Mexico"), xlim=c(-135,-53), ylim=c(23,58), col="gray90", fill=FALSE)
map("state", boundary = FALSE, col="gray",add=TRUE)
points(x=Lng,y=Lat,col="gray10",pch=21,bg=realcolors)
title("Locations of dissertations in Composition and/or Rhetoric")
legend(x="bottomright", title="Dissertations \n per school", legend=c("[1,2)", "[2,5)", "[5,10)", "[10,50)", "[50,100)"), pch=21, pt.bg=grays, bty="n", col="gray10")
dev.off()


# I'd like to see the same map-view, this time with all schools in the Carnegie database
map("worldHires",c("usa","Canada","Mexico"), xlim=c(-135,-53), ylim=c(23,58), fill=FALSE)
map("state", boundary = FALSE, col="gray",add=TRUE)
points(x=cdoc2010.geo$Lng, y=cdoc2010.geo$Lat, pch=24, col="gray10", bg=cc.realcolors) # CC2010 as triangles
title(main="Locations of doctoral programs in 2010", sub="based on Carnegie Classifications, IPGRAD2010 > 11")
legend(x="bottomright", title="Dissertations per school", legend=c("[1,2)", "[2,5)", "[5,10)", "[10,50)", "[50,100)"), pch=21, col="deepskyblue4", pt.bg=brewer.pal(5,"Blues"), bty="n")


# Once more, this time with just consortium schools
map("worldHires",c("usa","Canada","Mexico"), xlim=c(-135,-53), ylim=c(23,58), fill=FALSE)
map("state", boundary = FALSE, col="gray",add=TRUE)
points(x=consorts.geo$Lng, y=consorts.geo$Lat, pch=6, col="black") # Xs
title(main="Locations of Schools in the Consortium /n of Doctoral Programs in Rhetoric and Composition")


# superimposing!
filename <- paste0(imageloc, "comp-rhet schools (consorts and non) superimposed on carnegie2010 doctoral schools, N", nrow(disses.by.school), ".pdf")
pdf(file=filename)

	par(mfrow = c(1,1))

	map("worldHires",c("usa","Canada"), xlim=c(-135,-53), ylim=c(23,58), col="gray40", fill=FALSE)
	map("worldHires",c("Mexico"), xlim=c(-135,-53), ylim=c(23,58), col="gray70", fill=FALSE, add=TRUE)
	map("state", boundary = FALSE, col="gray70", add=TRUE)
	# map("province", boundary = FALSE, col="gray70", add=TRUE)
	points(x=cdoc2010.geo$Lng, y=cdoc2010.geo$Lat, pch=24, col="gray10", bg=cc.realcolors) # CC2010 as upward-facing triangles
	points(x=consorts.geo$Lng, y=consorts.geo$Lat, pch=6, cex=1, col="black") # consortium as downward-facing triangles
	points(x=schools.geo$Lng,y=schools.geo$Lat,col="gray10",pch=21,bg=realcolors) # my data as circles
	legend(x="bottomright", title="Dissertations \n per school, \n2001-2010", legend=c("1", "2-4", "5-9", "10-49", "50+"), fill=grays, bty="n")
	legend(x="bottomleft", legend=c("Doctoral programs","R/C dissertations", "Consortium of R/C"), pch=c(24,21,6), bg=alpha("white",0.3),box.lty="blank")
	title(main="Most doctoral progams in the US \n now have some rhet/comp dissertations", sub="List of doctoral programs from Carnegie classification, IPGRAD2010 > 11")

dev.off()




# now let's do one more zoomed out
filename <- paste0(imageloc, "all dissertations, world map, N", diss.count, ".pdf")
pdf(file=filename)
map("worldHires", col="gray70", fill=FALSE)
	# points(x=cdoc2010.geo$Lng, y=cdoc2010.geo$Lat, pch=24, cex=0.6, col="gray20", bg=cc.realcolors) # CC2010 as upward-facing triangles
	# points(x=consorts.geo$Lng, y=consorts.geo$Lat, pch=6, cex=0.6, col="gray20") # consortium as downward-facing triangles
	points(x=schools.geo$Lng, y=schools.geo$Lat, col="gray20", cex=0.6, pch=20, bg=realcolors) # my data as solid dots
	# legend(x="bottomleft", title="Dissertations \n per school", legend=c("[1,2)", "[2,5)", "[5,10)", "[10,50)", "[50,100)"), fill=grays, bty="n")
	# legend(x="bottomleft", legend=c("Doctoral programs","Rhet/Comp disses", "Consortium of R/C"), pch=c(24,21,6), bty="n")

dev.off()


## define groups
case <- which(Case.sum>0); casesum <- sum(Case.sum)
crit <- which(Crit.sum>0); critsum <- sum(Crit.sum)
cult <- which(Cult.sum>0); cultsum <- sum(Cult.sum)
disc <- which(Disc.sum>0); discsum <- sum(Disc.sum)
ethn <- which(Ethn.sum>0); ethnsum <- sum(Ethn.sum)
expt <- which(Expt.sum>0); exptsum <- sum(Expt.sum)
hist <- which(Hist.sum>0); histsum <- sum(Hist.sum)
intv <- which(Intv.sum>0); intvsum <- sum(Intv.sum)
meta <- which(Meta.sum>0); metasum <- sum(Meta.sum)
modl <- which(Modl.sum>0); modlsum <- sum(Modl.sum)
phil <- which(Phil.sum>0); philsum <- sum(Phil.sum)
poet <- which(Poet.sum>0); poetsum <- sum(Poet.sum)
pract <- which(Pract.sum>0); practsum <- sum(Pract.sum)
rhet <- which(Rhet.sum>0); rhetsum <- sum(Rhet.sum)
surv <- which(Surv.sum>0); survsum <- sum(Surv.sum)
othr <- which(Othr.sum>0); othrsum <- sum(Othr.sum)










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

