require(maps)
require(mapdata)
require(mapplots)
require(maptools)
require(scales)

# NB: schools.geo comes from "map by school 1 (setup).R";
# cdoc2010 comes from "carnegie 1 (setup).R"

if(!exists("schools.geo")) {
	source(file="map by school 1 (setup).R")
}

if(!exists("cdoc2010")) {
	source(file="carnegie 1 (setup).R")
}


bins <- cut(schools.geo$DissCount,c(1,2,5,10,50,100),right=FALSE)
disses.all.fields <- cdoc2010$PROF_D + cdoc2010$SOC_D + cdoc2010$STEM_D + cdoc2010$HUM_D
cc.bins <- cut(disses.all.fields,c(1,2,5,10,50,100),right=FALSE)

grays <- gray(length(levels(bins)):0 / length(levels(bins)))

# set up color ramp for schools.geo (comp/rhet data)
realcolors <- c()
for (i in 1:length(bins)) {
	bin.index <- which(levels(bins)==bins[i])
	realcolors <- c(realcolors,grays[bin.index])
}

# set up color ramp for cdoc2010 (schools in carnegie doctoral classes)
cc.realcolors <- c()
for (i in 1:length(cc.bins)) {
	bin.index <- which(levels(cc.bins) == cc.bins[i])
	cc.realcolors <- c(cc.realcolors,grays[bin.index])
}


# First graph: superimposing all 10 years of C/R data onto 2010 Carnegie schools
filename <- paste0(imageloc, "comp-rhet schools superimposed on carnegie2010 doctoral schools.pdf")
pdf(file=filename)

par(mfrow = c(1,1))

map("worldHires",c("usa","Canada","Mexico"), xlim=c(-135,-53), ylim=c(23,58), col="gray40", fill=FALSE)
map("state", boundary = FALSE, col="gray70",add=TRUE)
points(x=cdoc2010.geo$Lng, y=cdoc2010.geo$Lat, pch=24, col="gray10", bg=cc.realcolors) # CC2010 as triangles
points(x=schools.geo$Lng,y=schools.geo$Lat,col="gray10",pch=21,bg=realcolors) # my data as circles
legend(x="bottomright", title="Dissertations \n per school", legend=c("[1,2)", "[2,5)", "[5,10)", "[10,50)", "[50,100)"), fill=grays, bty="n")
legend(x="bottomleft", legend=c("Carnegie","Comp/Rhet"), pch=c(24,21), bty="n")
title(main="Most doctoral progams in the US \n now have some comp/rhet dissertations", sub="Carnegie classifications based on IPGRAD2010 > 11")

dev.off()