require(maps)
require(mapdata)
require(mapplots)
require(maptools)
require(scales)

# Retrieve variables from other map-related scripts
source(file="carnegie 1 (setup).R")


if(!exists("maptags1", mode="function")) {	
	source(file="map by school 1 (setup).R") 
}
schools.geo <- maptags1("noexcludes")
consorts.geo <- maptags1("consorts")
head(cdoc2010.geo)
head(consorts.geo)

bins <- cut(schools.geo$DissCount, c(1,2,5,10,50,100, 1000), right=FALSE)
# fix weirdness in schools reported as having doctoral programs but no doctorates awarded
	disses.all.fields <- cdoc2010$PROF_D + cdoc2010$SOC_D + cdoc2010$STEM_D + cdoc2010$HUM_D
	cc.doctotal <- cdoc2010$DOCTOT
	argh <- which(disses.all.fields < cc.doctotal)
	data.frame("Sum of disses by field"=disses.all.fields[argh], "Reported diss total"=cc.doctotal[argh])
	disses.all.fields[argh] <- cc.doctotal[argh]
	
	argh <- which(disses.all.fields == 0)
	cdoc2010 <- cdoc2010[-argh,]
	cdoc2010.geo <- cdoc2010.geo[-argh]
	disses.all.fields <- disses.all.fields[-argh]
	rm(argh)

cc.bins <- cut(disses.all.fields, c(1,2,5,10,50,100, 1000), right=FALSE)

grays <- gray(length(levels(bins)):0 / length(levels(bins)))

# set up color ramp for schools.geo (comp/rhet data)
realcolors <- c()
for (i in 1:length(bins)) {
	bin.index <- which(levels(bins)==bins[i])
	realcolors <- c(realcolors, grays[bin.index])
}

# set up color ramp for cdoc2010 (schools in carnegie doctoral classes)
cc.realcolors <- c()
for (i in 1:length(cc.bins)) {
	bin.index <- which(levels(cc.bins) == cc.bins[i])
	cc.realcolors <- c(cc.realcolors, grays[bin.index])
}


# First graph: superimposing all 10 years of C/R data onto 2010 Carnegie schools
if(remake_figs) { 
	filename <- paste0(imageloc, "comp-rhet schools superimposed on carnegie2010 doctoral schools.pdf")
	pdf(file=filename)
}

par(mfrow = c(1,1))

# # map("worldHires",c("usa","Canada","Mexico"), xlim=c(-135,-53), ylim=c(23,58), col="gray40", fill=FALSE)
# map("state", boundary = FALSE, col="gray70",add=TRUE)
# points(x=cdoc2010.geo$Lng, y=cdoc2010.geo$Lat, col="gray10", pch=24, bg=cc.realcolors)  # CC2010 as triangles
# points(x=schools.geo$Lng, y=schools.geo$Lat, col="gray10", pch=21, bg=realcolors) 		# my data as circles
# legend(x="bottomright", title="Dissertations \n per school", legend=c("[1,2)", "[2,5)", "[5,10)", "[10,50)", "[50,100)"), fill=grays, bty="n")
# legend(x="bottomleft", legend=c("Carnegie","Comp/Rhet"), pch=c(24,21), bty="n")
# title(main="Most doctoral progams in the US \n now have some comp/rhet dissertations", sub="Carnegie classifications based on IPGRAD2010 > 11")

	map("worldHires",c("usa","Canada"), xlim=c(-135,-53), ylim=c(23,58), col="gray40", fill=FALSE)
	map("worldHires",c("Mexico"), xlim=c(-135,-53), ylim=c(23,58), col="gray70", fill=FALSE, add=TRUE)
	map("state", boundary = FALSE, col="gray70", add=TRUE)
	# map("province", boundary = FALSE, col="gray70", add=TRUE)
	points(x=cdoc2010.geo$Lng, y=cdoc2010.geo$Lat, pch=24, col="gray10", bg=cc.realcolors) # CC2010 as upward-facing triangles
	points(x=consorts.geo$Lng, y=consorts.geo$Lat, pch=6, cex=1, col="black") # consortium as downward-facing triangles
	points(x=schools.geo$Lng,y=schools.geo$Lat,col="gray10",pch=21,bg=realcolors) # my data as circles
	legend(x="bottomright", title="Dissertations \n per school, \n2001-2010", legend=c("1", "2-4", "5-9", "10-49", "50-100", "100+"), fill=grays, bty="n")
	legend(x="bottomleft", legend=c("Doctoral programs","R/C dissertations", "Consortium of R/C"), pch=c(24,21,6), bg=alpha("white",0.3),box.lty="blank")
	title(main="Most doctoral progams in the US \n now have some rhet/comp dissertations", sub="List of doctoral programs from Carnegie classification, IPGRAD2010 > 11")


if(remake_figs) {
	dev.off()
}
