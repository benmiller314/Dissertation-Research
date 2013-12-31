## Goal: for each year, find the number of institutions and plot them as a line graph

if(!exists("noexcludes")) {
	source(file="dataprep.R")
}
attach(noexcludes)

# spy = schools per year; dpy = dissertations per year
spy <- dpy <- c()

for (i in 2001:2010) {
	s <- noexcludes[which(Year == i),]		# filter by year
	n <- factor(s$School)					# merge duplicate schools
	spy[i-2000] <- length(levels(n))		# store count of schools
	d <- nrow(s)							# count dissertations	
	dpy[i-2000] <- d						# store count of dissertations
	
	per.year <- data.frame(spy,dpy)
}


names(per.year) <- c("Schools","Dissertations")
row.names(per.year) <- c(2001:2010)		# label counts by year
per.year

main <- "Dissertations and Granting Institutions by Year"
filename <- paste0(imageloc, main,", N",diss.count,".pdf") 
pdf(filename)
	max.y <- round(max(per.year)/10) * 10
	plot(x=c(2001:2010), y=seq(from=0,to=max.y,length.out=10), type="n", main=main, ylab="Count", xlab="", bty="l")
	lines(x=c(2001:2010),y=per.year$Dissertations,col="blue",type="b",pch=16)
	lines(x=c(2001:2010),y=per.year$Schools,col="red",type="b",pch=17)
	legend("bottomright",legend=c("Dissertations","Schools"),pch=c(16,17),col=c("blue","red"),bty="n")
	rm(main,filename)
dev.off()
