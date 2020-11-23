## Goal: for each year, find the number of institutions and their dissertation outputs; 
#  optionally plot these numbers as a line graph

peryear <- function(dataset_name	= "noexcludes", 
					do.plot			= TRUE
					)
{
	dataset <- get(dataset_name)	

	# Schools per year
	spy <- aggregate(dataset$School, by=list(dataset$Year), FUN=function(s) {
		length(levels(factor(s)))
	})
	
	# Dissertations per year
	dpy <- aggregate(dataset$Title, by=list(dataset$Year), FUN=length)

	to.return <- merge(spy, dpy, by="Group.1")
	names(to.return) <- c("Year", "Schools", "Dissertations")
	
	if(do.plot)	{
		main <- "Dissertations and Granting Institutions by Year"
		sub <- paste0(dataset_name, ", N", nrow(dataset))
	
		if(remake_figs) {
			filename <- file.path(imageloc, paste0(main,", ", dataset_name, ", N",diss.count,".pdf")) 
			pdf(filename)
		}
		
		# set plot limits
		plot(to.return$Year, seq(from=0, to=max(to.return$Dissertations), length.out=nrow(to.return)), bty="n", type="n", lab=c(nrow(to.return), 5, 7), xlab="Year", ylab="", main=main)
		mtext(sub)

		# add lines for data
		lines(x=to.return$Year, y=to.return$Dissertations, col="blue", type="b", pch=16)
		lines(x=to.return$Year, y=to.return$Schools, col="red", type="b", pch=17)
		
		# add legend
		if (!exists ("outside_legend", mode="function")) {
		    source(file="outside_legend.R")
		}
		outside_legend("bottomright",legend=c("Dissertations","Schools"),pch=c(16,17),col=c("blue","red"),bty="n")

		if(remake_figs) {	dev.off()	}
	}
	
	return(to.return)
}

if(autorun) {
	remake_figs
	peryear()
	peryear("consorts.plus")
	peryear("noexcludes2001_2015", do.plot=T)
}
