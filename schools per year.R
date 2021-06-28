## Goal: for each year, find the number of institutions and their dissertation outputs; 
#  optionally plot these numbers as a line graph

peryear <- function(dataset_name	= "noexcludes", 
					do.plot			= TRUE,
					do.stats        = FALSE,
					leaveout        = NULL     # vector of schools to exclude from the analysis
					)
{
	dataset <- get(dataset_name)	

	if(!is.null(leaveout)) {
    	dataset <- dataset[-which(dataset$School %in% leaveout),]
	}
	
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
		mtext(sub, side = 3)

		# add lines for data
		lines(x=to.return$Year, y=to.return$Dissertations, col="blue", type="b", pch=16)
		lines(x=to.return$Year, y=to.return$Schools, col="red", type="b", pch=17)
		
		# add averages
		
		# add legend
		if (!exists ("outside_legend", mode="function")) {
		    source(file="outside_legend.R")
		}
		outside_legend("bottomright",legend=c("Dissertations","Schools"),pch=c(16,17),col=c("blue","red"),bty="n")

		if(do.stats) {
		    statreport <- paste("Dissertations per year:\n", 
		                        "mean:", round(mean(to.return$Dissertations), 2), "\n",
		                        "median:", median(to.return$Dissertations))
		    
		    if(do.plot) {
		        outside_legend("bottomleft", 
		                   legend = statreport,
		                   bty="n"
		        )         
		    }
		    
		    message(statreport)
		    
		}
		
		
		
		if(!is.null(leaveout)) {
		    mtext(paste("These schools have been excluded from the analysis:",
		                paste(leaveout, sep=", ")),
		          side = 1)
		}
		
		if(remake_figs) {	dev.off()	}
	}
	
	return(to.return)
}

if(autorun) {
	remake_figs
	peryear()
	peryear("consorts.plus")
	peryear("noexcludes2001_2015", do.plot=T)
	peryear("knownprograms2001_2015", 
	        do.plot=T, 
	        do.stats=T,
	        leaveout = c("Indiana University of Pennsylvania",
	                     "University of Arizona",
	                     "Purdue University-Main Campus"))
	peryear
} else {
    message("One function loaded by `schools per year.R`: \n",
            "    peryear(dataset_name, do.plot=T, leaveout=NULL)")
}
