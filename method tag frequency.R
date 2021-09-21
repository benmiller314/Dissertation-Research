############################################################################## frequency of method tags.R
#
# Strategy: 
# 1. given a dataset and tagset, use apply() to sum the tag columns
# 2. sort and plot as horizontal bars
# 3. combine subset and superset for a comparative figure
#####


# Step 0. Make sure we're all set up.
if(!exists("thresh", mode="function")) {source(file="start here.R")}
source(file="compare method ranks.R")

# Step 1. Sum the tag columns
if(!exists("get_tags", mode="function")) { source(file="get tags.R") }

if(FALSE) {      # test values
	a <- get_tags("noexcludes")
	b <- get_tags("consorts")
	d <- get_tags("nonconsorts")
	e <- thresh("nonconsorts")$thresh.data
	e <- get_tags(dataset_name = "e")
	e[order(e)]
}


# Step 2. Graph 'em
methodfreq_combined <- function(bigset="noexcludes", 
								smallset="consorts", 
								label_diffs=T,
								diffset="nonconsorts",
								tagset="tagnames") 
{

	main <- "Frequency of Assigned Method Tags"
	a <- get_tags(bigset, tagset)
	b <- get_tags(smallset, tagset)
	if(label_diffs) {
	    d <- get_tags(diffset, tagset)
	}
	
	if (remake_figs) { 
		filename <- file.path(imageloc, paste0(main, ", ", smallset," within ", bigset,
							 ".pdf"))
		pdf(file=filename) 
	}
		
	# plot largest set as a baseline
	barplot(a[order(a)], 
			horiz=TRUE, 
			xpd=FALSE, 
			las=1, 
			axes=FALSE, 
			main=main, 
			col="gray80"
	)
	text(x=a[order(a)] + 30, 
		 y=seq(from=0.7, to=18.7, length.out=16), 
		 labels=a[order(a)]
	)
	
	# plot subset as an overlay, using the same order as the large set
	barplot(b[order(a)], 
			las=1, 
			horiz=TRUE, 
			xpd=FALSE, 
			axes=FALSE, 
			col="white", 
			add=TRUE
	)
	
	# only add subset labels if there really is a subset
	if (bigset != smallset) {
    	text(x=30, 
    		 y=seq(from=0.7, to=18.7, length.out=16), 
    		 labels=b[order(a)]
    	)
	}
	
	if(label_diffs) {	
		# no need to actively plot the diffset: that's the gap where the
		# baseline shows through. just add labels.

		text(x=a[order(a)] - d[order(a)] + 30,
			 y=seq(from=0.7, to=18.7, length.out=16), 
			 labels=d[order(a)]
		)
	    
	    legend(x="bottomright", 
	           c(smallset, diffset), 
	           fill=c("white","gray80"), 
	           bty="n"
	    )
	} else {
	    legend(x="bottomright", 
	           smallset, 
	           fill="white", 
	           bty="n"
	    )
	}
	
		mtext(paste("Tags are non-exclusive, so sum will be greater than",
					"the", nrow(get(bigset)), "included dissertations.", 
			  side=1)
		)
	
		
	if (remake_figs) { dev.off() }
}

if(autorun) {
	remake_figs
	methodfreq_combined() 
	methodfreq_combined(bigset="consorts.plus", 
						smallset="consorts", 
						diffset="top.nonconsorts") 
	if(exists("new_noexcludes")) {
	    depts_determined <- new_noexcludes[-which(new_excludes$Department==""),]
	    rhetmap_programs <- realrhetmaps[which(realrhetmaps$Pub.number %in% depts_determined$Pub.number),]
	    nrow(rhetmap_programs)
	    nrow(realrhetmaps)
	    methodfreq_combined(bigset="depts_determined", smallset="rhetmap_programs", label_diffs=F)
	    methodfreq_combined("new_noexcludes", "new_noexcludes", label_diffs = F)
	}

	methodfreq_combined("noexcludes2001_2005", "noexcludes2001_2005", label_diffs = F)
	methodfreq_combined("noexcludes2006_2010", "noexcludes2006_2010", label_diffs = F)
	methodfreq_combined("noexcludes2011_2015", "noexcludes2011_2015", label_diffs = F)
	
	methodfreq_combined("rhetmaps2001_2005", "rhetmaps2001_2005", label_diffs = F, tagset = "tagnames.simple")
	methodfreq_combined("rhetmaps2006_2010", "rhetmaps2006_2010", label_diffs = F, tagset = "tagnames.simple")
	methodfreq_combined("rhetmaps2011_2015", "rhetmaps2011_2015", label_diffs = F, tagset = "tagnames.simple")
	
} else {
    message("The following function has been loaded:")
    message("    methodfreq_combined(bigset, smallset, label_diffs, diffset, tagset)    ")
}
