############################################################################## method tag frequency.R
#
# Strategy:
# 1. given a dataset and tagset_name, use apply() to sum the tag columns
# 2. sort and plot as horizontal bars
# 3. combine subset and superset for a comparative figure
#####


# Step 0. Make sure we're all set up.
if(!exists("thresh", mode="function")) {source(file="start here.R")}
source(file="compare method ranks.R")

# Step 1. Sum the tag columns
if(!exists("get_tags", mode="function")) { source(file="get tags.R") }

if(FALSE) {     # test values
	a <- get_tags("noexcludes")
	b <- get_tags("consorts")
	d <- get_tags("nonconsorts")
	e <- thresh("nonconsorts")$thresh.data
	e <- get_tags(dataset_name = "e")
	e[order(e)]
}


# Step 2. Graph 'em
methodfreq_combined <- function(dataset_name = "knownprograms2001_2015",
                                tagset_name="tagnames",
                                subset_name = NULL,
								label_diffs = is.null(subset_name),
								diffset_name = NULL
){

	main <- "Frequency of Assigned Method Tags"
	a <- get_tags(dataset_name, tagset_name)

	if (!is.null(subset_name)) {
    	b <- get_tags(subset_name, tagset_name)

    	if(label_diffs) {
    	    d <- get_tags(diffset_name, tagset_name)
    	}

    	filename <- file.path(imageloc, paste0(main, ", ", subset_name," within ", dataset_name,
    	                                       ", ", tagset_name ,".pdf"))
	} else {
	    filename <- file.path(imageloc, paste0(main, ", ", dataset_name, ".pdf"))
	}

	if (remake_figs) {
		pdf(file=filename)
	}

	# plot largest set as a baseline
	pl <- barplot(a[order(a)],
			horiz=TRUE,
			xpd=FALSE,
			las=1,
			axes=FALSE,
			main=main,
			col="gray80"
	)
	text(x=a[order(a)] + 15,
		 y=pl,
		 labels=a[order(a)]
	)

	if(!is.null(subset_name)) {
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
    	if (dataset_name != subset_name) {
        	text(x=30,
        		 y=pl,
        		 labels=b[order(a)]
        	)
    	}


    	if(label_diffs) {
    		# no need to actively plot the diffset: that's the gap where the
    		# baseline shows through. just add labels.

    		text(x=a[order(a)] - d[order(a)] + 30,
    			 y=pl,
    			 labels=d[order(a)]
    		)

    	    legend(x="bottomright",
    	           c(subset_name, diffset_name),
    	           fill=c("white","gray80"),
    	           bty="n"
    	    )
    	} else {
    	    legend(x="bottomright",
    	           subset_name,
    	           fill="white",
    	           bty="n"
    	    )
    	}
	} # end of if(!is.null(subset_name))

		mtext(text = paste("Tags are non-exclusive, so sum will be greater than",
					"the", nrow(get(dataset_name)), "included dissertations."),
			  side = 1,
			  outer = F
		)


	if (remake_figs) { dev.off() }
}


# TO DO: Given a set of method tags, find the number of dissertations with 
#        at least one of those tags. (i.e. method-tag reach)

method_reach <- function(dataset_name = "knownprograms2001_2015",
                         tags) 
{
    require(data.table)
    
    dataset <- get(dataset_name)
    
    if(! is.data.table(dataset)) {
        dt <- data.table(dataset, key="Pub.number")
    }
    
    dt[, sum:=rowSums(.SD), .SDcols=tags]
    reached <- dt[sum > 0, ]
    dt[, sum:=NULL]
    
    return(list(dt = reached,
                reach = nrow(reached),
                pct = nrow(reached) / nrow(dataset),
                call = match.call())
           )
}

# Tests and runs worth revisiting
if(FALSE) {
	remake_figs
	methodfreq_combined()
	methodfreq_combined(dataset_name="consorts.plus",
						subset_name="consorts",
						diffset_name="top.nonconsorts")
	if(exists("new_noexcludes")) {
	    depts_determined <- new_noexcludes[-which(new_excludes$Department==""),]
	    rhetmap_programs <- realrhetmaps[which(realrhetmaps$Pub.number %in% depts_determined$Pub.number),]
	    nrow(rhetmap_programs)
	    nrow(realrhetmaps)
	    methodfreq_combined(dataset_name="depts_determined", subset_name="rhetmap_programs", label_diffs=F)
	    methodfreq_combined("new_noexcludes", "new_noexcludes", label_diffs = F)
	}

	methodfreq_combined(dataset_name="noexcludes2001_2015",
	                    subset_name="knownprograms2001_2015",
	                    diffset_name="nonconsorts2001_2015",
	                    tagset_name="tagnames")
	                    # tagset_name="tagnames.simple")

	methodfreq_combined(dataset_name = "knownprograms2001_2015",
	                    tagset_name = "no_ped_tagnames")

	methodfreq_combined("noexcludes2001_2005", "noexcludes2001_2005", label_diffs = F)
	methodfreq_combined("noexcludes2006_2010", "noexcludes2006_2010", label_diffs = F)
	methodfreq_combined("noexcludes2011_2015", "noexcludes2011_2015", label_diffs = F)

	methodfreq_combined("rhetmaps2001_2005", "rhetmaps2001_2005", label_diffs = F, tagset_name = "tagnames.simple")
	methodfreq_combined("rhetmaps2006_2010", "rhetmaps2006_2010", label_diffs = F, tagset_name = "tagnames.simple")
	methodfreq_combined("rhetmaps2011_2015", "rhetmaps2011_2015", label_diffs = F, tagset_name = "tagnames.simple")

	
	# Method Tag Group Reach
	dialec <- method_reach(tags = c("Phil", "Crit", "Hist", "Rhet", "Modl"))
	phenom <- method_reach(tags = c("Clin", "Ethn"))
	aggreg <- method_reach(tags = c("Disc", "Expt", "Surv", "Meta", "Intv"))
	perform <- method_reach(tags = c("Poet", "Prac"))
	
	method_reach_table <- rbind(c("dialec", dialec$reach, dialec$pct),
	                            c("phenom", phenom$reach, phenom$pct),
	                            c("aggreg", aggreg$reach, aggreg$pct),
	                            c("perform", perform$reach, perform$pct))
	colnames(method_reach_table) <- c("Method.grp", "Count", "Pct")
	print(method_reach_table)
	
} else {
    message("The following functions have been loaded: \n",
            "    methodfreq_combined(dataset_name, tagset_name, [subset_name], label_diffs, [diffset_name])\n",
            "    method_reach(dataset_name, tagset_name)"
    )
}
