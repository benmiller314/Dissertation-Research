########################
# compare method ranks.R
# GOAL: Find the difference in method frequency between two sets
#       by arranging method tags in two columns, and 
#		connecting matching methods with lines for ease of comparison.
#
#		For set1 and set2, use text strings naming variables, not the 
#		variables themselves, so we can use them to label the figure.
#####

compare_method_ranks <- function(set1="consorts",
		set2="nonconsorts",
		pcts=TRUE, 			# Label with percent of docs with that tag? 
							# If FALSE, use real counts. 
		colorful=FALSE,		# Use multiple colors to distinguish lines?
		betterlabels=NULL, 	# Optional vector of length 2, giving set labels.
		tagset_name="tagnames",   # Which tags to use?
		include_other = FALSE,
		verbose = FALSE)    # include "othr" method tags?
{
		
	if(!exists("get_tags", mode="function")) { source(file="get tags.R") }
	b <- get_tags(set1, tagset_name)
	d <- get_tags(set2, tagset_name)
	
	nset1 <- nrow(get(set1))
	nset2 <- nrow(get(set2))
	
	# Line up tag names 
	# set1 first:
	b0 <- if (include_other) { b } else { b[!names(b) %in% "Othr"] }    # Exclude "other" tag
	b1 <- names(b0)[order(b0, decreasing=T)]			# Sort by rank	
	
	# repeat for set2:
	d0 <- if (include_other) { d } else { d[!names(d) %in% "Othr"] }	# Exclude "other" tag
	d1 <- names(d0)[order(d0, decreasing=T)]			# Sort by rank	
	
	
	# Add percentages or diss counts
	if (pcts) {
		# Add percentages to each tag
		b2 <- paste0(b1, " (", round(100*b0[order(b0, decreasing=T)] / 
					 nset1, 0), "%)")
		d2 <- paste0(d1, " (", round(100*d0[order(d0, decreasing=T)] / 
					 nset2, 0), "%)")
	
		filename <- file.path(imageloc, paste0("Ranks of methods in ", set1, " v ", 
							set2, "--", tagset_name,", no Othr, pcts.pdf"))
	} else {
		# Add diss counts to each tag
		b2 <- paste0(b1, " (", b0[order(b0, decreasing=T)], ")")	
		d2 <- paste0(d1, " (", d0[order(d0, decreasing=T)], ")")
	
		filename <- file.path(imageloc, paste0("Ranks of methods in ", set1, " v ", 
		                   set2, "--", tagset_name,", no Othr, counts.pdf"))
	}
	
	## Test significance of any differences

	# Strategy: For each tag, construct a 2x2 contingency matrix with columns
	# = {set1, set2} and rows = {this-tag, not-this-tag}; try to reject the
	# null hypothesis that the ratio within each column is the same. Account
	# for the fact of multiple comparisons, and thus higher chance of
	# randomly low p value somewhere in the set, via Bonferroni correction.
	# Return asterisks or blank space to add to the label.
	
	onetag.fisher <- function(tag="Clin", verbose=F) {
		mat <- matrix(nrow=2,
			  data=c(b[tag], (nset1 - b[tag]), 	# first column
			  		 d[tag], (nset2 - d[tag])		# second column
					  ),
			  dimnames=list(c(tag, paste("Not", tag)),
							c(set1, set2)
					  )
			   )
		if(verbose) {
		    message("Testing relative odds ratio of ", tag, ":")
		}
		fish <- fisher.test(mat)
		if(verbose) { print(mat); print(fish) }
		
		# Bonferroni correction: divide target significance levels 
		# by the number of comparisons in the set
		
		if(fish$p.value < (0.001 / length(b))) {
			if(verbose) {
			    message(paste(realtags(tag, tagset_name), "is very significantly different", 
			   "(Bonferroni corrected p < 0.001) between", set1, "and", set2), "\n\n")
			}
			return(" ** ")
		} else if(fish$p.value < (0.05 / length(b))) {
			if(verbose) {
			    message(paste(realtags(tag, tagset_name), "is significantly different", 
			   "(Bonferroni corrected p < 0.05) between", set1, "and", set2), "\n\n")
			}
			return("  * ")
		} else {
			if(verbose) {
			    message(paste(realtags(tag, tagset_name), "is not significantly different", 
			   "between", set1, "and", set2), "\n\n")
			}
			return("    ")
		}
	} # end of onetag.fisher() 
	
	# Add significance labels	
	sig.b <- sapply(b1, FUN=function(x) onetag.fisher(x, verbose=verbose))
	sig.d <- sapply(d1, FUN=function(x) onetag.fisher(x, verbose=F))

	b2 <- paste0(sig.b, b2)		# on left, add labels to the left;
	d2 <- paste0(d2, sig.d)		# on right, add labels to the right.
	
	if(remake_figs) { pdf(file=filename) }

		# set up a blank plot
		plot(x=c(rep(0, length(b)+2), 5), 
			 y=0:(length(b)+2), 
			 axes=FALSE, 
			 type="n", 
			 xlab="", 
			 ylab="")
		
		# arrange set1 in descending rank order on the left, set2 on right
		# myoffset <- length(b2)/3 + 0.4
		myoffset <- 0.3
		
		text(labels=b2, 
			 x=rep(1, length(b2)), 
			 y=length(b2):1,
			 pos=2
		)
		text(labels=d2, 
			x=rep(4, length(d2)), 
			y=length(d2):1,
			pos=4
		)
		
		## connect matching methods with lines for ease of comparison

		# optionally add color to lines to detangle spaghetti
		if(colorful) {
			require(RColorBrewer)
			mycol <- brewer.pal(4, "Dark2")
		} else {
			mycol <- c("#000000")
		}
		
		tag <- b1[1]
		lapply(b1, mycol=mycol, FUN=function(tag, mycol) {
			# locate each tag on the plot
			y.left  <- length(b2) - grep(tag, b1) + 1
			y.right <- length(b2) - grep(tag, d1) + 1
			col.index <- (y.left-1) %% length(mycol) + 1
			
			# draw a line between tag's positions on left and on right	
			segments(x0=1 + myoffset, 
					 y0=y.left,
			         x1=4 - myoffset, 
			         y1=y.right,
			         col=mycol[col.index]
			)
			
			# extend those lines to point horizontally to the tags, 
			# to remove ambiguity
			segments(x0=1 + myoffset, y0=y.left,
					 x1=1.1 , y1=y.left,
			         col=mycol[col.index])
			segments(x0=4 - myoffset, y0=y.right,
					 x1=3.9, y1=y.right,
			         col=mycol[col.index])		
		})

		# label the two columns
		if(!is.null(betterlabels)) { 
			if(length(betterlabels)==2) {
				text(labels=betterlabels, 
					 x=c(1, 4), 
					 y=rep(length(b)+2, 2)
				)
			} else {
				warning("Incorrect number of betterlabels: ",
						"must be vector of length 2. Using set names.")
				text(labels=c(set1, set2), 
					 x=c(1, 4), 
					 y=rep(length(b) + 2, 2)
				)
			}
		} else {
		    text(labels=c(set1, set2), 
		         x=c(1, 4), 
		         y=rep(length(b) + 2, 2)
		    )
		}
	
		text(labels=c(paste0("(N=", nset1, ")"), 
					  paste0("(N=", nset2, ")")), 
		     x=c(1, 4), 
		     y=rep(length(b) + 1, 2),
			 cex=0.8
		)
		
		# add legend for significance
		if(any(grep("*", sig.b, fixed=T))) {
			mtext(paste("Bonferroni corrected 2-sided Fisher Exact Test of Independence:\n",
			            "* p < 0.05         ** p < 0.001"),
				  cex = 0.8,
				  side = 1,
				  outer = T,
				  line = -2
			)
		} else {
		    mtext("No comparisons significant via Bonferroni corrected Fisher exact test at p < 0.05",
		          cex=0.8,
		          side=1)
		}
		
	if (remake_figs) { dev.off() }

}		# end of wrapper function compare_method_ranks


# Plot changes in a given set of methods over time.
method_line_graph <- function(tagset = no_ped_tagnames,     # character vector of method tags
                              dataset = knownprograms2001_2015,
                              start_year = 2001,
                              end_year = 2015, 
                              scaled = FALSE,
                              do.plot = TRUE, 
                              label_year_n = TRUE,
                              mycolors = NULL,
                              ... )   # graphing parameters
{
    # Placeholders for data
    tagcounts <- c()
    yearspan <- length(start_year:end_year)
    all_year_n <- c()
    
    # Tweak sum function depending on length of tagset
    sumfun <- if(length(tagset) == 1) sum else colSums
    
    # Loop over years, gathering data into place
    for (year in start_year:end_year) {
    
        yearcounts <- sumfun(dataset[which(dataset$Year == year), tagset])
        year_n <- length(dataset[which(dataset$Year == year), "Year"])
        if(scaled) {
            yearcounts <- yearcounts / year_n
        }
        tagcounts <- cbind(tagcounts, yearcounts)
        all_year_n <- c(all_year_n, year_n)
    }
    
    # Label the years
    colnames(tagcounts) <- start_year:end_year
    names(all_year_n) <- start_year:end_year
    
    # If only one method, have to add that name
    if(is.null(row.names(tagcounts))) {
        row.names(tagcounts) <- tagset
    }

    # If we're using percentages, present it neatly    
    if(scaled) {
        tagcounts <- round(100*tagcounts, 2)
    }
    
    if(remake_figs) {
        if(!exists("build_plot_title")) {
            source(file = "build_plot_title.R")
        }
        outfile <- paste("method line graph", 
                          # tagset
                          if(length(tagset) > 10) {
                              substitute(tagset)
                          } else if (length(tagset) == 1) { 
                              tagset 
                          } else { 
                              paste0(tagset, sep="-") 
                          },
                         
                          # year span
                          start_year, "-", 
                          end_year,
                         
                          # scaled?
                          if(scaled) "scaled" else ""
                         )
        
                                   
        outfile <- paste0(outfile, ".pdf")
        outfile <- file.path(imageloc, outfile)
        
        pdf(outfile)
    }
    
    # Create empty plot using max value + some extra space
    plot(NULL,
         xlim = c(0, yearspan + 1),
         ylim = c(0, max(tagcounts)),
         bty = "n",
         xaxt = "n",
         xlab = "Year",
         ylab = paste0(if(scaled) "% " else "", "Dissertations Tagged")
    )
    
    # Label the years along the x-axis
    axis(side=1, at=1:yearspan, labels=colnames(tagcounts))

    # And optionally the number of dissertations for each year    
    if(label_year_n) {
        axis(side=1, at=1:yearspan, labels=paste0("n=",all_year_n),
             tick = F,
             line = 1,
             lwd = 0,
             cex.axis = 0.7,
             )
    }
    
    # If no colors are provided, use pretty colors
    if(is.null(mycolors)) {
        require(viridisLite)
        mycolors <- viridis(n = length(tagset))
    }
    
    # Use preferred labels to distinguish lines
    mypch <- c(1, 2, 0, 15:18, 3:7, 10, 13, 12)
    
    
    for (i in seq_along(tagset)) {
        points(tagcounts[tagset[i],], bty="n", xaxt="n", 
               col = mycolors[i],
               pch = mypch[i])
        lines(lowess(tagcounts[tagset[i],], f=1/2), 
              col = mycolors[i])
    }
    
    outside_legend("topright", 
                   legend = tagset, 
                   col = mycolors, 
                   pch = mypch,
                   bty = "l")
    
    if(remake_figs) {
        dev.off()
    }
    
    return(tagcounts)
}



# Testing area
if(FALSE) {   
	remake_figs=F

	method_line_graph(tagset=c("Ethn"), mycolors = "black")
	method_line_graph(tagset=c("Rhet", "Meta", "Clin"))
	
	method_line_graph(tagset=no_ped_tagnames)
	
	remake_figs=T
    	require(viridisLite)
    	mycolors <- viridis(n = length(no_ped_tagnames))
    	# TO DO: use method-group colors as per chapter 3
    	# TO DO: make small multiples plot with all methods
    	
    	for (i in seq_along(no_ped_tagnames)) {
    	    method_line_graph(tagset=c(no_ped_tagnames[i]), mycolors = mycolors[i])
    	}
	remake_figs=F
	
	compare_method_ranks("consorts", "nonconsorts", 
						betterlabels=c("Consortium", "All Non-Consortium"))
	compare_method_ranks("consorts", "top.nonconsorts", 
						betterlabels=c("Consortium", "Top Non-Consortium"))
	compare_method_ranks("noexcludes2001_2005", "noexcludes2006_2010", #tagset_name="tagnames.simple",
	                     betterlabels=c("All departments, 2001-2005",
	                                    "All departments, 2006-2010"))
	compare_method_ranks("noexcludes2006_2010", "noexcludes2011_2015", #tagset_name="tagnames.simple",
	                     betterlabels=c("All departments, 2006-2010",
	                                    "All departments, 2011-2015"))
	compare_method_ranks("realconsorts2001_2005", "realconsorts2006_2010", tagset_name="tagnames.simple",
	                     betterlabels=c("Consortium programs, 2001-2005",
	                                    "Consortium programs, 2006-2010"))
	compare_method_ranks("knownprograms2001_2015", "nonconsorts2001_2015")
	compare_method_ranks("knownprograms2001_2005", "knownprograms2011_2015", tagset_name="tagnames.simple")
	
	
	# RCWS vs non-RCWS
	compare_method_ranks("knownprograms2001_2015", 
	                     "nonrcws2001_2015", 
	                     tagset_name="no_ped_tagnames",
	                     colorful=T)
	
	compare_method_ranks(set1="knownprograms2001_2015", 
	                     set2="nonrcws2001_2015sans_badtops", 
	                     betterlabels=c("Confirmed RCWS dissertations", 
	                                    "Confirmed non-RCWS dissertations"), 
	                     tagset_name="no_ped_tagnames", verbose = T)
	
	# Different time bins
	    # no significant differences between first and second third
	compare_method_ranks("knownprograms2001_2005",
	                     "knownprograms2006_2010",
	                     tagset_name="no_ped_tagnames",
	                     betterlabels=c("RCWS 2001-2005", "RCWS 2006-2010"))
	
	    # in final third, highly significant increases in Rhet and Crit; all else not significant.
	compare_method_ranks("knownprograms2006_2010",
	                     "knownprograms2011_2015",
	                     tagset_name="no_ped_tagnames",
	                     betterlabels=c("RCWS 2006-2010", "RCWS 2011-2015"))
	
	    # what about first third compared to last third?
	compare_method_ranks("knownprograms2001_2005",
	                     "knownprograms2011_2015",
	                     tagset_name="no_ped_tagnames",
	                     betterlabels=c("RCWS 2001-2005", "RCWS 2011-2015"),
	                     verbose = T)
	    # now there are more changes:
        #   Rhet jumps from 16% (sixth place) to 49% (first place), p < 0.001
	    #   Crit climbs from 26% (third place) to 41% (second place), p < 0.001
	    #   Intv increases modestly from 8% to 14% (p = 0.0015)
	    #   Meta falls from 9% to 2% (last place) (p = 0.000025)
	
	for(startyear in 2001:2014) {
	    endyear <- startyear
	    compare_method_ranks(set1 = paste0("knownprograms", startyear, "_", endyear),
	                         set2 = paste0("knownprograms", startyear+1, "_", endyear+1),
	                         tagset_name = "no_ped_tagnames",
	                         betterlabels = c(paste0("RCWS ", startyear, "-", endyear),
	                                         paste0("RCWS ", startyear+1, "-", endyear+1))
	                        )
	}   # going year by year, the only significant jumps are in Rhet: * in 2010-11, and ** in 2011-12
	
	for(startyear in 2001:2014) {
	    endyear <- startyear + 1
	    compare_method_ranks(set1 = paste0("nonrcws2001_2015sans_badtops", startyear, "_", endyear),
	                         set2 = paste0("nonrcws2001_2015sans_badtops", startyear+1, "_", endyear+1),
	                         tagset_name = "no_ped_tagnames",
	                         betterlabels = c(paste0("Non-RCWS ", startyear, "-", endyear),
	                                          paste0("Non-RCWS ", startyear+1, "-", endyear+1))
	    )
	}   # going year by year, the only significant jumps are in Rhet: ** in 2011-12
	
	compare_method_ranks("knownprograms2001_2002",
	                     "knownprograms2014_2015",
	                     tagset_name="no_ped_tagnames",
	                     betterlabels=c("RCWS 2001-2002", "RCWS 2014-2015"),
	                     verbose = T)
	method_line_graph()
} else {
    message("The following function has been loaded:\n",
            "    compare_method_ranks(set1, set2, pcts, colorful, betterlabels, tagset_name) \n",
            "    method_line_graph(tagset, dataset, start_year, end_year, scaled, ...)")
}
