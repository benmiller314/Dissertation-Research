#############################################################################
# heatmap_ben.R
# 
# Construct a map of squares, shaded according to value. Based heavily on R's
# core heatmap function, modified to print the value for each square of the
# grid.
#
# This file will be called by `method collocation heatmap.R`. Or maybe it should go 
# the other way. Apparently I used to just clutter my files with a lot of
# junk code. So. Still working on that.
#####


heatmap.ben <- function (
	dataset_name = "noexcludes2001_2015",
	tagset_name = "tagnames",
	sum.by.tags = NULL,     # Allow for passing in correlations from sumbytags; avoid infinite loop!
    diags    = FALSE,		# Should we outline diagonals?
	palette  = NULL,        # Optionally pass a vector of hex colors or a generator function
	                        # NB: if palette is a vector, numCols will be ignored.
	highval  = "#818181",	# Darkest color
	lowval   = "#FAFAFA",	# Lightest color
	numCols  = 10,			# How many different shades?
	rowscale = FALSE,		# Should we norm each row by tag totals?
	verbose  = TRUE,		# Should we add a subtitle about solo tags?
	legend   = TRUE,		# Should we output a separate file with a legend 
							# for the color map?
	dendro	 = FALSE,		# Should we output dendrograms showing method
							# clustering?
	mytitle    = NULL,      # optionally pass a title for the plot
	clust.method = hclust,  # or any option from cluster package, like agnes or diana
	rowInd = NULL,          # use for replicating row order across samples
	colInd = NULL           # use for replicating column order across samples
	) 
{
    
    require(cluster)
    
    # get the data
     
    if (!is.null(sum.by.tags)) {
        
        # if it exists and is a matrix, move on!
        # if it's not a matrix, extract dataset/tagset info from it.
        if(!is.matrix(sum.by.tags)) {
            # check if it's from the right dataset
            if((dataset_name != sum.by.tags$dataset_name) ||
               (tagset_name != sum.by.tags$tagset_name)) {
                warning("heatmap.ben: dataset or tagset mismatch.\n",
                        "Using values from passed correlation matrix, ",
                        "but filenames may be inaccurate. \n", 
                        "Internal remake_figs set to FALSE; save manually or fix mismatch.")
                remake_figs <- F
            }
            dataset_name <- sum.by.tags$dataset_name
            tagset_name <- sum.by.tags$tagset_name
        }
        
    # if the data doesn't exist, create it now.     
    } else {
        if(!exists("sumbytags", mode="function")) {
            source(file="sum by tags.R")
        }
        sum.by.tags <- sumbytags(dataset_name, tagset_name)
    } 
    
    dataset <- get(dataset_name)
    
    
	# extract the matrix, if need be
	if(!is.matrix(sum.by.tags)) {
		sum.by.tags.s <- sum.by.tags$correlations
		if(is.null(sum.by.tags.s)) { 
			sum.by.tags.s <- as.matrix(sum.by.tags) 
		}
	} else { 
		sum.by.tags.s <- sum.by.tags 
	}
	
	# is it symmetrical?
	symm <- all(sum.by.tags.s == t(sum.by.tags.s))

	## sort the matrix (borrowed from heatmap())
	
	Rowv <- rowMeans(sum.by.tags.s, na.rm = TRUE)	# Find row means
	hcr <- clust.method(dist(sum.by.tags.s))				# Cluster based on distances
    ddr <- as.dendrogram(hcr)						# Convert to dendrogram 
													# (which we might use later)
   	ddr <- reorder(ddr, -Rowv)						# Reorder the dendrogram, 
   	
   	if(is.null(rowInd)) {                                                # highest values on top
        rowInd <- order.dendrogram(ddr)					# Extract the new row order 
   	}
   	Colv <- colMeans(sum.by.tags.s, na.rm = TRUE)	# Find column means
    hcc <- clust.method(dist( if(symm) {sum.by.tags.s}	# Cluster based on distances 
													# (from the col perspective)
  						else {t(sum.by.tags.s)}))
	ddc <- as.dendrogram(hcc)						# Convert to dendrogram 
													# (which we might use later)
	ddc <- reorder(ddc, -Colv)						# Reorder the dendrogram, 
	
	if(is.null(colInd)) {                                            # highest values at left
	    colInd <- order.dendrogram(ddc)					# Extract the new column order
	}
	
	sum.by.tags.s <- sum.by.tags.s[rowInd, colInd]	# Apply row and column 
													# orders from above
	
	
	# make variables more readable for later
	n.col <- ncol(sum.by.tags.s); # print(n.col)
	n.row <- nrow(sum.by.tags.s); # print(n.row)


	# norm by tag totals
    if (rowscale) {
		# get the totals from the sumbytags() list object
		totals <- sum.by.tags$total.counts			
		
		# put it in the same order as the rows
		totals <- totals[rowInd]					
		
		# divide each row by the total of that row's tag
		sum.by.tags.s <- apply(sum.by.tags.s, 1, 	
			FUN=function(x) { x/totals })
			
		# round to make it prettier	
		sum.by.tags.s <- round(sum.by.tags.s, 2) 	
		sapply(sum.by.tags.s, FUN=function(x) { if(is.na(x)) x <- 0 })
    }

	## color function
	
	
    # if we're norming rows, override defaults and use white for 0 and black for 100%
		if (rowscale) {				
			max.val <- 1
			min.val <- 0
			highval <- "#000000"
			lowval  <- "#FFFFFF"
		} else {
		    max.val <- max(sum.by.tags.s)
			min.val <- min(sum.by.tags.s)
		}
	
	# but if a palette already exists, use it.
    	if (is.null(palette)) { 
    	    pal <- colorRampPalette(c(lowval, highval))
    	    cols <- pal(numCols)
    	} else {
    	    # if the palette isn't a function, let's hope it's a vector of colors...
    	    if (mode(palette) == "function") {
        	    pal <- palette 
    	        cols <- pal(numCols)
    	    } else if (mode(palette) == "character") {
        	    cols <- palette
    	    } else {
    	        warning("heatmap.ben: palette was passed, but isn't a function or vector of hex values;\n",
    	                "proceeding with default values.")
    	        pal <- colorRampPalette(c(lowval, highval))
    	        cols <- pal(numCols)
        	}
    	}
	
	

	colorme <- function (val) {
	    # make sure actual zero values have blank/white backgrounds
	    if(val == 0) { return ("#FFFFFF") }
	    
	    # otherwise, use the palette defined above
	    # TO DO: ugh, forgot to account for negative numbers. Maybe a uniform shift up by abs(min.val)?
	    
		colIndex <- round(numCols * (val - min.val) / (max.val - min.val))
		colIndex <- max(1,colIndex)
		return(cols[colIndex])
	}
	
	if(legend) {
		if(remake_figs) { 
			if(rowscale) {
				filename <- file.path(imageloc, paste0("color legend for ", 
									dataset_name, 
									" method correlations, normed.pdf"))
			} else {
				filename <- file.path(imageloc, paste0("color legend for ", 
				                    dataset_name, 
									" method correlations, raw.pdf"))
			}
			pdf(filename)
		}
		xleft <- seq(0, 1, length.out=numCols)
		xdiff <- xleft[2]-xleft[1]
		plot(x = 0, 
			 y = 0, 
			 xlim = c(0,1+xdiff), 
			 ylim = c(0,1), 
			 type = "n", 
			 xaxt = "n", 
			 yaxt = "n", 
			 xlab = "", 
			 ylab = "", 
			 bty="n"
		)
		
		rect(xleft = xleft, 
			 xright = xleft + xdiff, 
			 ybottom = 1, 
			 ytop = 1 - xdiff, 
			 col = cols
		)
		
		text(x = xleft + xdiff / 2, 
			 y = 1 - 2 * xdiff, 
			 labels = round(seq(min.val, max.val, length.out=numCols), 1))

		if(remake_figs) { dev.off() }
		
	} # end of if(legend)

	# plot the heatmap itself
	if(remake_figs) {
	    if(rowscale) {
	        raw_or_cooked <- "scaled"
	    } else {
	        raw_or_cooked <- "raw"
	    }
    	
	    if (!is.null(mytitle)) {
	        filename <- file.path(imageloc, paste0(mytitle, ".pdf"))
	    } else {
    	    filename <- file.path(imageloc, paste0("Method correlation heatmap (",
                                               raw_or_cooked, " values), ",
	                                           dataset_name, ", ",
	                                           tagset_name, ", N",
	                                           nrow(dataset), ".pdf"))
	    }
	    pdf(filename)
	}
	
	# set up a blank canvas of the right size
	plot(0, 0, xlim=c(0.5,0.5+n.col), ylim=c(0.5,0.5+n.row), type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")

	# map each square
	for (i in 1:n.row) {
		for (j in 1:n.col) {
			# print(c('i = ',i,' j = ',j))
			diagcheck <- NULL				# outline the diagonals if need be
			if (diags && i == j) {
				diagcheck <- "black"
			}

			symbols(x = j,
					y = 1 + n.row - i,
					squares = 1, 
					add = TRUE, 
					inches = FALSE, 
					fg = diagcheck, 
					bg = colorme(sum.by.tags.s[i,j]))
			text(x = j,
				 y = 1 + n.row - i,
				 round(sum.by.tags.s[i,j], 2), 
				 cex=0.65
			)
		}
	}

	# add axis labels
	axis(side = 2, 
		 at = n.row:1, 
		 labels = rownames(sum.by.tags.s), 
		 pos = 0.5, 
		 las = 2, 
		 col = "white"
	)
	axis(side = 1, 
		 at = 1:n.col, 
		 labels = colnames(sum.by.tags.s), 
		 pos = 0.5, 
		 las = 2, 
		 col="white"
	)
	
	# add subtitle indicating scaled / not scaled
	if (verbose) {
		if (rowscale) {
			title(main = mytitle,
			       sub = paste("Each row normed by dividing over total number", 
						"of dissertations for that row's tag."))
		    mtext(side=3, text="A box in row Y, column X gives the probability that a dissertation tagged Y is also tagged X")
		} else {
		    mtext(side=3, text="A box in row Y, column X gives the number of dissertations tagged Y that are also tagged X")
		} 
	    
	    mtext(side=4, text="Diagonals represent tags occurring on one-method dissertations.")
	}
	
	if(remake_figs) {
	    dev.off()
	}
	
	if(dendro) {
		if(remake_figs) { 
			if(rowscale) {
				filename <- file.path(imageloc, paste0("dendrogram for ", 
									dataset_name, 
									" method column correlations.pdf"))
			} else {
				filename <- file.path(imageloc, paste0("dendrogram for ", 
									dataset_name, 
									" method correlations, raw.pdf"))
			}
			pdf(filename)
		}
		plot(ddc)
		if(remake_figs) { dev.off() }
		
		if(rowscale) { 
			if(remake_figs) {
				filename <- file.path(imageloc, paste0("dendrogram for ", 
									dataset_name, 
									" method row correlations.pdf"))
				pdf(filename)
			} 
	    	plot(ddr)
			if(remake_figs) { dev.off() }

		}
	} # end of if(dendro)
	
	# return stuff you might use (e.g. for duplicating this row/column order)
	invisible(list(plottedData = sum.by.tags.s,
	               rowInd = rowInd, 
	               colInd = colInd, 
	               Rowv = ddr, 
	               Colv = ddc))
	
} # end of wrapper function heatmap.ben()

if(FALSE) {
    a <- heatmap.ben(dataset_name="noexcludes2001_2015", 
                tagset_name="no_ped_tagnames",
                diags=T,
                rowscale=T)
    
    heatmap.ben(dataset_name="knownprograms2001_2015", 
                tagset_name="no_ped_tagnames",
                diags=T,
                rowscale=F)
    
}
