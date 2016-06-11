#############################################################################
# heatmap_ben.R
# 
# Construct a map of squares, shaded according to value. Based heavily on R's
# core heatmap function, modified to print the value for each square of the
# grid.
#####


heatmap.ben <- function (
	sum.by.tags, 			# A list output by sumbytags() containing 
							# a correlation matrix, solos, & totals
	diags    = FALSE,		# Should we outline diagonals? 
	highval  = "#818181",	# Darkest color
	lowval   = "#FAFAFA",	# Lightest color
	numCols  = 10,			# How many different shades?
	rowscale = FALSE,		# Should we norm each row by tag totals?
	verbose  = TRUE,		# Should we add a subtitle about solo tags?
	legend   = TRUE,		# Should we output a separate file with a legend 
							# for the color map?
	dendro	 = FALSE		# Should we output dendrograms showing method
							# clustering?
	) 
{

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
	hcr <- hclust(dist(sum.by.tags.s))				# Cluster based on distances
    ddr <- as.dendrogram(hcr)						# Convert to dendrogram 
													# (which we might use later)
   	ddr <- reorder(ddr, Rowv)						# Reorder the dendrogram
    rowInd <- order.dendrogram(ddr)					# Extract the row order 
   	Colv <- colMeans(sum.by.tags.s, na.rm = TRUE)	# Find column means
    hcc <- hclust(dist( if(symm) {sum.by.tags.s}	# Cluster based on distances 
													# (from the col perspective)
  						else {t(sum.by.tags.s)}))
	ddc <- as.dendrogram(hcc)						# Convert to dendrogram 
													# (which we might use later)
	ddc <- reorder(ddc, Colv)						# Reorder the dendrogram
	colInd <- order.dendrogram(ddc)					# Extract the column order
	
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

	# color function
		# if we're norming rows, use white for 0 and black for 100%
		if(rowscale) {				
			max.val <- 1
			min.val <- 0
			highval <- "#000000"
			lowval  <- "#FFFFFF"
		} else {
			if(any(sum.by.tags.s == 0)) { lowval <- "#FFFFFF" }
			max.val <- max(sum.by.tags.s)
			min.val <- min(sum.by.tags.s)
		}

		pal <- colorRampPalette(c(lowval, highval))
		cols <- pal(numCols)

	colorme <- function (val) {
		colIndex <- round(numCols * (val - min.val) / (max.val - min.val))
		colIndex <- max(1,colIndex)
		return(cols[colIndex])
	}
	
	if(legend) {
		if(remake_figs) { 
			if(rowscale) {
				filename <- paste0(imageloc, "color legend for ", 
									sum.by.tags$dataset, 
									" method correlations, normed.pdf")
			} else {
				filename <- paste0(imageloc, "color legend for ", 
									sum.by.tags$dataset, 
									" method correlations, raw.pdf")
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
	plot(ddc)
	
	# add subtitle indicating scaled / not scaled
	if (verbose) {
		if (rowscale) {
			h2 <- paste("Each row normed by dividing over total number 
						of dissertations for that row's tag.",
						"\n",
						"Diagonals represent tags occurring on one-method 
						dissertations.")
		} else {
			h2 <- "Diagonals represent tags occurring on one-method
				  dissertations."
		}
		
		title(sub=h2)
	}
	
	if(dendro) {
		if(remake_figs) { 
			if(rowscale) {
				filename <- paste0(imageloc, "dendrogram for ", 
									sum.by.tags$dataset, 
									" method column correlations.pdf")
			} else {
				filename <- paste0(imageloc, "dendrogram for ", 
									sum.by.tags$dataset, 
									" method correlations, raw.pdf")
			}
			pdf(filename)
		}
		plot(ddc)
		if(remake_figs) { dev.off() }
		
		if(rowscale) { 
			if(remake_figs) {
				filename <- paste0(imageloc, "dendrogram for ", 
									sum.by.tags$dataset, 
									" method row correlations.pdf")
				pdf(filename)
			} 
		plot(ddr)
			if(remake_figs) { dev.off() }

		}
	} # end of if(dendro)
	
} # end of wrapper function heatmap.ben()
