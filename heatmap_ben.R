# Construct a map of squares, shaded according to value

heatmap.ben <- function (
	sum.by.tags, 			# a list output by sumbytags() containing a correlation matrix, solos, & totals
	diags    = FALSE,		# should we outline diagonals? 
	highval  = "#818181",	# darkest color
	lowval   = "#FAFAFA",	# lightest color
	numCols  = 10,			# how many different shades?
	rowscale = FALSE,		# should we norm each row by tag totals?
	verbose  = TRUE,		# should we add a subtitle explaining about solo tags?
	legend   = TRUE,		# should we output a separate file with a legend for the color map?
	) {

	# extract the matrix, if need be
	if(!is.matrix(sum.by.tags)) {
		sum.by.tags.s <- sum.by.tags$correlations
		if(is.null(sum.by.tags.s)) { sum.by.tags.s <- as.matrix(sum.by.tags) }
	} else { sum.by.tags.s <- sum.by.tags }
	
	# is it symmetrical?
	symm <- all(sum.by.tags.s == t(sum.by.tags.s))

	# sort the matrix (borrowed from heatmap())
	Rowv <- rowMeans(sum.by.tags.s, na.rm = TRUE)	# find row means
	hcr <- hclust(dist(sum.by.tags.s))				# cluster based on distances
    ddr <- as.dendrogram(hcr)						# convert to dendrogram (which we might use later)
   	ddr <- reorder(ddr, Rowv)						# reorder the dendrogram
    rowInd <- order.dendrogram(ddr)					# extract the row order
    
   	Colv <- colMeans(sum.by.tags.s, na.rm = TRUE)	# find column means
    hcc <- hclust(dist( if(symm) {sum.by.tags.s}	# cluster based on distances (from the col perspective)
  						else {t(sum.by.tags.s)}))
	ddc <- as.dendrogram(hcc)						# convert to dendrogram (which we might use later)
	ddc <- reorder(ddc, Colv)						# reorder the dendrogram
	colInd <- order.dendrogram(ddc)					# extract the column order
	
	sum.by.tags.s <- sum.by.tags.s[rowInd, colInd]	# apply row and column orders from above
	
	
	# make variables more readable for later
	n.col <- ncol(sum.by.tags.s); # print(n.col)
	n.row <- nrow(sum.by.tags.s); # print(n.row)


	# norm by tag totals
    if (rowscale) {
		totals <- sum.by.tags$total.counts			# get the totals from the sumbytags() list object
		totals <- totals[rowInd]					# put it in the same order as the rows
		sum.by.tags.s <- apply(sum.by.tags.s, 1, 	# divide each row by the total of that row's tag
			FUN=function(x) { x/totals })
		sum.by.tags.s <- round(sum.by.tags.s, 2) 	# round to make it prettier
		sapply(sum.by.tags.s, FUN=function(x) { if(is.na(x)) x <- 0 })
    }

	# color function
		if(rowscale) {					# if we're norming rows, use white for 0 and black for 100%
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
				filename <- paste0(imageloc, "color legend for ", sum.by.tags$dataset, " method correlations, normed.pdf")
			} else {
				filename <- paste0(imageloc, "color legend for ", sum.by.tags$dataset, " method correlations, raw.pdf")
			}
			pdf(filename)
		}
		xleft <- seq(0, 1, length.out=numCols)
		xdiff <- xleft[2]-xleft[1]
		plot(0, 0, xlim=c(0,1+xdiff), ylim=c(0,1), type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
		rect(xleft=xleft, xright=xleft+xdiff, ybottom=1, ytop=1-xdiff, col=cols)
		text(x=xleft+xdiff/2, y=1-2*xdiff, labels=round(seq(min.val, max.val, length.out=numCols), 1))

		if(remake_figs) { dev.off() }
	}

	# set up a blank canvas of the right size
	plot(0, 0, xlim=c(0.5,0.5+n.col), ylim=c(0.5,0.5+n.row), type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")

	# map each square
	for (i in 1:n.row) {
		for (j in 1:n.col) {
			# print(c('i = ',i,' j = ',j))
			diagcheck <- NULL					# outline the diagonals if need be
			if (diags && i == j) {
				diagcheck <- "black"
			}

			symbols(j,1+n.row-i,squares=1, add=TRUE, inches=FALSE, fg=diagcheck, bg=colorme(sum.by.tags.s[i,j]))
			text(j,1+n.row-i, round(sum.by.tags.s[i,j], 2), cex=0.65)
		}
	}

	# add axis labels
	axis(side=2, at=n.row:1, labels=rownames(sum.by.tags.s), pos=0.5, las=2, col="white")
	axis(side=1, at=1:n.col, labels=colnames(sum.by.tags.s), pos=0.5, las=2, col="white")
	
	# add subtitle indicating scaled / not scaled
	if (verbose) {
		if (rowscale) {
			h2 <- paste("Each row normed by dividing over total number of dissertations for that row's tag.",
						"\n",
						"Diagonals represent tags occurring on one-method dissertations.")
		} else {
			h2 <- "Diagonals represent tags occurring on one-method dissertations."
		}
		
		title(sub=h2)
	}
}
