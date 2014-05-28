# Construct a map of squares, shaded according to value

heatmap.ben <- function (
	sum.by.tags, 			# a list output by sumbytags() containing a correlation matrix, solos, & totals
	diags   = FALSE,		# should we outline diagonals? 
	highval = "#818181",	# darkest color
	lowval  = "#FAFAFA",	# lightest color
	numCols = 20			# how many different shades?
	) {
	
	# extract the matrix, if need be
	if(!is.matrix(sum.by.tags)) {
		sum.by.tags.s <- sum.by.tags$correlations
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
    hcc <- hclust(dist( if(symm) {sum.by.tags.s}
  						else {t(sum.by.tags.s)}))	# cluster based on distances (from the col perspective)
	ddc <- as.dendrogram(hcc)						# convert to dendrogram (which we might use later)
	ddc <- reorder(ddc, Colv)						# reorder the dendrogram
	colInd <- order.dendrogram(ddc)					# extract the column order
	
	sum.by.tags.s <- sum.by.tags.s[rowInd, colInd]	# apply row and column orders from above
	
	
	# make variables more readable for later
	n.col <- ncol(sum.by.tags.s); # print(n.col)
	n.row <- nrow(sum.by.tags.s); # print(n.row)

	# color function
	colorme <- function (val) {
		pal <- colorRampPalette(c(lowval, highval))
		cols <- pal(numCols)
		max.val <- max(sum.by.tags.s)
		min.val <- min(sum.by.tags.s)
		colIndex <- round(numCols * (val - min.val) / (max.val - min.val))
		colIndex <- max(1,colIndex)
		return(cols[colIndex])
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
	axis(side=4, at=n.row:1, labels=rownames(sum.by.tags.s), pos=0.5+n.col, las=2, col="white")
	axis(side=1, at=1:n.col, labels=colnames(sum.by.tags.s), pos=0.5, las=2, col="white")	
}