# Construct a map of squares, shaded according to value

heatmap.ben <- function (sum.by.tags.s, diags=FALSE) {
	
	highval <- "#818181"		# darkest color
	lowval  <- "#FAFAFA"		# lightest color
	
	n.col <- ncol(sum.by.tags.s); # print(n.col)
	n.row <- nrow(sum.by.tags.s); # print(n.row)

	# color function
	colorme <- function (val) {
		numCols <- 20
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
	axis(side=4, at=n.row:1, labels=row.names(sum.by.tags.s), pos=0.5+n.col, las=2, col="white")
	axis(side=1, at=1:n.col, labels=names(sum.by.tags.s), pos=0.5, las=2, col="white")	
}