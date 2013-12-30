# Factor-Bug fixing:
# when you find out your reconciliation picked the wrong version, and now you get invalid factor levels
# NB: Now included in function scratchpad

fix_factor <- function(f, to.add, to.remove = NULL) {
	ff <- as.character(f)

	if (!is.null(to.remove)) {
		ff[which(ff == to.remove)] <- to.add
	} else {
		ff <- c(ff, to.add)
	}
	return(factor(ff))
}

