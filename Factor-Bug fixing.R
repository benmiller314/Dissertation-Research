## Rationale:
#  When dealing with text, R likes to pre-determine what counts as a valid possibility 
#  (because it thinks everything is an experimental observation with controlled variables). 
#  Trying to add new rows in a text column, therefore, sometimes causes problems. 
#  fix_factor allows you to add new items to (or edit old ones in) your factor-ish vectors.
#
## Parameters: 
#  f         - a factor, i.e. a text column, in which you want to add or edit some entry
#  to.add    - the entry you wish to add, or the revised value if editing. required.
#  to.remove - the entry you wish to replace, if editing. optional.
#
## Usage:
#  some.factor <- fix_factor(some.factor, to.add=some.new.text)

fix_factor <- function(f, to.add, to.remove = NULL) {
	ff <- as.character(f)

	if (!is.null(to.remove)) {
		ff[which(ff %in% to.remove)] <- to.add
	} else {
		ff <- c(ff, to.add)
	}
	return(factor(ff))
}

## Re-factor all factor columns to remove holdovers from supersets
#  Rationale: When you subset a data.frame, the factors can have more values than there are rows.
#             We want to fix that.
#  Usage:
#  consorts <- refactor.all("consorts")
#
refactor.all <- function(dataset_name="consorts") {
	dataset <- get(dataset_name)
	for (i in 1:ncol(dataset)) {
		if(is.factor(dataset[,i])) {
			dataset[,i] <- factor(dataset[,i])
		}
	}
	return(dataset)
}
