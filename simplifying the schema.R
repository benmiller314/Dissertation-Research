## Goal: Group tags to pool influence of (e.g.) quantitative approaches.
#  Possible groups: Aggregable (disc, expt, surv, meta), Phenomenological (case, ethn), Dialectical (crit, hist, modl, phil, rhet), Craft-Based (poet, prac [and tool-building]; cf. Johnson 2010). Bad fits: intv, meta. Drop cult and intv, move meta to Agg.
# As an alternative pool, cf. Michael Carter's "Ways of Knowing and Doing in the Disciplines"?

if(!exists(sfreq)) {
	source(file="top 10 lists.R")
}
if(!exists(noexcludes)) {
	source(file="dataprep.R")
}

# define shortcut for new tag names
tagnames.collapse <- c("Aggreg", "Phenom", "Dialec", "Crafty")
	
short_schema <- function (data) {	
	# Create a data frame to hold the updated info; we'll merge later.
	collapse <- data.frame(
			Pub.number = data["Pub.number"],
			Aggreg = -1,
			Phenom = -1,
			Dialec = -1,
			Crafty = -1,
			Counts.collapse = -1
	)
	head(collapse)
	
	# For each data row, record if it's tagged with any member of each group
	for (i in 1:nrow(data)) {
		# Aggregable
		a1 <- as.integer(data[i,"Disc"])
		a2 <- as.integer(data[i,"Expt"])
		a3 <- as.integer(data[i,"Surv"])
		a4 <- as.integer(data[i,"Meta"])
		a <- max(a1, a2, a3, a4)
		ag <- collapse[i,"Aggreg"] <- a
		
		# Phenomenological
		a1 <- as.integer(data[i,"Case"])
		a2 <- as.integer(data[i,"Ethn"])
		a <- max(a1, a2)
		ph <- collapse[i,"Phenom"] <- a
		
		# Dialectical
		a1 <- as.integer(data[i,"Crit"])
		a2 <- as.integer(data[i,"Hist"])
		a3 <- as.integer(data[i,"Modl"])
		a4 <- as.integer(data[i,"Phil"])
		a5 <- as.integer(data[i,"Rhet"])
		a <- max(a1, a2, a3, a4, a5)
		di <- collapse[i,"Dialec"] <- a
		
		# Craft-Based
		a1 <- as.integer(data[i,"Poet"])
		a2 <- as.integer(data[i,"Prac"])
		a <- max(a1, a2)
		cr <- collapse[i,"Crafty"] <- a
		
		# Now look for multi-modality across these broad categories
		collapse[i, "Counts.collapse"] <- sum(ag, ph, di, cr)
	}
	# Clean up the workspace
	rm(a, a1, a2, a3, a4, a5, ag, ph, di, cr)

	return(collapse)
}

# # Confirm the function works properly
# collapse <- short_schema(noexcludes)
# head(collapse, 10)
	
# # Explore the newly configured data
# table(collapse$Counts.collapse)
# mean(collapse$Counts.collapse)
# noexcludes[which(collapse$Counts == 0),c("Method.Terms",tagnames)]

# names(noexcludes)

# colSums(collapse[2:(ncol(collapse)-1)])



