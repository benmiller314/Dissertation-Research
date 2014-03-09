## Goal: Group tags to pool influence of (e.g.) quantitative approaches.
#  Possible groups: Aggregable (disc, expt, surv, meta), Phenomenological (case, ethn), Dialectical (crit, hist, modl, phil, rhet), Craft-Based (poet, prac [and tool-building]; cf. Johnson 2010). Bad fits: intv, meta. Drop cult and intv, move meta to Agg.
# As an alternative pool, cf. Michael Carter's "Ways of Knowing and Doing in the Disciplines"?

# # Make sure we have data to work with during testing
# if(!exists("tagnames")) {
	# source(file="dataprep.R")
# }

# if(!exists("noexcludes")) {
	# source(file="dataprep 2 - load data.R")
# }

# define shortcut for new tag names
tagnames.simple <- c("Aggreg", "Phenom", "Dialec", "Crafty", "Pract")
tagnames.simple.long <- c(
						   "Aggregable", 
						   "Phenomenological", 
						   "Dialectical", 
						   "Craft-based", 
						   "Practitioner / Teacher Research")

	
short_schema <- function (data) {	
	# Check that the columns we're adding don't already exist
	while(any(names(data) %in% tagnames.simple)) {
		c <- readline("Looks like data has already been parsed. Overwrite (O) or Abort (A)? \n short_schema > ")
		if(c == "A") {
			warning("Short_schema not applied; data already parsed.")
			return()
		} else if (c == "O") {
			break
		} else {
			print(noquote("I do not understand your response. Try again?"))
		}
	}

	# Create a data frame to hold the updated info; we'll merge later.
	simple <- data.frame(
			Pub.number = data["Pub.number"],
			Aggreg = -1,		# Aggregable
			Phenom = -1,		# Phenomenological
			Dialec = -1,		# Dialectical
			Crafty = -1,		# Craft-Based
			Pract  = -1,		# Practitioner (a little redundant, but makes `simple` simpler.)
			Counts.simple = -1
	)
	head(simple)
	
	# For each data row, record if it's tagged with any member of each group
	for (i in 1:nrow(data)) {
		# Aggregable
		a1 <- as.integer(data[i,"Disc"])
		a2 <- as.integer(data[i,"Expt"])
		a3 <- as.integer(data[i,"Surv"])
		a4 <- as.integer(data[i,"Meta"])
		a <- max(a1, a2, a3, a4)
		ag <- simple[i,"Aggreg"] <- a
		
		# Phenomenological
		a1 <- as.integer(data[i,"Clin"])
		a2 <- as.integer(data[i,"Ethn"])
		a <- max(a1, a2)
		ph <- simple[i,"Phenom"] <- a
		
		# Dialectical
		a1 <- as.integer(data[i,"Crit"])
		a2 <- as.integer(data[i,"Hist"])
		a3 <- as.integer(data[i,"Modl"])
		a4 <- as.integer(data[i,"Phil"])
		a5 <- as.integer(data[i,"Rhet"])
		a <- max(a1, a2, a3, a4, a5)
		di <- simple[i,"Dialec"] <- a
		
		# Craft-Based
		a1 <- as.integer(data[i,"Poet"])
#		a2 <- as.integer(data[i,"Prac"])
		a3 <- as.integer(grep("tool-building",data[i,"Method.Terms"],ignore.case=TRUE))
		a <- max(a1, 
		       # a2, 
				 a3)
		cr <- simple[i,"Crafty"] <- a
		
		# Practitioner
		pr <- simple[i, "Pract"] <- as.integer(data[i,"Prac"])
		
		# Now look for multi-modality across these broad categories
		simple[i, "Counts.simple"] <- sum(ag, ph, di, cr, pr)
	}
	# # Clean up the workspace (not needed after testing)
	# rm(a, a1, a2, a3, a4, a5, ag, c, ph, di, cr, pr)

	data[, names(simple)] <- simple
	return(data)
}

# Confirm the function works properly
# data <- head(bigarray)
# data
# data <- short_schema(data)
# data
# data[, tagnames.simple] <- -1
# data
# data <- short_schema(data)
# data
# rm(data, simple)
	
# # Explore the newly configured data
# table(simple$Counts.simple)
# mean(simple$Counts.simple)
# noexcludes[which(simple$Counts == 0),c("Method.Terms",tagnames)]

# names(noexcludes)

# colSums(simple[2:(ncol(simple)-1)])



