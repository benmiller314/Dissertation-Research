#############################################################################
# method tag array.R
#
# GOAL: given method terms in one column, create and append an array of tag
# labels, 0 or 1, and append columns for Method Count and Exclude Level
# (0=keep, 1=maybe throw out, 2=throw out). Note that this used to be done in
# GoogleRefine, but I want it more automate-able.
#
# This file is sourced during `dataprep 2 - load data.R`
#####

parse_tags <- function(data, 
                       tagstyle="long", 
                       tagcol="Method.Terms", 
                       standalone=FALSE,
                       excludecol=tagcol) {
	# Check that the columns we're adding don't already exist
	while(any(names(data) %in% tagnames)) {
		val <- readline(paste("Looks like data has already been parsed.", 
							"Overwrite (O) or Abort (A)? \n parse_tags > "))
		if(val == "A") {
			warning("Parse_tags not applied; data already parsed.")
			return(data)
		} else if (val == "O") {
			break
		} else {
			print(noquote("I do not understand your response. Try again?"))
		}
	}
    
    # Check that tagstyle is legal
    if (!tagstyle %in% c("long", "short")) {
        stop("Only 'long' and 'short' are implemented for tagstyle parameter.")
    }
    
    # Remove blank rows. (I can't think of an occasion when you'd want to leave these in.)
    blankindex <- which(data[, tagcol] == "")
    if (length(blankindex) > 0) {
        data <- data[-blankindex,]
        warning(paste("Removed", length(blankindex),
                      "dissertations that have not yet been tagged,\n",
                      "leaving", nrow(data), "tagged dissertations."))
    }
    

	# Create a data frame to hold the updated info; we'll merge later.
	tags <- data.frame(
		"Pub.number" = data["Pub.number"],
		"Clin" = 0,
		"Crit" = 0,
		"Disc" = 0,
		"Ethn" = 0,
		"Expt" = 0,
		"Hist" = 0,
		"Intv" = 0,
		"Meta" = 0,
		"Modl" = 0,
		"Phil" = 0,
		"Poet" = 0,
		"Prac" = 0,
		"Rhet" = 0,
		"Surv" = 0,
		"Othr" = 0,
		"Ped"  = 0,
		"Method.Count" = 0,
		"Exclude.Level" = 0
	)
	
	# For each method tag, deduce from Method.Terms what tags are present.
	mt <- data[, tagcol]
	if (tagstyle == "long") {
    	searchterms <- c("Clinical", "Hermeneutical",
    					"Discourse", "Ethnographic", "Experimental",
    					"Historical", "Interview", "Meta-Analy", "Model",
    					"Philosophical", "Poetic", "Practitioner", "Rhetorical",
    					"Survey", "Other", "Pedagogical Projection")
	} else if (tagstyle == "short") {
	    searchterms <- tagnames
	}
	
	searchresults <- lapply(searchterms, FUN=function(x) { 
							grep(x, mt, ignore.case=T) } )
	names(searchresults) <- tagnames
	
	## bug-hunting
	# grep("Clinical", a[,"Method.Terms"], ignore.case=F)
######## megabug, caught 2018-03-07 ########	
#	for (i in 1:length(searchresults)) {
#		tags[searchresults[[i]], i] <- 1    # ohmygod, this was i+1 and it worked, 
#		                                    # when did it change from 0-indexed to 1-indexed??
#	}
#########################
	
	# use names rather than numbers to index the array
	for (tag in names(searchresults)) {
	    tags[searchresults[[tag]], tag] <- 1
	}
	
	# Populate Method.Count by summing across each row
	for (i in 1:nrow(tags)) {
		tags[i,"Method.Count"] <- sum(tags[i,tagnames])
	}	
	
	# Account for Method.Count==0, which means either that the only tag was excluded
	# above, or that the diss hasn't been tagged yet
	zeroindex <- which(tags$Method.Count==0)
	if (length(zeroindex) > 0) {
	    warning(paste("Converted", length(zeroindex),
    	              "dissertations with solo tags now excluded",
    	              "from the schema to solo 'Other'"))
    	tags[zeroindex, "Othr"] <- 1
    	tags[zeroindex, "Method.Count"] <- 1
	}
	
	   
	# Populate Exclude.Level 
	et <- data[, excludecol]
	el <- grep("xclude", et)
	tags[el, "Exclude.Level"] <- tags[el, "Exclude.Level"] + 2
	cbind(data[el,"Method.Terms"], tags[el,"Exclude.Level"])
	
	# Find excludes with question mark (with optional space beforehand)
	el2 <- grep("xclude ?\\?", et)
	tags[el2, "Exclude.Level"] <- tags[el2, "Exclude.Level"] - 1
	
	# make sure it worked
	head(data.frame(				
		"Method.Terms" = data[which(tags$Exclude.Level > 0), tagcol], 
		"Exclude.Level" = tags[which(tags$Exclude.Level> 0), "Exclude.Level"]
	    ), 30)
	
	
	# # Explore the data
	# table(tags["Exclude.Level"])
	# table(tags["Method.Count"])
	# a <- which(tags$Method.Count == 1)
	# lapply(tags[a,tagnames],sum)
	# a <- which(tags$Cult == 1 & tags$Method.Count ==1)
	# noexcludes[a,c("Title","ABSTRACT")]
	# cbind(noexcludes[a,c("Method.Terms","Method.Count")], 
	# 		tags[a, "Method.Count"])
	# head(tags)
	# head(mt)
	
	if(standalone) {
	    return(tags)
	}
	
	## Satisfied that the foregoing worked, let's merge 
	data[, names(tags)] <- tags
	return(data)
}
