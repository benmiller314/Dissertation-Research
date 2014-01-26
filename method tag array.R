## GOAL: given method terms in one column, create and append an array of tag labels, 0 or 1,
## and append columns for Method Count and Exclude Level (0=keep, 1=maybe throw out, 2=throw out).
## Note that this used to be done in GoogleRefine, but I want it more automate-able.


# # Make sure we have data to work with during testing
# if(!exists("tagnames")) {
	# source(file="dataprep.R")
# }

# if(!exists("noexcludes")) {
	# source(file="dataprep 2 - load data.R")
# }


parse_tags <- function(data) {
	# Check that the columns we're adding don't already exist
	while(any(names(data) %in% tagnames)) {
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
	tags <- data.frame(
		"Pub.number" = data["Pub.number"],
		"Clin" = 0,
		"Crit" = 0,
		"Cult" = 0,
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
		"Method.Count" = 0,
		"Exclude.Level" = 0
	)
	
	# For each method tag, deduce from Method.Terms what tags are present.
	mt <- data[, "Method.Terms"]
	searchterms <- c("Clinical","Hermeneutical","Cultural","Discourse","Ethnographic","Experimental","Historical","Interview","Meta-Analy","Model","Philosophical","Poetic","Practitioner","Rhetorical", "Survey", "Other")
	
	searchresults <- lapply(searchterms, FUN=function(x) grep(x, mt, ignore.case=F))
	
	## bug-hunting
	# grep("Clinical", a[,"Method.Terms"], ignore.case=F)
	
	for (i in 1:length(searchresults)) {
		tags[searchresults[[i]], i+1] <- 1
	}
	
	# Populate Method.Count by summing across each row
	for (i in 1:nrow(tags)) {
		tags[i,"Method.Count"] <- sum(tags[i,tagnames])
	}
	
	# Populate Exclude.Level 
	el <- grep("xclude", mt, fixed=T)
	tags[el, "Exclude.Level"] <- tags[el, "Exclude.Level"] + 2
	cbind(bigarray[el,"Method.Terms"], tags[el,"Exclude.Level"])
	
	el2 <- grep("xclude ?", mt, fixed=T)
	tags[el2, "Exclude.Level"] <- tags[el2, "Exclude.Level"] - 1
	
	head(data.frame(				# make sure it worked
			"Method.Terms" = data[which(tags$Exclude.Level > 0),"Method.Terms"], 
			"Exclude.Level" = tags[which(tags$Exclude.Level > 0),"Exclude.Level"]
	    ), 30)
	
	
	# # Explore the data
	# table(tags["Exclude.Level"])
	# table(tags["Method.Count"])
	# a <- which(tags$Method.Count == 1)
	# lapply(tags[a,tagnames],sum)
	# a <- which(tags$Cult == 1 & tags$Method.Count ==1)
	# noexcludes[a,c("Title","ABSTRACT")]
	# cbind(noexcludes[a,c("Method.Terms","Method.Count")], tags[a,"Method.Count"])
	# head(tags)
	# head(mt)
	
	## Satisfied that the foregoing worked, let's merge 
	data2 <- merge(data,tags,by="Pub.number")
	return(data2)
}

# clean up the workspace -- not needed after function notation is put in
# rm(a, mt, n, data, searchterms, searchresults, el, el2)
