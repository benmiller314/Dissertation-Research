# Exploring topics within individual dissertations. Now with author name search, for convenience.
# NB: Author names from ProQuest are in all caps, like this: "LASTNAME, FIRSTNAME"

if(!exists("get.topics4doc", mode="function")) { source(file="top docs per topic.R") }
get.topics4author <- function(authorname, dataset_name="consorts", ntopics=55, howmany=10, showlabels=TRUE) {

	pubnum <- noexcludes[grep(authorname, noexcludes$Author, ignore.case=T), "Pub.number"]
	if(length(pubnum) == 1) {
		print(noquote(paste("$author: ", noexcludes.dt[as.character(pubnum), list(Author)]$Author)))
		print(get.topics4doc(pubnum, dataset_name, ntopics, howmany, showlabels))
	} else if (length(pubnum) > 1) {
		message("More than one match; please use exact author name from list below.")
		results <- noexcludes.dt[as.character(pubnum), list(Author, Title)]
		print(results)
		
		a <- as.integer(readline("Search again using row number... "))
		pubnum <- results[a, Pub.number]
		print(noquote(paste("$author: ", results[a, Author])))
		print(get.topics4doc(pubnum, dataset_name, ntopics, howmany, showlabels))
	}
}

if(autorun) {
	get.topics4author("MUELLER, DEREK")			
	get.topics4author("Lucas")
}	
