# Exploring topics within individual dissertations. Now with author name search, for convenience.
# NB: Author names from ProQuest are in all caps, like this: "LASTNAME, FIRSTNAME"

if(!exists("get.topics4doc", mode="function")) { source(file="top docs per topic.R") }
get.topics4author <- function(authorname, dataset_name="consorts", ntopics=55, howmany=10, showlabels=TRUE) {

	pubnum <- noexcludes[grep(authorname, noexcludes$Author), "Pub.number"]
	get.topics4doc(pubnum, dataset_name, ntopics, howmany, showlabels)	
}

if(autorun) {
	get.topics4author("MUELLER, DEREK")			
}	
