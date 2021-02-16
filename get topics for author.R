# Exploring topics within individual dissertations. Now with author name search, for convenience.
# NB: Author names from ProQuest are in all caps, like this: "LASTNAME, FIRSTNAME"

# load dependencies
if(!exists("get.topics4doc", mode="function")) { source(file="top docs per topic.R") }

# main wrapper function for author search
get.topics4author <- function(authorname, 
                              dataset_name = "noexcludes2001_2015", 
                              ntopics=50, 
                              iter_index = 1, 
                              subset_name = NULL,
                              howmany = 10, 
                              showlabels = TRUE) {

    require(data.table)
    dataset.dt <- data.table(get(dataset_name), key = "Pub.number")
    
    author.index <- grep(authorname, dataset.dt$Author, ignore.case = T)
    
    if (length(author.index)) {
        pubnum <- dataset.dt[author.index, Pub.number]
    } else {
        tryCatch(expr = {
            if (!exists("namepart", mode="function")) { source(file = "advisor relations.R") }
            authornames <- namepart(authorname, "list")
            author.index <- c()
            for (name in authornames) {
                author.index <- c(author.index, grep(name, dataset.dt$Author, ignore.case = T))
            }
            pubnum <- dataset.dt[author.index, Pub.number]
        }, 
        e = "No match for that author; try just last name or first name.",
        finally = "No exact match found; building list from space-separated name parts."
        )
    }
    
	if(length(pubnum) == 1) {
		print(noquote(paste("$author: ", dataset.dt[as.character(pubnum), list(Author)]$Author)))
	    
	} else if (length(pubnum) > 1) {
		message("More than one match; please use exact author name from list below.")
		results <- dataset.dt[as.character(pubnum), list(Author, Title, Pub.number)]
		print(results)

		a <- as.integer(readline("Search again using row number... "))
		pubnum <- results[a, Pub.number]
		print(noquote(paste("$author: ", results[a, Author])))
	}
    
    if( !is.null(subset_name) && !(pubnum %in% get(subset_name)$Pub.number)) {
        warning("Specified dissertation is not in the subset", subset_name)
    }
    
    print(get.topics4doc(pubnum=pubnum, 
                         dataset_name = dataset_name, 
                         ntopics = ntopics, 
                         iter_index = iter_index, 
                         howmany = howmany, 
                         showlabels = showlabels))
}

if(autorun) {
	get.topics4author("MUELLER, DEREK")
	get.topics4author("Lucas")
	get.topics4author(authorname = "Adam Lawrence", dataset_name = "noexcludes2001_2015",
	                  ntopics = 50,
	                  iter_index = 1)
}

