# Goal: fix author name so it's first-last and name case;
# output some basic info to begin building an edge table of authors and advisors

titleCase <- function(x) {
    x <- tolower(x)
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}


unreverseName <- function(x, use.title.case = F) {
    x <- as.character(x)
    s <- strsplit(x, ", ")[[1]]
    # To do: account for more than one comma, e.g. the case of suffixes
    if (length(s) < 2) {
        return(ifelse(use.title.case, titleCase(s), s))
    } else {
        r <- paste(s[2], s[1], collapse=" ")
        return(ifelse(use.title.case, titleCase(r), r))
    }
}

reverseName <- function(x) {
    x <- as.character(x)
    # To do: account for a comma, e.g. the case of suffixes
    s <- strsplit(x, " ")[[1]]
    last <- tail(s, 1)
    first <- paste(head(s, -1), collapse=" ")
    paste0(last, ", ", first)
}

namepart <- function(name, part=c("first", "last", "list", "all"), 
                     all.joiner=" ", 
                     casefunction=toupper) {
    if (length(name) < 1) {
        return(name)
    }
    
    name <- casefunction(unreverseName(reverseName(unreverseName(name))))
    namelist <- strsplit(name, " |,")[[1]]
    if (part == "list") {
        return(namelist)
    } else if (part == "first") {
        return(head(namelist, 1))
    } else if (part == "last") {
        return(tail(namelist, 1))
    } else if (part == "all") {
        return(paste(namelist, collapse=all.joiner))
    } else {
        warning("Only 'first', 'last', 'list', and 'all' parts are implemented for namepart();",
                " name returned unchanged.")
        return(name)
    }
}


save_relations <- function(dataset_name) {
    dataset <- get(dataset_name)
    a <- dataset[,c("Pub.number", "Author", "Advisor.Name", "School")]
    a$Author <- sapply(a$Author, unreverseName)
    
    write.csv(a, file = paste0(dataloc, "advisor_relations_", dataset_name, ".csv"))
}



find_person_in_column <- function(person_name, 
                                  dataset_name = "knownprograms2001_2015", 
                                  search_column = "Author") {
    require(data.table)
    dataset.dt <- data.table(get(dataset_name), key="Pub.number")
    
    if (!exists("namepart", mode="function")) { source(file = "advisor relations.R") }
    person_name <- namepart(person_name, part = 'all', casefunction = titleCase)
    
    person_index <- c()
    for (i in seq_along(search_column)) {
        to_search <- sapply(unlist(dataset.dt[, .SD, .SDcols=search_column[i]]), 
                            namepart, part= 'all', casefunction=titleCase)
        
        # to maximize hits, search the name by parts
        tryCatch(expr = {
                person_names <- namepart(person_name, "list", casefunction=titleCase)
                for (name in person_names) {
                    person_index <- c(person_index, grep(name, to_search, ignore.case = T))
                }
            },
            error = paste0("No match for person", person_name, "; try just last name or first name."),
            finally = "No exact match found; building list from space-separated name parts."
            )
    } # end of for loop; repeat for all search_column s to build up person_index
    
        
    # get Pub.numbers for every row in person_index, deduplicate
    pubnum <- as.character(unique(dataset.dt[person_index, Pub.number]))

    
    if(length(pubnum) < 1) {
        stop("find_person_in_column: No match for person '", person_name, 
             "' in column '", search_column, "'; try just last name or first name.")
        
    } else if (length(pubnum) > 1) {
        message("More than one match:")
        results <- dataset.dt[pubnum, .SD, .SDcols=c(search_column, "Title", "Year",
                                                     "School", "Pub.number")]
        setkeyv(results, search_column)
        print(results)
        
        message("Please select desired matches for ", person_name, " from list above. \n",
                "To select multiple, use c(1, 2) or 1:3 notation; to select none, enter 0.")
            
        a <- readline("Use result number... ")
        a <- eval(parse(text = a))
        pubnum <- as.character(results[a, Pub.number])
    }
    
    # NB: also returns if there was exactly one match
    return(dataset.dt[pubnum])
}

# # Test values:
# test1 <- find_person_in_column("Collin Brooke", dataset_name = "noexcludes", search_column = c("Advisor"))
# test2 <- find_person_in_column("Collin Brooke", dataset_name = "noexcludes", search_column = c("Advisor.Name"))
# test3 <- find_person_in_column("Collin Brooke", dataset_name = "noexcludes", search_column = c("Advisor", "Advisor.Name"))


