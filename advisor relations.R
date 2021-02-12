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
    
    to_search <- sapply(unlist(dataset.dt[, ..search_column]), namepart, part= 'all', casefunction=titleCase)
    
    person_index <- grep(person_name, to_search, ignore.case = T)
    
    if (length(person_index)) {
        pubnum <- as.character(unique(dataset.dt[person_index, Pub.number]))
    } else {
        tryCatch(expr = {
            
            person_names <- namepart(person_name, "list")
            person_index <- c()
            for (name in person_names) {
                person_index <- c(person_index, grep(name, to_search, ignore.case = T))
            }
            pubnum <- as.character(unique(dataset.dt[person_index, Pub.number]))
        },
        e = "No match for that person; try just last name or first name.",
        finally = "No exact match found; building list from space-separated name parts."
        )
    }
    
    if(length(pubnum) < 1) {
        stop("find_person_in_column: No match for person '", person_name, 
             "' in column '", search_column, "'; try just last name or first name.")
        
    } else if (length(pubnum) > 1) {
        message("More than one match; please try an exact person name from list below.")
        results <- dataset.dt[pubnum, .SD, .SDcols=c(search_column, "Title", "Year",
                                                     "School", "Pub.number")]
        print(results)
        
        # TO DO: Allow for the case of no correct matches (i.e. skip)
        a <- as.integer(readline("Use result number... "))
        pubnum <- as.character(results[a, Pub.number])
    }
    
    return(dataset.dt[pubnum])
}

