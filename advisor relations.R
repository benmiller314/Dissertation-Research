# Goal: fix author name so it's first-last and name case;
# output some basic info to begin building an edge table of authors and advisors

titleCase <- function(x) {
    x <- tolower(x)
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}


unreverseName <- function(x) {
    x <- as.character(x)
    s <- strsplit(x, ", ")[[1]]
    # To do: account for more than one comma, e.g. the case of suffixes
    if (length(s) < 2) {
        return(titleCase(s))
    } else {
        r <- paste(s[2], s[1], collapse=" ")
        return(titleCase(r))
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

namepart <- function(name, part=c("first", "last", "list")) {
    name <- unreverseName(reverseName(unreverseName(name)))
    namelist <- strsplit(name, " ")[[1]]
    if (part == "list") {
        return(namelist)
    } else if (part == "first") {
        return(head(namelist, 1))
    } else if (part == "last") {
        return(tail(namelist, 1))
    } else {
        warning("Only 'first', 'last', and 'list' parts are implemented for namepart();",
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
