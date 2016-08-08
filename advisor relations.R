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
    r <- paste(s[2], s[1], collapse=" ")
    r <- titleCase(r)
}

save_relations <- function(dataset_name) {
    dataset <- get(dataset_name)
    a <- dataset[,c("Pub.number", "Author", "Advisor.Name", "School")]
    a$Author <- sapply(a$Author, unreverseName)
    
    write.csv(a, file = paste0(dataloc, "advisor_relations_", dataset_name, ".csv"))
}
