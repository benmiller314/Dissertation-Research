########
#
# `update realconsorts.R`: Runs during `dataprep 2 - load data.R`. Adds a column
#  to noexcludes as an index to realconsorts, i.e. real consortium program 
#  dissertations. If new alumni lists become available, update the file in line
#  37. For programs without such lists, manually verify the departments/programs 
#  of dissertations at consortium schools and update the file in line 31.
#
###


# TODO: Use alumni lists to fill in as many gaps as possible
# PLAN:
#       1. start with consorts with unknown departments
#       2. match the department and name on a separate spreadsheet that Alyssa's making
#       2a. if the name is an exact match, continue
#       2b. else match last name and prompt to either continue or skip
#       3. for matches,
#       3a. set department to whatever the spreadsheet has
#       3b. set unknown or 0 realconsorts to 1
#       4. for nonmatches, if realconsorts == 1, prompt to keep or reset to 0

realconsorts_by_list <- function(dataset_name = "consorts",
                                 manual_file = NULL,      # a file.path to a csv department-gathering
                                 alumni_file = NULL,      # a file.path to a list of alumni
                                 matchlist_file = NULL)    # a file.path to save/load matched rows
{
    dataset <- get(dataset_name)
    
    if (is.null(manual_file)) {
        manual_file <- file.path(dataloc, "department-gathering2.csv")
    }
    
    if (is.null(alumni_file)) {
        # NB: this file assembled by Alyssa Rodriguez from public alumni lists on departmental websites
        alumni_file <- file.path(newdataloc, "known consortium graduates 2017-07-30.csv")
    }
    if (is.null(matchlist_file)) {
        matchlist_file <- file.path(newdataloc, "realconsorts from alumni lists.csv")
    }
    
    if (file.exists(manual_file)) {
        # get the department-matching data
        a <- read.csv(manual_file)
        
        # read out just the Pub.numbers from confirmed dissertations in Consortium programs
        confirmed_yes.index <- a[which(a$Consortium == "yes"), "Pub.number"]
        confirmed_no.index <- a[which(a$Consortium == "no"), "Pub.number"]
        
        # use those numbers to update our index
        dataset[dataset$Pub.number %in% confirmed_yes.index, "realconsort"] <- 1 
        dataset[dataset$Pub.number %in% confirmed_no.index, "realconsort"] <- 0
        
        # sanity check: confirm that we're only getting dissertations at Consortium schools
        if (all(dataset[dataset$Pub.number %in% confirmed_yes.index, "School"] %in% conschools)) {
            message(paste("Found", length(confirmed_yes.index), "dissertations manually confirmed from Consortium programs."))
        } else {
            warning("`update realconsorts.R` indexes non-Consortium schools. Time to debug!")
        }
    } else {
        warning("`update realconsorts.R`: couldn't find csv of manually confirmed Consortium dissertations:\n", manual_file)
    }
    
    if (file.exists(alumni_file)) {
        # Get spreadsheet of alumni, from public departmental lists
        alumni_list <- read.csv(alumni_file)
        # View(alumni_list)
        alumni_list$Lastname <- sapply(alumni_list$Name, function(x) namepart(x, "last"))
        alumni_list$Firstname <- sapply(alumni_list$Name, function(x) namepart(x, "first"))
        alumni_list <- alumni_list[order(alumni_list$Consortium_School, alumni_list$Lastname, alumni_list$Firstname),]
        alumni_list <- alumni_list[, c("Consortium_School", "Name", "Department", "Lastname", "Firstname", "Alumni_List")]
        # View(alumni_list)
        
        
        # Narrow to just Consortium schools, then 
        unknown_dept <- dataset[which(dataset$School %in% conschools),]
        # get list of those alumni with unknown departments
        unknown_dept <- dataset[which(dataset$Department %in% c("?", "")), c("Author", "School", "Department", "realconsort", "Pub.number", "Title")]
        unknown_dept$Lastname <- sapply(unknown_dept$Author, function(x) namepart(x, "last"))
        unknown_dept$Firstname <- sapply(unknown_dept$Author, function(x) namepart(x, "first"))
        unknown_dept <- unknown_dept[order(unknown_dept$School, unknown_dept$Author),]
        # View(unknown_dept)
        
        
        # Use merge to find matches
        matching <- merge(unknown_dept, alumni_list, by.x=c("School", "Lastname"), by.y=c("Consortium_School", "Lastname"))
        matching <- matching[, c("School", "Lastname", "Firstname.x", "Firstname.y", "Author", "Name", "Department.y", "realconsort", "Department.x", "Alumni_List", "Pub.number", "Title")]
        # View(matching)
        
        # Helper function: Update exact matches, or prompt to confirm
        update_from_list <- function(merged_list=matching) {
            matchlist <- data.frame("Pub.number"="", "Department"="", stringsAsFactors = FALSE)
            response <- ""
            for (i in 1:nrow(merged_list)) {
                if (merged_list[i, "Firstname.x"] == merged_list[i, "Firstname.y"]) {
                    matchlist[i, "Pub.number"] <- as.character(merged_list[i, "Pub.number"])
                    matchlist[i, "Department"] <- as.character(merged_list[i, "Department.y"])
                } else {
                    while (! tolower(response) %in% c("y", "n", "a")) {
                        print(merged_list[i, c("Author", "Name", "School", "Title", "Pub.number")])
                        response <- readline("Is this a match? ([Y]es / [N]o / [A]bort) > ")
                    
                        if (tolower(response) == "y") {
                            matchlist[i, "Pub.number"] <- as.character(merged_list[i, "Pub.number"])
                            matchlist[i, "Department"] <- as.character(merged_list[i, "Department.y"])
                            response <- ""
                            break()
                        } else if (tolower(response) == "n") {
                            response <- ""
                            break()
                        } else if (tolower(response) == "a") {
                            warning("Process aborted; matchlist incomplete.")
                            return(matchlist)
                        } else {
                            message("I didn't get that.")    
                        }
                    }      # end of while loop (y/n/a prompt)
                }
            }       # end of for loop
            return(matchlist)
            
        }       # end of function update_from_list()
        # debug(update_from_list)
        
        if (remake_figs) {
            # Run helper function
            matchlist <- update_from_list()
            matchlist <- matchlist[which(!is.na(matchlist$Pub.number)),]    
            
            # save that list!
            write.csv(matchlist, file=matchlist_file)
        } else {
            # load from previously saved list
            if (file.exists(matchlist_file)) {
                matchlist <- read.csv(matchlist_file, stringsAsFactors=FALSE)   
                matchlist <- matchlist[which(!is.na(matchlist$Pub.number)),]    
            } else {
                warning("Could not locate previously saved matched list ",
                        "of realconsorts; defaulting to manual entry.")
                matchlist <- update_from_list()
                matchlist <- matchlist[which(!is.na(matchlist$Pub.number)),]    
            }
        }
        
        # merge newly matched data into the dataset
        ## NB: Straight merge doesn't work because dataset$Department is a factor
        # dataset <- merge(dataset, matchlist, all.x=T)
        
        dataset$Department <- as.character(dataset$Department)
        for (i in 1:nrow(matchlist)) {
            index <- which(dataset$Pub.number %in% matchlist[i, "Pub.number"]) 
            dataset[index, "Department"] <- matchlist[i, "Department"]
            dataset[index, "realconsort"] <- 1
        }
        dataset$Department <- as.factor(dataset$Department)
        
        ## check results
        # dataset[which(dataset$realconsort == 1), c("School", "Department", "realconsort")]
        
        # sanity check: confirm that we're only getting dissertations at Consortium schools
        if (all(dataset[which(dataset$realconsort == 1), "School"] %in% conschools)) {
            message(paste("Found", nrow(matchlist), "dissertations confirmed from Consortium program alumni lists."))
        } else {
            warning("`update realconsorts.R` indexes non-Consortium schools. Time to debug!")
        }
        
    } else {
        warning("`update realconsorts.R`: Could not load Consortium program alumni file: \n", alumni_file)
    }
    
    return(dataset)
    
}

## run when the file is sourced
# debug(realconsorts_by_list)
# noexcludes <- realconsorts_by_list("noexcludes")

