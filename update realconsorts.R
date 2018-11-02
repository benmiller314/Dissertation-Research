########
#
# `update realconsorts.R`: Runs during `dataprep 2 - load data.R`. Adds a column
#  to noexcludes as an index to realconsorts, i.e. real consortium program 
#  dissertations. If new alumni lists become available, update the file in line
#  47. For programs without such lists, manually verify the departments/programs 
#  of dissertations at consortium schools and update the file in line 40.
#
###


# TODO: Use alumni lists to fill in as many gaps as possible
# PLAN:
#       1. start with consorts with unknown departments
#       2. match the department and name on a separate spreadsheet that Alyssa's making
#          2a. if the name is an exact match, continue
#          2b. else match last name and prompt to either continue or skip
#       3. for matches,
#          3a. set department to whatever the spreadsheet has
#          3b. set unknown or 0 realconsorts to 1
#       4. for nonmatches, if realconsorts == 1, prompt to keep or reset to 0
#
# UPDATE 2018-07-05 
# GOAL: Avoid having to re-match and -verify all the realconsorts added previously
# PLAN:
#       1. always allow users to specify specific new schools to add - not only from alumni lists
#       2. before adding realconsorts/realrhetmaps columns, check to see if they exist
#               Never mind: I'm adding them only implicitly (by reference), so this is already true
#       3. if remake_figs isn't True, prompt with option to turn it on temporarily
#       4. prompt to save output to new file or overwrite existing file

if(!exists("namepart", mode="function")) {
    source(file=file.path(sourceloc, "advisor relations.R"))
}

# Helper function: filter by school
filter_realconsorts <- function(updata,                # data in file to update from
                                schools,               # which schools to filter to?
                                school_col = "School") # relevant column name
                                {
    if (!is.null(schools)) {
        message("`update realconsorts.R`: I see the following list of schools:\n")
        print(schools)
        filter_option <- readline("Update realconsorts only for these schools (S), all schools (A), or none (N)?  > ")
        
        while(length(filter_option) > 0) {
            if (tolower(filter_option) == "n") { 
                message("Canceling update of realconsorts.")
                warning("update_realconsorts called but canceled by user.")
                return(updata)
            } else if (tolower(filter_option) == "s") {
                updata <- updata[which(updata[[school_col]] %in% schools),]
                break()
            } else if (tolower(filter_option) == "a") {
                break()
            } else {
                message("I didn't understand. I see the following list of schools: \n")
                print(paste(schools, sep="\n"))
                filter_option <- readline("Update realconsorts only for these schools (S), all schools (A), or none (N)?  > ")
            }
        } 
        # message("The value of 'filter_option' is: ", filter_option)
        # message("Exiting while loop")
    } else {
        message("`update realconsorts.R`: No schools entered to filter by, so continuing with all schools in the dataset.")
    }
    
    message("Preparing to work with ", nrow(updata), " rows of updated data...")
    return(updata)
}


realconsorts_by_list <- function(dataset_name = "noexcludes",
                                 manual_file = NULL,     # a set of file.paths to department-gathering csvs 
                                 alumni_file = NULL,      # a file.path to a list of known alumni
                                 matchlist_file = NULL,   # a file.path to save/load matched rows from alumni_file
                                 output_file = NULL,      # a file.path to export the full dataset after matching
                                 schools = NULL)          # a list of new schools to filter for matching/adding
{
    dataset <- get(dataset_name)
    
    
    ### Set defaults ###
    if (is.null(manual_file)) {
        # NB: this file assembled by me and Moriah Purdy in spring of 2016, based on dissertations
        # and some limited searching for public CVs; 
        # updated by Janetta Brundage and Michelle Hillock in spring 2018
        # manual_file <- file.path(dataloc, "department-gathering2.csv")        # previous file
        manual_file <- file.path(dataloc, "department-gathering-6.csv")
    }
    
    if (is.null(alumni_file)) {
        # NB: this file assembled by Alyssa Rodriguez in summer 2017 
        # from public alumni lists on departmental websites, and 
        # updated by Janetta Brundage and Michelle Hillock in spring 2018
        # alumni_file <- file.path(newdataloc, "known consortium graduates 2017-07-30.csv")  # older file
        alumni_file <- file.path(newdataloc, "known consortium graduates 2018-07-02.csv")
    }
    
    if (is.null(matchlist_file)) {
        matchlist_file <- file.path(newdataloc, "realconsorts from alumni lists.csv")
    }
    
    if (is.null(output_file)) {
        output_file <- file.path(dataloc, paste0(dataset_name, " with realconsorts updated ", Sys.Date(), ".csv"))
    }
    
    if (is.null(schools)) {
        # prompt for schools to filter on if they're not already specified
        message("`update realconsorts.R`: No schools entered to filter by; are you sure you want to update all of them? \n")
        add <- readline("To add a school to the filter, type it now (must be exact Carnegie name), or press Enter to skip: \n")
        
        while (nchar(add) > 0) {
            # TO DO: handle removal of items from the list
            if (grepl("[[:digit:]]+", add)) {
                if (as.numeric(add) < 0 || as.numeric(add) > length(schools)) {
                    message("`update realconsorts.R`: ERROR: To remove a school from the filter, enter a positive integer number from the array.")
                } else {
                    schools <- schools[-as.numeric(add)]
                }
            } else {
                schools <- c(schools, add)
            }
            message("`update realconsorts.R`: I see the following list of schools: ")
            print(schools)
            add <- readline(paste("To add a school to the filter, enter it now (must be exact Carnegie name); \n",
                                  "to remove a school, enter its number in the array; \n",
                                  "or leave blank and press Enter to continue: \n"))
        }
    }
    
    
    ### Individually located departments: no need to match again, just read out 
    message("`update realconsorts.R`: File with *individually located* dissertations is currently: ", manual_file)
    do_manual <- readline("Update realconsorts from this file? (y/n)  ")
    if (tolower(substr(do_manual, 1, 1)) != "n" & file.exists(manual_file)) {
        # get the department-matching data
        a <- read.csv(manual_file)
        
        # optionally filter by school
        a <- filter_realconsorts(a, schools)
         
        # read out just the Pub.numbers from confirmed dissertations in Consortium/rhetmap programs
        confirmed_consort.index <- a[which(a$Consortium_program == 1), "Pub.number"]
        confirmed_nonconsort.index <- a[which(a$Consortium_program == 0), "Pub.number"]
        confirmed_rhetmap.index <- a[which(a$rhetmap_program == 1), "Pub.number"]
        confirmed_nonrhetmap.index <- a[which(a$rhetmap_program == 0), "Pub.number"]
        
        # use Pub.numbers to update our set of realconsorts / realrhetmaps
        
        dataset[dataset$Pub.number %in% confirmed_consort.index, "realconsort"] <- 1 
        dataset[dataset$Pub.number %in% confirmed_nonconsort.index, "realconsort"] <- 0
        dataset[dataset$Pub.number %in% confirmed_rhetmap.index, "realrhetmap"] <- 1 
        dataset[dataset$Pub.number %in% confirmed_nonrhetmap.index, "realrhetmap"] <- 0
        
        # sanity check: confirm that we're only getting dissertations at Consortium schools
        if (all(dataset[which(dataset$Pub.number %in% union(confirmed_consort.index, confirmed_rhetmap.index)), "School"] %in% union(conschools, rhetmapschools))) {
            message(paste("Found", length(confirmed_consort.index), "dissertations manually confirmed from Consortium programs",
                          "and", length(confirmed_rhetmap.index), "from programs listed on rhetmap.org." ))
        } else {
            warning("`update realconsorts.R` indexes non-Consortium schools. Time to debug!")
        }
        
    } else if (!file.exists(manual_file)) {
        warning("`update realconsorts.R`: couldn't find csv of manually confirmed Consortium dissertations:\n", manual_file)
    }
    
    ### Alumni lists: these will need to be screened and confirmed.
    # We have to at least generate the list of unconfirmed departments, for subsetting
    if (file.exists(alumni_file)) {
        # Get spreadsheet of alumni, from public departmental lists
        alumni_list <- read.csv(alumni_file)
        
        # report on the number of schools indexed
        message("Found alumni lists for ", 
                length(levels(alumni_list$Consortium_School)), 
                " of ", length(conschools), " schools in the Consortium.")
        
        # which schools don't we have lists for? (maybe contact them...)
        no_alumni_list <<- levels(factor(conschools[which(!conschools %in% alumni_list$Consortium_School)]))
        message("Schools without known alumni lists can be accessed with the variable `no_alumni_list`")
        if(remake_figs) {
            filename <- file.path(dataloc, paste0("desired_alumni_lists_",Sys.Date(),".csv"))
            write.csv(no_alumni_list, file=filename)
            message("and in the file ", filename, ".")
        }
        
        ## re-do the following report only at the end of the update; 
        ## otherwise we'll accidentally report back manually confirmed disses as still missing
        # 
        # no_alumni_disses <<- dataset[which(dataset$School %in% no_alumni_list), ]
        # if(exists("confirmed_yes.index")) {
        #     no_alumni_disses <<- no_alumni_disses[-which(no_alumni_disses$Pub.number %in% confirmed_yes.index),]
        #     no_alumni_disses <<- no_alumni_disses[-which(no_alumni_disses$Pub.number %in% confirmed_no.index),]
        # }
        # 
        # if(remake_figs) {
        #     filename <- file.path(dataloc, paste0("unconfirmed_consorts_",Sys.Date(),".csv"))
        #     message(paste("Saving consortium-school dissertations without known department status to",
        #                   filename))
        #     write.csv(no_alumni_disses, file=filename)
        # } else {
        #     # View(no_alumni_disses)
        #     message(paste("Dissertations from Consortium schools but unconfirmed departments can be accessed",
        #                   "from the variable `no_alumni_disses`; to export, set remake_figs to TRUE", 
        #                   "before running `update realconsorts` (called in `dataprep2`)."))
        # }
        
        message("`update realconsorts.R`: File with *alumni lists* is currently: \n", alumni_file)
        do_alumni <- readline("Match realconsorts from this file? (y/n)  ")
        if (tolower(substr(do_alumni, 1, 1)) == 'y') {
            
            # Optionally filter by schools
            alumni_list <- filter_realconsorts(alumni_list, schools, school_col = "Consortium_School")
            
            
            # View(alumni_list)
            alumni_list$Lastname <- sapply(alumni_list$Name, function(x) namepart(x, "last"))
            alumni_list$Firstname <- sapply(alumni_list$Name, function(x) namepart(x, "first"))
            alumni_list <- alumni_list[order(alumni_list$Consortium_School, alumni_list$Lastname, alumni_list$Firstname),]
            alumni_list <- alumni_list[, c("Consortium_School", "Name", "Department", "Lastname", "Firstname", "Alumni_List")]
            # View(alumni_list)
            
                    # Narrow to just Consortium schools, then... 
            unknown_dept <- dataset[which(dataset$School %in% union(conschools, rhetmapschools)),]
            # ... get list of those alumni with unknown departments
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
                # start empty
                matchlist <- data.frame("Pub.number"="", "Department"="", stringsAsFactors = FALSE)
                response <- ""
                if (nrow(merged_list > 0)) {             # avoid errors caused by over-filtering
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
                }       # end of if statement            
                matchlist <- matchlist[which(!is.na(matchlist$Pub.number)),]    
                return(matchlist)
                
            }       # end of function update_from_list()
            # debug(update_from_list)
        
            
            if (update_realconsorts & is.null(schools)) {
                # Remake the match list from scratch
                matchlist <- update_from_list()
            } else {
                # load from previously saved list
                if (file.exists(matchlist_file)) {
                    matchlist <- read.csv(matchlist_file, stringsAsFactors=FALSE)
                    
                    # add new schools to that list, if that's what we're doing
                    if(!is.null(schools)) {
                        addendum <- update_from_list()
                        matchlist <- merge(matchlist, addendum) # was rbind, but I worry about column order
                    }
                } else {
                    warning("Could not locate previously saved matched list ",
                            "of realconsorts at ", matchlist_file,"; defaulting to manual confirmation of matches.")
                    matchlist <- update_from_list()
                }
            
            }
            
            # optionally (but encouragedly) overwrite the file
            if (!remake_figs) {
                fix_fig <- readline(paste("remake_figs is currently set to FALSE. Save file anyway \n",
                        "to avoid verifying all these matches again? (y to save)"  ))
                if(tolower(substr(fix_fig, 1, 1)) == "y") {
                    message("Saving consortium program matches to file: ", paste0(matchlist_file, Sys.Date()), "... ")
                    write.csv(matchlist, file=paste0(matchlist_file, Sys.Date()), row.names=FALSE)
                    message("Done.")
                }
            } else {
                message("Saving consortium program matches to file: ", paste0(matchlist_file, Sys.Date()), "... ")
                write.csv(matchlist, file=paste0(matchlist_file, Sys.Date()), row.names=FALSE)
                message("Done.")
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
        } # end of if (do_alumni=y)
        
    } else if (!file.exists(alumni_file)) {
        warning("`update realconsorts.R`: Could not load Consortium program alumni file: \n", alumni_file)
    }
    
    # optionally (but encouragedly) save an updated file of the full dataset
    if (!remake_figs) {
        fix_fig <- readline(paste("remake_figs is currently set to FALSE. Save updated ", dataset_name, 
                                  "\n file anyway to avoid verifying all these matches again? (y to save)"  ))
        if(tolower(substr(fix_fig, 1, 1)) == "y") {
            message("Saving ", dataset_name, " with updated realconsorts to file: ", output_file, "...")
            write.csv(dataset, file=output_file, row.names=FALSE)
            message("Done.")
        } else {
            message("Okay, not writing to file then.")
        }
    } else {
        message("Saving ", dataset_name, " with updated realconsorts to file: ", output_file, "...")
        write.csv(dataset, file=output_file, row.names=FALSE)
        message("Done.")
    }
    
    return(dataset)
    
}

## debugging: run when the file is sourced
# debug(realconsorts_by_list)
# noexcludes <- realconsorts_by_list("noexcludes")

realconsorts_ratios <- function(dataset_name = "consorts",
                                 alumni_file = NULL      # a file.path to a list of alumni
) {
    dataset <- get(dataset_name)
    
    if (is.null(alumni_file)) {
        # NB: this file assembled by Alyssa Hernandez from public alumni lists on departmental websites
        # alumni_file <- file.path(newdataloc, "known consortium graduates 2017-07-30.csv")  # older file
        alumni_file <- file.path(newdataloc, "known consortium graduates 2018-07-02.csv")
    }
    
    if (file.exists(alumni_file)) {
        # Get spreadsheet of alumni, from public departmental lists
        alumni_list <- read.csv(alumni_file)
        names(alumni_list)
        schools_known_alums <- levels(alumni_list$Consortium_School)
        
        known_subset <- dataset[which(dataset$School %in% schools_known_alums), c("School", "realconsort")]
        consort_count <- aggregate(known_subset$realconsort, by=list(known_subset$School), FUN=function(s) {
            sum(s, na.rm=T)
        })
        
        total <- aggregate(known_subset$realconsort, by=list(known_subset$School), FUN=function(s) {
            length(s)
        })
        
        mytab <- merge(consort_count, total, by = "Group.1")
        names(mytab) <- c("School", "Consortium", "Total")
        mytab$Ratio <- mytab$Consortium / mytab$Total     
        mytab <- mytab[order(mytab$Ratio, decreasing=T), ]
        
        return(mytab)
    }
}    
# summary(realconsorts_ratios()$Ratio)
