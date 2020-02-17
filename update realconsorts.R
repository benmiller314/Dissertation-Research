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
        message("filter_realconsorts: I see the following list of schools:\n")
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
        # message("The value of 'filter_option' is: ", filter_option)       # debugging
        # message("Exiting while loop")                                     # debugging
    } else {
        message("filter_realconsorts: No schools entered to filter by, so continuing with all schools in the dataset.")
    }
    
    message("Preparing to work with ", nrow(updata), " rows of updated data...")
    return(updata)
}


realconsorts_by_list <- function(dataset_name = "noexcludes",
                                 manual_file = NULL,      # a set of file.paths to department-gathering csvs 
                                 alumni_file = NULL,      # a file.path to a list of known alumni
                                 matchlist_file = NULL,   # a file.path to save/load matched rows from alumni_file
                                 output_file = NULL,      # a file.path to export the full dataset after matching
                                 schools = NULL,          # a list of new schools to filter for matching/adding
                                 school_col = "School")    # which column has the reconciled school names?
{
    dataset <- get(dataset_name)
    
    
    ### Set defaults ###
    if (is.null(manual_file)) {
        # NB: this file assembled by me and Moriah Purdy in spring of 2016, based on dissertations
        # and some limited searching for public CVs; 
        # updated by Janetta Brundage and Michelle Hillock in spring 2018
        # manual_file <- file.path(dataloc, "department-gathering2.csv")        # previous file
        manual_file <- file.path(dataloc, "department-gathering-7.csv")
    }
    
    if (is.null(alumni_file)) {
        # NB: this file assembled by Alyssa Rodriguez in summer 2017 
        # from public alumni lists on departmental websites, and 
        # updated by Janetta Brundage and Michelle Hillock in spring 2018
        # new info pulled from Consortium member reports by Michael ____ (Eng dept work/study) spring 2019.
        # alumni_file <- file.path(dataloc, "known consortium graduates 2017-07-30.csv")  # older file
        # alumni_file <- file.path(dataloc, "known consortium graduates 2018-07-02.csv")  # older file
        alumni_file <- file.path(dataloc, "consortium-graduates-2019-06-Updated-6_14-reconciled-lastfirst.csv")
    }
    
    if (is.null(matchlist_file)) {
        matchlist_name <- "realconsorts from alumni lists"
        # if you're working on this file, you probably have one from today
        matchlist_file <- file.path(dataloc, paste0(matchlist_name, " ", Sys.Date(), ".csv"))
        # but if not, just load the default
        if(!file.exists(matchlist_file)) {
            matchlist_file <- file.path(dataloc, (paste0(matchlist_name, ".csv")))
        }
    }
    
    if (is.null(output_file)) {
        output_file <- file.path(dataloc, paste0(dataset_name, " with realconsorts updated ", Sys.Date(), ".csv"))
    }
    
    if (is.null(schools)) {
        # prompt for schools to filter on if they're not already specified
        message("realconsorts_by_list: No schools entered to filter by; are you sure you want to update all of them? \n")
        add <- readline("To add a school to the filter, type it now (must be exact Carnegie name), or press Enter to skip: \n")
        
        while (nchar(add) > 0) {
            # TO DO: handle removal of items from the list
            if (grepl("[[:digit:]]+", add)) {
                if (as.numeric(add) < 0 || as.numeric(add) > length(schools)) {
                    message("realconsorts_by_list: ERROR: To remove a school from the filter, enter a positive integer number from the array.")
                } else {
                    schools <- schools[-as.numeric(add)]
                }
            } else {
                schools <- c(schools, add)
            }
            message("realconsorts_by_list: I see the following list of schools: ")
            print(schools)
            add <- readline(paste("To add a school to the filter, enter it now (must be exact Carnegie name); \n",
                                  "to remove a school, enter its number in the array; \n",
                                  "or leave blank and press Enter to continue: \n"))
        }
    }
    
    
    ### Individually located departments: no need to match again, just read out 
    message("realconsorts_by_list: File with *individually located* dissertations is currently: ", manual_file)
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
            warning("realconsorts_by_list indexes non-Consortium schools. Time to debug!")
        }
        
        if(remake_figs) {
            message("Saving ", dataset_name, " with updated realconsorts from *manual* (non-alumni list) matches to file: ", output_file, "...")
            write.csv(dataset, file=output_file, row.names=FALSE, na="")
            message("Done.")
        }
        
    } else if (!file.exists(manual_file)) {
        warning("realconsorts_by_list: couldn't find csv of manually confirmed Consortium dissertations:\n", manual_file)
    }
    
    ### Alumni lists: these will need to be screened and confirmed.
    # We have to at least generate the list of unconfirmed departments, for subsetting
    if (file.exists(alumni_file)) {
        # Get spreadsheet of alumni, from public departmental lists
        alumni_list <- read.csv(alumni_file)
        message("realconsorts_by_list: loading alumni lists from file", alumni_file)
        # report on the number of schools indexed
        message("Found alumni lists for ", 
                length(levels(alumni_list[, school_col])), 
                " of ", length(conschools), " schools in the Consortium.")
        
        # which schools don't we have lists for? (maybe contact them...)
        no_alumni_list <<- levels(factor(conschools[which(!conschools %in% alumni_list[, school_col])]))
        message("Schools without known alumni lists can be accessed with the variable `no_alumni_list`")
        if(remake_figs) {
            filename <- file.path(dataloc, paste0("desired_alumni_lists_",Sys.Date(),".csv"))
            write.csv(no_alumni_list, file=filename, na="")
            message("and in the file ", filename, ".")
        }
        
        # if(exists("confirmed_yes.index")) {
        #     no_alumni_disses <<- no_alumni_disses[-which(no_alumni_disses$Pub.number %in% confirmed_yes.index),]
        #     no_alumni_disses <<- no_alumni_disses[-which(no_alumni_disses$Pub.number %in% confirmed_no.index),]
        # }
        # 
        # if(remake_figs) {
        #     filename <- file.path(dataloc, paste0("unconfirmed_consorts_",Sys.Date(),".csv"))
        #     message(paste("Saving consortium-school dissertations without known department status to",
        #                   filename))
        #     write.csv(no_alumni_disses, file=filename, na="na")
        # } else {
        #     # View(no_alumni_disses)
        #     message(paste("Dissertations from Consortium schools but unconfirmed departments can be accessed",
        #                   "from the variable `no_alumni_disses`; to export, set remake_figs to TRUE", 
        #                   "before running `update realconsorts` (called in `dataprep2`)."))
        # }
        
        message("realconsorts_by_list: File with *alumni lists* is currently: \n", alumni_file)
        do_alumni <- readline("Match realconsorts from this file? (y/n)  ")
        if (tolower(substr(do_alumni, 1, 1)) == 'y') {
            
            # Optionally filter by schools
            alumni_list <- filter_realconsorts(alumni_list, schools, school_col = school_col)
            
            
            # View(alumni_list)
            alumni_list$Lastname <- sapply(alumni_list$Name, function(x) namepart(x, "last"))
            alumni_list$Firstname <- sapply(alumni_list$Name, function(x) namepart(x, "first"))
            alumni_list <- alumni_list[order(alumni_list[, school_col], alumni_list$Lastname, alumni_list$Firstname),]
            # alumni_list <- alumni_list[, c(school_col, "Name", "Department", "Lastname", "Firstname", "Source", "Year")]
            
            
            # View(alumni_list)
            
            # Narrow dataset to just Consortium/Rhetmap schools with unknown program status
            maybeconsort_index <- intersect(which(dataset$School %in% conschools), which(!dataset$realconsort %in% c(0,1)))
            mayberhetmap_index <- intersect(which(dataset$School %in% rhetmapschools), which(!dataset$realrhetmap %in% c(0,1)))
            
            unknown_dept <- dataset[union(maybeconsort_index, mayberhetmap_index), ]
                                    # c("Author", "School", "Department", "realconsort", "Pub.number", "Title", "Year")]
            
            unknown_dept$Lastname <- sapply(unknown_dept$Author, function(x) namepart(x, "last"))
            unknown_dept$Firstname <- sapply(unknown_dept$Author, function(x) namepart(x, "first"))
            unknown_dept <- unknown_dept[order(unknown_dept$School, unknown_dept$Author),]
            # View(unknown_dept)
            
            
            # Use merge to find matches
            matching <- merge(unknown_dept, alumni_list, by.x=c("School", "Lastname"), by.y=c(school_col, "Lastname"))
            matching <- matching[, c("School", "Lastname", "Firstname.x", "Firstname.y", "Year.x", "Year.y", "Author", "Name", "Department.x", "Department.y", "realconsort", "Source", "Pub.number", "Title", "Checked")]
            
            # View(matching)
            
            
            # Helper function: Update exact matches, or prompt to confirm
            update_from_list <- function(merged_list=matching) {
                # start empty
                matchlist <- data.frame("Pub.number"="", "Department"="", stringsAsFactors = FALSE)
                merged_list[which(is.na(merged_list$Checked)), "Checked"] <- ""
                response <- ""
                if (nrow(merged_list > 0)) {             # avoid errors caused by over-filtering
                    i <- 1L                               # use while loop to allow backward movement
                    while (i <= nrow(merged_list)) {
                        if (!is.null(merged_list[i, "Checked"]) & merged_list[i, "Checked"] == "y") {
                            i <- i + 1L
                        } else if (merged_list[i, "Firstname.x"] == merged_list[i, "Firstname.y"]) {
                            matchlist[i, "Pub.number"] <- as.character(merged_list[i, "Pub.number"])
                            matchlist[i, "Department"] <- as.character(merged_list[i, "Department.y"])
                            merged_list[i, "Checked"] <- "y"
                            i <- i + 1L
                        } else {
                            while (! response %in% c("y", "n", "a", "b")) {
                                print(merged_list[i, c("Author", "Year.x", "Name", "Year.y", "School", "Title", "Pub.number")])
                                response <- tolower(substr(readline("Is this a match? ([Y]es / [N]o / [A]bort) > "), 1,1))
                                    
                                if (response == "y") {
                                    matchlist[i, "Pub.number"] <- as.character(merged_list[i, "Pub.number"])
                                    matchlist[i, "Department"] <- as.character(merged_list[i, "Department.y"])
                                    merged_list[i, "Checked"] <- "y"
                                    response <- ""
                                    i <- i + 1L
                                    break()
                                } else if (response == "n") {
                                    merged_list[i, "Checked"] <- "y"
                                    response <- ""
                                    i <- i + 1L
                                    break()
                                } else if (response == "a") {
                                    warning("Process aborted; matchlist incomplete.")
                                    return(matchlist)
                                } else if (response == "b") { 
                                    # to go back, we need not the previous row,
                                    # but the previous row prompting about mismatched first names 
                                    findthis <- matchlist[i-1, "Pub.number"]
                                    i <- min(which(merged_list["Pub.number"] == findthis))
                                    message("going back to previous match-prompt...")
                                    merged_list[i, "Checked"] <- ""
                                    response <- ""
                                    break()
                                } else {
                                    message("I didn't get that.")    
                                }
                            }      # end of inner while loop (y/n/a/b prompt)
                        }
                    }       # end of outer while loop (fake for-loop)
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
                    warning("realconsorts_by_list: Could not locate previously saved matched list ",
                            "of realconsorts at ", matchlist_file,"; defaulting to manual confirmation of matches.")
                    matchlist <- update_from_list()
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
            
            # optionally (but encouragedly) overwrite the file
            if (!remake_figs) {
                fix_fig <- readline(paste("remake_figs is currently set to FALSE. Save ", dataset, " to file anyway \n",
                        "to avoid verifying all these matches again? (y to save)"  ))
                if(tolower(substr(fix_fig, 1, 1)) == "y") {
                    tryCatch(
                        expr = function() {
                            message("Saving ", dataset_name, " with updated realconsorts from *alumni list* matches to file: ", output_file, "...")
                            write.csv(dataset, file=output_file, row.names=FALSE, na="")
                            message("Done.")
                            
                            filename <- file.path(dataloc, paste(matchlist_name, Sys.Date(), ".csv"))
                            message("Saving *alumni* matchlist to file: ", filename, "... ")
                            write.csv(matchlist, file=filename, row.names=FALSE, na="")
                        },
                        error = function(e) {
                            message("Can't save file, so exporting it instead")
                            matchlist_err <<- matchlist
                            warning("update_realconsorts.R: something went wrong in file save, line 338ff")
                        },
                        finally = message("Done.")
                    )
                }
            } else {
                tryCatch(
                    expr = function() {
                        filename <- file.path(dataloc, paste(matchlist_name, Sys.Date(), ".csv"))
                        message("Saving consortium program matches from *alumni lists* to file: ", filename, "... ")
                        write.csv(matchlist, file=filename, row.names=FALSE, na="")
                    },
                    error = function(e) {
                        message("Can't save file, so exporting it instead")
                        matchlist_err <<- matchlist
                        warning("update_realconsorts.R: something went wrong in file save, line 338ff")
                    },
                    finally = message("Done.")
                )
            }
        
           
            ## check results
            # dataset[which(dataset$realconsort == 1), c("School", "Department", "realconsort")]
            
            # sanity check: confirm that we're only getting dissertations at Consortium schools
            if (all(dataset[which(dataset$realconsort == 1), "School"] %in% conschools)) {
                message(paste("Found", nrow(matchlist), "dissertations confirmed from Consortium program alumni lists."))
            } else {
                warning("realconsorts_by_list` indexes non-Consortium schools. Time to debug!")
            }
        } # end of if (do_alumni=y)
        
    } else if (!file.exists(alumni_file)) {
        warning("realconsorts_by_list: Could not load Consortium program alumni file: \n", alumni_file)
    }
    
    # optionally (but encouragedly) save an updated file of the full dataset
    if (!remake_figs) {
        fix_fig <- readline(paste("remake_figs is currently set to FALSE. Save updated ", dataset_name, 
                                  "\n file anyway to avoid verifying all these matches again? (y to save)"  ))
        if(tolower(substr(fix_fig, 1, 1)) == "y") {
            message("Saving ", dataset_name, " with updated realconsorts to file: ", output_file, "...")
            write.csv(dataset, file=output_file, row.names=FALSE, na="")
            message("Done.")
        } else {
            message("Okay, not writing to file then.")
        }
    } else {
        message("Saving ", dataset_name, " with updated realconsorts to file: ", output_file, "...")
        write.csv(dataset, file=output_file, row.names=FALSE, na="")
        message("Done.")
    }
    
    # Redo index of possible but unknown consortium/rhetmap disses
    maybeconsort_index <- intersect(which(dataset$School %in% conschools), which(!dataset$realconsort %in% c(0,1)))
    mayberhetmap_index <- intersect(which(dataset$School %in% rhetmapschools), which(!dataset$realrhetmap %in% c(0,1)))
    
    # Make this available outside the function
    maybeconsorts <<- dataset[union(maybeconsort_index, mayberhetmap_index), ]
    message("Disses at consortium/rhetmap schools but with unconfirmed program status can be accessed as maybeconsorts.")
    if(remake_figs) {
        write.csv(maybeconsorts, file=file.path(dataloc, paste0("maybeconsorts_", Sys.Date(), ".csv")), row.names=F, na="")
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
        # alumni_file <- file.path(dataloc, "known consortium graduates 2017-07-30.csv")  # older file
        alumni_file <- file.path(dataloc, "known consortium graduates 2018-07-02.csv")
    }
    
    if (file.exists(alumni_file)) {
        # Get spreadsheet of alumni, from public departmental lists
        alumni_list <- read.csv(alumni_file)
        names(alumni_list)
        schools_known_alums <- levels(alumni_list[, school_col])
        
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

if(FALSE) {     # for testing; will never run on its own
    realconsorts_by_list()
}



# I don't know how it happened, but somehow all the above somehow managed to miss 
# a bunch of dissertations for which I knew the department, but hadn't labeled as
# realconsort or realrhetmap. I processed them in OpenRefine. This function will 
# copy the new realconsort/realrhetmap values from that spreadsheet into noexcludes
# and re-export. (There were still 358 dissertations at rhetmap schools for which
# I still don't have departments.) --Ben 2019-07-01

refine_consorts <- function(dataset_name, 
                            sourcefile=file.path(dataloc, "maybeconsorts_2019-07-01_refined-take-2.csv"),
                            output_file = NULL      # a file.path to export the full dataset after matching
                            ){
    
    if(file.exists(sourcefile)) {
        
        if (is.null(output_file)) {
            output_file <- file.path(dataloc, paste0(dataset_name, " with realconsorts updated ", Sys.Date(), ".csv"))
        }
        
        dataset <- get(dataset_name)
    
        sourceset <- read.csv(sourcefile)
        for(i in 1:nrow(sourceset)) {
            dataset[which(dataset$Pub.number == sourceset[i, "Pub.number"]), "realconsort"] <- sourceset[i, "realconsort"]
            dataset[which(dataset$Pub.number == sourceset[i, "Pub.number"]), "realrhetmap"] <- sourceset[i, "realrhetmap"]
        }
        
        message("Updated ", nrow(sourceset), " rows with realconsort/realrhetmap values from ", sourcefile)
        
        if(remake_figs) {
            message("Saving ", dataset_name, " with updated realconsorts from openrefined matches to file: ", output_file, "...")
            write.csv(dataset, file=output_file, row.names=FALSE, na="")
            message("Done.")
        }
        
        return(dataset)
    }
}

if(FALSE) {    # This should be a one-time operation
    noexcludes <- refine_consorts("noexcludes")
}
