# ACH Presentation

# GOAL: find interesting vocab differences / choices among synonyms across related but distinct topics

# PLAN:
#       1a. Create mini subcorpora by setting a minimum level of a given topic
#       1b. If time allows, also set maximum levels of a second topic?
#       2.  Given one subcorpus, extract significant phrases based on log-likelihood ratio relative to... what? rest of corpus? COCA-Academic 2001-2015?
#       ALT: Search for the keyword primings (phrases) identified by Dylan Dryer in six major journals. How do they shake out by topic? By school?




# Given a pub.number, read in a text
id = "3007997"
one_text <- function(id, location=file.path(fulltextloc, "clean_noexcludes2001_2015_only")) {
    filename <- file.path(location, paste0("cleaned_", id, ".txt"))
    if( file.exists(filename)) {
        txt <- readLines(filename)
        txt <- scan(filename, what="character", encoding="UTF-8")
    } else {
        
    }
    txt <- tryCatch(readLines(filename),
                    error=function(e) e,
                    finally=message("Read ", filename))
}


# Tokenize the text to 