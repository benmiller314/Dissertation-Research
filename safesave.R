# if I start to save a file and it already exists, ask me first if I want to overwrite
safesave <- function(savefunction,  # how to save 
                     value,        # what to save
                     filename,     # where to save
                     ...           # additional params for savefunction
                     
) {
    
    if(file.exists(filename)) {
        overwrite <- readline(paste0("File `", filename, "` already exists. Overwrite? (Y/N)  "))
        while(!tolower(substr(overwrite, 1, 1)) %in% c("y", "n")) {
            overwrite <- readline("Sorry, I didn't get that. Overwrite `", filename, "`? (Y/N)  ")
        }
        
        if (overwrite == 'y') {
            savefunction(value, filename, ...)
            message("saved ", substitute(value), " to ", filename)
        } else if (overwrite == 'n') {
            newname <- readline("Enter new filename, including path: ")
            tryCatch(safesave(savefunction, value, newname, ...),
                     error = function(e) e,
                     finally = "done."
            )
        }
        
    } else {
        savefunction(value, filename, ...)
        message("saved ", substitute(value), " to ", filename)
    }
}
