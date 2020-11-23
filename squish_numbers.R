# system-wide functions to easily store and retrieve numeric vectors as one observation in a data-frame

squish <- function(numbervec) {
    if(!is.numeric(numbervec)) {
        warning("This is supposed to take a numeric vector and store it temporarily as text", 
                " for storage in a data.frame. The variable passed (below) was not a numeric vector.", 
                numbervec)
    }
    return(paste(numbervec, collapse=" "))
}

stretch <- function(numbervec) {
    if(!is.character(numbervec)) {
        warning("This is supposed to take a numeric vector stored temporarily as text,",
                " for storage in a data.frame, and make it numeric again. ",
                "The variable passed (below) was not a character string.",
                numbervec)
    }
    return(as.numeric(strsplit(numbervec, " ")[[1]]))
}
    