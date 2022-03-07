# modified from https://stackoverflow.com/questions/15057769/treat-na-as-zero-only-when-adding-a-number
na_plus <- function(x, y) {
    if(length(x) > 1) {
        result <- sapply(1:max(length(x), length(y)), FUN=function(d) {
            na_plus(x[(d-1) %% length(x)+1], y[(d-1) %% length(y)+1])
        })
    } else if (is.na(x) && is.na(y)) {
        result <- x
    } else {
        result <- sum(x, y, na.rm=TRUE)
    }
    return(result)
}
