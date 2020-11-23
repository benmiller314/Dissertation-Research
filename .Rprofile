# shortcut for retrieving the last entered value
ans <- function() {
	.Last.value
	}

# shortcut for determining what functions are being debugged
debugs <- function() {
    names(which(sapply(unlist(lapply(search(), function(x) lsf.str(pos=x))), isdebugged)))
}

# set timezone
Sys.setenv(TZ='America/New_York') 


# set locale
Sys.setlocale("LC_ALL", "C")

# from https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics,
# a way to add a legend outside of the plot area
outside_legend <- function(...) {
    opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
                mar=c(0, 0, 0, 0), new=TRUE)
    on.exit(par(opar))
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    legend(...)
}

source(file="squish_numbers.R")
