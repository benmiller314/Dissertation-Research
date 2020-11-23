# percentage of dissertations accounted for by each tag

tag <- "Othr"

count <- length(which(noexcludes[,tag] == 1))
round(count / diss.count * 100, 1)

ccount <- length(which(consorts[,tag] == 1))
round (ccount / nrow(consorts) * 100, 1)