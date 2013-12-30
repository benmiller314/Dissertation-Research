# get all subject tags, put them in a big list
extract_subjects <- function (s) {
	if(!is.character(s)) { s <- as.character(s);}
	
	s2 <- sapply(s,FUN=function(x) unlist(strsplit(x,"|",fixed=TRUE)));
	
	output <- c();
	
	for (i in 1:length(s2)) {
		output <- c(output,s2[[i]]);
	}
	output <- factor(output)
	return(output);
}