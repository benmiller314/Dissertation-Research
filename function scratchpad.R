require(doBy)

# given two yes-or-no vectors of the 15 possible methods tags, calculate a difference measure.
tagdiff <- function (tagvector1,tagvector2,skip=0) {
	v1 <- tagvector1[(skip+1):length(tagvector1)];
	v2 <- tagvector2[(skip+1):length(tagvector2)];
	
#	print(cbind(v1,v2));
	if(!is.integer(v1)) {
		v1 <- as.integer(v1);
	} 
	if(!is.integer(v2)) {
		v2 <- as.integer(v2);
	}
	
	diffscore <- sum(abs(v1-v2));
	
	return(diffscore);
}

# given two yes-or-no vectors of the 15 possible methods tags, calculate a similarity measure.
# 'skip' argument assumes column one contains names; if not, use skip=0.
tagmatch <- function(tagvector1,tagvector2,skip=0) {
	v1 <- tagvector1[(skip+1):length(tagvector1)];
	v2 <- tagvector2[(skip+1):length(tagvector2)];
	
	if(!is.integer(v1)) {
		v1 <- as.integer(v1);
	} 
	if(!is.integer(v2)) {
		v2 <- as.integer(v2);
	}		
	sumv <- v1+v2;  # add vectors together; positive matches will equal 2, matches by ommission will equal 0, misses will equal 1.
	testv <- trunc(sumv/2) # divide by two and round down. positive matches now equal 1, all else 0.
	
	matchscore <- sum(testv); 			# count the matches.
	if(sum(sumv) == 0) {
		matchpct <- 0;
	} else {
		matchpct <- matchscore/sum(sumv);	# divide matches by combined number of tags
	}
	matchpct <- matchpct * 2;			# scale back to the interval [0,1]
	matchpct <- round(matchpct,digits=3);
	
	return(matchpct);
}

# given an array of tagvectors, produce a vector of similarity scores for a given row.
rowmatch <- function (tagarray,basenumber=1,skip=0) {
	rows <- nrow(tagarray);
	columns <- ncol(tagarray);
	
	matches <- NaN;
	
	for (i in 1:rows) {
		base <- tagarray[basenumber,(skip+1):columns];
		targ <- tagarray[i,(skip+1):columns];
		
		match <- tagmatch(base,targ);

		matches <- c(matches,match);
	}
	
	return (matches[2:length(matches)]);	# strip the NaN row.
}

# given an array of tagvectors, produce an array of rowmatch-style difference vectors.
matcharray <- function (tagarray,skip=1) {
	rows <- nrow(tagarray)
	
	output <- NaN;
	
	for (i in 1:rows) {
		m <- rowmatch(tagarray,basenumber=i,skip=skip);		# calculate similarity vector for row i
		output <- rbind(output,m);		# bind this vector to the output matrix as a new row
	}
	clean_output <- output[2:nrow(output),];  # strip the NaN row
	
	return(clean_output);
}

# given an array of tagvectors, produce a vector of difference scores for a given row.
rowdiff <- function (tagarray,basenumber=1,skip=0) {
	rows <- nrow(tagarray);
	columns <- ncol(tagarray);
	
	diffs <- NaN;
	
	for (i in 1:rows) {
		base <- tagarray[basenumber,(skip+1):columns];
		targ <- tagarray[i,(skip+1):columns];
		
		diff <- tagdiff(base,targ);

		diffs <- c(diffs,diff);
	}
	
	return (diffs[2:length(diffs)]);		# strip the NaN row.
}

# given an array of tagvectors, produce an array of rowdiff-style difference vectors.
diffarray <- function (tagarray,skip=1) {
	rows <- nrow(tagarray)
	
	output <- NaN;
	
	for (i in 1:rows) {
		rd <- rowdiff(tagarray,basenumber=i,skip=skip);   # calculate difference vector for row i
		output <- rbind(output,rd);  # bind this vector to the output matrix as a new row
	}
	clean_output <- output[2:nrow(output),];  # strip the NaN row
	
	return(clean_output);
}

# given an array of tagvectors, find totals for each method tag
# 'skip' argument assumes first column contains names; if not, use skip=0.
tagtotals <- function (tagarray, skip=1) {
	labels <- names(tagarray);
	
	output <- NaN;
	
	for (i in (skip+1):length(labels)) {
		count <- sum(tagarray[,i]);
		#label <- labels[i];
		
		#iput <- rbind(label,count);
		output <- c(output,count);
	}
	
	names(output) <- labels 
	clean_output <- output[2:length(labels)];	# strip NaN column
	

	return(clean_output);
}

# given an array of tagvectors and a seriation of type ser_permutation_vector,
# reorder the array using this seriation.
reorder_ser <- function (tagarray, order_vector) {
	o <- get_order(order_vector);
	rows <- nrow(tagarray);
	
	# verify that order_vector is the right size
	if (length(o) != rows) {
		error_msg <-c ("Error: length of ",o," does not match length of ",tagarray,".");
		print(c);
		break;
	}
	
	output <- NaN;								# give the output something to bind to
	
	for (i in 1:5) {
		row_to_get <- o[i];						# determine next row from permutation vector
		row_gotten <- tagarray[row_to_get,];	# pull that row from the original array

		output <- rbind(output,row_gotten);		# add the new row to the output
	}
	
	clean_output <- output[2:length(output),];	# strip NaN row
	
	print(clean_output);
}

# given a dissimilarity array, find the best sorting method among those provided by agnes()
best_sort <- function(diffs) {
	agn_methods <- c("average","single","complete","ward","weighted");
	
	agn1 <- agnes(diffs,diss=TRUE,method="average"); 
	agn2 <- agnes(diffs,diss=TRUE,method="single"); 
	agn3 <- agnes(diffs,diss=TRUE,method="complete");
	agn4 <- agnes(diffs,diss=TRUE,method="ward");
	agn5 <- agnes(diffs,diss=TRUE,method="weighted");

	c(agn1$ac,agn2$ac,agn3$ac,agn4$ac,agn5$ac) -> all_ac
	print(rbind(agn_methods,all_ac));
	
	max_ac <- max(all_ac);
	max_index <- which(all_ac==max_ac);	
	print(c('Best method = ',agn_methods[max_index]));

	outputs <- rbind(agn1,agn2,agn3,agn4,agn5)
	order <- outputs[max_index];
	value <- all_ac[max_index]

	return(cbind(value,order));
}

# given a data array, iterate matcharray and best_sort 
# until the new order is the same as the old order.
shuffle <- function(tagarray,skip=1,max_reps=5) {
	print("Did you remember to bind the output? This function takes a long time to run!")

	length <- nrow(tagarray);
	reps <- 0;

	repeat {
		reps <- reps+1;
		# matches <- matcharray(tagarray);
		# diffs <- 1-matches;
		diffs <- dist(tagarray);
		
		best_sort(diffs) -> o;
		o[[1]] -> ac
		o[[2]] -> new_order;
		
		sum(abs(new_order - c(1:length))) -> notyet; # if order is not strictly linear, sum will be non-zero
		tagarray <- rbind(tagarray[new_order,]);		

		if (reps == max_reps) {
			print(c("Reached max_reps (",reps,"); stopping."));
			break;
		} else if (ac > .99) {
			print("Close enough! Agglomeration coefficient > .99");
			break;
		} else if (notyet != 0) {    
			print(c("Looks like we've got a new order... (notyet = ",notyet,")"));
		} else {
			print("Reordering has converged. Yay!");
			break;
		}
	}	
	
	return(tagarray);	
}

# given a csv version of the standard tagging spreadsheet with the 15 extra columns for tag markers,
# get the data into a names-and-data-tags-only format for use in the above analyses.
datawrangle <- function (filename) {
	read.csv(filename) -> sourcearray;
	tagarray <- sourcearray[,c(2,tagnames)];
	return(tagarray);
}

# given one row of a big sourcearray (with all columns), extract the data tags as a vector with 15 columns.
get_tags <- function(data_row) {
	tagarray <- data_row[tagnames];
	return(tagarray);
}

# in a big sourcearray (with all columns), extract the data tags for all rows with a given value in a specified column.
filter_data <- function (sourcearray, searchcolumn, searchvalue) {
	attach(sourcearray)
	result_index <- which(searchcolumn == searchvalue);
	
	output <- NaN;
	
	for (i in result_index) {
		itags <- get_tags(sourcearray[i,]);
		output <- rbind(output,itags);
	}
	
	clean_output <- output[2:nrow(output),]
	return(clean_output);
}

# more efficiently (?) summarize tag counts by a specified column
# tagmeans <- function (sourcearray, searchcolumn) {
	# require(doBy);
	# summaryBy(Case+Crit+Cult+Disc+Ethn+Expt+Hist+Intv+Meta+Modl+Phil+Poet+Pract+Rhet+Surv ~ searchcolumn,data=sourcearray,FUN=mean) -> tagmeans;
	
	# return(tagmeans);
# }

# find tag cooccurences for a single tag type
tags.allbyone <- function (sourcearray, byterm, do.fun="mean") {
	require(doBy);
	
	search <- paste(tagnames,collapse="+");
	output <- summaryBy(eval(parse(text=search)) ~ eval(parse(byterm)), FUN=do.fun);
	return(output);
}

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


heatmap.ben <- function (sum.by.tags.s, diags=FALSE) {
	n.col <- ncol(sum.by.tags.s); # print(n.col)
	n.row <- nrow(sum.by.tags.s); # print(n.row)

	# color function
	colorme <- function (val) {
		numCols <- 20
		pal <- colorRampPalette(c("#FAFAFA","#818181"))
		cols <- pal(numCols)
		max.val <- max(sum.by.tags.s)
		min.val <- min(sum.by.tags.s)
		colIndex <- round(numCols * (val - min.val) / (max.val - min.val))
		colIndex <- max(1,colIndex)
		return(cols[colIndex])
	}

	# set up a blank canvas of the right size
	plot(0, 0, xlim=c(0.5,0.5+n.col), ylim=c(0.5,0.5+n.row), type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")

	# map each square
	for (i in 1:n.row) {
		for (j in 1:n.col) {
			# print(c('i = ',i,' j = ',j))
			diagcheck <- NULL					# outline the diagonals if need be
			if (diags && i == j) {
				diagcheck <- "black"
			}

			symbols(j,1+n.row-i,squares=1, add=TRUE, inches=FALSE, fg=diagcheck, bg=colorme(sum.by.tags.s[i,j]))
			text(j,1+n.row-i, round(sum.by.tags.s[i,j], 2), cex=0.65)
		}
	}

	# add axis labels
	axis(side=4, at=n.row:1, labels=row.names(sum.by.tags.s), pos=0.5+n.col, las=2, col="white")
	axis(side=1, at=1:n.col, labels=names(sum.by.tags.s), pos=0.5, las=2, col="white")	
}


# Factor-Bug fixing:
# when you find out your reconciliation picked the wrong version, and now you get invalid factor levels

fix_factor <- function(f, to.add, to.remove = NULL) {
	ff <- as.character(f)

	if (!is.null(to.remove)) {
		ff[which(ff == to.remove)] <- to.add
	} else {
		ff <- c(ff, to.add)
	}
	return(factor(ff))
}

