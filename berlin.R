# Read in Berlin dissertation prize data (including winners and nominees), and calculate various stats

berlin <- read.csv(file="../berlin dissertation award committee finalists/berlin nominees.csv")
head(berlin)

## second try: loop through the rows, building three new lists out of noexcludes:

i <- 2
berlin.found <- berlin.missing <- berlin.multi <- c()
		
for (i in 1:nrow(berlin)) {
	x <- berlin[i,"Name"]
	y <- grep(x, noexcludes$Author, ignore.case=TRUE)
	
	if (length(y) == 1) {									#  1) successfully found
		r <- noexcludes[y,]
		r["Winner"] <- berlin[i, "Winner."]
		berlin.found <- rbind(berlin.found, r)
	} else if (length(y) < 1) {								#  2) not found
		berlin.missing <- rbind(berlin.missing, berlin[i,])
	} else if (length(y) > 1) {								#  3) multiples
		berlin.multi <- rbind(berlin.multi, berlin[i,]) 
	}	
}

## then re-examine and parse multiples by title, adding them to successfully found (if they are)
# for i in 1:nrow(berlin.missing) {
	
# }

## explore the data
winners <- berlin.found[which(berlin.found$Winner == "yes"),]
nrow(winners)
winners$Advisor.Name <- factor(winners$Advisor.Name)
table(winners$Advisor.Name)

runnersup <- berlin.found[which(berlin.found$Winner == "no"),]
nrow(runnersup)
barplot(table(factor(runnersup$School)))

berlin.found$School <- factor(berlin.found$School)
head(berlin.found)

tags <- noquote(paste(tagnames,collapse="+"))
summaryBy(Case+Crit+Cult+Disc+Ethn+Expt+Hist+Intv+Meta+Modl+Phil+Poet+Pract+Rhet+Surv+Othr~School, data=winners, FUN=sum)

head(berlin)



## then re-run other analyses on the resulting successful dataframe

