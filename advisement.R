# within the consortium schools (consorts -- created by 'dataprep.R'), find min / max / average number of dissertations per advisor. Identify the top 10 people.

require(doBy)

advisement <- summaryBy(data=consorts, Author ~ Advisor.Name, FUN=length)
names(advisement) <- c("advisor","count")
o <- order(-advisement$count)
advisement.sorted <- advisement[o,]
head(advisement.sorted)
advisement.sorted <- advisement.sorted[2:nrow(advisement.sorted),]  # remove top row, which is blank advisors
summary(advisement.sorted$count)
boxplot(advisement.sorted$count)
text(x=3,y=advisement.sorted$count[1:10],labels=as.character(advisement.sorted$advisor[1:10])) ## TOTALLY UNFINISHED: trying to add labels for the outliers
advisement.sorted[1:10,]
tail(advisement.sorted)

# same measures for non-consortium schools (nonconsorts -- created by 'dataprep.R')

advisement.non <- summaryBy(data=nonconsorts, Author ~ Advisor.Name, FUN=length)
names(advisement.non) <- c("advisor","count")
min(advisement.non$count)
max(advisement.non$count)
mean(advisement.non$count)
median(advisement.non$count)
summary(advisement.non$count)
boxplot(advisement.non$count)
o <- order(-advisement.non$count)
advisement.non.sorted <- advisement.non[o,]
head(advisement.sorted)
