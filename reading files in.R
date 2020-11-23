# TO DO: Read in the text files for topic modeling.

textloc <- "/Users/benmiller314/Documents/fulltext dissertations/100txt/"
a <- noexcludes$fulltextpointers <- paste0(textloc, noexcludes$Pub.number, ".txt")

a <- sort(a)
head(a)
b <- file(a[1])

# scan(b, what=character(), 100, na.strings="\xfe\xff", skipNul=T)
d <- readLines(b, 10, skipNul=T)		# with skipNul, it can read! yay!
