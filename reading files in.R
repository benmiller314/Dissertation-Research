# TO DO: Read in the text files for topic modeling.

textloc <- "/Users/benmiller314/Documents/fulltext dissertations/100txt/"
a <- noexcludes$fulltextpointers <- paste0(textloc, noexcludes$Pub.number, ".txt")

a <- sort(a)
head(a)
b <- file(a[1])

readLines(b, 10)						# just spits back "\xfe\xff" "" over and over
readLines(b, encoding="UTF-16", 10)		# but so does this

pdfloc  <- "/Users/benmiller314/Documents/fulltext dissertations/"
c <- paste0(pdfloc, noexcludes$Pub.number, ".pdf")
c <- sort(c)
d <- file(c[1])

readLines(d, 10, encoding="UCS-2")		# lots 


# list possible encodings (there are many)
iconvlist()