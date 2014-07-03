## Topic modeling, take 2
# 
# The plan:
# 1. Compile all noexcludes into a single massive data file, with one text per line
# 2. Read in that file as a file-backed big.matrix
# 3. a. Set up MALLET to analyze that big.matrix
#    b. For number of topics, try a sequence from 10-200, interval 10. 
#    c. Store all these crazy outputs as big.matrices.
# 5. Find the ideal number of topics by maximizing log.likelihood or minimizing perplexity.
##

# 0. Establish the working environment.
if (!exists(sourceloc)) { 
	source(file="/Users/benmiller314/Dropbox/coursework, etc/dissertation/R experiments/Dissertation Research/dataprep.R") 
}
setwd(sourceloc)

# 1. Run shell script to combine data into a single file.
# (NB: Make sure the correct functions in ben_clean_and_consolidate.sh are commented in/out).
system("'./Shell scripts and commands/ben_clean_and_consolidate.sh'")

# 2. That system command should output to the same directory every time. 
# Go there and read in the file using the `bigmemory` package.
library(bigmemory)
fulltext_noexcludes <- read.big.matrix("/Users/benmiller314/Documents/fulltext_dissertations/cumulative/noexcludes_cumulative.csv", type="character")
, backingfile="noexcludes_backing", backingpath="/Users/benmiller314/Documents/fulltext_dissertations/cumulative/", type="character")

