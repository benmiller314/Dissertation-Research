## Topic modeling, take 2
# 
# The plan:
# 1. Choose a directory of cleaned text files, and an optional number of topics
# 2. Set up MALLET to analyze that directory, but don't run yet
#	 a. use Ben Marwick's approach ('r2mallet.R') to create the instance file
#    b. use David Mimno's approach ('topic modeling with mallet package.R') to train the model
#	    (but use my token regex from that file, not his original one)
# 3. If we don't know number of topics, 
#	 a. use foreach() to try a sequence from 10-200, interval 10, then
#    b. maximize log.likelihood/token, which MALLET outputs somewhere by default. (Find it!)
# 4. Run MALLET with the parameters set up in Step 2, with the topics as chosen in 1 or 3.
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
