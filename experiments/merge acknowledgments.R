## GOALS:
# Import csv with "canonical names" of acknowledgments
# Split that into sources and targets, preserving row numbers
# Merge sources and targets together into a combined node/edge table
# Prune unnecessary columns
# Export as csv


# import
rows <- read.csv(file.choose())

# split
rows.split <- split(rows, rows$Origin..source.target.)
sources <- rows.split[[1]]
targets <- rows.split[[2]]

# merge
rows.combined <- merge(sources, targets, by="RowNumber", all=FALSE)
head(rows.combined)

# prune
rows.pruned <- rows.combined[, c("DissID.x", "RowNumber", "Canonical.Name.x", "Canonical.Name.y", "as_broader_term.x", "Node.x", "Node.y")]
names(rows.pruned) <- c("Diss_ID", "Row_Number", "Source_canonical", "Target_canonical", "As_What_broad", "Source_as_written", "Target_as_written")

# add column for the main verb, reorder
rows.pruned$Relation <- "mentioned"
rows.pruned <- rows.pruned[c("Diss_ID", "Row_Number", "Source_canonical", "Relation", "Target_canonical", "As_What_broad", "Source_as_written", "Target_as_written")]

# export
filename <- file.path("/Users", "millerb", "Box Sync", "research", "dissertations", "acknowledgments - cuny pitt uiuc - edge table.csv", sep="/")
write.csv(rows.pruned, filename)

rm(rows, rows.split, sources, targets, rows.combined, rows.pruned)
