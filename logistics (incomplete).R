# GOAL: Make a massive predictive model to see what characteristics are most predictive of RCWS-ness.
# Stretch goal: Use it on the unknown data, spot-check strong predictions.
# 
# PLAN: 1. Subset noexcludes2001_2015 based on known departments.
#       2. Get doc-topic grid for that subset.
#       3. Add method tags for each document in the doc-topic grid.
#       4. We now have LOTS of potentially independent (but probably interdependent) variables. 
#       5. For our dependent variable, is it confirmed-RCWS or confirmed non-RCWS?
#       6. Try to see what's predictive, i.e. strongly associated with RCWS
#          (or the opposite). Principal Components for dimension reduction?

dataset_name = "noexcludes2001_2015"
ntopics = 50
iter_index = 1
subset1 = knownprograms2001_2015
subset2 = nonrcws2001_2015sans_badtops
tagset_name = "no_ped_tagnames"

subset_index <- union(subset1$Pub.number, subset2$Pub.number)

if(!exists("get.doctopic.grid")) { source(file="get doctopic grid.R")}
mygrid <- get.doctopic.grid(dataset_name=dataset_name, ntopics=ntopics, iter_index=iter_index)$outputfile.dt
mygrid <- mygrid[Pub.number %in% subset_index]
nrow(mygrid)

dataset.dt <- data.table(get(dataset_name), key="Pub.number")
tagnames <- get(tagset_name)
tagnames <- setdiff(tagnames, "Othr")

for (i in tagnames) {
    mycol <- dataset.dt[subset_index, ..i]
    mygrid[, eval(i) := mycol]
}
mygrid[, tagnames := dataset.dt[subset_index, ..tagnames]]
mygrid[, "knownprogram" := dataset.dt[subset_index, realconsort] | dataset.dt[subset_index, realrhetmap]]

