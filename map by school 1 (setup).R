## Plan of Attack
#
# A. for each school, determine a latitude and longitude (solved by geocode.R)
# B. create a table with columns for school, lat, lng, each tagname sum, total disses (this file)
# C. use map() to define a map area
# D. use add.pie() to plot a point for each school, with colors set by RColorBrewer, z values set by tagname sums, and radius = sqrt of total disses


## Set requirements
require(doBy)

# # check that we have all the source files we'll need
# if(!exists("imageloc")) {source(file="dataprep.R")}
# if(!exists("noexcludes")) {source(file="dataprep 2 - load data.R")}



## B. Build the Table

# Begin wrapper function
maptags1 <- function (dataset_name="noexcludes", tagset_name="tagnames") {

	# 0. convert parameters into useable values (we'll use the names later, for saving files)
	dataset <- get(dataset_name)
	tagset <- get(tagset_name)
	
	# 1. sum each method type for all schools.
	a1 <- summaryBy(.~School,data=dataset, FUN=sum)
	
	# limit output columns to those in the relevant tagset
	sumnames <- paste0(tagset, ".sum")
	a1 <- a1[, which(names(a1) %in% c("School", sumnames))]
	
	# save the output
	if(remake_figs) {
		filename <- paste0(dataloc, tagset_name, " tagsums by school, ", dataset_name, ", N", nrow(dataset), ".csv")
		write.csv(a1, file=filename)
	}
	
	# 2. count total dissertations for each school
	a2 <- summaryBy(Year~School, data=dataset,FUN=length)
	names(a2) <- c("School","DissCount")
	head(a2)
	
	if(remake_figs) {
		filename <- paste0(dataloc, "disses by school, ", dataset_name, ", N", nrow(dataset), ".csv")
		write.csv(a2, file=filename)
	}

	# 3. load file with school names and lat/lng data, created by geocode.R, which is much faster than geocoding anew; 
	# NB: diss.count created by 'dataprep 2 - load the data.R'
	if (!exists("all_schools.geo")) {
		action <- readline("Geocoding data is missing. To load a pre-created file, press L; to geocode now, press G.")
		if (tolower(action) == "l") { 
			invisible(readline("Select the geocoding csv file from geocode.R. (Filename is like 'geocoding by school, noexcludes, N2711.csv'; press <Enter> when ready."))
			all_schools.geo <<- read.csv(file=file.choose())
			
			# trim the first column, which is just the row number added on file save
			all_schools.geo <<- data.frame(all_schools.geo[,2:ncol(all_schools.geo)])
			head(all_schools.geo)	
			
		} else if (tolower(action) == "g") {
			if(!exists("geoCodeAll", mode="function")) { source(file="geocode.R") }
			all_schools.geo <<- geoCodeAll(dataset_name)
			
		} else {
			warning("Selection for geocoding action not understood; trying default for this dataset.")		
			filename <- paste0(dataloc, "geocoding by school, ", dataset_name, ", N", diss.count,".csv")
			all_schools.geo <<- read.csv(filename)

			# trim the first column, which is just the row number added on file save
			all_schools.geo <<- data.frame(all_schools.geo[,2:ncol(all_schools.geo)])
			head(all_schools.geo)
		}
	}

	# get clean column names
	names(all_schools.geo) <- c("School","Lat","Lng","City","State", "City.State")
	head(all_schools.geo)

		
	# 4. stitch together steps 1-3, inner join to eliminate schools left over from false positives.
	# this should give us a geocoded index of schools with columns for total disscount and for counts of each tag in the tagset.
	a4 <- merge(all_schools.geo, a1, by="School")
	a4 <- merge(a4, a2, by="School")
	
	# 5. return the merged table, since that should be enough to make maps. Give some sign of success.
	print(head(a4))
	return(a4)
	
}	# Close wrapper function maptags1

# run function
if(autorun) {
	schools.geo <- maptags1()
}
