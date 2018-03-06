#############################################################################
# map by school 1 (setup).R
#
# Goal: create a table with columns for school, lat, lng, sum of each tagname
# and total number of dissertations
#####

# Load required packages
require(doBy)

# Begin wrapper function
maptags1 <- function (dataset_name="noexcludes", tagset_name="tagnames") {

	# 0. convert parameters into useable values (we'll use the names later,
	# for saving files)
	dataset <- get(dataset_name)
	tagset <- get(tagset_name)
	
	# 1. sum each method type for all schools.
	a1 <- summaryBy(.~School, data=dataset, FUN=sum)
	
	# limit output columns to those in the relevant tagset
	sumnames <- paste0(tagset, ".sum")
	a1 <- a1[, which(names(a1) %in% c("School", sumnames))]
	
	# save the output
	if(remake_figs) {
		filename <- file.path(dataloc, paste0(tagset_name, " tagsums by school, ", 
						   dataset_name, ", N", nrow(dataset), ".csv"))
		write.csv(a1, file=filename)
	}
	
	# 2. count total dissertations for each school
	a2 <- summaryBy(Year~School, data=dataset, FUN=length)
	names(a2) <- c("School", "DissCount")
	head(a2)
	
	if(remake_figs) {
		filename <- file.path(dataloc, paste0("disses by school, ", dataset_name, 
							", N", nrow(dataset), ".csv"))
		write.csv(a2, file=filename)
	}

	# 3. if possible, load file with school names and lat/lng data, created
	# by geocode.R, which is much faster than geocoding anew.
	if (!exists("all_schools.geo")) {
		action <- readline("Geocoding data is missing. To load a pre-created
							 file, press L; to geocode now, press G.")
		if (tolower(action) == "l") {
			# Load the existing file 
			invisible(readline("Select the geocoding csv file from geocode.R. 
								(Filename is like 'geocoding by school, 
								noexcludes, N2711.csv'; press <Enter> when 
								ready.)"))
			all_schools.geo <<- read.csv(file=file.choose())
			
			# trim the first column, which is just the row number added on
			# file save
			all_schools.geo <<-
				data.frame(all_schools.geo[,2:ncol(all_schools.geo)])
			head(all_schools.geo)				
		} else if (tolower(action) == "g") {
			# Do the geocoding now
			if(!exists("geoCodeAll", mode="function")) {
				source(file="geocode.R") 
			}
			all_schools.geo <<- geoCodeAll(dataset_name)
			
		} else {
			warning("Selection for geocoding action not understood; 
					trying default for this dataset.")		
			filename <- file.path(dataloc, paste0("geocoding by school, ",
							 dataset_name, ", N", diss.count,".csv"))
			all_schools.geo <<- read.csv(filename)

			# trim the first column, which is just the row number, added on
			# file save
			all_schools.geo <<-
				data.frame(all_schools.geo[,2:ncol(all_schools.geo)])
			head(all_schools.geo)
		}
	}

	# get clean column names
	names(all_schools.geo) <- c("School", "Lat", "Lng", "City", "State", 
								 "City.State")
	head(all_schools.geo)

		
	# 4. stitch together steps 1-3, inner join to eliminate schools left over
	# from false positives. this should give us a geocoded index of schools
	# with columns for total disscount and for counts of each tag in the
	# tagset.
	a4 <- merge(all_schools.geo, a1, by="School")
	a4 <- merge(a4, a2, by="School")
	
	# 5. return the merged table, since that should be enough to make maps.
	# Give some sign of success.
	print(head(a4))
	return(a4)
	
}	# Close wrapper function maptags1

# run function
if(autorun) {
	schools.geo <- maptags1()
}
