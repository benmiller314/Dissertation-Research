# read in and parse the Carnegie Classification data
carnegie.all <- read.csv(file=paste0(dataloc, "cc2010_classification_data_datasheet_06.03.2013.csv"))
attach(carnegie.all)

# identify and subset out schools in my dataset
# all schools with at least one doctoral program

cdoc2010 <<- carnegie.all[which(carnegie.all$IPGRAD2010 > 11),]
cdoc2005 <- carnegie.all[which(CCIPGRAD > 11),]
cdoc2000 <- carnegie.all[which(CC2000 %in% c(15,16)),]



detach(carnegie.all)
if (!exists("cdoc2010.geo")) {
		action <- readline("Geocoding data is missing for Carnegie 
						Classification doctoral schools. To load a
						pre-created file, press L; to geocode now, press G.")
						
		if (tolower(action) == "l") { 
			invisible(readline("Select the geocoding csv file from geocode.R.
						 (Filename is like 'geocoding by school, cdoc2010,
						  N449.csv'; press <Enter> when ready."))
						  
			cdoc2010.geo <<- read.csv(file=file.choose())
			
			# trim the first column, which is just the row number 
			# added when the file is saved
			cdoc2010.geo <<- data.frame(cdoc2010.geo[,2:ncol(cdoc2010.geo)])
			head(cdoc2010.geo)	
			
		} else if (tolower(action) == "g") {
			if(!exists("geoCodeAll", mode="function")) {
				 source(file="geocode.R") 
			}
			# takes about 15 minutes to geocode from scratch
			cdoc2010.geo <<- geoCodeAll("cdoc2010", "NAME")	
			
		} else {
			warning("Selection for geocoding action not understood; 
					trying default for cdoc2010.")		
			filename <- paste0(dataloc, "geocoding by school, cdoc2010, N",
					nrow(cdoc2010),".csv")
			cdoc2010.geo <<- read.csv(filename)

			# trim the first column, which is just the row number 
			# added when the file is saved
			cdoc2010.geo <<- data.frame(cdoc2010.geo[,2:ncol(cdoc2010.geo)])
			head(cdoc2010.geo)
		}
	} else {
		message("Found cdoc2010.geo, using existing data frame.")
	}

# cdoc2010.geo <- merge(cdoc2010, all_schools.geo[, c("all_schools", "Lat", "Lng")], by.x="NAME", by.y="all_schools")

# inspect the results
if(any(is.na(cdoc2010.geo$Lat))) {
	warning(paste("Still missing", length(which(is.na(cc$Lat)))," of ",
					nrow(schools.geo),"schools. Try OpenRefine."))
} else {
	message("All Carnegie-indexed doctoral schools geocoded 
			and saved as cdoc2010.geo.")
}

# TO DO: determine whether these schools changed classification between 2000 and 2010

# TO DO: related analyses to try
# 1. all comp/rhet dissertations by school classification (try different levels of drill-down; see 2010classifications_logic.pdf / http://classifications.carnegiefoundation.org/methodology/grad_program.php)
# 2. correlation table of school classification vs. aggregate method tags
