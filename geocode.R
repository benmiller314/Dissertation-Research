# Geocoding instructions from http://allthingsr.blogspot.com/2012/01/geocode-your-data-using-r-json-and.html

# NB: This file will be called by `map by school 1 (setup).R`

library("RJSONIO") #Load Library

getGeoCode <- function( gcStr, 				# what location should we find lat/lng for?
						throttle = 2)		# how long to wait, in seconds, between requests?
{
  
  # correct for locations that GoogleMaps API can't find the names of
  if (gcStr == "Aquinas Institute of Theology") {
  	gcStr <- "3 South Spring Avenue St. Louis, Missouri"
  } else if (gcStr == "Emmanuel College of Victoria University (Canada)") {
  	gcStr <- "75 Queens Park Crescent East, Toronto, ON M5S 1K7, Canada"
  } else if (gcStr == "Fuller Theological Seminary, School of Theology") {
  	gcStr <- "135 N. Oakland Avenue, Pasadena, CA 91182"
  } else if (gcStr == "Seton Hall University, College of Education and Human Services") {
  	gcStr <- "400 South Orange Avenue, South Orange, NJ 07079"
  }

  gcStr2 <- gsub(' ','%20',gcStr) #Encode URL Parameters
  


 #Open Connection
 connectStr <- paste('http://maps.google.com/maps/api/geocode/json?sensor=false&address=',gcStr2, sep="") 
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)

#Flatten the received JSON
  data.json <- unlist(data.json)
  lat <- data.json["results.geometry.location.lat"]
  lng <- data.json["results.geometry.location.lng"]
  if(is.na(lat)) {
  	print(paste("Unable to match ",gcStr))
  }
  else {print(paste("Successfully matched",gcStr))}
  gcodes <- c(lat, lng)
  names(gcodes) <- c("Lat", "Lng")

  Sys.sleep(throttle)
  return (gcodes)
} 	# end of getGeoCode()

geoCodeAll <- function(dataset_name="noexcludes", schoolColName=c("School", "NAME")) {

	dataset <- get(dataset_name)
	if (schoolColName == "NAME") {
		all_schools <- levels(factor(dataset$NAME))
	} else {
		all_schools <- levels(factor(dataset$School))
	}
	
	# For each school found, use the function above to create new Lat and Long columns.	
	geoCols <- lapply(all_schools, function (val) { getGeoCode(val) } )
	
	# geoCols gives Lat/long in rows; flip it and add to school names
	all_schools.geo <- data.frame(all_schools, t(data.frame(geoCols)) )	
		head(all_schools.geo)
	row.names(all_schools.geo) <- NULL	
		head(all_schools.geo)
	
	# now to fix the ones that didn't match:
		# 1. get city and state data from "carnegie 1 (setup).R"
		all_schools.geo <- merge(all_schools.geo, carnegie.all[,c("NAME", "CITY", "STABBR")], by.x="all_schools", by.y="NAME", all.x=T, all.y=F)
		head(all_schools.geo)
	
		# 2. combine the city and state for ease of search
		all_schools.geo$City.State <- paste0(all_schools.geo$CITY, ", " ,all_schools.geo$STABBR)
	
		# 3. find blank Lat or Lng 
		blanks.index <- which(is.na(all_schools.geo$Lat))
	
		# 4. search based on city and state
		for (i in blanks.index) {
			lookup <- getGeoCode(all_schools.geo[i, "City.State"])
				
			# add the new Lat/Lng values to the levels, so they won't get rejected
			levels(all_schools.geo$Lat) <- c(levels(all_schools.geo$Lat), lookup[["Lat"]])
			levels(all_schools.geo$Lng) <- c(levels(all_schools.geo$Lng), lookup[["Lng"]])		
			
			# add the new values to the appropriate Lat/Lng cells
			all_schools.geo[i, "Lat"] <- lookup[["Lat"]]
			all_schools.geo[i, "Lng"] <- lookup[["Lng"]]
		}

	# check output
	print(all_schools.geo)
	
	# convert Lat/Lng factors to numbers
	all_schools.geo$Lat <- as.numeric(levels(all_schools.geo$Lat)[all_schools.geo$Lat])
	all_schools.geo$Lng <- as.numeric(levels(all_schools.geo$Lng)[all_schools.geo$Lng])

	# save that file!
	if(remake_figs) {
		filename <- paste0(dataloc, "geocoding by school, ", dataset_name, ", N", nrow(dataset),".csv")
		write.csv(all_schools.geo, file=filename)
	}
	
	return(all_schools.geo)
	
} 	# end of wrapper function geoCodeAll
