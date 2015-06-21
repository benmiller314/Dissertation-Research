# Geocoding instructions from http://allthingsr.blogspot.com/2012/01/geocode-your-data-using-r-json-and.html

# NB: This file will be called by `map by school 1 (setup).R`

library("RJSONIO") #Load Library

getGeoCode <- function(gcStr)
{
  throttle <- 2		# how long to wait, in seconds, between requests?
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
}

# For each school found, use the function above to create new Lat and Long columns.
all_schools <- levels(noexcludes$School)

geoCols <- lapply(all_schools,function(val){getGeoCode(val)})

geoCols <- t(data.frame(geoCols))			# Lat/long is in rows; flip it
all_schools <- cbind(all_schools,geoCols)
row.names(all_schools) <- NULL	
all_schools <- data.frame(all_schools)
names(all_schools.m) <- c("all_schools", "Lat", "Lng")


# now to fix the ones that didn't match:
	# first, get city and state data from "carnegie 1 (setup).R"
all_schools <- merge(all_schools, carnegie.all[,2:4], by.x="all_schools", by.y="NAME", all.x=T, all.y=F)

for (i in 1:nrow(all_schools)) {
	# for all rows, combine the city and state for ease of search
	all_schools[i,6] <- paste(all_schools$CITY[i], "," ,all_schools$STABBR[i])

	if (is.na(all_schools[i,2]) || is.na(all_schools[i,3])) {
		# if blank, get Lat/Lng values based on city and state
		geoCols <- getGeoCode(all_schools[i,6])
		
		# add the new Lat/Lng values to the levels, so they won't get rejected
		l2 <- levels(all_schools[,2])
		l2 <- c(l2, geoCols[[1]])		# Lat
		levels(all_schools[,2]) <- l2
		
		l3 <- levels(all_schools[,3])
		l3 <- c(l3, geoCols[[2]]) 		# Lng
		levels(all_schools[,3]) <- l3
		
		# add the new values to the appropriate Lat/Lng cells
		all_schools[i,2] <- geoCols[[1]]
		all_schools[i,3] <- geoCols[[2]]
	}
}
levels(all_schools[,2])
levels(all_schools[,3])

rm(l2,l3,geoCols)

#save that file!
if(remake_figs) {
	filename <- paste0(dataloc, "geocoding by school, N", diss.count,".csv")
	write.csv(all_schools,file=filename)
}

