# Source in the csv file
library(data.table)
library(dplyr)
library(ggmap)
# Set wd to LouisvilleCrime folder
setwd("D:/RProgram/random_code/LouisvilleCrime")
addresses <- fread("unique_addresses.csv", data.table = FALSE)
lou_clean <- fread("clean_louisville_crime.csv", data.table = FALSE)

#-----------------------------------------------------------------------------------
# Basic geocoding function
# This would timeout every so often and I had to manually restart
# A full fledged implementation of the geocoding function needs to address this
# TODO: Address timeout error in geocode function.
    # Perhaps check for the error and if present, restart the function?
  # zip_codes <-  lou_clean %>%
  #   select(zip_code) %>%
  #   distinct(zip_code)
# Geocoding full addresses now with dsk api
  # 2.52 ggmap allows use of dsk instead of google maps api
full_addresses <- lou_clean %>%
  select(full_address) %>%
  distinct(full_address)


geocodeResults <- function(address){
  # query using dsk api: should be no limit
  geocode_reply <- geocode(address, output = "all", messaging = TRUE,
                           override_limit = TRUE, source = "dsk")
  
  # extracting information we want
  answer <- data.frame(supplied_address = address, lat = NA, lng = NA, 
                         returned_address = NA, status = NA)
  answer$status <- geocode_reply$status
  
  # For google API use only
  # pausing while we are over query limit
  while(geocode_reply$status == "OVER_QUERY_LMIT"){
    print("OVER QUERY LIMIT -pausing for 1 hour at:")
    print(as.character(Sys.time()))
    Sys.sleep(60*60)
    geocode_reply <- geocode(address, output = "all", messaging = TRUE,
                             override_limit = TRUE, source = "dsk")
    answer$status <- geocode_reply$status
  }
  
  # return NA's if status != "OK"
  if(geocode_reply$status != "OK"){
    return(answer)
  }
  
  # else: extract what information I am looking for
  answer$lat <- geocode_reply$results[[1]]$geometry$location$lat
  answer$lng <- geocode_reply$results[[1]]$geometry$location$lng
  answer$returned_address <- geocode_reply$results[[1]]$formatted_address
  
  return(answer)
  
}

# initializing the data frame for the results
  geocoded <- data.frame()
  # looking for existing temp file. if it exists find where it left off and start
  # from there
  startindex <- 1
  tempfilename <- paste0("input", "_temp_geocoded.rds")
  if(file.exists(tempfilename)){
    print("Found file - resuming from index:")
    geocoded <- readRDS(tempfilename)
    startindex <- nrow(geocoded)
    print(startindex)
  }

  for(i in seq(startindex, nrow(full_addresses))){
    print(paste("Working on index", i, "of", nrow(full_addresses)))
    # query geocoder
    result <- geocodeResults(full_addresses[i,])
    print(result$status)
    result$index <- i
    # append the answer to the results file
    geocoded <- rbind(geocoded, result)
    # save temporary results as we go
    saveRDS(geocoded, tempfilename)
  }
#----------------------------------------------------------------------------------

  
# adding lat/lng data to clean louisville data frame
lou_clean <- left_join(lou_clean, geocoded[, 1:3],
                            by = c("full_address" = "supplied_address"))

# making zip_code factor again
lou_clean$zip_code <- as.factor(lou_clean$zip_code)

# clarify these lat/lng are for zip code, not specific street address
names(lou_clean)[names(lou_clean) == "lat"] <- "lat_zip_code"
names(lou_clean)[names(lou_clean) == "lng"] <- "lng_zip_code"

#----------------------------------------------------------------------------------
# Adding column for hour crime occured
library(lubridate)
lou_clean$hour_occured <- round(hour(lou_clean$date_occured) + minute(lou_clean$date_occured)/60, 0)
# Adding day of week column
lou_clean$weekday <- wday(lou_clean$date_occured, label = TRUE, abbr = FALSE)

# write full data frame to csv for easy access
write.csv(lou_clean, "clean_louisville_crime.csv", row.names = FALSE)

# writing partial dataframe for shiny app
lou_shiny <- lou_clean%>%
  select(date_occured, crime_type, premise_type, zip_code, year_occured, month_occured,
         lat_zip_code, lng_zip_code, hour_occured, nibrs_code, weekday, lat, lng, full_address)
write.csv(lou_shiny, "lou_shiny_data.csv", row.names = FALSE)
