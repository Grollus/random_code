# Source in the csv file
library(data.table)
library(dplyr)
library(ggmap)
addresses <- fread("unique_addresses.csv", data.table = FALSE)
lou_clean <- fread("clean_louisville_crime.csv", data.table = FALSE)

#-----------------------------------------------------------------------------------
# Geocoding by zip code
zip_codes <-  lou_clean %>%
  select(zip_code) %>%
  distinct(zip_code)


geocodeResults <- function(address){
  # query google map api
  geocode_reply <- geocode(address, output = "all", messaging = TRUE,
                           override_limit = TRUE)
  
  # extracting information we want
  answer <- data.frame(supplied_address = address, lat = NA, lng = NA, 
                         returned_address = NA, status = NA)
  answer$status <- geocode_reply$status
  
  
  # pausing while we are over query limit
  while(geocode_reply$status == "OVER_QUERY_LMIT"){
    print("OVER QUERY LIMIT -pausing for 1 hour at:")
    print(as.character(Sys.time()))
    Sys.sleep(60*60)
    geocode_reply <- geocode(address, output = "all", messaging = TRUE,
                             override_limit = TRUE)
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

for(i in seq(startindex, nrow(zip_codes))){
  print(paste("Working on index", i, "of", nrow(zip_codes)))
  # query geocoder
  result <- geocodeResults(zip_codes[i,])
  print(result$status)
  result$index <- i
  # append the answer to the results file
  geocoded <- rbind(geocoded, result)
  # save temporary results as we go
  saveRDS(geocoded, tempfilename)
}