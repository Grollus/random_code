unzip("Crime_Data_All.zip")
unzip("AssaultedOfficerData.zip")
#Reading data in
library(data.table)
library(dplyr)

crime_raw <- fread("Crime_Data_All.csv", stringsAsFactors = FALSE, 
                   data.table = FALSE)
assaulted_officers_raw <- fread("AssaultedOfficerData.csv", stringsAsFactors = FALSE,
                                data.table = FALSE)
#column convertions
crime_raw$DATE_REPORTED <- as.POSIXct(crime_raw$DATE_REPORTED, format = "%Y-%m-%d %H:%M:%S")
crime_raw$DATE_OCCURED <- as.POSIXct(crime_raw$DATE_OCCURED, format = "%Y-%m-%d %H:%M:%S")
crime_raw$CRIME_TYPE <- as.factor(crime_raw$CRIME_TYPE)
crime_raw$NIBRS_CODE <- as.factor(crime_raw$NIBRS_CODE)
crime_raw$UCR_HIERARCHY <- as.factor(crime_raw$UCR_HIERARCHY)
crime_raw$ATT_COMP <- as.factor(crime_raw$ATT_COMP)
crime_raw$LMPD_DIVISION <- as.factor(crime_raw$LMPD_DIVISION)
crime_raw$LMPD_BEAT <- as.factor(crime_raw$LMPD_BEAT)
crime_raw$PREMISE_TYPE <- as.factor(crime_raw$PREMISE_TYPE)

crime_raw$UOR_DESC <- as.factor(crime_raw$UOR_DESC)

#tidying up formatting
names(crime_raw) <- tolower(names(crime_raw))
crime_raw$crime_type <- tolower(crime_raw$crime_type)
crime_raw$ucr_hierarchy <- tolower(crime_raw$ucr_hierarchy)
crime_raw$att_comp <- tolower(crime_raw$att_comp)
crime_raw$lmpd_division <- tolower(crime_raw$lmpd_division)
crime_raw$premise_type <- tolower(crime_raw$premise_type)
crime_raw$block_address <- tolower(crime_raw$block_address)
crime_raw$city <- tolower(crime_raw$city)
crime_raw$uor_desc <- tolower(crime_raw$uor_desc)

#trimming trailing whitespace
crime_raw$block_address <- gsub("\\s+$", "", crime_raw$block_address)


#How many incidents by zip code regardless of date
by_zip <- crime_raw %>%
  filter( !(zip_code == "" | zip_code == "`"))%>%
  group_by(zip_code)%>%
  summarise(total = n())%>%
  arrange(desc(total))
            
#mapping this crime data using ggmap
library(ggmap)
louisville <- get_map(location = c(lon = -85.6767705, lat = 38.188805),
                      maptype = "roadmap",
                      source = "google")
crime_map <- ggmap(louisville,
      extent = "device",
      ylab = "Latitude",
      xlab = "Longitude") +
  geom_point(data = by_zip, )





###Houston test example
str(crime)
qmap('houston', zoom = 13)
gglocator(2)
