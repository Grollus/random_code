library(dplyr)
library(ggplot2)
library(readr)

#Downloading File
url <- "http://files.hubwaydatachallenge.org/hubway_2011_07_through_2013_11.zip"
download.file(url, "raw.zip")

#Unzipping and reading in csv files
unzip("raw.zip")
hubway.stations <- read_csv("hubway_stations.csv")
hubway.trips <- read_csv("hubway_trips.csv")


#Converting dates to POSIX format
hubway.trips$start_date <- as.POSIXct(as.character(hubway.trips$start_date),
                                      format = "%m/%d/%Y %H:%M:%S")
hubway.trips$end_date <- as.POSIXct(as.character(hubway.trips$end_date), 
                                    format = "%m/%d/%Y %H:%M:%S")

#Removing ' from start of zip code
hubway.trips$zip_code <- gsub("'", "", as.character(hubway.trips$zip_code))

#Converting missing values to NA's
hubway.trips$zip_code[hubway.trips$zip_code == ""] <- NA
hubway.trips$gender[hubway.trips$gender == ""] <- NA

#Converting several variables to factors as it seems to make more sense
hubway.trips$strt_statn <- as.factor(hubway.trips$strt_statn)
hubway.trips$end_statn <- as.factor(hubway.trips$end_statn)
hubway.trips$zip_code <- as.factor(hubway.trips$zip_code)

###############################################################################
#What is the average trip duration for annual members vs. casual users
#Initial exploration shows trip duration is listed in seconds
#May want to create new variable later with more interpretable time format
summary(hubway.trips$duration)
#Shows negative values and some incredibly high values
#Negatives are just removed--we'll create a copy of data frame for this question
hub.duration <- subset(hubway.trips, duration > 0)
#Just duration and subsc_type info
duration <- hub.duration[,c(4, 10)]
#Using dplyr to group by subsc_type and then filter any outliers according to 
#hampel proc (median + (5 *mad))
duration <- duration %>%
        group_by(subsc_type)%>%
        filter(duration <= (median(duration) + 5 * mad(duration)))
means <- duration%>%
        group_by(subsc_type)%>%
        summarise(mean = mean(duration))

#Creating a nice plot
ggplot(duration, aes(x = subsc_type, y =duration))+
        geom_boxplot(aes(fill = subsc_type))+
        scale_y_continuous(breaks = seq(0, 5000, 250))+
        ggtitle("Average Trip duration for annual members vs. casual users")+
        xlab("Type of Subscription") +
        ylab("Duration (Seconds)")+
        geom_text(data = means, aes(label = round(mean,1), y = mean + 50))

################################################################################
#What are the peak Hubway hours?
#Creating copy for question
peak.hub <- hubway.trips$start_date
#Rounding to nearest hours
hub.hours <- round(peak.hub, "hours")
#Extracting hour attribute and adding to original data set
hubway.trips$start.hour <- hub.hours[["hour"]]
hubway.trips$start.hour <- as.factor(hubway.trips$start.hour)
#Plotting 
ggplot(hubway.trips, aes(start.hour))+
        geom_bar(fill = "midnightblue") +
        scale_y_continuous(breaks = seq(0, 200000, 10000))+
        ggtitle("Usage by Starting Hour")+
        xlab("Hour of Day")+
        ylab("Frequency")

###############################################################################
#Which days of the week get the most Hubway traffic?
#Creating a new variable identifying the day of the week trip started
hubway.trips$start.day <- factor(weekdays(hubway.trips$start_date), 
                                    levels = c("Monday", "Tuesday", "Wednesday",
                                               "Thursday", "Friday", "Saturday",
                                               "Sunday"))
#Plotting the frequency of trips by day of the week
ggplot(hubway.trips, aes(start.day))+
        geom_bar(fill = "blue4")+
        scale_y_continuous(breaks = seq(0, 250000, 25000))+
        coord_flip()+
        ggtitle("Usage by Starting Weekday")+
        xlab("Day of Week")+
        ylab("Frequency")
#Wanted to explore if the median duration of a trip varied by the day of week
#Use dplyr to group by day of week and then create a summarise column for avg.
day.avg <- hubway.trips%>%
        group_by(start.day, subsc_type)%>%
        filter(duration > 0) %>%
        summarise(med.dur = median(as.numeric(duration)), 
                  mean.dur = mean(duration))

ggplot(day.avg, aes(x = start.day, y =mean.dur))+
        geom_bar(stat= "identity", fill = "darkgrey")+
        facet_wrap(~subsc_type)+
        scale_y_continuous(breaks = seq(0, 2600, 250))+
        ggtitle("Trip duration by Day of Week")+
        xlab("Day of Week")+
        ylab("Mean Trip Duration (in Seconds)")

################################################################################
#Which stations are most popular? Which stations make up the most popular 
#origin/destination pairs?

#Initially, let's try plotting most popular starting station and most popular
#ending station to see if there is a difference
#How many unique starting stations are there?
length(unique(hubway.trips$strt_statn))
#What about end stations?
length(unique(hubway.trips$end_statn))
#Data frame with starting station popularity (by number of trips)
pop.start <- hubway.trips%>%
        filter(!is.na(strt_statn))%>%
        count(strt_statn)%>%
        arrange(desc(n))
#Subsetting to top twentyfive for now
top.twentyfive.start <- pop.start%>%
        head(n=25)
#Simple Plot of station use by starting station        
p1 <- ggplot(top.twentyfive.start, aes(reorder(strt_statn, -n),n))+
        geom_bar(stat = "identity")+
        scale_y_continuous(breaks = seq(0, 60000, 10000))+
        xlab("Starting Station")+
        ylab("Number of Trips")+
        ggtitle("Top Twenty-Five Most Used Stations By Starting Station")
#What about by ending station
pop.end <- hubway.trips%>%
        filter(!is.na(end_statn))%>%
        count(end_statn)%>%
        arrange(desc(n))
#Again subsetting top twentyfive
top.twentyfive.end <- pop.end%>%
        head(n=25)
#Simple plot of station use by ending station
p2 <- ggplot(top.twentyfive.end, aes(reorder(end_statn, -n), n))+
        geom_bar(stat = "identity")+
        scale_y_continuous(breaks = seq(0, 60000, 10000))+
        xlab("Ending Station")+
        ylab("Number of Trips")+
        ggtitle("Top Twenty-Five Most Used Stations by Ending Station")

#Putting together to compare
library(gridExtra)
grid.arrange(arrangeGrob(p1, p2))
#How many of the top twentyfive are different?
sum(ifelse(top.twentyfive.start$strt_statn == top.twentyfive.end$end_statn, 
           1, 0))
#8 are in the same positions as one another
sum(ifelse(top.twentyfive.start$strt_statn %in% top.twentyfive.end$end_statn,
           1, 0))
#But both lists contain the same twenty five stations

#What about the least popular? Just use the tail of the previous dataframes
bottom.twentyfive.start <- pop.start%>%
        tail(n = 25)
bottom.twentyfive.end <- pop.end%>%
        tail(n = 25)
p3 <- ggplot(bottom.twentyfive.start, aes(reorder(strt_statn, -n), n))+
        geom_bar(stat = "identity")+
        scale_y_continuous(breaks = seq(0, 1500, 100))+
        xlab("Starting Station")+
        ylab("Number of Trips")+
        ggtitle("Bottom Twenty-Five Most Used Stations by Starting Station")
p4 <- ggplot(bottom.twentyfive.end, aes(reorder(end_statn, -n), n))+
        geom_bar(stat = "identity")+
        scale_y_continuous(breaks = seq(0, 1500, 100))+
        xlab("Ending Station")+
        ylab("Number of Trips")+
        ggtitle("Bottom Twenty-Five Most Used Stations by Ending Station")
grid.arrange(arrangeGrob(p3, p4))

#Does station popularity differ between registered and casual users
subsc.pop.start <- hubway.trips%>%
        filter(!is.na(strt_statn))%>%
        count(subsc_type, strt_statn)%>%
        arrange(desc(n))%>%
        top_n(10) 

subsc.pop.end <- hubway.trips%>%
        filter(!is.na(end_statn))%>%
        count(subsc_type, end_statn)%>%
        arrange(desc(n))%>%
        top_n(10)

p5 <- ggplot(subset(subsc.pop.start, subsc_type == "Registered"),
                aes(reorder(strt_statn, -n), n))+
        geom_bar(stat = "identity")+
        scale_y_continuous(breaks = seq(0, 50000, 10000))+
        xlab("Starting Station")+
        ylab("Number of Trips")+
        ggtitle("Top 25 Starting Stations of Registered Users")

p6 <- ggplot(subset(subsc.pop.start, subsc_type == "Casual"),
                aes(reorder(strt_statn, -n), n))+
        geom_bar(stat = "identity")+
        scale_y_continuous(breaks = seq(0, 50000, 10000))+
        xlab("Starting Station")+
        ylab("Number of Trips")+
        ggtitle("Top 25 Starting Stations of Casual Users")

p7 <- ggplot(subset(subsc.pop.end, subsc_type == "Registered"),
             aes(reorder(end_statn, -n), n))+
        geom_bar(stat = "identity")+
        scale_y_continuous(breaks = seq(0, 50000, 10000))+
        xlab("Ending Station")+
        ylab("Number of Trips")+
        ggtitle("Top 25 Ending Stations of Registered Users")

p8 <- ggplot(subset(subsc.pop.end, subsc_type == "Casual"),
             aes(reorder(end_statn, -n), n))+
        geom_bar(stat = "identity")+
        scale_y_continuous(breaks = seq(0, 50000, 10000))+
        xlab("Ending Station")+
        ylab("Number of Trips")+
        ggtitle("Top 25 Ending Stations of Casual Users")

grid.arrange(arrangeGrob(p5, p7, p6, p8))

#What are the most popular origin/destination pairs?
#First lets address this as a whole---not broken down by user type
pairs <- hubway.trips%>%
        filter(!is.na(strt_statn))%>%
        count(strt_statn, end_statn)%>%
        ungroup()%>%
        arrange(desc(n))
pairs$beg.end <- factor(paste0(as.character(pairs$strt_statn), 
                        as.character(pairs$end_statn)))

pairs$beg.end <- reorder(pairs$beg.end, pairs$n)

ggplot(head(pairs, n = 10), aes(y = beg.end, x = n))+
        geom_point()+
        theme_bw()+
        xlab("Number of Trips")+
        theme(axis.text.x = element_text(size = 10))+
        theme(axis.title.y = element_text(size = 11))+
        ylab("Starting/Ending Station Pair")+
        ggtitle("Most Popular Station Pairs")

###############################################################################
#Which stations are the most asymmetric - more trips start there than end there
#, or vice versa? Are they all at the top of hills?

#First dealing with most asymmetric. If I sum the number of starting and ending
#trips for each station, then take the abs(start-end) that should give some 
#measure of the symmetry.  Big numbers mean a trip has way more of one than the
#other.  Should it be an absolute value though? 

pop.start <- hubway.trips%>%
        filter(!is.na(strt_statn))%>%
        count(strt_statn)%>%
        arrange(desc(strt_statn))
pop.end <- hubway.trips%>%
        filter(!is.na(end_statn))%>%
        count(end_statn)%>%
        arrange(desc(end_statn))
station.sums <- cbind(pop.start, pop.end)

station.sums$diff <- station.sums[,2] - station.sums[,4]

station.sums$ <- reorder(station.sums$strt_statn, station.sums$diff)














#What does a year in the life of one Hubway bike look like?
#What is the best way to represent this question?  Using hubs as nodes and 
#plotting trips of that one bike? Maybe the most used bike? Let's explore

bike.time <- hubway.trips%>%
        filter(!is.na(strt_statn))%>%
        group_by(bike_nr)%>%
        summarise(usage.hours = round(sum(duration)/60/60),
                  trips = length(strt_statn))%>%
        arrange(desc(usage.hours))
bike.time$usage.per.trip <- round(bike.time$usage.hours/bike.time$trips,2)
bike.time$bike_nr <- reorder(bike.time$bike_nr, bike.time$usage.hours)

#First, let's just see the top twenty used bikes according to hours used
ggplot(head(bike.time, n = 20), aes(x = usage.hours, y = bike_nr))+
        geom_point()+
        theme_bw()+
        scale_x_continuous(breaks = seq(0, 4000,500))+
        theme(axis.text.y = element_text(size = 8))+
        theme(axis.title.y = element_text(size = 12))+
        ylab("Bike Number")+
        xlab("Hours in use")+
        ggtitle("Twenty most used bikes by total trip duration")


#But looking at this, 7 of the top twenty have usage per trip of a day or over.
#This seems like it is going to severly skew the data.  What if we filter out 
#trips that are in the bottom 1% or the top 99% for trip duration
hubway.trips2 <- hubway.trips%>%
        filter(duration >= 0 & duration >= quantile(hubway.trips$duration, c(.01)) 
               & duration <= quantile(hubway.trips$duration, c(.99)))

#Now let's try this again
bike.time2 <- hubway.trips2%>%
        group_by(bike_nr)%>%
        summarise(usage.hours = round(sum(duration)/60/60),
                  trips = length(strt_statn))%>%
        arrange(desc(usage.hours))
bike.time2$usage.per.trip <- round(bike.time2$usage.hours/bike.time2$trips, 2)
bike.time2$bike_nr <- reorder(bike.time2$bike_nr, bike.time2$usage.hours)

#Now let's plot and see the difference
ggplot(head(bike.time2, n = 20), aes(x = usage.hours, y = bike_nr))+
        geom_point()+
        theme_bw()+
        theme(axis.text.y = element_text(size = 8))+
        xlab("Hours in use")+
        ylab("Bike Number")+
        ggtitle("Twenty most used bikes by total trip duration")

#This needs more work--need to link data to maps.  Will try using gmaps api 
#or the Rgooglemaps package if possible

      
        

###############################################################################
#Which stations get the most tourist traffic, and which get the most commuters?
member.type <- hubway.trips2%>%
        filter(!is.na(strt_statn))%>%
        count(subsc_type, strt_statn)%>%
        arrange(desc(n))%>%
        top_n(5)
member.type$strt_statn <- as.integer(member.type$strt_statn)

member.type <- left_join(member.type, hubway.stations, 
                         by = c("strt_statn" = "id"))
member.type$station<-reorder(member.type$station, member.type$n)


ggplot(member.type, aes(x = station, y = n))+
        geom_point()+
        coord_flip()+
        facet_wrap(~subsc_type)+
        theme_bw()+
        ylab("Number of Trips")+
        xlab("")+
        ggtitle("Most Popular Tourist and Commuter Stations")


#How far does Hubway really reach? Which community should be the next to get
#Hubway stations?
library(ggmap)
library(grid)
#recode frequency of station use to discrete variable for plotting
quan <- quantile(pop.start$n)
pop.start$use_level <- cut(pop.start$n, breaks = quan, labels = c("Low", 
                                                                  "Medium", 
                                                                  "High",
                                                                  "Ultra-High"))


#Getting map of Boston
bostonmap <- get_map(location = "boston", 
                        zoom = 12)
png("hubwaystations.png")
usemap <- ggmap(bostonmap)+
        geom_point(data = hubway.stations, aes(x = lng, y = lat, 
                                               colour = pop.start$use_level,
                                               alpha = 0.9,
                                               size = factor(pop.start$n)))+
        theme(axis.title = element_blank(), legend.position = "none", 
              plot.margin = unit(c(0, 0, 0, 0), "lines"))+ 
        scale_size_discrete(range = c(1, 15))+
        scale_color_brewer(palette = "YlOrRd")
usemap
dev.off()
###############################################################################
#Are all of the Hubway rentals at 2:00am by people under 25?
#First create an age variable just to make things a little clearer
library(lubridate)
current.year <- year(Sys.Date())
hubway.trips2$age <- current.year - hubway.trips2$birth_date
#Filtering only those records with a birthdate and thus age (only registered
#users)
age.usage <- hubway.trips2 %>%
        filter(!is.na(birth_date))
age.usage$start.hour <- as.numeric(age.usage$start.hour)

at.two <- age.usage %>%
        filter(start.hour ==2)%>%
        count(age, start.hour)

ggplot(at.two, aes(x = age, y = n))+
        geom_histogram(stat = "identity")+
        scale_x_continuous(breaks = seq(20, 76, by = 2))+
        geom_vline(x = 25, colour = "red", linetype = "longdash", size = 1)+
        theme_bw()



##############################################################################
#Are there different top stations for male vs. female Hubway members?
#Gathering the gender and starting station data I need. Grouping by frequency
#of use by gender and station

gender <- hubway.trips2%>%
        filter(!is.na(gender))%>%
        count(gender, strt_statn)%>%
        filter(!is.na(strt_statn))%>%
        arrange(desc(n))
#Changing station id's to factors so I can join with gender dataframe
hubway.stations$id <- as.factor(hubway.stations$id)
#Joining gender information with station information
gender.station <- left_join(gender, hubway.stations,
                            by = c("strt_statn" = "id"))

#Initial plot of top 5 stations for each gender
#First selecting just the top 5 stations for each gender and putting in a new
#dataframe
gender.five <- gender.station%>%
        top_n(5, wt = n)

gender.five$station <- reorder(gender.five$station, gender.five$n)

#Plot
ggplot(gender.five, aes(x = station, y = n))+
        geom_point()+
        facet_wrap(~gender)+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90))+
        theme(axis.title.x = element_blank())+
        ylab("# of trips")





        



