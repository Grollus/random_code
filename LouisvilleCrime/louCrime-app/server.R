library(data.table)
library(dplyr)
library(ggplot2)
library(ggmap)
# I think I need to set the working directory here
setwd("D:/RProgram/random_code/LouisvilleCrime")

# Read data in from csv
louCrime <- fread('lou_shiny_data.csv', stringsAsFactors = FALSE, data.table = FALSE)

# # reformat this for correct classes
# louCrime$date_occured <- as.POSIXct(louCrime$date_occured)
# louCrime$crime_type <- factor(louCrime$crime_type)
# louCrime$premise_type <- factor(louCrime$premise_type)
# louCrime$zip_code <- factor(louCrime$zip_code)
# louCrime$year_occured <- factor(louCrime$year_occured)
# louCrime$month_occured <- factor(louCrime$month_occured)
# louCrime$hour_occured <- factor(louCrime$hour_occured, ordered = ordered(louCrime$hour_occured))
# louCrime$nibrs_code <- factor(louCrime$nibrs_code)
# louCrime$weekday <- factor(louCrime$weekday)

# Grab louisville map from google api
lou_map <- get_map(location = "louisville", zoom = 11, color = "bw",
                   maptype = "roadmap", source = "google")


shinyServer(function(input, output){
  
  
  
  
  
  # Plotting Louisville crime map
  output$map <- renderPlot({
    
    # Static map implementation (aside from zoom)  
    # TODO add reactivity to map parameters
    baseMap <- get_map("louisville",
                       zoom = input$zoom, # 10 is default city view. 11 seems better 
                       maptype = "roadmap",
                      color = "bw",
                      scale = 2) # high res image significantly clearer
    
    # add Cartesian coordinates to enable more geoms
    baseMap <- ggmap(baseMap, extent = "panel") + coord_cartesian() 
    
    
    # Main ggplot object
    mapFinal <- baseMap +
      
      # Trying it out with points initially
      # I think density plots may be better
      geom_point(aes(x = lng_zip_code,
                     y = lat_zip_code,
                     fill = ..level.., # Not sure about this argument in geom_point
                     alpha = ..level..),
                 size = 

                     ))
    
    # 
    
  })

  
    
})