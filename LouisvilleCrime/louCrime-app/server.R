# library(data.table)
library(dplyr)
library(ggplot2)
library(ggmap)

# Read data in from csv
# louCrime <- fread('lou_shiny_data.csv', stringsAsFactors = FALSE, data.table = FALSE)
louCrime <- readRDS("C:/RProgram/random_code/LouisvilleCrime/louCrime-app/Data/lou_shiny_data.rds")
louCrime <- as.data.frame(louCrime)

shinyServer(function(input, output){
  
  
  # Filtering Crime Data, return data frame
  crime <- reactive({
    # Temporary variables due to dplyr issue #318
    minYear <- input$year[1]
    maxYear <- input$year[2]
    # Month <- input$month
    # Weekday <- input$weekday
    # Premise <- input$premise
    # Crime <- input$crime
    
    # Apply filters
    c <- louCrime %>%
      filter(
        year_occured >= minYear,
        year_occured <= maxYear
      )
      
    # Optional: filter by crime type
    if(input$crime != "all"){
      c <- c %>% filter(crime_type == input$crime)
    }
    
    c <- as.data.frame(c)
    c
  })
  
  
  # Plotting Louisville crime map
  output$map <- renderPlot({
    
    # Static map implementation (aside from zoom)  
    # TODO add reactivity to map parameters
    baseMap <- reactive({get_map(location = "louisville", source = "google",
                       zoom = input$zoom, # 10 is default city view. 11 seems better 
                       maptype = "roadmap",
                       color = "bw",
                       scale = 2) # high res image significantly clearer
    })
    # add Cartesian coordinates to enable more geoms
    BM <- baseMap()
    baseMap <- ggmap(BM, extent = "panel") + coord_cartesian() 
    
    # subCrime <- crime()
    # Main ggplot object
    mapFinal <- baseMap +
      
      # Trying it out with points initially
      # I think density plots may be better
      stat_density2d(data = crime(), aes(x = lng,
                     y = lat,
                     fill = ..level.., # Not sure about this argument in geom_point
                     alpha = ..level..),
                     geom = "polygon")
    
    print(mapFinal)

    
  })

  
    
})