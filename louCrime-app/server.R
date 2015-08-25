# library(data.table)
library(dplyr)
library(ggplot2)
library(ggmap)

# Read data in from csv
# louCrime <- fread('lou_shiny_data.csv', stringsAsFactors = FALSE, data.table = FALSE)
louCrime <- readRDS("Data/lou_shiny_data.rds")
louCrime <- as.data.frame(louCrime)

shinyServer(function(input, output){
  
  
  # Filtering Crime Data, return data frame
  crime <- reactive({
    # Temporary variables due to dplyr issue #318
    minYear <- input$year[1]
    maxYear <- input$year[2]
    
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
    
    # Optional: filter by month
    if(input$month != "All"){
      c <- c %>% filter(month_occured == input$month)
    }
    
    # Optional: filter by weekday
    if(input$weekday != "All"){
      c <- c %>% filter(weekday == input$weekday)
    }
    
    # Optional: filter by premise type
    if(input$premise != "all"){
      c <- c %>% filter(premise_type == input$premise)
    }
    c <- as.data.frame(c)
    c
  })
  
  
  # Plotting Louisville crime map
  output$map <- renderPlot({
    
    # Static map implementation (aside from zoom)  
    # TODO add reactivity to map parameters
    baseMap <- reactive({
      BM <- get_map(location = "louisville", source = "google",
                       zoom = input$zoom, # 10 is default city view. 11 seems better 
                       maptype = "roadmap",
                       color = "bw",
                       scale = 2) # high res image significantly clearer
      BM
    })
    # add Cartesian coordinates to enable more geoms
    
    bMap <- ggmap(baseMap(), extent = "panel") + coord_cartesian() 
    
    # subCrime <- crime()
    # Main ggplot object
    mapFinal <- bMap +
      
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