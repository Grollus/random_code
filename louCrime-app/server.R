# library(data.table)
library(dplyr)
library(ggplot2)
library(ggmap)

# Read data in from csv
# louCrime <- fread('lou_shiny_data.csv', stringsAsFactors = FALSE, data.table = FALSE)
louCrime <- readRDS("Data/lou_shiny_data.rds")
louCrime <- as.data.frame(louCrime)
louCrime$month_occured <- factor(louCrime$month_occured, levels = c("Jan", "Feb", "Mar",
                                                                    "Apr", "May", "Jun",
                                                                    "Jul", "Aug", "Sep",
                                                                    "Oct", "Nov", "Dec"))
louCrime$weekday <- factor(louCrime$weekday, levels = c("Sunday", "Monday", "Tuesday",
                                                        "Wednesday", "Thursday", "Friday",
                                                        "Saturday"))

shinyServer(function(input, output){
  
  # Geocode input location for base maps
  bMap_geocode <- reactive({
    data.frame(geocode = geocode(paste(input$location, "Louisville, KY"), source = "google"))
    
  })
  # Filtering Crime Data, return data frame
  crime <- reactive({
    
    # Apply filters
    c <- louCrime %>%
      filter(
        year_occured >= input$year[1],
        year_occured <= input$year[2]
      )
    # Optional: filter by zip_code
    if(input$location != "Louisville" & input$location %in% c$zip_code){
      c <- c %>% filter(zip_code == input$location)
    }
    
    # Optional: filter by crime type
    ifelse(input$crime != "All", c <- c %>% filter(crime_type %in% input$crime), c)
    
    # Optional: filter by month
    if(input$month != "All"){
      c <- c %>% filter(month_occured == input$month)
    }
    
    # Optional: filter by weekday
    if(input$weekday != "All"){
      c <- c %>% filter(weekday == input$weekday)
    }
    
    # Optional: filter by premise type
    ifelse(input$premise != "All", c <- c %>% filter(premise_type %in% input$premise), c)
    
    c <- as.data.frame(c)
    c
  })
  
  
  # Plotting Louisville crime map
  output$map <- renderPlot({
    
    # Get base map; reactivity based on zoom input
    temp_bMap_geocode <- bMap_geocode()
    
    baseMap <- reactive({
      
      base <- get_map(location = as.matrix(temp_bMap_geocode), source = "google",
                       zoom = input$zoom, # 10 is default city view. 11 seems better 
                       maptype = "roadmap",
                       color = "bw",
                       scale = 2) # high res image significantly clearer
      base
    })
    # add Cartesian coordinates to enable more geoms
    
    bMap <- ggmap(baseMap(), extent = "panel") + coord_cartesian() 
    
    # Main ggplot object
    mapFinal <- bMap +
      
      # Trying it out with points initially
      # I think density plots may be better
      stat_density2d(data = crime(), aes(x = lng,
                     y = lat,
                     fill = ..level.., # Not sure about this argument in geom_point
                     alpha = ..level..),
                     geom = "polygon",
                     bins = input$bins,
                     size = input$boundwidth) +
      
      # Configuring scale and panel
      scale_alpha(range = input$alpharange) +
      scale_fill_gradient(low = input$low, high = input$high) +
      
      # Title and labels
      labs(x = "Longitude", y = "Latitude") + 
      ggtitle(paste("Crimes in Louisville from ", input$year[1], " to ", 
                    input$year[2], sep = "")) + 
      
      # Other plot details
      theme_bw() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        legend.position = "none"
      )
    
    # Faceting 
    if(input$facet == "no faceting") {mapFinal}
    if(input$facet == "month") {mapFinal <- mapFinal + facet_wrap(~ month_occured)}
    if(input$facet == "year") {mapFinal <- mapFinal + facet_wrap(~ year_occured)}
    if(input$facet == "weekday") {mapFinal <- mapFinal + facet_wrap(~ weekday)}
    suppressWarnings(print(mapFinal))
  })
  
  
})