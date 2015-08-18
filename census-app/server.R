#server.R
library(maps)
library(mapproj)
source("helpers.R")
counties <- readRDS("data/counties.rds")


shinyServer(
  function(input, output){
    output$map <- renderPlot({
      data <- switch(input$var,
                     "Percent White" = counties$white,
                     "Percent Black" = counties$black,
                     "Percent Hispanic" = counties$hispanic,
                     "Percent Asian" = counties$asian)
      
      color <- switch(input$var,
                      "Percent White" = "steelblue",
                      "Percent Black" = "black",
                      "Percent Hispanic" = "saddlebrown",
                      "Percent Asian" = "deeppink")
      
      legend <- switch(input$var,
                       "Percent White" = "% White",
                       "Percent Black" = "% Black",
                       "Percent Hispanic" = "% Hispanic",
                       "Percent Asian" = "% Asian")
      
      
      percent_map(var = data, color = color, legend.title = legend,
                  max = input$range[2], min = input$range[1])
    })
  }
)