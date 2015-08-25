library(ggplot2)

shinyUI(fluidPage(
  titlePanel("Louisville Crime Mapping"),
  fluidRow(
    column(3,
           wellPanel(
             h4("Filter"),
             sliderInput("year", "Display Years",
                         min = 1920, max = 2015, value = c(2014, 2015), step = 1),
             selectInput("month", "Display Months", 
                         choices = c("All", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")), # TODO expand to seasons. Need basic functionality first
             selectInput("weekday", "Day(s) of Week",
                         choices = c("All", "Sunday", "Monday", "Tuesday", "Wednesday",
                                     "Thursday", "Friday", "Saturday")),
             checkboxGroupInput("crime", "Select crimes to display",
                                choices = c("all", "arson", "assault", "burglary",
                                            "disturbing the peace", "drugs/alcohol violations",
                                            "dui", "fraud", "homicide", "motor vehicle theft",
                                            "other", "robbery", "sex crimes", "theft/larceny",
                                            "vandalism", "vehicle break-in/theft", "weapons"),
                                selected = "all"),
             selectInput("premise", "Crime Premise Type",
                         choices  = c("all", "abandoned/condemned structure", "air / bus / train terminal",
                                      "amusement park", "atm separate from bank", "attached residential garage",
                                      "auto dealership (new or used)", "bank / savings & loan",
                                      "bar / night club", "camp / campground",
                                      "child daycare facility", "church / synagogue / temple",
                                      "commercial / office building", "construction site",
                                      "convenience store", "department / discount store",
                                      "dock/wharf/freight/modal terminal", "drug store/dr`s office/hospital",
                                      "fairgrounds / stadium / arena", "farm facility",
                                      "field / woods", "government / public building", 
                                      "grocery / supermarket", "highway / road / alley",
                                      "homeless shelter / mission", "hotel / motel / etc.",
                                      "industrial site", "jail / penitentary", "lake / waterway",
                                      "liquor store", "mall / shopping center", "military installation",
                                      "non attached resd garage/shed/buld", "other / unknown",
                                      "other residence (apartment/condo)", "park / playground",
                                      "parking lot / garage", "race track/gambling facility",
                                      "rental / storage facility", "residence / home",
                                      "rest area", "restaurant", "school - college / university",
                                      "school - elementary / secondary", "service / gas station",
                                      "specialty store (tv, fur, etc)", "tribal lands"))
            )),
    mainPanel(
      plotOutput("map", width = "750px", "750px"),
      sliderInput("zoom", label = h3("Zoom"),
                  min = 9, max = 18, value = 11)
    )
  )
))