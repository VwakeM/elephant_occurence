library(shiny)
library(tidyverse)
library(shinythemes)
library(plotly)
library(leaflet)
library(rgdal)

states <- readOGR("Merge.shp")

records1 <- read.csv("elephant_records.csv", header = T, stringsAsFactors = FALSE)

record_type <- c("All", records1$Occurence_type)
year_range <- c("All", "2000-2010", "2011-2019")
  
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel(title = "Elephant records"),
  tabsetPanel(
    id = "Ele_records",
    tabPanel(
      "Occurence Maps",
      tags$br(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(inputId = "type", label = "Select type of incident:", record_type),
          selectInput(inputId = "years", label = "Select year range:", year_range),
          submitButton(text = "Click to load/refresh map", icon = NULL, width = "100%"),
          br(),
          downloadButton("download_records", "Download data as CSV")
        ),
        mainPanel(leafletOutput("mymap", height = 800))
      )
    )
  )
)

server <- function(input, output) {
  records <- read.csv("elephant_records.csv", header = T, stringsAsFactors = FALSE)
  
  ele_filter <- reactive({
    
    type <- input$type
    year <- input$years
    
    if(!identical(type, "All"))
    {
      records %>% filter(Occurence_type == type) -> df
    }
    
    else if(identical(type, "All")){
      records -> df
    }
    # 
    # if(identical(year, "2000-2010")){
    #  recors %>% filter()
    
    # }
    # else if(identical(year, "2000-2010")){
    #   
    # }
    
  })
  
  elephantIcon <- makeIcon(
    iconUrl = "https://upload.wikimedia.org/wikipedia/commons/1/1f/Asian_Elephant_Icon.svg",
    iconWidth = 15, iconHeight = 15,
    iconAnchorX = 0, iconAnchorY = 0,
    shadowUrl = "https://upload.wikimedia.org/wikipedia/commons/1/1f/Asian_Elephant_Icon.svg",
    shadowWidth = 15, shadowHeight = 15,
    shadowAnchorX = 0, shadowAnchorY = 0
  )
  
  output$mymap <- renderLeaflet({
    
    ele_df <- NULL
    ele_df <- ele_filter()
    
    points <- cbind(ele_df$Long, ele_df$Lat)
    
    leaflet(states)%>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addMarkers(data = points, popup = records$Title, icon = elephantIcon, label = records$Title)%>%
      addPolygons(color = "green", weight = 2, 
                  highlightOptions = highlightOptions(color = "blue", weight = 2,
                                                      bringToFront = TRUE))
  })
}

shinyApp(ui = ui, server = server)