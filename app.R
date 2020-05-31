library(shiny)
library(tmap)
library(tmaptools)
library(tidyverse)
library(shinythemes)
library(plotly)
library(leaflet)
library(rgdal)

record_type <- c("All years","Conflict", "Sighting")
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
          selectInput(inputId = "type", label = "Select type of record:", record_type),
          selectInput(inputId = "years", label = "Select year:", year_range),
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
  points <- cbind(records$Long, records$Lat)
  
  greenLeafIcon <- makeIcon(
    iconUrl = "https://upload.wikimedia.org/wikipedia/commons/1/1f/Asian_Elephant_Icon.svg",
    iconWidth = 38, iconHeight = 38,
    iconAnchorX = 22, iconAnchorY = 94,
    shadowUrl = "https://upload.wikimedia.org/wikipedia/commons/1/1f/Asian_Elephant_Icon.svg",
    shadowWidth = 50, shadowHeight = 64,
    shadowAnchorX = 4, shadowAnchorY = 62
  )

  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points, popup = records$Title, icon = greenLeafIcon)
  })
}

shinyApp(ui = ui, server = server)