library(shiny)
library(tidyverse)
library(shinythemes)
library(plotly)
library(leaflet)
library(rgdal)

protected_areas <- readOGR("Merge.shp")

records <- read.csv("elephant_records.csv", header = T, stringsAsFactors = FALSE)

record_type <- c("All", records$Occurence_type)
year_range <- c("All", "2001-2010", "2011-2020")
  
ui <- fluidPage(
  theme = shinytheme("journal"),
  titlePanel(title = "Elephant records"),
  tabsetPanel(
    id = "Ele_records",
    tabPanel(
      "Occurence records from news articles and blogs.",
      tags$br(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(inputId = "type", label = "Select incident type:", record_type),
          selectInput(inputId = "years", label = "Select year range:", year_range),
          submitButton(text = "Click to update map", icon = NULL, width = "100%"),
          br(),
          downloadButton("download_records", "Download data as CSV")
        ),
        mainPanel(leafletOutput("mymap", height = 580))
      )
    )
  )
)

server <- function(input, output) {
  
  ele_filter <- reactive({
    type <- input$type
    year <- input$years
    
    if(!identical(type, "All"))
    {
       if(identical(year, "2001-2010")){
        records %>% filter(Occurence_type == type) %>% filter(Date_range == 1) -> df_filt

      }
      else if(identical(year, "2011-2020")){
        records %>% filter(Occurence_type == type) %>% filter(Date_range == 2) -> df_filt
      }
      else{
        records %>% filter(Occurence_type == type) -> df_filt
      }
    }
    
    else{
      
      if(identical(year, "2001-2010")){
        print(year)
        records %>% filter(Date_range == 1) -> df_filt
      }
      else if(identical(year, "2011-2020")){
        print(year)
        records %>% filter(Date_range == 2) -> df_filt
      }
      else{
        print(year)
        records -> df_filt
      }
    }
    
  })
  
  elephantIcon <- makeIcon(
    iconUrl = "https://upload.wikimedia.org/wikipedia/commons/e/e5/Ele_pic.svg",
    iconWidth = 20, iconHeight = 12,
    iconAnchorX = 0, iconAnchorY = 0
  )
  
  output$mymap <- renderLeaflet({
    ele_df <- ele_filter()
    
    print(ele_df)
    points <- cbind(ele_df$Long, ele_df$Lat)
    
    leaflet(protected_areas)%>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addMarkers(data = points, popup = records$Link, icon = elephantIcon, label = records$Link)%>%
      addPolygons(color = "green", weight = 2, popup = "Protected areas", label = "Protected areas", 
                  highlightOptions = highlightOptions(color = "blue", weight = 2,
                                                      bringToFront = TRUE))
  })
  
  output$download_records <- downloadHandler(
    filename = "ele_records.csv", content = function(file) {
      write.csv(ele_filter(), file)
    }
  )
}

shinyApp(ui = ui, server = server)