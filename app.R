library(shiny)
library(tidyverse)
library(shinythemes)
library(plotly)
library(leaflet)
library(rgdal)

protected_areas <- readOGR("Merge.shp")
reserve_forest <- readOGR("RF.shp")


records <- read.csv("elephant_records.csv", header = T, stringsAsFactors = FALSE)

record_type <- c("All", str_trim(records$Occurence_type))
year_range <- c("All", "2001-2010", "2011-2020")

infra_type <- c("All","Dams", "Mini Hydel", "National Highway", "Small Roads", "Kaccha Roads", "Powerline", "Pipeline")
  
ui <- fluidPage(
  theme = shinytheme("journal"),
  titlePanel(title = "Western Ghats"),
  tabsetPanel(
    id = "Ele_records",
    tabPanel(
      "Occurence records from news articles and blogs",
      tags$br(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(inputId = "type", label = "Incident Type:", record_type),
          selectInput(inputId = "years", label = "Year range:", year_range),
          submitButton(text = "Click to update map", icon = NULL, width = "100%"),
          br()
         ## downloadButton("download_records", "Download data as CSV")
        ),
        mainPanel(leafletOutput("EOMap", height = 580))
      )
    ),
    tabPanel(
      "Linear Infrastructure",
      tags$br(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(inputId = "type", label = "Infrastructure Type:", infra_type),
          submitButton(text = "Click to update map", icon = NULL, width = "100%"),
          br()
          ##downloadButton("download_records", "Download data as CSV")
        ),
        mainPanel(leafletOutput("LIMap", height = 580))
      )
    )
    ,
    tabPanel(
      "Forest diverted for Infrastructure",
      tags$br(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(inputId = "type", label = "Infrastructure Type:", infra_type),
          submitButton(text = "Click to update map", icon = NULL, width = "100%"),
          br()
          ##downloadButton("download_records", "Download data as CSV")
        ),
        mainPanel(plotlyOutput("plotGraph", width="500px",height="400px") )
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
        ##print(year)
        records %>% filter(Date_range == 1) -> df_filt
      }
      else if(identical(year, "2011-2020")){
        ##print(year)
        records %>% filter(Date_range == 2) -> df_filt
      }
      else{
        ##print(year)
        records -> df_filt
      }
    }
  })
  dam_locations <- readOGR("DamLocations.shp")
  
  infra_filter <- reactive({
    type <- input$type
    
  
      if(identical(type, "Mini Hydel")){
        dam_locations <- readOGR("DamLocations.shp")
      }
     
      else{
        dam_locations <- readOGR("RF.shp")
      }
    
  })
  
  elephantIcon <- makeIcon(
    iconUrl = "https://upload.wikimedia.org/wikipedia/commons/e/e5/Ele_pic.svg",
    iconWidth = 20, iconHeight = 12,
    iconAnchorX = 0, iconAnchorY = 0
  )
  
  output$EOMap <- renderLeaflet({
    ele_df <- ele_filter()
    
    ##print(ele_df)
    points <- cbind(ele_df$Long, ele_df$Lat)
    
   records <- records %>% dplyr::mutate(Link = paste0('<a href="',records$Link,'"target="_blank">',records$Title,'</a>.'))
    
   leaflet()%>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addMarkers(data = points,popup=records$Link, icon = elephantIcon, label = records$Title)%>%
      addPolygons(data = protected_areas, color = "green", weight = 1, popup = "Protected areas", label = "Protected areas", 
                  highlightOptions = highlightOptions(color = "blue", weight = 2, bringToFront = TRUE)) %>%
     addPolygons(data = reserve_forest, color = "green", weight = 1, popup = reserve_forest$RF_NAME, label = reserve_forest$RF_NAME, 
                 highlightOptions = highlightOptions(color = "blue", weight = 2, bringToFront = TRUE))
     
  })
  output$LIMap <- renderLeaflet({
    
    leaflet()%>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addCircles(data = dam_locations, radius =100, color = "#292626",fillColor =  "#292626",  popup=dam_locations$Name, label = dam_locations$Name)%>%
      addPolygons(data = protected_areas, color = "green", weight = 1, popup = "Protected areas", label = "Protected areas", 
                  highlightOptions = highlightOptions(color = "blue", weight = 2, bringToFront = TRUE)) %>%
      addPolygons(data = reserve_forest, color = "green", weight = 1, popup = reserve_forest$RF_NAME, label = reserve_forest$RF_NAME, 
                  highlightOptions = highlightOptions(color = "blue", weight = 2, bringToFront = TRUE))
    
  })
  
  output$download_records <- downloadHandler(
    filename = "ele_records.csv", content = function(file) {
      write.csv(ele_filter(), file)
    }
  )
}

shinyApp(ui = ui, server = server)