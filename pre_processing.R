library(tidyverse)
library(rgdal)
library(leaflet)

records <- read_csv(file.choose(), trim_ws = T)
records <- filter(records, !is.na(Locations))
records_sep <- separate(records, col="Locations", into=c("Lat","Long"), sep=",") 

write_csv(records_sep, path = "clean_ele_records.csv")

shp <-readOGR(file.choose())
shp_84 <- spTransform(shp, CRS("+init=epsg:4326"))

saveRDS(object = shp_84, file = "elephant_records.Rds")

