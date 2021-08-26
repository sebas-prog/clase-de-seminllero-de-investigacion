library(maptools)  
library(RColorBrewer) 
library(readxl)
library(leaflet)
library(broom)
library(dplyr)
library(ggplot2)
library(classInt) 
library(sp)
library(rgdal)
library(spdep) 
library(ggpubr)
library(faraway)
library(mapproj)
library(rgeos)
#
setwd("C:/Users/atita/Desktop/departamentos")

peru_d <- st_read("dp.shp")

peru_prov  <- readShapePoly("dp.shp")

plot(peru_prov, col = "orange", bor = "lightgrey")

peru_provincias <- tidy(peru_prov) 

peru_provincias %>%
  ggplot() +
  geom_polygon(aes( x= long, y = lat, group = group),
               fill = "orange",
               color = "white",
               size = 0.05) +
  theme_void() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(colour= "darkgrey", size= 0.5)) +
  ggtitle("Perú. Provincias") +
  coord_map()


pal <- colorFactor(
  palette = "Paired",
  domain = peru_prov@data$DEPARTAMEN)

leaflet(peru_prov) %>%
  addTiles() %>%
  addPolygons(weight = 1,
              stroke = TRUE,
              color = "white",
              fillColor = ~pal(DEPARTAMEN),
              fillOpacity = 0.7,
              dashArray = "3",
              popup = ~paste(
                             "Departamento:", DEPARTAMEN),
              highlight = highlightOptions(
                weight = 2,
                dashArray = "",
                color = "grey",
                bringToFront = TRUE
              ))
