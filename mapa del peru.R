#######################################################################
# Mapa del Perú 
#
library(sf)
library(purrr)
library(tidyverse)
library(ggplot2)
library(ggrepel)

peru_d <- st_read("dp.shp")
head(peru_d)

ggplot(data = peru_d) +
  geom_sf()

ggplot(data = peru_d %>%
         filter(DEPARTAMEN=="PIURA")) +
  geom_sf()


#Centroides: Podemos crear un punto al centro de cada unidad, lo cual nos permitirá colocar el nombre de cada departamento

#Se crea el centroide
peru_d <- peru_d %>% mutate(centroid = map(geometry, st_centroid), coords = map(centroid, 
                                                                                st_coordinates), coords_x = map_dbl(coords, 1), coords_y = map_dbl(coords, 
                                                                                                                                                   2))

#Mapa con etiquetas de departamentos
ggplot(data = peru_d) +
  geom_sf(fill="skyblue", color="black")+ #Se le agrega un relleno celeste y bordes negros
  geom_text_repel(mapping = aes(coords_x, coords_y, label = DEPARTAMEN), size = 2.25) #Se inserta el nombre de cada departamento

peru_datos <- peru_d %>% #Juntamos ambas bases de datos
  left_join(contagios)

names(contagios)[1]<- c("DEPARTAMEN")

a<- ggplot(peru_datos) +
  geom_sf(aes(fill = n))+
  labs(title = "Porcentaje de población pobre por departamento (2018)",
       caption = "Fuente: Enaho (2018)
       Elaboración propia",
       x="Longitud",
       y="Latitud")+
  scale_fill_continuous(guide_legend(title = "Incidencia de la pobreza"))

ggplot(peru_datos) +
  geom_sf(aes(fill = n))+
  labs(title = "Porcentaje de población pobre por departamento (2018)",
       caption = "Fuente: Enaho (2018)
       Elaboración propia",
       x="Longitud",
       y="Latitud")+
  scale_fill_continuous(guide_legend(title = "Incidencia de la pobreza"))+
  geom_text_repel(mapping = aes(coords_x, coords_y, label = DEPARTAMEN), size = 2.25)

