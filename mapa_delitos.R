library(tidyverse)
library(dplyr)
library(sf)
library(ggplot2)
library(jsonlite)
library(httr)
library(geojsonsf)
library(geoAr)
library(rgdal)

# Vamos a plotear el mapa de líneas de colectivos de CABA
# sobre el mapa de delitos de CABA (2021) para ver en 
# qué medida coinciden

# Leemos el csv de delitos

delitos21_raw <- read.csv("delitos_2021.csv", sep = ";")

glimpse(delitos21_raw) # latitud y longitud están en chr

# pasamos ambas columnas a numericas para poder
# establecerlas como geometría 

clean_delitos21 <- delitos21_raw %>% 
  janitor::clean_names() %>% 
  drop_na(longitud,latitud) %>%
  # reemplazamos comas por puntos
  mutate(longitud = str_replace(string = longitud, pattern = ",", replacement = "."), 
         latitud = str_replace(string =latitud, pattern = ",", replacement = ".")) %>% 
  mutate(longitud = as.numeric(longitud), 
         latitud = as.numeric(latitud)) %>% 
  drop_na(latitud, longitud) %>% 
  glimpse()

# ambas columnas son de tipo numéricas ahora:
min(clean_delitos21$longitud)
max(clean_delitos21$latitud)

# se transforma el csv en un formato geográfico

delitos21_geo  <- clean_delitos21 %>% 
  st_as_sf(coords = c("longitud", "latitud")) %>% 
  glimpse()

# todavía no tiene un sistema de coodernadas

st_crs(delitos21_geo)

# seteamos el sistema de coordenadas

delitos_21geo <- delitos21_geo %>% 
  st_set_crs(value = 4326) 

st_crs(delitos_21geo) # verificamos

# mapa de delitos 

mapa_delitos <- ggplot(data = delitos_21geo) +
  geom_sf(aes(), alpha = 0.03)

# mapa de delitos según la cantidad
mapa_delitos_cant <- ggplot(data = delitos_21geo) +
  geom_sf(aes(size = cantidad), alpha = 0.5) 

# se baja el alpha para una correcta visualización


# leemos el shp de lineas CABA

lineas_caba <- st_read("Lineas_CABA_BADATA.shp")

# mapa de las lineas:

mapa_lineas <- ggplot(data = lineas_caba) +
  geom_sf(aes(color=agency_id))

# Lineas de CABA sobre mapa delitos CABA

ggplot() +
  geom_sf(data = delitos_21geo, color = "black") +
  geom_sf(data = lineas_caba, color = "red")
# el mapa de líneas tapa el mapa de delitos CABA

# Hay que calcular el área de los dos datasets
# para establecer los límites correctamente
# y que al plotear un mapa sobre el otro, se visualice bien

# para asegurarnos que tengan el mismo sist de coordenadas:
datos <- st_transform(delitos_21geo, crs = st_crs(lineas_caba))

limites <- st_bbox(datos)

# ploteamos el mapa de líneas sobre el de delitos

ggplot() +
  geom_sf(data = delitos_21geo, color = "black", alpha = 0.5) +
  geom_sf(data = lineas_caba, color = "red", size = 0.01) +
  coord_sf(xlim = c(limites["xmin"], limites["xmax"]),
           ylim = c(limites["ymin"], limites["ymax"]),
           expand = FALSE) +
  theme_void()

