library(tidyverse)
library(dplyr)
library(sf)
library(ggplot2)
library(jsonlite)
library(geojsonsf)

# Explorando algunos datasets de Datos Abiertos Argentina

aportes <- read.csv("aportes_colectivoscd.csv", sep = ";")

lineas_nacionales <- read.csv("lineas_nacional (1).csv", sep = ";")

lineas_municipales <- read.csv("lineas_municipal (1).csv", sep = ";")

lineas_provinciales <- read.csv("lineas_provincial (1).csv", sep = ";")

lineas_m <- st_read("lineasmunicipales.kml")
lineas_n <- st_read("lineasnacionales.kml")
lineas_p <- st_read("lineasprovinciales.kml")

# Queremos plotear las líneas sobre el mapa de la RMBA

geo <- geojson_sf("RMBA.geojson")
  
st_crs(geo)
st_crs(lineas_n)
st_crs(lineas_m)
st_crs(lineas_p)

# chequeamos los sistemas de coordenadas de los 
# archivos geográficos
# todos son 4326

#  Gráfico de las líneas municipales, provinciales y
# nacionales en la RMBA

mapa_lineas_nacionales <- ggplot(data = geo) +
  geom_sf(color = "black") +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Mapa de las líneas nacionales en la RMBA") +
  geom_sf(data = lineas_n, color = "red") 

mapa_lineas_provinciales <- ggplot(data = geo) +
  geom_sf(color = "black") +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Mapa de las líneas provinciales en la RMBA") +
  geom_sf(data = lineas_p, color = "green")

mapa_lineas_municipales <- ggplot(data = geo) +
  geom_sf(color = "black") +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Mapa de las líneas municipales en la RMBA") +
  geom_sf(data = lineas_m, color = "yellow")

