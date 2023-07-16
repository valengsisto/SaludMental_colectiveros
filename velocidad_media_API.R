library(httr)
library(jsonlite)
library(tidyverse)
library(janitor)
library(ggmap)
library(ggplot2)

url <- "https://apitransporte.buenosaires.gob.ar/colectivos/vehiclePositionsSimple?client_id=426a274e8e7045e28b6284b64127418d&client_secret=47c3e20972C543E3B1df96B2e791cBf8"

# el objetivo es obtener información
# sobre la velocidad promedio a la que
# van los colectivos en CABA y hacer un gráfico que
# la represente



respuesta <- GET(url = url)
status_code(respuesta) # verificamos que esté funcionando

raw_content <- rawToChar(respuesta$content) # pasamos el content a string

# pasamos el content a foramato json

content <- fromJSON(raw_content)
glimpse(content)

content$speed

min(content$speed)
max(content$speed)

# calculamos la velocidad promedio por línea

linea <- content %>% 
  arrange(route_short_name) %>% 
  group_by(agency_id,route_short_name) %>%
  summarise(velocidad_media = mean(speed)) %>%
  glimpse()

min(linea$velocidad_media)
max(linea$velocidad_media)
# las velocidades promedio van entre 0 y 26.5
# Para graficar la distribución de las 
# velocidades las agrupamos en 3 categorías:


linea$velocidad_agrup <- ifelse (linea$velocidad_media <= 8, "0-8 km/h",
                          ifelse (linea$velocidad_media > 8 & linea$velocidad_media <= 17, "9-17 km/h",
                          ifelse (linea$velocidad_media >= 18, "18-27 km/h", " ")))

# establecemos los colores para el gráfico
# según cuán lento van los colectivos

colores <- c("red", "green", "orange")
  
# Para poder hacer un gráfico que indique los porcentajes
# de cada categoría y que agrupe todas las líneas 
# según su pertenencia a uno de los tres grupos de velocidad media:
  
frecuencias_color <- table(linea$velocidad_agrup) # calculamos las frecuencias de cada categoría de velocidad
color_porcentajes <- prop.table(frecuencias_color) * 100 # ahora las calculamos en porcentaje

# creamos un df con estas dos columnas y los nombres de las categorías
df <- data.frame(Categoria = as.character(names(frecuencias_color)),
                 Frequency = as.factor(frecuencias_color),
                 Porcentaje = color_porcentajes)  

# gráfico 

  ggplot(data = df, aes(x = "", y = Porcentaje.Freq, fill = Categoria)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") + # para hacer un gráfico de torta
    scale_fill_manual(values = colores) +
    labs(fill = "Velocidad") +
    ggtitle("Velocidad promedio de las líneas de colectivo en CABA") +
    geom_text(aes(label = paste0(round(Porcentaje.Freq, 1), "%")),
              position = position_stack(vjust = 0.5)) +
    theme_void()
