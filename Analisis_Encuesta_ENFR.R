library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(rgdal)
library(janitor)

# queremos analizar algunas variables de salud mental según sexo
# usaremos la Encuesta Nacional de Factores de Riesgo (ENFR) del INDEC
# de los años 2013 (para consulta psicólogo) y 2018 (para sensación de ansiedad
# y depresión según sexo)

# leemos la base de datos de 2013
encuesta_salud_2013 <- read.table("ENFR2013_baseusuario.txt",sep="|",header=T)
#view(encuesta_salud_2013)

# BIAM01_03: En los últimos días, ¿consultó al
# psicólogo/psicoanalista/psiquiatra? 
#BHCH04: Sexo

dfreducido <- encuesta_salud_2013 %>% 
  select(BHCH04, BIAM01_03) %>%  #solo selecciono las columnas a analizar
  glimpse()

# etiquetamos las categorías

dfreducido$BHCH04<-as.character(dfreducido$BHCH04) 
dfreducido$BIAM01_03<-as.character(dfreducido$BIAM01_03) 
# pasamos las variable a chr para poder transformar a valores chr

dfreducido <- dfreducido %>% 
  mutate(sexo = case_when(BHCH04 == 1 ~ "Varon",
                          BHCH04 == 2 ~ "Mujer",
                          TRUE ~ BHCH04)) %>%
  select(!BHCH04) %>% 
  glimpse()

dfreducido <- dfreducido %>% 
  mutate(consulta_psi = case_when(BIAM01_03 == 1 ~ "Sí",
                                  BIAM01_03 == 2 ~ "No",
                          TRUE ~ BIAM01_03)) %>%
  select(!BIAM01_03) %>% 
  glimpse()

# Consulta al psicólogo/psiquiatra según sexo
consultaxsexo <- dfreducido %>%
tabyl(sexo, consulta_psi)
view(consultaxsexo)

# más del doble de las personas que consultan al psicólogo
# o al psiquiatra son mujeres

# leemos la base de datos de 2018

encuesta_salud_2018 <- read.table("ENFR 2018 - Base usuario.txt",sep="|",header=T)
#view(encuesta_salud_2018)

# bisg06: En relación a la ansiedad/depresión, ¿en el día de hoy…
# 1 no está ansioso ni deprimido?
# 2 está moderadamente ansioso o deprimido?
# 3 está muy ansioso o deprimido?
# bhch03: Sexo

dfreducido18 <- encuesta_salud_2018 %>% 
  select(bisg06, bhch03) %>%  #solo selecciono las columnas a analizar
  glimpse()

# etiquetamos las categorías

dfreducido18$bisg06<-as.character(dfreducido18$bisg06) 
dfreducido18$bhch03<-as.character(dfreducido18$bhch03) 
# pasamos las variable a chr para poder transformar a valores chr

dfreducido18 <- dfreducido18 %>% 
  mutate(sexo = case_when(bhch03 == 1 ~ "Varon",
                          bhch03 == 2 ~ "Mujer",
                          TRUE ~ bhch03)) %>%
  select(!bhch03) %>% 
  glimpse()

dfreducido18 <- dfreducido18 %>% 
  mutate(ansiedad_depresion = case_when(bisg06 == 1 ~ "Ni ansioso ni deprimido",
                                        bisg06 == 2 ~ "Moderadamente ansioso/deprimido",
                                        bisg06 == 3 ~ "Muy ansioso o deprimido",
                                  TRUE ~ bisg06)) %>%
  select(!bisg06) %>% 
  glimpse()

# Sensación de ansiedad/depresión según sexo
ansiedad_depresion_xsexo <- dfreducido18 %>%
  tabyl(sexo, ansiedad_depresion)
view(ansiedad_depresion_xsexo)
# El 66% de las personas que reportan estar “muy ansiosas o deprimidas” son mujeres

