library(tidyverse)
library(skimr)
library(janitor)
library (dplyr)
library (ggplot2)
library (readxl)

#leemos el libro de códigos que usaremos para etiquetar las variables de la encuesta

codigos_clasificacion_sectores <- read_excel("ECETSS_Diseño_registro.xlsx", sheet= 4)
codigos <- read_excel("ECETSS_Diseño_registro.xlsx", sheet= 9)

# leemos el csv con la encuesta
base_ocupados18 <- read_csv("ecetss_ocupados.csv")

base_ocupados18 <- clean_names (base_ocupados18)
base_ocupados18

base_ocupados18_clean <- remove_empty(base_ocupados18, which = c("rows", "cols"), quiet = FALSE)
# no hay filas vacías a eliminar. Hay una columna vacía a eliminar (c2p10_32_7)

# buscando filas duplicadas

base_ocupados18_clean <- distinct (base_ocupados18_clean)
base_ocupados18_clean

glimpse (base_ocupados18_clean)

# seleccionamos la columna ramacaes_o (clasificación de las actividades económicas)

base_ocupados18_clean$ramacaes_o<-as.character(base_ocupados18_clean$ramacaes_o)
glimpse (base_ocupados18_clean)

# pasamos la variable ramacaes_o a character para poder unificarla con
# categorìas character en la nueva variable

unique(base_ocupados18_clean$ramacaes_o) 

ocupados18_valores <- base_ocupados18_clean %>%
  mutate(sector = case_when(ramacaes_o == 1 ~ "Actividades primarias",
                              ramacaes_o == 2 ~ "Industria manufacturera",
                              ramacaes_o == 3 ~ "Construcción",
                              ramacaes_o == 4 ~ "Comercio",
                              ramacaes_o == 5 ~ "Hoteles y restaurantes",
                              ramacaes_o == 6 ~ "Transporte alm y comunic",
                              ramacaes_o == 7 ~ "Serv financ inm alq y emp",
                              ramacaes_o == 8 ~ "Admin pública y defensa",
                              ramacaes_o == 9 ~ "Enseñanza",
                              ramacaes_o == 10 ~ "Servicios sociales y de salud",
                              ramacaes_o == 11 ~ "Trabajo doméstico",
                              ramacaes_o == 12 ~ "Otros serv comunit soc y per",
                              ramacaes_o == 13 ~ "Otras ramas",
                              ramacaes_o == 99 ~ "Sin especificar",
                              TRUE ~ ramacaes_o)) %>%
  select(!ramacaes_o) %>% # eliminamos la columna original
  glimpse()
                              
ocupados18_valores$sector # chequeamos la nueva columna

# analizando variables de riesgo psicosocial según sector de actividad

# 1) variables a analizar: sector y exigencia cognitiva 
# variable exigencia cognitiva: ¿Con qué frecuencia su trabajo exige que tenga 
# que controlar muchas cosas a la vez?

muchascosas_alavez <- ocupados18_valores %>% 
  select(exig_cogni, sector) %>%  #solo selecciono las columnas de exigencia cognitiva y sector
  glimpse()

unique(muchascosas_alavez$exig_cogni)

muchascosas_alavez$exig_cogni<-as.character(muchascosas_alavez$exig_cogni)
# pasamos la variable exig_cogni a character para poder unificarla con categorìas character en la nueva variable
glimpse(muchascosas_alavez)

# etiquetamos las categorías de la variable
muchascosas_alavez_valores <- muchascosas_alavez %>%
  mutate(exigencia_cognit = case_when(exig_cogni == 1 ~ "Siempre",
                                      exig_cogni == 2 ~ "Muchas veces",
                                      exig_cogni == 3 ~ "Algunas veces",
                                      exig_cogni == 4 ~ "Sólo alguna vez",
                                      exig_cogni == 5 ~ "Nunca",
                                      exig_cogni == 99 ~ "Ns/nc",
                                      TRUE ~ exig_cogni)) %>%
  select(!exig_cogni) %>% # eliminamos la columna original
  glimpse()


# tabla cruzada entre las variables sector y exigencia cognitiva en absolutos
exigencia_por_sector <- muchascosas_alavez_valores %>%
  tabyl(sector, exigencia_cognit)
#view(exigencia_por_sector)

# tabla cruzada entre las variables sector y exigencia cognitiva en porcentajes

exigencia_por_sector_pct <- exigencia_por_sector %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) # %>%
  #adorn_ns() para mantener también los absolutos
#view(exigencia_por_sector_pct)

# gráfico de barras comparando la incidencia de la exigencia cognitiva
# según sector

ggplot(data = muchascosas_alavez_valores, aes(x = sector, y= exigencia_cognit, fill = exigencia_cognit)) +
  geom_col() +
  coord_flip() 

# analizando sólo el sector transporte, almacenamiento y comunicación

exig_cognit_transporte <- muchascosas_alavez_valores %>%
  filter(sector == "Transporte alm y comunic")

#view(exig_cognit_transporte)

# distribución de frecuencias absoluta

frecuencias_exig_cognit <- table(exig_cognit_transporte) 
#view(frecuencias_exig_cognit)

# distribución de frecuencias porcentual

frecuencias_egix_cognit_pct <- (frecuencias_exig_cognit / sum (frecuencias_exig_cognit))*100 
#view(frecuencias_egix_cognit_pct)


# graficamos la exigencia cognitiva en el sector de transporte, almacenamiento y comunicación

barplot(frecuencias_egix_cognit_pct, ylab = "Porcentaje", las = 2, cex.names =.7,5, main = "Nivel de exigencia cognitiva en el sector transporte", cex.main = 1) # rotamos las etiquetas de las categorías y achicamos la letra para que se visualice bien

exigcognit_t_df <- as.data.frame(frecuencias_egix_cognit_pct)

# eliminamos la columna ns/nc
exigcognit_t_df <- exigcognit_t_df[!(exigcognit_t_df$exigencia_cognit=="Ns/nc"),]

grafico <- ggplot(exigcognit_t_df,
                  aes(x=exigencia_cognit, y = Freq, fill= exigencia_cognit)) +
  geom_bar(stat= "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("Nunca" = "green",
                               "Sólo alguna vez" = "green",
                               "Ns/nc" = "grey",
                               "Siempre" = "red",
                               "Muchas veces" = "red",
                               "Algunas veces" = "red")) +
  ylab("Porcentaje") +
  xlab("Frecuencia") +
  ggtitle ("Nivel de exigencia cognitiva entre los trabajadores del transporte") +
  theme_minimal() +
  geom_text(aes(label = exigencia_cognit), size = 6, nudge_y = 1)

# reordenando las barras:

otro_orden <- c("Siempre", "Algunas veces", "Muchas veces", "Nunca", "Sólo alguna vez")

exigcognit_t_df$exigencia_cognit <- factor(exigcognit_t_df$exigencia_cognit, levels = otro_orden)

grafico <- ggplot(exigcognit_t_df,
                  aes(x=exigencia_cognit, y = Freq, fill= exigencia_cognit)) +
  geom_bar(stat= "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("Nunca" = "green",
                               "Sólo alguna vez" = "green",
                               "Ns/nc" = "grey",
                               "Siempre" = "red",
                               "Muchas veces" = "red",
                               "Algunas veces" = "red")) +
  ylab("Porcentaje") +
  xlab("Frecuencia") +
  ggtitle ("Nivel de exigencia cognitiva entre los trabajadores del transporte") +
  theme_classic() +
  geom_text(aes(label = exigencia_cognit), size = 6, nudge_y = 1)


# 2) variables a analizar: sector y exigencia emocional 1 
# exig_emoc_1: ¿Con qué frecuencia su trabajo exige que esconda sus emociones o sentimientos?

exig_emocional1 <- ocupados18_valores %>% 
  select(exig_emoc_1, sector) %>%  #solo selecciono las columnas de exigencia emocional 1 y sector
  glimpse()

unique(exig_emocional1$exig_emoc_1)

exig_emocional1$exig_emoc_1<-as.character(exig_emocional1$exig_emoc_1)

# etiquetamos las categorías de la variable
exig_emocional1_valores <- exig_emocional1 %>%
  mutate(exigencia_emocional = case_when(exig_emoc_1 == 1 ~ "Siempre",
                                         exig_emoc_1  == 2 ~ "Muchas veces",
                                         exig_emoc_1  == 3 ~ "Algunas veces",
                                         exig_emoc_1 == 4 ~ "Sólo alguna vez",
                                         exig_emoc_1  == 5 ~ "Nunca",
                                      exig_emoc_1 == 99 ~ "Ns/nc",
                                      TRUE ~ exig_emoc_1)) %>%
  select(!exig_emoc_1) %>% # eliminamos la columna original
  glimpse()

# comparando la exigencia emocional 1 por sector

ggplot(data = exig_emocional1_valores, aes(x = sector, y= exigencia_emocional, fill = exigencia_emocional)) +
geom_col() +
coord_flip() 

# tabla cruzada entre las variables sector y exigencia emocional 1 en absolutos

exigencia_emocional_porsector <- exig_emocional1_valores %>%
  tabyl(sector, exigencia_emocional)  # adorn_title para mantener la variable de la columna

#view(exigencia_emocional_porsector) 

# tabla cruzada entre las variables sector y exigencia emocional 1 en porcentajes

exigencia_emocional_porsector_pct <- exigencia_emocional_porsector %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2)
  #  adorn_ns()

#view(exigencia_emocional_porsector_pct)

# análisis de la exigencia emocional sólo en el sector transporte

exig_emocional1_transporte <- exig_emocional1_valores %>%
  filter(sector == "Transporte alm y comunic")

#view(exig_emocional1_transporte)

# distribución de frecuencias absoluta

frecuencias_exig_emocional <- table(exig_emocional1_transporte) 

# distribución de frecuencias porcentual

frecuencias_pct_emocional1 <- (frecuencias_exig_emocional / sum (frecuencias_exig_emocional))*100 

#view(frecuencias_pct_emocional1)

# graficamos la exigencia emocional 1 en el sector de transporte, almacenamiento y comunicación

barplot(frecuencias_pct_emocional1, ylab = "porcentaje", las = 2, cex.names =.7,5, main = "Nivel de exigencia emocional en el sector transporte", cex.main = 1) # rotamos las etiquetas de las categorías y achicamos la letra para que se visualice bien


# 3 variables a analizar: sector y exig_emoc_2
# exig_emoc_2: ¿Su trabajo es emocionalmente desgastador?

exig_emocional2 <- ocupados18_valores %>% 
  select(exig_emoc_2, sector) %>%  #solo selecciono las columnas de exigencia emocional2 y sector
  glimpse()

unique(exig_emocional2$exig_emoc_2)

exig_emocional2$exig_emoc_2<-as.character(exig_emocional2$exig_emoc_2)

exig_emocional2_valores <- exig_emocional2 %>%
  mutate(exigencia_emocional2 = case_when(exig_emoc_2 == 1 ~ "Siempre",
                                          exig_emoc_2 == 2 ~ "Muchas veces",
                                          exig_emoc_2 == 3 ~ "Algunas veces",
                                          exig_emoc_2 == 4 ~ "Sólo alguna vez",
                                          exig_emoc_2 == 5 ~ "Nunca",
                                          exig_emoc_2 == 99 ~ "Ns/nc",
                                      TRUE ~ exig_emoc_2)) %>%
  select(!exig_emoc_2) %>% # eliminamos la columna original
  glimpse()

# tabla cruzada entre las variables sector y exigencia emocional 2 en absolutos

exigencia_emocional2_porsector_abs <- exig_emocional2_valores %>%
  tabyl(sector, exigencia_emocional2)
#view(exigencia_emocional2_porsector_abs)

# tabla cruzada entre las variables sector y exigencia emocional 2 en porcentajes

exigencia_emocional2_porsector_pct <- exigencia_emocional2_porsector_abs %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) # %>%
#adorn_ns() para mantener también los absolutos
#view(exigencia_emocional2_porsector_pct)

# analizando sólo el sector transporte, almacenamiento y comunicación

exig_emocional2_transporte <- exig_emocional2_valores %>%
  filter(sector == "Transporte alm y comunic")

#view(exig_emocional2_transporte)

# distribución de frecuencias absoluta

frecuencias_exig_emocional2 <- table(exig_emocional2_transporte) 
#view(frecuencias_exig_emocional2)

# distribución de frecuencias porcentual

frecuencias_pct_emocional2 <- (frecuencias_exig_emocional2 / sum (frecuencias_exig_emocional2))*100 
#view(frecuencias_pct_emocional2)

# graficamos la exigencia emocional 2 en el sector de transporte, almacenamiento y comunicación
barplot(frecuencias_pct_emocional2, ylab = "porcentaje", las = 2, cex.names =.7,5, main = "Nivel de desgaste emocional en el sector transporte", cex.main = 1, col = "blue") # rotamos las etiquetas de las categorías y achicamos la letra para que se visualice bien

frecuencias_pct_emocional2_df <- as.data.frame(frecuencias_pct_emocional2)
#view(frecuencias_pct_emocional2_df)


grafico <- ggplot(frecuencias_pct_emocional2_df,
                  aes(x=exigencia_emocional2, y = Freq, fill = exigencia_emocional2)) +
  geom_bar(stat= "identity", show.legend = FALSE) +
  ylab("Porcentaje") +
  xlab ("Frecuencia") +
  ggtitle("Nivel de desgaste emocional según sector") +
  theme_minimal() 

# 4) variables a analizar: sector y agresión externa
# variable agres ext: En su puesto de trabajo /actividad en el último año,
# ¿con qué frecuencia ud. fue objeto de situaciones de agresión de los clientes, 
# pacientes, alumnos, público, etc.?

agres_ext <- ocupados18_valores %>% 
  select(agres_ext, sector) %>%  #solo selecciono las columnas de agresión externa y sector
  glimpse()

unique(agres_ext$agres_ext)

agres_ext$agres_ext<-as.character(agres_ext$agres_ext)
# pasamos la variable agres_ext a character para poder unificarla con categorìas character en la nueva variable
glimpse(agres_ext)

# etiquetamos las categorías de la variable 

agres_ext_valores <- agres_ext %>%
  mutate(agres_externa = case_when(agres_ext == 1 ~ "Siempre",
                                    agres_ext == 2 ~ "Muchas veces",
                                    agres_ext == 3 ~ "Algunas veces",
                                    agres_ext == 4 ~ "Sólo alguna vez",
                                    agres_ext == 5 ~ "Nunca",
                                    agres_ext == 6 ~ "No corresponde",
                                    agres_ext == 99 ~ "Ns/nc",
                                      TRUE ~ agres_ext)) %>%
  select(!agres_ext) %>% # eliminamos la columna original
  glimpse()


# tabla cruzada entre las variables sector y agresión externa en absolutos

agres_ext_xsector <- agres_ext_valores %>%
  tabyl(sector, agres_externa)
#view(agres_ext_xsector)

# tabla cruzada entre las variables sector y agresión externa en porcentajes

agres_ext_xsector_pct <- agres_ext_xsector %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) # %>%
#adorn_ns() para mantener también los absolutos
#view(agres_ext_xsector_pct)

# analizando sólo el sector transporte, almacenamiento y comunicación

agres_ext_transporte <- agres_ext_valores %>%
  filter(sector == "Transporte alm y comunic")

#view(agres_ext_transporte)

# distribución de frecuencias absoluta

frecuencias_agres_ext <- table(agres_ext_transporte) 
#view(frecuencias_agres_ext)

# distribución de frecuencias porcentual

frecuencias_agres_ext_pct <- (frecuencias_agres_ext / sum (frecuencias_agres_ext))*100 
print(frecuencias_agres_ext_pct)
#view(frecuencias_agres_ext_pct)

# graficamos la agresión externa en el sector de transporte, almacenamiento y comunicación

barplot(frecuencias_agres_ext_pct, ylab = "porcentaje", las = 2, cex.names =.7,5, main = "Nivel de agresión externa en el sector transporte", cex.main = 1) # rotamos las etiquetas de las categorías y achicamos la letra para que se visualice bien

# 5) variables a analizar: sector y apoyo de compañeros
# variable apoyo_comp: ¿Con qué frecuencia obtiene ayuda y/o apoyo de sus compañeros
# de trabajo en la realizacion de sus tareas?

apoyo_comp <- ocupados18_valores %>% 
  select(apoyo_comp, sector) %>%  #solo selecciono las columnas de apoyo de compañeros y sector
  glimpse()

unique(apoyo_comp$apoyo_comp)

# pasamos la variable apoyo_comp a character para poder unificarla con categorìas character en la nueva variable

apoyo_comp$apoyo_comp<-as.character(apoyo_comp$apoyo_comp)
glimpse(apoyo_comp)

# etiquetamos las categorías de la variable

apoyo_comp_valores <- apoyo_comp %>%
  mutate(apoyo_compa = case_when(apoyo_comp == 1 ~ "Siempre",
                                apoyo_comp == 2 ~ "Muchas veces",
                                apoyo_comp == 3 ~ "Algunas veces",
                                apoyo_comp == 4 ~ "Sólo alguna vez",
                                apoyo_comp == 5 ~ "Nunca",
                                apoyo_comp == 6 ~ "No corresponde",
                                apoyo_comp == 99 ~ "Ns/nc",
                                    TRUE ~ apoyo_comp)) %>%
  select(!apoyo_comp) %>% # eliminamos la columna original
  glimpse()


# tabla cruzada entre las variables sector y apoyo de compañeros en absolutos
apoyo_comp_xsector <- apoyo_comp_valores %>%
  tabyl(sector, apoyo_compa)
#view(apoyo_comp_xsector)

# tabla cruzada entre las variables sector y apoyo de compañeros en porcentajes

apoyo_comp_xsector_pct <- apoyo_comp_xsector %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) # %>%
#adorn_ns() para mantener también los absolutos
#view(apoyo_comp_xsector_pct)

# analizando sólo el sector transporte, almacenamiento y comunicación

apoyo_comp_transporte <- apoyo_comp_valores %>%
  filter(sector == "Transporte alm y comunic")
#view(apoyo_comp_transporte)

# distribución de frecuencias absoluta

frecuencias_apoyo_comp <- table(apoyo_comp_transporte) 
#view(frecuencias_apoyo_comp)

# distribución de frecuencias porcentual

frecuencias_apoyo_comp_pct <- (frecuencias_apoyo_comp / sum (frecuencias_apoyo_comp))*100 
#view(frecuencias_apoyo_comp_pct) # La opción "no corresponde" representa el 30% de los casos

# 6) variables a analizar: sector y recomp valorac: 
# variable recomp valorac: ¿Con qué frecuencia su trabajo es valorado?

recomp_valorac <- ocupados18_valores %>% 
  select(recomp_valorac, sector) %>%  #solo selecciono las columnas de apoyo de compañeros y sector
  glimpse()

unique(recomp_valorac$recomp_valorac)

recomp_valorac$recomp_valorac<-as.character(recomp_valorac$recomp_valorac)
glimpse(recomp_valorac)

# etiquetamos las categorías de la variable

recomp_valorac_valores <- recomp_valorac %>%
  mutate(valoracion = case_when(recomp_valorac == 1 ~ "Siempre",
                                recomp_valorac == 2 ~ "Muchas veces",
                                recomp_valorac == 3 ~ "Algunas veces",
                                recomp_valorac == 4 ~ "Sólo alguna vez",
                                recomp_valorac == 5 ~ "Nunca",
                                recomp_valorac == 6 ~ "No corresponde",
                                recomp_valorac == 99 ~ "Ns/nc",
                                 TRUE ~ recomp_valorac)) %>%
  select(!recomp_valorac) %>% # eliminamos la columna original
  glimpse()

recomp_valorac_valores

# tabla cruzada entre las variables sector y valoración del trabajo en absolutos

recomp_xsector <- recomp_valorac_valores %>%
  tabyl(sector, valoracion)
#view(recomp_xsector)

# tabla cruzada en porcentajes

recomp_xsector_pct <- recomp_xsector %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) # %>%
#adorn_ns() para mantener también los absolutos
#view(recomp_xsector_pct)

# analizando sólo el sector transporte, almacenamiento y comunicación

recomp_transporte <- recomp_valorac_valores %>%
  filter(sector == "Transporte alm y comunic")
#view(recomp_transporte)

frecuencias_recompt <- table(recomp_transporte) # distribución de frecuencias absoluta
#view(frecuencias_recompt)

frecuencias_recompt_pct <- (frecuencias_recompt / sum (frecuencias_recompt))*100 # distribución de frecuencias porcentual
#view(frecuencias_recompt_pct)

# graficamos la percepción de valoración del trabajo en el sector de transporte, almacenamiento y comunicación
barplot(frecuencias_recompt_pct, ylab = "porcentaje", las = 2, cex.names =.7,5, main = "Nivel de valoración de su trabajo", cex.main = 1, col = "blue") # rotamos las etiquetas de las categorías y achicamos la letra para que se visualice bien

frecuencias_recompt_pct_df <- as.data.frame(frecuencias_recompt_pct)
#view(frecuencias_recompt_pct_df)

# 7) variables a analizar: sector y acoso psicológico: 
# variable acoso_psi:  ¿con qué frecuencia ud. fue objeto de situaciones de
# acoso moral, hostigamiento y/o maltrato psicologico en su trabajo?

acoso_psi <- ocupados18_valores %>% 
  select(acoso_psi, sector) %>%  #solo selecciono las columnas de apoyo de compañeros y sector
  glimpse()

unique(acoso_psi$acoso_psi)

acoso_psi$acoso_psi<-as.character(acoso_psi$acoso_psi)
glimpse(acoso_psi)

# etiquetamos las categorías de la variable

acoso_psi_valores <- acoso_psi %>%
  mutate(acoso_psico = case_when(acoso_psi == 1 ~ "Siempre",
                                 acoso_psi == 2 ~ "Muchas veces",
                                 acoso_psi == 3 ~ "Algunas veces",
                                 acoso_psi == 4 ~ "Sólo alguna vez",
                                 acoso_psi == 5 ~ "Nunca",
                                 acoso_psi == 6 ~ "No corresponde",
                                 acoso_psi == 99 ~ "Ns/nc",
                                TRUE ~ acoso_psi)) %>%
  select(!acoso_psi) %>% # eliminamos la columna original
  glimpse()

acoso_psi_valores

# tabla cruzada entre las variables sector y acoso psicológico en absolutos

acosopsi_xsector <- acoso_psi_valores %>%
  tabyl(sector, acoso_psico)
#view(acosopsi_xsector)

# tabla cruzada entre las variables sector y acoso psicológico en porcentajes

acosopsi_xsector_pct <- acosopsi_xsector %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) # %>%
#adorn_ns() para mantener también los absolutos
#view(acosopsi_xsector_pct)

# analizando sólo el sector transporte, almacenamiento y comunicación

acosopsi_transporte <- acoso_psi_valores %>%
  filter(sector == "Transporte alm y comunic")
#view(acosopsi_transporte)

# distribución de frecuencias absoluta

frecuencias_acosopsi <- table(acosopsi_transporte) 
#view(frecuencias_acosopsi)

# distribución de frecuencias porcentual

frecuencias_acosopsi_pct <- (frecuencias_acosopsi / sum (frecuencias_acosopsi))*100 
#view(frecuencias_acosopsi_pct)

# graficamos el acoso psicológico en el sector de transporte, almacenamiento y comunicación

frecuencias_acosopsi_pct_df <- as.data.frame(frecuencias_acosopsi_pct)
#view(frecuencias_acosopsi_pct_df)

grafico <- ggplot(frecuencias_acosopsi_pct_df,
                  aes(x=acoso_psico, y = Freq)) +
  geom_bar(stat= "identity")

# 8) variables a analizar: sector y ayuda/apoyo del jefe: 
# variable C2P7.9: ¿Frecuencia con la que recibe ayuda y o apoyo de jefe inmediato?

glimpse(ocupados18_valores)

apoyo <- ocupados18_valores %>% 
  select(c2p7_9, sector) %>%  #solo selecciono las columnas de apoyo del jefe y sector
  glimpse()

unique(apoyo$c2p7_9)

apoyo$c2p7_9<-as.character(apoyo$c2p7_9)
glimpse(apoyo)

apoyo <- apoyo %>% 
  drop_na()

unique(apoyo$c2p7_9)

# etiquetamos las categorías de la variable

apoyo_valores <- apoyo %>%
  mutate(apoyo_jefe = case_when(c2p7_9 == 1 ~ "Siempre",
                                 c2p7_9 == 2 ~ "Muchas veces",
                                 c2p7_9 == 3 ~ "Algunas veces",
                                 c2p7_9 == 4 ~ "Sólo alguna vez",
                                 c2p7_9 == 5 ~ "Nunca",
                                 c2p7_9 == 99 ~ "Ns/nc",
                                 TRUE ~ c2p7_9)) %>%
  select(!c2p7_9) %>% # eliminamos la columna original
  glimpse()


# tabla cruzada entre las variables sector y apoyo del jefe en absolutos

apoyo_xsector <- apoyo_valores %>%
  tabyl(sector, apoyo_jefe)
#view(apoyo_xsector)

# tabla cruzada entre las variables sector y apoyo del jefe en porcentajes

apoyo_xsector_pct <- apoyo_xsector %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) # %>%
#adorn_ns() para mantener también los absolutos
#view(apoyo_xsector_pct)

# analizando sólo el sector transporte, almacenamiento y comunicación

apoyo_transporte <- apoyo_valores %>%
  filter(sector == "Transporte alm y comunic")
#view(apoyo_transporte)

# distribución de frecuencias absoluta

frecuencias_apoyot <- table(apoyo_transporte) 
#view(frecuencias_apoyot)

# distribución de frecuencias porcentual

frecuencias_apoyot_pct <- (frecuencias_apoyot / sum (frecuencias_apoyot))*100
#view(frecuencias_apoyot_pct)

# graficamos el apoyo del jefe en el sector de transporte, almacenamiento y comunicación

frecuencias_apoyot_pct_df <- as.data.frame(frecuencias_apoyot_pct)

grafico <- ggplot(frecuencias_apoyot_pct_df,
                  aes(x=apoyo_jefe, y = Freq, fill = apoyo_jefe)) +
  geom_bar(stat= "identity") +
  ylab("Porcentaje") +
  xlab("Frecuencia") +
  ggtitle ("Nivel de apoyo del jefe en el sector transporte")

# 9) variables a analizar: sector y síntoma mental
# variable sintom_mental: En los últimos 12 meses, pensando en su salud en gral., ¿padeció cansancio mental?

glimpse(ocupados18_valores)

sintom_mental <- ocupados18_valores %>% 
  select(sintom_mental, sector) 

unique(sintom_mental$sintom_mental)

sintom_mental$sintom_mental<-as.character(sintom_mental$sintom_mental)

glimpse(sintom_mental)

# etiquetamos la variable

sintoma_mental_valores <- sintom_mental %>%
  mutate(sintoma_mental = case_when(sintom_mental == 1 ~ "Si",
                                   sintom_mental == 2 ~ "No",
                                   sintom_mental == 99 ~ "Ns/Nc",
                                TRUE ~ sintom_mental)) %>%
  select(!sintom_mental) %>% # eliminamos la columna original
  glimpse()


# tabla cruzada entre las variables sector y síntoma mental en absolutos

sm_xsector <- sintoma_mental_valores %>%
  tabyl(sector, sintoma_mental)
#view(sm_xsector)


# tabla cruzada entre las variables sector y síntoma mental en porcentajes

sm_xsector_pct <- sm_xsector %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) # %>%
#adorn_ns() para mantener también los absolutos
#view(sm_xsector_pct)

# graficamos sintoma_mental según sector
ggplot(data = sintoma_mental_valores, aes(x = sector, y= sintoma_mental, fill = sintoma_mental)) +
  geom_col() +
  coord_flip() 


# analizando sólo el sector transporte, almacenamiento y comunicación

sm_transporte <- sintoma_mental_valores %>%
  filter(sector == "Transporte alm y comunic")
#view(sm_transporte)

# distribución de frecuencias absoluta

frecuencias_sm <- table(sm_transporte) 
#view(frecuencias_sm)

# distribución de frecuencias porcentual

frecuencias_sm_pct <- (frecuencias_sm / sum (frecuencias_sm))*100 
#view(frecuencias_sm_pct)

# graficamos síntoma mental en el sector de transporte, almacenamiento y comunicación

frecuencias_sm_pct_df <- as.data.frame(frecuencias_sm_pct)
#view(frecuencias_sm_pct_df)

grafico <- ggplot(frecuencias_apoyot_pct_df,
                  aes(x=apoyo_jefe, y = Freq)) +
  geom_bar(stat= "identity")

# gráfico de barras comparando sectores

ggplot(data = sintoma_mental_valores, aes(x = sector, y= sintoma_mental, fill = sintoma_mental)) +
  geom_col() +
  coord_flip() 

sm_xsector_pct_df <- as.data.frame(sm_xsector_pct)

grafico <- ggplot(sm_xsector_pct_df,
                  aes(x=sector, y = Si, fill=factor(ifelse(sector=="Transporte alm y comunic","Higlighted","Normal")))) +
  geom_bar(stat= "identity", show.legend = FALSE) +
  scale_fill_manual(name = "sector", values=c("red", "grey50")) +
  coord_flip() +
  ylab("Porcentaje") +
  xlab("Sector") +
  ggtitle("Trabajadores que padecieron cansancio mental por sector") +
  theme_classic()

# reordenando las barras:

otro_orden_sectores <- c("Enseñanza", "Servicios sociales y de salud", "Serv financ inm alq y emp", "Admin pública y defensa", "Transporte alm y comunic", "Comercio", "Industria manufacturera", "Hoteles y restaurantes", "Trabajo doméstico", "Otros serv comunit soc y per", "Actividades primarias", "Otras ramas", "Construcción", "Sin especificar")    

sm_xsector_pct_df$sector <- factor(sm_xsector_pct_df$sector, levels = otro_orden_sectores)

ggplot(data = muchascosas_alavez_valores, aes(x = sector, y= exigencia_cognit, fill = exigencia_cognit)) +
  geom_col() +
  coord_flip() 

grafico <- ggplot(sm_xsector_pct_df,
                  aes(x=sector, y = Si, fill=factor(ifelse(sector=="Transporte alm y comunic","Higlighted","Normal")))) +
  geom_bar(stat= "identity", show.legend = FALSE) +
  scale_fill_manual(name = "sector", values=c("red", "grey50")) +
  coord_flip() +
  ylab("Porcentaje") +
  xlab("Sector") +
  ggtitle("Trabajadores que padecieron cansancio mental por sector") +
  theme_classic()


# Indice de riesgo laboral (indicadores de riesgo psico-social y medioambiental)

# seleccionamos las columnas que queremos considerar en el armado del índice:
# exig_cogni: ¿Con qué frecuencia su trabajo exige que tenga que controlar muchas cosas a la vez?
# acoso_psi: En su puesto de trabajo /actividad en el último año, ¿con qué frecuencia ud. fue objeto de 
# situaciones de acoso moral, hostigamiento y/o maltrato psicologico?
# agres_ext: En su puesto de trabajo /actividad en el último año, ¿con qué frecuencia ud. fue 
# objeto de situaciones de agresión de los clientes, pacientes, alumnos, público, etc.?
# exig_emoc 1: ¿Con qué frecuencia su trabajo exige que esconda sus emociones o sentimientos?
# exig_emoc 2: ¿Su trabajo es emocionalmente desgastador?
# exig_ritmo: ¿Con qué frecuencia tiene que trabajar muy rápido?
# recomp_salar: ¿Con qué frecuencia su salario/remuneracion es justo con respecto a su rendimiento laboral?
# robo_trab: "Durante los últimos 12 meses, ¿ha sufrido algún episodio de robo o hurto en su lugar de trabajo 
# o durante su jornada laboral?: categorías 1 si/ 2 no (mientras más bajo, peor)
# venti: Frecuencia de exposición a mala ventilación en su lugar de trabajo
# esp_redu: Frecuencia de exposición a  espacios físicos (tamaño) reducidos en su lugar de trabajo
# sanit: Frecuencia de exposición a  servicios sanitarios en malas condiciones (baños, vestuarios, 
# agua potable, etc) en su lugar de trabajo
# ruido: En una jornada de trabajo habitual, frec. de exposición a un nivel de ruido que le obliga 
# a elevar la voz para conversar
# vibra: En su trabajo habitual, ¿usted está expuesto a vibraciones?
# movrep: En su trabajo habitual, ¿realiza movimientos repetitivos con los dedos, manos o 
# brazos cada pocos segundos?


df_indice <- ocupados18_valores %>% 
  select(region, sector, exig_cogni, acoso_psi, agres_ext, exig_emoc_1, exig_emoc_2, exig_ritmo, recomp_salar, robo_trab, venti, esp_redu, sanit, ruido, vibra, movrep) %>%  
  glimpse()

# eliminamos de todas las columnas los valores NA, 6 (No corresponde) y 99 (Ns/Nc)
# porque no deben considerarse en el índice

df_indice_f <- df_indice %>%
  filter(
    !(is.na(exig_cogni) | exig_cogni == 99 | exig_cogni == 6) &
      !(is.na(acoso_psi) | acoso_psi == 99 | acoso_psi == 6) &
      !(is.na(agres_ext) | agres_ext == 99 | agres_ext == 6) &
      !(is.na(exig_emoc_1) | exig_emoc_1 == 99 | exig_emoc_1 == 6) &
      !(is.na(exig_emoc_2) | exig_emoc_2 == 99 | exig_emoc_2 == 6) &
      !(is.na(exig_ritmo) | exig_ritmo == 99 | exig_ritmo == 6) &
      !(is.na(recomp_salar) | recomp_salar == 99 | recomp_salar == 6) &
      !(is.na(robo_trab) | robo_trab == 99 | robo_trab == 6) &
      !(is.na(venti) | venti == 99 | venti == 6) &
      !(is.na(esp_redu) | esp_redu == 99 | esp_redu == 6) &
      !(is.na(sanit) | sanit == 99 | sanit == 6) &
      !(is.na(ruido) | ruido == 99 | ruido == 6) &
      !(is.na(vibra) | vibra == 99 | vibra == 6) &
      !(is.na(movrep) | movrep == 99 | movrep == 6)
  )



# verificamos que en las distintas columnas se hayan eliminado los valores 
# correctamente

unique(df_indice_f$exig_cogni)
unique(df_indice_f$acoso_psi)
unique(df_indice_f$agres_ext)
unique(df_indice_f$exig_emoc_1)
unique(df_indice_f$exig_emoc_2)
unique(df_indice_f$exig_ritmo)
unique(df_indice_f$recomp_salar)
unique(df_indice_f$robo_trab)
unique(df_indice_f$venti)
unique(df_indice_f$esp_redu)
unique(df_indice_f$sanit)
unique(df_indice_f$ruido)
unique(df_indice_f$vibra)
unique(df_indice_f$movrep)

glimpse(df_indice_f)

# creamos el ìndice sumando por cada fila los valores de las columnas
# seleccionadas y luego se agrega el índice al DF como columna

indice <- rowSums(df_indice_f[, c("exig_cogni", "acoso_psi", "agres_ext", "exig_emoc_1", "exig_emoc_2", "exig_ritmo", "recomp_salar", "robo_trab", "venti", "esp_redu", "sanit", "ruido", "vibra", "movrep") ])
df_indice_f$indice <- indice
#view(df_indice_f)
# se calcula el ìndice promedio por sector 

indice_promedio_sector <- aggregate(indice ~ sector, data = df_indice_f, FUN = mean)

# verificamos los valores promedio del índice por sector
# valores más bajos indican un riesgo laboral mayor, porque las categorías
# de los indicadores seleccionados son:
# 1 Siempre, 2 Muchas veces, 3 Algunas veces, 4 Sólo algunas vez, 5 Nunca
# es decir, a menor índice, mayor frecuencia de factores de riesgo laboral


print(indice_promedio_sector) 

# Para la correcta lectura del índice hay que normalizarlo e invertirlo

indice_promedio_sector <- c(52.28, 49.25, 51.65, 48.32, 48.86, 49.42, 49.73, 47.82, 52.36, 51.87, 48.45, 51.69, 56.00, 48.14)
df_indice <- data.frame(Sector = c("Actividades primarias", "Admin pública y defensa", "Comercio", "Construcción", "Enseñanza", "Hoteles y restaurantes",
                            "Industria manufacturera", "Otras ramas", "Otros serv comunit soc y per", "Serv financ inm alq y emp",
                            "Servicios sociales y de salud", "Sin especificar", "Trabajo doméstico", "Transporte alm y comunic"),
                 Indice = indice_promedio_sector)

df_normalizado <- data.frame(Sector = df_indice$Sector)

df_normalizado$indice_n <- (df_indice$Indice - min(df_indice$Indice)) / (max(df_indice$Indice) - min(df_indice$Indice))

df_normalizado$indicefinal <- 1 - df_normalizado$indice_n

df_normalizado <- subset(df_normalizado, select = c(-indice_n))

# Índice por región

indice_promedio_region <- aggregate(indice ~ region, data = df_indice_f, FUN = mean)
print(indice_promedio_region) 

indice_promedio_region <- c(51.19, 48.58, 50.42, 50.17, 51.14, 50.11)
df_indice_r <- data.frame(Region = c("Gran Buenos Aires", "NOA", "NEA", "Cuyo", "Pampeana", "Patagonia"),
                        Indice = indice_promedio_region)

df_normalizado_r <- data.frame(region = df_indice_r$Region)

df_normalizado_r$indice_n <- (df_indice_r$Indice - min(df_indice_r$Indice)) / (max(df_indice_r$Indice) - min(df_indice_r$Indice))

df_normalizado_r$indicefinal <- 1 - df_normalizado_r$indice_n

df_normalizado <- subset(df_normalizado_r, select = c(-indice_n))


