
# Paquetes --- -------------------------------------------------------------
# data manipulation
library(tidyverse)
library(lubridate)
library(janitor)

# time series and forecasting
library(tsibble)
library(fable)
library(fabletools)
library(feasts)

library(scico)

# Importar y preparar data --- ----------------------------------------------

# Precios de los combustibles ----
combustibles <- readxl::read_excel("data/semana_combustible_new_date.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate(fecha = as.Date(fecha)) %>% 
  filter(!is.na(premium))

# para filtrar en el siguiente paso
last_date <- last(pull(combustibles, fecha))

# secuencia de fechas desde marzo del 2001
combustibles_diarios <- tibble(fecha = seq(from = ymd("2001-03-20"), to = Sys.Date(), by = "day")) %>% 
  # se resta cuatro para llevar las fechas de día miércoles de cada semana,
  # al sábado anterior
  left_join(mutate(combustibles, fecha = fecha - 4)) %>%
  fill(-fecha, .direction = "down") %>% 
  filter(fecha <= last_date, !is.na(premium)) %>% 
  mutate(date_ym = tsibble::yearmonth(fecha)) %>% 
  select(fecha, date_ym, everything()) 

# pasa de data diaria a promedio mensual
combustibles_month <- group_by(combustibles_diarios, date_ym) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

# Data con precios ajustados
precios <- combustibles_month %>% 
  select(date_ym, premium, regular, glp, gasoil_reg) %>% 
  pivot_longer(cols = -date_ym, names_to = "name", values_to = "precio") %>%
  arrange(name, date_ym) %>% 
  mutate(
    name = recode(
      name, "premium" = "Gasolina premium",
      "regular" = "Gasolina regular",
      "glp" = "GLP",
      "gasoil_reg" = "Gasoil"))


# Indice de precios por artículos ----

# crea funciones útiles
source("scripts/get_ipc_articulos.R")

# Descarga la data del ipc a todos lo niveles de desagregación posible
ipc_desagregado <- get_ipc_articulos()

# adecuando algunos campos para tener el árbol ascendente de cada artículo
ipc_desagregado <- ipc_desagregado %>%
  mutate(
    date_ym = paste(year, crear_mes(mes, "text_to_number"), "01", sep = "-"),
    date_ym = tsibble::yearmonth(ymd(date_ym)),
    grupo = ifelse(division == "Grupo", name, NA),
    subgrupo = ifelse(division == 'Subgrupo', name, NA),
    clase = ifelse(division == 'Clase', name, NA),
    subclase = ifelse(division == "SubClase", name, NA)
  ) %>%
  fill(c(grupo, subgrupo, clase, subclase), .direction = "down")

# series de los artículos que compnone la clase de combustibles y lubricantes
ipc_combustibles <- ipc_desagregado %>% 
  filter(clase == "Combustibles y lubricantes para equipo de transporte",
         division %in% c("SubClase", "Articulo")) %>%
  select(year, mes, date_ym, name, division, ponderador, indice) %>% 
  mutate(name = recode(name, "Gas licuado para vehículos (GLP)" = "GLP")) %>% 
  arrange(name, date_ym)

# agregando la incidencia y las variaciones mensuales e interanuales
ipc_combustibles <- ipc_combustibles %>% 
  group_by(name) %>% 
  mutate(
    variacion_mes = indice / lag(indice) - 1,
    variacion_interanual = indice / lag(indice, 12) - 1,
    #to_see = mean(ponderador[division == "SubClase"]),
    incidencia = ponderador / mean(ponderador[division == "SubClase"])
  ) %>% ungroup()

# merge de los precios y los índices de los precios de los combustibles
combustibles_precio_indice <- left_join(
    precios, ipc_combustibles,
    by = c("date_ym", "name")
  ) %>% 
  select(year, mes, date_ym, name, division, ponderador, indice, precio) %>% 
  filter(year > 2010)


# Modeling --- -------------------------------------------------------------
  
# Anidando la data
combustibles_nested <- ipc_combustibles %>%
  group_by(name, division, ponderador) %>% 
  nest() 

combustibles_nested %>% 
  mutate(
    # serie de tiempo
    ts = map(
      data,
      ~.x %>% select(variacion_mes) %>% 
        slice(-1) %>% 
        ts(start = c(2011, 2), frequency = 12)
      ),
    
    # Parametros para los modelos
    params = map(
      ts,
      ~list(
        auto.arima = list(y = .x),
        ets = list(y = .x),
        bats = list(y = .x),
        thetaf = list(y = .x)
        )
    ),
    
    params = map(params, enframe, name = "f", value = "params"),
    
    # Fiting models
    models = map(
      params,
      ~.x %>% mutate(fit = invoke_map(f, params))
    ),
    
    # Foreacast
    
  )
