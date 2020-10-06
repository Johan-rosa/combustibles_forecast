
# Paquetes --- -------------------------------------------------------------
# data manipulation
library(tidyverse)
library(lubridate)
library(janitor)

# time series and forecasting
library(forecast)
library(timetk)
library(sweep)
library(scico)

# Tidy time series
library(tsibble)
library(fable)
library(fabletools)
library(feasts)

# Tablas HTML
library(kableExtra)
library(formattable)

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
  # se resta cuatro para llevar las fechas de miércoles de cada semana,
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

# Descarga la data del ipc a todos lo niveles de desagregaciÃ³n posible
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

# Creando los elementos para el forecast de cada serie
combustibles_nested <- combustibles_nested %>% 
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
        bats = list(y = .x)
        )
    ),
    
    # poniendo los parametros en una forma tabular
    params = map(params, enframe, name = "f", value = "params"),
    
    # estimando los modelos
    models = map(
      params,
      ~.x %>% mutate(fit = invoke_map(f, params))
      ),
    
    # Forecast de cada modelo
    models = map(models, ~mutate(.x, fcast = map(fit, forecast, h = 1))),
    
    # agregando una versión tidy del forecast de cada modelo
    models = map(
      models,
      ~.x %>%
        mutate(
          sweep = map(
            fcast,
            sw_sweep,
            fitted = FALSE,
            timetk_index = TRUE,
            rename_index = "date"
          )
        )
    ),
    
    sweep = map(
      models, ~.x %>% 
        select(f, sweep) %>% 
        unnest(sweep)
    )
  )

combustibles_glamce <- combustibles_nested %>% 
select(models) %>% 
  unnest(models) %>%
  mutate(
   glance =  map(fit, sw_glance)
  ) %>% 
  select(name, glance) %>% 
  unnest(glance)

combustibles_augment <- combustibles_nested %>% 
  select(models) %>% 
  unnest(models) %>% 
  mutate(augment = map(fit, sw_augment, rename_index = "date")) %>% 
  ungroup() %>% 
  select(name, f, augment) %>% 
  unnest(augment)

combustibles_augment %>% 
  ggplot(aes(x = date, y = .resid)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  theme_light() +
  facet_grid(str_wrap(name, 15) ~ f)


# Tabla html con el resumen de cada modelo
combustibles_glamce %>% 
  mutate_if(is.numeric, round, digits = 5) %>% 
  ungroup() %>% 
  select(model.desc:ACF1) %>% 
  kableExtra::kable() %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>% 
    pack_rows("Combustibles y lubricantes para vehículos", 1, 3) %>% 
  pack_rows("Gasoil", 4, 6) %>% 
  pack_rows("Gasolina premium", 7, 9) %>% 
  pack_rows("Gasolina regular", 10, 12) %>%
  pack_rows("GLP", 13, 15) %>% 
  pack_rows("Lubricantes y aceites para vehículos", 16, 18)
  

# Modeling de la serie con el dato observado
combustibles_nested_xreg <-  combustibles_precio_indice %>% 
  group_by(name) %>% 
  summarise(date_ym, across(c(indice, precio), .fns =  ~(. / lag(.) - 1))) %>% 
  slice(-1) %>% 
  nest() %>% 
  mutate(
    indice_ts = map(data, ~select(.x, indice) %>% ts(start = c(2011, 2), frequency = 12)),
    precio_ts = map(data, ~select(.x, precio) %>% ts(start = c(2011, 2), frequency = 12)),
    model = map2(indice_ts, precio_ts, ~auto.arima(.x, xreg = .y)),
    glance = map(model, sw_glance),
    augment = map(model, sw_augment)
  )

combustibles_nested_xreg %>% 
  select(name, glance) %>% 
  unnest(glance)

combustibles_nested_xreg %>% 
  select(name, augment) %>% 
  unnest(augment) %>% 
  mutate(origen = "xreg") %>% 
  bind_rows(
    filter(
      combustibles_augment,
      name %in% c("Gasoil", "Gasolina premium", "Gasolina regular", "GLP"),
      f == "auto.arima") %>%
      rename(index = date) %>% 
      select(-f) %>% 
      mutate(origen = "simple")
  ) %>% 
  ggplot(aes(x = index, y = .resid)) +
  #geom_point() +
  geom_line() +
  facet_grid(name ~ origen, scales = 'free_y') +
  theme_light()

#save.image("data/forecasting_ws")

mtcars %>% 
  group_by(cyl) %>% 
  summarise(
    across(c(mpg, disp, wt), mean),
    across(c(am, gear, carb), median)
  )




