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

# themes
library(scales)


Sys.setlocale("LC_TIME", "Spanish")


# Objetos utilitarios -----------------------------------------------------

arimaForecast <- function(x, xreg = NULL, h = 1) {
  forecast(
    auto.arima(x, xreg = xreg, parallel = TRUE,
               stepwise = FALSE, biasadj = TRUE,
               approximation = FALSE), 
    h = h)
}

# Importar y preparar data --- ----------------------------------------------

# Precios de los combustibles ----
combustibles <- readxl::read_excel("data/semana_combustible_new_date.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate(fecha = as.Date(fecha)) %>% 
  filter(fecha > '2007-12-31')

# para filtrar en el siguiente paso
last_date <- last(pull(combustibles, fecha))

# secuencia de fechas desde marzo del 2001
combustibles_diarios <- tibble(
  fecha = seq(from = ymd("2007-12-29"), to = Sys.Date(), by = "day")
  ) %>% 
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

# Precio de las primeras 2 y 3 semanas del mes

precios_weeks <- map(
  c(8, 15, 22),
  ~combustibles_diarios %>% 
    mutate(
      day = day(fecha),
      year = year(fecha),
      mes = month(fecha)) %>% 
    select(fecha, date_ym, year, mes, day, premium, regular, gasoil_reg, glp, kerosene) %>% 
    group_by(date_ym) %>% 
    filter(day < .x) %>% 
    summarise(
      year = unique(year),
      mes = unique(mes),
      across(c(premium:kerosene), mean, na.rm = T)
    )
) %>% 
  set_names("one", "two", "three") %>% 
  # Cambio de forma y ajuste de los nombre de los combustibles. 
  map(
    ~ .x %>% 
      pivot_longer(cols = -c(date_ym, year, mes), names_to = "name", values_to = "precio") %>%
      arrange(name, date_ym) %>% 
      mutate(
        name = recode(
          name, "premium" = "Gasolina premium",
          "regular" = "Gasolina regular",
          "glp" = "GLP",
          "gasoil_reg" = "Gasoil",
          "kerosene" = "Kerosene")
      ) %>% 
      ungroup
  ) %>% 
  bind_rows(.id = "weeks")


# Data con precios ajustados
precios <- combustibles_month %>% 
  select(date_ym, premium, regular, glp, gasoil_reg, kerosene) %>% 
  pivot_longer(cols = -date_ym, names_to = "name", values_to = "precio") %>%
  arrange(name, date_ym) %>% 
  mutate(
    name = recode(
      name, "premium" = "Gasolina premium",
      "regular" = "Gasolina regular",
      "glp" = "GLP",
      "gasoil_reg" = "Gasoil",
      "kerosene" = "Kerosene")
    )


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
    #incidencia = ponderador / mean(ponderador[division == "SubClase"])
  ) %>% 
  ungroup() %>% 
  mutate(
    incidencia = ponderador / mean(ponderador[division == "SubClase"])
  )

# merge de los precios y los índices de los precios de los combustibles
combustibles_precio_indice <- left_join(
    precios, ipc_combustibles,
    by = c("date_ym", "name")
  ) %>% 
  select(year, mes, date_ym, name, division, ponderador, indice, precio) %>% 
  filter(year > 2010) %>% 
  # Agregando variaciones mensuales
  mutate(
    across(c(indice, precio), list(vm = ~. / lag(.) - 1)),
    date = ymd(paste(year, bcdata::crear_mes(mes), "01")),
    year = parse_number(year)
  )


# Modeling --- -------------------------------------------------------------

## Ejercio pronóstico simple --------------------------------------

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

# Sólo con arimas
# combustibles_cv <- combustibles_nested %>% 
#   select(
#     name, division, data, ts
#   ) %>% 
#   mutate(
#     model = map(ts, ~auto.arima(y = .x, parallel = TRUE, biasadj = TRUE, approximation = FALSE)),
#     
#     errores = map(ts, ~tsCV(.x, arimaForecast))
#   )


# Resumen de los criterios de información del modelo
combustibles_glamce <- combustibles_nested %>% 
select(models) %>% 
  unnest(models) %>%
  mutate(
   glance =  map(fit, sw_glance)
  ) %>% 
  select(name, glance) %>% 
  unnest(glance)

# Resumen de los valores ajustados y residuos por modelo
combustibles_augment <- combustibles_nested %>% 
  select(models) %>% 
  unnest(models) %>% 
  mutate(augment = map(fit, sw_augment, rename_index = "date")) %>% 
  ungroup() %>% 
  select(name, f, augment) %>% 
  unnest(augment)

# Gráfico de los residuos
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
  rename(`Descripción` = model.desc) %>% 
  kableExtra::kable() %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>% 
    pack_rows("Combustibles y lubricantes para vehículos", 1, 3) %>% 
  pack_rows("Gasoil", 4, 6) %>% 
  pack_rows("Gasolina premium", 7, 9) %>% 
  pack_rows("Gasolina regular", 10, 12) %>%
  pack_rows("GLP", 13, 15) %>% 
  pack_rows("Lubricantes y aceites para vehículos", 16, 18)
  

## Modeling de la serie con el dato observado -----------------------------

### Preparando la data ----------------------------------

# Precio de los combustibles por semana
combustibles_precios_week_wide <-  
  combustibles_precio_indice %>%
  left_join(
    precios_weeks %>% 
      pivot_wider(names_from = weeks, values_from = precio) %>% 
      filter(!is.nan(one)) %>% 
      select(-mes),
    
    by = c("date_ym", "year", "name")
  ) %>% 
  select(
    date, year, mes, date_ym, name, indice, general = precio,
    one, two, three
  ) %>% 
  ungroup()

# Variaciones de los precios 
combustibles_variaciones_week_wide <- 
combustibles_precios_week_wide %>% 
  arrange(date) %>% 
  group_by(name) %>% 
  mutate(
    indice = (indice / lag(indice) -1),
    across(c(one, two, three), ~. / lag(general) - 1),
    general = general / lag(general) - 1
  ) 

# Versión long de las variaciones
combustibles_variaciones_week_long <- 
  combustibles_variaciones_week_wide %>% 
  arrange(name, date) %>% 
  pivot_longer(
    cols = c(general, one, two, three),
    names_to = "weeks",
    values_to = "variacion"
  ) %>% 
  arrange(name, weeks, date) %>% 
  ungroup() %>% select(date:name, weeks, indice, variacion)


# Versión long de los precios
combustibles_precios_week_long <- 
  combustibles_precios_week_wide %>% 
  pivot_longer(
    cols = c(general, one, two, three),
    names_to = "weeks",
    values_to = "precio"
  )

combustibles_precios_week_long <- 
  combustibles_precios_week_long %>% 
  left_join(
    select(combustibles_variaciones_week_long, -indice)
  )


## Modelo usando los precios observados

combustibles_by_week_model <- combustibles_variaciones_week_wide %>%
  pivot_longer(cols = c(one:three), names_to = "weeks", values_to = "variaciones") %>% 
  filter(!is.na(indice)) %>%
  arrange(name, weeks, date) %>% 
  group_by(name, weeks) %>% 
  nest() %>% 
  mutate(
    # ts
    indice_ts = map(data, ~select(.x, indice) %>% ts(start = c(2011, 2), frequency = 12)),
    weeks_ts = map(data, ~select(.x, variaciones) %>% ts(start = c(2011, 2), frequency = 12)),
    
    # Modelos
    model = map2(indice_ts, weeks_ts, ~auto.arima(.x, xreg = .y)),
    glance = map(model, sw_glance),
    augment = map(model, sw_augment),
    
    # errores
    errores = map2(
      indice_ts, weeks_ts,
      .f = ~tsCV(y = .x, forecastfunction = arimaForecast,
            h = 1, xreg = .y)
      ),
    
    rmse = map_dbl(errores, ~mean(.x**2, na.rm = TRUE) ** 0.5)
    
    )

combustibles_by_week_model %>% 
  select(name, glance) %>% 
  unnest(glance)

combustibles_by_week_model %>% 
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
  filter(!weeks %in% c("two", "three")) %>% 
  ggplot(aes(x = index, y = .resid)) +
  #geom_point() +
  geom_line() +
  facet_grid(name ~ origen, scales = 'free_y') +
  theme_light()

# Comparando los residuos del modelo simple con el de los modelos dinámicos
combustibles_augment %>% 
  filter(name %in% c("Gasoil", "Gasolina premium", "Gasolina regular", "GLP")) %>% 
  rename(index = date)

# Visualizaciones ---------------------------------------------------------

## Gráfico comparación Variación índice y precios promedios
(plt_gasolina_indice_precio_vm <- combustibles_precio_indice %>% 
   filter(str_detect(name, "solina")) %>%
   group_by(name) %>% 
   slice(-c(1, 2)) %>% 
   ggplot(aes(x = date)) +
   geom_line(aes(y = precio_vm, color = "Precio"), size = 1) +
   geom_line(aes(y = indice_vm, color = "Índice"), size = 1) +
   scale_y_continuous(labels = percent, breaks = pretty_breaks(7)) +
   scale_x_date(label = bcdata::date_label, breaks = pretty_breaks(5)) +
   facet_wrap(~name, scales = "free") +
   scale_color_manual(values = c("gray45", "black")) +
   theme_minimal() +
   theme(
     legend.position = "bottom"
   ) +
   labs(x = NULL, y = NULL))

ggsave(
  "visualizaciones/plt_gasolina_indice_precio_vm.png",
  plt_gasolina_indice_precio_vm, dpi = 350,
  height = 4, width = 9)

# Comparación entre precios según semanas
combustibles_precios_week_long %>% 
  ggplot(aes(x = date, y = variacion, color = weeks)) +
  facet_wrap(~name) +
  geom_line()


combustibles_variaciones_week_wide %>% 
  mutate(across(c(one, two, three), ~(. - general))) %>% 
  select(name, one, two, three) %>% 
  pivot_longer(
    cols = c(one, two, three),
    names_to = "weeks",
    values_to = "values"
  ) %>%
  mutate(
    weeks = factor(weeks, levels = c("one", "two", "three"), labels = c("Una", "Dos", "Tres"))
  ) %>% 
  ggplot(aes(x = values, fill = weeks)) +
  geom_density(alpha = 0.5, color = NA) +
  facet_wrap(~name, scales = "free") +
  theme_minimal() +
  labs(fill = "Semanas", x = "puntos porcentuales") +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = pretty_breaks(6))


## Gráfico de los residuos del modelo 
combustibles_by_week_model %>% 
  select(name, augment) %>% 
  unnest(augment) %>% 
  mutate(
    weeks = factor(weeks, levels = c("one", "two", "three"), labels = c("Una", "Dos", "Tres"))
  ) %>% 
  ggplot(aes(x = .resid, fill = weeks)) +
  geom_density(alpha = 0.7, color = NA) +
  facet_wrap(~name, scales = "free") +
  theme_minimal() +
  labs(x = "Puntos porcentuales", y = NULL, fill = "Semanas") +
  theme(legend.position = "bottom")


# Saving workspace --------------------------------------------------------

save.image("workspace")
load("workspace")

# Cosas faltantes

# Agregar otro modelo con los precios semanales del petróleo
# Estrategia proyectando los precios semanales de los combustibles y usarlo como variable de regresión
# Utilizar dummys
 
