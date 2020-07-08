
# Paquetes --- -------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(janitor)

# Importar y preparar data --- ----------------------------------------------

# Precios de los combustibles ----
combustibles <- read_delim("data/datasemanal.txt", delim = '\t') %>%
  janitor::clean_names() %>% 
  mutate(fecha = mdy(fecha)) %>%  
  filter(!is.na(premium)) 

# para filtrar en el siguiente paso
last_date <- last(pull(combustibles, fecha))

# secuencia de fechas desde marzo del 2001
combustibles_diarios <- tibble(fecha = seq(from = ymd("2001-03-20"), to = Sys.Date(), by = "day")) %>% 
  # se resta cuatro para llevar las fechas de día miercoles de cada semana,
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

# se ajusta la data para extraer solo los artículos, si agregaciones superirores
ipc_articulos <- ipc_desagregado %>%
  mutate(
    date_ym = paste(year, crear_mes(mes, "text_to_number"), "01", sep = "-"),
    date_ym = tsibble::yearmonth(ymd(date_ym)),
    grupo = ifelse(division == "Grupo", nombre, NA),
    subgrupo = ifelse(division == 'Subgrupo', nombre, NA),
    clase = ifelse(division == 'Clase', nombre, NA),
    subclase = ifelse(division == "SubClase", nombre, NA)
  ) %>%
  fill(c(grupo, subgrupo, clase, subclase), .direction = "down") %>% 
  filter(division == "Articulo") 
  

indices <- ipc_articulos %>% 
  filter(subclase == "Combustibles y lubricantes para vehículos") %>%
  mutate(name = ifelse(str_detect(name, 'Gas lic'), "GLP", name)) %>%
  filter(name != "Lubricantes y aceites para vehículos") %>% 
  select(date_ym, name, ponderador, indice)
  

# Unión precios indices ---------------------------------------------------

indice_precio <- indices %>% 
  left_join(precios) %>%
  arrange(name, date_ym) %>%
  group_by(name) %>% 
  mutate(indice_vm = (indice / lag(indice, 1) - 1),
         precio_vm = (precio / lag(precio, 1) - 1))



# Visualizaciones ---------------------------------------------------------

# Evolución variación mensual por combustibles
indice_precio %>% 
  ggplot() +
  geom_line(aes(x = date_ym, y = indice_vm, color = "Índice") ) +
  geom_line(aes(x = date_ym, y = precio_vm, color = "Precio")) +
  facet_wrap(~name) +
  theme_light() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_yearmonth(breaks = scales::pretty_breaks(8)) +
  theme(legend.position = "bottom",
        panel.grid = element_blank()) +
  scale_color_manual(values = c("black", " gray69")) +
  labs(y = "Variación mensual",
       x = NULL, color = NULL)


# histomgrama de los precios 

indice_precio %>% 
  ggplot(aes(x = precio_vm)) +
  geom_histogram(binwidth = 0.02) +
  scale_x_continuous(labels = scales::percent, breaks = scales::pretty_breaks(7)) +
  theme_light() +
  labs(x = NULL, y = "Frecuencia") +
  facet_wrap(~name, scales = "free_x")


