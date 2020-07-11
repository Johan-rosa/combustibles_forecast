
indices <- ipc_desagregado %>%
  filter(division == "Articulo") %>% 
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

# Exploración del grupo de bienes y servicios Transporte

# Data
transporte <- ipc_desagregado %>% 
  filter(grupo == "Transporte", division %in% c("Grupo", "Subgrupo")) %>%
  group_by(name, division) %>%
  arrange(date_ym) %>% 
  mutate(variacion = (indice/lag(indice, 12) - 1),
         # Se divide entre 18 el ponderador porque esa es la ponderación
         # total de grupo de transporte
         incidencia = variacion * (ponderador / 18.0))

# Variación de los grupos de 

ipc_desagregado %>% 
  filter(grupo == "Transporte", 
         subgrupo == "Funcionamiento de Equipo de Transporte",
         clase == "Combustibles y lubricantes para equipo de transporte",
         division == "SubClase"
         #division %in% c("Grupo", "Subgrupo")
  ) %>% 
  distinct(subclase, ponderador)


ggplot() +
  geom_col(
    data = filter(transporte, name != "Transporte", date_ym > yearmonth(ymd("2011-12-01"))),
    mapping = aes(x = date_ym, y = incidencia, fill = name)
  ) + 
  geom_line(
    data = filter(transporte, name == "Transporte", date_ym > yearmonth(ymd("2011-12-01"))),
    mapping = aes(x = date_ym, y = variacion),
    size = 1
  ) +
  theme_light() +
  theme(legend.position = "bottom") +
  ggthemes::scale_fill_tableau()

# Funcionamiento de equipos de transporte acumula la mayor insidencia en la evolución 
# del índice de ese grupo.


# Evolución variación mensual por combustibles
(plt_evolucion_combustible <- indice_precio %>% 
    ggplot() +
    geom_line(aes(x = date_ym, y = indice_vm, color = "Índice") ) +
    geom_line(aes(x = date_ym, y = precio_vm, color = "Precio")) +
    facet_wrap(~name) +
    theme_light() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_yearmonth(breaks = scales::pretty_breaks(8)) +
    theme(legend.position = "bottom",
          panel.grid = element_blank()) +
    scale_color_manual(values = c("black", " red")) +
    labs(y = "Variación mensual",
         x = NULL, color = NULL)
)


# histomgrama de los precios 

(plt_histograma_preciosvm <- indice_precio %>% 
    ggplot(aes(x = precio_vm)) +
    geom_histogram(binwidth = 0.02) +
    scale_x_continuous(labels = scales::percent, breaks = scales::pretty_breaks(7)) +
    theme_light() +
    labs(x = NULL, y = "Frecuencia") +
    facet_wrap(~name, scales = "free_x"))

indice_precio

# Creando series de tiempo ------------------------------------------------

read_rds("data/articulos_detalles.rds") %>% 
  filter(str_detect(nombre, "Combusti"))




