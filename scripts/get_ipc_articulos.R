crear_mes <- function(mes, type = "text_to_number") {
  # creating the pipe
  `%>%` <- magrittr::`%>%`
  
  if(type == "number_to_text") {
  
    new_mes <- dplyr::recode(mes,
                             `1` = "Enero",
                             `2` = "Febrero",
                             `3` = "Marzo",
                             `4` = "Abril",
                             `5` = "Mayo",
                             `6` = "Junio",
                             `7` = "Julio",
                             `8` = "Agosto",
                             `9` = "Septiembre",
                             `10` = "Octubre",
                             `11` = "Noviembre",
                             `12` = "Diciembre")
    
  }
  
  if(type == "number_to_shorttext"){
    
    new_mes <- dplyr::recode(mes,
                             `1` = "Ene",
                             `2` = "Feb",
                             `3` = "Mar",
                             `4` = "Abr",
                             `5` = "May",
                             `6` = "Jun",
                             `7` = "Jul",
                             `8` = "Ago",
                             `9` = "Sep",
                             `10` = "Oct",
                             `11` = "Nov",
                             `12` = "Dic")
    
  }
  
  if(type == "text_to_number"){
    
    mes  <-  str_to_title(mes)
    
    new_mes <- dplyr::recode(mes,
                             "Ene" = 01,
                             "Feb" = 02,
                             "Mar" = 03,
                             "Abr" = 04,
                             "May" = 05,
                             "Jun" = 06,
                             "Jul" = 07,
                             "Ago" = 08,
                             "Sep" = 09,
                             "Oct" = 10,
                             "Nov" = 11,
                             "Dic" = 12,
                             "Enero" = 01,
                             "Febrero" = 02,
                             "Marzo" = 03,
                             "Abril" = 04,
                             "Mayo" = 05,
                             "Junio" = 06,
                             "Julio" = 07,
                             "Agosto" = 08,
                             "Septiembre" = 09,
                             "Octubre" = 10,
                             "Noviembre" = 11,
                             "Diciembre" = 12)
  }
  
  return(new_mes)
}


get_ipc_articulos <- function() {
  articulos_detalle <- read_rds("data/articulos_detalles.rds")
  
  url <- "https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_articulos_base_2010.xlsx"
  
  temp_path <- tempfile(fileext = ".xlsx")
  
  download.file(url, temp_path, mode = "wb", quiet = TRUE)
  
  sheets <- stringr::str_subset(readxl::excel_sheets(temp_path), "METADATOS", negate = TRUE)
  
  ipc_articulos_long <- purrr::map(
    sheets,
    ~suppressMessages(readxl::read_excel(temp_path, sheet = .x, skip = 4)) %>%
      janitor::remove_empty(which = "cols") %>% 
      janitor::clean_names() %>% 
      rename(name = x1, ponderador = x2) %>% 
      tidyr::pivot_longer(-c(name, ponderador), names_to = "mes", values_to = "indice")
  ) %>% 
    setNames(readr::parse_number(sheets)) %>% 
    dplyr::bind_rows(.id = "year") %>% 
    left_join(articulos_detalle, by = c("name" = "nombre", "ponderador"))   
  
  return(ipc_articulos_long)
  }

 # Creada por Rob Hyndman 
benchmarks <- function(y, h = 1) {
  require(forecast)
  # Compute four benchmark methods
  fcasts <- rbind(
    Naive = snaive(y, h)$mean,
    ETS = forecast(ets(y), h)$mean,
    Arima = forecast(auto.arima(y), h)$mean,
    T = thetaf(y, h)$mean)
  
  colnames(fcasts) <- seq(h)
  method_names <- rownames(fcasts)
  # Compute all possible combinations
  method_choice <- rep(list(0:1), length(method_names))
  names(method_choice) <- method_names
  combinations <- expand.grid(method_choice) %>% tail(-1) %>% as.matrix()
  # Construct names for all combinations
  for (i in seq(NROW(combinations))) {
    rownames(combinations)[i] <- paste0(method_names[which(combinations[i, ] > 0)],
                                        collapse = " ")
  }
  # Compute combination weights
  combinations <- sweep(combinations, 1, rowSums(combinations), FUN = "/")
  # Compute combinations of forecasts
  return(combinations %*% fcasts)
}




