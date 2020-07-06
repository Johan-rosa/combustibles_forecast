# Paquetes -------------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
library(forecast)
library(zoo)
library(tsibble)
library(forecast)

# Lectura de datos -----------------------------------------------------------------
combustibles <- read_delim("data/datasemanal.txt", delim = '\t') %>% 
  tibble::as.tibble() %>% janitor::clean_names()

# Manipulación de objetos ----------------------------------------------------------

# agregando variable de tiempo al data frame
combustibles <- combustibles %>%
  mutate(
    fecha = mdy(fecha),
    year = year(fecha),
    month = month(fecha),
    year_month = tsibble::yearmonth(fecha),
    year_week = tsibble::yearweek(fecha)
    ) %>%
  group_by(year, month) %>%
  mutate(
    month_week = dplyr::row_number()
  ) %>%
  ungroup()



gasolinas <-  combustibles %>%
  filter(year > 2010) %>%
  group_by(year_month) %>%
  summarise(premium = mean(premium, na.rm = TRUE)) %>%
  left_join(
    combustibles %>%
      filter(year > 2010, month_week < 3) %>%
      group_by(year_month) %>%
      summarise(premium_fw = mean(premium, na.rm = TRUE))
  ) %>%
  mutate(
    diff = premium_fw - premium
  )

ipc_gasolina <- ipc_articulos_long %>%
  filter(str_detect(nombre, "^Gasolina premium")) %>%
  select(fecha_ym, mes, nombre, indice)

gasolinas %>%
  select(premium, premium_fw) %>%
  ts(start = )

ts_gasolina <- combustibles %>%
  ungroup() %>%
  filter(fecha > "2010-12-31") %>%
  mutate(fecha = floor_date(fecha, "week")) %>%
  select(premium) %>%
  ts(start = c(2011, 1), frequency = 365.25/7)

# Visualización de datos -----------------------------------------------------------

gasolinas %>%
  ggplot() +
  geom_line(aes(x = year_month, y = premium, color = 'premium')) +
  geom_line(aes(x = year_month, y = premium_fw, color = 'premium_fw')) +
  theme(
    legend.position = 'bottom'
  )

gasolinas %>%
  ggplot(aes(x = diff)) +
  geom_histogram(bins = 20)

ggplot(ipc_gasolina, aes(x = fecha_ym, y = indice)) + 
  geom_line() +
  scale_y_log10() +
  scale_x_yearmon()


# Time series forecasting ----------------------------------------------------------


autoplot(ts_gasolina)

ts_gasolina %>%
  #log10() %>%
  #diff() %>%
  auto.arima() %>%
  forecast(h = 52) %>%
  autoplot()
  
bestfit <- list(aicc=Inf)

for(i in 1:25) {
  fit <- auto.arima(ts_gasolina, xreg=fourier(ts_gasolina, K=i), seasonal=FALSE)
  if(fit$aicc < bestfit$aicc)
    bestfit <- fit
  else break;
}

fc <- forecast(bestfit, xreg=fourier(ts_gasolina, K=2, h=2))

plot(fc)


combustibles %>% 
  select(fecha, premium) %>% 
  as_tibble() %>% 
  mutate(
    fecha = mdy(fecha),
    #weekn = week(fecha),
    #start_week = floor_date(fecha, unit = "week"),
    start_mont = floor_date(fecha, unit = "month"),
    end_month = lubridate::ceiling_date(fecha, unit = "month"),
    last_saturday = fecha - 4,
    mes = month(fecha)
  ) %>% 
  group_by(month)







