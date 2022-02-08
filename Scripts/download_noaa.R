library(rnoaa)
library(rnoaahelpers)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)

start_date <- as.Date("2010-01-01")
end_date <- as.Date("2021-03-31")
station_id <- "GHCND:USW00003904" ## COLLEGE STATION EASTERWOOD FIELD, TX US
data_type_id <- "PRCP"
token <- Sys.getenv("noaakey")


#https://www.ncei.noaa.gov/data/local-climatological-data/doc/LCD_documentation.pdf
daily_summaries <- tibble(year = 2010:2021) %>%
  mutate(report = map(year, ~lcd(station = "74746003904", year = .x)))



df <- daily_summaries %>%
  mutate(report = map(report, ~mutate(.x, across(everything(), as.character)))) %>%
  unnest(cols = c(report)) %>%
  select(station, date, name, dailyaveragerelativehumidity, dailymaximumdrybulbtemperature, dailyprecipitation) %>%
  filter(!is.na(dailyaveragerelativehumidity)) %>%
  mutate(date = stringr::str_sub(date, 0, 10),
         dailyaveragerelativehumidity = stringr::str_replace(dailyaveragerelativehumidity,
                                                             "\\*",
                                                             ""),
         dailymaximumdrybulbtemperature = stringr::str_replace(dailymaximumdrybulbtemperature,
                                                               "\\*",
                                                               ""),
         dailyprecipitation = stringr::str_replace(dailyprecipitation,
                                                   "\\*",
                                                   ""),
         dailyprecipitation = stringr::str_replace(dailyprecipitation,
                                                   "T",
                                                   "0.001")) %>%
  mutate(date = as.Date(date),
         dailyaveragerelativehumidity = as.numeric(dailyaveragerelativehumidity),
         dailymaximumdrybulbtemperature = as.numeric(dailymaximumdrybulbtemperature),
         dailyprecipitation = as.numeric(dailyprecipitation))

write_csv(df, here::here("Processed-Data/noaa_precip/easterwood.csv"))

df <- download_ncdc(start_date, end_date, station_id, data_type_id, token, progress = TRUE)
df

write_csv(df, here::here("Processed-Data/noaa_precip/easterwood_precip.csv"))

data_type_id <- "TMAX"
df <- download_ncdc(start_date, end_date, station_id, data_type_id, token, progress = TRUE)
df

write_csv(df, here::here("Processed-Data/noaa_precip/easterwood_tmax.csv"))


data_type_id <- "TAVG"
df <- download_ncdc(start_date, end_date, station_id, data_type_id, token, progress = TRUE)
df

write_csv(df, here::here("Processed-Data/noaa_precip/easterwood_tavg.csv"))