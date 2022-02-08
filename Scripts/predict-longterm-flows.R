## predict long term naturalized flows

# Setup ####
## Load libraries ####
## readr imports data
library(readr)
library(tidyr)
## tibbles are advanced fancy dataframes
library(tibble)
## dplyr for data handling and piping functions
library(dplyr)
## ggplot for plots
library(ggplot2)
## stringr to read and manipulate strings
library(stringr)
## here is a function to ensure file paths are correct
library(here)
## units and ggforce facilitate attaching units to data
library(units)
library(ggforce)
## hrbrtheme is optional, I use it to pretty my plots
library(hrbrthemes)
## patchwork and cowplot support arranging multiple ggplots into one plot
library(patchwork)
library(cowplot)
library(ggridges)
## lubridate provides functions for handling time and date
library(lubridate)
## purrr lets us use map_ functions as an alternative to loops
library(purrr)
## hydroGOF provide goodness of fit metrics (NSE, RMSE, etc.)
library(hydroGOF)
## tsibble and imputeTS will allow some simple time series interpolation
library(tsibble)
library(imputeTS)
## gt
library(gtsummary)
library(flextable)
## nls.multstart fits non-linear least squares using the Levenberg-Marquardt algorithm with multiple starting values.
library(nls.multstart)
##
library(mgcv)
library(gratia)
## apply drainage area ratio
library(dartx)
##fancyplots
library(ragg)
library(ggstance)
#library(modelr)
library(rsample)
set.seed(20)




## Set options ####
update_geom_font_defaults(font_rc)
units_options(parse = FALSE)


## Functions ####
theme_ms <- function(...) {
  theme_ipsum_rc(plot_margin = margin(10,10,10,10),
                 axis_title_just = "c") +
    theme(axis.ticks.x = element_line(size = 0.5),
          axis.ticks.y = element_line(size = 0.5),
          legend.position = "bottom",
          panel.background = element_rect(fill = "white", 
                                          colour = NA), 
          panel.border = element_rect(fill = NA, 
                                      colour = "black"),
          strip.text = element_text(size = 9),
          ...)
}


theme_marginal_ms <- function(...) {
  theme_ipsum_rc(plot_margin = margin(2,2,2,2),
                 axis_title_just = "c") +
    theme(axis.ticks.x = element_line(size = 0.5),
          axis.ticks.y = element_line(size = 0.5),
          axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          # legend.key.width = unit(.25, "points"),
          # legend.key.height = unit(.35, "points"),
          legend.position = "right",
          legend.text = element_text(size = 6),
          legend.title = element_text(size = 6),
          panel.background = element_rect(fill = "white", 
                                          colour = NA), 
          panel.border = element_rect(fill = NA, 
                                      colour = "black"),
          plot.title = element_text(size = 9),
          strip.text = element_text(size = 9),
          ...)
}

# Import Data ####

## Forcing variables ####


prediction_data <- read_csv("Processed-Data/noaa_precip/easterwood.csv") %>%
  select(date, station, 
         ewood_precip = dailyprecipitation,
         ewood_tmax = dailymaximumdrybulbtemperature,
         ewood_rh = dailyaveragerelativehumidity) %>%
  mutate(month = lubridate::month(date))
  
prediction_data <-  prediction_data %>%
  mutate(lagPrecip = lag(ewood_precip),
                            wetness = map(row_number(.$date),
                                          ~{if(.x - 1 <= 0) {prediction_data$ewood_precip[.x]}
                                            if(.x - 2 <= 0) {sum(prediction_data$ewood_precip[.x],
                                                                 prediction_data$ewood_precip[.x-1])}
                                            if(.x - 2 > 0) {
                                              sum(
                                                prediction_data$ewood_precip[.x],
                                                prediction_data$ewood_precip[.x-1],
                                                prediction_data$ewood_precip[.x-2],
                                                na.rm = TRUE
                                              ) 
                                            }}),
                            et = map(row_number(.$date),
                                     ~{if(.x - 1 <= 0) {prediction_data$ewood_tmax[.x]}
                                       if(.x - 2 <= 0) {prediction_data$ewood_tmax[.x-1]}
                                       if(.x - 3 <= 0) { mean(c(prediction_data$ewood_tmax[.x-1],
                                                                prediction_data$ewood_tmax[.x-2]),
                                                              na.rm = TRUE)}
                                       if(.x - 4 <= 0) { mean(c(df_16396$ewood_tmax[.x-1],
                                                                df_16396$ewood_tmax[.x-2],
                                                                df_16396$ewood_tmax[.x-3]),
                                                              na.rm = TRUE)}
                                       if(.x - 5 <= 0) { mean(c(df_16396$ewood_tmax[.x-1],
                                                                df_16396$ewood_tmax[.x-2],
                                                                df_16396$ewood_tmax[.x-3],
                                                                df_16396$ewood_tmax[.x-4]),
                                                              na.rm = TRUE)}
                                       mean(c(df_16396$ewood_tmax[.x-1],
                                              df_16396$ewood_tmax[.x-2],
                                              df_16396$ewood_tmax[.x-3],
                                              df_16396$ewood_tmax[.x-4],
                                              df_16396$ewood_tmax[.x-5]),
                                            na.rm = TRUE)})) %>%
  unnest(c(wetness, et)) %>%
