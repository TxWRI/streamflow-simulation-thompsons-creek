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

### Precip ####

easterwood_precip <- read_csv("Processed-Data/noaa_precip/easterwood.csv") %>%
  select(date, station, value = dailyprecipitation) %>%
  mutate(value =  set_units(value, "in")) %>%
  filter(date <= as.Date("2021-03-31"))

# easterwood_precip <- read_csv("Processed-Data/noaa_precip/easterwood_precip.csv",
#                               col_types = cols(
#                                 date = col_datetime(format = ""),
#                                 datatype = col_character(),
#                                 station = col_character(),
#                                 value = col_double(),
#                                 fl_m = col_character(),
#                                 fl_q = col_character(),
#                                 fl_so = col_character(),
#                                 fl_t = col_character(),
#                                 units = col_character())) %>%
#   mutate(value =  set_units(value/10, "mm")) %>%
#   select(date, datatype, station, value)



easterwood_precip %>%
  mutate(value = as.numeric(value)) %>%
  ggplot() +
  geom_line(aes(date, value)) +
  geom_point(aes(date, value), alpha = 0) +
  labs(y = "Daily precipitation [in]",
       x = "Date") +
  #facet_wrap(~station, ncol = 1) +
  theme_ms(plot.margin = margin(10,0,10,10),
           panel.spacing = unit(0, "lines"),
           panel.spacing.x = unit(0, "lines"),
           panel.spacing.y = unit(2, "lines")) -> p1



easterwood_precip %>%
  mutate(value = as.numeric(value)) %>%
  ggplot() +
  geom_density_line(aes(value)) +
  #facet_wrap(~station, ncol = 1) +
  labs(y = "Scaled Density") +
  coord_flip() +
  theme_ms() +
  theme(axis.line.x = element_line(color = "black"),
        #axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_blank(),
        plot.margin = margin(10,10,10,0),
        panel.spacing = unit(0, "lines"),
        panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(2, "lines"),
        strip.text = element_text(color="transparent")) -> p2

agg_png(here::here("Figures/precip.png"),
        width = 6,
        height = 3.5,
        units = "in",
        res = 300)
plot_grid(p1,p2,align = "h", axis = "bt", rel_widths = c(2, 1))
dev.off()

### Temp ####


easterwood_tmax <- read_csv("Processed-Data/noaa_precip/easterwood.csv") %>%
  select(date, station, value = dailymaximumdrybulbtemperature) %>%
  mutate(value =  set_units(value, "°F")) %>%
  filter(date <= as.Date("2021-03-31"))


# easterwood_tmax <- read_csv("Processed-Data/noaa_precip/easterwood_tmax.csv",
#                             col_types = cols(
#                               date = col_datetime(format = ""),
#                               datatype = col_character(),
#                               station = col_character(),
#                               value = col_double(),
#                               fl_m = col_character(),
#                               fl_q = col_character(),
#                               fl_so = col_character(),
#                               fl_t = col_character(),
#                               units = col_character())) %>%
#   mutate(value =  set_units(value/10, "°C")) %>%
#   select(date, datatype, station, value)

easterwood_tmax %>%
  mutate(value = as.numeric(value)) %>%
  ggplot() +
  geom_step(aes(date, value)) +
  labs(y = "Daily Maximum Temperature [°F]",
       x = "Date") +
  theme_ms(plot.margin = margin(10,0,10,10),
           panel.spacing = unit(0, "lines"),
           panel.spacing.x = unit(0, "lines"),
           panel.spacing.y = unit(2, "lines")) -> p1

easterwood_tmax %>%
  mutate(value = as.numeric(value)) %>%
  ggplot() +
  geom_density_line(aes(value)) +
  facet_wrap(~station, ncol = 1) +
  labs(y = "Scaled Density") +
  coord_flip() +
  theme_ms() +
  theme(axis.line.x = element_line(color = "black"),
        #axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_blank(),
        plot.margin = margin(10,10,10,0),
        panel.spacing = unit(0, "lines"),
        panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(2, "lines"),
        strip.text = element_text(color="transparent")) -> p2

agg_png(here::here("Figures/temp.png"),
        width = 6,
        height = 3.5,
        units = "in",
        res = 300)
plot_grid(p1,p2,align = "h", axis = "bt", rel_widths = c(2, 1))
dev.off()

### RH ####


easterwood_rh <- read_csv("Processed-Data/noaa_precip/easterwood.csv") %>%
  select(date, station, value = dailyaveragerelativehumidity) %>%
  filter(date <= as.Date("2021-03-31"))

easterwood_rh %>%
  ggplot() +
  geom_step(aes(date, value)) +
  labs(y = "Relative Humidity [%]",
       x = "Date") +
  theme_ms(plot.margin = margin(10,0,10,10),
           panel.spacing = unit(0, "lines"),
           panel.spacing.x = unit(0, "lines"),
           panel.spacing.y = unit(2, "lines")) -> p1

easterwood_rh %>%
  ggplot() +
  geom_density_line(aes(value)) +
  facet_wrap(~station, ncol = 1) +
  labs(y = "Scaled Density") +
  coord_flip() +
  theme_ms() +
  theme(axis.line.x = element_line(color = "black"),
        #axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_blank(),
        plot.margin = margin(10,10,10,0),
        panel.spacing = unit(0, "lines"),
        panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(2, "lines"),
        strip.text = element_text(color="transparent")) -> p2

agg_png(here::here("Figures/rh.png"),
        width = 6,
        height = 3.5,
        units = "in",
        res = 300)
plot_grid(p1,p2,align = "h", axis = "bt", rel_widths = c(2, 1))
dev.off()

## External variables ####

### WWTF discharge ####

wwtf <- read_csv("Processed-Data/EPA_WWTF/mean_daily_discharges.csv",
                 col_types = cols(
                   npdes_id = col_character(),
                   date = col_date(format = ""),
                   mgd = col_double(),
                   cfs = col_double()))
agg_png(here::here("Figures/wwtf.png"),
        width = 6,
        height = 3.5,
        units = "in",
        res = 300)
wwtf %>%
  ggplot() +
  geom_step(aes(date, cfs, color = npdes_id)) +
  scale_x_date(date_breaks = "month",
               date_labels = "%F") +
  scale_color_discrete(labels = c("WQ0010426002, Still Creek WWTF",
                                  "WQ0003821000, Sanderson Farm Inc." )) +
  labs(x = "Date [yyyy-mm-dd]", y = "Mean Daily Discharge [cfs]") + 
  theme_ms() +
  guides(x = guide_axis(angle = 45)) +
  theme(axis.line.x = element_blank(),
        axis.ticks.y = element_line(color = "black"),
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.length = grid::unit(5, "pt"),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
dev.off()

### Drainage areas ####
dar_table <- tibble(Site = c("SWQM-16396", "SWQM-16397", "SWQM-16882", "USGS-08065800", "USGS-08109800", "USGS-08110100"),
                    Description = c("Thompsons Creek at Silver Hill Rd",
                                    "Thompsons Creek at Hwy 21",
                                    "Still Creek at Hwy 21",
                                    "Bedias Creek near Madisonville",
                                    "East Yegua Creek near Dime Box",
                                    "Davidson Creek near Lyons"),
                    Area = c(42.33,24.21,10.03,321,244,195))

knitr::kable(dar_table,
      caption = "TCEQ SWQM stations and USGS stream gages used to develop flows with drainage area ratio and linear regression methods.")

### USGS streamflow ####

usgs_08065800 <- readr::read_csv("Processed-Data/USGS_Streamflow/08065800.csv") %>%
  dplyr::rename(Flow_08065800 = "133946_00060_00003") %>%
  dplyr::select(datetime, Flow_08065800)

usgs_08109800 <- readr::read_csv("Processed-Data/USGS_Streamflow/08109800.csv") %>%
  dplyr::rename(Flow_08109800 = `135356_00060_00003`) %>%
  dplyr::select(datetime, Flow_08109800)

usgs_08110100 <- readr::read_csv("Processed-Data/USGS_Streamflow/08110100.csv") %>%
  dplyr::rename(Flow_08110100 = `135389_00060_00003`) %>%
  dplyr::select(datetime, Flow_08110100)

agg_png(here::here("Figures/usgs-streamflows.png"),
        width = 6,
        height = 6,
        units = "in",
        res = 300)
usgs_08065800 %>%
  pivot_longer(Flow_08065800,
               names_to = "USGS_Station") %>%
  bind_rows(usgs_08109800 %>%
              pivot_longer(Flow_08109800,
                           names_to = "USGS_Station")) %>%
  bind_rows(usgs_08110100 %>%
              pivot_longer(Flow_08110100,
                           names_to = "USGS_Station")) %>%
  mutate(USGS_Station = forcats::fct_relabel(USGS_Station, ~str_remove(.x, "Flow_"))) %>%
  ggplot() +
  geom_line(aes(datetime, value)) +
  facet_wrap(~USGS_Station, ncol = 1) +
  labs(x = "Date", y = "Mean Daily Streamflow [cfs]") +
  theme_ms()
dev.off()

### Naturalized streamflows ####
# mean daily minus upstream discharges

df <- read_csv("Processed-Data/model_df.csv",
               col_types = cols(
                 Site = col_character(),
                 date = col_date(format = ""),
                 mean_daily = col_double()
               ))



df %>%
  bind_rows(tibble(Site = c(rep("SWQM-16396",5),rep("SWQM-16397",5),rep("SWQM-16882",5)),
                   date = rep(seq.Date(as.Date("2020-02-27"), as.Date("2020-03-02"), by = "day"),3),
                   mean_daily = NA)) %>%
  arrange(date) %>%
  left_join(easterwood_precip, by = c("date" = "date")) %>%
  mutate(value = as.numeric(value)) %>%
  dplyr::rename(ewood_precip = value) %>%
  left_join(easterwood_tmax, by = c("date" = "date")) %>%
  mutate(value = as.numeric(value)) %>%
  dplyr::rename(ewood_tmax = value) %>%
  left_join(easterwood_rh, by = c("date" = "date")) %>%
  mutate(value = as.numeric(value)) %>%
  dplyr::rename(ewood_rh = value) %>%
  dplyr::select(Site, date, mean_daily, ewood_precip, ewood_tmax, ewood_rh) %>%
  left_join(wwtf %>% pivot_wider(id_cols = date, names_from = npdes_id, values_from = cfs),
            by = c("date" = "date")) %>%
  ## remove WWTF influence from discharge record
  mutate(adjusted_flow = case_when(
    Site == "SWQM-16396" ~ mean_daily - TX0025071 - TX0113603, 
    Site == "SWQM-16882" ~ mean_daily - TX0025071,
    Site == "SWQM-16397" ~ mean_daily)) %>%
  mutate(adjusted_flow = case_when(
    adjusted_flow < 0 ~ 0,
    adjusted_flow >= 0 ~ adjusted_flow)) %>%
  mutate(month = lubridate::month(date))-> df

## plot hydrograph of Thompsons @ Silver Hill
p1 <- ggplot(df %>% filter(Site == "SWQM-16396") %>% mutate(date = as.Date(date))) + 
  geom_line(aes(date, adjusted_flow, color = "Mean Daily Streamflow")) +
  scale_y_continuous(position = "left", 
                     limits = c(0,800),
                     expand = c(0,0)) +
  scale_x_date(date_breaks = "month",
               date_labels = "%b-%y") +
  labs(y = "Mean daily streamflow [cfs]", 
       x = "Date [Month-Year]",
       caption = "16396, Thompsons Creek at Silver Hill Rd") +
  scale_color_manual(values = c("dodgerblue")) +
  theme_ms() +
  guides(x = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 8),
        axis.title.y.left = element_text(hjust = 0),
        axis.ticks.y.left = element_line(color = "black"),
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.length = grid::unit(5, "pt"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none") 

p2 <- ggplot(df %>% filter(Site == "SWQM-16396")) + 
  geom_line(aes(date, ewood_precip, color = "Total Daily Preciptitation")) +
  scale_y_reverse(position = "right", 
                  limits = c(35,0),
                  breaks = c(0,2,4),
                  labels = c(0,2,4),
                  expand = c(0,0)) +
  labs(y = "Total Daily Precipitation [in]") +
  scale_color_manual(values = c("dodgerblue4")) +
  theme_ms() +
  guides(x = guide_axis(angle = 90)) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y.right = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.ticks.y.right = element_line(color = "black"),
        axis.ticks.length = grid::unit(5, "pt"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       color = "transparent"),
        legend.position = "none"
  ) 
set_null_device("agg")
aligned_plots <- align_plots(p1, p2, align="hv", axis="tblr")
set_null_device("agg")
hg_plot1 <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
hg_plot1

## plot hydrograph of Thompsons @ Hwy21
p1 <- ggplot(df %>% filter(Site == "SWQM-16397") %>% mutate(date = as.Date(date))) + 
  geom_line(aes(date, adjusted_flow, color = "Mean Daily Streamflow")) +
  scale_y_continuous(position = "left", 
                     limits = c(0,75),
                     expand = c(0,0)) +
  scale_x_date(date_breaks = "month",
               date_labels = "%b-%y") +
  labs(y = "Mean daily streamflow [cfs]", 
       x = "Date [Month-Year]",
       caption = "16397, Thompsons Creek at Hwy 21") +
  scale_color_manual(values = c("dodgerblue")) +
  theme_ms() +
  guides(x = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 8),
        axis.title.y.left = element_text(hjust = 0),
        axis.ticks.y.left = element_line(color = "black"),
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.length = grid::unit(5, "pt"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none") 

p2 <- ggplot(df %>% filter(Site == "SWQM-16397")) + 
  geom_line(aes(date, ewood_precip, color = "Total Daily Preciptitation")) +
  scale_y_reverse(position = "right", 
                  limits = c(35,0),
                  breaks = c(0,2,4),
                  labels = c(0,2,4),
                  expand = c(0,0)) +
  labs(y = "Total Daily Precipitation [in]") +
  scale_color_manual(values = c("dodgerblue4")) +
  theme_ms() +
  guides(x = guide_axis(angle = 90)) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y.right = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.ticks.y.right = element_line(color = "black"),
        axis.ticks.length = grid::unit(5, "pt"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       color = "transparent"),
        legend.position = "none"
  ) 
set_null_device("agg")
aligned_plots <- align_plots(p1, p2, align="hv", axis="tblr")
set_null_device("agg")
hg_plot2 <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])


## plot hydrograph of Sill Creek @ Hwy21
p1 <- ggplot(df %>% filter(Site == "SWQM-16882") %>% mutate(date = as.Date(date))) + 
  geom_line(aes(date, adjusted_flow, color = "Mean Daily Streamflow")) +
  scale_y_continuous(position = "left", 
                     limits = c(0,100),
                     expand = c(0,0)) +
  scale_x_date(date_breaks = "month",
               date_labels = "%b-%y") +
  labs(y = "Mean daily streamflow [cfs]", 
       x = "Date [Month-Year]",
       caption = "16882, Still Creek at Hwy 21") +
  scale_color_manual(values = c("dodgerblue")) +
  theme_ms() +
  guides(x = guide_axis(angle = 45)) +
  theme(axis.text.x = element_text(size = 8),
        axis.title.y.left = element_text(hjust = 0),
        axis.ticks.y.left = element_line(color = "black"),
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.length = grid::unit(5, "pt"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none") 

p2 <- ggplot(df %>% filter(Site == "SWQM-16882")) + 
  geom_line(aes(date, ewood_precip, color = "Total Daily Preciptitation")) +
  scale_y_reverse(position = "right", 
                  limits = c(35,0),
                  breaks = c(0,2,4),
                  labels = c(0,2,4),
                  expand = c(0,0)) +
  labs(y = "Total Daily Precipitation [in]") +
  scale_color_manual(values = c("dodgerblue4")) +
  theme_ms() +
  guides(x = guide_axis(angle = 90)) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y.right = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.ticks.y.right = element_line(color = "black"),
        axis.ticks.length = grid::unit(5, "pt"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       color = "transparent"),
        legend.position = "none"
  ) 
set_null_device("agg")
aligned_plots <- align_plots(p1, p2, align="hv", axis="tblr")
set_null_device("agg")
hg_plot3 <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])

agg_png(here::here("Figures/naturalized-hydrograph.png"),
        width = 6,
        height = 8.5,
        units = "in",
        res = 300)
hg_plot1 / hg_plot2 / hg_plot3
dev.off()

## calculate some summary statistics
df %>%
  group_by(Site) %>%
  summarise(mean = mean(adjusted_flow, na.rm = TRUE),
            sd = sd(adjusted_flow, na.rm = TRUE),
            median = median(adjusted_flow, na.rm = TRUE))

df %>%
  group_by(Site) %>%
  filter(!is.na(adjusted_flow)) %>%
  mutate(n_flow = case_when(
    adjusted_flow > 0 ~ 1,
    adjusted_flow == 0 ~ 0)
  ) %>%
  summarise(n_flow = sum(n_flow, na.rm = FALSE),
            n = n()) %>%
  mutate(n_flow_p = n_flow/n * 100)

# Format Data ####
## create a model df for 16396
## additional variables = 
## lagPrecip = 1 day lag precipitation
## wetness index = 3 day total precip
## ET index = 5 day avg tmax


df %>%
  dplyr::filter(Site == "SWQM-16396") %>%
  arrange(date) -> df_16396

df_16396 %>%
  mutate(lagPrecip = lag(ewood_precip),
         wetness = map(row_number(.$date),
                       ~{if(.x - 1 <= 0) {df_16396$ewood_precip[.x]}
                         if(.x - 2 <= 0) {sum(df_16396$ewood_precip[.x],
                                              df_16396$ewood_precip[.x-1])}
                         if(.x - 2 > 0) {
                           sum(
                             df_16396$ewood_precip[.x],
                             df_16396$ewood_precip[.x-1],
                             df_16396$ewood_precip[.x-2],
                             na.rm = TRUE
                           ) 
                         }}),
         et = map(row_number(.$date),
                  ~{if(.x - 1 <= 0) {df_16396$ewood_tmax[.x]}
                    if(.x - 2 <= 0) {df_16396$ewood_tmax[.x-1]}
                    if(.x - 3 <= 0) { mean(c(df_16396$ewood_tmax[.x-1],
                                             df_16396$ewood_tmax[.x-2]),
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
  dplyr::filter(!is.na(mean_daily)) -> df_16396

df_16396 %>%
  mutate(ewood_precip = log1p(ewood_precip),
         lagPrecip = log1p(lagPrecip),
         wetness = log1p(lagPrecip),
         ewood_tmax = ewood_tmax^2,
         et = et^2) -> df_16396


## create a model df for 16397
## additional variable = 
## wetness index = 3 day total precip
## ET index = 5 day avg tmax

df %>%
  dplyr::filter(Site == "SWQM-16397") %>%
  arrange(date) -> df_16397

df_16397 %>%
  mutate(lagPrecip = lag(ewood_precip),
         wetness = map(row_number(.$date),
                       ~{if(.x - 1 <= 0) {df_16397$ewood_precip[.x]}
                         if(.x - 2 <= 0) {sum(df_16397$ewood_precip[.x],
                                              df_16397$ewood_precip[.x-1])}
                         if(.x - 2 > 0) {
                           sum(
                             df_16397$ewood_precip[.x],
                             df_16397$ewood_precip[.x-1],
                             df_16397$ewood_precip[.x-2],
                             na.rm = TRUE
                           ) 
                         }}),
         et = map(row_number(.$date),
                  ~{if(.x - 1 <= 0) {df_16397$ewood_tmax[.x]}
                    if(.x - 2 <= 0) {df_16397$ewood_tmax[.x-1]}
                    if(.x - 3 <= 0) { mean(c(df_16397$ewood_tmax[.x-1],
                                             df_16397$ewood_tmax[.x-2]),
                                           na.rm = TRUE)}
                    if(.x - 4 <= 0) { mean(c(df_16397$ewood_tmax[.x-1],
                                             df_16397$ewood_tmax[.x-2],
                                             df_16397$ewood_tmax[.x-3]),
                                           na.rm = TRUE)}
                    if(.x - 5 <= 0) { mean(c(df_16397$ewood_tmax[.x-1],
                                             df_16397$ewood_tmax[.x-2],
                                             df_16397$ewood_tmax[.x-3],
                                             df_16397$ewood_tmax[.x-4]),
                                           na.rm = TRUE)}
                    mean(c(df_16397$ewood_tmax[.x-1],
                           df_16397$ewood_tmax[.x-2],
                           df_16397$ewood_tmax[.x-3],
                           df_16397$ewood_tmax[.x-4],
                           df_16397$ewood_tmax[.x-5]),
                         na.rm = TRUE)})) %>%
  unnest(c(wetness, et)) %>%
  dplyr::filter(!is.na(mean_daily)) -> df_16397

df_16397 %>%
  mutate(ewood_precip = log1p(ewood_precip),
         lagPrecip = log1p(lagPrecip),
         wetness = log1p(lagPrecip),
         ewood_tmax = ewood_tmax^2,
         et = et^2) -> df_16397

## create a model df for 16882
## additional variable = 
## wetness index = 3 day total precip
## ET index = 5 day avg tmax

df %>%
  dplyr::filter(Site == "SWQM-16882") %>%
  arrange(date) -> df_16882

df_16882 %>%
  mutate(lagPrecip = lag(ewood_precip),
         wetness = map(row_number(.$date),
                       ~{if(.x - 1 <= 0) {df_16882$ewood_precip[.x]}
                         if(.x - 2 <= 0) {sum(df_16882$ewood_precip[.x],
                                              df_16882$ewood_precip[.x-1])}
                         if(.x - 2 > 0) {
                           sum(
                             df_16882$ewood_precip[.x],
                             df_16882$ewood_precip[.x-1],
                             df_16882$ewood_precip[.x-2],
                             na.rm = TRUE
                           ) 
                         }}),
         et = map(row_number(.$date),
                  ~{if(.x - 1 <= 0) {df_16882$ewood_tmax[.x]}
                    if(.x - 2 <= 0) {df_16882$ewood_tmax[.x-1]}
                    if(.x - 3 <= 0) { mean(c(df_16882$ewood_tmax[.x-1],
                                             df_16882$ewood_tmax[.x-2]),
                                           na.rm = TRUE)}
                    if(.x - 4 <= 0) { mean(c(df_16882$ewood_tmax[.x-1],
                                             df_16882$ewood_tmax[.x-2],
                                             df_16882$ewood_tmax[.x-3]),
                                           na.rm = TRUE)}
                    if(.x - 5 <= 0) { mean(c(df_16882$ewood_tmax[.x-1],
                                             df_16882$ewood_tmax[.x-2],
                                             df_16882$ewood_tmax[.x-3],
                                             df_16882$ewood_tmax[.x-4]),
                                           na.rm = TRUE)}
                    mean(c(df_16882$ewood_tmax[.x-1],
                           df_16882$ewood_tmax[.x-2],
                           df_16882$ewood_tmax[.x-3],
                           df_16882$ewood_tmax[.x-4],
                           df_16882$ewood_tmax[.x-5]),
                         na.rm = TRUE)})) %>%
  unnest(c(wetness, et)) %>%
  dplyr::filter(!is.na(mean_daily)) %>%
  mutate(non_zero = case_when(
    adjusted_flow == 0 ~ 0,
    adjusted_flow > 0 ~ 1
  ))-> df_16882

df_16882 %>%
  mutate(ewood_precip = log1p(ewood_precip),
         lagPrecip = log1p(lagPrecip),
         wetness = log1p(lagPrecip),
         ewood_tmax = ewood_tmax^2,
         et = et^2) -> df_16882

# Drainage Area Ratio ####

df_16396 %>%
  dplyr::select(Site, date, adjusted_flow) %>%
  left_join(usgs_08065800, by = c("date" = "datetime")) %>%
  dartx(Flow_08065800, 42.33/321) %>%
  dplyr::rename(DAR_Q_08065800 = Q,
                Q_percentile_08065800 = Q_percentile,
                exp_08065800 = exp) %>%
  left_join(usgs_08109800,  by = c("date" = "datetime")) %>%
  dartx(Flow_08109800, 42.33/244) %>%
  dplyr::rename(DAR_Q_08109800 = Q,
                Q_percentile_08109800 = Q_percentile,
                exp_08109800 = exp) %>%
  left_join(usgs_08110100,  by = c("date" = "datetime")) %>%
  dartx(Flow_08110100, 42.33/195) %>%
  dplyr::rename(DAR_Q_08110100 = Q,
                Q_percentile_08110100 = Q_percentile,
                exp_08110100 = exp) -> dar_results_16396

df_16882 %>%
  dplyr::select(Site, date, adjusted_flow) %>%
  left_join(usgs_08065800, by = c("date" = "datetime")) %>%
  dartx(Flow_08065800, 24.21/321) %>%
  dplyr::rename(DAR_Q_08065800 = Q,
                Q_percentile_08065800 = Q_percentile,
                exp_08065800 = exp) %>%
  left_join(usgs_08109800,  by = c("date" = "datetime")) %>%
  dartx(Flow_08109800, 24.21/244) %>%
  dplyr::rename(DAR_Q_08109800 = Q,
                Q_percentile_08109800 = Q_percentile,
                exp_08109800 = exp) %>%
  left_join(usgs_08110100,  by = c("date" = "datetime")) %>%
  dartx(Flow_08110100, 24.21/195) %>%
  dplyr::rename(DAR_Q_08110100 = Q,
                Q_percentile_08110100 = Q_percentile,
                exp_08110100 = exp) -> dar_results_16882

df_16397 %>%
  dplyr::select(Site, date, adjusted_flow) %>%
  left_join(usgs_08065800, by = c("date" = "datetime")) %>%
  dartx(Flow_08065800, 10.03/321) %>%
  dplyr::rename(DAR_Q_08065800 = Q,
                Q_percentile_08065800 = Q_percentile,
                exp_08065800 = exp) %>%
  left_join(usgs_08109800,  by = c("date" = "datetime")) %>%
  dartx(Flow_08109800, 10.03/244) %>%
  dplyr::rename(DAR_Q_08109800 = Q,
                Q_percentile_08109800 = Q_percentile,
                exp_08109800 = exp) %>%
  left_join(usgs_08110100,  by = c("date" = "datetime")) %>%
  dartx(Flow_08110100, 10.03/195) %>%
  dplyr::rename(DAR_Q_08110100 = Q,
                Q_percentile_08110100 = Q_percentile,
                exp_08110100 = exp) -> dar_results_16397

dar_results_16396 %>%
  dplyr::select(Site, date, adjusted_flow, DAR_Q_08065800, DAR_Q_08109800, DAR_Q_08110100) %>%
  pivot_longer(c(DAR_Q_08065800, DAR_Q_08109800, DAR_Q_08110100),
               names_to = "Source_Site",
               values_to = "Estimated_Flow") %>%
  mutate(Source_Site = case_when(
    Source_Site == "DAR_Q_08065800" ~ "Estimated flow using USGS-08065800",
    Source_Site == "DAR_Q_08109800" ~ "Estimated flow using USGS-08109800",
    Source_Site == "DAR_Q_08110100" ~ "Estimated flow using USGS-08110100"
  )) %>%
  ggplot() +
  geom_point(aes(Estimated_Flow, adjusted_flow, color = "DAR Estimates against Naturalized Flow"), alpha = 0.3) +
  # geom_smooth(aes(Estimated_Flow, adjusted_flow, color = "DAR Estimates against Naturalized Flow"), 
  #             method = "lm", se = FALSE, alpha = 0.3) +
  geom_abline(aes(linetype = "1:1 Line", intercept = 0, slope = 1)) +
  facet_wrap(~Source_Site,
             shrink = FALSE,
             ncol = 1) +
  scale_x_continuous(trans = scales::pseudo_log_trans(0.001, 10),
                     breaks=c(0, 10, 100, 1000)) + 
  scale_y_continuous(trans = scales::pseudo_log_trans(0.001, 10),
                     breaks=c(0, 10, 100, 1000)) +
  coord_cartesian(xlim = c(0,2500),
                  ylim = c(0,2500)) +
  labs(x = "DAR Estimated Flow [cfs]", y = "Naturalized Flow [cfs]") +
  guides(color = guide_legend(""), linetype = guide_legend("")) +
  theme_ms() + 
  theme(legend.margin = margin(0,0,0,0),
        legend.box = "vertical",
        legend.box.margin = margin(0,0,0,0),
        legend.direction = "vertical") -> p1

dar_results_16396 %>%
  dplyr::select(Site, date, DAR_Q_08065800, DAR_Q_08109800, DAR_Q_08110100) %>%
  pivot_longer(c(DAR_Q_08065800, DAR_Q_08109800, DAR_Q_08110100),
               names_to = "Source_Site",
               values_to = "Estimated_Flow") %>%
  mutate(Source_Site = case_when(
    Source_Site == "DAR_Q_08065800" ~ "Estimated flow using USGS-08065800",
    Source_Site == "DAR_Q_08109800" ~ "Estimated flow using USGS-08109800",
    Source_Site == "DAR_Q_08110100" ~ "Estimated flow using USGS-08110100"
  )) %>%
  ggplot() +
  geom_line(aes(date, Estimated_Flow, color = "DAR Estimated Flow")) +
  geom_line(data = dar_results_16396, aes(date, adjusted_flow, color = "Naturalized Flow SWQM 16396"), alpha = 0.4) +
  facet_wrap(~Source_Site, scales = "free_y", ncol = 1) +
  labs(x = "Date", y = "Mean Daily Flow [cfs]") +
  theme_ms() + 
  theme(legend.title = element_blank(),
        legend.direction = "vertical") -> p2
agg_png(here::here("Figures/dar-16396.png"),
        width = 8,
        height = 8,
        units = "in",
        res = 300)
p1 + p2
dev.off()

dar_results_16397 %>%
  dplyr::select(Site, date, adjusted_flow, DAR_Q_08065800, DAR_Q_08109800, DAR_Q_08110100) %>%
  pivot_longer(c(DAR_Q_08065800, DAR_Q_08109800, DAR_Q_08110100),
               names_to = "Source_Site",
               values_to = "Estimated_Flow") %>%
  mutate(Source_Site = case_when(
    Source_Site == "DAR_Q_08065800" ~ "Estimated flow using USGS-08065800",
    Source_Site == "DAR_Q_08109800" ~ "Estimated flow using USGS-08109800",
    Source_Site == "DAR_Q_08110100" ~ "Estimated flow using USGS-08110100"
  )) %>%
  ggplot() +
  geom_point(aes(Estimated_Flow, adjusted_flow, color = "DAR Estimates against Naturalized Flow"), alpha = 0.3) +
  # geom_smooth(aes(Estimated_Flow, adjusted_flow, color = "DAR Estimates against Naturalized Flow"), 
  #             method = "lm", se = FALSE, alpha = 0.3) +
  geom_abline(aes(linetype = "1:1 Line", intercept = 0, slope = 1)) +
  facet_wrap(~Source_Site,
             shrink = FALSE,
             ncol = 1) +
  scale_x_continuous(trans = scales::pseudo_log_trans(0.001, 10),
                     breaks=c(0, 10, 100, 1000)) + 
  scale_y_continuous(trans = scales::pseudo_log_trans(0.001, 10),
                     breaks=c(0, 10, 100, 1000)) +
  coord_cartesian(xlim = c(0,1000),
                  ylim = c(0,1000)) +
  labs(x = "DAR Estimated Flow [cfs]", y = "Naturalized Flow [cfs]") +
  guides(color = guide_legend(""), linetype = guide_legend("")) +
  theme_ms() + 
  theme(legend.margin = margin(0,0,0,0),
        legend.box = "vertical",
        legend.box.margin = margin(0,0,0,0),
        legend.direction = "vertical") -> p1

dar_results_16397 %>%
  dplyr::select(Site, date, DAR_Q_08065800, DAR_Q_08109800, DAR_Q_08110100) %>%
  pivot_longer(c(DAR_Q_08065800, DAR_Q_08109800, DAR_Q_08110100),
               names_to = "Source_Site",
               values_to = "Estimated_Flow") %>%
  mutate(Source_Site = case_when(
    Source_Site == "DAR_Q_08065800" ~ "Estimated flow using USGS-08065800",
    Source_Site == "DAR_Q_08109800" ~ "Estimated flow using USGS-08109800",
    Source_Site == "DAR_Q_08110100" ~ "Estimated flow using USGS-08110100"
  )) %>%
  ggplot() +
  geom_line(aes(date, Estimated_Flow, color = "DAR Estimated Flow")) +
  geom_line(data = dar_results_16397, aes(date, adjusted_flow, color = "Naturalized Flow SWQM 16397"), alpha = 0.4) +
  facet_wrap(~Source_Site, scales = "free_y", ncol = 1) +
  labs(x = "Date", y = "Mean Daily Flow [cfs]") +
  theme_ms() + 
  theme(legend.title = element_blank(),
        legend.direction = "vertical") -> p2
agg_png(here::here("Figures/dar-16397.png"),
        width = 8,
        height = 8,
        units = "in",
        res = 300)
p1 + p2
dev.off()


dar_results_16882 %>%
  dplyr::select(Site, date, adjusted_flow, DAR_Q_08065800, DAR_Q_08109800, DAR_Q_08110100) %>%
  pivot_longer(c(DAR_Q_08065800, DAR_Q_08109800, DAR_Q_08110100),
               names_to = "Source_Site",
               values_to = "Estimated_Flow") %>%
  mutate(Source_Site = case_when(
    Source_Site == "DAR_Q_08065800" ~ "Estimated flow using USGS-08065800",
    Source_Site == "DAR_Q_08109800" ~ "Estimated flow using USGS-08109800",
    Source_Site == "DAR_Q_08110100" ~ "Estimated flow using USGS-08110100"
  )) %>%
  ggplot() +
  geom_point(aes(Estimated_Flow, adjusted_flow, color = "DAR Estimates against Naturalized Flow"), alpha = 0.3) +
  # geom_smooth(aes(Estimated_Flow, adjusted_flow, color = "DAR Estimates against Naturalized Flow"), 
  #             method = "lm", se = FALSE, alpha = 0.3) +
  geom_abline(aes(linetype = "1:1 Line", intercept = 0, slope = 1)) +
  facet_wrap(~Source_Site,
             shrink = FALSE,
             ncol = 1) +
  scale_x_continuous(trans = scales::pseudo_log_trans(0.001, 10),
                     breaks=c(0, 10, 100, 1000)) + 
  scale_y_continuous(trans = scales::pseudo_log_trans(0.001, 10),
                     breaks=c(0, 10, 100, 1000)) +
  coord_cartesian(xlim = c(0,1000),
                  ylim = c(0,1000)) +
  labs(x = "DAR Estimated Flow [cfs]", y = "Naturalized Flow [cfs]") +
  guides(color = guide_legend(""), linetype = guide_legend("")) +
  theme_ms() + 
  theme(legend.margin = margin(0,0,0,0),
        legend.box = "vertical",
        legend.box.margin = margin(0,0,0,0),
        legend.direction = "vertical") -> p1

dar_results_16882 %>%
  dplyr::select(Site, date, DAR_Q_08065800, DAR_Q_08109800, DAR_Q_08110100) %>%
  pivot_longer(c(DAR_Q_08065800, DAR_Q_08109800, DAR_Q_08110100),
               names_to = "Source_Site",
               values_to = "Estimated_Flow") %>%
  mutate(Source_Site = case_when(
    Source_Site == "DAR_Q_08065800" ~ "Estimated flow using USGS-08065800",
    Source_Site == "DAR_Q_08109800" ~ "Estimated flow using USGS-08109800",
    Source_Site == "DAR_Q_08110100" ~ "Estimated flow using USGS-08110100"
  )) %>%
  ggplot() +
  geom_line(aes(date, Estimated_Flow, color = "DAR Estimated Flow")) +
  geom_line(data = dar_results_16882, aes(date, adjusted_flow, color = "Naturalized Flow SWQM 16882"), alpha = 0.4) +
  facet_wrap(~Source_Site, scales = "free_y", ncol = 1) +
  labs(x = "Date", y = "Mean Daily Flow [cfs]") +
  theme_ms() + 
  theme(legend.title = element_blank(),
        legend.direction = "vertical") -> p2
agg_png(here::here("Figures/dar-16882.png"),
        width = 8,
        height = 8,
        units = "in",
        res = 300)
p1 + p2
dev.off()

# Linear Regression ####

df_16396 %>%
  dplyr::select(Site, date, adjusted_flow) %>%
  left_join(usgs_08065800, by = c("date" = "datetime")) %>%
  left_join(usgs_08109800,  by = c("date" = "datetime")) %>%
  left_join(usgs_08110100,  by = c("date" = "datetime")) %>%
  mutate(lag_Flow_08065800 = lag(Flow_08065800),
         lag_Flow_08109800 = lag(Flow_08109800),
         lag_Flow_08110100 = lag(Flow_08110100),
         log_Q = log1p(adjusted_flow)) -> df_16396_lm

m1.lm <- lm(log_Q ~ log1p(Flow_08065800) + 
              log1p(Flow_08109800) + 
              log1p(Flow_08110100) + 
              log1p(lag_Flow_08065800) + 
              log1p(lag_Flow_08109800) + 
              log1p(lag_Flow_08110100),
            data = df_16396_lm)


flextable::as_flextable(m1.lm) %>%
  set_caption("Summary of linear regression coefficients at SWQM 16396") %>%
  save_as_docx(values = NULL, 
               path = "Output-Data/LinearRegression_16396.docx", 
               pr_section = NULL)


df_16397 %>%
  dplyr::select(Site, date, adjusted_flow) %>%
  left_join(usgs_08065800, by = c("date" = "datetime")) %>%
  left_join(usgs_08109800,  by = c("date" = "datetime")) %>%
  left_join(usgs_08110100,  by = c("date" = "datetime")) %>%
  mutate(lag_Flow_08065800 = lag(Flow_08065800),
         lag_Flow_08109800 = lag(Flow_08109800),
         lag_Flow_08110100 = lag(Flow_08110100),
         log_Q = log1p(adjusted_flow)) -> df_16397_lm

m2.lm <- lm(log_Q ~ log1p(Flow_08065800) + 
              log1p(Flow_08109800) + 
              log1p(Flow_08110100) + 
              log1p(lag_Flow_08065800) + 
              log1p(lag_Flow_08109800) + 
              log1p(lag_Flow_08110100),
            data = df_16397_lm)


flextable::as_flextable(m2.lm) %>%
  set_caption("Summary of linear regression coefficients at SWQM 16397") %>%
  save_as_docx(values = NULL, 
               path = "Output-Data/LinearRegression_16397.docx", 
               pr_section = NULL)

df_16882 %>%
  dplyr::select(Site, date, adjusted_flow) %>%
  left_join(usgs_08065800, by = c("date" = "datetime")) %>%
  left_join(usgs_08109800,  by = c("date" = "datetime")) %>%
  left_join(usgs_08110100,  by = c("date" = "datetime")) %>%
  mutate(lag_Flow_08065800 = lag(Flow_08065800),
         lag_Flow_08109800 = lag(Flow_08109800),
         lag_Flow_08110100 = lag(Flow_08110100),
         log_Q = log1p(adjusted_flow)) -> df_16882_lm

m3.lm <- lm(log_Q ~ log1p(Flow_08065800) + 
              log1p(Flow_08109800) + 
              log1p(Flow_08110100) + 
              log1p(lag_Flow_08065800) + 
              log1p(lag_Flow_08109800) + 
              log1p(lag_Flow_08110100),
            data = df_16882_lm)


flextable::as_flextable(m3.lm) %>%
  set_caption("Summary of linear regression coefficients at SWQM 16882") %>%
  save_as_docx(values = NULL, 
               path = "Output-Data/LinearRegression_16882.docx", 
               pr_section = NULL)


df_16396_lm %>%
  mutate(fits = as.numeric(predict(m1.lm, newdata = .))) %>%
  mutate(fits = expm1(fits)) -> df_16396_lm


df_16397_lm %>%
  mutate(fits = as.numeric(predict(m2.lm, newdata = .))) %>%
  mutate(fits = expm1(fits)) -> df_16397_lm

df_16882_lm %>%
  mutate(fits = as.numeric(predict(m3.lm, newdata = .))) %>%
  mutate(fits = expm1(fits)) -> df_16882_lm

df_16396_lm %>%
  ggplot() +
  geom_point(aes(fits, adjusted_flow, color = "Regression Estimates against Naturalized Flow"),
             alpha =0.4) +
  geom_abline(aes(linetype = "1:1 Line", intercept = 0, slope = 1)) +
  scale_x_continuous(trans = scales::pseudo_log_trans(.001, 10),
                     breaks=c(1, 10, 100, 1000)) + 
  scale_y_continuous(trans = scales::pseudo_log_trans(.001, 10),
                     breaks=c(1, 10, 100, 1000)) +
  coord_cartesian(xlim = c(1,2500),
                  ylim = c(1,2500)) +
  labs(x = "Regression Estimated Flow [cfs]", y = "Naturalized Flow [cfs]",
       subtitle = "SWQM-16396") +
  guides(color = guide_legend(""), linetype = guide_legend("")) +
  theme_ms() +
  theme(legend.margin = margin(0,0,0,0),
        legend.box = "vertical",
        legend.box.margin = margin(0,0,0,0),
        legend.direction = "vertical",
        legend.position = "none")-> p1

df_16396_lm %>%
  ggplot() +
  geom_line(aes(date, fits, color = "Linear Regression Estimated Flow")) +
  geom_line(aes(date, adjusted_flow, color = "Naturalized Flow SWQM 16396"), alpha = 0.4) +
  labs(x = "Date", y = "Mean Daily Flow [cfs]") +
  theme_ms() +
  theme(legend.title = element_blank(),
        legend.direction = "vertical",
        legend.position = "none") -> p2


df_16397_lm %>%
  ggplot() +
  geom_point(aes(fits, adjusted_flow, color = "Regression Estimates against Naturalized Flow"),
             alpha =0.4) +
  geom_abline(aes(linetype = "1:1 Line", intercept = 0, slope = 1)) +
  scale_x_continuous(trans = scales::pseudo_log_trans(.001, 10),
                     breaks=c(1, 10, 100, 1000)) + 
  scale_y_continuous(trans = scales::pseudo_log_trans(.001, 10),
                     breaks=c(1, 10, 100, 1000)) +
  coord_cartesian(xlim = c(.1,100),
                  ylim = c(.1,100)) +
  labs(x = "Regression Estimated Flow [cfs]", y = "Naturalized Flow [cfs]",
       subtitle = "SWQM-16397") +
  guides(color = guide_legend(""), linetype = guide_legend("")) +
  theme_ms() +
  theme(legend.margin = margin(0,0,0,0),
        legend.box = "vertical",
        legend.box.margin = margin(0,0,0,0),
        legend.direction = "vertical",
        legend.position = "none")-> p3

df_16397_lm %>%
  ggplot() +
  geom_line(aes(date, fits, color = "Linear Regression Estimated Flow")) +
  geom_line(aes(date, adjusted_flow, color = "Naturalized Flow SWQM 16397"), alpha = 0.4) +
  labs(x = "Date", y = "Mean Daily Flow [cfs]") +
  theme_ms() +
  theme(legend.title = element_blank(),
        legend.direction = "vertical",
        legend.position = "none") -> p4

df_16882_lm %>%
  ggplot() +
  geom_point(aes(fits, adjusted_flow, color = "Regression Estimates against Naturalized Flow"),
             alpha =0.4) +
  geom_abline(aes(linetype = "1:1 Line", intercept = 0, slope = 1)) +
  scale_x_continuous(trans = scales::pseudo_log_trans(0.001, 10),
                     breaks=c(0, 10, 100, 1000)) + 
  scale_y_continuous(trans = scales::pseudo_log_trans(0.001, 10),
                     breaks=c(0, 10, 100, 1000)) +
  coord_cartesian(xlim = c(0,1000),
                  ylim = c(0,1000)) +
  labs(x = "Regression Estimated Flow [cfs]", y = "Naturalized Flow [cfs]",
       subtitle = "SWQM-16882") +
  guides(color = guide_legend(""), linetype = guide_legend("")) +
  theme_ms() +
  theme(legend.margin = margin(0,0,0,0),
        legend.box = "vertical",
        legend.box.margin = margin(0,0,0,0),
        legend.direction = "vertical")-> p5

df_16882_lm %>%
  ggplot() +
  geom_line(aes(date, fits, color = "Linear Regression Estimated Flow")) +
  geom_line(aes(date, adjusted_flow, color = "Naturalized Flow"), alpha = 0.4) +
  labs(x = "Date", y = "Mean Daily Flow [cfs]") +
  theme_ms() +
  theme(legend.title = element_blank(),
        legend.direction = "vertical") -> p6

agg_png(here::here("Figures/linear-regression.png"),
        width = 8,
        height = 8,
        units = "in",
        res = 300)
(p1 + p2)/(p3 + p4)/(p5 + p6)
dev.off()

# GAM ####



options(mgcv.vc.logrange = 20) #default = 25

m1.gam <- gam(adjusted_flow ~
                s(ewood_precip) +
                s(ewood_tmax) +
                s(lagPrecip) +
                s(wetness) +
                s(et) +
                s(ewood_rh) +
                s(month, bs = "cc"),
               data = df_16396,
               select = TRUE,
               family = Gamma(link = "log"),
               method = "REML")


#m1.gam <- read_rds(here::here("Processed-Data/gam_16396.rds"))
summary(m1.gam)
pacf(resid(m1.gam))

agg_png(here::here("Figures/appendix_marginal_gam1.png"),
        width = 6.5,
        height = 4,
        units = "in",
        res = 300)
patchwork::wrap_plots(
  draw(evaluate_smooth(m1.gam, "s(ewood_precip)"),
       fun = inv_link(m1.gam),
       title = "ƒ(P)",
       xlab = "P"),
  draw(evaluate_smooth(m1.gam, "s(ewood_tmax)"),
       fun = inv_link(m1.gam),
       title = "ƒ(T)",
       xlab = "T"),
  draw(evaluate_smooth(m1.gam, "s(lagPrecip)"),
       fun = inv_link(m1.gam),
       title = expression(ƒ(P["lag"])),
       xlab = expression(P["lag"])),
  draw(evaluate_smooth(m1.gam, "s(wetness)"),
       fun = inv_link(m1.gam),
       title = expression(ƒ(P["sum,3"])),
       xlab = expression(P["sum,3"])),
  draw(evaluate_smooth(m1.gam, "s(et)"),
       fun = inv_link(m1.gam),
       title = expression(ƒ(T["mean,5"])),
       xlab = expression(T["mean,5"])),
  draw(evaluate_smooth(m1.gam, "s(ewood_rh)"),
       fun = inv_link(m1.gam),
       title = "ƒ(H)",
       xlab = "H"),
  draw(evaluate_smooth(m1.gam, "s(month)"),
       fun = inv_link(m1.gam),
       title = "ƒ(M)",
       xlab = "M") + scale_x_continuous(breaks = c(3,6,9,12)),
  ncol = 3) & theme_marginal_ms()
dev.off()



flextable::as_flextable(m1.gam) %>%
  flextable::set_caption("Summary of GAM coefficients at SWQM 16396") %>%
  save_as_docx(values = NULL, 
               path = "Output-Data/GAM_16396.docx", 
               pr_section = NULL)

m2.gam <- gam(adjusted_flow ~
                s(ewood_precip) +
                s(ewood_tmax) +
                s(lagPrecip) +
                s(wetness) +
                s(et) +
                s(ewood_rh) +
                s(month, bs = "cc"),
              data = df_16397,
              select = TRUE,
              family = Gamma(link = "log"),
              method = "REML")

#m2.gam <- read_rds(here::here("Processed-Data/gam_16397.rds"))

agg_png(here::here("Figures/appendix_marginal_gam2.png"),
        width = 6.5,
        height = 4,
        units = "in",
        res = 300)

patchwork::wrap_plots(
  draw(evaluate_smooth(m2.gam, "s(ewood_precip)"),
       fun = inv_link(m2.gam),
       title = "ƒ(P)",
       xlab = "P"),
  draw(evaluate_smooth(m2.gam, "s(ewood_tmax)"),
       fun = inv_link(m2.gam),
       title = "ƒ(T)",
       xlab = "T"),
  draw(evaluate_smooth(m2.gam, "s(lagPrecip)"),
       fun = inv_link(m2.gam),
       title = expression(ƒ(P["lag"])),
       xlab = expression(P["lag"])),
  draw(evaluate_smooth(m2.gam, "s(wetness)"),
       fun = inv_link(m2.gam),
       title = expression(ƒ(P["sum,3"])),
       xlab = expression(P["sum,3"])),
  draw(evaluate_smooth(m2.gam, "s(et)"),
       fun = inv_link(m2.gam),
       title = expression(ƒ(T["mean,5"])),
       xlab = expression(T["mean,5"])),
  draw(evaluate_smooth(m2.gam, "s(ewood_rh)"),
       fun = inv_link(m2.gam),
       title = "ƒ(H)",
       xlab = "H"),
  draw(evaluate_smooth(m2.gam, "s(month)"),
       fun = inv_link(m2.gam),
       title = "ƒ(M)",
       xlab = "M") + scale_x_continuous(breaks = c(3,6,9,12)),
  ncol = 3) & theme_marginal_ms()
dev.off()

flextable::as_flextable(m2.gam) %>%
  flextable::set_caption("Summary of GAM coefficients at SWQM 16397") %>%
  save_as_docx(values = NULL, 
               path = "Output-Data/GAM_16397.docx", 
               pr_section = NULL)


# we need a hurdle model here. We will fit logistic regression to predict 
# the probability of a non-zero flow
# and a scaled-t model with log link to predict the mean of the non-zero data 

m3.gam <- gam(non_zero ~
                s(ewood_precip) +
                s(ewood_tmax) +
                s(lagPrecip) +
                s(wetness) +
                s(et) +
                s(ewood_rh) +
                s(month, bs = "cc"),
              data = df_16882,
              select = TRUE,
              family = binomial(),
              method = "REML",
              control = gam.control(nthreads = 2))


agg_png(here::here("Figures/appendix_marginal_gam3.png"),
        width = 6.5,
        height = 4,
        units = "in",
        res = 300)
patchwork::wrap_plots(
  draw(evaluate_smooth(m3.gam, "s(ewood_precip)"),
       fun = inv_link(m3.gam),
       title = "ƒ(P)",
       xlab = "P"),
  draw(evaluate_smooth(m3.gam, "s(ewood_tmax)"),
       fun = inv_link(m3.gam),
       title = "ƒ(T)",
       xlab = "T"),
  draw(evaluate_smooth(m3.gam, "s(lagPrecip)"),
       fun = inv_link(m3.gam),
       title = expression(ƒ(P["lag"])),
       xlab = expression(P["lag"])),
  draw(evaluate_smooth(m3.gam, "s(wetness)"),
       fun = inv_link(m3.gam),
       title = expression(ƒ(P["sum,3"])),
       xlab = expression(P["sum,3"])),
  draw(evaluate_smooth(m3.gam, "s(et)"),
       fun = inv_link(m3.gam),
       title = expression(ƒ(T["mean,5"])),
       xlab = expression(T["mean,5"])),
  draw(evaluate_smooth(m3.gam, "s(ewood_rh)"),
       fun = inv_link(m3.gam),
       title = "ƒ(H)",
       xlab = "H"),
  draw(evaluate_smooth(m3.gam, "s(month)"),
       fun = inv_link(m3.gam),
       title = "ƒ(M)",
       xlab = "M") + scale_x_continuous(breaks = c(3,6,9,12)),
  ncol = 3) & theme_marginal_ms()

dev.off()
# 
m4.gam <- gam(adjusted_flow ~
                s(ewood_precip) +
                s(ewood_tmax) +
                s(lagPrecip) +
                s(wetness) +
                s(et) +
                s(month, bs = "cc"),
              data = subset(df_16882, non_zero == 1),
              select = TRUE,
              family = Gamma(link = "log"),
              method = "REML",
              control = gam.control(nthreads = 2))


flextable::as_flextable(m3.gam) %>%
  flextable::set_caption("Summary of GAMcoefficients at SWQM 16882")  %>%
  save_as_docx(values = NULL, 
               path = "Output-Data/GAM_16882_logistic.docx", 
               pr_section = NULL)
flextable::as_flextable(m4.gam) %>%
  flextable::set_caption("Summary of GAMcoefficients at SWQM 16882")  %>%
  save_as_docx(values = NULL, 
               path = "Output-Data/GAM_16882.docx", 
               pr_section = NULL)

agg_png(here::here("Figures/appendix_marginal_gam4.png"),
        width = 6.5,
        height = 4,
        units = "in",
        res = 300)

patchwork::wrap_plots(
  draw(evaluate_smooth(m4.gam, "s(ewood_precip)"),
       fun = inv_link(m4.gam),
       title = "ƒ(P)",
       xlab = "P"),
  draw(evaluate_smooth(m4.gam, "s(ewood_tmax)"),
       fun = inv_link(m4.gam),
       title = "ƒ(T)",
       xlab = "T"),
  draw(evaluate_smooth(m4.gam, "s(lagPrecip)"),
       fun = inv_link(m4.gam),
       title = expression(ƒ(P["lag"])),
       xlab = expression(P["lag"])),
  draw(evaluate_smooth(m4.gam, "s(wetness)"),
       fun = inv_link(m4.gam),
       title = expression(ƒ(P["sum,3"])),
       xlab = expression(P["sum,3"])),
  draw(evaluate_smooth(m4.gam, "s(et)"),
       fun = inv_link(m4.gam),
       title = expression(ƒ(T["mean,5"])),
       xlab = expression(T["mean,5"])),
  draw(evaluate_smooth(m4.gam, "s(month)"),
       fun = inv_link(m4.gam),
       title = "ƒ(M)",
       xlab = "M") + scale_x_continuous(breaks = c(3,6,9,12)),
  ncol = 3) & theme_marginal_ms()
dev.off()

## predictions 

df_16396 %>%
  bind_cols(
    as_tibble(predict(m1.gam, df_16396, type = "response"))) %>%
  dplyr::rename(response = value)-> df_16396_gam

df_16397 %>%
  bind_cols(
    as_tibble(predict(m2.gam, df_16397, type = "response"))) %>%
  dplyr::rename(response = value) -> df_16397_gam


df_16882 %>%
  bind_cols(
    as_tibble(predict(m3.gam, df_16882, se.fit = FALSE,
                      type = "response"))) %>%
  mutate(pred_zero = case_when(
    value < 0.5 ~ 0,
    value >= 0.5 ~ 1)) %>%
  bind_cols(
    as_tibble(predict(m4.gam, df_16882, se.fit = FALSE,
                      type = "response")) %>%
      dplyr::rename(response = value)) %>%
  mutate(response = case_when(
    pred_zero == 0 ~ 0,
    pred_zero != 0 ~ response)) %>%
  mutate(response = case_when(
    response < 0 ~ 0,
    response >= 0 ~ response)) -> df_16882_gam



df_16396_gam %>%
  ggplot() +
  geom_point(aes(response, adjusted_flow,
                 color = "GAM Estimates against Naturalized Flow"),
             alpha = 0.4) +
  geom_abline(aes(linetype = "1:1 Line", intercept = 0, slope = 1)) +
  scale_x_continuous(trans = scales::pseudo_log_trans(0.001, 10),
                     breaks=c(1, 10, 100, 1000)) +
  scale_y_continuous(trans = scales::pseudo_log_trans(0.001, 10),
                     breaks=c(1, 10, 100, 1000)) +
  coord_cartesian(xlim = c(1,1000),
                  ylim = c(1,1000)) +
  labs(x = "GAM Estimated Flow [cfs]", y = "Naturalized Flow [cfs]",
       subtitle = "SWQM-16396") +
  guides(color = guide_legend(""), linetype = guide_legend("")) +
  theme_ms() +
  theme(legend.margin = margin(0,0,0,0),
        legend.box = "vertical",
        legend.box.margin = margin(0,0,0,0),
        legend.direction = "vertical",
        legend.position = "none")-> p1

df_16396_gam %>%
  ggplot() +
  geom_line(aes(date, response, color =  "GAM Estimated Flow")) +
  geom_line(aes(date, adjusted_flow, color = "Naturalized Flow SWQM 16396"),
            alpha = 0.4) +
  labs(x = "Date", y = "Mean Daily Flow [cfs]") +
  theme_ms() +
  theme(legend.title = element_blank(),
        legend.direction = "vertical",
        legend.position = "none") -> p2




df_16397_gam %>%
  ggplot() +
  geom_point(aes(response, adjusted_flow,
                 color = "GAM Estimates against Naturalized Flow"),
             alpha = 0.4) +
  geom_abline(aes(linetype = "1:1 Line", intercept = 0, slope = 1)) +
  scale_x_continuous(trans = scales::pseudo_log_trans(0.001, 10),
                     breaks=c(1, 10, 100)) + 
  scale_y_continuous(trans = scales::pseudo_log_trans(0.001, 10),
                     breaks=c(1, 10, 100)) +
  coord_cartesian(xlim = c(1,100),
                  ylim = c(1,100)) +
  labs(x = "GAM Estimated Flow [cfs]", y = "Naturalized Flow [cfs]",
       subtitle = "SWQM-16397") +
  guides(color = guide_legend(""), linetype = guide_legend("")) +
  theme_ms() +
  theme(legend.margin = margin(0,0,0,0),
        legend.box = "vertical",
        legend.box.margin = margin(0,0,0,0),
        legend.direction = "vertical",
        legend.position = "none")-> p3

df_16397_gam %>%
  ggplot() +
  geom_line(aes(date, response, color =  "GAM Estimated Flow")) +
  geom_line(aes(date, adjusted_flow, color = "Naturalized Flow SWQM 16397"),
            alpha = 0.4) +
  labs(x = "Date", y = "Mean Daily Flow [cfs]") +
  theme_ms() +
  theme(legend.title = element_blank(),
        legend.direction = "vertical",
        legend.position = "none") -> p4


df_16882_gam %>%
  ggplot() +
  geom_point(aes(response, adjusted_flow,
                 color = "GAM Estimates against Naturalized Flow"),
             alpha = 0.4) +
  geom_abline(aes(linetype = "1:1 Line", intercept = 0, slope = 1)) +
  scale_x_continuous(trans = scales::pseudo_log_trans(0.001, 10),
                     breaks=c(0, 1, 10, 100)) + 
  scale_y_continuous(trans = scales::pseudo_log_trans(0.001, 10),
                     breaks=c(0, 1, 10, 100)) +
  coord_cartesian(xlim = c(0,100),
                  ylim = c(0,100)) +
  labs(x = "GAM Estimated Flow [cfs]", y = "Naturalized Flow [cfs]",
       subtitle = "SWQM-16882") +
  guides(color = guide_legend(""), linetype = guide_legend("")) +
  theme_ms() +
  theme(legend.margin = margin(0,0,0,0),
        legend.box = "vertical",
        legend.box.margin = margin(0,0,0,0),
        legend.direction = "vertical")-> p5

df_16882_gam %>%
  ggplot() +
  geom_line(aes(date, response, color =  "GAM Estimated Flow")) +
  geom_line(aes(date, adjusted_flow, color = "Naturalized Flow"),
            alpha = 0.4) +
  labs(x = "Date", y = "Mean Daily Flow [cfs]") +
  theme_ms() +
  theme(legend.title = element_blank(),
        legend.direction = "vertical") -> p6


agg_png(here::here("Figures/gam.png"),
        width = 8,
        height = 8,
        units = "in",
        res = 300)
(p1+p2)/(p3 +p4)/(p5+p6)
dev.off()

# Goodness of fit ####


tibble(model = c("DAR_08065800", "DAR_08109800", "DAR_08110100",
                 "Linear Regression",
                 "GAM"),
       'SWQM-16396' = c(
         hydroGOF::mNSE(dar_results_16396$DAR_Q_08065800, as.numeric(dar_results_16396$adjusted_flow)),
         hydroGOF::mNSE(dar_results_16396$DAR_Q_08109800, as.numeric(dar_results_16396$adjusted_flow)),
         hydroGOF::mNSE(dar_results_16396$DAR_Q_08110100, as.numeric(dar_results_16396$adjusted_flow)),
         hydroGOF::mNSE(df_16396_lm$fits, df_16396_lm$adjusted_flow),
         hydroGOF::mNSE(as.numeric(df_16396_gam$response), df_16396_gam$adjusted_flow)),
       'SWQM-16397' = c(
         hydroGOF::mNSE(dar_results_16397$DAR_Q_08065800, as.numeric(dar_results_16397$adjusted_flow)),
         hydroGOF::mNSE(dar_results_16397$DAR_Q_08109800, as.numeric(dar_results_16397$adjusted_flow)),
         hydroGOF::mNSE(dar_results_16397$DAR_Q_08110100, as.numeric(dar_results_16397$adjusted_flow)),
         hydroGOF::mNSE(df_16397_lm$fits, df_16397_lm$adjusted_flow),
         hydroGOF::mNSE(as.numeric(df_16397_gam$response), df_16397_gam$adjusted_flow)),
       'SWQM-16882' = c(
         hydroGOF::mNSE(dar_results_16882$DAR_Q_08065800, as.numeric(dar_results_16882$adjusted_flow)),
         hydroGOF::mNSE(dar_results_16882$DAR_Q_08109800, as.numeric(dar_results_16882$adjusted_flow)),
         hydroGOF::mNSE(dar_results_16882$DAR_Q_08110100, as.numeric(dar_results_16882$adjusted_flow)),
         hydroGOF::mNSE(df_16882_lm$fits, df_16882_lm$adjusted_flow),
         hydroGOF::mNSE(as.numeric(df_16882_gam$response), df_16882_gam$adjusted_flow)))

tibble(model = c("DAR_08065800", "DAR_08109800", "DAR_08110100",
                 "Linear Regression",
                 "GAM"),
       'SWQM-16396' = c(
         hydroGOF::KGE(dar_results_16396$DAR_Q_08065800, as.numeric(dar_results_16396$adjusted_flow)),
         hydroGOF::KGE(dar_results_16396$DAR_Q_08109800, as.numeric(dar_results_16396$adjusted_flow)),
         hydroGOF::KGE(dar_results_16396$DAR_Q_08110100, as.numeric(dar_results_16396$adjusted_flow)),
         hydroGOF::KGE(df_16396_lm$fits, df_16396_lm$adjusted_flow),
         hydroGOF::KGE(as.numeric(df_16396_gam$response), df_16396_gam$adjusted_flow)),
       'SWQM-16397' = c(
         hydroGOF::KGE(dar_results_16397$DAR_Q_08065800, as.numeric(dar_results_16397$adjusted_flow)),
         hydroGOF::KGE(dar_results_16397$DAR_Q_08109800, as.numeric(dar_results_16397$adjusted_flow)),
         hydroGOF::KGE(dar_results_16397$DAR_Q_08110100, as.numeric(dar_results_16397$adjusted_flow)),
         hydroGOF::KGE(df_16397_lm$fits, df_16397_lm$adjusted_flow),
         hydroGOF::KGE(as.numeric(df_16397_gam$response), df_16397_gam$adjusted_flow)),
       'SWQM-16882' = c(
         hydroGOF::KGE(dar_results_16882$DAR_Q_08065800, as.numeric(dar_results_16882$adjusted_flow)),
         hydroGOF::KGE(dar_results_16882$DAR_Q_08109800, as.numeric(dar_results_16882$adjusted_flow)),
         hydroGOF::KGE(dar_results_16882$DAR_Q_08110100, as.numeric(dar_results_16882$adjusted_flow)),
         hydroGOF::KGE(df_16882_lm$fits, df_16882_lm$adjusted_flow),
         hydroGOF::KGE(as.numeric(df_16882_gam$response), df_16882_gam$adjusted_flow)))



tibble(model = c("DAR_08065800", "DAR_08109800", "DAR_08110100",
                 "Linear Regression",
                 "GAM"),
       'SWQM-16396' = c(
         hydroGOF::mae(dar_results_16396$DAR_Q_08065800, as.numeric(dar_results_16396$adjusted_flow)),
         hydroGOF::mae(dar_results_16396$DAR_Q_08109800, as.numeric(dar_results_16396$adjusted_flow)),
         hydroGOF::mae(dar_results_16396$DAR_Q_08110100, as.numeric(dar_results_16396$adjusted_flow)),
         hydroGOF::mae(df_16396_lm$fits, df_16396_lm$adjusted_flow),
         hydroGOF::mae(as.numeric(df_16396_gam$response), df_16396_gam$adjusted_flow)),
       'SWQM-16397' = c(
         hydroGOF::mae(dar_results_16397$DAR_Q_08065800, as.numeric(dar_results_16397$adjusted_flow)),
         hydroGOF::mae(dar_results_16397$DAR_Q_08109800, as.numeric(dar_results_16397$adjusted_flow)),
         hydroGOF::mae(dar_results_16397$DAR_Q_08110100, as.numeric(dar_results_16397$adjusted_flow)),
         hydroGOF::mae(df_16397_lm$fits, df_16397_lm$adjusted_flow),
         hydroGOF::mae(as.numeric(df_16397_gam$response), df_16397_gam$adjusted_flow)),
       'SWQM-16882' = c(
         hydroGOF::mae(dar_results_16882$DAR_Q_08065800, as.numeric(dar_results_16882$adjusted_flow)),
         hydroGOF::mae(dar_results_16882$DAR_Q_08109800, as.numeric(dar_results_16882$adjusted_flow)),
         hydroGOF::mae(dar_results_16882$DAR_Q_08110100, as.numeric(dar_results_16882$adjusted_flow)),
         hydroGOF::mae(df_16882_lm$fits, df_16882_lm$adjusted_flow),
         hydroGOF::mae(as.numeric(df_16882_gam$response), df_16882_gam$adjusted_flow)))

#### Cross validation #####

## use futures to parallel this.
library(furrr)
plan(multisession, workers = 3)

df_16396 %>%
  mc_cv(7/10, times = 100, strata = adjusted_flow, breaks = 25) %>%
  mutate(train = map(splits, ~as.data.frame(.x)),
         test = map(splits, ~as.data.frame(.x, data = "assessment"))) %>%
  mutate(model = future_map(train, ~gam(adjusted_flow ~
                                          s(ewood_precip) +
                                          s(ewood_tmax) +
                                          s(lagPrecip) +
                                          s(wetness) +
                                          s(et) +
                                          s(ewood_rh) +
                                          s(month, bs = "cc"),
                                        data = .x,
                                        select = TRUE,
                                        family = Gamma(link = "log"),
                                        method = "REML"),
                            .progress = TRUE)) %>%
  mutate(preds = map2(test, model, ~predict(.y,
                                            newdata = .x,
                                            type = "response",
                                            se.fit = FALSE))) %>%
  mutate(NSE = map2_dbl(test, preds, ~mNSE(as.numeric(.y), .x$adjusted_flow)),
         KGE = map2_dbl(test, preds, ~KGE(as.numeric(.y), .x$adjusted_flow)),
         nRMSE = map2_dbl(test, preds, ~hydroGOF::nrmse(as.numeric(.y), .x$adjusted_flow, norm="maxmin")),
         r2 = map2_dbl(test, preds, ~cor(as.numeric(.y), .x$adjusted_flow, method = "pearson"))) -> df_16396_kfold

df_16397 %>%
  mc_cv(7/10, times = 100, strata = adjusted_flow, breaks = 25) %>%
  mutate(train = map(splits, ~as.data.frame(.x)),
         test = map(splits, ~as.data.frame(.x, data = "assessment"))) %>%
  mutate(model = future_map(train, ~gam(adjusted_flow ~
                                          s(ewood_precip) +
                                          s(ewood_tmax) +
                                          s(lagPrecip) +
                                          s(wetness) +
                                          s(et) +
                                          s(ewood_rh) +
                                          s(month, bs = "cc"),
                                        data = .x,
                                        select = TRUE,
                                        family = Gamma(link = "log"),
                                        method = "REML"),
                            .progress = TRUE)) %>%
  mutate(preds = map2(test, model, ~predict(.y,
                                            newdata = .x,
                                            type = "response",
                                            se.fit = FALSE))) %>%
  mutate(NSE = map2_dbl(test, preds, ~mNSE(as.numeric(.y), .x$adjusted_flow)),
         KGE = map2_dbl(test, preds, ~KGE(as.numeric(.y), .x$adjusted_flow)),
         nRMSE = map2_dbl(test, preds, ~hydroGOF::nrmse(as.numeric(.y), .x$adjusted_flow, norm="maxmin")),
         r2 = map2_dbl(test, preds, ~cor(as.numeric(.y), .x$adjusted_flow, method = "pearson"))) -> df_16397_kfold





df_16882 %>%
  mc_cv(9.5/10, times = 100, strata = non_zero) %>%
  mutate(train = map(splits, ~as.data.frame(.x)),
         test = map(splits, ~as.data.frame(.x, data = "assessment"))) %>%
  mutate(model1 = future_map(train, ~gam(non_zero ~
                                           s(ewood_precip) +
                                           s(ewood_tmax) +
                                           s(lagPrecip) +
                                           s(wetness) +
                                           s(et) +
                                           s(ewood_rh) +
                                           s(month, bs = "cc"),
                                         data = .x,
                                         select = TRUE,
                                         family = binomial(),
                                         method = "REML"),
                            .progress = TRUE),
         model2 = future_map(train,
                             ~gam(adjusted_flow ~
                                    s(ewood_precip) +
                                    s(ewood_tmax) +
                                    s(lagPrecip) +
                                    s(wetness) +
                                    s(et) +
                                    s(month, bs = "cc"),
                                  data = subset(.x, non_zero == 1),
                                  select = TRUE,
                                  family = Gamma(link = "log"),
                                  method = "REML"))) -> df_16882_kfold

df_16882_kfold %>%
  mutate(preds = pmap(.l = list(model1, model2, test),
                      ~{preds <- ..3 %>%
                          bind_cols(
                            as_tibble(predict(..1,
                                              ..3,
                                              se.fit = FALSE,
                                              type = "response"))) %>%
                          mutate(pred_zero = case_when(value < 0.5 ~ 0,
                                                       value >= 0.5 ~1)) %>%
                          bind_cols(
                            as_tibble(predict(..2,
                                              ..3,
                                              se.fit = FALSE,
                                              type = "response")) %>%
                              dplyr::rename(response = value)) %>%
                          mutate(response = case_when(
                            pred_zero == 0 ~ 0,
                            pred_zero != 0 ~ response)) %>%
                          mutate(response = case_when(
                            response < 0 ~ 0,
                            response >= 0 ~ response))
                      preds$response
                      }
                          )) %>%
  mutate(NSE = map2_dbl(test, preds, ~mNSE(as.numeric(.y), .x$adjusted_flow)),
         KGE = map2_dbl(test, preds, ~KGE(as.numeric(.y), .x$adjusted_flow, method = "2012")),
         nRMSE = map2_dbl(test, preds, ~hydroGOF::nrmse(as.numeric(.y), .x$adjusted_flow, norm="maxmin")),
         r2 = map2_dbl(test, preds, ~cor(as.numeric(.y), .x$adjusted_flow, method = "pearson"))) -> df_16882_kfold
    



df_16396_kfold %>%
  pivot_longer(cols = c(nRMSE, NSE, KGE, r2)) %>%
  mutate(Site = "16396") %>%
  bind_rows(
    df_16397_kfold %>%
      pivot_longer(cols = c(nRMSE, NSE, KGE, r2)) %>%
      mutate(Site = "16397") 
  ) %>%
  select(Site, name, value) %>%
  bind_rows(
    df_16882_kfold %>%
      pivot_longer(cols = c(nRMSE, NSE, KGE, r2)) %>%
      mutate(Site = "16882") %>%
      select(Site, name, value)) -> cross_validation_results


agg_png(here::here("Figures/cross-validation.png"),
        width = 6.5,
        height = 8.5,
        units = "in",
        res = 300)
wrap_plots(
cross_validation_results %>%
  filter(name == "nRMSE") %>%
  ggplot() +
  stat_density_ridges(aes(x = value, y = Site, vline_color = ..quantile.., scale = 1),
                      quantile_lines = TRUE, quantiles = 0.5) +
  stat_summaryh(fun.x=median, geom="text",
                  aes(x = value, y = Site,
                      label=paste0(round(..x.., 2))),
                  angle=90, position=position_nudge(x=-1, y = 0.15), size = 3) +
  scale_discrete_manual("vline_color",
                        values = c("dodgerblue", "black"), 
                        breaks = c(1),
                        labels = c("median"),
                        name = NULL) +
  coord_cartesian(xlim = c(0,100)) +
  labs(x = "nRMSE (%)",
       y = "") +
  theme_ms() +
  theme(legend.position = "none"),


cross_validation_results %>%
  filter(name == "NSE") %>%
  ggplot() +
  stat_density_ridges(aes(x = value, y = Site, vline_color = ..quantile.., scale = 1),
                      quantile_lines = TRUE, quantiles = 0.5) +
  stat_summaryh(fun.x=median, geom="text",
                aes(x = value, y = Site,
                    label=paste0(round(..x.., 2))),
                angle=90, position=position_nudge(x=-.1, y = 0.15), size = 3) +
  scale_discrete_manual("vline_color",
                        values = c("dodgerblue", "black"), 
                        breaks = c(1),
                        labels = c("median"),
                        name = NULL) +
  coord_cartesian(xlim = c(-2.5,1)) +
  labs(x = "NSE",
       y = "") +
  theme_ms() +
  theme(legend.position = "none"),

cross_validation_results %>%
  filter(name == "KGE") %>%
  ggplot() +
  stat_density_ridges(aes(x = value, y = Site, vline_color = ..quantile.., scale = 1),
                      quantile_lines = TRUE, quantiles = 0.5) +
  stat_summaryh(fun.x=median, geom="text",
                aes(x = value, y = Site,
                    label=paste0(round(..x.., 2))),
                angle=90, position=position_nudge(x=-.1, y = 0.15), size = 3) +
  scale_discrete_manual("vline_color",
                        values = c("dodgerblue", "black"), 
                        breaks = c(1),
                        labels = c("median"),
                        name = NULL) +
  coord_cartesian(xlim = c(-2.5,1)) +
  labs(x = "KGE",
       y = "") +
  theme_ms() +
  theme(legend.position = "none"),
cross_validation_results %>%
  filter(name == "r2") %>%
  ggplot() +
  stat_density_ridges(aes(x = value, y = Site, vline_color = ..quantile.., scale = 1),
                      quantile_lines = TRUE, quantiles = 0.5) +
  stat_summaryh(fun.x=median, geom="text",
                aes(x = value, y = Site,
                    label=paste0(round(..x.., 2))),
                angle=90, position=position_nudge(x=-.01, y = 0.15), size = 3) +
  scale_discrete_manual("vline_color",
                        values = c("dodgerblue", "black"), 
                        breaks = c(1),
                        labels = c("median"),
                        name = NULL) +
  coord_cartesian(xlim = c(0,1)) +
  labs(x = "r2",
       y = "") +
  theme_ms() +
  theme(legend.position = "none"),

ncol = 2)
dev.off()


write_rds(m1.gam, here::here("Processed-Data/Models/m1_gam.rds"))
write_rds(m2.gam, here::here("Processed-Data/Models/m2_gam.rds"))
write_rds(m3.gam, here::here("Processed-Data/Models/m3_gam.rds"))
write_rds(m4.gam, here::here("Processed-Data/Models/m4_gam.rds"))




## linear regression CV
## use futures to parallel this.
library(furrr)
plan(multisession, workers = 3)

df_16396_lm %>%
  mc_cv(7/10, times = 100, strata = adjusted_flow, breaks = 25) %>%
  mutate(train = map(splits, ~as.data.frame(.x)),
         test = map(splits, ~as.data.frame(.x, data = "assessment"))) %>%
  mutate(model = future_map(train, ~  lm(log_Q ~ log1p(Flow_08065800) + 
                                           log1p(Flow_08109800) + 
                                           log1p(Flow_08110100) + 
                                           log1p(lag_Flow_08065800) + 
                                           log1p(lag_Flow_08109800) + 
                                           log1p(lag_Flow_08110100),
                                         data = .x),
                            .progress = TRUE)) %>%
  mutate(preds = map2(test, model, ~predict(.y,
                                            .x,
                                            type = "response"))) %>%
  mutate(NSE = map2_dbl(test, preds, ~mNSE(expm1(as.numeric(.y)), .x$adjusted_flow)),
         KGE = map2_dbl(test, preds, ~KGE(expm1(as.numeric(.y)), .x$adjusted_flow)),
         nRMSE = map2_dbl(test, preds, ~hydroGOF::nrmse(expm1(as.numeric(.y)), .x$adjusted_flow, norm="maxmin")),
         r2 = map2_dbl(test, preds, ~cor(expm1(as.numeric(.y)), .x$adjusted_flow, 
                                         use = "complete.obs", method = "pearson"))) -> df_16396_kfold_lm
  

df_16397_lm %>%
  mc_cv(7/10, times = 100, strata = adjusted_flow, breaks = 25) %>%
  mutate(train = map(splits, ~as.data.frame(.x)),
         test = map(splits, ~as.data.frame(.x, data = "assessment"))) %>%
  mutate(model = future_map(train, ~  lm(log_Q ~ log1p(Flow_08065800) + 
                                           log1p(Flow_08109800) + 
                                           log1p(Flow_08110100) + 
                                           log1p(lag_Flow_08065800) + 
                                           log1p(lag_Flow_08109800) + 
                                           log1p(lag_Flow_08110100),
                                         data = .x),
                            .progress = TRUE)) %>%
  mutate(preds = map2(test, model, ~predict(.y,
                                            .x,
                                            type = "response"))) %>%
  mutate(NSE = map2_dbl(test, preds, ~mNSE(expm1(as.numeric(.y)), .x$adjusted_flow)),
         KGE = map2_dbl(test, preds, ~KGE(expm1(as.numeric(.y)), .x$adjusted_flow)),
         nRMSE = map2_dbl(test, preds, ~hydroGOF::nrmse(expm1(as.numeric(.y)), .x$adjusted_flow, norm="maxmin")),
         r2 = map2_dbl(test, preds, ~cor(expm1(as.numeric(.y)), .x$adjusted_flow, 
                                         use = "complete.obs", method = "pearson"))) -> df_16397_kfold_lm

df_16882_lm %>%
  mc_cv(7/10, times = 100, strata = adjusted_flow, breaks = 25) %>%
  mutate(train = map(splits, ~as.data.frame(.x)),
         test = map(splits, ~as.data.frame(.x, data = "assessment"))) %>%
  mutate(model = future_map(train, ~  lm(log_Q ~ log1p(Flow_08065800) + 
                                           log1p(Flow_08109800) + 
                                           log1p(Flow_08110100) + 
                                           log1p(lag_Flow_08065800) + 
                                           log1p(lag_Flow_08109800) + 
                                           log1p(lag_Flow_08110100),
                                         data = .x),
                            .progress = TRUE)) %>%
  mutate(preds = map2(test, model, ~predict(.y,
                                            .x,
                                            type = "response"))) %>%
  mutate(NSE = map2_dbl(test, preds, ~mNSE(expm1(as.numeric(.y)), .x$adjusted_flow)),
         KGE = map2_dbl(test, preds, ~KGE(expm1(as.numeric(.y)), .x$adjusted_flow)),
         nRMSE = map2_dbl(test, preds, ~hydroGOF::nrmse(expm1(as.numeric(.y)), .x$adjusted_flow, norm="maxmin")),
         r2 = map2_dbl(test, preds, ~cor(expm1(as.numeric(.y)), .x$adjusted_flow, 
                                         use = "complete.obs", method = "pearson"))) -> df_16882_kfold_lm



df_16396_kfold_lm %>%
  pivot_longer(cols = c(nRMSE, NSE, KGE, r2)) %>%
  mutate(Site = "16396") %>%
  bind_rows(
    df_16397_kfold_lm %>%
      pivot_longer(cols = c(nRMSE, NSE, KGE, r2)) %>%
      mutate(Site = "16397") 
  ) %>%
  select(Site, name, value) %>%
  bind_rows(
    df_16882_kfold_lm %>%
      pivot_longer(cols = c(nRMSE, NSE, KGE, r2)) %>%
      mutate(Site = "16882") %>%
      select(Site, name, value)) -> cross_validation_results_lm

agg_png(here::here("Figures/cross-validation_lm.png"),
        width = 6.5,
        height = 8.5,
        units = "in",
        res = 300)
wrap_plots(
  cross_validation_results_lm %>%
    filter(name == "nRMSE") %>%
    ggplot() +
    stat_density_ridges(aes(x = value, y = Site, vline_color = ..quantile.., scale = 1),
                        quantile_lines = TRUE, quantiles = 0.5) +
    stat_summaryh(fun.x=median, geom="text",
                  aes(x = value, y = Site,
                      label=paste0(round(..x.., 2))),
                  angle=90, position=position_nudge(x=-1, y = 0.15), size = 3) +
    scale_discrete_manual("vline_color",
                          values = c("dodgerblue", "black"), 
                          breaks = c(1),
                          labels = c("median"),
                          name = NULL) +
    coord_cartesian(xlim = c(0,100)) +
    labs(x = "nRMSE (%)",
         y = "") +
    theme_ms() +
    theme(legend.position = "none"),
  
  
  cross_validation_results_lm %>%
    filter(name == "NSE") %>%
    ggplot() +
    stat_density_ridges(aes(x = value, y = Site, vline_color = ..quantile.., scale = 1),
                        quantile_lines = TRUE, quantiles = 0.5) +
    stat_summaryh(fun.x=median, geom="text",
                  aes(x = value, y = Site,
                      label=paste0(round(..x.., 2))),
                  angle=90, position=position_nudge(x=-.1, y = 0.15), size = 3) +
    scale_discrete_manual("vline_color",
                          values = c("dodgerblue", "black"), 
                          breaks = c(1),
                          labels = c("median"),
                          name = NULL) +
    coord_cartesian(xlim = c(-1,1)) +
    labs(x = "NSE",
         y = "") +
    theme_ms() +
    theme(legend.position = "none"),
  
  cross_validation_results_lm %>%
    filter(name == "KGE") %>%
    ggplot() +
    stat_density_ridges(aes(x = value, y = Site, vline_color = ..quantile.., scale = 1),
                        quantile_lines = TRUE, quantiles = 0.5) +
    stat_summaryh(fun.x=median, geom="text",
                  aes(x = value, y = Site,
                      label=paste0(round(..x.., 2))),
                  angle=90, position=position_nudge(x=-.1, y = 0.15), size = 3) +
    scale_discrete_manual("vline_color",
                          values = c("dodgerblue", "black"), 
                          breaks = c(1),
                          labels = c("median"),
                          name = NULL) +
    coord_cartesian(xlim = c(-1,1)) +
    labs(x = "KGE",
         y = "") +
    theme_ms() +
    theme(legend.position = "none"),
  cross_validation_results_lm %>%
    filter(name == "r2") %>%
    ggplot() +
    stat_density_ridges(aes(x = value, y = Site, vline_color = ..quantile.., scale = 1),
                        quantile_lines = TRUE, quantiles = 0.5) +
    stat_summaryh(fun.x=median, geom="text",
                  aes(x = value, y = Site,
                      label=paste0(round(..x.., 2))),
                  angle=90, position=position_nudge(x=-.01, y = 0.15), size = 3) +
    scale_discrete_manual("vline_color",
                          values = c("dodgerblue", "black"), 
                          breaks = c(1),
                          labels = c("median"),
                          name = NULL) +
    coord_cartesian(xlim = c(0,1)) +
    labs(x = "r2",
         y = "") +
    theme_ms() +
    theme(legend.position = "none"),
  
  ncol = 2)
dev.off()


## Flow Duration Curve ###########

agg_png(here::here("Figures/fdc_16396.png"),
        width = 6.5,
        height = 4.5,
        units = "in",
        res = 300)
df_16396_gam %>%
  left_join(df_16396_lm %>% select(date, fits), by = "date") %>%
  arrange(desc(adjusted_flow)) %>%
  mutate(adjusted_flow_exceedance = 1 - cume_dist(adjusted_flow)) %>%
  arrange(desc(response)) %>%
  mutate(gam_flow_exceedance = 1 - cume_dist(response)) %>%
  arrange(desc(fits)) %>%
  mutate(lm_flow_exceedance = 1 - cume_dist(fits)) %>%
  ggplot() +
  geom_line(aes(adjusted_flow_exceedance, adjusted_flow, color = "Measured FDC")) +
  geom_line(aes(gam_flow_exceedance, response, color = "GAM FDC")) +
  geom_line(aes(lm_flow_exceedance, fits, color = "LR FDC")) +
  scale_y_log10() +
  labs(x="Proportion of Days Flow Exceeded",
       y="Mean Daily Flow (cfs)") +
  theme_ms() +
  theme(legend.title = element_blank())
dev.off()


agg_png(here::here("Figures/fdc_16397.png"),
        width = 6.5,
        height = 4.5,
        units = "in",
        res = 300)
df_16397_gam %>%
  left_join(df_16397_lm %>% select(date, fits), by = "date") %>%
  arrange(desc(adjusted_flow)) %>%
  mutate(adjusted_flow_exceedance = 1 - cume_dist(adjusted_flow)) %>%
  arrange(desc(response)) %>%
  mutate(gam_flow_exceedance = 1 - cume_dist(response)) %>%
  arrange(desc(fits)) %>%
  mutate(lm_flow_exceedance = 1 - cume_dist(fits)) %>%
  ggplot() +
  geom_line(aes(adjusted_flow_exceedance, adjusted_flow, color = "Measured FDC")) +
  geom_line(aes(gam_flow_exceedance, response, color = "GAM FDC")) +
  geom_line(aes(lm_flow_exceedance, fits, color = "LR FDC")) +
  scale_y_log10() +
  labs(x="Proportion of Days Flow Exceeded",
       y="Mean Daily Flow (cfs)") +
  theme_ms() +
  theme(legend.title = element_blank())
dev.off()


agg_png(here::here("Figures/fdc_16882.png"),
        width = 6.5,
        height = 4.5,
        units = "in",
        res = 300)
df_16882_gam %>%
  left_join(df_16882_lm %>% select(date, fits), by = "date") %>%
  arrange(desc(adjusted_flow)) %>%
  mutate(adjusted_flow_exceedance = 1 - cume_dist(adjusted_flow)) %>%
  arrange(desc(response)) %>%
  mutate(gam_flow_exceedance = 1 - cume_dist(response)) %>%
  arrange(desc(fits)) %>%
  mutate(lm_flow_exceedance = 1 - cume_dist(fits)) %>%
  ggplot() +
  geom_line(aes(adjusted_flow_exceedance, adjusted_flow, color = "Measured FDC")) +
  geom_line(aes(gam_flow_exceedance, response, color = "GAM FDC")) +
  geom_line(aes(lm_flow_exceedance, fits, color = "LR FDC")) +
  scale_y_continuous(trans = "log10") +
  labs(x="Proportion of Days Flow Exceeded",
       y="Mean Daily Flow (cfs)") +
  theme_ms() +
  theme(legend.title = element_blank())
dev.off()


# predict full time series ##########


### Precip ####

easterwood_precip <- read_csv("Processed-Data/noaa_precip/easterwood.csv") %>%
  select(date, station, value = dailyprecipitation) %>%
  mutate(value =  set_units(value, "in"))
easterwood_tmax <- read_csv("Processed-Data/noaa_precip/easterwood.csv") %>%
  select(date, station, value = dailymaximumdrybulbtemperature) %>%
  mutate(value =  set_units(value, "°F"))
easterwood_rh <- read_csv("Processed-Data/noaa_precip/easterwood.csv") %>%
  select(date, station, value = dailyaveragerelativehumidity) 

read_csv("Processed-Data/model_df.csv",
         col_types = cols(
           Site = col_character(),
           date = col_date(format = ""),
           mean_daily = col_double()
         )) %>%
  bind_rows(tibble(Site = c(rep("SWQM-16396",3349),rep("SWQM-16397",3349),rep("SWQM-16882",3349)),
                   date = rep(seq.Date(as.Date("2011-01-01"), as.Date("2020-03-02"), by = "day"),3),
                   mean_daily = NA)) %>%
  arrange(date) %>%
  left_join(easterwood_precip, by = c("date" = "date")) %>%
  mutate(value = as.numeric(value)) %>%
  dplyr::rename(ewood_precip = value) %>%
  left_join(easterwood_tmax, by = c("date" = "date")) %>%
  mutate(value = as.numeric(value)) %>%
  dplyr::rename(ewood_tmax = value) %>%
  left_join(easterwood_rh, by = c("date" = "date")) %>%
  mutate(value = as.numeric(value)) %>%
  dplyr::rename(ewood_rh = value) %>%
  dplyr::select(Site, date, mean_daily, ewood_precip, ewood_tmax, ewood_rh) %>%
  left_join(wwtf %>% pivot_wider(id_cols = date, names_from = npdes_id, values_from = cfs),
            by = c("date" = "date")) %>%
  ## remove WWTF influence from discharge record
  mutate(adjusted_flow = case_when(
    Site == "SWQM-16396" ~ mean_daily - TX0025071 - TX0113603, 
    Site == "SWQM-16882" ~ mean_daily - TX0025071,
    Site == "SWQM-16397" ~ mean_daily)) %>%
  mutate(adjusted_flow = case_when(
    adjusted_flow < 0 ~ 0,
    adjusted_flow >= 0 ~ adjusted_flow)) %>%
  mutate(month = lubridate::month(date))-> final_prediction_df


final_prediction_df %>%
  dplyr::filter(Site == "SWQM-16396") %>%
  arrange(date) -> df_16396_final

df_16396_final %>%
  mutate(lagPrecip = lag(ewood_precip),
         wetness = map(row_number(.$date),
                       ~{if(.x - 1 <= 0) {df_16396_final$ewood_precip[.x]}
                         if(.x - 2 <= 0) {sum(df_16396_final$ewood_precip[.x],
                                              df_16396_final$ewood_precip[.x-1])}
                         if(.x - 2 > 0) {
                           sum(
                             df_16396_final$ewood_precip[.x],
                             df_16396_final$ewood_precip[.x-1],
                             df_16396_final$ewood_precip[.x-2],
                             na.rm = TRUE
                           ) 
                         }}),
         et = map(row_number(.$date),
                  ~{if(.x - 1 <= 0) {df_16396_final$ewood_tmax[.x]}
                    if(.x - 2 <= 0) {df_16396_final$ewood_tmax[.x-1]}
                    if(.x - 3 <= 0) { mean(c(df_16396_final$ewood_tmax[.x-1],
                                             df_16396_final$ewood_tmax[.x-2]),
                                           na.rm = TRUE)}
                    if(.x - 4 <= 0) { mean(c(df_16396_final$ewood_tmax[.x-1],
                                             df_16396_final$ewood_tmax[.x-2],
                                             df_16396_final$ewood_tmax[.x-3]),
                                           na.rm = TRUE)}
                    if(.x - 5 <= 0) { mean(c(df_16396_final$ewood_tmax[.x-1],
                                             df_16396_final$ewood_tmax[.x-2],
                                             df_16396_final$ewood_tmax[.x-3],
                                             df_16396_final$ewood_tmax[.x-4]),
                                           na.rm = TRUE)}
                    mean(c(df_16396_final$ewood_tmax[.x-1],
                           df_16396_final$ewood_tmax[.x-2],
                           df_16396_final$ewood_tmax[.x-3],
                           df_16396_final$ewood_tmax[.x-4],
                           df_16396_final$ewood_tmax[.x-5]),
                         na.rm = TRUE)})) %>%
  unnest(c(wetness, et)) -> df_16396_final

df_16396_final %>%
  mutate(ewood_precip = log1p(ewood_precip),
         lagPrecip = log1p(lagPrecip),
         wetness = log1p(lagPrecip),
         ewood_tmax = ewood_tmax^2,
         et = et^2) %>%
  left_join(usgs_08065800, by = c("date" = "datetime")) %>%
  left_join(usgs_08109800,  by = c("date" = "datetime")) %>%
  left_join(usgs_08110100,  by = c("date" = "datetime")) %>%
  arrange(date) %>%
  mutate(lag_Flow_08065800 = lag(Flow_08065800),
         lag_Flow_08109800 = lag(Flow_08109800),
         lag_Flow_08110100 = lag(Flow_08110100),
         log_Q = log1p(adjusted_flow))-> df_16396_final

m1.lm.results <- predict(m1.lm, newdata = df_16396_final)
m1.gam.results <- predict(m1.gam, 
                          newdata = df_16396_final,
                          type = "response")

df_16396_final %>%
  mutate(lm_fit = expm1(m1.lm.results),
         gam_fit = m1.gam.results) %>%
  ggplot() +
  geom_line(aes(date, adjusted_flow, color = "Measured", linetype = "Measured")) +
  geom_line(aes(date, lm_fit, color = "LR", linetype = "LR"), alpha = 0.5) +
  geom_line(aes(date, gam_fit, color = "GAM", linetype = "GAM"), alpha = 0.5) +
  scale_color_brewer("", palette = "Dark2") +
  scale_linetype("") +
  labs(x = "Date", y = "Mean Daily Streamflow [cfs]") +
  theme_ms() +
  theme(axis.line.x = element_blank(),
        axis.title.y.right = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.ticks.y.right = element_line(color = "black"),
        axis.ticks.length = grid::unit(5, "pt"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       color = "transparent")) -> p1

ggplot(df_16396_final) + 
  geom_line(aes(date, expm1(ewood_precip), color = "Total Daily Preciptitation")) +
  scale_y_reverse(position = "right", 
                  limits = c(50,0),
                  breaks = c(0,2,4,6,8),
                  labels = c(0,2,4,6,8),
                  expand = c(0,0)) +
  labs(y = "Total Daily Precipitation [in]") +
  scale_color_manual(values = c("dodgerblue4")) +
  theme_ms() +
  guides(x = guide_axis(angle = 90)) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y.right = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.ticks.y.right = element_line(color = "black"),
        axis.ticks.length = grid::unit(5, "pt"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       color = "transparent"),
        legend.position = "none"
  ) -> p2

set_null_device("agg")
aligned_plots <- align_plots(p1, p2, align="hv", axis="tblr")
set_null_device("agg")
hg_plot1 <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
agg_png(here::here("Figures/predicted_por_16396.png"),
        width = 6.5,
        height = 4,
        units = "in",
        res = 300)
hg_plot1
dev.off()



## 16397
final_prediction_df %>%
  dplyr::filter(Site == "SWQM-16397") %>%
  arrange(date) -> df_16397_final

df_16397_final %>%
  mutate(lagPrecip = lag(ewood_precip),
         wetness = map(row_number(.$date),
                       ~{if(.x - 1 <= 0) {df_16397_final$ewood_precip[.x]}
                         if(.x - 2 <= 0) {sum(df_16397_final$ewood_precip[.x],
                                              df_16397_final$ewood_precip[.x-1])}
                         if(.x - 2 > 0) {
                           sum(
                             df_16397_final$ewood_precip[.x],
                             df_16397_final$ewood_precip[.x-1],
                             df_16397_final$ewood_precip[.x-2],
                             na.rm = TRUE
                           ) 
                         }}),
         et = map(row_number(.$date),
                  ~{if(.x - 1 <= 0) {df_16397_final$ewood_tmax[.x]}
                    if(.x - 2 <= 0) {df_16397_final$ewood_tmax[.x-1]}
                    if(.x - 3 <= 0) { mean(c(df_16397_final$ewood_tmax[.x-1],
                                             df_16397_final$ewood_tmax[.x-2]),
                                           na.rm = TRUE)}
                    if(.x - 4 <= 0) { mean(c(df_16397_final$ewood_tmax[.x-1],
                                             df_16397_final$ewood_tmax[.x-2],
                                             df_16397_final$ewood_tmax[.x-3]),
                                           na.rm = TRUE)}
                    if(.x - 5 <= 0) { mean(c(df_16397_final$ewood_tmax[.x-1],
                                             df_16397_final$ewood_tmax[.x-2],
                                             df_16397_final$ewood_tmax[.x-3],
                                             df_16397_final$ewood_tmax[.x-4]),
                                           na.rm = TRUE)}
                    mean(c(df_16397_final$ewood_tmax[.x-1],
                           df_16397_final$ewood_tmax[.x-2],
                           df_16397_final$ewood_tmax[.x-3],
                           df_16397_final$ewood_tmax[.x-4],
                           df_16397_final$ewood_tmax[.x-5]),
                         na.rm = TRUE)})) %>%
  unnest(c(wetness, et)) -> df_16397_final

df_16397_final %>%
  mutate(ewood_precip = log1p(ewood_precip),
         lagPrecip = log1p(lagPrecip),
         wetness = log1p(lagPrecip),
         ewood_tmax = ewood_tmax^2,
         et = et^2) %>%
  left_join(usgs_08065800, by = c("date" = "datetime")) %>%
  left_join(usgs_08109800,  by = c("date" = "datetime")) %>%
  left_join(usgs_08110100,  by = c("date" = "datetime")) %>%
  arrange(date) %>%
  mutate(lag_Flow_08065800 = lag(Flow_08065800),
         lag_Flow_08109800 = lag(Flow_08109800),
         lag_Flow_08110100 = lag(Flow_08110100),
         log_Q = log1p(adjusted_flow))-> df_16397_final

m2.lm.results <- predict(m2.lm, newdata = df_16397_final)
m2.gam.results <- predict(m2.gam, 
                          newdata = df_16397_final,
                          type = "response")

df_16397_final %>%
  mutate(lm_fit = expm1(m2.lm.results),
         gam_fit = m2.gam.results) %>%
  ggplot() +
  geom_line(aes(date, adjusted_flow, color = "Measured", linetype = "Measured")) +
  geom_line(aes(date, lm_fit, color = "LR", linetype = "LR"), alpha = 0.5) +
  geom_line(aes(date, gam_fit, color = "GAM", linetype = "GAM"), alpha = 0.5) +
  scale_color_brewer("", palette = "Dark2") +
  scale_linetype("") +
  labs(x = "Date", y = "Mean Daily Streamflow [cfs]") +
  theme_ms() +
  theme(axis.line.x = element_blank(),
        axis.title.y.right = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.ticks.y.right = element_line(color = "black"),
        axis.ticks.length = grid::unit(5, "pt"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       color = "transparent")) -> p1

ggplot(df_16397_final) + 
  geom_line(aes(date, expm1(ewood_precip), color = "Total Daily Preciptitation")) +
  scale_y_reverse(position = "right", 
                  limits = c(50,0),
                  breaks = c(0,2,4,6,8),
                  labels = c(0,2,4,6,8),
                  expand = c(0,0)) +
  labs(y = "Total Daily Precipitation [in]") +
  scale_color_manual(values = c("dodgerblue4")) +
  theme_ms() +
  guides(x = guide_axis(angle = 90)) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y.right = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.ticks.y.right = element_line(color = "black"),
        axis.ticks.length = grid::unit(5, "pt"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       color = "transparent"),
        legend.position = "none"
  ) -> p2

set_null_device("agg")
aligned_plots <- align_plots(p1, p2, align="hv", axis="tblr")
set_null_device("agg")
hg_plot1 <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
agg_png(here::here("Figures/predicted_por_16397.png"),
        width = 6.5,
        height = 4,
        units = "in",
        res = 300)
hg_plot1
dev.off()


## 16882
final_prediction_df %>%
  dplyr::filter(Site == "SWQM-16882") %>%
  arrange(date) -> df_16882_final

df_16882_final %>%
  mutate(lagPrecip = lag(ewood_precip),
         wetness = map(row_number(.$date),
                       ~{if(.x - 1 <= 0) {df_16882_final$ewood_precip[.x]}
                         if(.x - 2 <= 0) {sum(df_16882_final$ewood_precip[.x],
                                              df_16882_final$ewood_precip[.x-1])}
                         if(.x - 2 > 0) {
                           sum(
                             df_16882_final$ewood_precip[.x],
                             df_16882_final$ewood_precip[.x-1],
                             df_16882_final$ewood_precip[.x-2],
                             na.rm = TRUE
                           ) 
                         }}),
         et = map(row_number(.$date),
                  ~{if(.x - 1 <= 0) {df_16882_final$ewood_tmax[.x]}
                    if(.x - 2 <= 0) {df_16882_final$ewood_tmax[.x-1]}
                    if(.x - 3 <= 0) { mean(c(df_16882_final$ewood_tmax[.x-1],
                                             df_16882_final$ewood_tmax[.x-2]),
                                           na.rm = TRUE)}
                    if(.x - 4 <= 0) { mean(c(df_16882_final$ewood_tmax[.x-1],
                                             df_16882_final$ewood_tmax[.x-2],
                                             df_16882_final$ewood_tmax[.x-3]),
                                           na.rm = TRUE)}
                    if(.x - 5 <= 0) { mean(c(df_16882_final$ewood_tmax[.x-1],
                                             df_16882_final$ewood_tmax[.x-2],
                                             df_16882_final$ewood_tmax[.x-3],
                                             df_16882_final$ewood_tmax[.x-4]),
                                           na.rm = TRUE)}
                    mean(c(df_16882_final$ewood_tmax[.x-1],
                           df_16882_final$ewood_tmax[.x-2],
                           df_16882_final$ewood_tmax[.x-3],
                           df_16882_final$ewood_tmax[.x-4],
                           df_16882_final$ewood_tmax[.x-5]),
                         na.rm = TRUE)})) %>%
  unnest(c(wetness, et))  %>%
  mutate(non_zero = case_when(
    adjusted_flow == 0 ~ 0,
    adjusted_flow > 0 ~ 1
  ))-> df_16882_final

df_16882_final %>%
  mutate(ewood_precip = log1p(ewood_precip),
         lagPrecip = log1p(lagPrecip),
         wetness = log1p(lagPrecip),
         ewood_tmax = ewood_tmax^2,
         et = et^2) %>%
  left_join(usgs_08065800, by = c("date" = "datetime")) %>%
  left_join(usgs_08109800,  by = c("date" = "datetime")) %>%
  left_join(usgs_08110100,  by = c("date" = "datetime")) %>%
  arrange(date) %>%
  mutate(lag_Flow_08065800 = lag(Flow_08065800),
         lag_Flow_08109800 = lag(Flow_08109800),
         lag_Flow_08110100 = lag(Flow_08110100),
         log_Q = log1p(adjusted_flow))-> df_16882_final

m3.lm.results <- predict(m3.lm, newdata = df_16882_final)



df_16882_final %>%
  bind_cols(
    as_tibble(predict(m3.gam, df_16882_final, se.fit = FALSE,
                      type = "response"))) %>%
  mutate(pred_zero = case_when(
    value < 0.5 ~ 0,
    value >= 0.5 ~ 1)) %>%
  bind_cols(
    as_tibble(predict(m4.gam, df_16882_final, se.fit = FALSE,
                      type = "response")) %>%
      dplyr::rename(response = value)) %>%
  mutate(response = case_when(
    pred_zero == 0 ~ 0,
    pred_zero != 0 ~ response)) %>%
  mutate(response = case_when(
    response < 0 ~ 0,
    response >= 0 ~ response)) -> df_16882_final




df_16882_final %>%
  mutate(lm_fit = expm1(m3.lm.results)) %>%
  dplyr::rename(gam_fit = response) %>%
  ggplot() +
  geom_line(aes(date, adjusted_flow, color = "Measured", linetype = "Measured")) +
  geom_line(aes(date, lm_fit, color = "LR", linetype = "LR"), alpha = 0.5) +
  geom_line(aes(date, gam_fit, color = "GAM", linetype = "GAM"), alpha = 0.5) +
  scale_color_brewer("", palette = "Dark2") +
  scale_linetype("") +
  labs(x = "Date", y = "Mean Daily Streamflow [cfs]") +
  theme_ms() +
  theme(axis.line.x = element_blank(),
        axis.title.y.right = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.ticks.y.right = element_line(color = "black"),
        axis.ticks.length = grid::unit(5, "pt"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       color = "transparent")) -> p1

ggplot(df_16882_final) + 
  geom_line(aes(date, expm1(ewood_precip), color = "Total Daily Preciptitation")) +
  scale_y_reverse(position = "right", 
                  limits = c(50,0),
                  breaks = c(0,2,4,6,8),
                  labels = c(0,2,4,6,8),
                  expand = c(0,0)) +
  labs(y = "Total Daily Precipitation [in]") +
  scale_color_manual(values = c("dodgerblue4")) +
  theme_ms() +
  guides(x = guide_axis(angle = 90)) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y.right = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.ticks.y.right = element_line(color = "black"),
        axis.ticks.length = grid::unit(5, "pt"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       color = "transparent"),
        legend.position = "none"
  ) -> p2

set_null_device("agg")
aligned_plots <- align_plots(p1, p2, align="hv", axis="tblr")
set_null_device("agg")
hg_plot1 <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
agg_png(here::here("Figures/predicted_por_16882.png"),
        width = 6.5,
        height = 4,
        units = "in",
        res = 300)
hg_plot1
dev.off()

# Final FDCs #########


df_16396_final %>%
  arrange(desc(adjusted_flow)) %>%
  mutate(adjusted_flow_exceedance = 1 - cume_dist(adjusted_flow)) %>%
  mutate(lm_fit = expm1(m1.lm.results),
         gam_fit = m1.gam.results) %>%
  arrange(desc(lm_fit)) %>%
  mutate(lm_flow_exceedance = 1 - cume_dist(lm_fit)) %>%
  arrange(desc(gam_fit)) %>%
  mutate(gam_flow_exceedance = 1 - cume_dist(gam_fit)) -> df_16396_final

quantile(df_16396_final$gam_fit, na.rm = TRUE, probs = 0.95, type = 5)
quantile(df_16396_final$lm_fit, na.rm = TRUE, probs = 0.95, type = 5)

agg_png(here::here("Figures/predicted_fdc_16396.png"),
        width = 6.5,
        height = 4,
        units = "in",
        res = 300)
ggplot(df_16396_final) +
  geom_line(aes(adjusted_flow_exceedance, adjusted_flow, color = "Measured FDC")) +
  geom_line(aes(gam_flow_exceedance, gam_fit, color = "GAM FDC")) +
  geom_line(aes(lm_flow_exceedance, lm_fit, color = "LR FDC")) +
  geom_point(aes(x = 0.05, y = quantile(gam_fit, na.rm = TRUE, probs = 0.95, type = 5), color = "GAM FDC")) +
  geom_point(aes(x = 0.05, y = quantile(lm_fit, na.rm = TRUE, probs = 0.95, type = 5), color = "LR FDC")) +
  annotate("text", x = 0.25, y = quantile(df_16396_final$gam_fit, na.rm = TRUE, probs = 0.95, type = 5)*1.75,
                label = paste0("GAM 5% Exceedance=", round(quantile(df_16396_final$gam_fit, na.rm = TRUE, probs = 0.95, type = 5), 2)),
           hjust = 0) +
  annotate(
    geom = "curve", 
    xend = 0.05, 
    y = quantile(df_16396_final$gam_fit, na.rm = TRUE, probs = 0.95, type = 5)*1.75, 
    x = 0.25, 
    yend = quantile(df_16396_final$gam_fit, na.rm = TRUE, probs = 0.95, type = 5), 
    curvature = .2, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate("text", x = 0.25, y = quantile(df_16396_final$lm_fit, na.rm = TRUE, probs = 0.95, type = 5)*1.75,
           label = paste0("LR 5% Exceedance=", round(quantile(df_16396_final$lm_fit, na.rm = TRUE, probs = 0.95, type = 5), 2)),
           hjust = 0) +
  annotate(
    geom = "curve", 
    xend = 0.05, 
    y = quantile(df_16396_final$lm_fit, na.rm = TRUE, probs = 0.95, type = 5)*1.75, 
    x = 0.25, 
    yend = quantile(df_16396_final$lm_fit, na.rm = TRUE, probs = 0.95, type = 5), 
    curvature = .2, arrow = arrow(length = unit(2, "mm"))
  ) +
  scale_color_brewer("", palette = "Dark2") +
  scale_y_log10() +
  labs(x="Proportion of Days Flow Exceeded",
       y="Mean Daily Flow (cfs)") +
  theme_ms() +
  theme(legend.title = element_blank())
  
dev.off()



df_16397_final %>%
  arrange(desc(adjusted_flow)) %>%
  mutate(adjusted_flow_exceedance = 1 - cume_dist(adjusted_flow)) %>%
  mutate(lm_fit = expm1(m2.lm.results),
         gam_fit = m2.gam.results) %>%
  arrange(desc(lm_fit)) %>%
  mutate(lm_flow_exceedance = 1 - cume_dist(lm_fit)) %>%
  arrange(desc(gam_fit)) %>%
  mutate(gam_flow_exceedance = 1 - cume_dist(gam_fit)) -> df_16397_final

quantile(df_16397_final$gam_fit, na.rm = TRUE, probs = 0.95, type = 5)
quantile(df_16397_final$lm_fit, na.rm = TRUE, probs = 0.95, type = 5)

agg_png(here::here("Figures/predicted_fdc_16397.png"),
        width = 6.5,
        height = 4,
        units = "in",
        res = 300)

ggplot(df_16397_final) +
  geom_line(aes(adjusted_flow_exceedance, adjusted_flow, color = "Measured FDC")) +
  geom_line(aes(gam_flow_exceedance, gam_fit, color = "GAM FDC")) +
  geom_line(aes(lm_flow_exceedance, lm_fit, color = "LR FDC")) +
  geom_point(aes(x = 0.05, y = quantile(gam_fit, na.rm = TRUE, probs = 0.95, type = 5), color = "GAM FDC")) +
  geom_point(aes(x = 0.05, y = quantile(lm_fit, na.rm = TRUE, probs = 0.95, type = 5), color = "LR FDC")) +
  annotate("text", x = 0.25, y = quantile(df_16397_final$gam_fit, na.rm = TRUE, probs = 0.95, type = 5)*2,
           label = paste0("GAM 5% Exceedance=", round(quantile(df_16397_final$gam_fit, na.rm = TRUE, probs = 0.95, type = 5), 2)),
           hjust = 0) +
  annotate(
    geom = "curve", 
    xend = 0.05, 
    y = quantile(df_16397_final$gam_fit, na.rm = TRUE, probs = 0.95, type = 5)*2, 
    x = 0.25, 
    yend = quantile(df_16397_final$gam_fit, na.rm = TRUE, probs = 0.95, type = 5), 
    curvature = .2, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate("text", x = 0.25, y = quantile(df_16397_final$lm_fit, na.rm = TRUE, probs = 0.95, type = 5)*1.5,
           label = paste0("LR 5% Exceedance=", round(quantile(df_16397_final$lm_fit, na.rm = TRUE, probs = 0.95, type = 5), 2)),
           hjust = 0) +
  annotate(
    geom = "curve", 
    xend = 0.05, 
    y = quantile(df_16397_final$lm_fit, na.rm = TRUE, probs = 0.95, type = 5)*1.5, 
    x = 0.25, 
    yend = quantile(df_16397_final$lm_fit, na.rm = TRUE, probs = 0.95, type = 5), 
    curvature = .2, arrow = arrow(length = unit(2, "mm"))
  ) +
  scale_color_brewer("", palette = "Dark2") +
  scale_y_log10() +
  labs(x="Proportion of Days Flow Exceeded",
       y="Mean Daily Flow (cfs)") +
  theme_ms() +
  theme(legend.title = element_blank())

dev.off()


df_16882_final %>%
  mutate(lm_fit = expm1(m3.lm.results)) %>%
  dplyr::rename(gam_fit = response) %>%
  arrange(desc(adjusted_flow)) %>%
  mutate(adjusted_flow_exceedance = 1 - cume_dist(adjusted_flow)) %>%
  arrange(desc(lm_fit)) %>%
  mutate(lm_flow_exceedance = 1 - cume_dist(lm_fit)) %>%
  arrange(desc(gam_fit)) %>%
  mutate(gam_flow_exceedance = 1 - cume_dist(gam_fit)) -> df_16882_final 

agg_png(here::here("Figures/predicted_fdc_16882.png"),
        width = 6.5,
        height = 4,
        units = "in",
        res = 300)
ggplot(df_16882_final) +
  geom_line(aes(adjusted_flow_exceedance, adjusted_flow, color = "Measured FDC")) +
  geom_line(aes(gam_flow_exceedance, gam_fit, color = "GAM FDC")) +
  geom_line(aes(lm_flow_exceedance, lm_fit, color = "LR FDC")) +
  geom_point(aes(x = 0.05, y = quantile(gam_fit, na.rm = TRUE, probs = 0.95, type = 5), color = "GAM FDC")) +
  geom_point(aes(x = 0.05, y = quantile(lm_fit, na.rm = TRUE, probs = 0.95, type = 5), color = "LR FDC")) +
  annotate("text", x = 0.25, y = quantile(df_16882_final$gam_fit, na.rm = TRUE, probs = 0.95, type = 5)*2,
           label = paste0("GAM 5% Exceedance=", round(quantile(df_16882_final$gam_fit, na.rm = TRUE, probs = 0.95, type = 5), 2)),
           hjust = 0) +
  annotate(
    geom = "curve", 
    xend = 0.05, 
    y = quantile(df_16882_final$gam_fit, na.rm = TRUE, probs = 0.95, type = 5)*2, 
    x = 0.25, 
    yend = quantile(df_16882_final$gam_fit, na.rm = TRUE, probs = 0.95, type = 5), 
    curvature = .2, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate("text", x = 0.25, y = quantile(df_16882_final$lm_fit, na.rm = TRUE, probs = 0.95, type = 5)*1.5,
           label = paste0("LR 5% Exceedance=", round(quantile(df_16882_final$lm_fit, na.rm = TRUE, probs = 0.95, type = 5), 2)),
           hjust = 0) +
  annotate(
    geom = "curve", 
    xend = 0.05, 
    y = quantile(df_16882_final$lm_fit, na.rm = TRUE, probs = 0.95, type = 5)*1.5, 
    x = 0.25, 
    yend = quantile(df_16882_final$lm_fit, na.rm = TRUE, probs = 0.95, type = 5), 
    curvature = .2, arrow = arrow(length = unit(2, "mm"))
  ) +
  scale_color_brewer("", palette = "Dark2") +
  scale_y_log10() +
  labs(x="Proportion of Days Flow Exceeded",
       y="Mean Daily Flow (cfs)") +
  theme_ms() +
  theme(legend.title = element_blank())
dev.off()