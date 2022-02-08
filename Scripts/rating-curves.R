# Load libs ####
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
## nls.multstart fits non-linear least squares using the Levenberg-Marquardt algorithm with multiple starting values.
library(nls.multstart)
##
library(mgcv)
## save fancy figures
library(ragg)
## export excel
library(writexl)


# Functions #####
theme_ms <- function(...) {
  theme_ipsum_rc(plot_margin = margin(10,10,10,10),
                 axis_title_just = "c") +
    theme(axis.ticks.x = element_line(size = 0.5),
          axis.ticks.y = element_line(size = 0.5),
          legend.position = "bottom",
          panel.background = element_rect(fill = "white", 
                                          colour = NA), 
          panel.border = element_rect(fill = NA, 
                                      colour = "grey20"),
          strip.text = element_text(size = 9),
          ...)
}

exponent <- function(x, pow) {
  (abs(x)^pow)*sign(x)
}


## I need a pipe friendly function that assigns an attribute to 
## a tibble (tsibble) and returns a tibble (tsibble)
pipe_attr <- function(df) {
  attr(df, "interval") <- new_interval(minute = 1)
  return(df)
}
#### end custom functions


# Import Data ####


## Hobo Data ####

## make a list of files to import
file_paths <- paste0(here("Raw-Data/Hobo"),
                     "/",
                     list.files(path = here("Raw-Data/Hobo"),
                                pattern = ".csv"))

## create a blank tibble to fill
hobo_df <- tibble()


## loop through file paths to read each file
for (i in file_paths) {
  x <- read_csv(
    i,
    skip = 2,
    col_names = c(
      "Row",
      "Date",
      "Time",
      "Abs_Pres",
      "Temp",
      "Bar_Pressure",
      "Water_Level",
      "Coupler_Detached",
      "Coupler_Attached",
      "Stopped",
      "EOF"
    ),
    col_types = "nccnnnncccc"
  )
  x$file <- i
  hobo_df <- bind_rows(hobo_df, x)
  rm(x)
}

## clean up the dataframe
hobo_df <- hobo_df %>%
  mutate(
    ## regex extracts site number from file path
    Site = str_extract(file, "\\d{1,6}"),
    
    ## convert date and time columns to date/time format
    dt = paste(Date, Time),
    Date_Time = as.POSIXct(paste(Date, Time),
                           tz = "Etc/GMT-6",
                           format = "%m/%d/%y %I:%M:%S %p")) %>%
  mutate(Site = as.factor(Site)) %>%
  ## select the columns we need to keep
  dplyr::select(Abs_Pres, Temp, Water_Level, Site, Date_Time) %>%
  ## filter rows without water_level
  dplyr::filter(!is.na(Water_Level)) %>%
  ## filter records that are from when unit was placed or removed from water
  filter(
    Site == "16882" & Date_Time >= as.POSIXct("2020-03-03 10:19:16", tz = "Etc/GMT-6") &
      Date_Time != as.POSIXct("2020-07-30 10:19:16", tz = "Etc/GMT-6") & 
      Date_Time <= as.POSIXct("2021-03-31 07:43:57", tz = "Etc/GMT-6") |
      Site == "16397" & Date_Time >= as.POSIXct("2020-03-03 09:43:57",tz = "Etc/GMT-6")  &
      Date_Time != as.POSIXct("2021-01-14 09:13:57", tz = "Etc/GMT-6")  & 
      Date_Time <= as.POSIXct("2021-03-31 07:43:57", tz = "Etc/GMT-6") &
      Date_Time != as.POSIXct("2020-08-06 10:58:57", tz = "Etc/GMT-6") |
      Site == "16396" & Date_Time >= as.POSIXct("2020-03-03 09:02:19", tz = "Etc/GMT-6") &
      Date_Time != as.POSIXct("2021-01-14 08:17:19", tz = "Etc/GMT-6") &
      Date_Time <= as.POSIXct("2021-03-31 07:00:00", tz = "Etc/GMT-6")
  ) %>%
  ## subtract 0.625 feet from SWQM16397 on records between 2020-11-10 10:58:57 Etc/GMT-6
  ## through 2021-01-14 09:13:57 Etc/GMT-6
  mutate(Water_Level =
           case_when(
             Site == "16397" & 
               Date_Time >= as.POSIXct("2020-11-10 10:58:57", tz = "Etc/GMT-6") &  
               Date_Time <= as.POSIXct("2021-01-14 09:13:57", tz = "Etc/GMT-6") ~ Water_Level - 0.625,
             Site == "16397" & 
               Date_Time < as.POSIXct("2020-11-10 10:58:57", tz = "Etc/GMT-6") ~ Water_Level,
             Site == "16397" & 
               Date_Time > as.POSIXct("2021-01-14 09:13:57", tz = "Etc/GMT-6") ~ Water_Level,
             Site == "16882" | Site == "16396" ~ Water_Level
           ))

## attach units to our columns
units(hobo_df$Water_Level) <- as_units("ft")
units(hobo_df$Temp) <- as_units("°F")
units(hobo_df$Abs_Pres) <- as_units("psi")

## relabel swqm sites for plotting
hobo_df %>%
  mutate(Site = forcats::fct_relabel(Site, ~ paste0("SWQM-", .x))) -> hobo_df

## report summary stats
hobo_df %>% 
  select(Site, Water_Level) %>%
  mutate(Water_Level = as.numeric(Water_Level)) %>%
  tbl_summary(by = Site,
              label = list(Water_Level = "Stream Height"),
              digits = list(Water_Level = 2),
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label = "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Sites**") %>%
  modify_caption("Summary statistics of 15 minute stream levels measured at each site.") %>%
  as_kable()

## plot heights
agg_png(here::here("Figures/01-hobo.png"),
        width = 6,
        height = 6,
        units = "in",
        res = 300)

hobo_df %>%
  group_by(Site) %>%
  nest() %>%
  mutate(data = purrr::map(data, ~as_tsibble(.x))) %>%
  mutate(data = purrr::map(data, ~fill_gaps(.x))) %>%
  mutate(data = purrr::map(data, ~as_tibble(.x))) %>%
  unnest(data) %>%
  ggplot() +
  geom_line(aes(Date_Time, Water_Level)) +
  facet_wrap(~Site, ncol = 1, scales = "free_y") +
  scale_x_datetime("Date [yyyy-mm-dd]", date_breaks = "1 month",
                   date_labels = "%F") +
  scale_y_unit("Depth") +
  guides(x = guide_axis(angle = 45)) +
  theme_ms() +
  theme(axis.text.x = element_text(size = 8))

dev.off()

## End Hobo Data Import

## IQPlus Data ####

## make a list of files to import
file_paths <- paste0(here("Raw-Data/IQPlus"),
                     "/",
                     list.files(path = here("Raw-Data/IQPlus"),
                                pattern = ".csv"))

## create a blank tibble to fill
iqplus_df <- tibble()

## loop through file paths to read each file
for (i in file_paths) {
  x <- read_csv(
    i,
    col_types = "nc______n__n______________________nn______n_________"
  )
  x$file <- i
  iqplus_df <- bind_rows(iqplus_df, x)
  rm(x)
}


iqplus_df <- iqplus_df %>%
  mutate(
    ## regex extracts site number from file path
    Site = str_extract(file, "\\d{1,6}")) %>%
  ## use `dplyr::` to specify which rename function to use, just in case
  dplyr::rename(Sample_Number =`Sample number`,
                Date_Time = `Sample time`,
                Depth = `Depth (ft)`,
                Flow = `Flow (ft³/s)`,
                System_In_Water = `System in water (%)`,
                System_Status = `System status (status codes)`,
                Index_Velocity = `Velocity (mean) (ft/s)`) %>%
  dplyr::select(-c(Sample_Number, file)) %>%
  mutate(Date_Time = as.POSIXct(Date_Time,
                                tz = "Etc/GMT-6",
                                format = "%Y-%m-%d %H:%M:%S"))


## attach units to our columns
units(iqplus_df$Depth) <- as_units("ft")
units(iqplus_df$Flow) <- as_units("ft^3/s")
units(iqplus_df$Index_Velocity) <- as_units("ft/s")

## some data cleaning
iqplus_df %>%
  filter(System_Status == 0,
         System_In_Water == 100,
         #as.numeric(Depth) >= 0.26, ## minimum operating depth
         as.numeric(Flow) > 0) %>%
  filter(Site == "16396" &
           Date_Time >= as.POSIXct("2020-05-03", tz = "Etc/GMT-6") &
           Date_Time <= as.POSIXct("2020-05-31", tz = "Etc/GMT-6") &
           as.numeric(Depth) >= 0.875 |
           Site == "16396" &
           Date_Time >= as.POSIXct("2020-12-01", tz = "Etc/GMT-6") &
           Date_Time <= as.POSIXct("2021-01-31", tz = "Etc/GMT-6") &
           as.numeric(Depth) >= 0.875 |
           Site == "16396" &
           Date_Time >= as.POSIXct("2021-12-31", tz = "Etc/GMT-6") &
           as.numeric(Depth) >= 0.875 |
           Site == "16397" &
           Date_Time >= as.POSIXct("2020-12-15", tz = "Etc/GMT-6") &
           Date_Time <= as.POSIXct("2020-12-31", tz = "Etc/GMT-6") &
           as.numeric(Depth) >= 1.25 |
           Site == "16397" &
           Date_Time >= as.POSIXct("2021-01-05", tz = "Etc/GMT-6") &
           as.numeric(Depth) >= 1.25 |
           # as.numeric(Depth) > 2 |
           Site == "16882" &## possible sedimentation at low flows, removing low flow measurements.
           Date_Time >= as.POSIXct("2020-05-01", tz = "Etc/GMT-6") &
           Date_Time < as.POSIXct("2020-06-01", tz = "Etc/GMT-6") &
           as.numeric(Depth) >= 0.875 |
           Site == "16882" & ## possible sedimentation at low flows, removing low flow measurements.
           Date_Time >= as.POSIXct("2020-10-11", tz = "Etc/GMT-6") &
           Date_Time < as.POSIXct("2020-10-31", tz = "Etc/GMT-6") & 
           as.numeric(Depth) >= 0.875 |
           Site == "16882" &
           Date_Time >= as.POSIXct("2020-12-10", tz = "Etc/GMT-6") &
           Date_Time < as.POSIXct("2020-12-31", tz = "Etc/GMT-6") &
           as.numeric(Depth) >= 1.25 |
           Site == "16882" &
           Date_Time >= as.POSIXct("2021-12-31", tz = "Etc/GMT-6") &
           as.numeric(Depth) >= 1.25) -> iqplus_df

## relabel swqm sites for plotting
iqplus_df %>%
  mutate(Site = forcats::fct_relabel(Site, ~ paste0("SWQM-", .x))) -> iqplus_df



## plot iqdata
agg_png(here::here("Figures/02-iqplus.png"),
        width = 6,
        height = 6,
        units = "in",
        res = 300)
ggplot(iqplus_df) +
  geom_point(aes(Date_Time, Flow), alpha = 0.2) +
  facet_wrap(~Site, ncol = 1, scales = "free_y") +
  scale_x_datetime("Date [yyyy-mm-dd]", date_breaks = "1 month",
                   date_labels = "%F") +
  labs(x = "Date") +
  guides(x = guide_axis(angle = 45)) +
  theme_ms() +
  theme(axis.text.x = element_text(size = 8))
dev.off()

## End IQPlus data import

# Preprocess Data ####

## Join ####
## use purrr::map to run interpolation on each site
hobo_df %>%
  split(.$Site) %>%
  map(~dplyr::mutate(.x, Date_Time = round_date(.x$Date_Time, unit = "minute"))) %>%
  map(~as_tsibble(.x, key = Site, index = Date_Time)) %>%
  map(~pipe_attr(.x)) %>%
  map(~fill_gaps(.x)) %>%
  map(~mutate(.x, Water_Level = na_interpolation(as.numeric(Water_Level), option = "linear"))) %>%
  bind_rows() %>%
  as_tibble() %>%
  select(Water_Level, Site, Date_Time) -> hobo_df_interpolated

## replace Depth in iqplus_df with Water_Level reading from Hobo
## this is the dataframe we will develop rating curves from.
iqplus_df %>%
  left_join(hobo_df_interpolated, by = c("Site" = "Site", "Date_Time" = "Date_Time")) %>%
  select(Date_Time, Flow, System_In_Water, System_Status, Site, Water_Level) %>%
  rename(Depth = Water_Level) %>%
  mutate(Depth = set_units(Depth, "ft")) -> iqplus_df
# Fit Rating Curves ####

## Site 16396 ####

## Make dataframe for site 16396 before may 27 2020
iqplus_df %>%
  filter(Site == "SWQM-16396",
         Date_Time < as.POSIXct("2020-05-18", tz = "Etc/GMT-6")) %>%
  arrange(Date_Time) %>%
  mutate(time_lag = lag(Date_Time, default = Date_Time[1]),
         diff_time = as.numeric(difftime(Date_Time, time_lag, units = "hours"))) %>%
  group_split(cumsum(diff_time > 8)) %>%
  ## remove events where max flow did not go over 10 cfs
  keep(~ max(as.numeric(.x$Flow)) > 10) %>%
  map(~select(.x, Date_Time, Depth, Flow)) %>%
  map(~mutate(.x,
              time_lag = lag(Date_Time, default = Date_Time[1]),
              time_lead = lead(Date_Time),
              diff_time = as.numeric(difftime(time_lead,time_lag, units = "hours")),
              diff_depth = lead(Depth) - lag(Depth))) %>%
  imap(~mutate(.x, event = as.character(.y))) %>%
  bind_rows() %>%
  filter(!is.na(diff_depth)) %>%
  mutate(J = as.numeric(diff_depth)/as.numeric(diff_time)) -> df_16396_2020_03

## Make dataframe for site 16396 between may 27 and dec 14
iqplus_df %>%
  filter(Site == "SWQM-16396",
         Date_Time >= as.POSIXct("2020-05-18", tz = "Etc/GMT-6") &
           Date_Time < as.POSIXct("2020-12-14", tz = "Etc/GMT-6")) %>%
  arrange(Date_Time) %>%
  mutate(time_lag = lag(Date_Time, default = Date_Time[1]),
         diff_time = as.numeric(difftime(Date_Time, time_lag, units = "hours"))) %>%
  group_split(cumsum(diff_time > 8)) %>%
  ## remove events where max flow did not go over 10 cfs
  keep(~ max(as.numeric(.x$Flow)) > 10) %>%
  map(~select(.x, Date_Time, Depth, Flow)) %>%
  map(~mutate(.x,
              time_lag = lag(Date_Time, default = Date_Time[1]),
              time_lead = lead(Date_Time),
              diff_time = as.numeric(difftime(time_lead,time_lag, units = "hours")),
              diff_depth = lead(Depth) - lag(Depth))) %>%
  imap(~mutate(.x, event = as.character(.y))) %>%
  bind_rows() %>%
  filter(!is.na(diff_depth)) %>%
  mutate(J = as.numeric(diff_depth)/as.numeric(diff_time)) -> df_16396_2020_05


## Make dataframe for site 16396 Dec 14 2020 through Jend of record
iqplus_df %>%
  filter(Site == "SWQM-16396",
         Date_Time >= as.POSIXct("2020-12-14", tz = "Etc/GMT-6")) %>%
  arrange(Date_Time) %>%
  mutate(time_lag = lag(Date_Time, default = Date_Time[1]),
         diff_time = as.numeric(difftime(Date_Time, time_lag, units = "hours"))) %>%
  group_split(cumsum(diff_time > 8)) %>%
  ## remove events where max flow did not go over 10 cfs
  keep(~ max(as.numeric(.x$Flow)) > 10) %>%
  map(~select(.x, Date_Time, Depth, Flow)) %>%
  map(~mutate(.x,
              time_lag = lag(Date_Time, default = Date_Time[1]),
              time_lead = lead(Date_Time),
              diff_time = as.numeric(difftime(time_lead,time_lag, units = "hours")),
              diff_depth = lead(Depth) - lag(Depth))) %>%
  imap(~mutate(.x, event = as.character(.y))) %>%
  bind_rows() %>%
  filter(!is.na(diff_depth)) %>%
  mutate(J = as.numeric(diff_depth)/as.numeric(diff_time)) -> df_16396_2020_12

## use nls to estimate parameters in Jones formula
jones_form <- formula(log(as.numeric(Flow)) ~ K*exponent(x = log(as.numeric(Depth)) - a, pow = n) * exponent(x = (1 + x * J), pow = (1/2)))

## some starting paremeters. nls_multstart will use multiple
## starting parameters and model selection to find 
## global minimum
start_lower <- list(K = 0, a = 0, n = 0, x = -5)
start_upper <- list(K = 10, a = 10, n = 5, x = 5)

## fit nls
rc_16396_2020_03 <- nls_multstart(jones_form,
                                  data = df_16396_2020_03,
                                  iter = 1000,
                                  start_lower = start_lower,
                                  start_upper = start_upper,
                                  convergence_count = FALSE,
                                  supp_errors = "Y")

## set parameter starting limits
start_lower <- list(K = -500, a = 0, n = 0, x = -5000)
start_upper <- list(K = 500, a = 1000, n = 50, x = 5000)

## fit nls
rc_16396_2020_05 <- nls_multstart(jones_form,
                                  data = df_16396_2020_05,
                                  iter = 1000,
                                  start_lower = start_lower,
                                  start_upper = start_upper,
                                  convergence_count = FALSE,
                                  supp_errors = "Y")

start_lower <- list(K = -100, a = 0, n = 0, x = -5000)
start_upper <- list(K = 100, a = 1000, n = 50, x = 5000)
## fit nls
rc_16396_2020_12 <- nls_multstart(jones_form,
                                  data = df_16396_2020_12,
                                  iter = 1000,
                                  start_lower = start_lower,
                                  start_upper = start_upper,
                                  convergence_count = FALSE,
                                  supp_errors = "Y")


## setup dataframe with parameter results. Will use this later to report parameters and GOF metrics
df_results_16369 <- tibble(Site = c("16396","16396","16396"),
                           Period = c("2020-03-03 : 2020-05-17",
                                      "2020-05-18 : 2020-12-13",
                                      "2020-12-14 : 2021-01-31"),
                           K = c(coefficients(rc_16396_2020_03)[["K"]],
                                 coefficients(rc_16396_2020_05)[["K"]],
                                 coefficients(rc_16396_2020_12)[["K"]]),
                           a = c(coefficients(rc_16396_2020_03)[["a"]],
                                 coefficients(rc_16396_2020_05)[["a"]],
                                 coefficients(rc_16396_2020_12)[["a"]]),
                           n = c(coefficients(rc_16396_2020_03)[["n"]],
                                 coefficients(rc_16396_2020_05)[["n"]],
                                 coefficients(rc_16396_2020_12)[["n"]]),
                           x = c(coefficients(rc_16396_2020_03)[["x"]],
                                 coefficients(rc_16396_2020_05)[["x"]],
                                 coefficients(rc_16396_2020_12)[["x"]]))

## Develop rating curve predictions and
## create table with GOF metrics
df_16396_2020_03 %>%
  filter(!is.na(J)) %>%
  mutate(predicted = exp(predict(rc_16396_2020_03, .))) -> df_16396_2020_03

df_16396_2020_05 %>%
  filter(!is.na(J)) %>%
  mutate(predicted = exp(predict(rc_16396_2020_05, .))) -> df_16396_2020_05

df_16396_2020_12 %>%
  filter(!is.na(J)) %>%
  mutate(predicted =  exp(predict(rc_16396_2020_12, .))) -> df_16396_2020_12


df_results_16369 %>%
  mutate(NSE = c(
    hydroGOF::NSE(df_16396_2020_03$predicted, as.numeric(df_16396_2020_03$Flow)),
    hydroGOF::NSE(df_16396_2020_05$predicted, as.numeric(df_16396_2020_05$Flow)),
    hydroGOF::NSE(df_16396_2020_12$predicted, as.numeric(df_16396_2020_12$Flow))),
    nRMSE = c(hydroGOF::nrmse(df_16396_2020_03$predicted, as.numeric(df_16396_2020_03$Flow), norm = "maxmin"),
              hydroGOF::nrmse(df_16396_2020_05$predicted, as.numeric(df_16396_2020_05$Flow), norm = "maxmin"),
              hydroGOF::nrmse(df_16396_2020_12$predicted, as.numeric(df_16396_2020_12$Flow), norm = "maxmin"))
  ) -> df_results_16369

##display table
knitr::kable(df_results_16369, 
      caption = "Rating curve parameter estimates and goodness-of-fit metrics for station 16369.") 

## plot rating curve results

p1 <- ggplot(df_16396_2020_03) +
  geom_point(aes(as.numeric(predicted), as.numeric(Flow), 
                 color = "Rating curve prediction against measured flow"), 
             alpha = 0.25,
             size = 0.5) +
  geom_abline(aes(slope = 1, intercept = 0, linetype = "1:1 line")) +
  scale_x_continuous(name = "Rating curve flow estimate [cfs]", trans = "log10") +
  scale_y_continuous(name = "Measured flow [cfs]", trans = "log10") +
  theme_ms() + labs(subtitle = "2020-03-03 through 2020-05-17") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.subtitle = element_text(size = 8))

p2 <- ggplot(df_16396_2020_03) +
  geom_point(aes(as.numeric(Flow), Depth, color = "Measured"), size = 0.5) +
  geom_path(aes(as.numeric(Flow), Depth, color = "Measured", linetype = "Measured"),
            alpha = 0.5) +
  geom_point(aes(as.numeric(predicted), Depth, color = "Rating Curve"), size = 0.5) +
  geom_path(aes(as.numeric(predicted), Depth, color = "Rating Curve", linetype = "Rating Curve"),
            alpha = 0.5) +
  scale_color_discrete("") + scale_linetype_discrete("") +
  scale_x_continuous("Flow [cfs]", trans = "log10") +
  labs(subtitle = "2020-03-03 through 2020-05-26") + theme_ms() + 
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 8))

p3 <- ggplot(df_16396_2020_05) +
  geom_point(aes(as.numeric(predicted), as.numeric(Flow), 
                 color = "Rating curve prediction against measured flow"), 
             alpha = 0.25,
             size = 0.5) +
  geom_abline(aes(slope = 1, intercept = 0, linetype = "1:1 line")) +
  scale_x_continuous(name = "Rating curve flow estimate [cfs]", trans = "log10") +
  scale_y_continuous(name = "Measured flow [cfs]", trans = "log10") +
  theme_ms() + labs(subtitle = "2020-05-18 through 2020-12-13") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.subtitle = element_text(size = 8))

p4 <- ggplot(df_16396_2020_05) +
  geom_point(aes(as.numeric(Flow), Depth, color = "Measured"), size = 0.5) +
  geom_path(aes(as.numeric(Flow), Depth, color = "Measured", linetype = "Measured"),
            alpha = 0.5) +
  geom_point(aes(as.numeric(predicted), Depth, color = "Rating Curve"), size = 0.5) +
  geom_path(aes(as.numeric(predicted), Depth, color = "Rating Curve", linetype = "Rating Curve"),
            alpha = 0.5) +
  scale_color_discrete("") + scale_linetype_discrete("") +
  scale_x_continuous("Flow [cfs]", trans = "log10") +
  labs(subtitle = "2020-05-27 through 2020-12-13") + theme_ms() + 
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 8))

p5 <- ggplot(df_16396_2020_12) +
  geom_point(aes(as.numeric(predicted), as.numeric(Flow), 
                 color = "Rating curve prediction against measured flow"), 
             alpha = 0.25,
             size = 0.5) +
  geom_abline(aes(slope = 1, intercept = 0, linetype = "1:1 line")) +
  scale_x_continuous(name = "Rating curve flow estimate [cfs]", trans = "log10") +
  scale_y_continuous(name = "Measured flow [cfs]", trans = "log10") +
  theme_ms() + labs(subtitle = "2020-12-14 through 2021-03-31") +
  theme(legend.title = element_blank(),
        plot.subtitle = element_text(size = 8))

p6 <- ggplot(df_16396_2020_12) +
  geom_point(aes(as.numeric(Flow), Depth, color = "Measured"), size = 0.5) +
  geom_path(aes(as.numeric(Flow), Depth, color = "Measured", linetype = "Measured"),
            alpha = 0.5) +
  geom_point(aes(as.numeric(predicted), Depth, color = "Rating Curve"), size = 0.5) +
  geom_path(aes(as.numeric(predicted), Depth, color = "Rating Curve", linetype = "Rating Curve"),
            alpha = 0.5) +
  scale_color_discrete("") + scale_linetype_discrete("") +
  scale_x_continuous("Flow [cfs]", trans = "log10") +
  labs(subtitle = "2020-12-14 through 2021-03-31") + theme_ms() +
  theme(plot.subtitle = element_text(size = 8))


## plot
agg_png(here::here("Figures/03-ratingcurve-16396.png"),
        width = 6.5,
        height = 8,
        units = "in",
        res = 300)
(p1 + p2) / (p3 + p4) / (p5 + p6) + plot_annotation(tag_levels = "A")
dev.off()

## Site 16397 ####

## setup dataframe to fit rating curve to 16397


## Make dataframe for site 16397 before Jan 20 2021 

iqplus_df %>%
  filter(Site == "SWQM-16397",
           Date_Time <= as.POSIXct("2021-01-20", tz = "Etc/GMT-6")) %>%
  arrange(Date_Time) %>%
  mutate(time_lag = lag(Date_Time, default = Date_Time[1]),
         diff_time = as.numeric(difftime(Date_Time, time_lag, units = "hours"))) %>%
  group_split(cumsum(diff_time > 8)) %>%
  map(~select(.x, Date_Time, Depth, Flow)) %>%
  map(~mutate(.x,
              time_lag = lag(Date_Time, default = Date_Time[1]),
              diff_time = as.numeric(difftime(Date_Time, time_lag, units = "hours")),
              diff_depth = c(0, diff(.x$Depth)))) %>%
  imap(~mutate(.x, event = as.character(.y))) %>%
  bind_rows() %>%
  filter(!is.na(diff_depth)) %>%
  mutate(J = as.numeric(diff_depth)/as.numeric(diff_time)) -> df_16397_2021_01

iqplus_df %>%
  filter(Site == "SWQM-16397",
         Date_Time > as.POSIXct("2021-01-20", tz = "Etc/GMT-6")) %>%
  arrange(Date_Time) %>%
  mutate(time_lag = lag(Date_Time, default = Date_Time[1]),
         diff_time = as.numeric(difftime(Date_Time, time_lag, units = "hours"))) %>%
  group_split(cumsum(diff_time > 8)) %>%
  map(~select(.x, Date_Time, Depth, Flow)) %>%
  map(~mutate(.x,
              time_lag = lag(Date_Time, default = Date_Time[1]),
              diff_time = as.numeric(difftime(Date_Time, time_lag, units = "hours")),
              diff_depth = c(0, diff(.x$Depth)))) %>%
  imap(~mutate(.x, event = as.character(.y))) %>%
  bind_rows() %>%
  filter(!is.na(diff_depth)) %>%
  filter(as.numeric(Depth) >= 1) %>%
  mutate(J = as.numeric(diff_depth)/as.numeric(diff_time)) -> df_16397_2021_03

## power function
power_form <- formula(log(as.numeric(Flow)) ~ K*(log(as.numeric(Depth)) - log(H_0))^Z)

## parameter starting limits
start_lower <- list(K = -10, Z = -10, H_0 = 0.0001)
start_upper <- list(K = 10, Z = 10, H_0 = 8)
rc_16397_2021_01 <- nls_multstart(power_form,
                          data = df_16397_2021_01,
                          iter = 1000,
                          start_lower = start_lower,
                          start_upper = start_upper,
                          convergence_count = FALSE,
                          supp_errors = "Y")

power_form <- formula(log(as.numeric(Flow)) ~ K*(log(as.numeric(Depth)) - log(H_0))^Z)
start_lower <- list(K = -10, Z = -10, H_0 = 0)
start_upper <- list(K = 10, Z = 10, H_0 = 2)
rc_16397_2021_03 <- nls_multstart(power_form,
                                  data = df_16397_2021_03,
                                  iter = 1000,
                                  start_lower = start_lower,
                                  start_upper = start_upper,
                                  convergence_count = FALSE,
                                  supp_errors = "Y")

## setup dataframe with parameter results. Will use this later to report parameters and GOF metrics
df_results_16397 <- tibble(Site = c("16397", "16397"),
                           Period = c("2020-03-03 : 2021-01-20",
                                      "2021-01-21 : 2021-03-31"),
                           K = c(coefficients(rc_16397_2021_01)[["K"]],
                                 coefficients(rc_16397_2021_03)[["K"]]),
                           H_0 = c(coefficients(rc_16397_2021_01)[["H_0"]],
                                   coefficients(rc_16397_2021_03)[["H_0"]]),
                           Z = c(coefficients(rc_16397_2021_01)[["Z"]],
                                 coefficients(rc_16397_2021_03)[["Z"]]))
df_16397_2021_01 %>%
  mutate(predicted = exp(predict(rc_16397_2021_01, .))) -> df_16397_2021_01

df_16397_2021_03 %>%
  mutate(predicted = exp(predict(rc_16397_2021_03, .))) -> df_16397_2021_03


df_results_16397 %>%
  mutate(NSE = c(hydroGOF::NSE(df_16397_2021_01$predicted, as.numeric(df_16397_2021_01$Flow)),
                 hydroGOF::NSE(df_16397_2021_03$predicted, as.numeric(df_16397_2021_03$Flow))),
         nRMSE = c(hydroGOF::nrmse(df_16397_2021_01$predicted, as.numeric(df_16397_2021_01$Flow),
                                 norm="maxmin"),
                   hydroGOF::nrmse(df_16397_2021_03$predicted, as.numeric(df_16397_2021_03$Flow),
                                   norm="maxmin"))) -> df_results_16397

knitr::kable(df_results_16397,
      caption = "Rating curve parameter estimates and goodness-of-fit metrics for station 16397.")



## plot rating curve results
p1 <- df_16397_2021_01 %>%
  mutate(predicted = set_units(predicted, "ft^3/s")) %>%
  ggplot() +
  geom_point(aes(as.numeric(predicted), as.numeric(Flow), 
                 color = "Rating curve prediction against measured flow"), 
             alpha = 0.9,
             size = 0.5) +
  geom_abline(aes(slope = 1, intercept = 0, linetype = "1:1 line")) +
  scale_x_continuous(name = "Rating curve flow estimate [cfs]", trans = "log10") +
  scale_y_continuous(name = "Measured flow [cfs]", trans = "log10") +
  labs(subtitle = "2020-03-03 : 2020-01-20") +
  theme_ms() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.subtitle = element_text(size = 8)) 

p2 <- ggplot(df_16397_2021_01 %>% arrange(Depth)) +
  geom_point(aes(as.numeric(Flow), Depth, color = "Measured"),
             size = 0.5) +
  geom_point(aes(as.numeric(predicted), Depth, color = "Rating Curve"),
             size = 0.5) +
  geom_line(aes(as.numeric(predicted), Depth, color = "Rating Curve"),
            alpha = 0.5) +
  scale_color_discrete("") +
  scale_x_continuous("Flow [cfs]", trans = "log10") +
  labs(subtitle = "2020-03-03 : 2020-01-20") +
  theme_ms() +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 8))

p3 <- df_16397_2021_03 %>%
  mutate(predicted = set_units(predicted, "ft^3/s")) %>%
  ggplot() +
  geom_point(aes(as.numeric(predicted), as.numeric(Flow), 
                 color = "Rating curve prediction against measured flow"), 
             alpha = 0.9,
             size = 0.5) +
  geom_abline(aes(slope = 1, intercept = 0, linetype = "1:1 line")) +
  scale_x_continuous(name = "Rating curve flow estimate [cfs]", trans = "log10") +
  scale_y_continuous(name = "Measured flow [cfs]", trans = "log10") +
  labs(subtitle = "2021-01-21 : 2021-03-31") +
  theme_ms() +
  theme(legend.title = element_blank(),
        plot.subtitle = element_text(size = 8))

p4 <- ggplot(df_16397_2021_03 %>% arrange(Depth)) +
  geom_point(aes(as.numeric(Flow), Depth, color = "Measured"),
             size = 0.5) +
  geom_point(aes(as.numeric(predicted), Depth, color = "Rating Curve"),
             size = 0.5) +
  geom_line(aes(as.numeric(predicted), Depth, color = "Rating Curve"),
            alpha = 0.5) +
  scale_color_discrete("") +
  scale_x_continuous("Flow [cfs]", trans = "log10") + 
  labs(subtitle = "2021-01-21 : 2021-03-31") +
  theme_ms() + theme(plot.subtitle = element_text(size = 8))


agg_png(here::here("Figures/04-ratingcurve-16397.png"),
        width = 6.5,
        height = 5,
        units = "in",
        res = 300)
(p1 + p2) /(p3 + p4) + plot_annotation(tag_levels = "A")
dev.off()

## Site 16882 ####

## make 3 different dataframes to fit jones formula to.

iqplus_df %>%
  filter(Site == "SWQM-16882") %>%
  filter(Date_Time > as.POSIXct("2020-05-01", tz = "Etc/GMT-6") &
           Date_Time < as.POSIXct("2020-05-31", tz = "Etc/GMT-6")) %>%
  arrange(Date_Time) %>%
  mutate(time_lag = lag(Date_Time, default = Date_Time[1]),
         diff_time = as.numeric(difftime(Date_Time, time_lag, units = "hours"))) %>%
  #filter(as.numeric(Depth) >= 2.36) %>%
  group_split(cumsum(diff_time > 8)) %>%
  ## remove events where max flow did not go over 10 cfs
  #keep(~ max(as.numeric(.x$Flow)) > 10) %>%
  map(~select(.x, Date_Time, Depth, Flow)) %>%
  map(~mutate(.x,
              time_lag = lag(Date_Time, default = Date_Time[1]),
              time_lead = lead(Date_Time),
              diff_time = as.numeric(difftime(time_lead,time_lag, units = "hours")),
              diff_depth = lead(Depth) - lag(Depth))) %>%
  imap(~mutate(.x, event = as.character(.y))) %>%
  bind_rows() %>%
  filter(!is.na(diff_depth)) %>%
  mutate(J = as.numeric(diff_depth)/as.numeric(diff_time)) -> df_16882_2020_03

iqplus_df %>%
  filter(Site == "SWQM-16882") %>%
  filter(Date_Time > as.POSIXct("2020-10-01", tz = "Etc/GMT-6") &
           Date_Time < as.POSIXct("2020-10-31", tz = "Etc/GMT-6")) %>%
  arrange(Date_Time) %>%
  mutate(time_lag = lag(Date_Time, default = Date_Time[1]),
         diff_time = as.numeric(difftime(Date_Time, time_lag, units = "hours"))) %>%
  group_split(cumsum(diff_time > 8)) %>%
  ## remove events where max flow did not go over 10 cfs
  keep(~ max(as.numeric(.x$Flow)) > 10) %>%
  map(~select(.x, Date_Time, Depth, Flow)) %>%
  map(~mutate(.x,
              time_lag = lag(Date_Time, default = Date_Time[1]),
              time_lead = lead(Date_Time),
              diff_time = as.numeric(difftime(time_lead,time_lag, units = "hours")),
              diff_depth = lead(Depth) - lag(Depth))) %>%
  imap(~mutate(.x, event = as.character(.y))) %>%
  bind_rows() %>%
  filter(!is.na(diff_depth)) %>%
  mutate(J = as.numeric(diff_depth)/as.numeric(diff_time)) -> df_16882_2020_10


iqplus_df %>%
  filter(Site == "SWQM-16882") %>%
  filter(Date_Time >= as.POSIXct("2020-11-01", tz = "Etc/GMT-6") &
           Date_Time < as.POSIXct("2021-03-29", tz = "Etc/GMT-6")) %>%
  arrange(Date_Time) %>%
  mutate(time_lag = lag(Date_Time, default = Date_Time[1]),
         diff_time = as.numeric(difftime(Date_Time, time_lag, units = "hours"))) %>%
  group_split(cumsum(diff_time > 8)) %>%
  ## remove events where max flow did not go over 10 cfs
  keep(~ max(as.numeric(.x$Flow)) > 5) %>%
  map(~select(.x, Date_Time, Depth, Flow)) %>%
  map(~mutate(.x,
              time_lag = lag(Date_Time, default = Date_Time[1]),
              time_lead = lead(Date_Time),
              diff_time = as.numeric(difftime(time_lead,time_lag, units = "hours")),
              diff_depth = lead(Depth) - lag(Depth))) %>%
  imap(~mutate(.x, event = as.character(.y))) %>%
  bind_rows() %>%
  filter(!is.na(diff_depth)) %>%
  mutate(J = as.numeric(diff_depth)/as.numeric(diff_time)) -> df_16882_2020_12

## estimate parameters for each dataset

## set parameter starting limits
start_lower <- list(K = 0, a = 0, n = 0, x = -5000)
start_upper <- list(K = 100, a = 1000, n = 50, x = 5000)

rc_16882_2020_03 <- nls_multstart(jones_form,
                                  data = df_16882_2020_03,
                                  iter = 1000,
                                  start_lower = start_lower,
                                  start_upper = start_upper,
                                  convergence_count = FALSE,
                                  supp_errors = "Y")

## set parameter starting limits
start_lower <- list(K = -100, a = 0, n = 0, x = -5000)
start_upper <- list(K = 100, a = 1000, n = 50, x = 5000)
rc_16882_2020_10 <- nls_multstart(jones_form,
                                  data = df_16882_2020_10,
                                  iter = 1000,
                                  start_lower = start_lower,
                                  start_upper = start_upper,
                                  convergence_count = FALSE,
                                  supp_errors = "Y")

## set parameter starting limits
start_lower <- list(K = 0, a = 0, n = 0, x = -5000)
start_upper <- list(K = 100, a = 1000, n = 50, x = 5000)
rc_16882_2020_12 <- nls_multstart(jones_form,
                                  data = df_16882_2020_12,
                                  iter = 1000,
                                  start_lower = start_lower,
                                  start_upper = start_upper,
                                  convergence_count = FALSE,
                                  supp_errors = "Y")

## setup dataframe with parameter results. Will use this later to report parameters and GOF metrics
df_results_16882 <- tibble(Site = c("16882", "16882", "16882"),
                           Period = c("2020-03-03 : 2020-10-22",
                                      "2020-10-23 : 2020-12-09",
                                      "2020-12-10 : 2021-03-29"),
                           K = c(coefficients(rc_16882_2020_03)[["K"]],
                                 coefficients(rc_16882_2020_10)[["K"]],
                                 coefficients(rc_16882_2020_12)[["K"]]),
                           a = c(coefficients(rc_16882_2020_03)[["a"]],
                                 coefficients(rc_16882_2020_10)[["a"]],
                                 coefficients(rc_16882_2020_12)[["a"]]),
                           n = c(coefficients(rc_16882_2020_03)[["n"]],
                                 coefficients(rc_16882_2020_10)[["n"]],
                                 coefficients(rc_16882_2020_12)[["n"]]),
                           x = c(coefficients(rc_16882_2020_03)[["x"]],
                                 coefficients(rc_16882_2020_10)[["x"]],
                                 coefficients(rc_16882_2020_12)[["x"]]))
# df_results_16882 <- tibble(Site = c("16882", "16882"),
#                            Period = c("2020-03-03 : 2020-10-22",
#                                       "2020-10-23 : 2020-03-29"),
#                            K = c(coefficients(rc_16882_2020_03)[["K"]],
#                                  coefficients(rc_16882_2020_10)[["K"]]),
#                            a = c(coefficients(rc_16882_2020_03)[["a"]],
#                                  coefficients(rc_16882_2020_10)[["a"]]),
#                            n = c(coefficients(rc_16882_2020_03)[["n"]],
#                                  coefficients(rc_16882_2020_10)[["n"]]),
#                            x = c(coefficients(rc_16882_2020_03)[["x"]],
#                                  coefficients(rc_16882_2020_10)[["x"]]))

## Develop rating curve predictions and
## create table with GOF metrics


df_16882_2020_03 %>%
  filter(!is.na(J)) %>%
  mutate(predicted = exp(predict(rc_16882_2020_03, data = .))) -> df_16882_2020_03

df_16882_2020_10 %>%
  filter(!is.na(J)) %>%
  mutate(predicted = exp(predict(rc_16882_2020_10, data = .))) -> df_16882_2020_10

df_16882_2020_12 %>%
  filter(!is.na(J)) %>%
  mutate(predicted = exp(predict(rc_16882_2020_12, data = .))) -> df_16882_2020_12


df_results_16882 %>%
  mutate(NSE = c(
    hydroGOF::NSE(df_16882_2020_03$predicted, as.numeric(df_16882_2020_03$Flow)),
    hydroGOF::NSE(df_16882_2020_10$predicted, as.numeric(df_16882_2020_10$Flow)),
    hydroGOF::NSE(df_16882_2020_12$predicted, as.numeric(df_16882_2020_12$Flow))),
    nRMSE = c(hydroGOF::nrmse(df_16882_2020_03$predicted, as.numeric(df_16882_2020_03$Flow), norm = "maxmin"),
              hydroGOF::nrmse(df_16882_2020_10$predicted, as.numeric(df_16882_2020_10$Flow), norm = "maxmin"),
              hydroGOF::nrmse(df_16882_2020_12$predicted, as.numeric(df_16882_2020_12$Flow), norm = "maxmin"))
  ) -> df_results_16882
# df_results_16882 %>%
#   mutate(NSE = c(
#     hydroGOF::NSE(df_16882_2020_03$predicted, as.numeric(df_16882_2020_03$Flow)),
#     hydroGOF::NSE(df_16882_2020_10$predicted, as.numeric(df_16882_2020_10$Flow))),
#     nRMSE = c(hydroGOF::nrmse(df_16882_2020_03$predicted, as.numeric(df_16882_2020_03$Flow), norm = "maxmin"),
#               hydroGOF::nrmse(df_16882_2020_10$predicted, as.numeric(df_16882_2020_10$Flow), norm = "maxmin"))
#   ) -> df_results_16882

##display table
knitr::kable(df_results_16882,
      caption = "Rating curve parameter estimates and goodness-of-fit metrics for station 16882.")

## plot rating curve results

p1 <- ggplot(df_16882_2020_03) +
  geom_point(aes(as.numeric(predicted), as.numeric(Flow), 
                 color = "Rating curve prediction against measured flow"), 
             alpha = 0.25,
             size = 0.5) +
  geom_abline(aes(slope = 1, intercept = 0, linetype = "1:1 line")) +
  scale_x_continuous(name = "Rating curve flow estimate [cfs]", trans = "log10") +
  scale_y_continuous(name = "Measured flow [cfs]", trans = "log10") +
  theme_ms() + labs(subtitle = "2020-03-03 : 2020-10-22") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.subtitle = element_text(size = 8))

p2 <- ggplot(df_16882_2020_03) +
  geom_point(aes(as.numeric(Flow), Depth, color = "Measured"),
             size = 0.5) +
  geom_path(aes(as.numeric(Flow), Depth, color = "Measured", linetype = "Measured"),
            alpha = 0.5) +
  geom_point(aes(as.numeric(predicted), Depth, color = "Rating Curve"),
             size = 0.5) +
  geom_path(aes(as.numeric(predicted), Depth, color = "Rating Curve", linetype = "Rating Curve"),
            alpha = 0.5) +
  scale_color_discrete("") + scale_linetype_discrete("") +
  scale_x_continuous("Flow [cfs]", trans = "log10") +
  labs(subtitle = "2020-03-03 : 2020-10-22") + theme_ms() + theme(legend.position = "none",
                                                                  plot.subtitle = element_text(size = 8))

p3 <- ggplot(df_16882_2020_10) +
  geom_point(aes(as.numeric(predicted), as.numeric(Flow), 
                 color = "Rating curve prediction against measured flow"), 
             alpha = 0.25,
             size = 0.5) +
  geom_abline(aes(slope = 1, intercept = 0, linetype = "1:1 line")) +
  scale_x_continuous(name = "Rating curve flow estimate [cfs]", trans = "log10") +
  scale_y_continuous(name = "Measured flow [cfs]", trans = "log10") +
  theme_ms() + labs(subtitle = "2020-10-23 : 2020-12-09") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.subtitle = element_text(size = 8))

p4 <- ggplot(df_16882_2020_10) +
  geom_point(aes(as.numeric(Flow), Depth, color = "Measured"),
             size = 0.5) +
  geom_path(aes(as.numeric(Flow), Depth, color = "Measured", linetype = "Measured"),
            alpha = 0.5) +
  geom_point(aes(as.numeric(predicted), Depth, color = "Rating Curve"),
             size = 0.5) +
  geom_path(aes(as.numeric(predicted), Depth, color = "Rating Curve", linetype = "Rating Curve"),
            alpha = 0.5) +
  scale_color_discrete("") + scale_linetype_discrete("") +
  scale_x_continuous("Flow [cfs]", trans = "log10") +
  labs(subtitle = "2020-10-23 : 2020-12-09") + theme_ms() + theme(legend.position = "none",
                                                                  plot.subtitle = element_text(size = 8))

p5 <- ggplot(df_16882_2020_12) +
  geom_point(aes(as.numeric(predicted), as.numeric(Flow), 
                 color = "Rating curve prediction against measured flow"), 
             alpha = 0.25,
             size = 0.5) +
  geom_abline(aes(slope = 1, intercept = 0, linetype = "1:1 line")) +
  scale_x_continuous(name = "Rating curve flow estimate [cfs]", trans = "log10") +
  scale_y_continuous(name = "Measured flow [cfs]", trans = "log10") +
  theme_ms() + labs(subtitle = "2020-12-10 : 2021-03-31") +
  theme(legend.title = element_blank(),
        plot.subtitle = element_text(size = 8))

p6 <- ggplot(df_16882_2020_12) +
  geom_point(aes(as.numeric(Flow), Depth, color = "Measured"),
             size = 0.5) +
  geom_path(aes(as.numeric(Flow), Depth, color = "Measured", linetype = "Measured"),
            alpha = 0.5) +
  geom_point(aes(as.numeric(predicted), Depth, color = "Rating Curve"),
             size = 0.5) +
  geom_path(aes(as.numeric(predicted), Depth, color = "Rating Curve", linetype = "Rating Curve"),
            alpha = 0.5,
            size = 0.5) +
  scale_color_discrete("") + scale_linetype_discrete("") +
  scale_x_continuous("Flow [cfs]", trans = "log10") +
  labs(subtitle = "2020-12-10 : 2021-03-31") + theme_ms() +
  theme(plot.subtitle = element_text(size = 8))

agg_png(here::here("Figures/04-ratingcurve-16882.png"),
        width = 6.5,
        height = 7,
        units = "in",
        res = 300)
(p1 + p2) / (p3 + p4) / (p5 + p6) + plot_annotation(tag_levels = "A")
dev.off()


# Generate Daily Flow Record ####

# use original hobo dataset
# estimate flows using rating curves by date
# aggregate to mean daily flows, report summary stats.

hobo_df %>%
  dplyr::select(Site, Depth = Water_Level, Date_Time) %>%
  group_split(Site) %>%
  map(~mutate(.x,
              time_lag = lag(Date_Time, default = Date_Time[1]),
              time_lead = lead(Date_Time),
              diff_time = as.numeric(difftime(time_lead,time_lag, units = "hours")),
              diff_depth = lead(Depth) - lag(Depth))) %>%
  map(~filter(.x, !is.na(diff_depth))) %>%
  map(~mutate(.x,
              J = as.numeric(diff_depth)/as.numeric(diff_time))) %>%
  bind_rows() -> estimated_data

## make dataframe of models, prediction data, then map predict function, and unnest datframes.
tibble(rating_curve = list(rc_16396_2020_03, rc_16396_2020_05, rc_16396_2020_12,
                           rc_16397_2021_01, rc_16397_2021_03,
                           rc_16882_2020_03, rc_16882_2020_10, rc_16882_2020_12),
       data = list(
         estimated_data %>% filter(Site == "SWQM-16396" & Date_Time < as.POSIXct("2020-05-18", tz = "Etc/GMT-6")),
         estimated_data %>% filter(Site == "SWQM-16396" &Date_Time >= as.POSIXct("2020-05-18", tz = "Etc/GMT-6") &
                                     Date_Time < as.POSIXct("2020-12-14", tz = "Etc/GMT-6")),
         estimated_data %>% filter(Site == "SWQM-16396" & Date_Time >= as.POSIXct("2020-12-14", tz = "Etc/GMT-6") & Date_Time <= as.POSIXct("2021-03-31", tz = "Etc/GMT-6")),
         estimated_data %>% filter(Site == "SWQM-16397" & Date_Time <= as.POSIXct("2021-01-20", tz = "Etc/GMT-6")),
         estimated_data %>% filter(Site == "SWQM-16397" & Date_Time > as.POSIXct("2021-01-20", tz = "Etc/GMT-6")),
         estimated_data %>% filter(Site == "SWQM-16882" & Date_Time < as.POSIXct("2020-10-23", tz = "Etc/GMT-6")),
         estimated_data %>% filter(Site == "SWQM-16882" & Date_Time >= as.POSIXct("2020-10-23", tz = "Etc/GMT-6") & Date_Time < as.POSIXct("2020-12-10", tz = "Etc/GMT-6")),
         estimated_data %>% filter(Site == "SWQM-16882" & Date_Time >= as.POSIXct("2020-12-10", tz = "Etc/GMT-6"))
       )
) %>%
  mutate(predicted = map2(rating_curve, data,
                          ~exp(predict(.x, newdata = .y))
  )) %>%
  tidyr::unnest(c(data, predicted)) %>%
  mutate(predicted = case_when(
    Site != "SWQM-16397" ~ predicted,
    Site == "SWQM-16397" &  
      Date_Time > as.POSIXct("2021-01-20", tz = "Etc/GMT-6") & 
      as.numeric(Depth) < coefficients(rc_16397_2021_03)[["H_0"]] ~
      1.03,
    Site == "SWQM-16397" &  
      Date_Time > as.POSIXct("2021-01-20", tz = "Etc/GMT-6") & 
      as.numeric(Depth) >= coefficients(rc_16397_2021_03)[["H_0"]] ~
      predicted,
    Site == "SWQM-16397" &  
      Date_Time <= as.POSIXct("2021-01-20", tz = "Etc/GMT-6") ~
      predicted,
    
  )) %>%
  as_tsibble(key = Site, index = Date_Time) -> estimated_data

## plot the data
agg_png(here::here("Figures/05-15-minute-streamflow.png"),
        width = 6.5,
        height = 6.5,
        units = "in",
        res = 300)
ggplot() +
  geom_line(data = estimated_data, aes(Date_Time, predicted, color = "Rating curve estimate"),
            alpha = 0.75) +
  geom_point(data = iqplus_df, aes(Date_Time, as.numeric(Flow), color = "Measured streamflow"), 
             shape = 21,
             alpha = 0.2,
             size = 0.5) +
  facet_wrap(~Site, ncol = 1, scales = "free_y") +
  scale_y_continuous("Flow [cfs]") +
  scale_x_datetime("Date [yyyy-mm-dd]", date_breaks = "1 month",
                   date_labels = "%F") +
  guides(x = guide_axis(angle = 45),
         color = guide_legend("", override.aes = list(linetype = c(0, 1),
                                                      shape = c(16, NA),
                                                      alpha = 1))) +
  theme_ms() +
  theme(axis.text.x = element_text(size = 8))
dev.off()

## calculate mean daily flows and report stats
estimated_data %>%
  dplyr::select(Site, Date_Time, predicted) %>%
  group_by_key() %>%
  index_by(date = ~as_date(.)) %>%
  summarise(mean_daily = mean(predicted, na.rm=TRUE)) -> mean_daily_flow

## report summary stats
mean_daily_flow %>% 
  as_tibble() %>%
  dplyr::select(Site, mean_daily) %>%
  tbl_summary(by = Site, 
              label = list(mean_daily = "Mean Daily Flow (cfs)"),
              digits = list(mean_daily = 2),
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label = "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Sites**") %>%
  modify_caption("Summary statistics of mean daily flow estimates at each site.") %>%
  as_kable()

iqplus_df %>%
  group_by(Site) %>%
  mutate(date = date(Date_Time)) %>%
  group_by(Site, date) %>%
  summarise(n = n(),
            discharge = mean(Flow, na.rm = TRUE)) %>%
  filter(!(n<96))

agg_png(here::here("Figures/06-mean-daily-streamflow.png"),
        width = 6.5,
        height = 6.5,
        units = "in",
        res = 300)
ggplot() + 
  geom_line(data = mean_daily_flow %>% fill_gaps(),
            aes(date, 
                as.numeric(mean_daily),
                color = "Mean daily streamflow (from rating curve)")) +
  geom_point(data = iqplus_df %>%
               group_by(Site) %>%
               mutate(date = date(Date_Time)) %>%
               group_by(Site, date) %>%
               summarise(n = n(),
                         discharge = mean(Flow, na.rm = TRUE)) %>%
               filter(n>96/2), 
             aes(date, 
                 as.numeric(discharge), 
                 color = "Measured mean daily streamflow (days with >= 48 measurements)")) +
  facet_wrap(~Site, ncol = 1, scales = "free_y") +
  scale_x_date("Date [yyyy-mm-dd]", date_breaks = "1 month",
               date_labels = "%F") +
  labs(y = "Mean daily streamflow [cfs]") +
  theme_ms() +
  guides(x = guide_axis(angle = 45),
         color = guide_legend("", override.aes = list(linetype = c(1, 0),
                                                      shape = c(NA, 16),
                                                      alpha = 1))) +
  theme(axis.text.x = element_text(size = 8),
        legend.direction = "vertical") 
dev.off()

mean_daily_flow %>% fill_gaps() -> mean_daily_flow

# Export Data ####

write_xlsx(list(hobo = hobo_df,
                iqplus = iqplus_df),
           here::here("Output-Data/depth_streamflow_data.xlsx"))

write_xlsx(list(rc_fit_2020_03 = df_16396_2020_03,
             rc_fit_2020_12 = df_16396_2020_12,
             parameters = df_results_16369,
             predictions = estimated_data %>%
               filter(Site == "SWQM-16396") %>%
               select(-rating_curve),
             mean_daily_flow = mean_daily_flow %>%
               filter(Site == "SWQM-16396")),
           here::here("Output-Data/16396-Rating-Curve.xlsx"))

write_xlsx(list(rc_fit_2021_03 = df_16397_2021_01,
                rc_fit_2021_01 = df_16397_2021_03,
                parameters = df_results_16397,
                predictions = estimated_data %>%
                  filter(Site == "SWQM-16397") %>%
                  select(-rating_curve),
                mean_daily_flow = mean_daily_flow %>%
                  filter(Site == "SWQM-16397")),
           here::here("Output-Data/16397-Rating-Curve.xlsx"))

write_xlsx(list(rc_fit_2020_03 = df_16882_2020_03,
                rc_fit_2020_10 = df_16882_2020_10,
                rc_fit_2020_12 = df_16882_2020_12,
                parameters = df_results_16882,
                predictions = estimated_data %>%
                  filter(Site == "SWQM-16882") %>%
                  select(-rating_curve),
                mean_daily_flow = mean_daily_flow %>%
                  filter(Site == "SWQM-16882")),
           here::here("Output-Data/16882-Rating-Curve.xlsx"))

write_csv(mean_daily_flow,
          here::here("Processed-Data/model_df.csv"))
