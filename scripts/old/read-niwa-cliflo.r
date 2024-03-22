#-------------------------------------------------------------------------------
#' rainfall
#' jo dudding
#' 2023-09-24
#-------------------------------------------------------------------------------

library(tidyverse)
library(scales)
library(here)
library(kableExtra)
library(janitor)
library(cli)

# read csv

auckland <- read_csv(here('data', 'auckland-rainfall-daily-noheader.csv')) |> 
  mutate(
    date = ymd(as.character(`Date(NZST)`)),
    month = month(date),
    year = year(date)
  ) %>%  
  clean_names() |> 
  select(date, month, year, amount_mm) |> 
  filter(year > min(year)) |> 
  glimpse()

min_date <- min(auckland$date)
max_date <- max(auckland$date)

cli_alert_info('Dates range from {min_date} to {max_date}')
cli_alert_info('seems to be a couple of years (1993/1994) that have missing days')

auckland |> 
  count(year) |> 
  arrange(n) |> 
  print()


auckland |> 
  group_by(year) |> 
  arrange(date) |> 
  mutate(
    cum_amount_mm = cumsum(amount_mm)
  ) |> 
  ungroup() |> 
  mutate(
    day_of_year = case_when(
      leap_year(date) & yday(date) == 60 ~ NA_real_,
      leap_year(date) & yday(date) > 60 ~ yday(date) -1,
      TRUE ~ yday(date)
    ),
    decade = case_when(
      year(date) == max(year(date)) ~ year(date),
      TRUE ~ (year(date) %/% 10) * 10
    ) |> as.factor()
  ) |> 
  ggplot(aes(day_of_year, cum_amount_mm, group = year, colour = decade)) +
  geom_line() +
  scale_colour_viridis_d()
