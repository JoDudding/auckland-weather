
# setup -------------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(glue)
library(janitor)
library(lubridate)
library(yaml)
library(cli)

# read parameters ---------------------------------------------------------

params <- read_yaml('params.yml')

cli_alert_info('Parameters used:')
cli_li(params)

# check data range --------------------------------------------------------

stations <- tempfile()
download.file(params$ghcnd_inventory, stations)

element_range <- tibble(lines = readLines(stations)) |> 
  separate(
    lines, 
    into = c('station', NA, NA, 'element', 'first', 'last'), 
    sep = '\\s+' 
  ) |> 
  filter(
    station == params$station,
    element %in% c("PRCP", "TMAX", "TMIN")
  ) |> 
  mutate(
    first = as.integer(first),
    last = as.integer(last),
    num_years = last - first
  ) |> 
  print()

if(max(element_range$num_years) < 50) {
  cli_alert_danger('Insufficient numbers of years available')
}

cli_alert_info('May be able to supplement with NIWA data')  


# read file ---------------------------------------------------------------

# download the zipped file
temp <- tempfile()
download.file(glue("{params$ghcnd_path}{params$station}.csv.gz"),temp)

# unzip and read
ghcn <- read_csv(
  temp,
  col_names = c("id", "yearmoda", "element", "value", "mflag", "qflag", 
    "sflag", "obs_time"),
  col_types = "cccncccc"
)

# delete the zipped file
unlink(temp)

# reformat data -----------------------------------------------------------

# subset and format
ghcn_wide <- ghcn |>
  select(yearmoda, element, value) |>
  filter(element %in% c("PRCP", "TMAX", "TMIN")) |>
  pivot_wider(names_from = element, values_from = value) |>
  clean_names() |> 
  mutate(
    date = ymd(yearmoda),
    year = year(date),
    month = month(date),
    month_name = month(date, label = TRUE),
    day = day(date),
    # convert from tenths
    tmax = tmax / 10,
    tmin = tmin / 10,
    prcp = prcp / 10
  ) |> 
  group_by(year) |> 
  arrange(date) |> 
  mutate(
    cum_precip = cumsum(prcp),
    day_of_year = case_when(
      leap_year(date) & yday(date) == 60 ~ NA_real_,
      leap_year(date) & yday(date) > 60 ~ yday(date) -1,
      TRUE ~ yday(date))
  ) |>
  select(year, month, month_name, day, date, day_of_year, prcp, cum_precip, tmax, tmin)

ghcn_wide |>
  count(year) |> 
  print(n = 50)

cli_alert_danger('Insufficient days per year')

# save data ---------------------------------------------------------------

write_csv(ghcn_wide, glue("data/{params$station}.csv"))

