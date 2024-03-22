#--------------------------------------------------------------------------
#' 02-get-noaa-data.r
#' auckland-weather
#' download data from NOAA
#--------------------------------------------------------------------------

source('scripts/_setup.r')

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
noaa <- ghcn |>
  select(yearmoda, element, value) |>
  filter(element %in% c("PRCP", "TMAX", "TMIN")) |>
  pivot_wider(names_from = element, values_from = value) |>
  clean_names() |> 
  transmute(
    date = ymd(yearmoda),
    year = year(date),
    month = month(date),
    decade = year %/%10 * 10,
    # convert from tenths
    tmax = tmax / 10,
    tmin = tmin / 10,
    amount_mm = prcp / 10
  ) |> 
  filter(year >= 2010)

# save as rds -------------------------------------------------------------

noaa |> 
  saveRDS('data/noaa_auckland.rds')

#--------------------------------------------------------------------------


