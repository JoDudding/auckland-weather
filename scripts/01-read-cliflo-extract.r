#--------------------------------------------------------------------------
#' 01-read-cliflo-extract.r
#' auckland-weather
#' read in data extracted from cliflo
#' https://cliflo.niwa.co.nz/pls/niwp/wgenf.genform1?cdt=ls_ra&cadd=t
#' daily rainfall and min/max temps from 1965-01-01
#' station is Auckland Aero (id 1962)
#--------------------------------------------------------------------------

source('scripts/_setup.r')

# read raw files ----------------------------------------------------------

cliflo_raw_temp <- read_csv("data/1962__Temperature__daily/1962__Temperature__daily.csv") |> 
  glimpse()

cliflo_raw_rain <- read_csv("data/1962__Rain__daily/1962__Rain__daily.csv") |> 
  glimpse()


# clean data and join -----------------------------------------------------

cliflo_temp <- cliflo_raw_temp |> 
  clean_names() |> 
  mutate(
    date_nzst = as_date(observation_time_utc),
    tmax_c = maximum_temperature_deg_c,
    tmin_c = minimum_temperature_deg_c,
    tmean_c = mean_temperature_deg_c         
  ) |> 
  select(date = date_nzst, tmax = tmax_c, tmin = tmin_c) |> 
  glimpse()

max(cliflo_temp$date)

cliflo_rain <- cliflo_raw_rain |> 
  clean_names() |> 
  mutate(date_nzst = as_date(observation_time_utc )) |> 
  select(date = date_nzst, amount_mm = rainfall_mm ) |> 
  glimpse()

max(cliflo_rain$date)

cliflow <- full_join(cliflo_temp, cliflo_rain, by = 'date') |> 
  mutate(
    month = month(date),
    year = year(date),
    decade = year %/%10 * 10
  ) |> 
  filter(year >= 1966) |> 
  group_by(year) |> 
  arrange(date) |> 
  mutate(
    cum_amount_mm = cumsum(coalesce(amount_mm, 0)),
    day_of_year = case_when(
      leap_year(date) & yday(date) == 60 ~ NA_real_,
      leap_year(date) & yday(date) > 60 ~ yday(date) -1,
      TRUE ~ yday(date))
  ) |> 
  ungroup()

cli_alert_info(
  'Data from 1965 was removed as is was not available for all metrics'
)

# check completeness ------------------------------------------------------

cliflow |> 
  group_by(year) |> 
  summarise(
    n_days = n(),
    across(tmax:amount_mm, ~mean(is.na(.x)))
  ) |> 
  gather(tmax:amount_mm, key = 'key', value = 'value') |> 
  mutate(
    label = case_when(
      value > 0.05 ~ year
    )
  ) |> 
  arrange(-value) |> 
  ggplot(aes(year, value, label = label)) +
  geom_col() +
  geom_text(vjust = 0) +
  scale_y_continuous(label = percent, limits = c(0, 1)) +
  facet_wrap(~key, ncol = 1)



# save as rds -------------------------------------------------------------

cliflow |> 
  saveRDS('data/cliflo_auckland.rds')

#--------------------------------------------------------------------------