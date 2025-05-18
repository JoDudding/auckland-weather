#--------------------------------------------------------------------------
#' 03-daily-temp.r
#' auckland-weather
#' create a csv with the median to max temperature range for each day
#--------------------------------------------------------------------------

source('scripts/_setup.r')

cliflow <- readRDS('data/cliflo_auckland.rds')

# daily stats -------------------------------------------------------------

cliflow |> 
  filter(year != max_year) |> 
  filter(! is.na(day_of_year)) |> 
  gather(c(tmax, tmin), key = 'key', value = 'value') |> 
  group_by(month = month(date, label = TRUE), day = day(date), key) |> 
  summarise(
    median = comma(quantile(value, 0.5, na.rm = T), 0.1),
    x90 = comma(quantile(value, 0.9, na.rm = T), 0.1),
    max = comma(max(value, na.rm = T), 0.1)
  ) |> 
  ungroup() |> 
  pivot_wider(
    names_from = key,
    values_from = median:max,
    names_glue = "{key}_{.value}"
  ) |> 
  select(month, day, starts_with("tmin"), starts_with("tmax")) |> 
  print() |> 
  write_csv("outputs/daily-extreme-temp.csv")
