#--------------------------------------------------------------------------
#' 02-trend-data.r
#' auckland-weather
#' calculate trend information
#--------------------------------------------------------------------------

source('scripts/_setup.r')

cliflow <- readRDS('data/cliflo_auckland.rds')

max_year <- max(cliflow$year)
min_date <- min(cliflow$date)
max_date <- max(cliflow$date)

# daily stats -------------------------------------------------------------

stats <- cliflow |> 
  filter(year != max_year) |> 
  filter(! is.na(day_of_year)) |> 
  gather(c(tmax, tmin, amount_mm, cum_amount_mm), key = 'key', value = 'value') |> 
  group_by(day_of_year, key) |> 
  summarise(
    max = max(value, na.rm = T),
    min = min(value, na.rm = T),
    x5 = quantile(value, 0.05, na.rm = T),
    x20 = quantile(value, 0.2, na.rm = T),
    x40 = quantile(value, 0.4, na.rm = T),
    x60 = quantile(value, 0.6, na.rm = T),
    x80 = quantile(value, 0.8, na.rm = T),
    x95 = quantile(value, 0.95, na.rm = T)
  ) |> 
  ungroup()


# x axis ------------------------------------------------------------------

x_axis <- cliflow |> 
  filter(! is.na(day_of_year)) |> 
  group_by(month) |> 
  summarise(
    date = min(date),
    day_of_year = min(day_of_year)
  ) |> 
  ungroup() |> 
  mutate(
    label = month(date, label = TRUE)
  )

x_break <- x_axis$day_of_year
x_label <- x_axis$label

# second y axis -----------------------------------------------------------

y_axis <- stats |> 
  filter(day_of_year == max(day_of_year)) |> 
  select(-day_of_year) |> 
  gather(-key, key = 'label', value = 'value') |> 
  mutate(
    label = case_match(
      label,
      'x5' ~ '5th',
      'x20' ~ '20th',
      'x40' ~ '40th',
      'x60' ~ '60th',
      'x80' ~ '80th',
      'x95' ~ '95th',
      .default = label
    )
  )

# this year ---------------------------------------------------------------

this_year <- cliflow |> 
  filter(year == max_year) |> 
  filter(! is.na(day_of_year)) |> 
  select(day_of_year, tmax, tmin, amount_mm, cum_amount_mm) |> 
  gather(-day_of_year, key = 'key', value = 'latest')


# create charts -----------------------------------------------------------

create_chart <- function(metric) {

  break_value <- y_axis |> 
    filter(key == metric) |>
    pull(value)
  
  break_labels <- y_axis |> 
    filter(key == metric) |>
    pull(label)

  metric_name <- case_match(
    metric,
    'tmax' ~ 'Maximum Temperature (°C)',
    'tmin' ~ 'Miminum Temperature (°C)',
    'cum_amount_mm' ~ 'Year to date Rainfall (mm)'
  )
  
  stats |> 
    left_join(this_year, by = c('key', 'day_of_year')) |> 
    mutate(
      record_high = case_when(latest > max ~ latest),
      record_low = case_when(latest < min ~ latest)
    ) |> 
    filter(key == metric) |> 
    ggplot(aes(day_of_year, latest)) +
    geom_vline(xintercept = x_break[2:12], lty = 3, lwd = 0.2) +
    geom_ribbon(aes(ymin = min, ymax = max),fill = "#bdc9e1") +
    # ribbon between the 5th and 20th, 80th to 95th percentiles
    geom_ribbon(aes(ymin = x5, ymax = x95), fill = "#74a9cf") +
    # ribbon between the 20th and 40th, 60th and 80th percentiles
    geom_ribbon(aes(ymin = x20, ymax = x80), fill = "#2b8cbe") +
    # ribbon between the 40th and 60th percentiles
    geom_ribbon(aes(ymin = x40, ymax = x60), fill = "#045a8d") +
    geom_line(lwd = 1.2) +
    geom_point(aes(y = record_high), colour = 'red') +
    geom_point(aes(y = record_low), colour = 'orange') +
    scale_x_continuous(
      expand = expansion(0.0), breaks = x_break + 15, labels = x_label
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(transform = ~.x, breaks = break_value, 
        labels = break_labels)
    ) +
    labs(
      x = NULL, y = NULL,
      title = glue('Daily {metric_name} at Auckland Aero station'),
      subtitle = glue('{min_date} to {max_date}'),
      caption = 'Source: NIWA CLIFLO'
    ) +
    theme(
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "linen", colour = "linen"),
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", size = 16),
      axis.ticks = element_blank()
    )

}

create_chart('cum_amount_mm')
ggsave('graphs/auckland-cum_amount_mm.png', width = 9, height = 4.5)
create_chart('tmax')
ggsave('graphs/auckland-tmax.png', width = 9, height = 4.5)
create_chart('tmin')
ggsave('graphs/auckland-tmin.png', width = 9, height = 4.5)
