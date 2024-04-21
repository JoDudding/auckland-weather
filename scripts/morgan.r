library(tidyverse)
library(scales)
library(ragg)

# mock up plots to use for testing ----------------------------------------

library(palmerpenguins)

p <- penguins |> 
  count(island, species) |> 
  mutate(
    pct = n / sum(n),
    .by = island
  ) |> 
  ggplot(aes(island, pct, fill = species)) +
  geom_col() +
  scale_y_continuous(label = percent, expand = c(0, 0)) +
  labs(
    x = 'Island', y = NULL, fill = NULL,
    title = 'Mix of penguin species by island'
  )

p + theme_bw()

# alter to use morgan theme -----------------------------------------------

morgan_swatch <- c('#FAAB72', '#ECB3D6', '#a0babc', '#F7DA94', '#c8e9eb', 
  '#F6A7A0')

morgan_swatch |> 
  show_col()

morgan_light_dark <- c(
  colorspace::lighten(morgan_swatch[4], 0.5),
  colorspace::darken(morgan_swatch[1], 0.25) 
)

morgan_light_dark |> 
  show_col()

morgan_background <- '#F6F2E9'

#' this will set the theme for all charts
#' you can use any font installed on your compute
#' the font used (montserrat) has been downloaded from google fonts
#' https://fonts.google.com/specimen/Montserrat?query=Montserrat


theme_set(
  theme_bw(base_size = 12, base_family = "Montserrat") +
    theme(
      plot.title.position = 'plot',
      plot.title = element_text(size = 20, face = 'bold'),
      panel.border = ggplot2::element_blank(),
      panel.background = element_rect(fill = morgan_background, colour = NA),
      plot.background = element_rect(fill = morgan_background, colour = NA)
    )
  )

p + scale_fill_manual(values = morgan_swatch) 


