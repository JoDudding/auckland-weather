library(tidyverse)
library(scales)
library(gfonts)

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


# select a font to use ----------------------------------------------------

get_all_fonts()

setup_font(
  id = 'roboto'
)

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

# this will set the theme for all charts

theme_set(
  theme_bw() +
    theme(
      plot.title.position = 'plot',
      panel.border = ggplot2::element_blank(),
      panel.background = element_rect(fill = morgan_background, colour = NA),
      plot.background = element_rect(fill = morgan_background, colour = NA)
    )
  )

p + scale_fill_manual(values = morgan_swatch) 










