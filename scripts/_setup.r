# libraries ---------------------------------------------------------------

library(tidyverse)
library(glue)
library(janitor)
library(yaml)
library(cli)
library(scales)
library(here)

# read parameters ---------------------------------------------------------

params <- read_yaml('params.yml')

cli_alert_info('Parameters used:')
cli_li(params)