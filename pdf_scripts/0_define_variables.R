# Load libraries ----------------------------------------------------------
library(readxl)
library(lubridate)
library(tidyr)
library(purrr)
library(dplyr)


# Define data paths -------------------------------------------------------
datasets <- plots <- list()

paths <- list(
  input_data = "data/RGAR_2020_input_data.xlsx",
  input_text = "data/NRS - RGAR 2020 - 3 - Create - 0 - Assets - data - text.xlsx"
)

# Style colors
# Neutral colours
col_neut_black <- "#000000"
col_neut_tundora <- "#4B4B4B"
col_neut_grey <- "#808080"
col_neut_silver <- "#b9b9b9"
col_neut_white <- "#FFFFFF"
col_nrs_purple <- "#84329b"
col_nrs_light <- "#EBE6E6"
# Household colours
col_house <- "#5C7B1E"
col_house_dark <- "#496218"
col_house_light <- "#ADBD8E"
# Population colours
col_pop <- "#2DA197"
col_pop_dark <- "#248078"
col_pop_light <- "#96D0CB"
# Births colours
col_births <- '#2E8ACA'
col_births_dark <- '#246EA1'
col_births_light <- '#81B8Df'
# Deaths colours
col_deaths <- "#284F99"
col_deaths_dark <- "#203F7A"
col_deaths_light <- "#93A7CC"
# Life expectancy colours
col_life_exp <- "#6566AE"
col_life_exp_dark <- "#50518B"
col_life_exp_light <- "#B2B2D6"
# Migration colours
col_mig <- "#90278E"
col_mig_dark <- "#731F71"
col_mig_light <- "#C793C6"
# Marriage and electoral colours
col_marri <- "#C9347C"
col_marri_dark <- "#A02963"
col_marri_light <- "#E499BD"
# Adoptions colours
col_adopt <- "#EE6214"
col_adopt_dark <- '#BE4E10'
col_adopt_light <- '#F6B089'
