library(arrow)
library(tidyverse)

election_all <- read_feather(here("data/CCES Cumulative/cumulative_2006-2023.feather"))
states_election_data <- read_csv(here("data/MEDSL/State Presidential Election Returns/1976-2020-president.csv"))
states_pop_income_data <- read_csv(here("data/state_pop_income.csv")) %>%
  dplyr::mutate(
    state = str_to_upper(NAME),
    year = if_else(year == 2019, 2020, year)
  ) %>%
  dplyr::select(-c(NAME, GEOID))
