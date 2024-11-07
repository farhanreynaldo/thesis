library(arrow)
library(ipumsr)
library(tidyverse)

election_all <- read_feather(here("data/CCES Cumulative/cumulative_2006-2023.feather"))
# ipums_data <- read_ipums_ddi("../data/IPUMS/usa_00006.xml") %>%
#   read_ipums_micro()
states_election_data <- read_csv(here("data/MEDSL/State Presidential Election Returns/1976-2020-president.csv"))
states_pop_income_data <- read_csv(here("data/state_pop_income.csv")) %>%
  dplyr::mutate(
    state = str_to_upper(NAME),
    year = if_else(year == 2019, 2020, year)
  ) %>%
  dplyr::select(-c(NAME, GEOID))

# Define constants
reg.lkup <- c(3, 4, 4, 3, 4, 4, 1, 1, 5, 3, 3, 4, 4, 2, 2, 2, 2, 3, 3, 1, 1, 1, 2, 2, 3, 2, 4, 2, 4, 1, 1, 4, 1, 3, 2, 2, 3, 4, 1, 1, 3, 2, 3, 3, 4, 1, 3, 4, 1, 2, 4)
stt.label <- c(state.name[1:8], "DC", state.name[9:50])
stt.abb.label <- c(state.abb[1:8], "DC", state.abb[9:50])
eth.label <- c("White", "Black", "Hispanic", "Other")
inc.label <- c("$0-20k", "$20-40k", "$40-75k", "$75-150k", "$150k+")
age.label <- c("18-29", "30-44", "45-64", "65+")
sex.label <- c("Male", "Female")
educ.label <- c("< HS", "HS", "Some College", "College", "Post-Grad")
metro.label <- c("Rural", "Suburban", "Urban")

# Create state dictionary
stt_dict <- data.frame(
  state_name = stt.label, state_abbr = stt.abb.label, stt = factor(1:51)
)