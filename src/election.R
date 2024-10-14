# Load libraries
library(tidyverse)
library(here)
library(haven)
library(arm)
library(conflicted)
library(rstanarm)
library(tidybayes)

# Set working directory
here::i_am("src/election.R")

# Source helper functions
source(here("src/helpers.R"))

# Resolve conflicts
conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
  dplyr::lag()
)

state_encoder <- setNames(1:51, c(state.name[1:8], "DC", state.name[9:50]))

# Function to process states data
process_states_data <- function(states_election_data, states_pop_income_data, year) {
  states_election <- states_election_data %>%
    filter(party_detailed %in% c("DEMOCRAT", "REPUBLICAN"), year > 2000) %>%
    group_by(year, state, state_abbr = state_po, party_detailed, totalvotes) %>%
    summarise(votes = sum(candidatevotes), .groups = "drop") %>%
    pivot_wider(names_from = party_detailed, values_from = votes, values_fill = list(votes = 0)) %>%
    rename(dem_votes = DEMOCRAT, rep_votes = REPUBLICAN)

  states_data <- states_election %>%
    left_join(states_pop_income_data, by = c("state", "year")) %>%
    group_by(state) %>%
    mutate(
      rep_votes_prev = lag(rep_votes),
      dem_votes_prev = lag(dem_votes),
      votes_prev = lag(totalvotes)
    ) %>%
    ungroup() %>%
    filter(year == !!year) %>%
    left_join(stt_dict, by = c("state_abbr" = "state_abbr")) %>%
    mutate(
      z_incstt = scales::rescale(median_income),
      z_trnprv = scales::rescale(votes_prev),
      z_repprv = scales::rescale(rep_votes_prev)
    )
}

# Process election data
process_election_data <- function(election_data, year) {
  vote_column <- case_when(
    year == 2008 ~ "voted_pres_08",
    year == 2012 ~ "voted_pres_12",
    year == 2016 ~ "voted_pres_16",
    year == 2020 ~ "voted_pres_20",
    TRUE ~ NA_character_
  )

  if (is.na(vote_column)) {
    stop(paste("Error: The vote column for the specified year =", year, "is not available."))
  }

  election_data %>%
    select(case_id, year, gender, age, race, educ, faminc, state, weight = weight_cumulative, vv_turnout_gvm, vote = all_of(vote_column)) %>%
    rename(age_detailed = age, stt = state) %>%
    mutate(
      inc = factor(
        case_when(
          faminc %in% 1:2 ~ 1,
          faminc %in% 3:4 ~ 2,
          faminc %in% 5:7 ~ 3,
          faminc %in% 8:11 ~ 4,
          faminc == 12 ~ 5,
          TRUE ~ -1
        )
      ),
      age = factor(
        case_when(
          age_detailed %in% 18:29 ~ 1,
          age_detailed %in% 30:44 ~ 2,
          age_detailed %in% 45:64 ~ 3,
          age_detailed >= 65 ~ 4,
          TRUE ~ -1
        )
      ),
      educ = factor(
        case_when(
          educ == 1 ~ 1,
          educ == 2 ~ 2,
          educ == 3 ~ 3,
          educ %in% 4:5 ~ 4,
          educ == 6 ~ 5,
          TRUE ~ -1
        )
      ),
      eth = factor(
        if_else(!race %in% 1:3, 4, race)
      ),
      stt = as_factor(stt, levels = "values")
    ) %>%
    # This line filters the dataset to include only rows where the variable 'vv_turnout_gvm' equals 1
    # and the 'vote' variable is not missing (NA). This is ensuring that only valid votes
    # are considered in the analysis.
    filter(vv_turnout_gvm == 1, !is.na(vote))
}

# Define function to process IPUMS data for poststratification
process_ipums_data <- function(ipums_data, year) {
  ipums_data %>%
    filter(YEAR == year, AGE >= 18) %>%
    mutate(
      case_id = str_c(SERIAL, SAMPLE, PERNUM),
      income_group = factor(
        case_when(
          between(HHINCOME, -Inf, 20000) ~ 1,
          between(HHINCOME, 20001, 40000) ~ 2,
          between(HHINCOME, 40001, 75000) ~ 3,
          between(HHINCOME, 75001, 150000) ~ 4,
          HHINCOME > 150000 ~ 5,
          TRUE ~ -1
        )
      ),
      age_group = factor(
        case_when(
          between(AGE, 18, 29) ~ 1,
          between(AGE, 30, 44) ~ 2,
          between(AGE, 45, 64) ~ 3,
          AGE >= 65 ~ 4,
          TRUE ~ -1
        )
      ),
      race = factor(
        case_when(
          HISPAN > 0 ~ 2, # Hispanic
          RACE == 1 & HISPAN == 0 ~ 1, # Non-hispanic White
          RACE == 2 & HISPAN == 0 ~ 3, # Non-hispanic Black
          TRUE ~ 4 # Other
        ),
        labels = eth.label
      ),
      educ_group = factor(
        case_when(
          EDUC %in% 0:3 ~ 1,
          EDUC == 6 & EDUCD %in% 65:66 ~ 3,
          EDUC %in% 4:6 ~ 2,
          EDUC %in% 7:10 ~ 4,
          EDUC == 11 ~ 5,
          TRUE ~ NA_integer_
        )
      ),
      state = as_factor(STATEFIP, levels = "values"),
      sex = as_factor(SEX, levels = "values")
    ) %>%
    group_by(sex, age = age_group, eth = race, educ = educ_group, inc = income_group, stt = state) %>% 
    summarise(pop = n(), 
              wtpop = sum(PERWT)) %>% 
    ungroup()
}