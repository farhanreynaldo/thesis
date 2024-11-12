# Load libraries
library(tidyverse)
library(here)
library(haven)
library(arm)
library(rstanarm)
library(tidybayes)

# Set working directory
here::i_am("src/election.R")

# Source helper functions
source(here("src/helpers.R"))

state_encoder <- setNames(1:51, c(state.name[1:8], "DC", state.name[9:50]))

# Function to process states data
process_states_data <- function(states_election_data, states_pop_income_data, year) {
  states_election <- states_election_data %>%
    dplyr::filter(party_detailed %in% c("DEMOCRAT", "REPUBLICAN"), year > 2000) %>%
    dplyr::group_by(year, state, state_abbr = state_po, party_detailed, totalvotes) %>%
    dplyr::summarise(votes = sum(candidatevotes), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = party_detailed, values_from = votes, values_fill = list(votes = 0)) %>%
    dplyr::rename(dem_votes = DEMOCRAT, rep_votes = REPUBLICAN) %>%
    dplyr::mutate(dem_votes = dem_votes / totalvotes, rep_votes = rep_votes / totalvotes)

  states_data <- states_election %>%
    dplyr::left_join(states_pop_income_data, by = c("state", "year")) %>%
    dplyr::mutate(turnout = totalvotes / total_pop) %>%
    dplyr::group_by(state) %>%
    dplyr::mutate(
      rep_votes_prev = lag(rep_votes),
      dem_votes_prev = lag(dem_votes),
      turnout_prev = lag(turnout)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == !!year) %>%
    dplyr::left_join(stt_dict, by = c("state_abbr" = "state_abbr")) %>%
    dplyr::mutate(
      z_incstt = arm::rescale(median_income),
      z_trnprv = arm::rescale(turnout_prev),
      z_repprv = arm::rescale(rep_votes_prev)
    )
}

# Process election data
process_election_data <- function(election_data, year) {
  rural_urban_codes <- readxl::read_excel("../data/OEM/ruralurbancodes2013.xls") %>%
    dplyr::rename(county_fips = FIPS) %>%
    dplyr::mutate(metro = factor(case_when(
      Description == "Metro - Counties in metro areas of 1 million population or more" ~ 3,
      Description %in% c("Metro - Counties in metro areas of 250,000 to 1 million population",
                         "Metro - Counties in metro areas of fewer than 250,000 population") ~ 2,
      stringr::str_starts(Description, "Nonmetro") ~ 1,
      TRUE ~ -1
    ))) %>%
    dplyr::select(county_fips, metro)

  election_data %>%
    dplyr::select(case_id, year, gender, age, race, educ, faminc, state, county_fips,
      weight = weight_cumulative, vv_turnout_gvm, vote = voted_pres_party
    ) %>%
    dplyr::filter(year %in% !!year) %>%
    dplyr::filter(vote %in% c("Republican", "Democratic")) %>%
    dplyr::rename(age_detailed = age) %>%
    dplyr::mutate(
      inc = factor(
        case_when(
          faminc %in% c("Less than 10k", "10k - 20k") ~ 1,
          faminc %in% c("20k - 30k", "30k - 40k") ~ 2,
          faminc %in% c("40k - 50k", "50k - 60k", "60k - 70k") ~ 3,
          faminc %in% c("70k - 80k", "80k - 100k", "100k - 120k", "120k - 150k") ~ 4,
          faminc == "150k+" ~ 5,
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
      state = if_else(state == "District of Columbia", "DC", state),
      stt = factor(state_encoder[state])
    ) %>%
    dplyr::left_join(rural_urban_codes, by = "county_fips") %>%
    dplyr::mutate(metro = if_else(is.na(metro), -1, metro)) %>%
    dplyr::filter(!is.na(vote))
}

# Define function to process IPUMS data for poststratification
process_ipums_data <- function(ipums_data, year) {
  ipums_agg <- ipums_data %>%
    dplyr::filter(YEAR == year, AGE >= 18) %>%
    dplyr::mutate(
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
          HISPAN > 0 ~ 2,              # Hispanic
          RACE == 1 & HISPAN == 0 ~ 1, # Non-hispanic White
          RACE == 2 & HISPAN == 0 ~ 3, # Non-hispanic Black
          TRUE ~ 4                     # Other
        )
      ),
      educ_group = factor(
        case_when(
          EDUC %in% 0:3 ~ 1,
          EDUC == 6 & EDUCD %in% 65:66 ~ 3,
          EDUC %in% 4:6 ~ 2,
          EDUC %in% 7:10 ~ 4,
          EDUC == 11 ~ 5,
          TRUE ~ -1
        )
      ),
      metro = factor(
        case_when(
          METRO == 1 ~ 1,          # Rural
          METRO %in% c(3, 4) ~ 2,  # Suburban
          METRO == 2 ~ 3,          # Urban
          TRUE ~ -1
        )
      ),
      state = as_factor(STATEFIP),
      state = if_else(state == "District of Columbia", "DC", state),
      state = factor(state_encoder[state]),
      sex = as_factor(SEX, levels = "values")
    ) %>%
    dplyr::group_by(sex,
      age = age_group, eth = race, educ = educ_group,
      inc = income_group, metro, stt = state
    ) %>%
    dplyr::summarise(
      pop = n(),
      wtpop = sum(PERWT)
    ) %>%
    dplyr::ungroup()

  ipums_agg

  # expand.grid(
  #   sex = factor(1:length(sex.label)),
  #   age = factor(1:length(age.label)),
  #   eth = factor(1:length(eth.label)),
  #   educ = factor(1:length(educ.label)),
  #   inc = factor(1:length(inc.label)),
  #   stt = factor(1:length(stt.label)),
  #   metro = factor(1:length(metro.label))
  # ) %>%
  #   left_join(ipums_agg, by = c("sex", "age", "eth", "educ", "inc", "metro", "stt")) %>%
  #   mutate(pop = replace_na(pop, 0),
  #          wtpop = replace_na(wtpop, 0)) %>%
  #   write_csv("../data/census-pums-pop-2016.csv")
}
