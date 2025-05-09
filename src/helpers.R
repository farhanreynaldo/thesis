library(dplyr)
library(here)
library(haven)
library(arm)
library(ipumsr)

# Set working directory
here::i_am("src/helpers.R")


# Constants ---------------------------------------------------------------

reg.lkup <- c(3, 4, 4, 3, 4, 4, 1, 1, 5, 3, 3, 4, 4, 2, 2, 2, 2, 3, 3, 1, 1, 1, 2, 2, 3, 2, 4, 2, 4, 1, 1, 4, 1, 3, 2, 2, 3, 4, 1, 1, 3, 2, 3, 3, 4, 1, 3, 4, 1, 2, 4)
reg.label <- c("Northeast", "Midwest", "South", "West", "DC")
stt.label <- c(state.name[1:8], "DC", state.name[9:50])
stt.abb.label <- c(state.abb[1:8], "DC", state.abb[9:50])
eth.label <- c("White", "Black", "Hispanic", "Other")
inc.label <- c("$0-20k", "$20-40k", "$40-75k", "$75-150k", "$150k+")
age.label <- c("18-29", "30-44", "45-64", "65+")
sex.label <- c("Male", "Female")
educ.label <- c("< HS", "HS", "Some College", "College", "Post-Grad")
metro.label <- c("Rural", "Suburban", "Urban")

state_encoder <- setNames(1:51, c(state.name[1:8], "DC", state.name[9:50]))
region_encoder <- setNames(reg.label, 1:5)

stt_dict <- data.frame(
  state_name = stt.label, state_abbr = stt.abb.label, stt = factor(1:51),
  region = factor(reg.lkup), region_name = region_encoder[reg.lkup]
)

# Modeling functions -----------------------------------------------------

process_states_data <- function(states_election_data, states_pop_income_data, year) {
  states_election <- states_election_data %>%
    dplyr::filter(party_simplified %in% c("DEMOCRAT", "REPUBLICAN"), year > 2000) %>%
    dplyr::group_by(year, state, state_abbr = state_po, party_simplified, totalvotes) %>%
    dplyr::summarise(votes = sum(candidatevotes), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = party_simplified, values_from = votes, values_fill = list(votes = 0)) %>%
    dplyr::rename(dem_raw_votes = DEMOCRAT, rep_raw_votes = REPUBLICAN) %>%
    dplyr::mutate(
      dem_votes = dem_raw_votes / (dem_raw_votes + rep_raw_votes),
      rep_votes = rep_raw_votes / (dem_raw_votes + rep_raw_votes)
    )

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

process_election_data <- function(election_data, year) {
  rural_urban_codes <- readxl::read_excel("../data/OEM/ruralurbancodes2013.xls") %>%
    dplyr::rename(county_fips = FIPS) %>%
    dplyr::mutate(metro = case_when(
      Description == "Metro - Counties in metro areas of 1 million population or more" ~ 3,
      Description %in% c(
        "Metro - Counties in metro areas of 250,000 to 1 million population",
        "Metro - Counties in metro areas of fewer than 250,000 population"
      ) ~ 2,
      stringr::str_starts(Description, "Nonmetro") ~ 1,
      TRUE ~ -1
    )) %>%
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
    dplyr::mutate(metro = factor(if_else(is.na(metro), -1, metro))) %>%
    dplyr::filter(!is.na(vote))
}

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
          HISPAN > 0 ~ 2, # Hispanic
          RACE == 1 & HISPAN == 0 ~ 1, # Non-hispanic White
          RACE == 2 & HISPAN == 0 ~ 3, # Non-hispanic Black
          TRUE ~ 4 # Other
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
          METRO == 1 ~ 1, # Rural
          METRO %in% c(3, 4) ~ 2, # Suburban
          METRO == 2 ~ 3, # Urban
          TRUE ~ -1
        )
      ),
      state = as_factor(STATEFIP),
      state = if_else(state == "District of Columbia", "DC", state),
      state = factor(state_encoder[state])
    ) %>%
    dplyr::group_by(
      age = age_group, eth = race, educ = educ_group,
      inc = income_group, metro, stt = state
    ) %>%
    dplyr::summarise(
      pop = n(),
      wtpop = sum(PERWT)
    ) %>%
    dplyr::ungroup()

  return(ipums_agg)
}

generate_poststratification_data <- function(ipums_data_path = "../data/IPUMS/usa_00008.xml") {
  ipums_data <- ipumsr::read_ipums_ddi(ipums_data_path) %>%
    ipumsr::read_ipums_micro()
  years <- c(2008, 2012, 2016, 2020)
  dfs <- purrr::map(years, ~ process_ipums_data(ipums_data, year = .x) %>% mutate(year = .x))

  expand.grid(
    year = years,
    age = factor(1:length(age.label)),
    eth = factor(1:length(eth.label)),
    educ = factor(1:length(educ.label)),
    inc = factor(1:length(inc.label)),
    stt = factor(1:length(stt.label)),
    metro = factor(1:length(metro.label))
  ) %>%
    dplyr::left_join(list_rbind(dfs), by = c("age", "eth", "educ", "inc", "metro", "stt", "year")) %>%
    dplyr::mutate(
      pop = replace_na(pop, 0),
      wtpop = replace_na(wtpop, 0)
    ) %>%
    tidyr::pivot_wider(
      names_from = year,
      values_from = c(pop, wtpop),
      names_sep = ""
    ) %>%
    readr::write_csv("../data/census-pums-pop.csv")
}

get_label <- function(col_name) {
  label <- switch(col_name,
                  eth = eth.label,
                  inc = inc.label,
                  age = age.label,
                  educ = educ.label,
                  metro = metro.label,
                  stt = stt.label
  )
  return(label)
}

relevel_label <- function(col_name) {
  col_name <- deparse(substitute(col_name))
  get_label(col_name)
}

map_label <- function(col) {
  col_name <- deparse(substitute(col))
  label <- get_label(col_name)
  label_mapper <- setNames(label, 1:length(label))
  result <- unname(label_mapper[as.character(col)])
  fct_relevel(result, label)
}


# Survey data processing functions ----------------------------------------

calc_and_add_weighted_results <- function(data_matrix, result_info_matrix) {
  weighted_results_info <- summarize(group_by(result_info_matrix, grp),
    n = sum(ones),
    wt_ybar = sum(vote * weight) / sum(weight),
    cell_design_eff = 1 + var(weight / mean(weight))
  )

  data_matrix <- merge(x = data_matrix, y = weighted_results_info, by = "grp", all.x = T)
  data_matrix <- arrange(data_matrix, ix)

  data_matrix$n[is.na(data_matrix$n)] <- 0
  design_eff <- weighted_mean(data_matrix$cell_design_eff, data_matrix$n, data_matrix$n > 1)
  data_matrix$n_eff <- data_matrix$n / design_eff
  data_matrix$wt_ybar[data_matrix$n_eff == 0] <- 0.5
  data_matrix <- mutate(data_matrix,
    yes = round(wt_ybar * n_eff),
    no = round((1 - wt_ybar) * n_eff),
    n_eff = yes + no
  )
  return(data_matrix)
}

weighted_mean <- function(data_v, weights = rep(1, length(data_v)), subset = rep(TRUE, length(data_v))) {
  keep <- !is.na(data_v) & !is.na(weights) & !is.na(subset) & subset
  return(sum((weights * data_v)[keep]) / sum(weights[keep]))
}

# Vote adjustment functions -----------------------------------------------

weighted_correction <- function(data_v, weights, x0) {
  delta <- optimize(calc_delta_correction, interval = c(-5, 5), data_v, weights, x0)$minimum
  corrected <- arm::invlogit(logit(data_v) + delta)
  return(list(delta = delta, corrected = corrected))
}

calc_delta_correction <- function(delta, data_v, weights, x0) {
  abs(x0 - sum(arm::invlogit(logit(data_v) + delta) * weights))
}

logit <- function(data_v) log(data_v / (1 - data_v))
