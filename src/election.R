# Load libraries
library(tidyverse)
library(here)
library(haven)
library(janitor)
library(ipumsr)
library(arm)
library(conflicted)
library(brms)
library(rstanarm)
library(tidybayes)
library(lme4)
library(ggtext)
library(usmap)
library(sf)

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

# Use all available cores for parallel processing
options(mc.cores = parallel::detectCores())

# Load data
election_all <- read_dta(here("data/CCES Cumulative/cumulative_2006-2023.dta"))
ipums_data <- read.table(here("data/census-pums-pop-2000-04-08.dat"), header = TRUE, sep = "\t")
states_election_data <- read_csv(here("data/MEDSL/State Presidential Election Returns/1976-2020-president.csv"))
states_pop_income_data <- read_csv(here("data/state_pop_income.csv")) %>%
  mutate(
    state = str_to_upper(NAME),
    year = if_else(year == 2019, 2020, year)
  ) %>%
  select(-c(NAME, GEOID))

# Define constants
reg.lkup <- c(3, 4, 4, 3, 4, 4, 1, 1, 5, 3, 3, 4, 4, 2, 2, 2, 2, 3, 3, 1, 1, 1, 2, 2, 3, 2, 4, 2, 4, 1, 1, 4, 1, 3, 2, 2, 3, 4, 1, 1, 3, 2, 3, 3, 4, 1, 3, 4, 1, 2, 4)
stt.label <- c(state.name[1:8], "DC", state.name[9:50])
stt.abb.label <- c(state.abb[1:8], "DC", state.abb[9:50])
eth.label <- c("White", "Black", "Hispanic", "Other")
inc.label <- c("$0-20k", "$20-40k", "$40-75k", "$75-150k", "$150k+")
age.label <- c("18-29", "30-44", "45-64", "65+")
sex.label <- c("Male", "Female")
educ.label <- c("< HS", "HS", "Some College", "College", "Post-Grad")

# Create state dictionary
stt_dict <- data.frame(
  state_abbr = stt.abb.label, stt = factor(1:51)
)

# Define theme for maps
theme_maps <- function(base_size = 12, dark_text = "#1A242F") {
  mid_text <- monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[2]
  light_text <- monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[3]

  theme_void(base_size = base_size) +
    theme(
      text = element_text(colour = mid_text, family = "IBM Plex Sans", lineheight = 1.1),
      plot.title = element_textbox_simple(colour = dark_text, family = "IBM Plex Sans", face = "bold", size = rel(1.6), margin = margin(12, 0, 12, 0)),
      plot.subtitle = element_textbox_simple(size = rel(1), face = "italic", margin = margin(5, 0, 40, 0)),
      plot.title.position = "plot",
      legend.title = element_text(size = rel(0.9)),
      plot.caption = element_text(size = rel(0.8), margin = margin(8, 0, 0, 0)),
      plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm")
    )
}

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

# Process states data
states_data <- process_states_data(states_election_data, states_pop_income_data, 2016)

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

# Process election data
election_processed <- process_election_data(election_all, 2016)

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
          TRUE ~ -1
        )
      ),
      state = STATEFIP,
    ) %>%
    count(sex = SEX, age = age_group, eth = race, educ = educ_group, inc = income_group, stt = state)
}

# Process IPUMS data for poststratification
# poststrat_data <- process_ipums_data(usa_data, 2016)
poststrat_data <- ipums_data

# Create data matrix
data_matrix <- expand.grid(
  stt = factor(1:length(stt.label)),
  eth = factor(1:length(eth.label)),
  inc = factor(1:length(inc.label)),
  age = factor(1:length(age.label))
) %>%
  mutate(
    grp = apply(., 1, paste, collapse = "_"),
    ix = row_number()
  ) %>%
  left_join(states_data, by = c("stt" = "stt")) %>%
  mutate(
    reg = factor(reg.lkup[as.numeric(stt)]),
    z_inc = scales::rescale(as.numeric(inc))
  )

# Process election subset
election_subset <- election_processed %>%
  mutate(
    vote = ifelse(vote == 1, 1, 0),
    grp = apply(.[, c("stt", "eth", "inc", "age")], 1, paste, collapse = "_"),
    ones = 1
  )

# Calculate and add weighted results
data_matrix <- calc_and_add_weighted_results(data_matrix, election_subset)

# Build and fit model
reg_formula <- as.formula(build_formula(level = 3))
start.time <- Sys.time()
mod <- stan_glmer(reg_formula, data = data_matrix, family = binomial(link = "logit"), iter = 500)
end.time <- Sys.time()
time.taken <- end.time - start.time

# Generate predictions
pred <- posterior_epred(mod, newdata = data_matrix)

# Poststratification
result <- ipums_data %>%
  mutate(grp = apply(.[, c("stt", "eth", "inc", "age")], 1, paste, collapse = "_")) %>%
  group_by(grp) %>%
  summarise(
    pop2004 = sum(wtd2004, na.rm = TRUE),
    pop2008 = sum(wtd2008, na.rm = TRUE)
  ) %>%
  right_join(data_matrix, by = "grp") %>%
  arrange(ix)

# Plot results
result %>%
  mutate(pred = colMeans(pred)) %>%
  group_by(stt, inc) %>%
  summarise(
    pred_all = sum(pred * pop2008, na.rm = TRUE) / sum(pop2008, na.rm = TRUE),
    pred_white = sum(pred[eth == 1] * pop2008[eth == 1]) / sum(pop2008[eth == 1])
  ) %>%
  ungroup() %>%
  left_join(stt_dict, by = c("stt" = "stt")) %>%
  filter(!state_abbr %in% c("AK", "HI", "DC")) %>%
  left_join(states_data %>% filter(year == 2016) %>% select(state_abbr, rep_votes), by = c("state_abbr" = "state_abbr")) %>%
  mutate(state_name = fct_reorder(state_abbr, rep_votes, .desc = TRUE)) %>%
  ggplot(aes(group = 1)) +
  geom_line(aes(x = inc, y = pred_all, color = "grey")) +
  geom_line(aes(x = inc, y = pred_white, color = "orange")) +
  scale_x_discrete(labels = c("poor", "", "mid", "", "rich")) +
  scale_y_continuous(labels = scales::label_percent(), breaks = seq(0, 1, 0.5)) +
  scale_color_identity() +
  labs(x = "", y = "") +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~state_name, ncol = 7) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(hjust = 0.5, margin = margin(10, 0, 0, 0)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 10),
    legend.position = "none"
  )
