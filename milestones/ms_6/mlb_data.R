library(tidyverse)
library(janitor)
library(rvest)
library(stringr)
library(magrittr)

###### MLB DATA ######

# read in mlb salaries data

mlb_salaries_raw <- read_csv("raw-data/mlb/salaries.csv") %>% 
  clean_names()

# read in mlb team performance data

mlb_teams <- read_csv("raw-data/mlb/teams.csv") %>% 
  clean_names()

# created grouped salary data by team and year

mlb_salaries <- mlb_salaries_raw %>% 
  group_by(team_id, year_id) %>% 
  summarize(payroll = sum(salary,
                          na.rm = TRUE))

# join team performance data with salary data

mlb_teams_salaries <- mlb_salaries %>% 
  right_join(mlb_teams, by = c("year_id", "team_id")) %>% 
  filter(year_id > 1984)

# read in mlb regular season win data

mlb_rs_wins <- read_html("https://www.baseball-reference.com/leagues/MLB/") %>% 
  html_nodes("table") %>% 
  html_table() %>% 
  as.data.frame() %>% 
  na_if("") %>% 
  filter(G != "G")

# tidy the rs win data

mlb_rs_wins %<>% 
  pivot_longer(cols = ARI:WSN,
               names_to = "franch_id", 
               values_to = "rs_wins",
               values_drop_na = TRUE) %>% 
  rename(games_played = G,
         year = Year) %>% 
  mutate(year = as.numeric(year),
         games_played = as.numeric(games_played),
         rs_wins = as.numeric(rs_wins),
         franch_id = gsub("LAA", "ANA", franch_id),
         franch_id = gsub("MIA", "FLA", franch_id),
         franch_id = gsub("TBR", "TBD", franch_id))

# join team performance and salary data with rs win data

mlb_full <- mlb_rs_wins %>% 
  left_join(mlb_teams_salaries, by = c("franch_id", 
                                       "year" = "year_id")) %>% 
  filter(year <= 2016,
         year >= 1985) %>% 
  mutate(rs_win_pct = rs_wins / games_played,
         div_win = ifelse(rank == 1,
                          "Y",
                          "N")) %>% 
  select(year, franch_id, team_id, name, payroll,
         games_played, rs_wins, rs_win_pct,
         lg_id, rank, div_win, lg_win, ws_win)

###### PLOT TIME

mlb_plot <- mlb_full %>% 
  ggplot(aes(payroll / 1000000, 
             round(rs_win_pct, digits = 2))) +
  geom_point() +
  facet_wrap(~year, scales = "free_x") +
  labs(title = "MLB Team Payroll and Regular Season Win Percentage by Season",
       subtitle = "Payroll and win percentage are positively associated
       (Overall correlation coefficient: a somewhat weak 0.217)",
       x = "Payroll (in millions of USD)",
       y = "Regular Season Win Percentage") +
  theme_classic() +
  geom_smooth(method = "lm", se = FALSE)
