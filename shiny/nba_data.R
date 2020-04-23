library(tidyverse)
library(janitor)
library(rvest)
library(stringr)
library(magrittr)
library(gt)

# load nba data saved in gather.Rmd

load("nba.Rdata")

###### PLOTS

# plotting change in payroll over time

nba_plot_1 <- nba_adjusted %>% 
  ggplot(aes(year, payroll_adjusted / 1000000)) +
  geom_point(aes(text = full_team_name), color = "dark blue", na.rm = TRUE) +
  labs(x = "Year",
       y = "Payroll (in millions, adjusted for inflation)") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  geom_smooth(method = "gam", se = FALSE, color = "gray")

# plotting wins and payroll by season

nba_plot_2 <- nba_adjusted %>% 
  ggplot(aes(payroll_adjusted / 1000000, rs_win_pct)) +
  geom_point(aes(text = full_team_name), na.rm = TRUE) +
  facet_wrap(~ season, scales = "free_x") +
  labs(x = "Payroll (in millions of USD)",
       y = "Regular Season Win Percentage") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", vjust = 0)) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE)

# plotting wins and payroll by team

nba_plot_3 <- nba_adjusted %>% 
  ggplot(aes(payroll_adjusted / 1000000, rs_win_pct, text = year)) +
  geom_point() +
  facet_wrap(~ franchise_id, scales = "free_x") +
  labs(x = "Payroll (in millions of USD)",
       y = "Regular Season Win Percentage") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", vjust = 0)) +
  geom_smooth(method = "lm", se = FALSE)



###### TABLES

# table for cor between payroll and wins by year

nba_year_cor_table <- nba_adjusted %>% 
  group_by(season) %>% 
  summarize(cor = cor(payroll_adjusted, rs_win_pct, use = "complete.obs")) %>%
  mutate(cor = round(cor, digits = 2)) %>% 
  gt() %>%
  tab_header(title = "Payroll and Regular Season Wins",
             subtitle = "For NBA, by season") %>%
  cols_label(season = "Season",
             cor = "Correlation") %>%
  cols_align(columns = "season", align = "left") %>% 
  tab_options(container.height = 700)

# table for cor between payroll and wins by team

nba_team_cor_table <- nba_adjusted %>% 
  group_by(franchise_id) %>% 
  summarize(cor = cor(payroll_adjusted, rs_win_pct, use = "complete.obs")) %>%
  mutate(cor = round(cor, digits = 2)) %>% 
  gt() %>%
  tab_header(title = "Payroll and Regular Season Wins",
             subtitle = "For NBA, by team") %>%
  cols_label(franchise_id = "Franchise",
             cor = "Correlation") %>%
  cols_align(columns = "franchise_id", align = "left") %>% 
  tab_options(container.height = 700)

nba_year_cor_table
