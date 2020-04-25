library(tidyverse)
library(janitor)
library(rvest)
library(stringr)
library(magrittr)

###### MLB DATA ######

# load mlb data saved in gather.Rmd

load("mlb.Rdata")

###### PLOT TIME

# plotting change in payroll over time

mlb_plot_1 <- mlb_adjusted %>% 
  ggplot(aes(year, payroll_adjusted / 1000000)) +
  geom_point(aes(text = name), color = "dark blue", na.rm = TRUE) +
  labs(x = "Year",
       y = "Payroll (in millions, adjusted)") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  scale_x_continuous(breaks = seq(1985, 2015, 5)) +
  geom_smooth(method = "gam", se = FALSE, color = "gray")

# payroll_adjusted and rs_win_pct by season

mlb_plot_2 <- mlb_adjusted %>% 
  ggplot(aes(payroll_adjusted / 1000000, rs_win_pct)) +
  geom_point(aes(text = name)) +
  facet_wrap(~year, scales = "free_x") +
  labs(x = "Payroll (in millions of USD, adjusted)",
       y = "Regular Season Win Percentage") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", vjust = 0)) +
  geom_smooth(method = "lm", se = FALSE)

mlb_plot_2

# payroll_rank and rs_win_pct by team

mlb_plot_3 <- mlb_adjusted %>% 
  ggplot(aes(payroll_rank, rs_win_pct, text = year)) +
  geom_point() +
  facet_wrap(~ franchise_id, scales = "free_x") +
  labs(x = "Payroll Rank",
       y = "Regular Season Win Percentage") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", vjust = 0)) +
  geom_smooth(method = "lm", se = FALSE)

mlb_plot_3

###### TABLES

# table for cor between payroll and wins by year

mlb_year_cor_table <- mlb_adjusted %>% 
  group_by(year) %>% 
  summarize(cor = cor(payroll_adjusted, rs_win_pct)) %>%
  mutate(cor = round(cor, digits = 2)) %>% 
  gt() %>%
  cols_label(year = md("**Year**"),
             cor = md("**Correlation Coefficient**")) %>%
  cols_align(columns = "year", align = "left") %>% 
  tab_options(container.height = 650)

# table for cor between payroll rank and wins by team

mlb_team_cor_table <- mlb_adjusted %>% 
  group_by(franchise_id) %>% 
  summarize(cor = cor(payroll_rank, rs_win_pct)) %>%
  mutate(cor = round(cor, digits = 2)) %>% 
  gt() %>%
  cols_label(franchise_id = md("**Franchise**"),
             cor = md("**Correlation Coefficient**")) %>%
  cols_align(columns = "franchise_id", align = "left") %>% 
  tab_options(container.height = 750)

mlb_team_cor_table

