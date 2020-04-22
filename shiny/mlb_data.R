library(tidyverse)
library(janitor)
library(rvest)
library(stringr)
library(magrittr)

###### MLB DATA ######

# load mlb data saved in gather.Rmd

load("mlb.Rdata")

###### PLOT TIME

# payroll_adjusted and rs_win_pct by season

mlb_plot_1 <- mlb_adjusted %>% 
  ggplot(aes(payroll_adjusted / 1000000, rs_win_pct)) +
  geom_point(aes(text = name)) +
  facet_wrap(~year, scales = "free_x") +
  labs(x = "Payroll (in millions of USD)",
       y = "Regular Season Win Percentage") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", vjust = 0)) +
  geom_smooth(method = "lm", se = FALSE)

# payroll_adjusted and rs_win_pct by team

mlb_plot_2 <- mlb_adjusted %>% 
  ggplot(aes(payroll_adjusted / 1000000, rs_win_pct, text = year)) +
  geom_point() +
  facet_wrap(~ franchise_id, scales = "free_x") +
  labs(x = "Payroll (in millions of USD)",
       y = "Regular Season Win Percentage") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", vjust = 0)) +
  geom_smooth(method = "lm", se = FALSE)

mlb_adjusted %>% 
  ggplot(aes(year, payroll_adjusted / 1000000, color = franch_id)) +
  geom_point()

mlb_plot_1

ggplotly(mlb_plot_2, tooltip = "text")
