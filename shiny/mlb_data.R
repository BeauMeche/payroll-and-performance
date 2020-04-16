library(tidyverse)
library(janitor)
library(rvest)
library(stringr)
library(magrittr)

###### MLB DATA ######

# load mlb data saved in gather.Rmd

load("mlb.Rdata")

###### PLOT TIME

mlb_plot <- mlb_adjusted %>% 
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
