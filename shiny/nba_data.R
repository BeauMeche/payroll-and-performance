library(tidyverse)
library(janitor)
library(rvest)
library(stringr)
library(magrittr)

# load nba data saved in gather.Rmd

load("nba.Rdata")

###### PLOT TIME

nba_plot <- nba_adjusted %>% 
  ggplot(aes(payroll / 1000000, rs_win_pct)) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~ season, scales = "free_x") +
  theme_classic() +
  labs(title = "NBA Team Payroll and Regular Season Win Percentage by Season",
       subtitle = "Payroll and win percentage are positively associated
       (Overall correlation coefficient: a weak 0.089)",
       x = "Payroll (in millions of USD)",
       y = "Regular Season Win Percentage") +
  geom_smooth(method = "lm", se = FALSE)
