library(tidyverse)
library(stringr)
library(gt)
library(broom)

# load nba data saved in gather.Rmd

load("data-files/nba.Rdata")


###### PLOTS

# plotting change in payroll over time

nba_plot_1 <- nba_adjusted %>% 
  ggplot(aes(year, payroll_adjusted / 1000000)) +
  geom_point(aes(text = full_team_name), color = "dark blue", na.rm = TRUE) +
  labs(x = "Year",
       y = "Payroll (in millions, adjusted)") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  scale_x_continuous(breaks = seq(1985, 2015, 5)) +
  geom_smooth(method = "gam", se = FALSE, color = "gray")


# plotting wins and payroll by season

nba_plot_2 <- nba_adjusted %>% 
  ggplot(aes(payroll_adjusted / 1000000, rs_win_pct)) +
  geom_point(aes(text = full_team_name), na.rm = TRUE) +
  facet_wrap(~ season, scales = "free_x") +
  labs(x = "Payroll (in millions of USD, adjusted)",
       y = "Regular Season Win Percentage") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust = 1)) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE)


# plotting wins and payroll_rank by team

nba_plot_3 <- nba_adjusted %>% 
  ggplot(aes(payroll_rank, rs_win_pct, text = year)) +
  geom_point() +
  facet_wrap(~ franchise_id, scales = "free_x") +
  labs(x = "Payroll Rank",
       y = "Regular Season Win Percentage") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  geom_smooth(method = "lm", se = FALSE)




###### TABLES

# table for cor between payroll and wins by season

nba_year_cor_table <- nba_adjusted %>% 
  group_by(season) %>% 
  summarize(cor = cor(payroll_adjusted, rs_win_pct, use = "complete.obs")) %>%
  mutate(cor = round(cor, digits = 2)) %>% 
  gt() %>%
  cols_label(season = md("**Season**"),
             cor = md("**Correlation Coefficient**")) %>%
  cols_align(columns = "season", align = "left") %>% 
  tab_options(container.height = 650)


# table for cor between payroll and wins by team

nba_team_cor_table <- nba_adjusted %>% 
  group_by(franchise_id) %>% 
  summarize(cor = cor(payroll_rank, rs_win_pct, use = "complete.obs")) %>%
  mutate(cor = round(cor, digits = 2)) %>% 
  gt() %>%
  cols_label(franchise_id = md("**Franchise**"),
             cor = md("**Correlation Coefficient**")) %>%
  cols_align(columns = "franchise_id", align = "left") %>% 
  tab_options(container.height = 750)




###### MODELING

# assign interaction model for payroll_rank and franchise_id

nba_mod_team <- lm(rs_win_pct * 100 ~ payroll_rank * franchise_id, nba_adjusted)

# tidy the model and select desired variables

nba_mod_team_tidy <- nba_mod_team %>% 
  tidy(conf.int = TRUE) %>% 
  select(term, estimate, conf.low, conf.high)

# keep only slope coefficients, and add variables showing slope (not just
# offset) for each team

nba_mod_team_tidy_effect <- nba_mod_team_tidy %>%  
  filter(str_detect(term, "payroll_rank")) %>% 
  mutate(effect = ifelse(term == "payroll_rank",
                         estimate,
                         estimate + filter(., term == "payroll_rank") %>% 
                           pull(estimate)),
         lower = ifelse(term == "payroll_rank",
                        conf.low,
                        conf.low + filter(., term == "payroll_rank") %>% 
                          pull(estimate)),
         upper = ifelse(term == "payroll_rank",
                        conf.high,
                        conf.high + filter(., term == "payroll_rank") %>% 
                          pull(estimate)))


# mutate to clean names for plot display, then plot

g <- nba_mod_team_tidy_effect %>% 
  mutate(term = ifelse(term == "payroll_rank",
                       "76ers",
                       str_remove(term, "payroll_rank:franchise_id")),
         term = fct_reorder(term, desc(effect), .fun = "median")) %>% 
  ggplot(aes(term, effect, text = paste(term, 
                                        as.character(round(effect, digits = 3)),
                                        sep = ": "))) +
  geom_point(color = "dark blue") +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                color = "dark blue", width = .2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  theme_classic() +
  labs(title = "Effect of Moving One Spot Up in Payroll Rank 
on Regular Season Win Percentage",
       x = "",
       y = "Effect of 1 means a 1 point increase in win percentage
Error bars show 95% confidence interval")
  

