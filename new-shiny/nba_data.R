# load necessary packages

library(tidyverse)
library(stringr)
library(gt)
library(broom)

# load nba data (saved from gather.Rmd and moved to shiny directory)

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
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
  scale_y_continuous(limits = c(.2, .8), breaks = c(.3, .5, .7))

# plotting wins and payroll_rank by team

nba_plot_3 <- nba_adjusted %>% 
  ggplot(aes(payroll_rank, rs_win_pct, text = year)) +
  geom_point() +
  facet_wrap(~ franchise_id, scales = "free_x") +
  labs(x = "Payroll Rank",
       y = "Regular Season Win Percentage") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(limits = c(.2, .8), breaks = c(.3, .5, .7))




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
  tab_options(container.height = 535) %>% 
  tab_header(title = "Payroll and Regular Season Performance",
             subtitle = "Strength of Relationship by Season")

# neat summary table for by-year correlations

nba_year_cor_sum <- nba_adjusted %>% 
  group_by(season) %>% 
  summarize(cor = cor(payroll_adjusted, rs_win_pct, use = "complete.obs")) %>% 
  mutate(Max = max(cor),
         Min = min(cor),
         Mean = mean(cor)) %>% 
  filter(Max == cor | Min == cor) %>% 
  pivot_longer(cols = c("Max", "Min", "Mean"), names_to = "type") %>% 
  filter(value == cor | type == "Mean")  %>% 
  mutate(season = ifelse(type == "Mean",
                         "",
                         as.character(season))) %>% 
  select(type, value, season) %>% 
  arrange(desc(value)) %>% 
  distinct() %>% 
  mutate(value = round(value, digits = 2)) %>% 
  gt() %>% 
  cols_label(type = "",
             value = md("**Correlation**"),
             season = md("**Season**")) %>% 
  cols_align(columns = "value", align = "center") %>% 
  tab_header(title = "Summary")

# table for cor between payroll and wins by team

nba_team_cor_table <- nba_adjusted %>% 
  group_by(franchise_id) %>% 
  summarize(cor = cor(payroll_rank, rs_win_pct, use = "complete.obs")) %>%
  mutate(cor = round(cor, digits = 2)) %>% 
  gt() %>%
  cols_label(franchise_id = md("**Franchise**"),
             cor = md("**Correlation Coefficient**")) %>%
  cols_align(columns = "franchise_id", align = "left") %>% 
  tab_options(container.height = 535) %>% 
  tab_header(title = "Payroll and Regular Season Performance",
             subtitle = "Strength of Relationship by Team")

# create summary table for by-team correlations

nba_top_3 <- nba_adjusted %>% 
  group_by(franchise_id) %>% 
  summarize(cor = cor(payroll_rank, rs_win_pct, use = "complete.obs")) %>%
  top_n(3, cor) %>% 
  arrange(desc(cor)) %>% 
  mutate(group = "Top 3")

nba_team_cor_mean <- nba_adjusted %>% 
  group_by(franchise_id) %>% 
  summarize(cor = cor(payroll_rank, rs_win_pct, use = "complete.obs")) %>%
  summarize(cor = mean(cor)) %>% 
  mutate(franchise_id = "Overall mean",
         group = "") %>% 
  select(franchise_id, cor, group)
  
nba_bottom_3 <- nba_adjusted %>% 
  group_by(franchise_id) %>% 
  summarize(cor = cor(payroll_rank, rs_win_pct, use = "complete.obs")) %>%
  top_n(3, desc(cor)) %>% 
  arrange(desc(cor)) %>% 
  mutate(group = "Bottom 3")

nba_team_cor_sum <- bind_rows(nba_team_cor_mean, nba_top_3, nba_bottom_3) %>% 
  mutate(cor = round(cor, digits = 2)) %>% 
  gt(groupname_col = "group") %>% 
  cols_align(columns = "franchise_id", align = "left") %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_row_groups(groups = c("Top 3", "Bottom 3"))) %>% 
  tab_header(title = "Summary") %>% 
  cols_label(franchise_id = md("**Franchise**"),
             cor = md("**Correlation Coefficient**"))


###### MODELS

# assign interaction model for payroll_rank and franchise_id

nba_mod_team <- lm(rs_win_pct * 100 ~ payroll_rank * franchise_id, nba_adjusted)

# tidy the model and select desired variables

nba_mod_team_tidy <- nba_mod_team %>% 
  tidy(conf.int = TRUE) %>% 
  select(term, estimate, conf.low, conf.high)

# keep only slope coefficients, and add variables showing slope (not just
# offset) for each team

nba_mod_team_effect <- nba_mod_team_tidy %>%  
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

nba_mod_plot_1 <- nba_mod_team_effect %>% 
  mutate(term = ifelse(term == "payroll_rank",
                       "76ers",
                       str_remove(term, "payroll_rank:franchise_id")),
         term = fct_reorder(term, desc(effect), .fun = "median")) %>% 
  ggplot(aes(term, effect, text = paste(term, 
                                        as.character(round(effect, digits = 3)),
                                        sep = ": "))) +
  geom_point(color = "dark blue") +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                color = "dark blue", width = .75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_classic() +
  labs(x = "",
       y = "Effect") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_text(face = "bold"))



# assign interaction model for payroll_adjusted and season

nba_mod_year <- nba_adjusted %>% 
  mutate(payroll_adjusted = payroll_adjusted / 1000000) %>% 
  lm(rs_win_pct * 100 ~ payroll_adjusted * season, data = .)

# tidy the model and select desired variables

nba_mod_year_tidy <- nba_mod_year %>% 
  tidy(conf.int = TRUE) %>% 
  select(term, estimate, conf.low, conf.high)

# keep only slope coefficients, and add variables showing slope (not just
# offset) for each team

nba_mod_year_effect <- nba_mod_year_tidy %>%  
  filter(str_detect(term, "payroll_adjusted")) %>% 
  mutate(effect = ifelse(term == "payroll_adjusted",
                         estimate,
                         estimate + filter(., term == "payroll_adjusted") %>% 
                           pull(estimate)),
         lower = ifelse(term == "payroll_adjusted",
                        conf.low,
                        conf.low + filter(., term == "payroll_adjusted") %>% 
                          pull(estimate)),
         upper = ifelse(term == "payroll_adjusted",
                        conf.high,
                        conf.high + filter(., term == "payroll_adjusted") %>% 
                          pull(estimate)))

# mutate to clean names for plot display, then plot

nba_mod_plot_2 <- nba_mod_year_effect %>% 
  mutate(term = ifelse(term == "payroll_adjusted",
                       "1984-85",
                       str_remove(term, "payroll_adjusted:season"))) %>% 
  ggplot(aes(term, effect, text = paste(term, 
                                        as.character(round(effect, digits = 3)),
                                        sep = ": "))) +
  geom_point(color = "dark blue") +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                color = "dark blue", width = .75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_classic() +
  labs(x = "",
       y = "Effect") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_text(face = "bold"))


