# loading necessary packages

library(tidyverse)
library(stringr)
library(gt)
library(broom)

# load mlb data (saved from gather.Rmd and moved to shiny directory)

load("data-files/mlb.Rdata")


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
  scale_y_continuous(breaks = seq(.3, .7, .2)) +
  labs(x = "Payroll (in millions of USD, adjusted)",
       y = "Regular Season Win Percentage") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", vjust = 0)) +
  geom_smooth(method = "lm", se = FALSE)


# payroll_rank and rs_win_pct by team

mlb_plot_3 <- mlb_adjusted %>% 
  ggplot(aes(payroll_rank, rs_win_pct, text = year)) +
  geom_point() +
  facet_wrap(~ franchise_id, scales = "free_x") +
  scale_y_continuous(breaks = seq(.3, .7, .2)) +
  labs(x = "Payroll Rank",
       y = "Regular Season Win Percentage") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", vjust = 0)) +
  geom_smooth(method = "lm", se = FALSE)




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
  tab_options(container.height = 535) %>% 
  tab_header(title = "Payroll and Regular Season Performance",
             subtitle = "Strength of Relationship by Year")

# neat summary table for by-year correlations

mlb_year_cor_sum <- mlb_adjusted %>% 
  group_by(year) %>% 
  summarize(cor = cor(payroll_adjusted, rs_win_pct)) %>% 
  mutate(Max = max(cor),
         Min = min(cor),
         Mean = mean(cor)) %>% 
  filter(Max == cor | Min == cor) %>% 
  pivot_longer(cols = c("Max", "Min", "Mean"), names_to = "type") %>% 
  filter(value == cor | type == "Mean") %>%
  mutate(year = ifelse(type == "Mean",
                         "",
                         year)) %>% 
  select(type, value, year) %>% 
  arrange(desc(value)) %>% 
  distinct() %>% 
  mutate(value = round(value, digits = 2)) %>% 
  gt() %>% 
  cols_label(type = "",
             value = md("**Correlation**"),
             year = md("**Year**")) %>% 
  cols_align(columns = "value", align = "center") %>% 
  tab_header(title = "Summary")

# table for cor between payroll rank and wins by team

mlb_team_cor_table <- mlb_adjusted %>% 
  group_by(franchise_id) %>% 
  summarize(cor = cor(payroll_rank, rs_win_pct)) %>%
  mutate(cor = round(cor, digits = 2)) %>% 
  gt() %>%
  cols_label(franchise_id = md("**Franchise**"),
             cor = md("**Correlation Coefficient**")) %>%
  cols_align(columns = "franchise_id", align = "left") %>% 
  tab_options(container.height = 535) %>% 
  tab_header(title = "Payroll and Regular Season Performance",
             subtitle = "Strength of Relationship by Team")

# create summary table for by-team correlations

mlb_top_3 <- mlb_adjusted %>% 
  group_by(franchise_id) %>% 
  summarize(cor = cor(payroll_rank, rs_win_pct)) %>%
  top_n(3, cor) %>% 
  arrange(desc(cor)) %>% 
  mutate(group = "Top 3")

mlb_team_cor_mean <- mlb_adjusted %>% 
  group_by(franchise_id) %>% 
  summarize(cor = cor(payroll_rank, rs_win_pct)) %>%
  summarize(cor = mean(cor)) %>% 
  mutate(franchise_id = "Overall mean",
         group = "") %>% 
  select(franchise_id, cor, group)

mlb_bottom_3 <- mlb_adjusted %>% 
  group_by(franchise_id) %>% 
  summarize(cor = cor(payroll_rank, rs_win_pct)) %>%
  top_n(3, desc(cor)) %>% 
  arrange(desc(cor)) %>% 
  mutate(group = "Bottom 3")

mlb_team_cor_sum <- bind_rows(mlb_team_cor_mean, mlb_top_3, mlb_bottom_3) %>% 
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

mlb_mod_team <- lm(rs_win_pct * 100 ~ payroll_rank * franchise_id, mlb_adjusted)

# tidy the model and select desired variables

mlb_mod_team_tidy <- mlb_mod_team %>% 
  tidy(conf.int = TRUE) %>% 
  select(term, estimate, conf.low, conf.high)

# keep only slope coefficients, and add variables showing slope (not just
# offset) for each team

mlb_mod_team_effect <- mlb_mod_team_tidy %>%  
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

mlb_mod_plot_1 <- mlb_mod_team_effect %>% 
  mutate(term = ifelse(term == "payroll_rank",
                       "ANA",
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



# assign interaction model for payroll_adjusted and year
# have to ungroup() in order to modify year
# mutating to make the units of payroll_adjusted millions and to treat year as a
# character/factor in the model (not as a numeric)

mlb_mod_year <- mlb_adjusted %>% 
  ungroup() %>% 
  mutate(payroll_adjusted = payroll_adjusted / 1000000,
         year = as.character(year)) %>% 
  lm(rs_win_pct * 100 ~ payroll_adjusted * year, data = .)

# tidy the model and select desired variables

mlb_mod_year_tidy <- mlb_mod_year %>% 
  tidy(conf.int = TRUE) %>% 
  select(term, estimate, conf.low, conf.high)

# keep only slope coefficients, and add variables showing slope (not just
# offset) for each team

mlb_mod_year_effect <- mlb_mod_year_tidy %>%  
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

mlb_mod_plot_2 <- mlb_mod_year_effect %>% 
  mutate(term = ifelse(term == "payroll_adjusted",
                       "1985",
                       str_remove(term, 
                                  "payroll_adjusted:year"))) %>% 
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
