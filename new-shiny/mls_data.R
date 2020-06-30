# MLS data

load("data-files/mls.Rdata")



###### PLOTS

# plotting change in payroll over time

mls_plot_1 <- mls_adjusted %>% 
  ggplot(aes(year, payroll_adjusted / 1000000)) +
  geom_point(aes(text = club), color = "dark blue") +
  labs(x = "Year",
       y = "Payroll (in millions, adjusted)") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  geom_smooth(method = "gam", se = FALSE, color = "gray") + 
  scale_x_continuous(breaks = seq(from = 2007, to = 2019, by = 2))
  
# plotting performance and payroll by season

mls_plot_2 <- mls_adjusted %>% 
  ggplot(aes(payroll_adjusted / 1000000, pts)) +
  geom_point(aes(text = club)) +
  facet_wrap(~ year, scales = "free_x") +
  labs(x = "Payroll (in millions of USD, adjusted)",
       y = "Regular Season Points") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust = 1)) +
  geom_smooth(method = "lm", se = FALSE)

# plotting performance and payroll_rank by team

mls_plot_3 <- mls_adjusted %>% 
  ggplot(aes(payroll_rank, pts, text = year)) +
  geom_point() +
  facet_wrap(~ club, scales = "free_x") +
  labs(x = "Payroll Rank",
       y = "Regular Season Points") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  geom_smooth(method = "lm", se = FALSE)



###### TABLES

# table for cor between payroll and points by season

mls_year_cor_table <- mls_adjusted %>% 
  group_by(year) %>% 
  summarize(cor = cor(payroll_adjusted, pts)) %>%
  mutate(cor = round(cor, digits = 2)) %>% 
  gt() %>%
  cols_label(year = md("**Year**"),
             cor = md("**Correlation Coefficient**")) %>%
  cols_align(columns = "year", align = "left") %>% 
  tab_options(container.height = 535) %>% 
  tab_header(title = "Payroll and Regular Season Performance",
             subtitle = "Strength of Relationship by Year")

# neat summary table for by-season correlations

mls_year_cor_sum <- mls_adjusted %>% 
  group_by(year) %>% 
  summarize(cor = cor(payroll_adjusted, pts)) %>% 
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

# table for cor between payroll and points by team

mls_team_cor_table <- mls_adjusted %>% 
  group_by(club) %>% 
  summarize(cor = cor(payroll_rank, pts)) %>%
  mutate(cor = round(cor, digits = 2)) %>% 
  gt() %>%
  cols_label(club = md("**Club**"),
             cor = md("**Correlation Coefficient**")) %>%
  cols_align(columns = "club", align = "left") %>% 
  tab_options(container.height = 535) %>% 
  tab_header(title = "Payroll and Regular Season Performance",
             subtitle = "Strength of Relationship by Team")

# create summary table for by-team correlations

mls_top_3 <- mls_adjusted %>% 
  group_by(club) %>% 
  filter(n() > 4) %>% 
  summarize(cor = cor(payroll_rank, pts)) %>%
  top_n(3, cor) %>% 
  arrange(desc(cor)) %>% 
  mutate(group = "Top 3")

mls_team_cor_mean <- mls_adjusted %>% 
  group_by(club) %>% 
  filter(n() > 4) %>% 
  summarize(cor = cor(payroll_rank, pts)) %>% 
  summarize(cor = mean(cor, na.rm = TRUE)) %>% 
  mutate(club = "Overall mean",
         group = "") %>% 
  select(club, cor, group)

mls_bottom_3 <- mls_adjusted %>% 
  group_by(club) %>% 
  filter(n() > 4) %>% 
  summarize(cor = cor(payroll_rank, pts)) %>%
  top_n(3, desc(cor)) %>% 
  arrange(desc(cor)) %>% 
  mutate(group = "Bottom 3")

mls_team_cor_sum <- bind_rows(mls_team_cor_mean, mls_top_3, mls_bottom_3) %>% 
  mutate(cor = round(cor, digits = 2)) %>% 
  gt(groupname_col = "group") %>% 
  cols_align(columns = "club", align = "left") %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_row_groups(groups = c("Top 3", "Bottom 3"))) %>% 
  tab_header(title = "Summary") %>% 
  cols_label(club = md("**Team**"),
             cor = md("**Correlation Coefficient**")) %>% 
  tab_footnote(footnote = "Considering only teams with at least 5 seasons of 
               data",
               locations = cells_title("title")) 



###### MODELS

# assign interaction model for payroll_rank and team

mls_mod_team <- lm(pts ~ payroll_rank * club, mls_adjusted)

# tidy the model and select desired variables

mls_mod_team_tidy <- mls_mod_team %>% 
  tidy(conf.int = TRUE) %>% 
  select(term, estimate, conf.low, conf.high)

# keep only slope coefficients, and add variables showing slope (not just
# offset) for each team

mls_mod_team_effect <- mls_mod_team_tidy %>%  
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

mls_mod_plot_1 <- mls_mod_team_effect %>% 
  mutate(term = ifelse(term == "payroll_rank",
                       "Atlanta United FC",
                       str_remove(term, "payroll_rank:club")),
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
  theme(axis.text.x = element_text(angle = 65, hjust = 1, vjust = 1),
        axis.title = element_text(face = "bold"))

# assign interaction model for payroll_adjusted and season

mls_mod_year <- mls_adjusted %>% 
  ungroup() %>% 
  mutate(payroll_adjusted = payroll_adjusted / 1000000,
         year = as.character(year)) %>% 
  lm(pts ~ payroll_adjusted * year, data = .)

# tidy the model and select desired variables

mls_mod_year_tidy <- mls_mod_year %>% 
  tidy(conf.int = TRUE) %>% 
  select(term, estimate, conf.low, conf.high)

# keep only slope coefficients, and add variables showing slope (not just
# offset) for each team

mls_mod_year_effect <- mls_mod_year_tidy %>%  
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

mls_mod_plot_2 <- mls_mod_year_effect %>% 
  mutate(
    term = ifelse(term == "payroll_adjusted",
                  "2007",
                  str_remove(term, "payroll_adjusted:year"))) %>% 
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
