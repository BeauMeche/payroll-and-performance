# NHL data

load("data-files/nhl.Rdata")



###### PLOTS

# plotting change in payroll over time

# jitter() command is necessary to allow geom_smooth() to work with "gam", but
# amount of jitter is minimized to avoid appearance of jittering

nhl_plot_1 <- nhl_adjusted %>% 
  ggplot(aes(jitter(year, amount = .000000001), payroll_adjusted / 1000000)) +
  geom_point(aes(text = team), color = "dark blue") +
  labs(x = "Year",
       y = "Payroll (in millions, adjusted)") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  geom_smooth(method = "gam", se = FALSE, color = "gray") +
  geom_vline(xintercept = 2005) +
  annotate("text", 
           x = 2005.2, 
           y = 60,
           label = "2005 season cancelled due to lockout",
           angle = -90)

# plotting performance and payroll by season

nhl_plot_2 <- nhl_adjusted %>% 
  ggplot(aes(payroll_adjusted / 1000000, pts)) +
  geom_point(aes(text = team)) +
  facet_wrap(~ season, scales = "free_x") +
  labs(x = "Payroll (in millions of USD, adjusted)",
       y = "Regular Season Points") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust = 1)) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE)

# plotting performance and payroll_rank by team

nhl_plot_3 <- nhl_adjusted %>% 
  ggplot(aes(payroll_rank, pts, text = year)) +
  geom_point() +
  facet_wrap(~ team, scales = "free_x") +
  labs(x = "Payroll Rank",
       y = "Regular Season Points") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  geom_smooth(method = "lm", se = FALSE)



###### TABLES

# table for cor between payroll and points by season

nhl_year_cor_table <- nhl_adjusted %>% 
  group_by(season) %>% 
  summarize(cor = cor(payroll_adjusted, pts)) %>%
  mutate(cor = round(cor, digits = 2)) %>% 
  gt() %>%
  cols_label(season = md("**Season**"),
             cor = md("**Correlation Coefficient**")) %>%
  cols_align(columns = "season", align = "left") %>% 
  tab_options(container.height = 350)

# table for cor between payroll and points by team

nhl_team_cor_table <- nhl_adjusted %>% 
  group_by(team) %>% 
  summarize(cor = cor(payroll_rank, pts)) %>%
  mutate(cor = round(cor, digits = 2)) %>% 
  gt() %>%
  cols_label(team = md("**Team**"),
             cor = md("**Correlation Coefficient**")) %>%
  cols_align(columns = "team", align = "left") %>% 
  tab_options(container.height = 300)



###### MODELS

# assign interaction model for payroll_rank and team

nhl_mod_team <- lm(pts ~ payroll_rank * team, nhl_adjusted)

# tidy the model and select desired variables

nhl_mod_team_tidy <- nhl_mod_team %>% 
  tidy(conf.int = TRUE) %>% 
  select(term, estimate, conf.low, conf.high)

# keep only slope coefficients, and add variables showing slope (not just
# offset) for each team

nhl_mod_team_effect <- nhl_mod_team_tidy %>%  
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

nhl_mod_plot_1 <- nhl_mod_team_effect %>% 
  mutate(term = ifelse(term == "payroll_rank",
                       "Anaheim Ducks",
                       str_remove(term, "payroll_rank:team")),
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

nhl_mod_year <- nhl_adjusted %>% 
  mutate(payroll_adjusted = payroll_adjusted / 1000000) %>% 
  lm(pts ~ payroll_adjusted * season, data = .)

# tidy the model and select desired variables

nhl_mod_year_tidy <- nhl_mod_year %>% 
  tidy(conf.int = TRUE) %>% 
  select(term, estimate, conf.low, conf.high)

# keep only slope coefficients, and add variables showing slope (not just
# offset) for each team

nhl_mod_year_effect <- nhl_mod_year_tidy %>%  
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

nhl_mod_plot_2 <- nhl_mod_year_effect %>% 
  mutate(term = ifelse(term == "payroll_adjusted",
                       "1998-99",
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
