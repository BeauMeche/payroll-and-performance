# NHL data

load("data-files/nhl.Rdata")

nhl_adjusted %>% 
  ggplot(aes(payroll_adjusted, pts)) +
  geom_point(aes(color = playoffs)) +
  geom_smooth(method = "lm") +
  facet_wrap(~ season, scales = "free")
