# MLS data

load("data-files/mls.Rdata")

mls_adjusted %>% 
  ggplot(aes(payroll_adjusted, pts)) +
  geom_point(aes(color = playoffs)) +
  facet_wrap(~ year, scales = "free")
