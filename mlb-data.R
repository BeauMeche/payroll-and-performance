library(readxl)
library(tidyverse)

salaries <- read.csv("Salaries.csv")

glimpse(salaries)
summary(salaries)

tail(salaries)

teams <- read.csv("Teams.csv")

glimpse(teams)

summary(teams)

teams

# Data is from here: http://www.seanlahman.com/baseball-archive/statistics/

# Somebody who did something similar to me:
# https://courses.cs.washington.edu/courses/cse140/13wi/projects/mirae-report.pdf

summary(salaries)

by_team_and_year <- salaries %>% 
  group_by(teamID, yearID) %>% 
  summarize(payroll = sum(salary,
                          na.rm = TRUE)) %>% 
  arrange(yearID)
 
by_team_and_year %>%   
   ggplot(aes(yearID,
             payroll)) +
  geom_line() +
  facet_wrap(~ teamID)

View(salaries)

salaries %>% 
  filter(teamID == "LAN")


install.packages("blscrapeR")



# Will want to adjust all dollar values for inflation

# Could look into big/sudden changes in team spending (e.g. LAN in ~2010) to see
# if the money had an impact

# Will want to see if I can find data on total wins for the applicable time
# window, not just on division or league or WS wins

# Money spent is independent variable and wins/success is dependent variable;
# trying to find out if money itself drives success, or if they tend to
# fluctuate together (e.g. because teams that do well sell more tickets and get
# more revenue and can then spend more to keep doing well)

# Can scrape https://hoopshype.com/salaries/ for NBA data from 90/91 - 2019/20

# Or, alternatively, can try downloading dataframes from 
# https://data.world/datadavis/nba-salaries

# Could also try scraping NHL data from \
# https://en.wikipedia.org/wiki/List_of_team_payrolls_in_the_NHL
# (small sample size though)

