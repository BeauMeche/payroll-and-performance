library(tidyverse)
library(janitor)
library(rvest)
library(stringr)
library(magrittr)

# loading nba salary data (1984-85 to 2017-18)

nba_salaries_raw <- read_csv("raw-data/nba/salaries_1985to2018.csv") %>% 
  clean_names()

# creating nba salary data grouped by team and season

nba_salaries <- nba_salaries_raw %>% 
  group_by(team, season) %>% 
  summarize(team_salary = sum(salary))

# creating function to load and clean data on team rs wins and postseason
# performance

team_season_data <- function(team_url, franchise_id){
  read_html(team_url) %>% 
    html_nodes("table") %>% 
    .[2] %>% 
    html_table() %>% 
    as.data.frame() %>% 
    slice(2, 5:38) %>% 
    row_to_names(1) %>% 
    clean_names() %>% 
    rename(conference = standing_2) %>% 
    rename(rs_win_pct = percent) %>% 
    rename(playoff_win_pct = percent_2) %>% 
    separate(w_l, into = c("wins", "losses"), sep = "-") %>% 
    separate(w_l_2, into = c("playoff_wins", "playoff_losses"), sep = "-") %>% 
    mutate(lockout = ifelse(season %>% endsWith(" *"), TRUE, FALSE)) %>% 
    mutate(season = ifelse(season %>% endsWith(" *"), 
                           season %>% substr(start = 1, stop = 7),
                           season)) %>% 
    
    # pattern from here: http://stla.github.io/stlapblog/posts/Numextract.html    
    
    mutate(standing = standing %>% str_extract("\\-*\\d+\\.*\\d*")) %>% 
    rename(conf_standing = standing) %>% 
    mutate(wins = as.numeric(wins),
           losses = as.numeric(losses),
           rs_win_pct = as.numeric(rs_win_pct),
           conf_standing = as.numeric(conf_standing),
           playoff_wins = as.numeric(playoff_wins),
           playoff_losses = as.numeric(playoff_losses),
           playoff_win_pct = as.numeric(playoff_win_pct))%>% 
    mutate(franchise_id = franchise_id)
}

# loading rs win and postseason performance data for all 30 teams

hawks <- team_season_data(
  "https://www.landofbasketball.com/teams/records_atlanta_hawks.htm",
  "Hawks"
)

celtics <- team_season_data(
  "https://www.landofbasketball.com/teams/records_boston_celtics.htm",
  "Celtics"
)

nets <- team_season_data(
  "https://www.landofbasketball.com/teams/records_brooklyn_nets.htm",
  "Nets"
)

# hornets only have 28 rows; started in 1988-89, missing 2002-03 and 2003-04

hornets <- team_season_data(
  "https://www.landofbasketball.com/teams/records_charlotte_hornets.htm",
  "Hornets/Bobcats"
)

bulls <- team_season_data(
  "https://www.landofbasketball.com/teams/records_chicago_bulls.htm",
  "Bulls"
)

cavs <- team_season_data(
  "https://www.landofbasketball.com/teams/records_cleveland_cavaliers.htm",
  "Cavaliers"
)

mavs <- team_season_data(
  "https://www.landofbasketball.com/teams/records_dallas_mavericks.htm",
  "Mavericks"
)

nuggets <- team_season_data(
  "https://www.landofbasketball.com/teams/records_denver_nuggets.htm",
  "Nuggets"
)

pistons <- team_season_data(
  "https://www.landofbasketball.com/teams/records_detroit_pistons.htm",
  "Pistons"
)

warriors <- team_season_data(
  "https://www.landofbasketball.com/teams/records_golden_state_warriors.htm",
  "Warriors"
)

rockets <- team_season_data(
  "https://www.landofbasketball.com/teams/records_houston_rockets.htm",
  "Rockets"
)

pacers <- team_season_data(
  "https://www.landofbasketball.com/teams/records_indiana_pacers.htm",
  "Pacers"
)

clippers <- team_season_data(
  "https://www.landofbasketball.com/teams/records_los_angeles_clippers.htm",
  "Clippers"
)

lakers <- team_season_data(
  "https://www.landofbasketball.com/teams/records_los_angeles_lakers.htm",
  "Lakers"
)

# grizzlies only have 23 rows; started in 1995-96

grizzlies <- team_season_data(
  "https://www.landofbasketball.com/teams/records_memphis_grizzlies.htm",
  "Grizzlies"
)

# heat only have 30 rows; started in 1988-89

heat <- team_season_data(
  "https://www.landofbasketball.com/teams/records_miami_heat.htm",
  "Heat"
)

bucks <- team_season_data(
  "https://www.landofbasketball.com/teams/records_milwaukee_bucks.htm",
  "Bucks"
)

# twolves only have 29 rows; started in 1989-90

twolves <- team_season_data(
  "https://www.landofbasketball.com/teams/records_minnesota_timberwolves.htm",
  "Timberwolves"
)

# pelicans only have 16 rows; started in 2002-03

pelicans <- team_season_data(
  "https://www.landofbasketball.com/teams/records_new_orleans_pelicans.htm",
  "Pelicans/Hornets"
)

knicks <- team_season_data(
  "https://www.landofbasketball.com/teams/records_new_york_knicks.htm",
  "Knicks"
)

thunder <- team_season_data(
  "https://www.landofbasketball.com/teams/records_oklahoma_city_thunder.htm",
  "Thunder/Supersonics"
) %>% 
  mutate(team = gsub("Supersonics", "SuperSonics", team))

# magic only have 29 rows; started in 1989-90

magic <- team_season_data(
  "https://www.landofbasketball.com/teams/records_orlando_magic.htm",
  "Magic"
)

p76ers <- team_season_data(
  "https://www.landofbasketball.com/teams/records_philadelphia_76ers.htm",
  "76ers"
)

suns <- team_season_data(
  "https://www.landofbasketball.com/teams/records_phoenix_suns.htm",
  "Suns"
)

blazers <- team_season_data(
  "https://www.landofbasketball.com/teams/records_portland_trailblazers.htm",
  "Trail Blazers"
)

kings <- team_season_data(
  "https://www.landofbasketball.com/teams/records_sacramento_kings.htm",
  "Kings"
)

spurs <- team_season_data(
  "https://www.landofbasketball.com/teams/records_san_antonio_spurs.htm",
  "Spurs"
)

# raptors only have 23 rows; started in 1995-96

raptors <- team_season_data(
  "https://www.landofbasketball.com/teams/records_toronto_raptors.htm",
  "Raptors"
)

jazz <- team_season_data(
  "https://www.landofbasketball.com/teams/records_utah_jazz.htm",
  "Jazz"
)

wizards <- team_season_data(
  "https://www.landofbasketball.com/teams/records_washington_wizards.htm",
  "Wizards/Bullets"
)

# putting all 30 teams in the same dataframe

nba_team_performance <- bind_rows(blazers,
                                  bucks,
                                  bulls,
                                  cavs,
                                  celtics,
                                  clippers,
                                  grizzlies,
                                  hawks,
                                  heat,
                                  hornets,
                                  jazz,
                                  kings,
                                  knicks,
                                  lakers,
                                  magic,
                                  mavs,
                                  nets,
                                  nuggets,
                                  p76ers,
                                  pacers,
                                  pelicans,
                                  pistons,
                                  raptors,
                                  rockets,
                                  spurs,
                                  suns,
                                  thunder,
                                  twolves,
                                  warriors,
                                  wizards) %>% 
  
  # modifying performance variables to NOT be conference-specific (conference
  # is a variable on its own)
  
  mutate(performance = gsub("DNQ", 
                            "Did not qualify", 
                            performance)) %>% 
  mutate(performance = gsub("Lost East Conf 1st Rd", 
                            "Lost 1st Round", 
                            performance)) %>% 
  mutate(performance = gsub("Lost West Conf 1st Rd", 
                            "Lost 1st Round", 
                            performance)) %>% 
  mutate(performance = gsub("Lost East Conf Semis", 
                            "Lost Conf Semis", 
                            performance)) %>% 
  mutate(performance = gsub("Lost West Conf Semis", 
                            "Lost Conf Semis", 
                            performance)) %>% 
  mutate(performance = gsub("Lost West Conf Finals", 
                            "Lost Conf Finals", 
                            performance)) %>% 
  mutate(performance = gsub("Lost East Conf Finals", 
                            "Lost Conf Finals", 
                            performance))

# checking to make sure I have all 30 franchises (and no more)

# nba_team_performance %>% 
#   group_by(franchise_id) %>% 
#   count()

# prepare to join performance data with salary data by splitting team into two
# columns, one for team name and one for location

nba_salaries %<>% 
  mutate(location = word(team, start = 1, end = -2)) %>% 
  mutate(mascot = word(team, -1)) %>% 
  mutate(mascot = gsub("Blazers", "Trail Blazers", mascot)) %>% 
  mutate(location = gsub("Portland Trail", "Portland", location))

# join performance data to salary data

nba_full <- nba_team_performance %>% 
  left_join(nba_salaries, by = c("team" = "mascot", "season")) %>% 
  mutate(season = as.factor(season)) %>% 
  mutate(conference = as.factor(conference)) %>% 
  mutate(performance = as.factor(performance)) %>% 
  mutate(performance = fct_relevel(performance,
                                   "Did not qualify",
                                   "Lost 1st Round",
                                   "Lost Conf Semis",
                                   "Lost Conf Finals",
                                   "Lost NBA Finals",
                                   "NBA Champions")) %>% 
  mutate(franchise_id = as.factor(franchise_id))



###### PLOT TIME

nba_plot <- nba_full %>% 
  ggplot(aes(team_salary / 1000000, rs_win_pct)) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~ season, scales = "free_x") +
  theme_classic() +
  labs(title = "NBA Team Payroll and Regular Season Win Percentage by Season",
       subtitle = "Payroll and win percentage are positively associated
       (Overall correlation coefficient: a weak 0.089)",
       x = "Payroll (in millions of USD)",
       y = "Regular Season Win Percentage") +
  geom_smooth(method = "lm", se = FALSE)
