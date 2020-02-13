library(readxl)
library(tidyverse)

salaries <- read.csv("Salaries.csv")

glimpse(salaries)
summary(salaries)

teams <- read.csv("Teams.csv")

glimpse(teams)

summary(teams)

teams

# Data is from here: http://www.seanlahman.com/baseball-archive/statistics/

