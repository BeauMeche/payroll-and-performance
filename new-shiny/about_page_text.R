# this file contains text to load into the about page, in an effort to not
# over-clutter by app.R file with text

p1 <- "My project examines data from the NBA and MLB, analyzing the relationship
between a team's payroll and their level of success in the league."

p2 <- "Payroll is tremendously complex in professional sports because of things 
like deferred payments (when athletes are paid for current work at future 
dates). I simplified it here by treating it as the total amount that a team 
spent on its current players' salaries in a given year."

p3 <- '"Success" is examined here only by total wins in the regular season. It
would be interesting to also look at success as measured by metrics of 
post-season performance (such as making the playoffs or winning a championship).
My data shows that payroll correlates with regular season win percentage, but 
will that hold true for playoff success as well? My hypothesis is that teams can
"buy" a certain level of regular season success by paying a lot to talented 
players, but that it does not necessarily win them a championship.'

p4 <- "In the future, it would also be interesting to add analysis of NFL, NHL,
or MLS data - but for now, I'm limited to data from the MLB and NBA because 
their payroll data was publicly available and easy to access."

p5 <- "It would also be interesting to look more closely into cases where teams 
changed ownership/management or location, and see if that had a significant 
impact on payroll or success, but time constraints have precluded this option 
for now."

# several chunks for the same paragraph here in order to create hyperlinks

p6 <- "Major thanks to Sean Lahman and the folks at Baseball Reference, who 
compiled and made available the MLB data used in this project. See "

p6_link_1 <- "http://www.seanlahman.com/baseball-archive/statistics/"

p6a <- " for the data I obtained from Sean, "

p6_link_2 <- "https://www.baseball-reference.com/leagues/MLB/"

p6b <- " for the data I scraped from Baseball Reference, and "

p6_link_3 <- "https://github.com/westleycook/payroll-and-performance/blob/master
/gather.Rmd"

p6c <- " for my scraping code."

# several chunks for the same paragraph here too, in order to create hyperlinks

p7 <- "Additional thanks goes out to Chris Davis, who compiled and made 
available the NBA payroll data used in this project, and to the folks at Land of
Basketball who compiled the NBA team performance data I used. See "

p7_link_1 <- "https://data.world/datadavis/nba-salaries"

p7a <- " for salary data from Chris, "

p7_link_2 <- "https://www.landofbasketball.com/nba_teams_year_by_year.htm"

p7b <- " for team performance data, and "

p7_link_3 <- "https://github.com/westleycook/payroll-and-performance/blob/master
/gather.Rmd"

p7c <- " for my scraping code."

p8 <- "My name is Westley Cook and I'm a Harvard senior majoring in East Asian 
Studies with a secondary in Government. You can reach me at 
westleykcook@gmail.com and find me on "

p8_link <- "https://github.com/westleycook"

p8a <- "."

p9 <- "The repo for this project can be found "

p9_link <- "https://github.com/westleycook/payroll-and-performance"