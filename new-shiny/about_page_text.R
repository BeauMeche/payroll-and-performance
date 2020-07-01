# this file contains text to load into the about page, in an effort to not
# over-clutter by app.R file with text

p1 <- "My project examines data from four professional sports leagues in the
United States (NBA, MLB, NHL, and MLS), analyzing the relationship between a 
team's payroll  and their level of success in the league."

p2 <- "Payroll is tremendously complex in professional sports because of things 
like deferred payments (when athletes are paid for current work at future 
dates). I simplified it here by treating it as the total amount that a team 
spent on its current players' salaries in a given year."

p3 <- '"Success" is examined here only by regular season performance. It
would be interesting to also look at success as measured by metrics of 
post-season performance (such as making the playoffs or winning a championship).
My data shows that to some extent, payroll correlates with regular season win 
percentage -- but  would that hold true for playoff success as well? My (as yet 
untested) hypothesis is that teams can "buy" a certain level of regular season 
success by paying a lot of money to talented players, but that doing so does not
necessarily win them a championship.'

# several chunks for the same paragraph here in order to create hyperlinks

p6 <- "Major thanks to Sean Lahman and the folks at Baseball Reference, who 
compiled and made available the MLB data used in this project. See "

p6_link_1 <- "http://www.seanlahman.com/baseball-archive/statistics/"

p6a <- " for the data I obtained from Sean, "

p6_link_2 <- "https://www.baseball-reference.com/leagues/MLB/"

p6b <- " for the data I scraped from Baseball Reference, and "

scraping_code <- "https://github.com/westleycook/payroll-and-performance/blob/master
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

p7c <- " for my scraping code."

# several chunks for the same paragraph here as well

p10 <- "My work with NHL data wouldn't have been possible without this "

p10_link_1 <- "https://en.wikipedia.org/wiki/List_of_team_payrolls_in_the_NHL"

p10a <- ", from which I scraped payroll data, or the folks at Hockey Reference
who compiled "

p10_link_2 <- "https://www.hockey-reference.com/teams/"

p10b <- " of team performance in the NHL. For my scraping code, see "

period <- "."

# last acknowledgement paragraph, in chunks

p11 <- "Finally, I'm indebted to the MLS Players Association, which puts out
annual Salary Guides with salary information for all MLS players; you can find
links to a PDF of each year's guide "

p11_link_1 <- "https://mlsplayers.org/resources/salary-guide"

p11a <- ". Regular season performance data for each season was compiled from 
the "

p11_link_2 <- "https://www.mlssoccer.com/standings/mls/2007/"

p11b <- " and playoff results were taken from "

p11_link_3 <- "https://en.wikipedia.org/wiki/2007_MLS_Cup_Playoffs"

p11c <- " (although, as noted above, no analysis of the playoff data has yet
been performed). For more detailed information on the data I used, see my 
scraping code "

# bio

p8 <- "My name is Westley Cook and I'm a Harvard '20 grad with a BA in East 
Asian Studies and a secondary in Government. You can reach me at 
westleykcook@gmail.com and find me on "

p8_link <- "https://github.com/westleycook"

p9 <- "The full repo for this project can be found "

p9_link <- "https://github.com/westleycook/payroll-and-performance"