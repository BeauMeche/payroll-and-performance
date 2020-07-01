# load necessary libraries

library(shiny)
library(plotly)

# load necessary data and code from helper files, including pre-made plots and
# tables to keep the server code below as clean and concise as possible

source("mlb_data.R")
source("nba_data.R")
source("nhl_data.R")
source("mls_data.R")

# load text for about page

source("about_page_text.R")

# create UI

ui <- navbarPage(
    
    # title
    
    "Payroll and Performance in Professional Sports",
    
    # trends panel, which has plots (plotlys) and tables to show data by league
    
    tabPanel("Payroll Trends",
             fluidPage(
                 titlePanel("Payroll by League"),
                 
                 # sidebar with explanatory text and selector to choose league 
                 
                 sidebarLayout(
                     sidebarPanel(
                         h4(strong("Select a League")),
                         selectInput(
                             "league_1",
                             "League",
                             c("NBA" = "nba", "MLB" = "mlb",
                               "NHL" = "nhl", "MLS" = "mls")
                         ),
                         p("All plots on this site are interactive; try hovering
                            your mouse over a point!"),
                         p("A control bar at the top of each plot offers 
                            additional options such as zoom, autoscale,
                            downloading as png, and resetting to the default.")
                     ),
                     
                     # display plotly of payroll over time; second bullet point
                     # changes by league
                     
                     mainPanel(
                         h3(strong("Change in Payroll over Time")),
                         h4(textOutput("payroll_over_time_subtitle")),
                         tabsetPanel(type = "tabs",
                                     tabPanel("Plot", plotlyOutput("payroll")),
                                     tabPanel("Key Takeaway",
                                              p(textOutput("payroll_over_time")
                                                 )
                                              )
                         )
                     )
                 )
             )
    ),
             
    tabPanel("Payroll and Performance by Year",
             
             fluidPage(
                 
                 # formatting for leftAlign class used below
                 
                 tags$head(tags$style(".leftAlign{float:left;}")),
                 
                 # title panel
                 
                 titlePanel("Payroll and Regular Season Performance"),
                 
                 # sidebar to hold league selector and notes
                 
                 sidebarLayout(
                     sidebarPanel(
                         h4(strong("Select a League")),
                         p("Note: display is best in large windows, and may take
                            time to load"),
                         selectInput(
                             "league_2",
                             "League",
                             c("NBA" = "nba", "MLB" = "mlb",
                               "NHL" = "nhl", "MLS" = "mls")
                         ),
                         p("All plots on this site are interactive; try hovering
                            your mouse over a point!"),
                         p("A control bar at the top of each plot offers 
                            additional options such as zoom, autoscale,
                            downloading as png, and resetting to the default.")
                     ),
                     
                     # main panel displays plotly of payroll and wins by year
                     
                     mainPanel(
                         h3(strong("Relationship by Year")),
                         h5("Across all leagues, the relationship between 
                            payroll and performance varies considerably 
                            year-to-year."),
                         h4(textOutput("by_year_subtitle")),
                         tabsetPanel(type = "tabs",
                                     tabPanel("Plot", 
                                              h5(strong("A season-by-season look
                                                at the line of best fit 
                                                demonstrating the relationship 
                                                between payroll and performance 
                                                (simple linear regression 
                                                model)")),
                                              plotlyOutput("by_year")),
                                     tabPanel("Correlation Tables",
                                              fluidRow(
                                                  column(6,
                                                         gt_output("year_cor_1")
                                                         ),
                                                  column(6,
                                                         gt_output("year_cor_2")
                                                         )
                                                  ),
                                              class = 'leftAlign'),
                                     tabPanel("Modeling Payroll's Effect on 
                                              Performance",
                                              h4("Effect of Spending Additional 
                                                 $1 Million: A Linear Model"),
                                              p(strong("An effect of 1 means 
                                              that a team's spending an 
                                              additional $1 million on payroll 
                                              is associated, on average, with a
                                              1 point increase in win percentage 
                                              (NBA, MLB) or an additional 1 
                                              point in regular season standings 
                                              (NHL, MLS).")),
                                              p("Bars show 95% confidence 
                                                interval"),
                                              plotlyOutput("year_effect")),
                                     tabPanel("Key Takeaway",
                                              textOutput("obs_by_year"))
                                    )
                    )
                 )
                 )
    ),
    tabPanel("Payroll and Performance by Team",
             
             fluidPage(
                 
                 tags$head(tags$style(".leftAlign{float:left;}")),
                 
                 titlePanel("Payroll and Regular Season Performance"),
                 
                 # sidebar to hold league selector and some notes
                 
                 sidebarLayout(
                     sidebarPanel(
                         h4(strong("Select a League")),
                         p("Note: display is best in large windows, and may take
                            time to load"),
                         selectInput(
                             "league_3",
                             "League",
                             c("NBA" = "nba", "MLB" = "mlb",
                               "NHL" = "nhl", "MLS" = "mls")
                         ),
                         p("All plots on this site are interactive; try hovering
                            your mouse over a point!"),
                         p("A control bar at the top of each plot offers 
                            additional options such as zoom, autoscale,
                            downloading as png, and resetting to the default.")
                     ),
                     
                     # main panel holds plotly of payroll and wins by team
                     
                     mainPanel(
                         h3(strong("Relationship by Franchise")),
                         h5("Across all leagues, the relationship between 
                            payroll and performance varies considerably 
                            by team."),
                         p(strong("Note the x axis: payroll rank is how a team's
                            spending in a season compared to others in the
                            league (1 is lowest)")),
                         h4(textOutput("by_team_subtitle")),
                         tabsetPanel(type = "tabs",
                                     tabPanel("Plot", 
                                              h5(strong("A team-by-team look
                                                at the line of best fit 
                                                demonstrating the relationship 
                                                between payroll and performance 
                                                (simple linear regression 
                                                model)")),
                                              plotlyOutput("by_team")),
                                     tabPanel("Correlation Tables",
                                              fluidRow(
                                                  column(6,
                                                         gt_output("team_cor_1")
                                                  ),
                                                  column(6,
                                                         gt_output("team_cor_2")
                                                  )
                                              ),
                                              class = 'leftAlign'),
                                     tabPanel("Modeling Payroll's Effect on 
                                              Performance",
                                              h4("Effect of Outspending 
                                                 Competitors: A Linear Model"),
                                              p(strong("An effect of 1 means 
                                              that moving up one spot in payroll
                                              rank (outspending one additional 
                                              team in the league) is 
                                              associated, on average, with a 1 
                                              point increase in win percentage 
                                              (NBA, MLB) or an additional 1 
                                              point in regular season standings 
                                              (NHL, MLS).")),
                                              p("Bars show 95% confidence 
                                                interval"),
                                              plotlyOutput("team_effect")),
                                     tabPanel("Key Takeaway",
                                              textOutput("obs_by_team"))
                         )
                     )
             )
    )
    ),
    
    # about panel, to explain the project and do some shameless PR. All text is
    # assigned to objects in about_page_text.R
    
    tabPanel("About", 
             titlePanel("About the Project"),
             p(strong(p1)),
             h3("Definitional Notes"),
             p(p2),
             h3("More to Come?"),
             p(p3),
             h3("Sources and Acknowledgements"),
             
             # gets a little messy creating hyperlinks
             
             p(p6, a(href = p6_link_1, "here",
                     .noWS = "outside", target = "_blank"),
               p6a, a(href = p6_link_2, "here",
                      .noWS = "outside", target = "_blank"),
               p6b, a(href = scraping_code, "here",
                      .noWS = "outside", target = "_blank"),
               p6c),
             
             # same thing here
             
             p(p7, a(href = p7_link_1, "here",
                     .noWS = "outside", target = "_blank"),
               p7a, a(href = p7_link_2, "here",
                      .noWS = "outside", target = "_blank"),
               p7b, a(href = scraping_code, "here",
                      .noWS = "outside", target = "_blank"),
               p7c),
             
             # and here
             
             p(p10, a(href = p10_link_1, "Wikipedia page",
                     .noWS = "outside", target = "_blank"),
               p10a, a(href = p10_link_2, "this database",
                      .noWS = "outside", target = "_blank"),
               p10b, a(href = scraping_code, "here",
                      .noWS = "outside", target = "_blank"),
               period),
             
             # aaaaaand here
             
             p(p11, a(href = p11_link_1, "here",
                      .noWS = "outside", target = "_blank"),
               p11a, a(href = p11_link_2, "MLS website",
                       .noWS = "outside", target = "_blank"),
               p11b, a(href = p11_link_3, "Wikipedia",
                       .noWS = "outside", target = "_blank"),
               p11c, a(href = scraping_code, "here",
                       .noWS = "outside", target = "_blank"),
               period),
             
             # and here, but at least we're done with acknowledgements now
             
             h3("About Me"),
             p(p8, a(href = p8_link, "GitHub",
                     .noWS = "outside", target = "_blank"),
               period),
             
             # aaaand here
             
             p(p9, a(href = p9_link, "here",
                     .noWS = "outside", target = "_blank"),
               ".")
             )
    
    )



# create server

server <- function(input, output) {
    
    # reactive subtitle for payroll over time plot
    
    output$payroll_over_time_subtitle <- renderText({
        
        subtitle_text <- case_when(input$league_1 == "nba" ~
                                       "NBA, 1985 - 2018",
                                   input$league_1 == "mlb" ~
                                       "MLB, 1985 - 2016",
                                   input$league_1 == "nhl" ~
                                       "NHL, 1999 - 2008",
                                   TRUE ~ "MLS, 2007 - 2019")
    })
    
    # reactive observation text for payroll over time plot, giving a unique
    # description for each league
    
    output$payroll_over_time <- renderText({
        
        # choose text to display based on input$league selector
        
        payroll_over_time_text <- case_when(
            input$league_1 == "nba" ~
                "Payroll has grown tremendously over time, with some growth in 
                inequality (payroll spread) as well. Inequality was greatest 
                between 1997 and 2008.",
            input$league_1 == "mlb" ~
               "Payroll over time has grown substantially. Inequality (payroll 
                spread) has also grown, led by the high-spending New York 
                Yankees.",
            input$league_1 == "nhl" ~
                "Payroll spread condensed dramatically after the 2005 lockout as
                top spenders began spending significantly less.",
            TRUE ~ 
                "The data is extremely skewed, with a few teams (in LA, NYC, and 
                Toronto) consistently outspending the pack. Overall, there has
                been slow growth in payroll over time.")
        
    })
    
    # reactive plotly for payroll over time based on league selector; pre-made
    # plots from helper code files
    
    output$payroll <- renderPlotly({
        
        # choose plot based on input$league selector
        
        plot1 <- case_when(input$league_1 == "nba" ~ list(nba_plot_1),
                           input$league_1 == "mlb" ~ list(mlb_plot_1),
                           input$league_1 == "nhl" ~ list(nhl_plot_1),
                           TRUE ~ list(mls_plot_1)) %>% 
            .[[1]]
        
        # display plot with hover revealing text and customize mode bar
        
        ggplotly(plot1, tooltip = "text") %>% 
            config(modeBarButtonsToRemove = list("sendDataToCloud", 
                                                 "hoverClosestCartesian", 
                                                 "hoverCompareCartesian", 
                                                 "select2d", 
                                                 "lasso2d", 
                                                 "toggleSpikelines",
                                                 "editInChartStudio",
                                                 "zoom2d",
                                                 "pan2d"))
    })
    
    # reactive subtitle for relationship-by-year plot
    
    output$by_year_subtitle <- renderText({
        
        subtitle_text <- case_when(input$league_2 == "nba" ~
                                       "NBA, 1985 - 2018",
                                   input$league_2 == "mlb" ~
                                       "MLB, 1985 - 2016",
                                   input$league_2 == "nhl" ~
                                       "NHL, 1999 - 2008",
                                   TRUE ~ "MLS, 2007 - 2019")
    })
    
    # reactive plotly to show payroll and wins by year, using plots from helper
    # files
    
    output$by_year <- renderPlotly({
        
        # choose plot based on input$league selector
        
        plot2 <- case_when(
            input$league_2 == "nba" ~ list(nba_plot_2),
            input$league_2 == "mlb" ~ list(mlb_plot_2),
            input$league_2 == "nhl" ~ list(nhl_plot_2),
            TRUE ~ list(mls_plot_2)) %>%
            .[[1]]
        
        # assign the plot to ggplotly so hover reveals text, and specify
        # dimensions - these were a pain with a large facet_wrap plot
        
        gp2 <- ggplotly(plot2, tooltip = "text", height = 600, width = 850)
        
        # change y-axis title position so it doesn't overlap with labels
        
        gp2[['x']][['layout']][['annotations']][[2]][['x']] <- -0.04
        
        # display plot, customizing mode bar to drop unnecessary buttons
        
        gp2 %>% 
            config(modeBarButtonsToRemove = list("sendDataToCloud", 
                                                 "hoverClosestCartesian", 
                                                 "hoverCompareCartesian", 
                                                 "select2d", 
                                                 "lasso2d", 
                                                 "toggleSpikelines",
                                                 "editInChartStudio",
                                                 "zoom2d",
                                                 "pan2d"))
    })
    
    # render the full gt table for the correlations tab display
    
    output$year_cor_1 <- render_gt({
        
        # choose table to show based on input$league selector
        
        year_cor_table <- case_when(
            input$league_2 == "nba" ~ list(nba_year_cor_table),
            input$league_2 == "mlb" ~ list(mlb_year_cor_table),
            input$league_2 == "nhl" ~ list(nhl_year_cor_table),
            TRUE ~ list(mls_year_cor_table)) %>%
        .[[1]]
        
        # show table
        
        year_cor_table
        
    })
    
    # render the summary gt table for the correlations tab display
    
    output$year_cor_2 <- render_gt({
        
        # choose table to show based on input$league selector
        
        year_cor_table <- case_when(
            input$league_2 == "nba" ~ list(nba_year_cor_sum),
            input$league_2 == "mlb" ~ list(mlb_year_cor_sum),
            input$league_2 == "nhl" ~ list(nhl_year_cor_sum),
            TRUE ~ list(mls_year_cor_sum)) %>%
            .[[1]]
        
        # show table
        
        year_cor_table
        
    })
    
    # render plotly: effect of payroll on wins with season interaction
    
    output$year_effect <- renderPlotly({
        
        # select league's plot to display based on input$league_2
        
        mod_plot_2 <- case_when(input$league_2 == "nba" ~ list(nba_mod_plot_2),
                                input$league_2 == "mlb" ~ list(mlb_mod_plot_2),
                                input$league_2 == "nhl" ~ list(nhl_mod_plot_2),
                                TRUE ~ list(mls_mod_plot_2)) %>%
            .[[1]]
        
        # display the plot with hover revealing text, specify dimensions, and
        # customize mode bar
        
        ggplotly(mod_plot_2, tooltip = "text", height = 400, width = 600) %>% 
            config(modeBarButtonsToRemove = list("sendDataToCloud", 
                                                 "hoverClosestCartesian", 
                                                 "hoverCompareCartesian", 
                                                 "select2d", 
                                                 "lasso2d", 
                                                 "toggleSpikelines",
                                                 "editInChartStudio",
                                                 "zoom2d",
                                                 "pan2d"))
    })
    
    # reactive text describing key takeaways for by-year data
    
    output$obs_by_year <- renderText({
        
        case_when(input$league_2 == "nba" ~
                      "League-wide, payroll's effect on performance declined in
                    magnitude from 1985-2001. From 2001-2008 -- during seasons 
                    characterized by high-spending teams such as the Knicks and 
                    Blazers performing poorly despite their high payrolls -- the
                    estimated effect of payroll on performance hovered near 
                    zero. However, since then there has been a slight rebound. 
                    Note that for all seasons except two (1984-85 and 1988-89),
                    the 95% confidence interval contains zero; this indicates
                    that except for those two seasons, we can NOT say with
                    confidence that payroll had ANY effect on performance across
                    the league.",
                  input$league_2 == "mlb" ~
                      "For all seasons except 1985, the estimated effect of
                    payroll on performance is near zero, and zero is well within
                    the bounds of each 95% confidence interval. This means that 
                    despite seeing some seasons with moderately strong 
                    correlations between payroll and performance (e.g. 1998, 
                    2016), the effect of payroll on performance appears to be 
                    negligible.",
                  input$league_2 == "nhl" ~
                      "In most seasons, the effect of payroll on performance 
                    appears to be small (an extra $1 million spent on payroll is
                    associated with an additional half point in the regular 
                    season standings), but the confidence intervals give reason 
                    to believe that some positive effect is indeed present. The
                    effect was strongest in the 2005-06 season, which is 
                    noteworthy because the season prior to that (2004-05) had 
                    been cancelled due to lockout.",
                  TRUE ~ 
                      "The linear relationship between payroll and performance 
                    in the MLS appears to be quite weak in most seasons (and 
                    is even NEGATIVE in four of them -- meaning that teams who 
                    spent more on payroll tended to earn FEWER points in the 
                    regular season standings). This is likely a result of the 
                    skewedness of the payroll data set: a simple linear model 
                    is easily influenced by outliers (i.e. the high-spending 
                    teams in New York, LA, and Toronto) and thus may not be the 
                    most useful model to describe this league's data.
                    ")
    })
    
    # reactive subtitle for relationship-by-team plot
    
    output$by_team_subtitle <- renderText({
        
        subtitle_text <- case_when(input$league_3 == "nba" ~
                                       "NBA, 1985 - 2018",
                                   input$league_3 == "mlb" ~
                                       "MLB, 1985 - 2016",
                                   input$league_3 == "nhl" ~
                                       "NHL, 1999 - 2008",
                                   TRUE ~ "MLS, 2007 - 2019")
    })
    
    # render plotly of payroll and wins by team
    
    output$by_team <- renderPlotly({
        
        # choose league to plot based on input$league selector
        
        plot3 <- case_when(input$league_3 == "nba" ~ list(nba_plot_3),
                           input$league_3 == "mlb" ~ list(mlb_plot_3),
                           input$league_3 == "nhl" ~ list(nhl_plot_3),
                           TRUE ~ list(mls_plot_3)) %>%
            .[[1]]
        
        
        # assign the plot to ggplotly so hover reveals text and specify
        # dimensions
        
        gp3 <- ggplotly(plot3, tooltip = "text", height = 600, width = 850)
        
        # change y-axis title position so it doesn't overlap with labels
        
        gp3[['x']][['layout']][['annotations']][[2]][['x']] <- -0.04
        
        # display plot, customizing mode bar
        
        gp3 %>% 
            config(modeBarButtonsToRemove = list("sendDataToCloud", 
                                                 "hoverClosestCartesian", 
                                                 "hoverCompareCartesian", 
                                                 "select2d", 
                                                 "lasso2d", 
                                                 "toggleSpikelines",
                                                 "editInChartStudio",
                                                 "zoom2d",
                                                 "pan2d"))
    })
    
    # render the full cor by team gt table for display in correlations tab
    
    output$team_cor_1 <- render_gt({
        
        # choose table to show based on input$league selector
        
        team_cor_table <- case_when(
            input$league_3 == "nba" ~ list(nba_team_cor_table),
            input$league_3 == "mlb" ~ list(mlb_team_cor_table),
            input$league_3 == "nhl" ~ list(nhl_team_cor_table),
            TRUE ~ list(mls_team_cor_table)) %>%
            .[[1]]
        
        # show table
        
        team_cor_table
        
    })
    
    # render the summary table for cor by team
    
    output$team_cor_2 <- render_gt({
        
        # choose table to show based on input$league selector
        
        team_cor_sum <- case_when(
            input$league_3 == "nba" ~ list(nba_team_cor_sum),
            input$league_3 == "mlb" ~ list(mlb_team_cor_sum),
            input$league_3 == "nhl" ~ list(nhl_team_cor_sum),
            TRUE ~ list(mls_team_cor_sum)) %>%
            .[[1]]
        
        # show table
        
        team_cor_sum
        
    })
    
    # render plotly: effect of payroll on wins with team interaction
    
    output$team_effect <- renderPlotly({
        
        # select league's plot to display based on input$league_2
        
        mod_plot_1 <- case_when(input$league_3 == "nba" ~ list(nba_mod_plot_1),
                                input$league_3 == "mlb" ~ list(mlb_mod_plot_1),
                                input$league_3 == "nhl" ~ list(nhl_mod_plot_1),
                                TRUE ~ list(mls_mod_plot_1)) %>%
            .[[1]]
        
        # display the plot with hover revealing text, specify dimensions, and
        # customize mode bar
        
        ggplotly(mod_plot_1, tooltip = "text", height = 400, width = 600) %>% 
            config(modeBarButtonsToRemove = list("sendDataToCloud", 
                                                 "hoverClosestCartesian", 
                                                 "hoverCompareCartesian", 
                                                 "select2d", 
                                                 "lasso2d", 
                                                 "toggleSpikelines",
                                                 "editInChartStudio",
                                                 "zoom2d",
                                                 "pan2d"))
    })
    
    # reactive text describing key takeaways for by-team data
    
    output$obs_by_team <- renderText({
        
        case_when(input$league_3 == "nba" ~
                      "Roughly half of the NBA's 30 teams experience a definite
                    positive effect on performance when they outspend their 
                    competitors (read: the 95% confidence interval for effect
                    does NOT contain zero) and all but three of the teams see
                    an estimated positive effect, with values ranging from a .05
                    to 1.3 point increase in regular season win percentage for 
                    each additional team they outspend. However, the Blazers, 
                    Pelicans, and Knicks have the dubious distinction of 
                    experiencing a NEGATIVE effect -- meaning they tend to 
                    win FEWER regular season games when they outspend their
                    opponents. This overall effect seems to be the result of 
                    their high spending and poor performance in the early 2000s.
                    ",
                  input$league_3 == "mlb" ~ 
                      "Only 7 of the league's 30 teams experience a definite
                    positive effect on performance when they outspend their 
                    competitors (read: the 95% confidence interval for effect
                    does NOT contain zero), and the estimated size of the effect
                    for each team except the New York Yankees tends to be 
                    smaller than those seen in the NBA. In stark contrast with
                    the rest of the league, the Yankees -- known for attracting
                    the league's best talent with big money -- see a very strong
                    relationship between how much they spend on payroll relative
                    to the rest of the league and how well they perform.",
                  input$league_3 == "nhl" ~
                      "Perhaps the most noteworthy thing in the NHL is the wide 
                      variation among teams: the New York Islanders, Nashville 
                      Predators, and Atlanta Thrashers all see an estimated 
                      effect of more than 2 additional points in the regular 
                      season standings for each team they outspend, while the
                      estimated effect for the Columbus Blue Jackets is an 
                      atrocious -1.4 and the estimated effect for the New York 
                      Rangers is even worse (nearly -3). It's interesting that
                      in the league where money seems to make the biggest 
                      difference (see Payroll and Performance by Year), some 
                      teams seem to be spending much more effectively than 
                      others.",
                  TRUE ~ 
                      "Like the NHL, MLS is noteworthy for a wide variation 
                      among teams in the effectiveness of payroll spending: 
                      Toronto FC sees an estimated effect of 2.5 additional 
                      points in the regular season standings for each team they 
                      outspend, while FC Dallas and New York City FC each see an 
                      effect of -0.9. Interestingly, among all four leagues, MLS
                      has the most teams (8) and highest proportion of teams 
                      (1/3) which experience a negative relationship between 
                      payroll and performance.")
        
    })

}

# Run the application 

shinyApp(ui = ui, server = server)
