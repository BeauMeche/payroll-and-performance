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
                                              h4(textOutput("payroll_over_time")
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
    
    # # model panel, to talk more about model and regression results
    # 
    # tabPanel("Comments on Payroll's Effect",
    #          titlePanel("Payroll and Performance: A Linear Model"),
    #          sidebarLayout(
    #              
    #              # sidebar panel holds selector to pick league to view and a few
    #              # observational notes about the data itself
    #              
    #              sidebarPanel(
    #                  h4(strong("Select a League")),
    #                  p("Note: the effects documented on this page are not
    #                    necessarily causal; rather, they represent the average
    #                    effect on win percentage associated with payroll"),
    #                  selectInput("league_4",
    #                              "League",
    #                              c("NBA" = "nba", "MLB" = "mlb",
    #                                "NHL" = "nhl", "MLS" = "mls")
    #                              ),
    #                  h4(strong("Observations:")),
    #                  h4("NBA"),
    #                  p("Roughly half of the teams' confidence intervals are
    #                    above zero, indicating that they have a definite positive 
    #                    association between payroll rank and win percentage"),
    #                  p("The Knicks and Pelicans have not historically been
    #                    smart spenders; they actually tend to lose more when 
    #                    spending more"),
    #                  h4("MLB"),
    #                  p("The New York Yankees have far and away the largest 
    #                    effect of payroll rank on win percentage; for all but a 
    #                    few of the other teams, the effect appears negligible"),
    #                  p("Generally, teams in the MLB seem to experience a lower
    #                    effect than teams in the NBA")
    #              )
    #          ),
    #          
    #          sidebarLayout(
    #              
    #              # sidebar panel holds observational notes about the data
    #              
    #              sidebarPanel(
    #                  h4(strong("Observations:")),
    #                  h4("NBA"),
    #                  p("Payroll's effect on win percentage declined from 1985
    #                    until 1997, at which point it leveled off"),
    #                  p("In every season except 1984-95 and 1988-89, the 95% 
    #                    confidence interval contains zero - so for all but two of
    #                    the seasons, we can't be sure an additional $1 million on
    #                    payroll had any effect at all on win percentage"),
    #                  h4("MLB"),
    #                  p("With the exception of 1985, there seems to be no real 
    #                     effect of payroll on win percentage regardless of season
    #                     - and even the 1985 effect is small ($1 million 
    #                     additional payroll is associated, on average, with less 
    #                    than a .5 point increase in win percentage)")
    #              )
    #          )
    # ),
    
    # about panel, to explain the project and do some shameless PR. All text is
    # assigned to objects in about_page_text.R
    
    tabPanel("About", 
             titlePanel("About the Project"),
             p(p1),
             h3("Definitional Notes"),
             p(p2),
             h3("More to Come?"),
             p(p3),
             # p(p4),
             p(p5),
             h3("Acknowledgements"),
             
             # gets a little messy creating hyperlinks
             
             p(p6, a(href = p6_link_1, "here",
                     .noWS = "outside", target = "_blank"),
               p6a, a(href = p6_link_2, "here",
                      .noWS = "outside", target = "_blank"),
               p6b, a(href = p6_link_3, "here",
                      .noWS = "outside", target = "_blank"),
               p6c),
             
             # same thing here
             
             p(p7, a(href = p7_link_1, "here",
                     .noWS = "outside", target = "_blank"),
               p7a, a(href = p7_link_2, "here",
                      .noWS = "outside", target = "_blank"),
               p7b, a(href = p7_link_3, "here",
                      .noWS = "outside", target = "_blank"),
               p7c),
             
             # and here
             
             h3("About Me"),
             p(p8, a(href = p8_link, "GitHub",
                     .noWS = "outside", target = "_blank"),
               p8a),
             
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
                "Tremendous growth in payroll over time, with some growth in 
                inequality (payroll spread) as well; inequality was greatest 
                between 1997 and 2008",
            input$league_1 == "mlb" ~
               "Payroll over time has grown substantially; inequality (payroll 
                spread) has also grown, led by the high-spending New York 
                Yankees",
            input$league_1 == "nhl" ~
                "Payroll spread condensed dramatically after 2005 lockout as top
                spenders began spending significantly less",
            TRUE ~ 
                "Extremely skewed data, with a few teams (in LA, NYC, and 
                Toronto) consistently outspending the pack; overall, slow growth
                in payroll over time")
        
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
                      "NBA",
                  input$league_2 == "mlb" ~
                      "MLB",
                  input$league_2 == "nhl" ~
                      "NHL",
                  TRUE ~ 
                      "MLS")
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
                      "NBA",
                  input$league_3 == "mlb" ~ 
                      "MLB",
                  input$league_3 == "nhl" ~
                      "NHL",
                  TRUE ~ 
                      "MLS")
    })

}

# Run the application 

shinyApp(ui = ui, server = server)
