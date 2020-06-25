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
    
    "Payroll and Performance in the NBA and MLB",
    
    # trends panel, which has plots (plotlys) and tables to show data by league
    
    tabPanel("Trends",
             fluidPage(
                 titlePanel("Payroll and Regular Season Win Percentage"),
                 
                 # sidebar with selector to choose league and explanatory text
                 
                 sidebarLayout(
                     sidebarPanel(
                         h4(strong("Select a League")),
                         p("Note: display is best in large windows, and may take
                            time to load"),
                         selectInput(
                             "league",
                             "League",
                             c("NBA" = "nba", "MLB" = "mlb",
                               "NHL" = "nhl", "MLS" = "mls")
                         ),
                         p("All plots on this page are interactive; try hovering
                            your mouse over a point!"),
                         p("A control bar at the top of each plot offers 
                            additional options such as zoom, autoscale,
                            downloading as png, and resetting to the default."),
                         p("If the plot to the right showing payroll over time
                            doesn't size properly, simply reload the page."),
                         p(strong("I recommend clicking autoscale on the larger
                                  plots."))
                     ),
                     
                     # display plotly of payroll over time; second bullet point
                     # changes by league
                     
                     mainPanel(
                         h3(strong("Change in Payroll over Time, 1985-2018")),
                         tags$ul(
                            tags$li(h4("Tremendous growth in payroll, even after 
                                    adjusting for inflation")),
                            tags$li(h4(textOutput("payroll_over_time"))),
                         ),
                         plotlyOutput("payroll")
                     )
                 ),
                 
                 # give a bit of white space between panels
                 
                 br(),
                 
                 # sidebar to hold table of correlation coefficients by year
                 # all text except title customized by league
                 
                 sidebarLayout(
                     sidebarPanel(
                         h4(strong("Payroll and Regular Season Wins: Correlation
                                   Table"),
                            align = "center"),
                         h4(textOutput("year_cor_text"),
                            align = "center"),
                         h5(textOutput("year_cor_max"),
                            align = "center"),
                         h5(textOutput("year_cor_min"),
                            align = "center"),
                         h5(textOutput("year_cor_avg"),
                            align = "center"),
                         gt_output("year_cor"),
                         br(),
                         p("Most seasons see a moderate correlation between
                           payroll and win percentage, but the relationship
                           is sometimes very weak or nonexistent.")
                     ),
                     
                     # main panel displays plotly of payroll and wins by year
                     
                     mainPanel(
                         h3(strong("Relationship by Year")),
                         h4("Does the relationship between payroll and win
                            percentage vary by year? Clearly, the answer is 
                            yes."),
                         plotlyOutput("by_year"),
                         
                         # set height so next sidebar doesn't overlap with plot
                         # in mobile view/small windows
                         
                         div(style = "height:200px"),
                    )
                 ),
                 
                 # give a bit of white space between panels
                 
                 br(),
                 
                 # sidebar to hold table of correlation coefficients by team
                 # all text except title customized by league
                 
                 sidebarLayout(
                     sidebarPanel(
                         h4(strong("Payroll and Regular Season Wins:
                                    Correlation Table"),
                            align = "center"),
                         h4(textOutput("team_cor_text"),
                            align = "center"),
                         h5(textOutput("team_cor_max"),
                            align = "center"),
                         h5(textOutput("team_cor_min"),
                            align = "center"),
                         h5(textOutput("team_cor_avg"),
                            align = "center"),
                         gt_output("team_cor"),
                         br(),
                         p("Most teams tend to earn more wins when they spend 
                           more on payroll. But others see no strong 
                           relationship between payroll and win percentage, and 
                           a few have the dubious distinction of experiencing a 
                           NEGATIVE relationship - meaning they tend to win LESS
                           when they spend more than their competitors.")
                     ),
                     
                     # main panel holds plotly of payroll and wins by team
                     
                     mainPanel(
                         h3(strong("Relationship by Franchise")),
                         h4("Does the relationship between payroll and win
                            percentage vary by franchise? Again, the answer is
                            yes."),
                         p(strong("Note the x axis: payroll rank is how a team's
                            spending in a season compared to others in the
                            league (1 is lowest)")),
                         plotlyOutput("by_team")
                         )
                     )
             )
    ),
    
    # model panel, to talk more about model and regression results
    
    tabPanel("Model",
             titlePanel("Payroll and Wins: A Linear Model"),
             sidebarLayout(
                 
                 # sidebar panel holds selector to pick league to view and a few
                 # observational notes about the data itself
                 
                 sidebarPanel(
                     h4(strong("Select a League")),
                     p("Note: the effects documented on this page are not
                       necessarily causal; rather, they represent the average
                       effect on win percentage associated with payroll"),
                     selectInput("league_2",
                                 "League",
                                 c("NBA" = "nba", "MLB" = "mlb",
                                   "NHL" = "nhl", "MLS" = "mls")
                                 ),
                     h4(strong("Observations:")),
                     h4("NBA"),
                     p("Roughly half of the teams' confidence intervals are
                       above zero, indicating that they have a definite positive 
                       association between payroll rank and win percentage"),
                     p("The Knicks and Pelicans have not historically been
                       smart spenders; they actually tend to lose more when 
                       spending more"),
                     h4("MLB"),
                     p("The New York Yankees have far and away the largest 
                       effect of payroll rank on win percentage; for all but a 
                       few of the other teams, the effect appears negligible"),
                     p("Generally, teams in the MLB seem to experience a lower
                       effect than teams in the NBA")
                 ),
                 
                 # main panel holds plotly: payroll's effect on win pct by team
                 
                 mainPanel(
                     h3(strong("Payroll's Effect on Win Percentage by Team")),
                     h4("Effect of Moving One Spot Up in Payroll Rank"),
                     p("Effect of 1 means an assosicated 1 point increase in win
                       percentage (on average)"),
                     p("Bars show 95% confidence interval"),
                     plotlyOutput("team_effect")
                 )
             ),
             sidebarLayout(
                 
                 # sidebar panel holds observational notes about the data
                 
                 sidebarPanel(
                     h4(strong("Observations:")),
                     h4("NBA"),
                     p("Payroll's effect on win percentage declined from 1985
                       until 1997, at which point it leveled off"),
                     p("In every season except 1984-95 and 1988-89, the 95% 
                       confidence interval contains zero - so for all but two of
                       the seasons, we can't be sure an additional $1 million on
                       payroll had any effect at all on win percentage"),
                     h4("MLB"),
                     p("With the exception of 1985, there seems to be no real 
                        effect of payroll on win percentage regardless of season
                        - and even the 1985 effect is small ($1 million 
                        additional payroll is associated, on average, with less 
                       than a .5 point increase in win percentage)")
                 ),
                 
                 # main panel holds plotly: payroll's effect on win pct by year
                 
                 mainPanel(
                     h3(strong("Payroll's Effect on Win Percentage by Season")),
                     h4("Effect of Spending Additional $1 Million"),
                     p("Effect of 1 means an assosicated 1 point increase in win
                       percentage (on average)"),
                     p("Bars show 95% confidence interval"),
                     plotlyOutput("year_effect")
                 )
             )
    ),
    
    # about panel, to explain the project and do some shameless PR. All text is
    # assigned to objects in about_page_text.R
    
    tabPanel("About", 
             titlePanel("About the Project"),
             p(p1),
             h3("Definitional Notes"),
             p(p2),
             h3("More to Come?"),
             p(p3),
             p(p4),
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
    
    # reactive bullet point for payroll over time plot, giving a unique
    # description for each league
    
    output$payroll_over_time <- renderText({
        
        # choose text to display based on input$league selector
        
        payroll_over_time_text <- ifelse(input$league == "nba",
               
               "In the NBA, inequality has also grown over time, though the 
               spread was widest between 1997 and 2008",
               
               "In the MLB, inequality has grown massively, led by the 
               high-spending New York Yankees")
    })
    
    # reactive plotly for payroll over time based on league selector; pre-made
    # plots from helper code files
    
    output$payroll <- renderPlotly({
        
        # choose plot based on input$league selector
        
        plot1 <- case_when(input$league == "nba" ~ list(nba_plot_1),
                           input$league == "mlb" ~ list(mlb_plot_1),
                           input$league == "nhl" ~ list(nhl_plot_1),
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
    
    # de-facto subtitle for cor by year table in sidebar
    
    output$year_cor_text <- renderText({
        
        # choose text to show based on input$league selector
        
        ifelse(input$league == "nba",
               year_cor_subtitle <- "For NBA, by season",
               year_cor_subtitle <- "For MLB, by year")
    })
    
    # text to summarize strongest cor values from by-year table
    
    output$year_cor_max <- renderText({
        
        # choose text to show based on input$league selector
        
        ifelse(input$league == "nba",
               strong_year_cor <- "Relationship strongest (> 0.55) in 1984-85, 
                                   1999-00, and 2017-18",
               strong_year_cor <- "Relationship strongest (> 0.55) in 1998, 
                                   1999, and 2016")
    })
    
    # text to summarize weakest cor values from by-year table
    
    output$year_cor_min <- renderText({
        
        # choose text to show based on input$league selector
        
        ifelse(input$league == "nba",
               weak_year_cor <- "Relationship weakest (<= 0.1) in 1986-87,
                                 2005-06, and 2006-07",
               weak_year_cor <- "Relationship weakest (< 0.05) in 1987, 1990,
                                 and 1992")
    })
    
    # text to summarize average (mean) cor value from by-year table
    
    output$year_cor_avg <- renderText({
        
        # choose text to show based on input$league selector
        
        ifelse(input$league == "nba",
               avg_year_cor <- "Average correlation across years: 0.36",
               avg_year_cor <- "Average correlation across years: 0.356")
    })
    
    # render the gt table for the sidebar display
    
    output$year_cor <- render_gt({
        
        # choose table to show based on input$league selector
        
        year_cor_table <- case_when(
            input$league == "nba" ~ list(nba_year_cor_table),
            input$league == "mlb" ~ list(mlb_year_cor_table),
            input$league == "nhl" ~ list(nhl_year_cor_table),
            TRUE ~ list(mls_year_cor_table)) %>%
        .[[1]]
        
        # show table
        
        year_cor_table
        
    })
    
    # reactive plotly to show payroll and wins by year, using plots from helper
    # files
    
    output$by_year <- renderPlotly({
        
        # choose plot based on input$league selector
        
        plot2 <- case_when(
            input$league == "nba" ~ list(nba_plot_2),
            input$league == "mlb" ~ list(mlb_plot_2),
            input$league == "nhl" ~ list(nhl_plot_2),
            TRUE ~ list(mls_plot_2)) %>%
            .[[1]]
        
        # assign the plot to ggplotly so hover reveals text, and specify
        # dimensions - these were a pain with a large facet_wrap plot
        
        gp2 <- ggplotly(plot2, tooltip = "text", height = 600, width = 850)
        
        # change y-axis title position so it doesn't overlap with labels
        
        gp2[['x']][['layout']][['annotations']][[2]][['x']] <- -0.045
        
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
    
    # de-facto subtitle for cor by team table in sidebar
    
    output$team_cor_text <- renderText({
        
        # choose text to show based on input$league selector
        
        ifelse(input$league == "nba",
               team_cor_subtitle <- "For NBA, by franchise",
               team_cor_subtitle <- "For MLB, by franchise")
    })
    
    # text to describe strongest cor values from by-team table
    
    output$team_cor_max <- renderText({
        
        # choose text to show based on input$league selector
        
        ifelse(input$league == "nba",
               strong_team_cor <- "Relationship strongest (> 0.55) for
                                   Grizzlies, Bulls, Mavericks, and Cavaliers",
               strong_team_cor <- "Relationship strongest (0.69!) for NYY;
                                   greater than 0.5 for DET, ANA, and ATL")
    })
    
    # text to describe weakest cor values from by-team table
    
    output$team_cor_min <- renderText({
        
        # choose text to show based on input$league selector
        
        ifelse(input$league == "nba",
               weak_team_cor <- "Relationship weakest (NEGATIVE!) for
                                 Pelicans/Hornets, Knicks, and Trail Blazers",
               weak_team_cor <- "Relationship weakest (NEGATIVE!) for TOR;
                                 less than .05 for CIN")
    })
    
    # text to describe average cor values from by-team table (useful to see if
    # a given team is above or below average)
    
    output$team_cor_avg <- renderText({
        
        # choose text to show based on input$league selector
        
        ifelse(input$league == "nba",
               avg_team_cor <- "Average correlation across franchises: 0.342",
               avg_team_cor <- "Average correlation across franchises: 0.265")
    })
    
    # render the cor by team gt table for display in sidebar
    
    output$team_cor <- render_gt({
        
        # choose table to show based on input$league selector
        
        team_cor_table <- case_when(
            input$league == "nba" ~ list(nba_team_cor_table),
            input$league == "mlb" ~ list(mlb_team_cor_table),
            input$league == "nhl" ~ list(nhl_team_cor_table),
            TRUE ~ list(mls_team_cor_table)) %>%
            .[[1]]
        
        # show table
        
        team_cor_table
        
    })
    
    # render plotly of payroll and wins by team
    
    output$by_team <- renderPlotly({
        
        # choose league to plot based on input$league selector
        
        plot3 <- case_when(input$league == "nba" ~ list(nba_plot_3),
                           input$league == "mlb" ~ list(mlb_plot_3),
                           input$league == "nhl" ~ list(nhl_plot_3),
                           TRUE ~ list(mls_plot_3)) %>%
            .[[1]]
                           
        
        # assign the plot to ggplotly so hover reveals text and specify
        # dimensions
        
        gp3 <- ggplotly(plot3, tooltip = "text", height = 600, width = 850)
        
        # change y-axis title position so it doesn't overlap with labels
        
        gp3[['x']][['layout']][['annotations']][[2]][['x']] <- -0.045
        
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
    
    # render plotly: effect of payroll on wins with team interaction
    
    output$team_effect <- renderPlotly({
        
        # select league's plot to display based on input$league_2
        
        ifelse(input$league_2 == "nba",
               mod_plot_1 <- nba_mod_plot_1,
               mod_plot_1 <- mlb_mod_plot_1)
        
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
    
    # render plotly: effect of payroll on wins with season interaction
    
    output$year_effect <- renderPlotly({
        
        # select league's plot to display based on input$league_2
        
        ifelse(input$league_2 == "nba",
               mod_plot_2 <- nba_mod_plot_2,
               mod_plot_2 <- mlb_mod_plot_2)
        
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

}

# Run the application 

shinyApp(ui = ui, server = server)
