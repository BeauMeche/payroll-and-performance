library(shiny)
library(plotly)

# load mlb data, including plot

source("mlb_data.R")

# load nba data, including plot

source("nba_data.R")

ui <- navbarPage(
    
    # title
    
    "Payroll and Performance in the NBA and MLB",
    
    # plots panel, which will have plots to show data by league
    
    tabPanel("Trends",
             fluidPage(
                 titlePanel("Payroll and Regular Season Win Percentage"),
                 sidebarLayout(
                     sidebarPanel(
                         h4(strong("Select a League")),
                         p("Note: displays best in large window, and may take
                            time to load"),
                         selectInput(
                             "league",
                             "League",
                             c("NBA" = "nba", "MLB" = "mlb")
                         ),
                         p("All plots on this page are interactive; try hovering
                            your mouse over a point!"),
                         p("A control bar at the top of each plot offers 
                            additional options such as zoom, autoscale,
                            downloading as png, and resetting to the default.")
                     ),
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
                         gt_output("year_cor")
                     ),
                     mainPanel(
                         h3(strong("Relationship by Year")),
                         h4("Does the relationship between payroll and win
                            percentage vary by year? Clearly, the answer is 
                            yes."),
                         plotlyOutput("by_year")
                    )
                 ),
                 br(),
                 br(),
                 br(),
                 br(),
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
                         gt_output("team_cor")
                     ),
                     mainPanel(
                         h3(strong("Relationship by Franchise")),
                         h4("Does the relationship between payroll and win
                            percentage vary by franchise? Again, the answer is
                            yes."),
                         h4("Most teams tend to earn more wins when they spend 
                            more on payroll. But others see no strong 
                            relationship between payroll and win percentage, and
                            a few have the dubious distinction of experiencing a
                            NEGATIVE relationship - meaning they tend to win 
                            LESS when they spend more than their competitors."),
                         p(strong("Note the x axis: payroll rank is how a team's
                            spending in a season compared to others in the
                            league (1 is lowest and 30 is highest).")),
                         plotlyOutput("by_team")
                    ))
             )),
    
    # model panel, to talk more about model and regression results
    
    tabPanel("Models",
             titlePanel("Modeling the Data"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them"),
             p("[Content forthcoming]")),
    
    # about panel, to explain the project and do some shameless PR
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Goals"),
             p("This is where I talk about my project.
               More content forthcoming."),
             h3("About Me"),
             p("My name is Westley Cook and I major in East Asian Studies
             with a secondary in Government. 
             You can reach me at wkcook@college.harvard.edu.")))





server <- function(input, output) {
    
    output$payroll_over_time <- renderText({
        
        payroll_over_time_text <- ifelse(input$league == "nba",
               
               "In the NBA, inequality has also grown over time, though the 
               spread was widest between 1997 and 2008",
               
               "In the MLB, inequality has grown massively, led by the 
               high-spending New York Yankees")
        
        payroll_over_time_text
    })
    
    output$payroll <- renderPlotly({
        
        ifelse(input$league == "nba",
               
               plot1 <- nba_plot_1,
               
               plot1 <- mlb_plot_1)
        
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
    
    output$by_year <- renderPlotly({
        
        # Choose league to plot based on input$league from ui
        
        ifelse(
            input$league == "nba",
            
            # If input$plot_type is "nba", plot nba data
            
            plot2 <- nba_plot_2,
            
            # Otherwise, plot mlb data (would change this if added more leagues)
            
            plot2 <- mlb_plot_2
        )
        
        # assign the plot to ggplotly so hover reveals text and specify
        # dimensions
        
        gp2 <- ggplotly(plot2, tooltip = "text", height = 800, width = 1050)
        
        # change y-axis title position so it doesn't overlap with labels
        
        gp2[['x']][['layout']][['annotations']][[2]][['x']] <- -0.035
        
        # display plot, customizing mode bar
        
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
    
    output$year_cor_text <- renderText({
        
        ifelse(input$league == "nba",
               year_cor_subtitle <- "For NBA, by season",
               year_cor_subtitle <- "For MLB, by year")
        year_cor_subtitle
        
    })
    
    output$year_cor_max <- renderText({
        
        ifelse(input$league == "nba",
               strong_year_cor <- "Relationship strongest (> 0.55) in 1984-85, 
                                   1999-00, and 2017-18",
               strong_year_cor <- "Relationship strongest (> 0.55) in 1998, 
                                   1999, and 2016")
        strong_year_cor
    })
    
    output$year_cor_min <- renderText({
        
        ifelse(input$league == "nba",
               weak_year_cor <- "Relationship weakest (<= 0.1) in 1986-87,
                                 2005-06, and 2006-07",
               weak_year_cor <- "Relationship weakest (< 0.05) in 1987, 1990,
                                 and 1992")
        weak_year_cor
        
    })
    
    output$year_cor_avg <- renderText({
        
        ifelse(input$league == "nba",
               avg_year_cor <- "Average correlation across years: 0.36",
               avg_year_cor <- "Average correlation across years: 0.356")
        avg_year_cor

    })
    
    output$year_cor <- render_gt({
        
        ifelse(input$league == "nba",
               year_cor_table <- nba_year_cor_table,
               year_cor_table <- mlb_year_cor_table)
        year_cor_table
        
    })

    output$by_team <- renderPlotly({
    
        # Choose league to plot based on input$league from ui
        
        ifelse(
            input$league == "nba",
            
            # If input$plot_type is "nba", plot nba data
            
            plot3 <- nba_plot_3,
            
            # Otherwise, plot mlb data (would change this if added more leagues)
            
            plot3 <- mlb_plot_3
        )
        
        # assign the plot to ggplotly so hover reveals text and specify
        # dimensions
        
        gp3 <- ggplotly(plot3, tooltip = "text", height = 800, width = 1050)
        
        # change y-axis title position so it doesn't overlap with labels
        
        gp3[['x']][['layout']][['annotations']][[2]][['x']] <- -0.035
        
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
    
    output$team_cor_text <- renderText({
        
        ifelse(input$league == "nba",
               team_cor_subtitle <- "For NBA, by franchise",
               team_cor_subtitle <- "For MLB, by franchise")
    })
    
    output$team_cor_max <- renderText({
        
        ifelse(input$league == "nba",
               strong_team_cor <- "Relationship strongest (> 0.55) for
                                   Grizzlies, Bulls, Mavericks, and Cavaliers",
               strong_team_cor <- "Relationship strongest (0.69!) for NYY;
                                   greater than 0.5 for DET, ANA, and ATL")
        strong_team_cor
    })
    
    output$team_cor_min <- renderText({
        
        ifelse(input$league == "nba",
               weak_team_cor <- "Relationship weakest (NEGATIVE!) for
                                 Pelicans/Hornets, Knicks, and Trail Blazers",
               weak_team_cor <- "Relationship weakest (NEGATIVE!) for TOR;
                                 less than .05 for CIN")
        weak_team_cor
        
    })
    
    output$team_cor_avg <- renderText({
        
        ifelse(input$league == "nba",
               avg_team_cor <- "Average correlation across franchises: 0.342",
               avg_team_cor <- "Average correlation across franchises: 0.265")
        avg_team_cor
        
    })
    
    output$team_cor <- render_gt({
        
        ifelse(input$league == "nba",
               team_cor_table <- nba_team_cor_table,
               team_cor_table <- mlb_team_cor_table)
        team_cor_table
        
        })

}

# Run the application 

shinyApp(ui = ui, server = server)
