library(shiny)
library(plotly)

# load mlb data, including plot

source("mlb_data.R")

# load nba data, including plot

source("nba_data.R")

ui <- navbarPage(
    
    # title
    
    "Payroll and Performance: Comparing MLB and the NBA",
    
    # plots panel, which will have plots to show data by league
    
    tabPanel("Plots",
             fluidPage(
                 titlePanel("Comparing Leagues"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "league",
                             "League",
                             c("NBA" = "nba", "MLB" = "mlb")
                         ),
                         h3("text")
                     ),
                     mainPanel(
                         h3("Change in Payroll over Time"),
                         h4("Say something about the amount getting higher (even
                            after adjusting for inflation) and the spread
                            (inequality) getting larger - but the NBA seems to
                            have done something about it circa 2008-09ish, 
                            while the MLB hasn't really"),
                         plotlyOutput("payroll")
                     )
                 ),
                 sidebarLayout(
                     sidebarPanel(
                         h4("text"),
                         p("add a description of what's happening in this plot
                           and what interactivity it has"),
                         gt_output("year_cor")
                         ),
                     mainPanel(
                         h3("Payroll and Win Percentage by Year"),
                         h4("Overall correlation is stronger for MLB than for 
                            NBA, but strength of correlation fluctuates
                            considerably from one year to another"),
                         plotlyOutput("by_year",
                                          # hover = "plot_hover",
                                          # hoverDelay = 0
                                          ))),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 sidebarLayout(
                     sidebarPanel(
                         h4("text"),
                         p("add a description of what's happening in this plot
                               and what interactivity it has"),
                         gt_output("team_cor")
                     ),
                     mainPanel(
                         h3("Payroll and Win Percentage by Franchise"),
                         h4("Do some teams spend more wisely than others (i.e. 
                            by actually winning more when they spend more,
                            instead of over-spending for poor performance)? Do
                            some teams win no matter how much they spend? Do
                            some teams spend indescriminately and still lose?"),
                         plotlyOutput("by_team")
                    )),
                 br(),
                 br()
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
    
    output$year_cor <- render_gt({
        
        ifelse(input$league == "nba",
               cor_table <- nba_year_cor_table,
               cor_table <- mlb_year_cor_table)
        cor_table
        
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
    
    output$team_cor <- render_gt({
        
        ifelse(input$league == "nba",
               cor_table <- nba_team_cor_table,
               cor_table <- mlb_team_cor_table)
        cor_table
        
        })

}

# Run the application 

shinyApp(ui = ui, server = server)
