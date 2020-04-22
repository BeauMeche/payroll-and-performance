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
                             "league_1",
                             "League",
                             c("MLB" = "mlb", "NBA" = "nba")
                         ),
                         h4("text"),
                         p("add a description of what's happening in this plot
                           and what interactivity it has")
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
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 sidebarPanel(
                     selectInput(
                         "league_2",
                         "League",
                         c("MLB" = "mlb", "NBA" = "nba")
                     ),
                     h4("text"),
                     p("add a description of what's happening in this plot
                           and what interactivity it has")
                 ),
                 mainPanel(
                     h3("Payroll and Win Percentage by Team"),
                     h4("[insert descriptive text here]"),
                     plotlyOutput("by_team")
                )
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
    output$by_year <- renderPlotly({
        
        # Choose league to plot based on input$league from ui
        
        ifelse(
            input$league_1 == "nba",
            
            # If input$plot_type is "nba", plot nba data
            
            plot1 <- nba_plot_1,
            
            # Otherwise, plot mlb data (would change this if added more leagues)
            
            plot1 <- mlb_plot_1
        )
        
        # make the plot
        
        ggplotly(plot1, tooltip = "text") %>% 
            config(modeBarButtonsToRemove = list("sendDataToCloud", 
                                                 "hoverClosestCartesian", 
                                                 "hoverCompareCartesian", 
                                                 "select2d", 
                                                 "lasso2d", 
                                                 "toggleSpikelines",
                                                 "editInChartStudio",
                                                 "zoom2d",
                                                 "pan2d")) %>% 
            layout(height = 900, width = 1100)
        
       # specify dimensions - otherwise it gets morphed by the window
        
    })

    output$by_team <- renderPlotly({
    
        # Choose league to plot based on input$league from ui
        
        ifelse(
            input$league_2 == "nba",
            
            # If input$plot_type is "nba", plot nba data
            
            plot2 <- nba_plot_2,
            
            # Otherwise, plot mlb data (would change this if added more leagues)
            
            plot2 <- mlb_plot_2
        )
        
        # show the plot
        
        ggplotly(plot2, tooltip = "text") %>% 
            config(modeBarButtonsToRemove = list("sendDataToCloud", 
                                                 "hoverClosestCartesian", 
                                                 "hoverCompareCartesian", 
                                                 "select2d", 
                                                 "lasso2d", 
                                                 "toggleSpikelines",
                                                 "editInChartStudio",
                                                 "zoom2d",
                                                 "pan2d")) %>% 
            layout(height = 900, width = 1100)
        
        # specify dimensions - otherwise it gets morphed by the window
        
    })

}

# Run the application 

shinyApp(ui = ui, server = server)
