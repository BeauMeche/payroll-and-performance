#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("mlb_data.R")
source("nba_data.R")

ui <- navbarPage(
    "Payroll and Performance: Comparing MLB and the NBA",
    tabPanel("Plots",
             fluidPage(
                 titlePanel("Comparing Leagues"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "league",
                             "League",
                             c("MLB" = "mlb", "NBA" = "nba")
                         )),
                     mainPanel(plotOutput("league_plot")))
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them"),
             p("[Content forthcoming]")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Goals"),
             p("This is where I talk about my project.
               More content forthcoming."),
             h3("About Me"),
             p("My name is Westley Cook and I major in East Asian Studies,
             with a secondary in Government. 
             You can reach me at wkcook@college.harvard.edu.")))

server <- function(input, output) {
    output$league_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
        
        ifelse(
            input$league == "nba",
            
            # If input$plot_type is "a", plot histogram of "waiting" column 
            # from the faithful dataframe
            
            plot   <- nba_plot,
            
            # If input$plot_type is "b", plot histogram of "eruptions" column
            # from the faithful dataframe
            
            plot   <- mlb_plot
        )
        
        plot
        
    }, height = 700, width = 700)
}

# Run the application 
shinyApp(ui = ui, server = server)
