library(shiny)

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
                             c("MLB" = "mlb", "NBA" = "nba")
                         )),
                     mainPanel(plotOutput("league_plot")))
             )),
    
    # discussion panel, to talk more about the content?
    
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
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
    output$league_plot <- renderPlot({
        
        # Generate type based on input$league_plot from ui
        
        ifelse(
            input$league == "nba",
            
            # If input$plot_type is "nba", plot nba data
            
            plot   <- nba_plot,
            
            # Otherwise, plot mlb data (would change this if added more leagues)
            
            plot   <- mlb_plot
        )
        
        # show the pre-made plot
        
        plot
        
       # specify dimensions - otherwise it gets morphed by the window
        
    }, height = 700, width = 700)
}

# Run the application 

shinyApp(ui = ui, server = server)
